# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ----------------------------------------------------------------------------
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module implements the PSyclone NEMO API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, Schedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType).

'''

from __future__ import print_function, absolute_import
import copy
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Node, \
    Loop, Kern, GenerationError, InternalError, colored, IfBlock, IfClause, \
    NameSpaceFactory, SCHEDULE_COLOUR_MAP as _BASE_CMAP, ACCEnterDataDirective
from fparser.two.utils import walk_ast, get_child
from fparser.two import Fortran2003

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"

# The valid types of loop and associated loop variable and bounds
VALID_LOOP_TYPES = {"lon": {"var": "ji", "start": "1", "stop": "jpi"},
                    "lat": {"var": "jj", "start": "1", "stop": "jpj"},
                    "levels": {"var": "jk", "start": "1", "stop": "jpk"},
                    # TODO what is the upper bound of tracer loops?
                    "tracers": {"var": "jt", "start": "1", "stop": ""},
                    "unknown": {"var": "", "start": "1", "stop": ""}}

# Mapping from loop variable to loop type. This is how we identify each
# explicit do loop we encounter.
NEMO_LOOP_TYPE_MAPPING = {"ji": "lon", "jj": "lat", "jk": "levels",
                          "jt": "tracers", "jn": "tracers"}

# Mapping from loop type to array index. NEMO uses an "i, j, k" data
# layout.
NEMO_INDEX_ORDERING = ["lon", "lat", "levels", "tracers"]


class ASTProcessor(object):
    '''
    Mixin class to provide functionality for processing the fparser2 AST.
    '''
    @staticmethod
    def nodes_to_code_block(parent, statements):
        '''
        Create a NemoCodeBlock for the supplied list of statements
        and then wipe the list of statements. A NemoCodeBlock is a node
        in the PSyIRe (Schedule) that represents a sequence of one or more
        Fortran statements which PSyclone does not attempt to handle.

        :param parent: Node in the PSyclone AST to which to add this code \
                       block.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param list statements: List of fparser2 AST nodes consituting the \
                                code block.
        :rtype: :py:class:`psyclone.nemo.NemoCodeBlock`
        '''
        if not statements:
            return None

        code_block = NemoCodeBlock(statements,
                                   parent=parent)
        parent.addchild(code_block)
        del statements[:]
        return code_block

    # TODO remove nodes_parent argument once fparser2 AST contains
    # parent information (fparser/#102).
    def process_nodes(self, parent, nodes, nodes_parent):
        '''
        Create the PSyclone IR of the supplied list of nodes in the
        fparser2 AST. Currently also inserts parent information back
        into the fparser2 AST. This is a workaround until fparser2
        itself generates and stores this information.

        :param parent: Parent node in the PSyclone IR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param nodes: List of sibling nodes in fparser2 AST.
        :type nodes: list of :py:class:`fparser.two.utils.Base` or \
                     :py:class:`fparser.two.utils.BlockBase`
        :param nodes_parent: the parent of the supplied list of nodes in \
                             the fparser2 AST.
        :type nodes_parent: :py:class:`fparser.two.utils.Base` or \
                            :py:class:`fparser.two.utils.BlockBase`

        '''
        code_block_nodes = []
        for child in nodes:
            # TODO remove this line once fparser2 contains parent
            # information (fparser/#102)
            child._parent = nodes_parent  # Retro-fit parent info
            if NemoLoop.match(child):
                # The start of a loop is taken as the end of any
                # existing code block so we create that now
                self.nodes_to_code_block(parent, code_block_nodes)
                parent.addchild(NemoLoop(child, parent=parent))
            elif NemoImplicitLoop.match(child):
                # An implicit loop marks the end of any current
                # code block
                self.nodes_to_code_block(parent, code_block_nodes)
                parent.addchild(NemoImplicitLoop(child, parent=parent))
            elif NemoIfBlock.match(child):
                self.nodes_to_code_block(parent, code_block_nodes)
                parent.addchild(NemoIfBlock(child, parent=parent))
            elif not isinstance(child, (Fortran2003.End_Do_Stmt,
                                        Fortran2003.Nonlabel_Do_Stmt,
                                        Fortran2003.Comment)):
                # Don't include do, end-do or comment in a code block
                code_block_nodes.append(child)

        # Complete any unfinished code-block
        self.nodes_to_code_block(parent, code_block_nodes)


class NemoInvoke(Invoke):
    '''
    Represents a NEMO 'Invoke' which, since NEMO is existing code, means
    an existing program unit, e.g. a subroutine.

    :param ast: node in fparser2 AST representing the program unit.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Module`
    :param str name: the name of this Invoke (program unit).
    '''
    def __init__(self, ast, name):
        self._schedule = None
        self._name = name
        # Store the whole fparser2 AST
        self._ast = ast
        self._name_space_manager = NameSpaceFactory().create()
        from fparser.two.Fortran2003 import Execution_Part, Specification_Part

        # Find the section of the tree containing the execution part
        # of the code
        exe_part = get_child(ast, Execution_Part)
        if not exe_part:
            # This subroutine has no execution part so we skip it
            # TODO log this event
            return

        # Store the root of this routine's specification in the AST
        self._spec_part = get_child(ast, Specification_Part)

        # List of unique loop variables that we generate for this Invoke
        # (e.g. when making implicit loops explicit)
        self._loop_vars = []

        # We now walk through the AST produced by fparser2 and construct a
        # new AST using objects from the nemo module.
        self._schedule = NemoSchedule(self, exe_part)

    def update(self):
        '''Updates the fparser2 AST associated with this Schedule to make it
        reflect any transformations that have been applied to the
        PSyclone AST.

        '''
        if not self._schedule:
            return
        self._schedule.update()


class NemoInvokes(Invokes):
    '''
    Class capturing information on all 'Invokes' (program units) within
    a single NEMO source file.

    :param ast: The fparser2 AST for the whole Fortran source file
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program`
    '''
    def __init__(self, ast):
        from fparser.two.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt, Name

        self.invoke_map = {}
        self.invoke_list = []
        # Keep a pointer to the whole fparser2 AST
        self._ast = ast

        # Find all the subroutines contained in the file
        routines = walk_ast(ast.content, [Subroutine_Subprogram,
                                          Function_Subprogram])
        # Add the main program as a routine to analyse - take care
        # here as the Fortran source file might not contain a
        # main program (might just be a subroutine in a module)
        main_prog = get_child(ast, Main_Program)
        if main_prog:
            routines.append(main_prog)

        # Analyse each routine we've found
        for subroutine in routines:
            # Get the name of this (sub)routine
            substmt = walk_ast(subroutine.content,
                               [Subroutine_Stmt, Function_Stmt, Program_Stmt])
            if isinstance(substmt[0], Function_Stmt):
                for item in substmt[0].items:
                    if isinstance(item, Name):
                        sub_name = str(item)
                        break
            else:
                sub_name = str(substmt[0].get_name())

            my_invoke = NemoInvoke(subroutine, name=sub_name)
            self.invoke_map[sub_name] = my_invoke
            self.invoke_list.append(my_invoke)

    def update(self):
        ''' Walk down the tree and update the underlying fparser2 AST
        to reflect any transformations. '''
        for invoke in self.invoke_list:
            invoke.update()


class NemoPSy(PSy):
    '''
    The NEMO-specific PSy class. This creates a NEMO-specific
    invokes object (which controls all the required invocation calls).
    Also overrides the PSy gen() method so that we update and then
    return the fparser2 AST for the (transformed) PSy layer.

    :param ast: the fparser2 AST for this PSy layer (i.e. NEMO routine)
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
    :raises InternalError: if no Fortran2003.Name nodes are found in the \
                           supplied AST.
    '''
    def __init__(self, ast):
        names = walk_ast(ast.content, [Fortran2003.Name])
        # The name of the program unit will be the first in the list
        if not names:
            raise InternalError("Found no names in supplied Fortran - should "
                                "be impossible!")
        self._name = str(names[0]) + "_psy"

        self._invokes = NemoInvokes(ast)
        self._ast = ast

    def inline(self, _):
        '''
        :raises NotImplementedError: since kernels in NEMO are, in general,
                                     already in-lined.
        '''
        # Override base-class method because we don't yet support it
        raise NotImplementedError("The NemoPSy.inline method has not yet "
                                  "been implemented!")

    @property
    def gen(self):
        '''
        Generate the (updated) fparser2 AST for the NEMO code represented
        by this NemoPSy object.

        :returns: the fparser2 AST for the Fortran code.
        :rtype: :py:class:`fparser.two.Fortran2003.Main_Program` or \
                :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
                :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
        '''
        # Walk down our Schedule and update the underlying fparser2 AST
        # to account for any transformations
        self.invokes.update()

        # Return the fparser2 AST
        return self._ast


class NemoSchedule(Schedule, ASTProcessor):
    '''
    The NEMO-specific schedule class. This is the top-level node in
    PSyclone's IR of a NEMO program unit (program, subroutine etc).

    :param invoke: The Invoke to which this Schedule belongs.
    :type invoke: :py:class:`psyclone.nemo.NemoInvoke`
    :param ast: the fparser2 AST of the NEMO code for which to generate \
                a Schedule.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.

    '''
    def __init__(self, invoke, ast):
        Node.__init__(self)

        self._invoke = invoke
        self._ast = ast
        self.process_nodes(self, ast.content, ast)

    def view(self, indent=0):
        '''
        Print a representation of this NemoSchedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this NemoSchedule. '''
        result = "NemoSchedule():\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result


class NemoCodeBlock(Node):
    '''
    Node representing some generic Fortran code that PSyclone
    does not attempt to manipulate. As such it is a leaf in the PSyclone
    IR and therefore has no children.

    :param statements: list of fparser2 AST nodes representing the Fortran \
                       code constituting the code block.
    :type statements: list of :py:class:`fparser.two.utils.Base` or \
                      :py:class:`fparser.two.utils.BlockBase` objects.
    :param parent: the parent node of this code block in the PSyIRe.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, statements, parent=None):
        Node.__init__(self, parent=parent)
        # Store a list of the parser objects holding the code associated
        # with this block. We make a copy of the contents of the list because
        # the list itself is a temporary product of the process of converting
        # from the fparser2 AST to the PSyIRe.
        if not statements:
            raise GenerationError("NemoCodeBlock must have content.")
        self._statements = statements[:]
        self._ast = self._statements[0]
        self._ast_start = self._statements[0]
        self._ast_end = self._statements[-1]

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :return: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("NemoCodeBlock", NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[" +
              str(type(self._statements[0])) + "]")

    def __str__(self):
        return "CodeBlock[{0} statements]".format(len(self._statements))

    def gen_code(self):
        '''
        Override abstract method from base class.

        :raises InternalError: because it is not relevant to the NEMO API and \
                               should never be called.
        '''
        raise InternalError("NemoCodeBlock.gen_code() should not be called.")


class NemoKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. Kernels are leaves in the Schedule AST (i.e. they have
    no children).

    :param loop: Reference to the loop (in the fparser2 AST) containing \
                 this kernel
    :type loop: :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
    :param parent: the parent of this Kernel node in the PSyclone AST
    type parent: :py:class:`psyclone.nemo.NemoLoop`
    '''
    def __init__(self, loop=None, parent=None):
        ''' Create an empty NemoKern object. The object is given state via
        a subsequent call to the load method if loop is None. '''
        # Create those member variables required for testing and to keep
        # pylint happy
        self._children = []
        self._name = ""
        # The Loop object created by fparser2 which holds the AST for the
        # section of code associated with this kernel
        self._loop = None
        # List of the loop variables, one for each loop
        self._loop_vars = []
        # A list of 2-tuples, one for each loop
        self._loop_ranges = []
        # List of variable names that must be thread-private
        self._private_vars = None
        # List of variable names that must be first-private because they
        # are scalars with a first access of read
        self._first_private_vars = None
        # Whether or not this kernel performs a reduction
        self._reduction = False
        # List of variables that are shared between threads
        self._shared_vars = None
        # Type of kernel (2D, 3D..)
        self._kernel_type = ""
        self._body = []
        # Will point to the corresponding set of nodes in the fparser2 AST
        self._ast = []
        if loop:
            self.load(loop)

    @staticmethod
    def match(node):
        '''
        Whether or not the AST fragment pointed to by node represents a
        kernel. A kernel is defined as a section of code that sits
        within a recognised loop structure and does not itself contain
        loops or IO operations.

        :param node: Node in fparser2 AST to check.
        :type node: :py:class:`fparser.two.Fortran2003.Base`
        :returns: True if this node conforms to the rules for a kernel.
        :rtype: bool
        '''
        from fparser.two.Fortran2003 import Subscript_Triplet,  \
            Block_Nonlabel_Do_Construct, Write_Stmt, Read_Stmt, Call_Stmt
        child_loops = walk_ast(node.content,
                               [Block_Nonlabel_Do_Construct, Write_Stmt,
                                Read_Stmt, Call_Stmt])
        if child_loops:
            # A kernel cannot contain other loops or reads or writes
            return False

        # Currently a kernel cannot contain implicit loops.
        # TODO we may have to differentiate between implicit loops over
        # grid points and any other implicit loop. Possibly using the
        # scope of the array being accessed?
        impl_loops = walk_ast(node.content, [Subscript_Triplet])
        if impl_loops:
            return False

        return True

    @property
    def ktype(self):
        '''
        :returns: what type of kernel this is.
        :rtype: str
        '''
        return self._kernel_type

    def load(self, loop):
        ''' Populate the state of this NemoKern object.

        :param loop: node in the fparser2 AST representing a loop (explicit \
                     or implicit).
        :type loop: :py:class:`fparser.two.Fortran2003.Assignment_Stmt` or \
                :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`

        :raises InternalError: if the supplied loop node is not recognised.
        '''
        from fparser.two.Fortran2003 import Block_Nonlabel_Do_Construct, \
            Assignment_Stmt

        if isinstance(loop, Block_Nonlabel_Do_Construct):
            self._load_from_loop(loop)
        elif isinstance(loop, Assignment_Stmt):
            self._load_from_implicit_loop(loop)
        else:
            raise InternalError(
                "Expecting either Block_Nonlabel_Do_Construct or "
                "Assignment_Stmt but got {0}".format(str(type(loop))))

    def _load_from_loop(self, loop):
        '''
        Populate the state of this NemoKern object from an fparser2
        AST for an explicit loop.

        :param loop: Node in the fparser2 AST representing an implicit loop.
        :type loop: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`

        :raises InternalError: if first child of supplied loop node is not a \
                           :py:class:`fparser.two.Fortran2003.Nonlabel_Do_Stmt`
        '''
        from fparser.two.Fortran2003 import Nonlabel_Do_Stmt, End_Do_Stmt

        # Keep a pointer to the original loop in the AST
        self._loop = loop

        # Search for start of loop (allow for Comment nodes)
        found_start = False
        for idx, child in enumerate(loop.content):
            if isinstance(child, Nonlabel_Do_Stmt):
                found_start = True
                break
        if not found_start:
            raise InternalError("Failed to find Nonlabel_Do_Stmt in children "
                                "of Block_Nonlabel_Do_Construct:\n"
                                "{0}".format(str(loop)))
        self._body = []
        for content in loop.content[idx:]:
            if isinstance(content, End_Do_Stmt):
                break
            self._body.append(content)

        # Kernel is "explicit" since we have a coded loop nest rather than
        # array notation
        self._kernel_type = "Explicit"

        # TODO decide how to provide this functionality. Do we use
        # Habakkuk or something else?
        #  Analyse the loop body to identify private and shared variables
        #  for use when parallelising with OpenMP.
        # from habakkuk.make_dag import dag_of_code_block
        #  Create a DAG of the kernel code block using Habakkuk
        # kernel_dag = dag_of_code_block(loop, "nemo_kernel")
        # inputs = kernel_dag.input_nodes()
        # outputs = kernel_dag.output_nodes()
        # print "Kernel has {0} outputs: ".format(len(outputs)) + \
        #     ",".join([node.variable.orig_name for node in outputs])
        self._shared_vars = set()
        self._first_private_vars = set()
        self._private_vars = set()
        #  If there are scalar variables that are inputs to the DAG (other than
        #  the loop counters) then they must be declared first-private in an
        #  OpenMP loop directive.
        # for node in inputs:
        #     if not node.node_type:
        #         if node.name not in NEMO_LOOP_TYPE_MAPPING:
        #             self._first_private_vars.add(node.name)
        # for key, node in kernel_dag._nodes.iteritems():
        #     if node.node_type == "array_ref":
        #         self._shared_vars.add(node.variable.orig_name)
        #     elif not node.node_type:
        #         self._private_vars.add(node.variable.orig_name)
        # self._private_vars -= self._first_private_vars
        # print "OpenMP shared vars: " + ",".join(self._shared_vars)
        # print "OpenMP private vars: " + ",".join(self._private_vars)
        # print "OpenMP first-private vars: " + \
        #     ",".join(self._first_private_vars)

    def _load_from_implicit_loop(self, loop):
        '''
        Populate the state of this NemoKern object from an fparser2
        AST for an implicit loop (Fortran array syntax).

        :param loop: Node in the fparser2 AST representing an implicit loop.
        :type loop: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        '''
        # TODO implement this method!
        self._kernel_type = "Implicit"
        self._loop = loop

    def local_vars(self):
        '''
        :returns: list of the variable (names) that are local to this loop \
                  (and must therefore be e.g. threadprivate if doing OpenMP)
        :rtype: list of str
        '''
        return []

    def view(self, indent=0):
        '''
        Print representation of this node to stdout.
        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[" +
              self.ktype + "]")


class NemoLoop(Loop, ASTProcessor):
    '''
    Class representing a Loop in NEMO.

    :param ast: node in the fparser2 AST representing the loop.
    :type ast: :py:class:`fparser.two.Block_Nonlabel_Do_Construct`
    :param parent: parent of this NemoLoop in the PSyclone AST.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, ast, parent=None):
        from fparser.two.Fortran2003 import Loop_Control
        from fparser.two.utils import BinaryOpBase
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

        # Get the loop variable
        ctrl = walk_ast(ast.content, [Loop_Control])

        # If this is a DO WHILE then the first element of items will be a
        # scalar logical expression. (See
        # `fparser.two.Fortran2003.Loop_Control`.) The `match` method should
        # have already rejected such loops so we should never get to here.
        if isinstance(ctrl[0].items[0], BinaryOpBase):
            raise NotImplementedError(
                "Cannot create a NemoLoop for a DO WHILE")

        # Second element of items member of Loop Control is itself a tuple
        # containing:
        #   Loop variable, [start value expression, end value expression, step
        #   expression]
        # Loop variable will be an instance of Fortran2003.Name
        loop_var = str(ctrl[0].items[1][0])
        self._variable_name = str(loop_var)

        # Identify the type of loop
        if self._variable_name in NEMO_LOOP_TYPE_MAPPING:
            self.loop_type = NEMO_LOOP_TYPE_MAPPING[self._variable_name]
        else:
            self.loop_type = "unknown"

        # Get the loop limits. These are given in a list which is the second
        # element of a tuple which is itself the second element of the items
        # tuple:
        # (None, (Name('jk'), [Int_Literal_Constant('1', None), Name('jpk'),
        #                      Int_Literal_Constant('1', None)]), None)
        limits_list = ctrl[0].items[1][1]
        self._start = str(limits_list[0])
        self._stop = str(limits_list[1])
        if len(limits_list) == 3:
            self._step = str(limits_list[2])
        else:
            # Default loop increment is 1
            self._step = "1"

        # Is this loop body a kernel?
        if NemoKern.match(self._ast):
            self.addchild(NemoKern(self._ast, parent=self))
            return
        # It's not - walk on down the AST...
        self.process_nodes(self, self._ast.content, self._ast)

    def __str__(self):
        result = ("NemoLoop[" + self._loop_type + "]: " + self._variable_name +
                  "=" + ",".join([self._start, self._stop, self._step]) + "\n")
        for entity in self._children:
            result += str(entity) + "\n"
        result += "EndLoop"
        return result

    @staticmethod
    def match(node):
        '''
        Tests the supplied node to see whether it is a recognised form of
        NEMO loop.

        :param node: the node in the fparser2 AST to test for a match.
        :type node: :py:class:`fparser.two.utils.Base`

        :returns: True if the node represents a recognised form of loop, \
                  False otherwise.
        :rtype: bool
        '''
        from fparser.two.utils import BinaryOpBase
        if not isinstance(node, Fortran2003.Block_Nonlabel_Do_Construct):
            return False
        ctrl = walk_ast(node.content, my_types=[Fortran2003.Loop_Control])
        if not ctrl:
            return False
        if ctrl[0].items[0]:
            # If this is a DO WHILE then the first element of items will be a
            # scalar logical expression. (See
            # `fparser.two.Fortran2003.Loop_Control`.)
            # TODO DO WHILE's are currently just put into CodeBlocks.
            return False
        return True

    @property
    def kernel(self):
        '''
        :returns: the kernel object if one is associated with this loop, \
                  None otherwise.
        :rtype: :py:class:`psyclone.nemo.NemoKern` or None

        :raises NotImplementedError: if the loop contains >1 kernel.
        '''
        kernels = self.walk(self.children, NemoKern)
        if kernels:
            # TODO cope with case where loop contains >1 kernel (e.g.
            # following loop fusion)
            if len(kernels) > 1:
                raise NotImplementedError(
                    "Kernel getter method does not yet support a loop "
                    "containing more than one kernel but this loop contains "
                    "{0}".format(len(kernels)))
            return kernels[0]
        return None


class NemoImplicitLoop(NemoLoop):
    '''
    Class representing an implicit loop in NEMO (i.e. using Fortran array
    syntax).

    :param ast: the part of the fparser2 AST representing the loop.
    :type ast: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
    :param parent: the parent of this Loop in the PSyIRe.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, ast, parent=None):
        from fparser.common.readfortran import FortranStringReader
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied node in the fparser2 AST represents
        an implicit loop (using Fortran array syntax).
        :param node: node in the fparser2 AST to check
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :returns: True if the node does represet an implicit loop.
        :rtype: bool
        '''
        if not isinstance(node, Fortran2003.Assignment_Stmt):
            return False
        # We are expecting something like:
        #    array(:,:,jk) = some_expression
        # but we have to beware of cases like the following:
        #   array(1,:,:) = a_func(array2(:,:,:), mask(:,:))
        # where a_func is an array-valued function and `array2(:,:,:)`
        # could just be `array2`.
        # We check the left-hand side...
        lhs = node.items[0]
        if not isinstance(lhs, Fortran2003.Part_Ref):
            # LHS is not an array reference
            return False
        colons = walk_ast(lhs.items, [Fortran2003.Subscript_Triplet])
        if not colons:
            # LHS does not use array syntax
            return False
        # What rank is the array?
        if len(lhs.items) < 2:
            # It's an array of rank 1 so we don't know what extent it has
            return False
        # Now check the right-hand side...
        rhs = node.items[2]
        if not hasattr(rhs, 'items'):
            return True
        colons = walk_ast(rhs.items, [Fortran2003.Subscript_Triplet])
        if not colons:
            # We just have array syntax on the LHS
            return True
        # Check that we haven't got array syntax used within the index
        # expression to another array. Array references are represented by
        # Part_Ref nodes in the fparser2 AST. This would be easier to do
        # if the fparser2 AST carried parent information with each node.
        # As it is we have to walk down the tree rather than come back up
        # from each colon.
        # Find all array references
        array_refs = []
        if isinstance(rhs, Fortran2003.Part_Ref):
            # Since walk_ast is slightly clunky we have to manually allow
            # for the top-level "rhs" node being an array reference
            array_refs.append(rhs)
        array_refs += walk_ast(rhs.items, [Fortran2003.Part_Ref])
        for ref in array_refs:
            nested_refs = walk_ast(ref.items, [Fortran2003.Part_Ref])
            # Do any of these nested array references use array syntax?
            for nested_ref in nested_refs:
                colons = walk_ast(nested_ref.items,
                                  [Fortran2003.Subscript_Triplet])
                if colons:
                    return False
        return True


class NemoIfBlock(IfBlock, ASTProcessor):
    '''
    Represents an if-block within a NEMO Schedule.
    Within the fparser2 AST, an if-block is represented as:
      If_Then_Stmt
      statement(s)
      Else_Stmt
      further statement(s)
      End_If_Stmt
    i.e. the statements contained inside the if-block are siblings
    of the control statements, not children of them.

    :param ast: reference to fparser2 AST representing if block.
    :type ast: :py:class:`fparser.two.Fortran2003.If_Construct`
    :param parent: parent node of this if block in the PSyIRe.
    :type parent: :py:class:`psyclone.psyGen.Node`

    :raises InternalError: if the fparser2 AST does not have the expected \
                           structure.
    '''
    def __init__(self, ast, parent=None):
        super(NemoIfBlock, self).__init__(parent=parent)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast
        # Check that the fparser2 AST has the expected structure. We have to
        # allow for Comment nodes.
        found_if_then = False
        for start_idx, child in enumerate(ast.content):
            if isinstance(child, Fortran2003.If_Then_Stmt):
                found_if_then = True
                break
        if not found_if_then:
            raise InternalError("Failed to find opening if then statement: "
                                "{0}".format(str(ast)))
        end_idx = -1
        for child in reversed(ast.content):
            if isinstance(child, Fortran2003.End_If_Stmt):
                end_idx = ast.content.index(child)
                break
        if end_idx == -1:
            raise InternalError("Failed to find closing end if statement: {0}".
                                format(str(ast)))

        clause_indices = []
        for idx, child in enumerate(ast.content):
            child._parent = self._ast  # Retrofit parent info
            if isinstance(child, (Fortran2003.If_Then_Stmt,
                                  Fortran2003.Else_Stmt,
                                  Fortran2003.Else_If_Stmt,
                                  Fortran2003.End_If_Stmt)):
                clause_indices.append(idx)
        # Create the body of the main If
        end_idx = clause_indices[1]
        self._condition = str(ast.content[start_idx].items[0])
        self.process_nodes(parent=self,
                           nodes=ast.content[start_idx+1:end_idx],
                           nodes_parent=ast)
        # Now deal with any other clauses (i.e. "else if" or "else")
        # An If block has one fewer clauses than it has control statements
        # (c.f. panels and posts):
        num_clauses = len(clause_indices) - 1
        for idx in range(1, num_clauses):
            start_idx = clause_indices[idx]
            # No need to subtract 1 here as Python's slice notation means
            # that the end_idx'th element is excluded
            end_idx = clause_indices[idx+1]
            ast.content[start_idx]._parent = ast  # Retrofit parent info
            self.addchild(NemoIfClause(ast.content[start_idx:end_idx],
                                       parent=self))

    def gen_code(self):
        '''
        Override abstract method of base class.
        :raises InternalError: because is not relevant to this API.
        '''
        # If we get here it's an error as the NEMO API does not generate
        # code (we manipulate existing code instead).
        raise InternalError("this method should not have been called!")

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied fparser2 AST represents an if-block
        that must be represented in the Schedule AST. If-blocks that do
        not contain kernels are just treated as code blocks.

        :param node: the node in the fparser2 AST representing an if-block
        :type node: :py:class:`fparser.two.Fortran2003.If_Construct`
        :returns: True if this if-block must be represented in the PSyIRe
        :rtype: bool

        '''
        if not isinstance(node, Fortran2003.If_Construct):
            return False

        # We only care about if-blocks if they contain something significant
        # i.e. a recognised type of loop (whether implicit or explicit).
        loops = walk_ast(node.content,
                         [Fortran2003.Subscript_Triplet,
                          Fortran2003.Block_Nonlabel_Do_Construct])
        if loops:
            return True
        return False


class NemoIfClause(IfClause, ASTProcessor):
    '''
    Represents a sub-clause of an if-block (else-if or else).

    :param list ast_nodes: List of nodes making up the clause in the fparser2 \
                           AST. First node is the else/else-if statement \
                           itself.
    :param parent: Parent of this clause in the PSyIRe (must be an IfBlock).
    :type parent: :py:class:`psyclone.nemo.NemoIfBlock`

    :raises InternalError: if fparser2 AST doesn't have the expected structure.
    '''
    def __init__(self, ast_nodes, parent=None):
        super(NemoIfClause, self).__init__(parent=parent)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast_nodes[0]
        self._ast_start = ast_nodes[0]
        self._ast_end = ast_nodes[-1]
        # Store what type of clause we are
        if isinstance(ast_nodes[0], Fortran2003.Else_Stmt):
            self._clause_type = "Else"
        elif isinstance(ast_nodes[0], Fortran2003.Else_If_Stmt):
            self._clause_type = "Else If"
        else:
            raise InternalError(
                "Unrecognised member of if block: '{0}'. Expected one of "
                "Else_Stmt or Else_If_Stmt.".format(type(ast_nodes[0])))
        # Continue on down the AST
        self.process_nodes(parent=self,
                           nodes=ast_nodes[1:],
                           nodes_parent=self._ast._parent)

    def gen_code(self):
        '''
        Override abstract method of base class.
        :raises InternalError: because is not relevant to this API.
        '''
        # If we get here it's an error as the NEMO API does not generate
        # code (we manipulate existing code instead).
        raise InternalError("This method should not have been called!")

class NemoACCEnterDataDirective(ACCEnterDataDirective):
    ''' '''
    def data_on_device(self, parent):
        ''' '''
        return

    def update(self):
        ''' '''
        start_text = "!$ACC DATA"
        self.add_region(start_text, start_index=0)
