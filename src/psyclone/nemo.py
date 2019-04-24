# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''This module implements the PSyclone NEMO API by specialising
   the required base classes for both code generation (PSy, Invokes,
   Invoke, InvokeSchedule, Loop, Kern, Arguments and KernelArgument)
   and parsing (Descriptor and KernelType).

'''

from __future__ import print_function, absolute_import
import copy
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, Node, \
    Loop, Kern, InternalError, IfBlock, NameSpaceFactory, \
    Fparser2ASTProcessor, SCHEDULE_COLOUR_MAP as _BASE_CMAP
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


class NemoFparser2ASTProcessor(Fparser2ASTProcessor):
    '''
    Specialisation of Fparser2ASTProcessor for the Nemo API. It is used
    as a Mixin in the Nemo API.
    '''
    def _create_child(self, child, parent=None):
        '''
        Adds Nemo API specific processors for certain fparser2 types
        before calling the parent _create_child.

        :param child: node in fparser2 AST.
        :type child:  :py:class:`fparser.two.utils.Base`
        :param parent: Parent node in the PSyclone IR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :return: Returns the PSyIRe representation of child.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        if NemoLoop.match(child):
            return NemoLoop(child, parent=parent)
        elif isinstance(child, Fortran2003.Nonlabel_Do_Stmt):
            pass
        elif NemoImplicitLoop.match(child):
            return NemoImplicitLoop(child, parent=parent)
        else:
            return super(NemoFparser2ASTProcessor,
                         self)._create_child(child, parent=parent)


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
        # A temporary workaround for the fact that we don't yet have a
        # symbol table to store information on the variable declarations.
        # TODO (#255) remove this workaround.
        self._loop_vars = []
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

        # We now walk through the AST produced by fparser2 and construct a
        # new AST using objects from the nemo module.
        self._schedule = NemoInvokeSchedule(self, exe_part)

    def update(self):
        '''
        Updates the fparser2 parse tree associated with this schedule to
        make it reflect any transformations that have been applied to
        the PSyclone PSyIR.
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


class NemoInvokeSchedule(InvokeSchedule, NemoFparser2ASTProcessor):
    '''
    The NEMO-specific InvokeSchedule sub-class. This is the top-level node in
    PSyclone's IR of a NEMO program unit (program, subroutine etc).

    :param invoke: The Invoke to which this NemoInvokeSchedule belongs.
    :type invoke: :py:class:`psyclone.nemo.NemoInvoke`
    :param ast: the fparser2 AST of the NEMO code for which to generate \
                a NemoInvokeSchedule.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.

    '''
    def __init__(self, invoke, ast):
        Node.__init__(self)
        NemoFparser2ASTProcessor.__init__(self)

        self._invoke = invoke
        self._ast = ast
        self.process_nodes(self, ast.content, ast)

    def view(self, indent=0):
        '''
        Print a representation of this NemoInvokeSchedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this NemoInvokeSchedule. '''
        result = "NemoInvokeSchedule():\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result


class NemoKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. Kernels are leaves in the PSyIR (i.e. they have
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
        self._parent = parent
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
            Block_Nonlabel_Do_Construct, Write_Stmt, Read_Stmt
        child_loops = walk_ast(node.content,
                               [Block_Nonlabel_Do_Construct, Write_Stmt,
                                Read_Stmt])
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

        if not isinstance(loop.content[0], Nonlabel_Do_Stmt):
            raise InternalError("Expecting Nonlabel_Do_Stmt as first child "
                                "of Block_Nonlabel_Do_Construct but "
                                "got {0}".format(type(loop.content[0])))
        self._body = []
        for content in loop.content[1:]:
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

    @property
    def ast(self):
        '''
        Override the default ast method as, for the NEMO API, we don't need
        to take any special action to get hold of the parse tree for the
        kernel.

        :returns: a reference to that part of the fparser2 parse tree that \
                  describes this kernel.
        :rtype: sub-class of :py:class:`fparser.two.utils.Base`
        '''
        return self._ast

class NemoLoop(Loop, NemoFparser2ASTProcessor):
    '''
    Class representing a Loop in NEMO.

    :param ast: node in the fparser2 AST representing the loop.
    :type ast: :py:class:`fparser.two.Block_Nonlabel_Do_Construct`
    :param parent: parent of this NemoLoop in the PSyclone AST.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, ast, parent=None):
        from fparser.two.Fortran2003 import Loop_Control
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        NemoFparser2ASTProcessor.__init__(self)
        # Keep a ptr to the corresponding node in the parse tree
        self._ast = ast

        # Get the loop variable
        ctrl = walk_ast(ast.content, [Loop_Control])
        # If this is a DO WHILE then the first element of items will
        # not be None. The `match` method should have already rejected
        # such loops so we should never get to here.
        if ctrl[0].items[0]:
            raise InternalError("NemoLoop constructor should not have been "
                                "called for a DO WHILE")

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

    @staticmethod
    def match(node):
        '''
        Tests the supplied node to see whether it is a recognised form of
        NEMO loop.

        :param node: the node in the fparser2 parse tree to test for a match.
        :type node: :py:class:`fparser.two.utils.Base`

        :returns: True if the node represents a recognised form of loop, \
                  False otherwise.
        :rtype: bool

        :raises InternalError: if the parse tree represents a loop but no \
                               Loop_Control element is present.

        '''
        if not isinstance(node, Fortran2003.Block_Nonlabel_Do_Construct):
            return False
        ctrl = walk_ast(node.content, my_types=[Fortran2003.Loop_Control])
        if not ctrl:
            raise InternalError("Unrecognised form of DO loop - failed to "
                                "find Loop_Control element in parse tree.")
        if ctrl[0].items[0]:
            # If this is a DO WHILE then the first element of items will not
            # be None. (See `fparser.two.Fortran2003.Loop_Control`.)
            # TODO #359 DO WHILE's are currently just put into CodeBlocks
            # rather than being properly described in the PSyIR.
            return False
        return True

    def __str__(self):
        result = ("NemoLoop[" + self._loop_type + "]: " + self._variable_name +
                  "=" + ",".join([self._start, self._stop, self._step]) + "\n")
        for entity in self._children:
            result += str(entity) + "\n"
        result += "EndLoop"
        return result

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
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

    def __str__(self):
        # Display the LHS of the assignment in the str representation
        return "NemoImplicitLoop[{0}]\n".format(self._ast.items[0])

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied node in the fparser2 AST represents
        an implicit loop (using Fortran array syntax).

        :param node: node in the fparser2 AST to check
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :returns: True if the node does represent an implicit loop.
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
        # Now check the right-hand side...
        rhs = node.items[2]
        try:
            if not walk_ast(rhs.items, [Fortran2003.Subscript_Triplet]):
                # We don't have any array syntax on the RHS
                return True
        except AttributeError:
            # The RHS doesn't have the `items` attribute (it may be just
            # a Name for instance).
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

