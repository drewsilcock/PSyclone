#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# -----------------------------------------------------------------------------
# Authors: Joerg Henrichs, Bureau of Meteorology


from fparser.two.utils import walk_ast

from psyclone.nemo import NemoKern, NemoLoop
from psyclone.core.access_info import VariablesAccessInfo
from psyclone.psyGen import Assignment, CodeBlock, IfBlock, Loop, TransInfo
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans, \
    ProfileRegionTrans


def is_blockable(node):
    '''Returns true if the node (and all included nodes in the subtree)
    can become part of one omp parallel block
    :param node: The node containing the statement(s) to be checked.
    :type node: :py:class:`psyclone.psyGen.Node`
    '''

    # Loops and Assignments can be executed in parallel (if a loop can
    # not be parallelised, we can add a omp serial directive, but it does
    # not prevent a larger omp parallel block from being created.
    if isinstance(node, Assignment):
        return True
    if isinstance(node, NemoLoop):
        return node.loop_type == "lat"

    # Likely a function call. This must be outside of an omp block, since
    # the called function itself might be omp parallelised.
    if isinstance(node, CodeBlock):
        return False

    # If blocks can be used, if all parts of the if and else bodies can
    # be within an omp block.
    if isinstance(node, IfBlock):
        # if statements can be used if all their statements are blockable
        for child in node.if_body:
            if not is_blockable(child):
                return False
        # The if body is fine, the whole if statement can be enclosed,
        # if the else body can be blocked as well (or is empty of course)
        for child in node.else_body:
            if not is_blockable(child):
                return False
        return True

    # Anything else - don't allow it to be inside an omp block.
    return False


def collect_loop_blocks(node, blocks=None):
    '''This function collects consecutive loops into one list. The idea is that
    these loops can then (potentially) be parallelised in one omp parallel
    region with individual omp do directives.
    :param node: Subtree to analyse.
    :param blocks:
    '''

    state = "find beginning"
    loops = []
    for child in node.children:
        if state == "find beginning" and not is_blockable(child):
            continue
        state = "collecting loops"
        if state == "collecting loops" and \
                not is_blockable(child):
            state = "find beginning"
            if len(loops) > 0:
                blocks.append(loops)
            loops = []
            continue
        loops.append(child)


def is_scalar_parallelisable(var_info):
    '''
    :param var_info:
    :type var_info: :py:class:`psyclone.core.var_info.VariableInfo`
    :return: True if the scalar variable is not a reduction, i.e. it \
        can be parallelised.
    '''

    # Read only scalar variables can be parallelised
    if var_info.is_read_only():
        return True

    all_accesses = var_info.get_all_accesses()
    # The variable is used only once. Either it is a read-only variable,
    # or it is supposed to store the result from the loop to be used outside
    # of the loop (or it is bad code). Read-only access has already been
    # tested above, so it must be a write access here, which prohibits
    # parallelisation.
    if len(all_accesses) == 1:
        return False

    # Now we have at least two accesses. Check if there is any READ and WRITE
    # access to the variable at the same location - this would indicate a
    # reduction, which atm can not be parallelised. This relies on the
    # accesses sorted by location!
    for i in range(len(all_accesses)-1):
        if all_accesses[i].get_access_type() != \
                all_accesses[i+1].get_access_type() and \
                all_accesses[i].get_location() == \
                all_accesses[i+1].get_location():
            return False

    return True

def is_array_parallelisable(var_name, var_info):
    '''Tries to determine if the access pattern for a variable
    given in var_info allows parallelisation along the variable
    var_name.
    :paramstr var_name: Name of the variable that is parallelised.
    :param var_info:


def can_loop_be_parallelised(loop, loop_variable=None):
    '''Returns true if the loop can be parallelised along the
    specified variable.'''

    if loop.loop_type != "lat":
        return False
    var_accesses = VariablesAccessInfo()
    loop.reference_accesses(var_accesses)

    loop_vars = [l.variable_name for l in loop.walk([loop], Loop)]
    print(loop_vars)

    for var_name in var_accesses.get_all_vars():
        # Ignore all loop variables - they look like reductions because of
        # the write-read access in the loop/
        if var_name in loop_vars:
            continue

        var_info = var_accesses.get_varinfo(var_name)
        print("var_name", var_name, var_info.is_array(), var_info.get_all_accesses()[0].get_indices())
        if var_info.is_array():
            #print(var_name, is_array_parallelisable(var_info))
            pass
        else:
            # Handle scalar variable
            print(var_name, is_scalar_parallelisable(var_name, var_info))

    return True


def create_omp(statements):
    parallel_loop = OMPLoopTrans()
    for statement in statements:
        # Assignments can for now be just done in parallel
        if isinstance(statement, Assignment):
            continue
        if isinstance(statement, NemoLoop):
            var = statement.variable_name
            if can_loop_be_parallelised(statement, var):
                parallel_loop.apply(statement)
        if isinstance(statement, IfBlock):
            create_omp(statement.if_body)
            if statement.else_body:
                create_omp(statement.else_body)


def create_all_omp_directives(statements):
    # Any assignment statements at the beginning and end of the statement list
    # can be ignored, they do not need to be inside the omp block.

    #while statements and isinstance(statements[0], Assignment):
    #    del statements[0]

    while statements and isinstance(statements[-1], Assignment):
        del statements[-1]

    parallel = OMPParallelTrans()
    sched, _ = parallel.apply(statements)
    create_omp(statements)


def trans(psy):
    print("Invokes found:")
    print(psy.invokes.names)
    sched = psy.invokes.get('step2d_tile').schedule

    # First step: split the subroutine into blocks, each one consisting
    # of loops, which are then separated by other statements (e.g. calls)
    blocks = []
    collect_loop_blocks(sched, blocks)
    # Now create individual OMP parallel directives around those blocks:
    for statements in blocks:
        create_all_omp_directives(statements)
    #create_all_omp_directives(blocks[0])
    #create_all_omp_directives(blocks[1])
    #create_all_omp_directives(blocks[2])
    #create_all_omp_directives(blocks[3])
    #create_all_omp_directives(blocks[4])

    # psy.invokes.get('step2d_tile').schedule = sched

    # sched.view()
    print(psy.gen)
    return psy
