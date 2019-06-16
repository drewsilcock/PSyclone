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


from __future__ import print_function, absolute_import

from psyclone.nemo import NemoLoop
from psyclone.core.access_info import VariablesAccessInfo
from psyclone.core.access_type import AccessType
from psyclone.psyGen import Assignment, CodeBlock, IfBlock, Loop
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans


def can_be_merged(loop1, loop2):

    var_accesses1 = VariablesAccessInfo()
    loop1.reference_accesses(var_accesses1)
    vars1 = var_accesses1.get_all_vars()
    var_accesses2 = VariablesAccessInfo()
    loop2.reference_accesses(var_accesses2)
    vars2 = var_accesses1.get_all_vars()

    for var1 in vars1:
        var_info1 = var_accesses1.get_varinfo(var1)
        if not var_info1.is_array():
            # We need to figure out what to do with shared/private
            # scalar variables :()
            continue
        try:
            var_info2= var_accesses2.get_varinfo(var1)
        except KeyError:
            continue
        if not var_info2.is_array():
            print("OOps, what is this? {0} used as array and scalar".format(var1))

        # Check if this variable is written in loop 1 and read in loop2:
        if var_info1.is_written() and var_info2.is_read():
            return (False, "{0} is written in loop1 and read in loop 2"
                .format(var_info1.get_var_name()))

        # Check if this variable is written in loop 2 and read in loop1:
        if var_info1.is_read() and var_info2.is_written():
            return (False, "{0} is read in loop1 and wrtten in loop 2"
                .format(var_info1.get_var_name()))

        # Check if this variable is written in both loops
        if var_info1.is_written() and var_info2.is_written():
            return (False, "{0} is written in both loops"
                .format(var_info1.get_var_name()))

    return (True, "")

def trans(psy):
    print("Invokes found:")
    print(psy.invokes.names)

    for subroutine in psy.invokes.names:
        sched = psy.invokes.get(subroutine).schedule

        all_loops =[]
        for l in sched.loops():
            if isinstance(l.children[0], NemoLoop):
                all_loops.append(l)

        for i, loop in enumerate(all_loops[:-1]):
            result, message = can_be_merged(loop, all_loops[i+1])
            if not result:
                print("Loop", i, "can not be swapped:", message, loop, "---", all_loops[i+1])
            else:
                print("Loop", i, "can be swapped.", loop, "---", all_loops[i+1])
            print()

