# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified by A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.fortran module'''

from __future__ import absolute_import

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.backend.base import VisitorError
from psyclone.psyir.backend.fortran import gen_intent, gen_dims, FortranWriter
from psyclone.psyGen import Symbol, Node, CodeBlock, Container, SymbolTable
from psyclone.tests.utilities import create_schedule
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


@pytest.fixture(scope="function", name="fort_writer")
def fixture_fort_writer():
    '''Create and return a FortranWriter object with default settings.'''
    return FortranWriter()


def test_gen_intent():
    '''Check the gen_intent function produces the expected intent
    strings.

    '''
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    assert gen_intent(symbol) is None
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(Symbol.Access.READ))
    assert gen_intent(symbol) == "in"
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(Symbol.Access.WRITE))
    assert gen_intent(symbol) == "out"
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(Symbol.Access.READWRITE))
    assert gen_intent(symbol) == "inout"


def test_gen_intent_error(monkeypatch):
    '''Check the gen_intent function raises an exception if an unsupported
    access type is found.

    '''
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    monkeypatch.setattr(symbol.interface, "_access", "UNSUPPORTED")
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_intent(symbol)
    assert "Unsupported access ''UNSUPPORTED'' found." in str(excinfo)


def test_gen_dims():
    '''Check the gen_dims function produces the expected dimension
    strings.

    '''
    arg = Symbol("arg", "integer",
                 interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    symbol = Symbol("dummy", "integer", shape=[arg, 2, None],
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    assert gen_dims(symbol) == ["arg", "2", ":"]


def test_gen_dims_error(monkeypatch):
    '''Check the gen_dims function raises an exception if a symbol shape
    entry is not supported.

    '''
    symbol = Symbol("dummy", "integer",
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    monkeypatch.setattr(symbol, "_shape", ["invalid"])
    with pytest.raises(NotImplementedError) as excinfo:
        _ = gen_dims(symbol)
    assert "unsupported gen_dims index 'invalid'" in str(excinfo)


def test_fw_gen_use(fort_writer):
    '''Check the FortranWriter class gen_use method produces the expected
    declaration. Also check that an exception is raised if the symbol
    does not describe a use statement.

    '''
    symbol = Symbol("dummy1", "deferred",
                    interface=Symbol.FortranGlobal("my_module"))
    result = fort_writer.gen_use(symbol)
    assert result == "use my_module, only : dummy1\n"

    symbol = Symbol("dummy1", "integer")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_use(symbol)
    assert ("gen_use() requires the symbol interface for symbol 'dummy1' to "
            "be a FortranGlobal instance but found 'NoneType'."
            in str(excinfo.value))


def test_fw_gen_vardecl(fort_writer):
    '''Check the FortranWriter class gen_vardecl method produces the
    expected declarations. Also check that an exception is raised if
    the symbol does not describe a variable declaration statement.

    '''
    # Basic entry
    symbol = Symbol("dummy1", "integer")
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer :: dummy1\n"

    # Array with intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.READ))
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(2,:,2), intent(in) :: dummy2\n"

    # Array with unknown intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, dimension(2,:,2) :: dummy2\n"

    # Constant
    symbol = Symbol("dummy3", "integer", constant_value=10)
    result = fort_writer.gen_vardecl(symbol)
    assert result == "integer, parameter :: dummy3 = 10\n"

    # Use statement
    symbol = Symbol("dummy1", "deferred",
                    interface=Symbol.FortranGlobal("my_module"))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_vardecl(symbol)
    assert ("gen_vardecl requires the symbol 'dummy1' to be a local "
            "declaration or an argument declaration, but found scope "
            "'global' and interface 'FortranGlobal'." in str(excinfo.value))


def test_gen_decls(fort_writer):
    '''Check the FortranWriter class gen_decls method produces the
    expected declarations. Also check that an exception is raised if
    an 'argument' symbol exists in the supplied symbol table and the
    optional argument 'args_allowed' is set to False.

    '''
    symbol_table = SymbolTable()
    use_statement = Symbol("my_use", "deferred",
                           interface=Symbol.FortranGlobal("my_module"))
    symbol_table.add(use_statement)
    argument_variable = Symbol("arg", "integer", interface=Symbol.Argument())
    symbol_table.add(argument_variable)
    local_variable = Symbol("local", "integer")
    symbol_table.add(local_variable)
    result = fort_writer.gen_decls(symbol_table)
    assert (result ==
            "use my_module, only : my_use\n"
            "integer :: arg\n"
            "integer :: local\n")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.gen_decls(symbol_table, args_allowed=False)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['arg']'." in str(excinfo.value))


def test_fw_exception(fort_writer):
    '''Check the FortranWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a,b,c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # pylint: disable=abstract-method
    # modify the reference to b to be something unsupported
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the Fortran visitor.'''
    # pylint: enable=abstract-method

    unsupported = Unsupported()
    assignment = schedule[0]
    binary_operation = assignment.rhs
    # The assignment.rhs method has no setter so access the reference
    # directly instead via children.
    assignment.children[1] = unsupported
    unsupported.children = binary_operation.children

    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unsupported node 'Unsupported' found" in str(excinfo)


def test_fw_container_1(fort_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a Container
    node with no content is found. Also tests that an exception is
    raised if Container.name does not have a value.

    '''
    container = Container("test")
    result = fort_writer(container)
    assert (
        "module test\n\n"
        "  contains\n\n"
        "end module test\n" in result)

    monkeypatch.setattr(container, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert "Expected Container node name to have a value." in str(excinfo)


def test_fw_container_2(fort_writer):
    '''Check the FortranWriter class outputs correct code when a Container
    node is found with a subroutine, use statements and
    declarations. Also raise an exception if the Container contains a
    Container.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "use test2_mod, only : a,b\n"
        "real :: c,d\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    container = schedule.root

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(container)

    assert (
        "module test\n"
        "  use test2_mod, only : a\n"
        "  use test2_mod, only : b\n"
        "  real :: c\n"
        "  real :: d\n\n"
        "  contains\n"
        "  subroutine tmp()\n\n\n"
        "  end subroutine tmp\n\n"
        "end module test\n" in result)

    container.children.append(Container("child", parent=container))
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("The Fortran back-end requires all children of a Container "
            "to be KernelSchedules." in str(excinfo))


def test_fw_container_3(fort_writer, monkeypatch):
    '''Check the FortranWriter class raises an exception when a Container
    node contains a symbol table with an argument declaration (as this
    does not make sense).

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "real :: a\n"
        "contains\n"
        "subroutine tmp()\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    container = schedule.root
    symbol = container.symbol_table.symbols[0]
    assert symbol.name == "a"
    monkeypatch.setattr(symbol, "_interface", Symbol.Argument())

    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(container)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['a']'." in str(excinfo))


def test_fw_kernelschedule(fort_writer, monkeypatch):
    '''Check the FortranWriter class outputs correct code when a
    KernelSchedule node is found. Also tests that an exception is
    raised if KernelSchedule.name does not have a value.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,b,c)\n"
        "  use my_mod, only : d\n"
        "  real, intent(out) :: a(:)\n"
        "  real, intent(in) :: b(:)\n"
        "  integer, intent(in) :: c\n"
        "  a = b/c\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)

    assert(
        "subroutine tmp(a,b,c)\n"
        "  use my_mod, only : d\n"
        "  real, dimension(:), intent(out) :: a\n"
        "  real, dimension(:), intent(in) :: b\n"
        "  integer, intent(in) :: c\n"
        "\n"
        "  a=b / c\n"
        "\n"
        "end subroutine tmp\n") in result

    monkeypatch.setattr(schedule, "_name", None)
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Expected node name to have a value." in str(excinfo)

# assignment and binaryoperation (not intrinsics) are already checked
# within previous tests


def test_fw_binaryoperator(fort_writer):
    '''Check the FortranWriter class binary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sign
    as the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sign(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=SIGN(1.0, 1.0)" in result


def test_fw_binaryoperator_unknown(fort_writer, monkeypatch):
    '''Check the FortranWriter class binary_operation method raises an
    exception if an unknown binary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sign(1.0,1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove sign() from the list of supported binary operators
    monkeypatch.delitem(Fparser2Reader.binary_operators, "sign")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unexpected binary op" in str(excinfo)


def test_fw_naryopeator(fort_writer):
    ''' Check that the FortranWriter class nary_operation method correctly
    prints out the Fortran representation of an intrinsic.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=MAX(1.0, 1.0, 2.0)" in result


def test_fw_naryopeator_unknown(fort_writer, monkeypatch):
    ''' Check that the FortranWriter class nary_operation method raises
    the expected error if it encounters an unknown operator.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a\n"
        "    a = max(1.0,1.0,2.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove max() from the list of supported nary operators
    monkeypatch.delitem(Fparser2Reader.nary_operators, "max")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as err:
        _ = fort_writer(schedule)
    assert "Unexpected N-ary op" in str(err)


def test_fw_reference(fort_writer):
    '''Check the FortranWriter class reference method prints the
    appropriate information (the name of the reference it points to).
    Also check the method raises an exception if it has children as
    this is not expected.

    '''
    # Generate fparser2 parse tree from Fortran code. The line of
    # interest is a(n) = 0.0. The additional a=1 line is added to get
    # round a bug in the parser.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = 1\n"
        "    a(n) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)

    # The asserts need to be split as the declaration order can change
    # between different versions of Python.
    assert (
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, dimension(n), intent(out) :: a\n"
        "\n"
        "  a=1\n"
        "  a(n)=0.0\n"
        "\n"
        "end subroutine tmp\n") in result

    # Now add a child to the reference node
    reference = schedule[1].lhs.children[0]
    reference.children = ["hello"]

    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        result = fort_writer(schedule)
    assert "Expecting a Reference with no children but found" in str(excinfo)


def test_fw_array(fort_writer):
    '''Check the FortranWriter class array method correctly prints
    out the Fortran representation of an array

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,3) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a(2,n,3)=0.0" in result

# literal is already checked within previous tests


def test_fw_ifblock(fort_writer):
    '''Check the FortranWriter class ifblock method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(inout) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    if (n.gt.2) then\n"
        "      n=n+1\n"
        "    end if\n"
        "    if (n.gt.4) then\n"
        "      a = -1\n"
        "    else\n"
        "      a = 1\n"
        "    end if\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert (
        "  if (n > 2) then\n"
        "    n=n + 1\n"
        "  end if\n"
        "  if (n > 4) then\n"
        "    a=-1\n"
        "  else\n"
        "    a=1\n"
        "  end if\n") in result


def test_fw_loop(fort_writer):
    '''Check the FortranWriter class loop method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: i, sum\n"
        "  sum = 0\n"
        "  do i = 1, 20, 2\n"
        "    sum = sum + i\n"
        "  end do\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "do i = 1, 20, 2\n" in result


def test_fw_unaryoperator(fort_writer):
    '''Check the FortranWriter class unary_operation method
    correctly prints out the Fortran representation. Uses -1 as the
    example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = -1\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=-1" in result


def test_fw_unaryoperator2(fort_writer):
    '''Check the FortranWriter class unary_operation method correctly
    prints out the Fortran representation of an intrinsic. Uses sin as
    the example.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a=SIN(1.0)" in result


def test_fw_unaryoperator_unknown(fort_writer, monkeypatch):
    '''Check the FortranWriter class unary_operation method raises an
    exception if an unknown unary operator is found.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n)\n"
        "    a = sin(1.0)\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Remove sin() from the dict of unary operators
    monkeypatch.delitem(Fparser2Reader.unary_operators, "sin")
    # Generate Fortran from the PSyIR schedule
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer(schedule)
    assert "Unexpected unary op" in str(excinfo)


def test_fw_return(fort_writer):
    '''Check the FortranWriter class return method
    correctly prints out the Fortran representation.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  return\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "  return\n" in result


def test_fw_codeblock_1(fort_writer):
    '''Check the FortranWriter class codeblock method correctly
    prints out the Fortran code contained within it.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: a\n"
        "  a=1\n"
        "  print *,\"I am a code block\"\n"
        "  print *,\"with more than one line\"\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")
    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)
    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert (
        "  a=1\n"
        "  PRINT *, \"I am a code block\"\n"
        "  PRINT *, \"with more than one line\"\n" in result)


def test_fw_codeblock_2(fort_writer):
    '''Check the FortranWriter class codeblock method correctly prints out
    the Fortran representation when there is a code block that is part
    of a line (not a whole line). In this case the ":" in the array
    access is a code block.

    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a,n)\n"
        "  integer, intent(in) :: n\n"
        "  real, intent(out) :: a(n,n,n)\n"
        "    a(2,n,:) = 0.0\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code, "tmp")

    # Check a code block exists in the schedule
    assert schedule.walk(CodeBlock)

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "a(2,n,:)=0.0" in result


def test_fw_codeblock_3(fort_writer):
    '''Check the FortranWriter class codeblock method raises the expected
    exception if an unsupported CodeBlock structure value is found.

    '''
    code_block = CodeBlock([], "unsupported")
    with pytest.raises(VisitorError) as excinfo:
        _ = fort_writer.codeblock_node(code_block)
    assert ("Unsupported CodeBlock Structure 'unsupported' found."
            in str(excinfo.value))


def get_nemo_schedule(parser, code):
    '''Utility function that returns the first schedule for a code with
    the nemo api.

    :param parser: the parser class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`
    :param str code: the code as a string.

    :returns: the first schedule in the supplied code.
    :rtype: :py:class:`psyclone.nemo.NemoInvokeSchedule`

    '''
    from psyclone.psyGen import PSyFactory
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(api="nemo").create(prog)
    return psy.invokes.invoke_list[0].schedule


def test_fw_nemoinvokeschedule(fort_writer, parser):
    '''Check that the FortranWriter class nemoinvokeschedule accepts the
    NemoInvokeSchedule node and prints the expected code (from any
    children of the node as the node itself simply calls its
    children).

    '''
    from psyclone.nemo import NemoInvokeSchedule
    code = (
        "program test\n"
        "  a=1\n"
        "end program test\n")
    schedule = get_nemo_schedule(parser, code)
    assert isinstance(schedule, NemoInvokeSchedule)
    result = fort_writer(schedule)
    assert "a=1\n" in result


def test_fw_nemokern(fort_writer, parser):
    '''Check the FortranWriter class nemokern method prints the
    class information and calls any children. This method is used to
    output nothing for a NemoKern object and simply call its children
    as NemoKern is a collection of PSyIR nodes so needs no
    output itself.

    '''
    from psyclone.nemo import NemoKern
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "program test\n"
        "  integer :: a,b,c\n"
        "  do k=1,n\n"
        "    do j=1,n\n"
        "      do i=1,n\n"
        "        a(i,j,k) = 0.0\n"
        "      end do\n"
        "    end do\n"
        "  end do\n"
        "end program test")
    schedule = get_nemo_schedule(parser, code)

    kernel = schedule[0].loop_body[0].loop_body[0].loop_body[0]
    assert isinstance(kernel, NemoKern)

    result = fort_writer(schedule)
    assert (
        "    do i = 1, n, 1\n"
        "      a(i,j,k)=0.0\n"
        "    enddo\n" in result)


def test_fw_nemoimplicitloop(fort_writer, parser):
    '''Check that the FortranWriter class nemoimplicitloop accepts the
    NemoImplicitLoop node and prints the expected code.

    '''
    from psyclone.nemo import NemoImplicitLoop
    code = (
        "program test\n"
        "  real a(10,10,10)\n"
        "  a(:,:,:)=0.0\n"
        "end program test\n")
    schedule = get_nemo_schedule(parser, code)
    implicit_loop = schedule[0]
    assert isinstance(implicit_loop, NemoImplicitLoop)
    result = fort_writer(schedule)
    assert "a(:, :, :) = 0.0\n" in result


def test_fw_size(fort_writer):
    ''' Check that the FortranWriter outputs a SIZE intrinsic call. '''
    code = ("module test_mod\n"
            "contains\n"
            "subroutine test_kern(a)\n"
            "  real, intent(in) :: a(:,:)\n"
            "  integer :: mysize\n"
            "  mysize = size(a, 2)\n"
            "end subroutine test_kern\n"
            "end module test_mod\n")
    schedule = create_schedule(code, "test_kern")

    # Generate Fortran from the PSyIR schedule
    result = fort_writer(schedule)
    assert "mysize=size(a, 2)" in result.lower()
