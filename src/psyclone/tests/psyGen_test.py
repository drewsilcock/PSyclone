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
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the psygen module '''


# internal classes requiring tests
# PSy,Invokes,Dependencies,NameSpaceFactory,NameSpace,Invoke,Node,Schedule,
# LoopDirective,OMPLoopDirective,Loop,Call,Inf,SetInfCall,Kern,Arguments,
# InfArguments,Argument,KernelArgument,InfArgument

# user classes requiring tests
# PSyFactory, TransInfo, Transformation
from __future__ import absolute_import, print_function
import os
import re
import pytest
from fparser import api as fpapi
from psyclone.core.access_type import AccessType
from psyclone.psyGen import TransInfo, Transformation, PSyFactory, NameSpace, \
    NameSpaceFactory, OMPParallelDoDirective, \
    OMPParallelDirective, OMPDoDirective, OMPDirective, Directive, CodeBlock, \
    Assignment, Reference, BinaryOperation, Array, Literal, Node, IfBlock, \
    KernelSchedule, Schedule, UnaryOperation, NaryOperation, Return, \
    ACCEnterDataDirective, ACCKernelsDirective, Container
from psyclone.psyGen import GenerationError, FieldNotFoundError, \
     InternalError, HaloExchange, Invoke, DataAccess
from psyclone.psyGen import Symbol, SymbolTable
from psyclone.psyGen import Kern, Arguments, CodedKern
from psyclone.dynamo0p3 import DynKern, DynKernMetadata, DynInvokeSchedule
from psyclone.parse.algorithm import parse, InvokeCall
from psyclone.transformations import OMPParallelLoopTrans, \
    DynamoLoopFuseTrans, Dynamo0p3RedundantComputationTrans, \
    ACCEnterDataTrans, ACCParallelTrans, ACCLoopTrans, ACCKernelsTrans
from psyclone.generator import generate
from psyclone.configuration import Config
from psyclone.tests.utilities import get_invoke

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")


# Module fixtures

@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"

# Tests for utilities


def test_object_index():
    ''' Tests for the object_index() utility. '''
    from psyclone.psyGen import object_index
    two = "two"
    my_list = ["one", two, "three"]
    assert object_index(my_list, two) == 1
    with pytest.raises(InternalError) as err:
        _ = object_index(my_list, None)
    assert "Cannot search for None item in list" in str(err)

# PSyFactory class unit tests


def test_invalid_api():
    '''test that psyfactory raises appropriate error when an invalid api
    is supplied'''
    with pytest.raises(GenerationError):
        _ = PSyFactory(api="invalid")


def test_psyfactory_valid_return_object():
    '''test that psyfactory returns a psyfactory object for all supported
    inputs'''
    psy_factory = PSyFactory()
    assert isinstance(psy_factory, PSyFactory)
    _config = Config.get()
    apis = _config.supported_apis[:]
    apis.insert(0, "")
    for api in apis:
        psy_factory = PSyFactory(api=api)
        assert isinstance(psy_factory, PSyFactory)


def test_psyfactory_valid_dm_flag():
    '''test that a PSyFactory instance raises an exception if the
    optional distributed_memory flag is set to an invalid value
    and does not if the value is valid '''
    with pytest.raises(GenerationError) as excinfo:
        _ = PSyFactory(distributed_memory="ellie")
    assert "distributed_memory flag" in str(excinfo.value)
    _ = PSyFactory(distributed_memory=True)
    _ = PSyFactory(distributed_memory=False)


# Transformation class unit tests

def test_base_class_not_callable():
    '''make sure we can not instantiate abstract Transformation class
    directly'''
    with pytest.raises(TypeError):
        _ = Transformation()  # pylint: disable=abstract-class-instantiated


# TransInfo class unit tests

def test_new_module():
    '''check that we can change the module where we look for
    transformations.  There should be no transformations
    available as the new module uses a different
    transformation base class'''
    from .test_files import dummy_transformations
    trans = TransInfo(module=dummy_transformations)
    assert trans.num_trans == 0


def test_new_baseclass():
    '''check that we can change the transformations baseclass. There
    should be no transformations available as the default
    transformations module does not use the specified base
    class'''
    from .test_files.dummy_transformations import \
        LocalTransformation
    trans = TransInfo(base_class=LocalTransformation)
    assert trans.num_trans == 0


def test_new_module_and_baseclass():
    '''check that we can change the module where we look for
    transformations and the baseclass. There should be one
    transformation available as the module specifies one test
    transformation using the specified base class '''
    from .test_files import dummy_transformations
    trans = TransInfo(module=dummy_transformations,
                      base_class=dummy_transformations.LocalTransformation)
    assert trans.num_trans == 1


def test_list_valid_return_object():
    ''' check the list method returns the valid type '''
    trans = TransInfo()
    assert isinstance(trans.list, str)


def test_list_return_data():
    ''' check the list method returns sensible information '''
    trans = TransInfo()
    assert trans.list.find("available") != -1


def test_invalid_low_number():
    '''check an out-of-range low number for get_trans_num method raises
    correct exception'''
    trans = TransInfo()
    with pytest.raises(GenerationError):
        _ = trans.get_trans_num(0)


def test_invalid_high_number():
    '''check an out-of-range high number for get_trans_num method raises
    correct exception'''
    trans = TransInfo()
    with pytest.raises(GenerationError):
        _ = trans.get_trans_num(999)


def test_valid_return_object_from_number():
    ''' check get_trans_num method returns expected type of instance '''
    trans = TransInfo()
    transform = trans.get_trans_num(1)
    assert isinstance(transform, Transformation)


def test_invalid_name():
    '''check get_trans_name method fails correctly when an invalid name
    is provided'''
    trans = TransInfo()
    with pytest.raises(GenerationError):
        _ = trans.get_trans_name("invalid")


def test_valid_return_object_from_name():
    ''' check get_trans_name method return the correct object type '''
    trans = TransInfo()
    transform = trans.get_trans_name("LoopFuse")
    assert isinstance(transform, Transformation)


# NameSpace class unit tests

def test_fail_context_label():
    '''check an error is raised if one of context and label is not None'''
    namespace = NameSpace()
    with pytest.raises(RuntimeError):
        namespace.create_name(context="dummy_context")
    with pytest.raises(RuntimeError):
        namespace.create_name(label="dummy_context")


def test_case_sensitive_names():
    ''' tests that in the case sensitive option, names that only differ by
    case are treated as being distinct'''
    namespace_cs = NameSpace(case_sensitive=True)
    name = "Rupert"
    name1 = namespace_cs.create_name(root_name=name)
    name2 = namespace_cs.create_name(root_name=name.lower())
    assert name1 == name
    assert name2 == name.lower()


def test_case_insensitive_names():
    ''' tests that in the case insensitive option (the default), names that
    only differ by case are treated as being the same '''
    namespace = NameSpace()
    name = "Rupert"
    name1 = namespace.create_name(root_name=name)
    name2 = namespace.create_name(root_name=name.lower())
    assert name1 == name.lower()
    assert name2 == name1 + "_1"


def test_new_labels():
    '''tests that different labels and contexts are treated as being
    distinct'''
    namespace = NameSpace()
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="work",
                                  label="me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="a bear")
    name4 = namespace.create_name(root_name=name, context="work",
                                  label="a bear")
    assert name1 == name.lower()
    assert name2 == name1+"_1"
    assert name3 == name1+"_2"
    assert name4 == name1+"_3"


def test_new_labels_case_sensitive():
    '''tests that different labels and contexts are treated as being
    distinct for case sensitive names'''
    namespace = NameSpace(case_sensitive=True)
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="work",
                                  label="me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="Me")
    name4 = namespace.create_name(root_name=name, context="Work",
                                  label="me")
    assert name1 == name
    assert name2 == name1+"_1"
    assert name3 == name1+"_2"
    assert name4 == name1+"_3"


def test_existing_labels():
    '''tests that existing labels and contexts return the previous name'''
    namespace = NameSpace()
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="work",
                                  label="me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="Me")
    name4 = namespace.create_name(root_name=name, context="Work",
                                  label="me")
    assert name1 == name.lower()
    assert name2 == name1+"_1"
    assert name3 == name1
    assert name4 == name2


def test_existing_labels_case_sensitive():
    '''tests that existing labels and contexts return the previous name'''
    namespace = NameSpace(case_sensitive=True)
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="Work",
                                  label="Me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name4 = namespace.create_name(root_name=name, context="Work",
                                  label="Me")
    assert name1 == name
    assert name2 == name1+"_1"
    assert name3 == name1
    assert name4 == name2


def test_reserved_names():
    '''tests that reserved names are not returned by the name space
    manager'''
    namea = "PSyclone"
    nameb = "Dynamo"
    namespace = NameSpace()
    namespace.add_reserved_name(namea)
    name1 = namespace.create_name(root_name=namea.lower())
    assert name1 == namea.lower()+"_1"
    namespace.add_reserved_names([nameb.lower()])
    name1 = namespace.create_name(root_name=nameb)
    assert name1 == nameb.lower()+"_1"


def test_reserved_names_case_sensitive():
    '''tests that reserved names are not returned by the case sensitive
    name space manager'''
    namea = "PSyclone"
    nameb = "Dynamo"
    namespace = NameSpace(case_sensitive=True)
    namespace.add_reserved_name(namea)
    name1 = namespace.create_name(root_name=namea)
    assert name1 == namea+"_1"
    name1 = namespace.create_name(root_name=namea.lower())
    assert name1 == namea.lower()
    namespace.add_reserved_names([nameb])
    name1 = namespace.create_name(root_name=nameb)
    assert name1 == nameb+"_1"
    name1 = namespace.create_name(root_name=nameb.lower())
    assert name1 == nameb.lower()


def test_reserved_name_exists():
    '''tests that an error is generated if a reserved name has already
    been used as a name'''
    name = "PSyclone"
    namespace = NameSpace()
    _ = namespace.create_name(root_name=name)
    with pytest.raises(RuntimeError):
        namespace.add_reserved_name(name)
    with pytest.raises(RuntimeError):
        namespace.add_reserved_name(name.lower())


def test_reserved_name_exists_case_sensitive():
    '''tests that an error is generated if a reserved name has already
    been used as a name'''
    name = "PSyclone"
    namespace = NameSpace(case_sensitive=True)
    _ = namespace.create_name(root_name=name)
    namespace.add_reserved_name(name.lower())
    with pytest.raises(RuntimeError):
        namespace.add_reserved_name(name)
    with pytest.raises(RuntimeError):
        namespace.add_reserved_names([name])


def test_anonymous_name():
    ''' tests that anonymous names are successfully created '''
    namespace = NameSpace()
    name1 = namespace.create_name()
    assert name1 == "anon"
    name2 = namespace.create_name()
    assert name2 == "anon_1"


def test_internal_name_clashes():
    ''' tests that names that are generated internally by the namespace
    manager can be used as root names'''
    anon_name = "Anon"
    namespace = NameSpace()
    name1 = namespace.create_name()
    name2 = namespace.create_name(root_name=anon_name)
    assert name1 == anon_name.lower()
    assert name2 == name1+"_1"
    name3 = namespace.create_name(root_name=anon_name+"_1")
    assert name3 == name2+"_1"


def test_intern_name_clash_case_sensitive():
    '''tests that names that are generated internally by the case
    sensitive namespace manager can be used as root names'''
    anon_name = "Anon"
    namespace = NameSpace(case_sensitive=True)
    _ = namespace.create_name()
    name2 = namespace.create_name(root_name=anon_name)
    assert name2 == anon_name
    name3 = namespace.create_name(root_name=anon_name.lower())
    assert name3 == anon_name.lower()+"_1"


# tests that the NameSpaceFactory class is working correctly

def test_create():
    '''tests that a NameSpace object is returned from the create method'''
    nsf = NameSpaceFactory()
    nspace = nsf.create()
    assert isinstance(nspace, NameSpace)


def test_singleton():
    '''test that the same NameSpace object is returned from different
    NameSpaceFactory's by default'''
    nsf = NameSpaceFactory()
    ns1 = nsf.create()
    nsf = NameSpaceFactory()
    ns2 = nsf.create()
    assert ns1 == ns2


def test_reset():
    ''' test that different NameSpace objects are returned from different
    NameSpaceFactory's when the reset option is set'''
    nsf = NameSpaceFactory()
    ns1 = nsf.create()
    nsf = NameSpaceFactory(reset=True)
    ns2 = nsf.create()
    assert ns1 != ns2

# tests for class Call


def test_invokes_can_always_be_printed():
    '''Test that an Invoke instance can always be printed (i.e. is
    initialised fully)'''
    inv = Invoke(None, None, None)
    assert inv.__str__() == "invoke()"

    invoke_call = InvokeCall([], "TestName")
    inv = Invoke(invoke_call, 12, DynInvokeSchedule)
    # Name is converted to lower case if set in constructor of InvokeCall:
    assert inv.__str__() == "invoke_testname()"

    invoke_call._name = None
    inv = Invoke(invoke_call, 12, DynInvokeSchedule)
    assert inv.__str__() == "invoke_12()"

    # Last test case: one kernel call - to avoid constructing
    # the InvokeCall, parse an existing Fortran file"

    _, invoke = parse(
        os.path.join(BASE_PATH, "1.12_single_invoke_deref_name_clash.f90"),
        api="dynamo0.3")

    alg_invocation = invoke.calls[0]
    inv = Invoke(alg_invocation, 0, DynInvokeSchedule)
    assert inv.__str__() == \
        "invoke_0_testkern_type(a, f1_my_field, f1 % my_field, m1, m2)"


def test_same_name_invalid():
    '''test that we raise an error if the same name is passed into the
    same kernel or built-in instance. We need to choose a particular
    API to check this although the code is in psyGen.py '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(
            os.path.join(BASE_PATH, "1.10_single_invoke_same_name.f90"),
            api="dynamo0.3")
    assert ("Argument 'f1' is passed into kernel 'testkern_code' code "
            "more than once") in str(excinfo.value)


def test_same_name_invalid_array():
    '''test that we raise an error if the same name is passed into the
    same kernel or built-in instance. In this case arguments have
    array references and mixed case. We need to choose a particular
    API to check this although the code is in psyGen.py. '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(
            os.path.join(BASE_PATH, "1.11_single_invoke_same_name_array.f90"),
            api="dynamo0.3")
    assert ("Argument 'f1(1, n)' is passed into kernel 'testkern_code' code "
            "more than once") in str(excinfo.value)


def test_derived_type_deref_naming():
    ''' Test that we do not get a name clash for dummy arguments in the PSy
    layer when the name generation for the component of a derived type
    may lead to a name already taken by another argument. '''
    _, invoke = parse(
        os.path.join(BASE_PATH, "1.12_single_invoke_deref_name_clash.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke)
    generated_code = str(psy.gen)
    print(generated_code)
    output = (
        "    SUBROUTINE invoke_0_testkern_type"
        "(a, f1_my_field, f1_my_field_1, m1, m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(inout) :: f1_my_field\n"
        "      TYPE(field_type), intent(in) :: f1_my_field_1, m1, m2\n")
    assert output in generated_code


FAKE_KERNEL_METADATA = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(3) =                    &
          (/ arg_type(gh_field, gh_write,     w3),     &
             arg_type(gh_field, gh_readwrite, wtheta), &
             arg_type(gh_field, gh_inc,       w1)      &
           /)
     integer :: iterates_over = cells
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


# Schedule class tests

def test_sched_view(capsys):
    ''' Check the view method of the Schedule class'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # For this test use the generic class
    psy.invokes.invoke_list[0].schedule.__class__ = Schedule
    psy.invokes.invoke_list[0].schedule.view()

    output, _ = capsys.readouterr()
    assert colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"]) in output


def test_sched_getitem():
    '''Test that Schedule has the [int] operator overloaded to return the
    given index child'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    sched = psy.invokes.invoke_list[0].schedule
    for indx in range(len(sched._children)):
        assert sched[indx] is sched._children[indx]

    # Test range indexing
    children = sched[:]
    assert len(children) == 2
    assert children[0] is sched._children[0]
    assert children[1] is sched._children[1]

    # Test index out-of-bounds Error
    with pytest.raises(IndexError) as err:
        _ = sched[len(sched._children)]
    assert "list index out of range" in str(err)


def test_sched_can_be_printed():
    ''' Check the schedule class can always be printed'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # For this test use the generic class
    psy.invokes.invoke_list[0].schedule.__class__ = Schedule
    output = str(psy.invokes.invoke_list[0].schedule)

    assert "Schedule:\n" in output


# InvokeSchedule class tests

def test_invokeschedule_view(capsys):
    ''' Check the view method of the InvokeSchedule class. We need an
    InvokeSchedule object for this so go via the dynamo0.3 sub-class '''
    from psyclone import dynamo0p3
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    super(dynamo0p3.DynInvokeSchedule,
          psy.invokes.invoke_list[0].schedule).view()
    output, _ = capsys.readouterr()
    assert colored("InvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"]) in output


def test_sched_ocl_setter():
    ''' Check that the opencl setter raises the expected error if not passed
    a bool. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    with pytest.raises(ValueError) as err:
        psy.invokes.invoke_list[0].schedule.opencl = "a string"
    assert "Schedule.opencl must be a bool but got " in str(err)


def test_invokeschedule_can_be_printed():
    ''' Check the InvokeSchedule class can always be printed'''
    from psyclone.psyGen import InvokeSchedule
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # For this test use the generic class
    psy.invokes.invoke_list[0].schedule.__class__ = InvokeSchedule
    output = str(psy.invokes.invoke_list[0].schedule)

    assert "InvokeSchedule:\n" in output


# Kern class test

def test_kern_get_kernel_schedule():
    ''' Tests the get_kernel_schedule method in the Kern class.
    '''
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    schedule = my_kern.get_kernel_schedule()
    assert isinstance(schedule, KernelSchedule)


def test_codedkern_class_view(capsys):
    ''' Tests the view method in the CodedKern class. The simplest way to
    do this is via the dynamo0.3 subclass '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    my_kern.view()
    out, _ = capsys.readouterr()
    expected_output = (
        colored("CodedKern", SCHEDULE_COLOUR_MAP["CodedKern"]) +
        " dummy_code(field_1,field_2,field_3) [module_inline=False]")
    assert expected_output in out


def test_kern_coloured_text():
    ''' Check that the coloured_text method of both CodedKern and
    BuiltIn return what we expect. '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    # Use a Dynamo example that has both a CodedKern and a BuiltIn
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ckern = schedule.children[0].loop_body[0]
    bkern = schedule.children[1].loop_body[0]
    ret_str = ckern.coloured_text
    assert colored("CodedKern", SCHEDULE_COLOUR_MAP["CodedKern"]) in ret_str
    ret_str = bkern.coloured_text
    assert colored("BuiltIn", SCHEDULE_COLOUR_MAP["BuiltIn"]) in ret_str


def test_kern_abstract_methods():
    ''' Check that the abstract methods of the Kern class raise the
    NotImplementedError. '''
    # We need to get a valid kernel object
    from psyclone import dynamo0p3
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    with pytest.raises(NotImplementedError) as err:
        super(dynamo0p3.DynKern, my_kern).gen_arg_setter_code(None)
    assert "gen_arg_setter_code must be implemented by sub-class" in str(err)


def test_call_abstract_methods():
    ''' Check that calling the abstract methods of Kern raises
    the expected exceptions '''
    my_arguments = Arguments(None)

    class KernType(object):
        ''' temporary dummy class '''
        def __init__(self):
            self.iterates_over = "stuff"
    my_ktype = KernType()

    class DummyClass(object):
        ''' temporary dummy class '''
        def __init__(self, ktype):
            self.module_name = "dummy_module"
            self.ktype = ktype

    dummy_call = DummyClass(my_ktype)
    my_call = Kern(None, dummy_call, "dummy", my_arguments)
    with pytest.raises(NotImplementedError) as excinfo:
        my_call.local_vars()
    assert "Kern.local_vars should be implemented" in str(excinfo.value)

    with pytest.raises(NotImplementedError) as excinfo:
        my_call.__str__()
    assert "Kern.__str__ should be implemented" in str(excinfo.value)

    with pytest.raises(NotImplementedError) as excinfo:
        my_call.gen_code(None)
    assert "Kern.gen_code should be implemented" in str(excinfo.value)


def test_arguments_abstract():
    ''' Check that we raise NotImplementedError if any of the virtual methods
    of the Arguments class are called. '''
    my_arguments = Arguments(None)
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.acc_args
    assert "Arguments.acc_args must be implemented in sub-class" in str(err)
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.scalars
    assert "Arguments.scalars must be implemented in sub-class" in str(err)
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.raw_arg_list()
    assert ("Arguments.raw_arg_list must be implemented in sub-class"
            in str(err))


def test_incremented_arg():
    ''' Check that we raise the expected exception when
    CodedKern.incremented_arg() is called for a kernel that does not have
    an argument that is incremented '''
    # Change the kernel metadata so that the the incremented kernel
    # argument has read access
    import fparser
    fparser.logging.disable(fparser.logging.CRITICAL)
    # If we change the meta-data then we trip the check in the parser.
    # Therefore, we change the object produced by parsing the meta-data
    # instead
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    for descriptor in metadata.arg_descriptors:
        if descriptor.access == AccessType.INC:
            descriptor._access = AccessType.READ
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    with pytest.raises(FieldNotFoundError) as excinfo:
        CodedKern.incremented_arg(my_kern)
    assert ("does not have an argument with gh_inc access"
            in str(excinfo.value))


def test_ompdo_constructor():
    ''' Check that we can make an OMPDoDirective with and without
    children '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    ompdo = OMPDoDirective(parent=schedule)
    assert not ompdo.children
    ompdo = OMPDoDirective(parent=schedule, children=[schedule.children[0]])
    assert len(ompdo.children) == 1


def test_ompdo_directive_class_view(capsys):
    '''tests the view method in the OMPDoDirective class. We create a
    sub-class object then call this method from it '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")

    cases = [
        {"current_class": OMPParallelDoDirective,
         "current_string": "[OMP parallel do]"},
        {"current_class": OMPDoDirective, "current_string": "[OMP do]"},
        {"current_class": OMPParallelDirective,
         "current_string": "[OMP parallel]"},
        {"current_class": OMPDirective, "current_string": "[OMP]"},
        {"current_class": Directive, "current_string": ""}]
    otrans = OMPParallelLoopTrans()
    for case in cases:
        for dist_mem in [False, True]:

            psy = PSyFactory("dynamo0.3", distributed_memory=dist_mem).\
                create(invoke_info)
            schedule = psy.invokes.invoke_list[0].schedule

            if dist_mem:
                idx = 3
            else:
                idx = 0

            _, _ = otrans.apply(schedule.children[idx])
            omp_parallel_loop = schedule.children[idx]

            # call the OMPDirective view method
            case["current_class"].view(omp_parallel_loop)

            out, _ = capsys.readouterr()

            directive = colored("Directive", SCHEDULE_COLOUR_MAP["Directive"])
            loop = colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])

            expected_output = (
                directive + case["current_string"] + "\n"
                "    " + loop + "[type='', field_space='w1', it_space='cells',"
                )

            assert expected_output in out


def test_acc_dir_view(capsys):
    ''' Test the view() method of OpenACC directives '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP

    acclt = ACCLoopTrans()
    accdt = ACCEnterDataTrans()
    accpt = ACCParallelTrans()
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    colour = SCHEDULE_COLOUR_MAP["Directive"]
    schedule = invoke.schedule
    # Enter-data
    new_sched, _ = accdt.apply(schedule)
    # Artificially add a child to this directive so as to get full
    # coverage of the associated view() method
    new_sched.children[0].addchild(new_sched.children[1])
    new_sched.children[0].view()
    out, _ = capsys.readouterr()
    assert out.startswith(
        colored("Directive", colour)+"[ACC enter data]")

    # Parallel region
    new_sched, _ = accpt.apply(new_sched.children[1])
    new_sched.children[1].view()
    out, _ = capsys.readouterr()
    assert out.startswith(
        colored("Directive", colour)+"[ACC Parallel]")

    # Loop directive
    new_sched, _ = acclt.apply(new_sched.children[1].children[0])
    new_sched.children[1].children[0].view()
    out, _ = capsys.readouterr()
    assert out.startswith(
        colored("Directive", colour)+"[ACC Loop, independent]")

    # Loop directive with collapse
    new_sched, _ = acclt.apply(new_sched.children[1].children[0].children[0],
                               collapse=2)
    new_sched.children[1].children[0].children[0].view()
    out, _ = capsys.readouterr()
    assert out.startswith(
        colored("Directive", colour)+"[ACC Loop, collapse=2, independent]")


def test_haloexchange_unknown_halo_depth():
    '''test the case when the halo exchange base class is called without
    a halo depth'''
    halo_exchange = HaloExchange(None)
    assert halo_exchange._halo_depth is None


def test_globalsum_view(capsys):
    '''test the view method in the GlobalSum class. The simplest way to do
    this is to use a dynamo0p3 builtin example which contains a scalar and
    then call view() on that.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    from psyclone import dynamo0p3
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    psy.invokes.invoke_list[0].schedule.view()
    output, _ = capsys.readouterr()
    print(output)
    expected_output = (colored("GlobalSum",
                               SCHEDULE_COLOUR_MAP["GlobalSum"]) +
                       "[scalar='asum']")
    assert expected_output in output
    gsum = None
    for child in psy.invokes.invoke_list[0].schedule.children:
        if isinstance(child, dynamo0p3.DynGlobalSum):
            gsum = child
            break
    assert gsum
    ret_str = super(dynamo0p3.DynGlobalSum, gsum).coloured_text
    assert colored("GlobalSum", SCHEDULE_COLOUR_MAP["GlobalSum"]) in ret_str


def test_args_filter():
    '''the args_filter() method is in both Loop() and Arguments() classes
    with the former method calling the latter. This example tests the
    case when unique is set to True and therefore any replicated names
    are not returned. The simplest way to do this is to use a
    dynamo0p3 example which includes two kernels which share argument
    names. We choose dm=False to make it easier to fuse the loops.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    # fuse our loops so we have more than one Kernel in a loop
    schedule = psy.invokes.invoke_list[0].schedule
    ftrans = DynamoLoopFuseTrans()
    schedule, _ = ftrans.apply(schedule.children[0],
                               schedule.children[1])
    # get our loop and call our method ...
    loop = schedule.children[0]
    args = loop.args_filter(unique=True)
    expected_output = ["a", "f1", "f2", "m1", "m2", "f3"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)


def test_args_filter2():
    '''the args_filter() method is in both Loop() and Arguments() classes
    with the former method calling the latter. This example tests the cases
    when one or both of the intent and type arguments are not specified.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]

    # arg_accesses
    args = loop.args_filter(arg_accesses=[AccessType.READ])
    expected_output = ["chi", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)

    # arg_types
    args = loop.args_filter(arg_types=["gh_operator", "gh_integer"])
    expected_output = ["mm_w0", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)

    # neither
    args = loop.args_filter()
    expected_output = ["chi", "mm_w0", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)


def test_reduction_var_error():
    '''Check that we raise an exception if the zero_reduction_variable()
    method is provided with an incorrect type of argument'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        call = schedule.kernels()[0]
        # args[1] is of type gh_field
        call._reduction_arg = call.arguments.args[1]
        with pytest.raises(GenerationError) as err:
            call.zero_reduction_variable(None)
        assert ("zero_reduction variable should be one of ['gh_real', "
                "'gh_integer']") in str(err)


def test_reduction_sum_error():
    '''Check that we raise an exception if the reduction_sum_loop()
    method is provided with an incorrect type of argument'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        call = schedule.kernels()[0]
        # args[1] is of type gh_field
        call._reduction_arg = call.arguments.args[1]
        with pytest.raises(GenerationError) as err:
            call.reduction_sum_loop(None)
        assert (
            "unsupported reduction access 'gh_write' found in DynBuiltin:"
            "reduction_sum_loop(). Expected one of '['gh_sum']") in str(err)


def test_call_multi_reduction_error(monkeypatch):
    '''Check that we raise an exception if we try to create a Call (a
    Kernel or a Builtin) with more than one reduction in it. Since we have
    a rule that only Builtins can write to scalars we need a built-in that
    attempts to perform two reductions. '''
    from psyclone import dynamo0p3_builtins
    monkeypatch.setattr(dynamo0p3_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "multi_reduction_builtins_mod.f90"))
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "16.4.1_multiple_scalar_sums2.f90"),
            api="dynamo0.3")
        with pytest.raises(GenerationError) as err:
            _ = PSyFactory("dynamo0.3",
                           distributed_memory=dist_mem).create(invoke_info)
        assert (
            "PSyclone currently only supports a single reduction in a kernel "
            "or builtin" in str(err))


def test_invoke_name():
    ''' Check that specifying the name of an invoke in the Algorithm
    layer results in a correctly-named routine in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)
    print(gen)
    assert "SUBROUTINE invoke_important_invoke" in gen


def test_multi_kern_named_invoke():
    ''' Check that specifying the name of an invoke containing multiple
    kernel invocations result in a correctly-named routine in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.9_named_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)
    print(gen)
    assert "SUBROUTINE invoke_some_name" in gen


def test_named_multi_invokes():
    ''' Check that we generate correct code when we have more than one
    named invoke in an Algorithm file '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "3.2_multi_functions_multi_named_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)
    print(gen)
    assert "SUBROUTINE invoke_my_first(" in gen
    assert "SUBROUTINE invoke_my_second(" in gen


def test_named_invoke_name_clash():
    ''' Check that we do not get a name clash when the name of a variable
    in the PSy layer would normally conflict with the name given to the
    subroutine generated by an Invoke. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.11_named_invoke_name_clash.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)
    print(gen)
    assert "SUBROUTINE invoke_a(invoke_a_1, b, c, istp, rdt," in gen
    assert "TYPE(field_type), intent(inout) :: invoke_a_1" in gen


def test_invalid_reprod_pad_size(monkeypatch, dist_mem):
    '''Check that we raise an exception if the pad size in psyclone.cfg is
    set to an invalid value '''
    # Make sure we monkey patch the correct Config object
    config = Config.get()
    monkeypatch.setattr(config._instance, "_reprod_pad_size", 0)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import Dynamo0p3OMPLoopTrans, \
        OMPParallelTrans
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    schedule, _ = otrans.apply(schedule.children[0], reprod=True)
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    schedule, _ = rtrans.apply(schedule.children[0])
    invoke.schedule = schedule
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert (
        "REPROD_PAD_SIZE in {0} should be a positive "
        "integer".format(Config.get().filename) in str(excinfo.value))


def test_argument_depends_on():
    '''Check that the depends_on method returns the appropriate boolean
    value for arguments with combinations of read and write access'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    arg_f1_inc_1 = schedule.children[0].loop_body[0].arguments.args[0]
    arg_f1_inc_2 = schedule.children[2].loop_body[0].arguments.args[0]
    arg_f2_read_1 = schedule.children[0].loop_body[0].arguments.args[2]
    arg_f2_inc = schedule.children[1].loop_body[0].arguments.args[0]
    arg_f2_read_2 = schedule.children[2].loop_body[0].arguments.args[1]
    # different names returns False
    assert not arg_f2_inc._depends_on(arg_f1_inc_1)
    # same name both reads returns False
    assert not arg_f2_read_1._depends_on(arg_f2_read_2)
    # same name both incs (write to read) returns True
    assert arg_f1_inc_2._depends_on(arg_f1_inc_1)
    # read to write returns True
    assert arg_f2_read_1._depends_on(arg_f2_inc)
    # write to read returns True
    assert arg_f2_inc._depends_on(arg_f2_read_1)
    # same name both writes (the 4.5 example only uses inc) returns True
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    arg_f1_write_1 = schedule.children[0].loop_body[0].arguments.args[1]
    arg_f1_write_2 = schedule.children[1].loop_body[0].arguments.args[0]
    assert arg_f1_write_1._depends_on(arg_f1_write_2)


def test_argument_find_argument():
    '''Check that the find_argument method returns the first dependent
    argument in a list of nodes, or None if none are found'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns none if none found
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    # a) empty node list
    assert not f1_first_read._find_argument([])
    # b) check many reads
    call_nodes = schedule.kernels()
    assert not f1_first_read._find_argument(call_nodes)
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    f3_first_read = schedule.children[0].loop_body[0].arguments.args[3]
    result = f3_write._find_argument(call_nodes)
    assert result == f3_first_read
    # 3: haloexchange node
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # a) kern arg depends on halo arg
    m2_read_arg = schedule.children[3].loop_body[0].arguments.args[4]
    m2_halo_field = schedule.children[2].field
    result = m2_read_arg._find_argument(schedule.children)
    assert result == m2_halo_field
    # b) halo arg depends on kern arg
    result = m2_halo_field._find_argument([schedule.children[3].loop_body[0]])
    assert result == m2_read_arg
    # 4: globalsum node
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # a) globalsum arg depends on kern arg
    kern_asum_arg = schedule.children[3].loop_body[0].arguments.args[1]
    glob_sum_arg = schedule.children[2].scalar
    result = kern_asum_arg._find_argument(schedule.children)
    assert result == glob_sum_arg
    # b) kern arg depends on globalsum arg
    result = glob_sum_arg._find_argument([schedule.children[3].loop_body[0]])
    assert result == kern_asum_arg


def test_argument_find_read_arguments():
    '''Check that the find_read_arguments method returns the appropriate
    arguments in a list of nodes.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns [] if not a writer. f1 is read, not written.
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    call_nodes = schedule.kernels()
    assert f1_first_read._find_read_arguments(call_nodes) == []
    # 2: return list of readers (f3 is written to and then read by
    # three following calls)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    result = f3_write._find_read_arguments(call_nodes[4:])
    assert len(result) == 3
    for idx in range(3):
        loop = schedule.children[idx+4]
        assert result[idx] == loop.loop_body[0].arguments.args[3]
    # 3: Return empty list if no readers (f2 is written to but not
    # read)
    f2_write = schedule.children[0].loop_body[0].arguments.args[0]
    assert f2_write._find_read_arguments(call_nodes[1:]) == []
    # 4: Return list of readers before a subsequent writer
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    result = f3_write._find_read_arguments(call_nodes)
    assert len(result) == 3
    for idx in range(3):
        loop = schedule.children[idx]
        assert result[idx] == loop.loop_body[0].arguments.args[3]


def test_globalsum_arg():
    ''' Check that the globalsum argument is defined as gh_readwrite and
    points to the GlobalSum node '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    glob_sum_arg = glob_sum.scalar
    assert glob_sum_arg.access == AccessType.READWRITE
    assert glob_sum_arg.call == glob_sum


def test_haloexchange_arg():
    '''Check that the HaloExchange argument is defined as gh_readwrite and
    points to the HaloExchange node'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[2]
    halo_exchange_arg = halo_exchange.field
    assert halo_exchange_arg.access == AccessType.READWRITE
    assert halo_exchange_arg.call == halo_exchange


def test_argument_forward_read_dependencies():
    '''Check that the forward_read_dependencies method returns the appropriate
    arguments in a schedule.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns [] if not a writer. f1 is read, not written.
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    _ = schedule.kernels()
    assert f1_first_read.forward_read_dependencies() == []
    # 2: return list of readers (f3 is written to and then read by
    # three following calls)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    result = f3_write.forward_read_dependencies()
    assert len(result) == 3
    for idx in range(3):
        loop = schedule.children[idx+4]
        assert result[idx] == loop.loop_body[0].arguments.args[3]
    # 3: Return empty list if no readers (f2 is written to but not
    # read)
    f2_write = schedule.children[0].loop_body[0].arguments.args[0]
    assert f2_write.forward_read_dependencies() == []


def test_argument_forward_dependence(monkeypatch, annexed):
    '''Check that forward_dependence method returns the first dependent
    argument after the current Node in the schedule or None if none
    are found. We also test when annexed is False and True as it
    affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    # 1: returns none if none found (check many reads)
    assert not f1_first_read.forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    f3_next_read = schedule.children[4].loop_body[0].arguments.args[3]
    result = f3_write.forward_dependence()
    assert result == f3_next_read
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 7
    else:
        index = 8
    f2_prev_arg = schedule.children[index-1].loop_body[0].arguments.args[0]
    f2_halo_field = schedule.children[index].field
    f2_next_arg = schedule.children[index+1].loop_body[0].arguments.args[1]
    # a) previous kern arg depends on halo arg
    result = f2_prev_arg.forward_dependence()
    assert result == f2_halo_field
    # b) halo arg depends on following kern arg
    result = f2_halo_field.forward_dependence()
    assert result == f2_next_arg
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_arg = schedule.children[0].loop_body[0].arguments.args[1]
    sum_arg = schedule.children[1].loop_body[0].arguments.args[0]
    global_sum_arg = schedule.children[2].scalar
    next_arg = schedule.children[3].loop_body[0].arguments.args[1]
    # a) prev kern arg depends on sum
    result = prev_arg.forward_dependence()
    assert result == sum_arg
    # b) sum arg depends on global sum arg
    result = sum_arg.forward_dependence()
    assert result == global_sum_arg
    # c) global sum arg depends on next kern arg
    result = global_sum_arg.forward_dependence()
    assert result == next_arg


def test_argument_backward_dependence(monkeypatch, annexed):
    '''Check that backward_dependence method returns the first dependent
    argument before the current Node in the schedule or None if none
    are found. We also test when annexed is False and True as it
    affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f1_last_read = schedule.children[6].loop_body[0].arguments.args[2]
    # 1: returns none if none found (check many reads)
    assert not f1_last_read.backward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    f3_prev_read = schedule.children[2].loop_body[0].arguments.args[3]
    result = f3_write.backward_dependence()
    assert result == f3_prev_read
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 7
    else:
        index = 8
    f2_prev_arg = schedule.children[index-1].loop_body[0].arguments.args[0]
    f2_halo_field = schedule.children[index].field
    f2_next_arg = schedule.children[index+1].loop_body[0].arguments.args[1]
    # a) following kern arg depends on halo arg
    result = f2_next_arg.backward_dependence()
    assert result == f2_halo_field
    # b) halo arg depends on previous kern arg
    result = f2_halo_field.backward_dependence()
    assert result == f2_prev_arg
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_arg = schedule.children[0].loop_body[0].arguments.args[1]
    sum_arg = schedule.children[1].loop_body[0].arguments.args[0]
    global_sum_arg = schedule.children[2].scalar
    next_arg = schedule.children[3].loop_body[0].arguments.args[1]
    # a) next kern arg depends on global sum arg
    result = next_arg.backward_dependence()
    assert result == global_sum_arg
    # b) global sum arg depends on sum arg
    result = global_sum_arg.backward_dependence()
    assert result == sum_arg
    # c) sum depends on prev kern arg
    result = sum_arg.backward_dependence()
    assert result == prev_arg


def test_node_depth():
    '''
    Test that the Node class depth method returns the correct value for a
    Node in a tree. The start depth to determine a Node's depth is set to
    0. Depth of a Schedule is 1 and increases for its descendants.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Assert that start_depth of any Node (including Schedule) is 0
    assert schedule.START_DEPTH == 0
    # Assert that Schedule depth is 1
    assert schedule.depth == 1
    # Depth increases by 1 for descendants at each level
    for child in schedule.children:
        assert child.depth == 2
    for child in schedule.children[3].children:
        assert child.depth == 3


def test_node_position():
    '''
    Test that the Node class position and abs_position methods return
    the correct value for a Node in a tree. The start position is
    set to 0. Relative position starts from 0 and absolute from 1.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.7_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    child = schedule.children[6]
    # Assert that position of a Schedule (no parent Node) is 0
    assert schedule.position == 0
    # Assert that start_position of any Node is 0
    assert child.START_POSITION == 0
    # Assert that relative and absolute positions return correct values
    assert child.position == 6
    assert child.abs_position == 7
    # Test InternalError for _find_position with an incorrect position
    with pytest.raises(InternalError) as excinfo:
        _, _ = child._find_position(child.root.children, -2)
    assert "started from -2 instead of 0" in str(excinfo.value)
    # Test InternalError for abs_position with a Node that does
    # not belong to the Schedule
    ompdir = OMPDoDirective()
    with pytest.raises(InternalError) as excinfo:
        _ = ompdir.abs_position
    assert ("PSyclone internal error: Error in search for Node position "
            "in the tree") in str(excinfo.value)


def test_node_root():
    '''
    Test that the Node class root method returns the correct instance
    for a Node in a tree.
    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.7_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    ru_schedule = invoke.schedule
    # Select a loop and the kernel inside
    ru_loop = ru_schedule.children[1]
    ru_kern = ru_loop.children[0]
    # Assert that the absolute root is a Schedule
    assert isinstance(ru_kern.root, Schedule)


def test_node_args():
    '''Test that the Node class args method returns the correct arguments
    for Nodes that do not have arguments themselves'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    kern1 = loop1.loop_body[0]
    loop2 = schedule.children[1]
    kern2 = loop2.loop_body[0]
    # 1) Schedule (not that this is useful)
    all_args = kern1.arguments.args
    all_args.extend(kern2.arguments.args)
    schedule_args = schedule.args
    for idx, arg in enumerate(all_args):
        assert arg == schedule_args[idx]
    # 2) Loop1
    loop1_args = loop1.args
    for idx, arg in enumerate(kern1.arguments.args):
        assert arg == loop1_args[idx]
    # 3) Loop2
    loop2_args = loop2.args
    for idx, arg in enumerate(kern2.arguments.args):
        assert arg == loop2_args[idx]
    # 4) Loop fuse
    ftrans = DynamoLoopFuseTrans()
    ftrans.same_space = True
    schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1])
    loop = schedule.children[0]
    kern1 = loop.loop_body[0]
    kern2 = loop.loop_body[1]
    loop_args = loop.args
    kern_args = kern1.arguments.args
    kern_args.extend(kern2.arguments.args)
    for idx, arg in enumerate(kern_args):
        assert arg == loop_args[idx]


def test_call_args():
    '''Test that the call class args method returns the appropriate
    arguments '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kern = schedule.children[0].loop_body[0]
    builtin = schedule.children[1].loop_body[0]
    # 1) kern
    for idx, arg in enumerate(kern.args):
        assert arg == kern.arguments.args[idx]
    # 2) builtin
    for idx, arg in enumerate(builtin.args):
        assert arg == builtin.arguments.args[idx]


def test_haloexchange_can_be_printed():
    '''Test that the HaloExchange class can always be printed'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    for haloexchange in schedule.children[:2]:
        assert "HaloExchange[field='" in str(haloexchange)
        assert "', type='" in str(haloexchange)
        assert "', depth='" in str(haloexchange)
        assert "', check_dirty='" in str(haloexchange)


def test_haloexchange_args():
    '''Test that the haloexchange class args method returns the appropriate
    argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    for haloexchange in schedule.children[:2]:
        assert len(haloexchange.args) == 1
        assert haloexchange.args[0] == haloexchange.field


def test_globalsum_args():
    '''Test that the globalsum class args method returns the appropriate
    argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert len(global_sum.args) == 1
    assert global_sum.args[0] == global_sum.scalar


def test_node_forward_dependence():
    '''Test that the Node class forward_dependence method returns the
    closest dependent Node after the current Node in the schedule or
    None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    read4 = schedule.children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # b) check no dependencies for a call
    assert not read4.children[0].forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[3]
    next_read = schedule.children[4]
    assert writer.forward_dependence() == next_read
    # a) check writer returned
    first_loop = schedule.children[0]
    assert first_loop.forward_dependence() == writer
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_loop = schedule.children[7]
    halo_field = schedule.children[8]
    next_loop = schedule.children[9]
    # a) previous loop depends on halo exchange
    assert prev_loop.forward_dependence() == halo_field
    # b) halo exchange depends on following loop
    assert halo_field.forward_dependence() == next_loop

    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_loop = schedule.children[0]
    sum_loop = schedule.children[1]
    global_sum_loop = schedule.children[2]
    next_loop = schedule.children[3]
    # a) prev loop depends on sum loop
    assert prev_loop.forward_dependence() == sum_loop
    # b) sum loop depends on global sum loop
    assert sum_loop.forward_dependence() == global_sum_loop
    # c) global sum loop depends on next loop
    assert global_sum_loop.forward_dependence() == next_loop


def test_node_backward_dependence():
    '''Test that the Node class backward_dependence method returns the
    closest dependent Node before the current Node in the schedule or
    None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: loop no backwards dependence
    loop3 = schedule.children[2]
    assert not loop3.backward_dependence()
    # 2: loop to loop backward dependence
    # a) many steps
    last_loop_node = schedule.children[6]
    prev_dep_loop_node = schedule.children[3]
    assert last_loop_node.backward_dependence() == prev_dep_loop_node
    # b) previous
    assert prev_dep_loop_node.backward_dependence() == loop3
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop2 = schedule.children[7]
    halo_exchange = schedule.children[8]
    loop3 = schedule.children[9]
    # a) following loop node depends on halo exchange node
    result = loop3.backward_dependence()
    assert result == halo_exchange
    # b) halo exchange node depends on previous loop node
    result = halo_exchange.backward_dependence()
    assert result == loop2
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    global_sum = schedule.children[2]
    loop3 = schedule.children[3]
    # a) loop3 depends on global sum
    assert loop3.backward_dependence() == global_sum
    # b) global sum depends on loop2
    assert global_sum.backward_dependence() == loop2
    # c) loop2 (sum) depends on loop1
    assert loop2.backward_dependence() == loop1


def test_call_forward_dependence():
    '''Test that the Call class forward_dependence method returns the
    closest dependent call after the current call in the schedule or
    None if none are found. This is achieved by loop fusing first.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    ftrans.same_space = True
    for _ in range(6):
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1])
    read4 = schedule.children[0].loop_body[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[0].loop_body[3]
    next_read = schedule.children[0].loop_body[4]
    assert writer.forward_dependence() == next_read
    # a) check writer returned
    first_loop = schedule.children[0].loop_body[0]
    assert first_loop.forward_dependence() == writer


def test_call_backward_dependence():
    '''Test that the Call class backward_dependence method returns the
    closest dependent call before the current call in the schedule or
    None if none are found. This is achieved by loop fusing first.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    ftrans.same_space = True
    for _ in range(6):
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1])
    # 1: loop no backwards dependence
    call3 = schedule.children[0].loop_body[2]
    assert not call3.backward_dependence()
    # 2: call to call backward dependence
    # a) many steps
    last_call_node = schedule.children[0].loop_body[6]
    prev_dep_call_node = schedule.children[0].loop_body[3]
    assert last_call_node.backward_dependence() == prev_dep_call_node
    # b) previous
    assert prev_dep_call_node.backward_dependence() == call3


def test_omp_forward_dependence():
    '''Test that the forward_dependence method works for Directives,
    returning the closest dependent Node after the current Node in the
    schedule or None if none are found. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()
    for child in schedule.children:
        schedule, _ = otrans.apply(child)
    read4 = schedule.children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # b) check no dependencies for the loop
    assert not read4.children[0].forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[3]
    next_read = schedule.children[4]
    assert writer.forward_dependence() == next_read
    # b) check writer returned
    first_omp = schedule.children[0]
    assert first_omp.forward_dependence() == writer
    # 3: directive and globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule, _ = otrans.apply(schedule.children[0])
    schedule, _ = otrans.apply(schedule.children[1])
    schedule, _ = otrans.apply(schedule.children[3])
    prev_omp = schedule.children[0]
    sum_omp = schedule.children[1]
    global_sum_loop = schedule.children[2]
    next_omp = schedule.children[3]
    # a) prev omp depends on sum omp
    assert prev_omp.forward_dependence() == sum_omp
    # b) sum omp depends on global sum loop
    assert sum_omp.forward_dependence() == global_sum_loop
    # c) global sum loop depends on next omp
    assert global_sum_loop.forward_dependence() == next_omp


def test_directive_backward_dependence():
    '''Test that the backward_dependence method works for Directives,
    returning the closest dependent Node before the current Node in
    the schedule or None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()
    for child in schedule.children:
        schedule, _ = otrans.apply(child)
    # 1: omp directive no backwards dependence
    omp3 = schedule.children[2]
    assert not omp3.backward_dependence()
    # 2: omp to omp backward dependence
    # a) many steps
    last_omp_node = schedule.children[6]
    prev_dep_omp_node = schedule.children[3]
    assert last_omp_node.backward_dependence() == prev_dep_omp_node
    # b) previous
    assert prev_dep_omp_node.backward_dependence() == omp3
    # 3: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule, _ = otrans.apply(schedule.children[0])
    schedule, _ = otrans.apply(schedule.children[1])
    schedule, _ = otrans.apply(schedule.children[3])
    omp1 = schedule.children[0]
    omp2 = schedule.children[1]
    global_sum = schedule.children[2]
    omp3 = schedule.children[3]
    # a) omp3 depends on global sum
    assert omp3.backward_dependence() == global_sum
    # b) global sum depends on omp2
    assert global_sum.backward_dependence() == omp2
    # c) omp2 (sum) depends on omp1
    assert omp2.backward_dependence() == omp1


def test_directive_get_private(monkeypatch):
    ''' Tests for the _get_private_list() method of OMPParallelDirective. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"), api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # We use Transformations to introduce the necessary directives
    from psyclone.transformations import Dynamo0p3OMPLoopTrans, \
        OMPParallelTrans
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    schedule, _ = otrans.apply(schedule.children[0], reprod=True)
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    schedule, _ = rtrans.apply(schedule.children[0])
    directive = schedule.children[0]
    assert isinstance(directive, OMPParallelDirective)
    # Now check that _get_private_list returns what we expect
    pvars = directive._get_private_list()
    assert pvars == ['cell']
    # Now use monkeypatch to break the Call within the loop
    call = directive.children[0].children[0].loop_body[0]
    monkeypatch.setattr(call, "local_vars", lambda: [""])
    with pytest.raises(InternalError) as err:
        _ = directive._get_private_list()
    assert ("call 'testkern_code' has a local variable but its name is "
            "not set" in str(err))


def test_node_is_valid_location():
    '''Test that the Node class is_valid_location method returns True if
    the new location does not break any data dependencies, otherwise it
    returns False'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: new node argument is invalid
    node = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location("invalid_node_argument")
    assert "argument is not a Node, it is a 'str'." in str(excinfo.value)
    # 2: optional position argument is invalid
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="invalid_node_argument")
    assert "The position argument in the psyGen" in str(excinfo.value)
    assert "method must be one of" in str(excinfo.value)
    # 3: parents of node and new_node are not the same
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(schedule.children[3].children[0])
    assert ("the node and the location do not have the same "
            "parent") in str(excinfo.value)
    # 4: positions are the same
    prev_node = schedule.children[0]
    node = schedule.children[1]
    next_node = schedule.children[2]
    # a) before this node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # b) after this node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # c) after previous node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(prev_node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # d) before next node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(next_node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # 5: valid no previous dependency
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 6: valid no prev dep
    node = schedule.children[2]
    assert node.is_valid_location(schedule.children[0])
    # 7: valid prev dep (after)
    node = schedule.children[6]
    assert node.is_valid_location(schedule.children[3], position="after")
    # 8: invalid prev dep (before)
    assert not node.is_valid_location(schedule.children[3], position="before")
    # 9: valid no following dep
    node = schedule.children[4]
    assert node.is_valid_location(schedule.children[6], position="after")
    # 10: valid following dep (before)
    node = schedule.children[0]
    assert node.is_valid_location(schedule.children[3], position="before")
    # 11: invalid following dep (after)
    node = schedule.children[0]
    assert not node.is_valid_location(schedule.children[3], position="after")


def test_node_ancestor():
    ''' Test the Node.ancestor() method '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].loop_body[0].loop_body[0]
    node = kern.ancestor(Node)
    assert isinstance(node, Schedule)
    node = kern.ancestor(Node, excluding=[Schedule])
    assert node is sched.children[0].loop_body[0]


def test_dag_names():
    '''test that the dag_name method returns the correct value for the
    node class and its specialisations'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    assert super(Schedule, schedule).dag_name == "node_0"
    assert schedule.dag_name == "schedule"
    assert schedule.children[0].dag_name == "checkhaloexchange(f2)_0"
    assert schedule.children[3].dag_name == "loop_4"
    schedule.children[3].loop_type = "colour"
    assert schedule.children[3].dag_name == "loop_[colour]_4"
    schedule.children[3].loop_type = ""
    assert (schedule.children[3].loop_body[0].dag_name ==
            "kernel_testkern_code_9")
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert global_sum.dag_name == "globalsum(asum)_2"
    builtin = schedule.children[1].loop_body[0]
    assert builtin.dag_name == "builtin_sum_x_12"


def test_openmp_pdo_dag_name():
    '''Test that we generate the correct dag name for the OpenMP parallel
    do node'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.7.2_setval_X_builtin.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    schedule, _ = otrans.apply(schedule.children[0])
    assert schedule.children[0].dag_name == "OMP_parallel_do_1"


def test_omp_dag_names():
    '''Test that we generate the correct dag names for omp parallel, omp
    do, omp directive and directive nodes'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    from psyclone.transformations import Dynamo0p3OMPLoopTrans, \
        OMPParallelTrans
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()
    # Put an OMP PARALLEL around this loop
    child = schedule.children[0]
    oschedule, _ = ptrans.apply(child)
    # Put an OMP DO around this loop
    schedule, _ = olooptrans.apply(oschedule.children[0].children[0])
    # Replace the original loop schedule with the transformed one
    omp_par_node = schedule.children[0]
    assert omp_par_node.dag_name == "OMP_parallel_1"
    assert omp_par_node.children[0].dag_name == "OMP_do_2"
    omp_directive = super(OMPParallelDirective, omp_par_node)
    assert omp_directive.dag_name == "OMP_directive_1"
    print(type(omp_directive))
    directive = super(OMPDirective, omp_par_node)
    assert directive.dag_name == "directive_1"


def test_acc_dag_names():
    ''' Check that we generate the correct dag names for ACC parallel,
    ACC enter-data and ACC loop directive Nodes '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    schedule = invoke.schedule

    acclt = ACCLoopTrans()
    accdt = ACCEnterDataTrans()
    accpt = ACCParallelTrans()
    # Enter-data
    new_sched, _ = accdt.apply(schedule)
    assert schedule.children[0].dag_name == "ACC_data_1"
    # Parallel region
    new_sched, _ = accpt.apply(new_sched.children[1])
    assert schedule.children[1].dag_name == "ACC_parallel_2"
    # Loop directive
    new_sched, _ = acclt.apply(new_sched.children[1].children[0])
    assert schedule.children[1].children[0].dag_name == "ACC_loop_3"
    # Base class
    name = super(ACCEnterDataDirective, schedule.children[0]).dag_name
    assert name == "ACC_directive_1"

# Class ACCKernelsDirective start


# (1/1) Method __init__
def test_acckernelsdirective_init():
    '''Test an ACCKernelsDirective can be created and that the optional
    arguments are set and can be set as expected.

    '''
    directive = ACCKernelsDirective()
    assert directive._default_present
    assert directive.parent is None
    assert directive.children == []
    directive = ACCKernelsDirective(default_present=False)
    assert not directive._default_present


# (1/1) Method dag_name
def test_acckernelsdirective_dagname():
    '''Check that the dag_name method in the ACCKernelsDirective class
    behaves as expected.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    trans = ACCKernelsTrans()
    _, _ = trans.apply(sched)
    assert sched.children[0].dag_name == "ACC_kernels_1"


# (1/1) Method view
def test_acckernelsdirective_view(capsys):
    '''Check that the view method in the ACCKernelsDirective class behaves
    as expected.

    '''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP

    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    colour = SCHEDULE_COLOUR_MAP["Directive"]

    trans = ACCKernelsTrans()
    _, _ = trans.apply(sched)

    sched.children[0].view()
    out, _ = capsys.readouterr()
    assert out.startswith(
        colored("Directive", colour)+"[ACC Kernels]")
    assert "Loop" in out
    assert "CodedKern" in out


# (1/1) Method gen_code
@pytest.mark.parametrize("default_present", [False, True])
def test_acckernelsdirective_gencode(default_present):
    '''Check that the gen_code method in the ACCKernelsDirective class
    generates the expected code. Use the dynamo0.3 API.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    trans = ACCKernelsTrans()
    _, _ = trans.apply(sched, default_present=default_present)

    code = str(psy.gen)
    string = ""
    if default_present:
        string = " default(present)"
    assert (
        "      !$acc kernels{0}\n"
        "      DO cell=1,f1_proxy%vspace%get_ncell()\n".format(string) in code)
    assert (
        "      END DO \n"
        "      !$acc end kernels\n" in code)


# (1/1) Method update
@pytest.mark.parametrize("default_present", [False, True])
def test_acckernelsdirective_update(parser, default_present):
    '''Check that the update method in the ACCKernelsDirective class
    generates the expected code. Use the nemo API.

    '''
    from fparser.common.readfortran import FortranStringReader
    reader = FortranStringReader("program implicit_loop\n"
                                 "real(kind=wp) :: sto_tmp(5,5)\n"
                                 "sto_tmp(:,:) = 0.0_wp\n"
                                 "end program implicit_loop\n")
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    kernels_trans = ACCKernelsTrans()
    schedule, _ = kernels_trans.apply(schedule.children[0:1],
                                      default_present=default_present)
    gen_code = str(psy.gen)
    string = ""
    if default_present:
        string = " DEFAULT(PRESENT)"
    assert ("  !$ACC KERNELS{0}\n"
            "  sto_tmp(:, :) = 0.0_wp\n"
            "  !$ACC END KERNELS\n".format(string) in gen_code)

# Class ACCKernelsDirective end

# Class ACCEnterDataDirective start


# (1/1) Method __init__
def test_acc_datadevice_virtual():
    ''' Check that we can't instantiate an instance of
    ACCEnterDataDirective. '''
    # pylint:disable=abstract-class-instantiated
    with pytest.raises(TypeError) as err:
        ACCEnterDataDirective()
    # pylint:enable=abstract-class-instantiated
    assert ("instantiate abstract class ACCEnterDataDirective with abstract "
            "methods data_on_device" in str(err))

# (1/1) Method view
# Covered in test test_acc_dir_view

# (1/1) Method dag_name
# Covered in test_acc_dag_names


# (1/4) Method gen_code
def test_accenterdatadirective_gencode_1():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with a single loop, raises the expected exception as there is no
    following OpenACC Parallel directive and at least one is
    required. This test uses the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    _ = acc_enter_trans.apply(sched)
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel directives within the region."
            in str(excinfo.value))


# (2/4) Method gen_code
def test_accenterdatadirective_gencode_2():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with multiple loops, raises the expected exception, as there is no
    following OpenACC Parallel directive and at least one is
    required. This test uses the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0').schedule
    _ = acc_enter_trans.apply(sched)
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel directives within the region."
            in str(excinfo.value))


# (3/4) Method gen_code
def test_accenterdatadirective_gencode_3():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with a single loop, produces the expected code (there should be
    "copy in" data as there is a following OpenACC parallel
    directive). This test uses the dynamo0.3 API.

    '''
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    _ = acc_par_trans.apply(sched.children)
    _ = acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert (
        "      !$acc enter data copyin(nlayers,a,f1_proxy,f1_proxy%data,"
        "f2_proxy,f2_proxy%data,m1_proxy,m1_proxy%data,m2_proxy,"
        "m2_proxy%data,ndf_w1,undf_w1,map_w1,ndf_w2,undf_w2,map_w2,"
        "ndf_w3,undf_w3,map_w3)\n" in code)


# (4/4) Method gen_code
def test_accenterdatadirective_gencode_4():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with multiple loops and multiple OpenACC parallel directives,
    produces the expected code (when the same argument is used in
    multiple loops there should only be one entry). This test uses the
    dynamo0.3 API.

    '''
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0').schedule
    _ = acc_par_trans.apply(sched.children[1])
    _ = acc_par_trans.apply(sched.children[0])
    _ = acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert (
        "      !$acc enter data copyin(nlayers,a,f1_proxy,f1_proxy%data,"
        "f2_proxy,f2_proxy%data,m1_proxy,m1_proxy%data,m2_proxy,m2_proxy%data,"
        "ndf_w1,undf_w1,map_w1,ndf_w2,undf_w2,map_w2,ndf_w3,undf_w3,map_w3,"
        "f3_proxy,f3_proxy%data)\n" in code)

# Class ACCEnterDataDirective end


def test_node_dag_no_graphviz(tmpdir, monkeypatch):
    '''test that dag generation does nothing if graphviz is not
    installed. We monkeypatch sys.modules to ensure that it always
    appears that graphviz is not installed on this system. '''
    import sys
    monkeypatch.setitem(sys.modules, 'graphviz', None)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    schedule.dag(file_name=my_file.strpath)
    assert not os.path.exists(my_file.strpath)


# Use a regex to allow for whitespace differences between graphviz
# versions. Need a raw-string (r"") to get new-lines handled nicely.
EXPECTED2 = re.compile(
    r"digraph {\n"
    r"\s*schedule_start\n"
    r"\s*schedule_end\n"
    r"\s*loop_1_start\n"
    r"\s*loop_1_end\n"
    r"\s*loop_1_end -> loop_3_start \[color=green\]\n"
    r"\s*schedule_start -> loop_1_start \[color=blue\]\n"
    r"\s*kernel_testkern_qr_code_2\n"
    r"\s*kernel_testkern_qr_code_2 -> loop_1_end \[color=blue\]\n"
    r"\s*loop_1_start -> kernel_testkern_qr_code_2 \[color=blue\]\n"
    r"\s*loop_3_start\n"
    r"\s*loop_3_end\n"
    r"\s*loop_3_end -> schedule_end \[color=blue\]\n"
    r"\s*loop_1_end -> loop_3_start \[color=red\]\n"
    r"\s*kernel_testkern_qr_code_4\n"
    r"\s*kernel_testkern_qr_code_4 -> loop_3_end \[color=blue\]\n"
    r"\s*loop_3_start -> kernel_testkern_qr_code_4 \[color=blue\]\n"
    r"}")
# pylint: enable=anomalous-backslash-in-string


def test_node_dag(tmpdir, have_graphviz):
    '''test that dag generation works correctly. Skip the test if
    graphviz is not installed'''
    if not have_graphviz:
        return
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.1_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    schedule.dag(file_name=my_file.strpath)
    result = my_file.read()
    print(result)
    assert EXPECTED2.match(result)
    my_file = tmpdir.join('test.svg')
    result = my_file.read()
    for name in ["<title>schedule_start</title>",
                 "<title>schedule_end</title>",
                 "<title>loop_1_start</title>",
                 "<title>loop_1_end</title>",
                 "<title>kernel_testkern_qr_code_2</title>",
                 "<title>kernel_testkern_qr_code_4</title>",
                 "<svg", "</svg>", ]:
        assert name in result
    for colour_name, colour_code in [("blue", "#0000ff"),
                                     ("green", "#00ff00"),
                                     ("red", "#ff0000")]:
        assert colour_name in result or colour_code in result

    with pytest.raises(GenerationError) as excinfo:
        schedule.dag(file_name=my_file.strpath, file_format="rubbish")
    assert "unsupported graphviz file format" in str(excinfo.value)


def test_haloexchange_halo_depth_get_set():
    '''test that the halo_exchange getter and setter work correctly '''
    halo_depth = 4
    halo_exchange = HaloExchange(None)
    # getter
    assert halo_exchange.halo_depth is None
    # setter
    halo_exchange.halo_depth = halo_depth
    assert halo_exchange.halo_depth == halo_depth


def test_haloexchange_vector_index_depend():
    '''check that _find_read_arguments does not return a haloexchange as a
    read dependence if the source node is a halo exchange and its
    field is a vector and the other halo exchange accesses a different
    element of the vector

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.9_named_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    first_d_field_halo_exchange = schedule.children[3]
    field = first_d_field_halo_exchange.field
    all_nodes = schedule.walk(Node)
    following_nodes = all_nodes[5:]
    result_list = field._find_read_arguments(following_nodes)
    assert len(result_list) == 1
    assert result_list[0].call.name == 'ru_code'


def test_find_write_arguments_for_write():
    '''when backward_write_dependencies is called from an field argument
    that does not read then we should return an empty list. This test
    checks this functionality. We use the dynamo0p3 api to create the
    required objects

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[3]
    kernel = loop.loop_body[0]
    field_writer = kernel.arguments.args[1]
    node_list = field_writer.backward_write_dependencies()
    assert node_list == []


def test_find_w_args_hes_no_vec(monkeypatch, annexed):
    '''when backward_write_dependencies, or forward_read_dependencies, are
    called and a dependence is found between two halo exchanges, then
    the field must be a vector field. If the field is not a vector
    then an exception is raised. This test checks that the exception
    is raised correctly. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_d_v3 = schedule.children[index]
    field_d_v3 = halo_exchange_d_v3.field
    monkeypatch.setattr(field_d_v3, "_vector_size", 1)
    with pytest.raises(InternalError) as excinfo:
        _ = field_d_v3.backward_write_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'd' in two "
            "halo exchange calls. Found '1' and '3'" in str(excinfo.value))


def test_find_w_args_hes_diff_vec(monkeypatch, annexed):
    '''when backward_write_dependencies, or forward_read_dependencies, are
    called and a dependence is found between two halo exchanges, then
    the associated fields must be equal size vectors . If the fields
    are not vectors of equal size then an exception is raised. This
    test checks that the exception is raised correctly. Also test with
    and without annexed dofs being computed as this affects the
    generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_d_v3 = schedule.children[index]
    field_d_v3 = halo_exchange_d_v3.field
    monkeypatch.setattr(field_d_v3, "_vector_size", 2)
    with pytest.raises(InternalError) as excinfo:
        _ = field_d_v3.backward_write_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'd' in two "
            "halo exchange calls. Found '2' and '3'" in str(excinfo.value))


def test_find_w_args_hes_vec_idx(monkeypatch, annexed):
    '''when backward_write_dependencies, or forward_read_dependencies are
    called, and a dependence is found between two halo exchanges, then
    the vector indices of the two halo exchanges must be different. If
    the vector indices have the same value then an exception is
    raised. This test checks that the exception is raised
    correctly. Also test with and without annexed dofs being computed
    as this affects the generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_d_v3 = schedule.children[index]
    field_d_v3 = halo_exchange_d_v3.field
    halo_exchange_d_v2 = schedule.children[index-1]
    monkeypatch.setattr(halo_exchange_d_v2, "_vector_index", 3)
    with pytest.raises(InternalError) as excinfo:
        _ = field_d_v3.backward_write_dependencies()
    assert ("DataAccess:update_coverage() The halo exchange vector indices "
            "for 'd' are the same. This should never happen"
            in str(excinfo.value))


def test_find_w_args_hes_vec_no_dep():
    '''when _find_write_arguments, or _find_read_arguments, are called,
    halo exchanges with the same field but a different index should
    not depend on each other. This test checks that this behaviour is
    working correctly
    '''

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange_d_v3 = schedule.children[5]
    field_d_v3 = halo_exchange_d_v3.field
    # there are two halo exchanges before d_v3 which should not count
    # as dependencies
    node_list = field_d_v3.backward_write_dependencies()
    assert node_list == []


def test_check_vect_hes_differ_wrong_argtype():
    '''when the check_vector_halos_differ method is called from a halo
    exchange object the argument being passed should be a halo
    exchange. If this is not the case an exception should be
    raised. This test checks that this exception is working correctly.
    '''

    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        # pass an incorrect object to the method
        halo_exchange.check_vector_halos_differ(psy)
    assert (
        "the argument passed to HaloExchange.check_vector_halos_differ() "
        "is not a halo exchange object" in str(excinfo.value))


def test_check_vec_hes_differ_diff_names():
    '''when the check_vector_halos_differ method is called from a halo
    exchange object the argument being passed should be a halo
    exchange with an argument having the same name as the local halo
    exchange argument name. If this is not the case an exception
    should be raised. This test checks that this exception is working
    correctly.
    '''

    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[0]
    # obtain another halo exchange object which has an argument with a
    # different name
    different_halo_exchange = schedule.children[1]
    with pytest.raises(GenerationError) as excinfo:
        # pass halo exchange with different name to the method
        halo_exchange.check_vector_halos_differ(different_halo_exchange)
    assert (
        "the halo exchange object passed to "
        "HaloExchange.check_vector_halos_differ() has a "
        "different field name 'm1' to self 'f2'" in str(excinfo.value))


def test_find_w_args_multiple_deps_error(monkeypatch, annexed):
    '''when _find_write_arguments finds a write that causes it to return
    there should not be any previous dependencies. This test checks
    that an error is raised if this is not the case. We test with
    annexed dofs is True and False as different numbers of halo
    exchanges are created.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # create halo exchanges between the two loops via redundant
    # computation
    if annexed:
        index = 1
    else:
        index = 4
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[index], depth=2)
    del schedule.children[index]
    loop = schedule.children[index+2]
    kernel = loop.loop_body[0]
    d_field = kernel.arguments.args[0]
    with pytest.raises(InternalError) as excinfo:
        d_field.backward_write_dependencies()
    assert (
        "Found a writer dependence but there are already dependencies"
        in str(excinfo.value))


def test_find_write_arguments_no_more_nodes(monkeypatch, annexed):
    '''when _find_write_arguments has looked through all nodes but has not
    returned it should mean that is has not found any write
    dependencies. This test checks that an error is raised if this is
    not the case. We test with and without computing annexed dofs as
    different numbers of halo exchanges are created.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 3
    else:
        index = 4
    del schedule.children[index]
    loop = schedule.children[index+1]
    kernel = loop.loop_body[0]
    d_field = kernel.arguments.args[5]
    with pytest.raises(InternalError) as excinfo:
        d_field.backward_write_dependencies()
    assert (
        "no more nodes but there are already dependencies"
        in str(excinfo.value))


def test_find_w_args_multiple_deps(monkeypatch, annexed):
    '''_find_write_arguments should return as many halo exchange
    dependencies as the vector size of the associated field. This test
    checks that this is the case and that the returned objects are
    what is expected. We test with annexed dofs is True and False as
    different numbers of halo exchanges are created.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # create halo exchanges between the two loops via redundant
    # computation
    if annexed:
        index = 1
    else:
        index = 4
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[index], depth=2)
    loop = schedule.children[index+3]
    kernel = loop.loop_body[0]
    d_field = kernel.arguments.args[0]
    vector_size = d_field.vector_size
    result_list = d_field.backward_write_dependencies()
    # we have as many dependencies as the field vector size
    assert vector_size == len(result_list)
    indices = set()
    for result in result_list:
        # each dependence is a halo exchange nodes
        assert isinstance(result.call, HaloExchange)
        # the name of the halo exchange field and the initial
        # field are the same
        assert result.name == d_field.name
        # the size of the halo exchange field vector and the initial
        # field vector are the same
        assert result.vector_size == vector_size
        indices.add(result.call.vector_index)
    # each of the indices are unique (otherwise the set would be
    # smaller)
    assert len(indices) == vector_size


def test_node_abstract_methods():
    ''' Tests that the abstract methods of the Node class raise appropriate
    errors. '''
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    loop = sched.children[0].loop_body[0]
    with pytest.raises(NotImplementedError) as err:
        Node.gen_code(loop, parent=None)
    assert "Please implement me" in str(err)


def test_kern_ast():
    ''' Test that we can obtain the fparser2 AST of a kernel. '''
    from psyclone.gocean1p0 import GOKern
    from fparser.two import Fortran2003
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].loop_body[0].loop_body[0]
    assert isinstance(kern, GOKern)
    assert kern.ast
    assert isinstance(kern.ast, Fortran2003.Program)


def test_dataaccess_vector():
    '''Test that the DataAccess class works as expected when we have a
    vector field argument that depends on more than one halo exchange
    (due to halo exchanges working separately on components of
    vectors).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # d from halo exchange vector 1
    halo_exchange_d_v1 = schedule.children[3]
    field_d_v1 = halo_exchange_d_v1.field
    # d from halo exchange vector 2
    halo_exchange_d_v2 = schedule.children[4]
    field_d_v2 = halo_exchange_d_v2.field
    # d from halo exchange vector 3
    halo_exchange_d_v3 = schedule.children[5]
    field_d_v3 = halo_exchange_d_v3.field
    # d from a kernel argument
    loop = schedule.children[6]
    kernel = loop.loop_body[0]
    d_arg = kernel.arguments.args[5]

    access = DataAccess(d_arg)
    assert not access.covered

    access.update_coverage(field_d_v3)
    assert not access.covered
    access.update_coverage(field_d_v2)
    assert not access.covered

    with pytest.raises(InternalError) as excinfo:
        access.update_coverage(field_d_v3)
    assert (
        "Found more than one dependent halo exchange with the same vector "
        "index" in str(excinfo.value))

    access.update_coverage(field_d_v1)
    assert access.covered

    access.reset_coverage()
    assert not access.covered
    assert not access._vector_index_access


def test_dataaccess_same_vector_indices(monkeypatch):
    '''If update_coverage() is called from DataAccess and the arguments
    are the same vector field, and the field vector indices are the
    same then check that an exception is raised. This particular
    exception is difficult to raise as it is caught by an earlier
    method (overlaps()).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # d for this halo exchange is for vector component 2
    halo_exchange_d_v2 = schedule.children[4]
    field_d_v2 = halo_exchange_d_v2.field
    # modify d from vector component 3 to be component 2
    halo_exchange_d_v3 = schedule.children[5]
    field_d_v3 = halo_exchange_d_v3.field
    monkeypatch.setattr(halo_exchange_d_v3, "_vector_index", 2)

    # Now raise an exception with our erroneous vector indices (which
    # are the same but should not be), but first make sure that the
    # overlaps() method returns True otherwise an earlier exception
    # will be raised.
    access = DataAccess(field_d_v2)
    monkeypatch.setattr(access, "overlaps", lambda arg: True)

    with pytest.raises(InternalError) as excinfo:
        access.update_coverage(field_d_v3)
    assert (
        "The halo exchange vector indices for 'd' are the same. This should "
        "never happen" in str(excinfo.value))


# Test CodeBlock class


def test_codeblock_view(capsys):
    ''' Check the view and colored_text methods of the Code Block class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    cblock = CodeBlock([], "dummy")
    coloredtext = colored("CodeBlock", SCHEDULE_COLOUR_MAP["CodeBlock"])
    cblock.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[" in output
    assert "]" in output


def test_codeblock_can_be_printed():
    '''Test that a CodeBlock instance can always be printed (i.e. is
    initialised fully)'''
    cblock = CodeBlock([], "dummy")
    assert "CodeBlock[" in str(cblock)
    assert "]" in str(cblock)


def test_codeblock_getastnodes():
    '''Test that the get_ast_nodes method of a CodeBlock instance returns
    a copy of the list of nodes from the original AST that are associated with
    this code block.

    For simplicity we use a list of strings rather than an AST.

    '''
    original = ["hello", "there"]
    cblock = CodeBlock(original, CodeBlock.Structure.EXPRESSION)
    result = cblock.get_ast_nodes
    assert result == original
    # Check that the list is a copy not a reference.
    assert result is not original


@pytest.mark.parametrize("structure", [CodeBlock.Structure.STATEMENT,
                                       CodeBlock.Structure.EXPRESSION])
def test_codeblock_structure(structure):
    '''Check that the structure property in the CodeBlock class is set to
    the provided value.

    '''
    cblock = CodeBlock([], structure)
    assert cblock.structure == structure

# Test Loop class


def test_loop_navigation_properties():
    ''' Tests the start_expr, stop_expr, step_expr and loop_body
    setter and getter properties'''
    from psyclone.psyGen import Loop
    loop = Loop()

    # Properties return an error if the node is incomplete
    error_str = ("Loop malformed or incomplete. It should have exactly 4 "
                 "children, but found")
    with pytest.raises(InternalError) as err:
        _ = loop.start_expr
    assert error_str in str(err.value)

    # Expressions that are not PSyIR are not accepted
    with pytest.raises(TypeError) as err:
        loop.start_expr = "start"
    assert "Only PSyIR nodes can be assigned as the Loop start expression" \
        ", but found '" in str(err.value)
    with pytest.raises(TypeError) as err:
        loop.stop_expr = "stop"
    assert "Only PSyIR nodes can be assigned as the Loop stop expression" \
        ", but found '" in str(err.value)
    with pytest.raises(TypeError) as err:
        loop.step_expr = "step"
    assert "Only PSyIR nodes can be assigned as the Loop step expression" \
        ", but found '" in str(err.value)

    loop.addchild(Literal("start", parent=loop))
    loop.addchild(Literal("stop", parent=loop))
    loop.addchild(Literal("step", parent=loop))

    # If it's not fully complete, it still returns an error
    with pytest.raises(InternalError) as err:
        _ = loop.start_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.stop_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.step_expr
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        _ = loop.loop_body
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.start_expr = Literal("invalid", parent=loop)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.stop_expr = Literal("invalid", parent=loop)
    assert error_str in str(err.value)
    with pytest.raises(InternalError) as err:
        loop.step_expr = Literal("invalid", parent=loop)
    assert error_str in str(err.value)

    # The fourth child has to be a Schedule
    loop.addchild(Literal("loop_body", parent=loop))
    with pytest.raises(InternalError) as err:
        _ = loop.loop_body
    assert "Loop malformed or incomplete. Fourth child should be a " \
        "Schedule node, but found loop with " in str(err.value)

    # Fix loop and check that Getters properties work
    del loop.children[3]
    loop.addchild(Schedule(parent=loop))
    loop.loop_body.addchild(Return(parent=loop.loop_body))

    assert loop.start_expr.value == "start"
    assert loop.stop_expr.value == "stop"
    assert loop.step_expr.value == "step"
    assert isinstance(loop.loop_body[0], Return)

    # Test Setters
    loop.start_expr = Literal("newstart", parent=loop)
    loop.stop_expr = Literal("newstop", parent=loop)
    loop.step_expr = Literal("newstep", parent=loop)

    assert loop.start_expr.value == "newstart"
    assert loop.stop_expr.value == "newstop"
    assert loop.step_expr.value == "newstep"


def test_loop_invalid_type():
    ''' Tests assigning an invalid type to a Loop object. '''
    from psyclone.psyGen import Loop
    _, invoke = get_invoke("single_invoke.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    loop = sched.children[0].loop_body[0]
    assert isinstance(loop, Loop)
    with pytest.raises(GenerationError) as err:
        loop.loop_type = "not_a_valid_type"
    assert ("loop_type value (not_a_valid_type) is invalid. Must be one of "
            "['inner', 'outer']" in str(err))


def test_loop_gen_code():
    ''' Check that the Loop gen_code method prints the proper loop '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # By default DynLoop has step = 1 and it is not printed in the Fortran DO
    gen = str(psy.gen)
    assert "DO cell=1,mesh%get_last_halo_cell(1)" in gen

    # Change step to 2
    loop = psy.invokes.get('invoke_important_invoke').schedule[3]
    loop.step_expr = Literal("2", parent=loop)

    # Now it is printed in the Fortran DO with the expression  ",2" at the end
    gen = str(psy.gen)
    assert "DO cell=1,mesh%get_last_halo_cell(1),2" in gen


# Test IfBlock class

def test_ifblock_invalid_annotation():
    ''' Test that initialising IfBlock with invalid annotations produce the
    expected error.'''

    with pytest.raises(InternalError) as err:
        _ = IfBlock(annotation="invalid")
    assert ("IfBlock with unrecognized annotation 'invalid', valid "
            "annotations are:") in str(err.value)


def test_ifblock_view(capsys):
    ''' Check the view and colored_text methods of the IfBlock class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP

    coloredtext = colored("If", SCHEDULE_COLOUR_MAP["If"])

    ifblock = IfBlock()
    ifblock.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[]" in output

    ifblock = IfBlock(annotation='was_elseif')
    ifblock.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[annotations='was_elseif']" in output


def test_ifblock_can_be_printed():
    '''Test that an IfBlock instance can always be printed (i.e. is
    initialised fully)'''
    ifblock = IfBlock()
    ref1 = Reference('condition1', parent=ifblock)
    ifblock.addchild(ref1)
    sch = Schedule(parent=ifblock)
    ifblock.addchild(sch)
    ret = Return(parent=sch)
    sch.addchild(ret)

    assert "If[]\n" in str(ifblock)
    assert "condition1" in str(ifblock)  # Test condition is printed
    assert "Return[]" in str(ifblock)  # Test if_body is printed


def test_ifblock_properties():
    '''Test that an IfBlock node properties can be retrieved'''
    ifblock = IfBlock()

    # Condition can't be retrieved before is added as a child.
    with pytest.raises(InternalError) as err:
        _ = ifblock.condition
    assert("IfBlock malformed or incomplete. It should have "
           "at least 2 children, but found 0." in str(err.value))

    ref1 = Reference('condition1', parent=ifblock)
    ifblock.addchild(ref1)

    # If_body can't be retrieved before is added as a child.
    with pytest.raises(InternalError) as err:
        _ = ifblock.if_body
    assert("IfBlock malformed or incomplete. It should have "
           "at least 2 children, but found 1." in str(err.value))

    sch = Schedule(parent=ifblock)
    ifblock.addchild(sch)
    ret = Return(parent=sch)
    sch.addchild(ret)

    # Now we can retrieve the condition and the if_body, but else is empty
    assert ifblock.condition is ref1
    assert ifblock.if_body[0] is ret
    assert not ifblock.else_body

    sch2 = Schedule(parent=ifblock)
    ifblock.addchild(sch2)
    ret2 = Return(parent=sch2)
    sch2.addchild(ret2)

    # Now we can retrieve else_body
    assert ifblock.else_body[0] is ret2


# Test Assignment class

def test_assignment_view(capsys):
    ''' Check the view and colored_text methods of the Assignment class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP

    assignment = Assignment()
    coloredtext = colored("Assignment", SCHEDULE_COLOUR_MAP["Assignment"])
    assignment.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[]" in output


def test_assignment_can_be_printed():
    '''Test that an Assignment instance can always be printed (i.e. is
    initialised fully)'''
    assignment = Assignment()
    assert "Assignment[]\n" in str(assignment)


def test_assignment_semantic_navigation():
    '''Test that the Assignment navigation properties reference the expected
    children'''
    assignment = Assignment()

    # lhs should fail if first child is not present
    with pytest.raises(InternalError) as err:
        _ = assignment.lhs
    assert "' malformed or incomplete. It needs at least 1 child to have " \
        "a lhs." in str(err)

    ref = Reference("a", assignment)
    assignment.addchild(ref)

    # rhs should fail if second child is not present
    with pytest.raises(InternalError) as err:
        _ = assignment.rhs
    assert " malformed or incomplete. It needs at least 2 children to have " \
        "a rhs." in str(err)

    lit = Literal("1", assignment)
    assignment.addchild(lit)
    assert assignment.lhs is assignment._children[0]
    assert assignment.rhs is assignment._children[1]


# Test Reference class


def test_reference_view(capsys):
    ''' Check the view and colored_text methods of the Reference class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    kschedule.symbol_table.add(Symbol("rname", "integer"))
    assignment = Assignment(parent=kschedule)
    ref = Reference("rname", assignment)
    coloredtext = colored("Reference", SCHEDULE_COLOUR_MAP["Reference"])
    ref.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[name:'rname']" in output


def test_reference_can_be_printed():
    '''Test that a Reference instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    kschedule.symbol_table.add(Symbol("rname", "integer"))
    assignment = Assignment(parent=kschedule)
    ref = Reference("rname", assignment)
    assert "Reference[name:'rname']" in str(ref)


def test_reference_symbol(monkeypatch):
    '''Test that the symbol method in a Reference Node instance returns
    the associated symbol if there is one and None if not. Also test
    for an incorrect scope argument.

    '''
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel_schedule = kernels[0].get_kernel_schedule()
    references = kernel_schedule.walk(Reference)

    # Symbol in KernelSchedule SymbolTable
    field_old = references[0]
    assert field_old.name == "field_old"
    assert isinstance(field_old.symbol(), Symbol)
    assert field_old.symbol().name == field_old.name

    # Symbol in KernelSchedule SymbolTable with KernelSchedule scope
    assert isinstance(field_old.symbol(scope_limit=kernel_schedule), Symbol)
    assert field_old.symbol().name == field_old.name

    # Symbol in KernelSchedule SymbolTable with parent scope
    assert field_old.symbol(scope_limit=field_old.parent) is None

    # Symbol in Container SymbolTable
    alpha = references[6]
    assert alpha.name == "alpha"
    assert isinstance(alpha.symbol(), Symbol)
    assert alpha.symbol().name == alpha.name

    # Symbol in Container SymbolTable with KernelSchedule scope
    assert alpha.symbol(scope_limit=kernel_schedule) is None

    # Symbol in Container SymbolTable with Container scope
    assert isinstance(kernel_schedule.root, Container)
    assert alpha.symbol(scope_limit=kernel_schedule.root).name == alpha.name

    # Symbol method with invalid scope type
    with pytest.raises(TypeError) as excinfo:
        _ = alpha.symbol(scope_limit="hello")
    assert ("The scope_limit argument 'hello' provided to the symbol method, "
            "is not of type `Node`." in str(excinfo.value))

    # Symbol method with invalid scope location
    with pytest.raises(ValueError) as excinfo:
        _ = alpha.symbol(scope_limit=alpha)
    assert ("The scope_limit node 'Reference[name:'alpha']' provided to the "
            "symbol method, is not an ancestor of this reference node "
            "'Reference[name:'alpha']'." in str(excinfo.value))

    # Symbol not in any container (rename alpha to something that is
    # not defined)
    monkeypatch.setattr(alpha, "_reference", "not_defined")
    assert not alpha.symbol()

# Test Array class


def test_array_view(capsys):
    ''' Check the view and colored_text methods of the Array class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    kschedule.symbol_table.add(Symbol("aname", "integer", [None]))
    assignment = Assignment(parent=kschedule)
    array = Array("aname", parent=assignment)
    coloredtext = colored("ArrayReference", SCHEDULE_COLOUR_MAP["Reference"])
    array.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[name:'aname']" in output


def test_array_can_be_printed():
    '''Test that an Array instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    kschedule.symbol_table.add(Symbol("aname", "integer"))
    assignment = Assignment(parent=kschedule)
    array = Array("aname", assignment)
    assert "ArrayReference[name:'aname']\n" in str(array)


# Test Literal class
def test_literal_value():
    '''Test the value property returns the value of the Literal object.

    '''
    literal = Literal("1")
    assert literal.value == "1"


def test_literal_view(capsys):
    ''' Check the view and colored_text methods of the Literal class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    literal = Literal("1")
    coloredtext = colored("Literal", SCHEDULE_COLOUR_MAP["Literal"])
    literal.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[value:'1']" in output


def test_literal_can_be_printed():
    '''Test that an Literal instance can always be printed (i.e. is
    initialised fully)'''
    literal = Literal("1")
    assert "Literal[value:'1']" in str(literal)


# Test BinaryOperation class
def test_binaryoperation_initialization():
    ''' Check the initialization method of the BinaryOperation class works
    as expected.'''

    with pytest.raises(TypeError) as err:
        _ = BinaryOperation("not an operator")
    assert "BinaryOperation operator argument must be of type " \
           "BinaryOperation.Operator but found" in str(err)
    bop = BinaryOperation(BinaryOperation.Operator.ADD)
    assert bop._operator is BinaryOperation.Operator.ADD


def test_binaryoperation_operator():
    '''Test that the operator property returns the binaryoperator in the
    binaryoperation.

    '''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert binary_operation.operator == BinaryOperation.Operator.ADD


def test_binaryoperation_view(capsys):
    ''' Check the view and colored_text methods of the Binary Operation
    class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    op1 = Literal("1", parent=binary_operation)
    op2 = Literal("1", parent=binary_operation)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    coloredtext = colored("BinaryOperation",
                          SCHEDULE_COLOUR_MAP["Operation"])
    binary_operation.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[operator:'ADD']" in output


def test_binaryoperation_can_be_printed():
    '''Test that a Binary Operation instance can always be printed (i.e. is
    initialised fully)'''
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    assert "BinaryOperation[operator:'ADD']" in str(binary_operation)
    op1 = Literal("1", parent=binary_operation)
    op2 = Literal("2", parent=binary_operation)
    binary_operation.addchild(op1)
    binary_operation.addchild(op2)
    # Check the node children are also printed
    assert "Literal[value:'1']\n" in str(binary_operation)
    assert "Literal[value:'2']" in str(binary_operation)


# Test UnaryOperation class
def test_unaryoperation_initialization():
    ''' Check the initialization method of the UnaryOperation class works
    as expected.'''

    with pytest.raises(TypeError) as err:
        _ = UnaryOperation("not an operator")
    assert "UnaryOperation operator argument must be of type " \
           "UnaryOperation.Operator but found" in str(err)
    uop = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert uop._operator is UnaryOperation.Operator.MINUS


def test_unaryoperation_operator():
    '''Test that the operator property returns the unaryoperator in the
    unaryoperation.

    '''
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert unary_operation.operator == UnaryOperation.Operator.MINUS


def test_unaryoperation_view(capsys):
    ''' Check the view and colored_text methods of the UnaryOperation
    class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    ref1 = Reference("a", parent=unary_operation)
    unary_operation.addchild(ref1)
    coloredtext = colored("UnaryOperation",
                          SCHEDULE_COLOUR_MAP["Operation"])
    unary_operation.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[operator:'MINUS']" in output


def test_unaryoperation_can_be_printed():
    '''Test that a UnaryOperation instance can always be printed (i.e. is
    initialised fully)'''
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    assert "UnaryOperation[operator:'MINUS']" in str(unary_operation)
    op1 = Literal("1", parent=unary_operation)
    unary_operation.addchild(op1)
    # Check the node children are also printed
    assert "Literal[value:'1']" in str(unary_operation)


def test_naryoperation_view(capsys):
    ''' Check the view and colored_text methods of the Nary Operation
    class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    nary_operation.addchild(Literal("1", parent=nary_operation))
    nary_operation.addchild(Literal("1", parent=nary_operation))
    nary_operation.addchild(Literal("1", parent=nary_operation))

    coloredtext = colored("NaryOperation",
                          SCHEDULE_COLOUR_MAP["Operation"])
    nary_operation.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[operator:'MAX']" in output


def test_naryoperation_can_be_printed():
    '''Test that an Nary Operation instance can always be printed (i.e. is
    initialised fully)'''
    nary_operation = NaryOperation(NaryOperation.Operator.MAX)
    assert "NaryOperation[operator:'MAX']" in str(nary_operation)
    nary_operation.addchild(Literal("1", parent=nary_operation))
    nary_operation.addchild(Literal("2", parent=nary_operation))
    nary_operation.addchild(Literal("3", parent=nary_operation))
    # Check the node children are also printed
    assert "Literal[value:'1']\n" in str(nary_operation)
    assert "Literal[value:'2']\n" in str(nary_operation)
    assert "Literal[value:'3']" in str(nary_operation)


# Test Return class

def test_return_view(capsys):
    ''' Check the view and colored_text methods of the Return class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    return_stmt = Return()
    coloredtext = colored("Return", SCHEDULE_COLOUR_MAP["Return"])
    return_stmt.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[]" in output


def test_return_can_be_printed():
    '''Test that a Return instance can always be printed (i.e. is
    initialised fully)'''
    return_stmt = Return()
    assert "Return[]\n" in str(return_stmt)


# Test Container class

def test_container_init():
    '''Test that a container is initialised as expected.'''
    container = Container("test")
    assert container._name == "test"
    assert container._parent is None
    assert isinstance(container._symbol_table, SymbolTable)


def test_container_init_parent():
    '''Test that a container parent argument is stored as expected.'''
    container = Container("test", parent="hello")
    assert container.parent == "hello"


def test_container_name():
    '''Test that the container name can be set and changed as
    expected.'''
    container = Container("test")
    assert container.name == "test"
    container.name = "new_test"
    assert container.name == "new_test"


def test_container_symbol_table():
    '''Test that the container symbol_table method returns the expected
    content.'''
    container = Container("test")
    assert isinstance(container._symbol_table, SymbolTable)
    assert container.symbol_table is container._symbol_table


def test_container_view(capsys):
    '''Check the view and colored_text methods of the Container class.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    cont_stmt = Container("bin")
    coloredtext = colored("Container", SCHEDULE_COLOUR_MAP["Container"])
    cont_stmt.view()
    output, _ = capsys.readouterr()
    assert coloredtext+"[bin]" in output


def test_container_can_be_printed():
    '''Test that a Container instance can always be printed (i.e. is
    initialised fully)'''
    cont_stmt = Container("box")
    assert "Container[box]\n" in str(cont_stmt)


# Test KernelSchedule Class

def test_kernelschedule_view(capsys):
    '''Test the view method of the KernelSchedule part.'''
    from psyclone.psyGen import colored, SCHEDULE_COLOUR_MAP
    kschedule = KernelSchedule("kname")
    kschedule.symbol_table.add(Symbol("x", "integer"))
    assignment = Assignment()
    kschedule.addchild(assignment)
    lhs = Reference("x", parent=assignment)
    rhs = Literal("1", parent=assignment)
    assignment.addchild(lhs)
    assignment.addchild(rhs)
    kschedule.view()
    coloredtext = colored("Schedule",
                          SCHEDULE_COLOUR_MAP["Schedule"])
    output, _ = capsys.readouterr()
    assert coloredtext+"[name:'kname']" in output
    assert "Assignment" in output  # Check child view method is called


def test_kernelschedule_can_be_printed():
    '''Test that a KernelSchedule instance can always be printed (i.e. is
    initialised fully)'''
    kschedule = KernelSchedule("kname")
    kschedule.symbol_table.add(Symbol("x", "integer"))
    assignment = Assignment()
    kschedule.addchild(assignment)
    lhs = Reference("x", parent=assignment)
    rhs = Literal("1", parent=assignment)
    assignment.addchild(lhs)
    assignment.addchild(rhs)
    assert "Schedule[name:'kname']:\n" in str(kschedule)
    assert "Assignment" in str(kschedule)  # Check children are printed
    assert "End KernelSchedule" in str(kschedule)


def test_kernelschedule_name_setter():
    '''Test that the name setter changes the kernel name attribute.'''
    kschedule = KernelSchedule("kname")
    assert kschedule.name == "kname"
    kschedule.name = "newname"
    assert kschedule.name == "newname"


# Test Symbol Class
def test_symbol_initialisation():
    '''Test that a Symbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions.'''

    # Test with valid arguments
    assert isinstance(Symbol('a', 'real'), Symbol)
    # real constants are not currently supported
    assert isinstance(Symbol('a', 'integer'), Symbol)
    assert isinstance(Symbol('a', 'integer', constant_value=0), Symbol)
    assert isinstance(Symbol('a', 'character'), Symbol)
    assert isinstance(Symbol('a', 'character', constant_value="hello"), Symbol)
    assert isinstance(Symbol('a', 'boolean'), Symbol)
    assert isinstance(Symbol('a', 'boolean', constant_value=False), Symbol)
    assert isinstance(Symbol('a', 'real', [None]), Symbol)
    assert isinstance(Symbol('a', 'real', [3]), Symbol)
    assert isinstance(Symbol('a', 'real', [3, None]), Symbol)
    assert isinstance(Symbol('a', 'real', []), Symbol)
    assert isinstance(Symbol('a', 'real', [], interface=Symbol.Argument()),
                      Symbol)
    assert isinstance(
        Symbol('a', 'real', [],
               interface=Symbol.Argument(access=Symbol.Access.READWRITE)),
        Symbol)
    assert isinstance(
        Symbol('a', 'real', [],
               interface=Symbol.Argument(access=Symbol.Access.READ)),
        Symbol)
    assert isinstance(
        Symbol('a', 'deferred',
               interface=Symbol.FortranGlobal(access=Symbol.Access.READ,
                                              module_use='some_mod')),
        Symbol)
    dim = Symbol('dim', 'integer', [])
    assert isinstance(Symbol('a', 'real', [dim]), Symbol)
    assert isinstance(Symbol('a', 'real', [3, dim, None]), Symbol)

    # Test with invalid arguments
    with pytest.raises(NotImplementedError) as error:
        Symbol('a', 'invalidtype', [], 'local')
    assert (
        "Symbol can only be initialised with {0} datatypes but found "
        "'invalidtype'.".format(str(Symbol.valid_data_types))) in str(
            error.value)

    with pytest.raises(ValueError) as error:
        Symbol('a', 'real', constant_value=3.14)
    assert ("A constant value is not currently supported for datatype "
            "'real'.") in str(error)

    with pytest.raises(TypeError) as error:
        Symbol('a', 'real', shape=dim)
    assert "Symbol shape attribute must be a list." in str(error.value)

    with pytest.raises(TypeError) as error:
        Symbol('a', 'real', ['invalidshape'])
    assert ("Symbol shape list elements can only be 'Symbol', "
            "'integer' or 'None'.") in str(error.value)

    with pytest.raises(TypeError) as error:
        bad_dim = Symbol('dim', 'real', [])
        Symbol('a', 'real', [bad_dim], 'local')
    assert ("Symbols that are part of another symbol shape can "
            "only be scalar integers, but found") in str(error.value)

    with pytest.raises(TypeError) as error:
        bad_dim = Symbol('dim', 'integer', [3])
        Symbol('a', 'real', [bad_dim], 'local')
    assert ("Symbols that are part of another symbol shape can "
            "only be scalar integers, but found") in str(error.value)

    with pytest.raises(ValueError) as error:
        Symbol('a', 'integer', interface=Symbol.Argument(), constant_value=9)
    assert ("Symbol with a constant value is currently limited to having "
            "local scope but found 'global'.") in str(error)

    with pytest.raises(ValueError) as error:
        Symbol('a', 'integer', shape=[None], constant_value=9)
    assert ("Symbol with a constant value must be a scalar but the shape "
            "attribute is not empty.") in str(error)

    with pytest.raises(ValueError) as error:
        Symbol('a', 'integer', constant_value=9.81)
    assert ("This Symbol instance's datatype is 'integer' which means the "
            "constant value is expected to be") in str(error)
    assert "'int'>' but found " in str(error)
    assert "'float'>'." in str(error)

    with pytest.raises(ValueError) as error:
        Symbol('a', 'character', constant_value=42)
    assert ("This Symbol instance's datatype is 'character' which means the "
            "constant value is expected to be") in str(error)
    assert "'str'>' but found " in str(error)
    assert "'int'>'." in str(error)

    with pytest.raises(ValueError) as error:
        Symbol('a', 'boolean', constant_value="hello")
    assert ("This Symbol instance's datatype is 'boolean' which means the "
            "constant value is expected to be") in str(error)
    assert "'bool'>' but found " in str(error)
    assert "'str'>'." in str(error)


def test_symbol_map():
    '''Test the mapping variable in the Symbol class does not raise any
    exceptions when it is used with the valid_data_types variable in
    the Symbol class.

    '''
    # "real" and "deferred" are not supported in the mapping so we expect
    # it to have 2 fewer entries than there are valid data types
    assert len(Symbol.valid_data_types) == len(Symbol.mapping) + 2
    for data_type in Symbol.valid_data_types:
        if data_type not in ["real", "deferred"]:
            assert data_type in Symbol.mapping


def test_symbol_can_be_printed():
    '''Test that a Symbol instance can always be printed. (i.e. is
    initialised fully.)'''
    symbol = Symbol("sname", "real")
    assert "sname: <real, Scalar, local>" in str(symbol)

    sym1 = Symbol("s1", "integer")
    assert "s1: <integer, Scalar, local>" in str(sym1)

    sym2 = Symbol("s2", "real", [None, 2, sym1])
    assert "s2: <real, Array['Unknown bound', 2, s1], local>" in str(sym2)

    sym3 = Symbol("s3", "real",
                  interface=Symbol.FortranGlobal(module_use="my_mod"))
    assert ("s3: <real, Scalar, global=FortranModule(my_mod)"
            in str(sym3))

    sym2._shape.append('invalid')
    with pytest.raises(InternalError) as error:
        _ = str(sym2)
    assert ("Symbol shape list elements can only be 'Symbol', 'integer' or "
            "'None', but found") in str(error.value)

    sym3 = Symbol("s3", "integer", constant_value=12)
    assert "s3: <integer, Scalar, local, constant_value=12>" in str(sym3)


def test_symbol_constant_value_setter():
    '''Test that a Symbol constant value can be set if given a new valid
    constant value. Also test that is_constant returns True

    '''

    # Test with valid constant value
    sym = Symbol('a', 'integer', constant_value=7)
    assert sym.constant_value == 7
    sym.constant_value = 9
    assert sym.constant_value == 9


def test_symbol_is_constant():
    '''Test that the Symbol is_constant property returns True if a
    constant value is set and False if it is not.

    '''
    sym = Symbol('a', 'integer')
    assert not sym.is_constant
    sym.constant_value = 9
    assert sym.is_constant


def test_symbol_scalar_array():
    '''Test that the Symbol property is_scalar returns True if the Symbol
    is a scalar and False if not and that the Symbol property is_array
    returns True if the Symbol is an array and False if not.

    '''
    sym1 = Symbol("s1", "integer")
    sym2 = Symbol("s2", "real", [None, 2, sym1])
    assert sym1.is_scalar
    assert not sym1.is_array
    assert not sym2.is_scalar
    assert sym2.is_array


def test_symbol_invalid_interface():
    ''' Check that the Symbol.interface setter rejects the supplied value if
    it is not a SymbolInterface. '''
    sym = Symbol("some_var", "real")
    with pytest.raises(TypeError) as err:
        sym.interface = "invalid interface spec"
    assert ("interface to a Symbol must be a SymbolInterface or None but"
            in str(err))


def test_symbol_interface():
    ''' Check the interface getter on a Symbol. '''
    symbol = Symbol("some_var", "real",
                    interface=Symbol.FortranGlobal(module_use="my_mod"))
    assert symbol.interface.module_name == "my_mod"


def test_symbol_interface_access():
    ''' Tests for the SymbolInterface.access setter. '''
    symbol = Symbol("some_var", "real",
                    interface=Symbol.FortranGlobal(module_use="my_mod"))
    symbol.interface.access = Symbol.Access.READ
    assert symbol.interface.access == Symbol.Access.READ
    # Force the error by supplying a string instead of a SymbolAccess type.
    with pytest.raises(TypeError) as err:
        symbol.interface.access = "read"
    assert "must be a 'Symbol.Access' but got " in str(err)


def test_symbol_argument_str():
    ''' Check the __str__ method of the Symbol.Argument class. '''
    # A Symbol.Argument represents a routine argument by default.
    interface = Symbol.Argument()
    assert str(interface) == "Argument(pass-by-value=False)"


def test_fortranglobal_str():
    ''' Test the __str__ method of Symbol.FortranGlobal. '''
    # If it's not an argument then we have nothing else to say about it (since
    # other options are language specific and are implemented in sub-classes).
    interface = Symbol.FortranGlobal("my_mod")
    assert str(interface) == "FortranModule(my_mod)"


def test_fortranglobal_modname():
    ''' Test the FortranGlobal.module_name setter error conditions. '''
    with pytest.raises(ValueError) as err:
        _ = Symbol.FortranGlobal("")
    assert "module_name must be one or more characters long" in str(err)
    with pytest.raises(TypeError) as err:
        _ = Symbol.FortranGlobal(1)
    assert "module_name must be a str but got" in str(err)


def test_symbol_copy():
    '''Test that the Symbol copy method produces a faithful separate copy
    of the original symbol.

    '''
    symbol = Symbol("myname", "real", shape=[1, 2], constant_value=None,
                    interface=Symbol.Argument(access=Symbol.Access.READWRITE))
    new_symbol = symbol.copy()

    # Check the new symbol has the same properties as the original
    assert symbol.name == new_symbol.name
    assert symbol.datatype == new_symbol.datatype
    assert symbol.shape == new_symbol.shape
    assert symbol.scope == new_symbol.scope
    assert symbol.constant_value == new_symbol.constant_value
    assert symbol.interface == new_symbol.interface

    # Change the properties of the new symbol and check the original
    # is not affected. Can't check constant_value yet as we have a
    # shape value
    new_symbol._name = "new"
    new_symbol._datatype = "integer"
    new_symbol.shape[0] = 3
    new_symbol.shape[1] = 4
    new_symbol._interface = None

    assert symbol.name == "myname"
    assert symbol.datatype == "real"
    assert symbol.shape == [1, 2]
    assert symbol.scope == "global"
    assert not symbol.constant_value

    # Now check constant_value
    new_symbol._shape = []
    new_symbol.constant_value = True

    assert symbol.shape == [1, 2]
    assert not symbol.constant_value


def test_symbol_copy_properties():
    '''Test that the Symbol copy_properties method works as expected.'''

    symbol = Symbol("myname", "real", shape=[1, 2], constant_value=None,
                    interface=Symbol.Argument(access=Symbol.Access.READWRITE))

    # Check an exception is raised if an incorrect argument is passed
    # in
    with pytest.raises(TypeError) as excinfo:
        symbol.copy_properties(None)
    assert ("Argument should be of type 'Symbol' but found 'NoneType'."
            "") in str(excinfo.value)

    new_symbol = Symbol("other_name", "integer", shape=[], constant_value=7)

    symbol.copy_properties(new_symbol)

    assert symbol.name == "myname"
    assert symbol.datatype == "integer"
    assert symbol.shape == []
    assert symbol.scope == "local"
    assert symbol.constant_value == 7


# Test SymbolTable Class

def test_symboltable_add():
    '''Test that the add method inserts new symbols in the symbol
    table, but raises appropiate errors when provided with wrong parameters
    or duplicate declarations.'''
    sym_table = SymbolTable()

    # Declare a symbol
    sym_table.add(Symbol("var1", "real", shape=[5, 1],
                         interface=Symbol.FortranGlobal(
                             access=Symbol.Access.READWRITE,
                             module_use="some_mod")))
    assert sym_table._symbols["var1"].name == "var1"
    assert sym_table._symbols["var1"].datatype == "real"
    assert sym_table._symbols["var1"].shape == [5, 1]
    assert sym_table._symbols["var1"].scope == "global"
    assert sym_table._symbols["var1"].access is Symbol.Access.READWRITE
    assert sym_table._symbols["var1"].interface.module_name == "some_mod"

    # Declare a duplicate name symbol
    with pytest.raises(KeyError) as error:
        sym_table.add(Symbol("var1", "real"))
    assert ("Symbol table already contains a symbol with name "
            "'var1'.") in str(error.value)


def test_symboltable_swap_symbol_properties():
    ''' Test the symboltable swap_properties method '''

    symbol1 = Symbol("var1", "integer", shape=[], constant_value=7)
    symbol2 = Symbol("dim1", "integer",
                     interface=Symbol.Argument(access=Symbol.Access.READ))
    symbol3 = Symbol("dim2", "integer",
                     interface=Symbol.Argument(access=Symbol.Access.READ))
    symbol4 = Symbol("var2", "real", shape=[symbol2, symbol3],
                     interface=Symbol.Argument(access=Symbol.Access.READWRITE))
    sym_table = SymbolTable()
    sym_table.add(symbol1)

    # Raise exception if the first argument is not a symbol
    with pytest.raises(TypeError) as excinfo:
        sym_table.swap_symbol_properties(None, symbol1)
    assert ("Arguments should be of type 'Symbol' but found 'NoneType'."
            "") in str(excinfo.value)

    # Raise exception if the second argument is not a symbol
    with pytest.raises(TypeError) as excinfo:
        sym_table.swap_symbol_properties(symbol1, "symbol")
    assert ("Arguments should be of type 'Symbol' but found 'str'."
            "") in str(excinfo.value)

    # Raise exception if the first symbol does not exist in the symbol table
    with pytest.raises(KeyError) as excinfo:
        sym_table.swap_symbol_properties(symbol4, symbol1)
    assert "Symbol 'var2' is not in the symbol table." in str(excinfo.value)

    # Raise exception if the second symbol does not exist in the symbol table
    with pytest.raises(KeyError) as excinfo:
        sym_table.swap_symbol_properties(symbol1, symbol4)
    assert "Symbol 'var2' is not in the symbol table." in str(excinfo.value)

    # Raise exception if both symbols have the same name
    with pytest.raises(ValueError) as excinfo:
        sym_table.swap_symbol_properties(symbol1, symbol1)
    assert("The symbols should have different names, but found 'var1' for "
           "both.") in str(excinfo.value)

    sym_table.add(symbol2)
    sym_table.add(symbol3)
    sym_table.add(symbol4)
    sym_table.specify_argument_list([symbol2, symbol3, symbol4])

    # Check that properties are swapped
    sym_table.swap_symbol_properties(symbol1, symbol4)

    assert symbol1.name == "var1"
    assert symbol1.datatype == "real"
    assert symbol1.shape == [symbol2, symbol3]
    assert symbol1.scope == "global"
    assert symbol1.constant_value is None
    assert symbol1.interface.access == Symbol.Access.READWRITE

    assert symbol4.name == "var2"
    assert symbol4.datatype == "integer"
    assert not symbol4.shape
    assert symbol4.scope == "local"
    assert symbol4.constant_value == 7
    assert not symbol4.interface

    # Check symbol references are unaffected
    sym_table.swap_symbol_properties(symbol2, symbol3)
    assert symbol1.shape[0].name == "dim1"
    assert symbol1.shape[1].name == "dim2"

    # Check argument positions are updated. The original positions
    # were [dim1, dim2, var2]. They should now be [dim2, dim1, var1]
    assert sym_table.argument_list[0].name == "dim2"
    assert sym_table.argument_list[1].name == "dim1"
    assert sym_table.argument_list[2].name == "var1"


def test_symboltable_lookup():
    '''Test that the lookup method retrieves symbols from the symbol table
    if the name exists, otherwise it raises an error.'''
    sym_table = SymbolTable()
    sym_table.add(Symbol("var1", "real", shape=[None, None]))
    sym_table.add(Symbol("var2", "integer", shape=[]))
    sym_table.add(Symbol("var3", "real", shape=[]))

    assert isinstance(sym_table.lookup("var1"), Symbol)
    assert sym_table.lookup("var1").name == "var1"
    assert isinstance(sym_table.lookup("var2"), Symbol)
    assert sym_table.lookup("var2").name == "var2"
    assert isinstance(sym_table.lookup("var3"), Symbol)
    assert sym_table.lookup("var3").name == "var3"

    with pytest.raises(KeyError) as error:
        sym_table.lookup("notdeclared")
    assert "Could not find 'notdeclared' in the Symbol Table." in \
        str(error.value)


def test_symboltable_view(capsys):
    '''Test the view method of the SymbolTable class, it should print to
    standard out a representation of the full SymbolTable.'''
    sym_table = SymbolTable()
    sym_table.add(Symbol("var1", "real"))
    sym_table.add(Symbol("var2", "integer"))
    sym_table.view()
    output, _ = capsys.readouterr()
    assert "Symbol Table:\n" in output
    assert "var1" in output
    assert "var2" in output


def test_symboltable_can_be_printed():
    '''Test that a SymbolTable instance can always be printed. (i.e. is
    initialised fully)'''
    sym_table = SymbolTable()
    sym_table.add(Symbol("var1", "real"))
    sym_table.add(Symbol("var2", "integer"))
    sym_table.add(Symbol("var3", "deferred",
                         interface=Symbol.FortranGlobal(module_use="my_mod")))
    sym_table_text = str(sym_table)
    assert "Symbol Table:\n" in sym_table_text
    assert "var1" in sym_table_text
    assert "var2" in sym_table_text
    assert "FortranModule(my_mod)" in sym_table_text


def test_symboltable_specify_argument_list():
    '''Test that the specify argument list method sets the argument_list
    with references to each Symbol and updates the Symbol attributes when
    needed.'''
    sym_table = SymbolTable()
    sym_v1 = Symbol("var1", "real", [])
    sym_table.add(sym_v1)
    sym_table.add(Symbol("var2", "real", []))
    sym_v1.interface = Symbol.Argument(access=Symbol.Access.UNKNOWN)
    sym_table.specify_argument_list([sym_v1])

    assert len(sym_table.argument_list) == 1
    assert sym_table.argument_list[0].scope == 'global'
    assert sym_table.argument_list[0].access == Symbol.Access.UNKNOWN

    # Test that repeated calls still produce a valid argument list
    sym_table.specify_argument_list([sym_v1])
    assert len(sym_table.argument_list) == 1

    # Check that specifying the Interface allows us to specify how
    # the argument is accessed
    sym_v2 = sym_table.lookup("var2")
    sym_v2.interface = Symbol.Argument(access=Symbol.Access.READWRITE)
    sym_table.specify_argument_list([sym_v1, sym_v2])
    assert sym_table.argument_list[1].scope == 'global'
    assert sym_table.argument_list[1].access == Symbol.Access.READWRITE


def test_symboltable_specify_argument_list_errors():
    ''' Check that supplying specify_argument_list() with Symbols that
    don't have the correct Interface information raises the expected
    errors. '''
    sym_table = SymbolTable()
    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", []))
    sym_v1 = sym_table.lookup("var1")
    # Attempt to say the argument list consists of "var1" which at this
    # point is just a local variable.
    with pytest.raises(ValueError) as err:
        sym_table.specify_argument_list([sym_v1])
    assert "Symbol 'var1:" in str(err)
    assert ("is listed as a kernel argument but has no associated "
            "Interface" in str(err))
    # Now add an Interface for "var1" but of the wrong type
    sym_v1.interface = Symbol.FortranGlobal("some_mod")
    with pytest.raises(ValueError) as err:
        sym_table.specify_argument_list([sym_v1])
    assert "Symbol 'var1:" in str(err)
    assert "has an interface of type '" in str(err)


def test_symboltable_argument_list_errors():
    ''' Tests the internal sanity checks of the SymbolTable.argument_list
    property. '''
    sym_table = SymbolTable()
    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", []))
    sym_table.add(Symbol("var3", "real",
                         interface=Symbol.FortranGlobal("some_mod")))
    # Manually put a local symbol into the internal list of arguments
    sym_table._argument_list = [sym_table.lookup("var1")]
    with pytest.raises(ValueError) as err:
        sym_table._validate_arg_list(sym_table._argument_list)
    pattern = ("Symbol \'var1.*\' is listed as a kernel argument but has "
               "no associated Interface")
    assert re.search(pattern, str(err)) is not None
    # Check that the argument_list property converts this error into an
    # InternalError
    with pytest.raises(InternalError) as err:
        _ = sym_table.argument_list
    assert re.search(pattern, str(err)) is not None
    # Check that we reject a symbol imported from a module
    with pytest.raises(ValueError) as err:
        sym_table._validate_arg_list([sym_table.lookup("var3")])
    # Manually put that symbol into the argument list
    sym_table._argument_list = [sym_table.lookup("var3")]
    pattern = (r"Symbol \'var3.*\' is listed as a kernel argument but has an "
               r"interface of type \'.*\.FortranGlobal\'>")
    assert re.search(pattern, str(err)) is not None
    # Check that the argument_list property converts this error into an
    # InternalError
    with pytest.raises(InternalError) as err:
        _ = sym_table.argument_list
    assert re.search(pattern, str(err)) is not None
    # Check that we get the expected TypeError if we provide a list containing
    # objects that are not Symbols
    with pytest.raises(TypeError) as err:
        sym_table._validate_arg_list(["Not a symbol"])
    assert "Expected a list of Symbols but found an object of type" in str(err)


def test_symboltable_validate_non_args():
    ''' Checks for the validation of non-argument entries in the
    SymbolTable. '''
    sym_table = SymbolTable()
    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", []))
    sym_table.add(Symbol("var3", "real",
                         interface=Symbol.FortranGlobal("some_mod")))
    # Everything should be fine so far
    sym_table._validate_non_args()
    # Add an entry with an Argument interface
    sym_table.add(Symbol("var4", "real",
                         interface=Symbol.Argument()))
    # Since this symbol isn't in the argument list, the SymbolTable
    # is no longer valid
    with pytest.raises(ValueError) as err:
        sym_table._validate_non_args()
    pattern = (r"Symbol 'var4.* is not listed as a kernel argument and yet "
               "has a Symbol.Argument interface")
    assert re.search(pattern, str(err)) is not None


def test_symboltable_contains():
    '''Test that the __contains__ method returns True if the given name
    is in the SymbolTable, otherwise returns False.'''
    sym_table = SymbolTable()

    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", [None]))

    assert "var1" in sym_table
    assert "var2" in sym_table
    assert "var3" not in sym_table


def test_symboltable_symbols():
    '''Test that the symbols property returns a list of the symbols in the
    SymbolTable.'''
    sym_table = SymbolTable()
    assert sym_table.symbols == []
    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", [None]))
    assert len(sym_table.symbols) == 2
    sym_table.add(Symbol("var3", "real", [],
                         interface=Symbol.FortranGlobal(module_use="my_mod")))
    assert len(sym_table.symbols) == 3


def test_symboltable_local_symbols():
    '''Test that the local_symbols property returns a list with the
    symbols with local scope.'''
    sym_table = SymbolTable()
    assert [] == sym_table.local_symbols

    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", [None]))
    sym_table.add(Symbol("var3", "real", []))

    assert len(sym_table.local_symbols) == 3
    assert sym_table.lookup("var1") in sym_table.local_symbols
    assert sym_table.lookup("var2") in sym_table.local_symbols
    assert sym_table.lookup("var3") in sym_table.local_symbols
    sym_v1 = sym_table.lookup("var1")
    sym_v1.interface = Symbol.Argument(access=Symbol.Access.READWRITE)
    sym_table.specify_argument_list([sym_v1])

    assert len(sym_table.local_symbols) == 2
    assert sym_table.lookup("var1") not in sym_table.local_symbols
    assert sym_table.lookup("var2") in sym_table.local_symbols
    assert sym_table.lookup("var3") in sym_table.local_symbols

    sym_table.add(Symbol("var4", "real", [],
                         interface=Symbol.FortranGlobal(module_use="my_mod")))
    assert len(sym_table.local_symbols) == 2
    assert sym_table.lookup("var4") not in sym_table.local_symbols


def test_symboltable_global_symbols():
    ''' Test that the global_symbols property returns those symbols with
    'global' scope (i.e. that represent data that exists outside the current
    scoping unit) but are not routine arguments. '''
    sym_table = SymbolTable()
    assert sym_table.global_symbols == []
    # Add some local symbols
    sym_table.add(Symbol("var1", "real", []))
    sym_table.add(Symbol("var2", "real", [None]))
    assert sym_table.global_symbols == []
    # Add some global symbols
    sym_table.add(Symbol("gvar1", "real", [],
                         interface=Symbol.FortranGlobal(module_use="my_mod")))
    assert sym_table.lookup("gvar1") in sym_table.global_symbols
    sym_table.add(
        Symbol("gvar2", "real", [],
               interface=Symbol.Argument(access=Symbol.Access.READWRITE)))
    gsymbols = sym_table.global_symbols
    assert len(gsymbols) == 1
    assert sym_table.lookup("gvar2") not in gsymbols


def test_symboltable_abstract_properties():
    '''Test that the SymbolTable abstract properties raise the appropriate
    error.'''
    sym_table = SymbolTable()

    with pytest.raises(NotImplementedError) as error:
        _ = sym_table.data_arguments
    assert "Abstract property. Which symbols are data arguments is " \
        "API-specific." in str(error.value)

    with pytest.raises(NotImplementedError) as error:
        _ = sym_table.iteration_indices
    assert "Abstract property. Which symbols are iteration indices is " \
        "API-specific." in str(error.value)


def test_modified_kern_line_length(kernel_outputdir, monkeypatch):
    '''Modified Fortran kernels are written to file linewrapped at 132
    characters. This test checks that this linewrapping works.

    '''
    from psyclone.transformations import Dynamo0p3KernelConstTrans
    psy, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    # This example does not conform to the <name>_code, <name>_mod
    # convention so monkeypatch it to avoid the PSyIR code generation
    # raising an exception. This limitation is the subject of issue
    # #520.
    monkeypatch.setattr(kernels[0], "_module_name", "testkern_mod")
    ktrans = Dynamo0p3KernelConstTrans()
    _, _ = ktrans.apply(kernels[0], number_of_layers=100)
    # Generate the code (this triggers the generation of new kernels)
    _ = str(psy.gen)
    filepath = os.path.join(str(kernel_outputdir), "testkern_0_mod.f90")
    assert os.path.isfile(filepath)
    # Check that the argument list is line wrapped as it is longer
    # than 132 characters.
    assert "undf_w3,&\n&map_w3)\n" in open(filepath).read()
