! -----------------------------------------------------------------------------
! BSD 3-Clause License
!
! Copyright (c) 2017-2019, Science and Technology Facilities Council
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!
! * Redistributions of source code must retain the above copyright notice, this
!   list of conditions and the following disclaimer.
!
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
!
! * Neither the name of the copyright holder nor the names of its
!   contributors may be used to endorse or promote products derived from
!   this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
! FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
! COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
! BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
! ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
! -----------------------------------------------------------------------------
! Author R. W. Ford, STFC Daresbury Lab
! Modified I. Kavcic, Met Office

module dummy_orientation_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  type, public, extends(kernel_type) :: dummy_orientation_type
     private
     type(arg_type), meta_args(4) = (/                 &
          arg_type(gh_field,    gh_inc,       w0),     &
          arg_type(gh_operator, gh_readwrite, w1, w1), &
          arg_type(gh_field,    gh_read,      w2),     &
          arg_type(gh_operator, gh_write,     w3, w3)  &
           /)
     type(func_type), meta_funcs(4) = (/ &
          func_type(w0, gh_orientation), &
          func_type(w1, gh_orientation), &
          func_type(w2, gh_orientation), &
          func_type(w3, gh_orientation)  &
          /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => dummy_orientation_code
  end type dummy_orientation_type

contains

  subroutine dummy_orientation_code(cell, nlayers,           &
                                    field1,                  &
                                    ncell_3d_op1, op1,       &
                                    field2,                  &
                                    ncell_3d_op2, op2,       &
                                    ndf_w0, undf_w0, map_w0, &
                                    orientation_w0,          &
                                    ndf_w1, orientation_w1,  &
                                    ndf_w2, undf_w2, map_w2, &
                                    orientation_w2,          &
                                    ndf_w3, orientation_w3)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w0, ndf_w1, undf_w2, ndf_w3
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d_op1
    integer(kind=i_def), intent(in) :: ncell_3d_op2
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: orientation_w0
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: orientation_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: orientation_w2
    integer(kind=i_def), intent(in), dimension(ndf_w3) :: orientation_w3
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field1
    real(kind=r_def), intent(in), dimension(undf_w2)    :: field2
    real(kind=r_def), intent(inout), dimension(ndf_w1,ndf_w1,ncell_3d_op1) :: op1
    real(kind=r_def), intent(out), dimension(ndf_w3,ndf_w3,ncell_3d_op2) :: op2

  end subroutine dummy_orientation_code

end module dummy_orientation_mod
