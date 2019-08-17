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
! Author R. W. Ford STFC Daresbury Lab
! Modified I. Kavcic Met Office

module testkern_operator_read_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  type, extends(kernel_type) :: testkern_operator_read_type
     type(arg_type), dimension(3) :: meta_args = (/ &
          arg_type(gh_operator, gh_read, w0, w0),   &
          arg_type(gh_field*3,  gh_inc,  w0),       &
          arg_type(gh_integer,  gh_read)            &
          /)
     type(func_type) :: meta_funcs(1) =             &
         (/ func_type(w0, gh_basis, gh_diff_basis)  &
          /)
     integer, parameter :: iterates_over = cells
     integer, parameter :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, public, nopass :: code => testkern_operator_read_code
  end type testkern_operator_read_type

contains

  subroutine testkern_operator_read_code(cell, nlayers, ncell_3d,      &
                                         op,                           &
                                         field_v1, field_v2, field_v3, &
                                         iscalar,                      &
                                         ndf_w0, undf_w0, map_w0,      &
                                         basis_w0, diff_basis_w0,      &
                                         np_xy, np_z,                  &
                                         weights_xy, weights_z)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w0
    integer(kind=i_def), intent(in) :: undf_w0
    integer(kind=i_def), intent(in) :: iscalar
    integer(kind=i_def), intent(in) :: cell
    integer(kind=i_def), intent(in) :: ncell_3d
    integer(kind=i_def), intent(in) :: np_xy, np_z
    integer(kind=i_def), intent(in), dimension(ndf_w0) :: map_w0
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field_v1
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field_v2
    real(kind=r_def), intent(inout), dimension(undf_w0) :: field_v3
    real(kind=r_def), intent(in), dimension(ndf_w0,ndf_w0,ncell_3d) :: op
    real(kind=r_def), intent(in), dimension(1,ndf_w0,np_xy,np_z) :: basis_w0
    real(kind=r_def), intent(in), dimension(3,ndf_w0,np_xy,np_z) :: diff_basis_w0
    real(kind=r_def), intent(in), dimension(np_xy) :: weights_xy
    real(kind=r_def), intent(in), dimension(np_z)  :: weights_z

  end subroutine testkern_operator_read_code

end module testkern_operator_read_mod
