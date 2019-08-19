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

module testkern_same_anyspace_stencil_mod

  use constants_mod
  use argument_mod
  use kernel_mod

  implicit none

  type, public, extends(kernel_type) :: testkern_same_anyspace_stencil_type
     private
     type(arg_type), dimension(3) :: meta_args = (/                 &
          arg_type(gh_field, gh_inc,  w1),                          &
          arg_type(gh_field, gh_read, any_space_1, stencil(cross)), &
          arg_type(gh_field, gh_read, any_space_1, stencil(cross))  &
          /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => testkern_same_anyspace_stencil_code
  end type testkern_same_anyspace_stencil_type

contains

  subroutine testkern_same_anyspace_stencil_code(nlayers,                      &
                           field1,                                             &
                           field2, field2_stencil_size, field2_stencil_dofmap, &
                           field3, field3_stencil_size, field3_stencil_dofmap, &
                           ndf_w1, undf_w1, map_w1,                            &
                           ndf_anyspace_1, undf_anyspace_1, map_anyspace_1)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_anyspace_1
    integer(kind=i_def), intent(in) :: undf_w1, undf_anyspace_1
    integer(kind=i_def), intent(in) :: field2_stencil_size, field3_stencil_size
    integer(kind=i_def), intent(in), dimension(ndf_w1)         :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_1) :: map_anyspace_1
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_1,field2_stencil_size) :: field2_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_anyspace_1,field3_stencil_size) :: field3_stencil_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w1)      :: field1
    real(kind=r_def), intent(in), dimension(undf_anyspace_1) :: field2
    real(kind=r_def), intent(in), dimension(undf_anyspace_1) :: field3

  end subroutine testkern_same_anyspace_stencil_code

end module testkern_same_anyspace_stencil_mod
