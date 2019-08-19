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

module testkern_multi_field_same_stencil_mod
  type, public, extends(kernel_type) :: testkern_multi_field_same_stencil_type
     private
     type(arg_type), dimension(5) :: meta_args = (/         &
          arg_type(gh_field, gh_inc,  w1),                  &
          arg_type(gh_field, gh_read, w1, stencil(cross)),  &
          arg_type(gh_field, gh_read, w1, stencil(cross)),  &
          arg_type(gh_field, gh_read, w2, stencil(xory1d)), &
          arg_type(gh_field, gh_read, w2, stencil(xory1d))  &
          /)
     integer :: iterates_over = cells
   contains
     procedure, public, nopass :: code => testkern_multi_field_same_stencil_code
  end type testkern_multi_field_same_stencil_type

contains

  subroutine testkern_multi_field_same_stencil_code(nlayers,                    &
                            field1,                                             &
                            field2, field2_stencil_size, field2_stencil_dofmap, &
                            field3, field3_stencil_size, field3_stencil_dofmap, &
                            field4, field4_stencil_size,                        &
                            field4_direction, field4_stencil_dofmap,            &
                            field5, field5_stencil_size,                        &
                            field5_direction, field5_stencil_dofmap,            &
                            ndf_w1, undf_w1, map_w1,                            &
                            ndf_w2, undf_w2, map_w2)

    implicit none

    integer(kind=i_def), intent(in) :: nlayers
    integer(kind=i_def), intent(in) :: ndf_w1
    integer(kind=i_def), intent(in) :: ndf_w2
    integer(kind=i_def), intent(in) :: undf_w1, undf_w2
    integer(kind=i_def), intent(in) :: field2_stencil_size, field3_stencil_size, &
                                       field4_stencil_size, field5_stencil_size
    integer(kind=i_def), intent(in) :: field4_direction, field5_direction
    integer(kind=i_def), intent(in), dimension(ndf_w1) :: map_w1
    integer(kind=i_def), intent(in), dimension(ndf_w2) :: map_w2
    integer(kind=i_def), intent(in), dimension(ndf_w1,field2_stencil_size) :: field2_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w1,field3_stencil_size) :: field3_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,field4_stencil_size) :: field4_stencil_dofmap
    integer(kind=i_def), intent(in), dimension(ndf_w2,field5_stencil_size) :: field5_stencil_dofmap
    real(kind=r_def), intent(inout), dimension(undf_w1) :: field1
    real(kind=r_def), intent(in), dimension(undf_w1)    :: field2
    real(kind=r_def), intent(in), dimension(undf_w1)    :: field3
    real(kind=r_def), intent(in), dimension(undf_w2)    :: field4
    real(kind=r_def), intent(in), dimension(undf_w2)    :: field5

  end subroutine testkern_multi_field_same_stencil_code

end module testkern_multi_field_same_stencil_mod
