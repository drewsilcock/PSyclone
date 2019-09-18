! Implements an extract of code from the tra_adv benchmark with a SIGN
! intrinsic replaced with equivalent code and the resultant expression
! simplified.
program test_ifs
  
  integer, parameter :: jpi=10, jpj=10, jpk=10
  real, dimension(jpi,jpj,jpk) :: zwx, zslpx
  integer :: ji, jj, jk
  
  DO jk = 1, jpk-1
     DO jj = 2, jpj
        DO ji = 2, jpi
           tmpx = zwx(ji,jj,jk) * zwx(ji-1,jj,jk)
           if (tmpx .ge. 0.0d0) then
              zslpx(ji,jj,jk) = 0.5d0 * ( zwx(ji,jj,jk) + zwx(ji-1,jj,jk) )
           else
              zslpx(ji,jj,jk) = 0.0d0
           end if
        END DO
     END DO
  END DO

end program
