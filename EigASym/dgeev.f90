! This program computes all eigenvalues and eigenvectors of 
! a real symmetric matrix A(4,4) using DSYEV subroutine.

! With eigenvalues as below:
! and eigenvectors are:  
program main
  implicit none
  integer(4), parameter :: N = 5
  integer(4), parameter :: LDA=5, LDVL=5, LDVR=5
  integer(4) :: INFO
  character(len=1), parameter :: JOBVR='V', JOBVL='V'
  real(8) :: WI(N), WR(N)
  integer(4), parameter :: LWORK=4*N
  integer(4) :: WORK(LWORK)
  real(8) :: A(LDA, N), VL(LDVL,N), VR(LDVR, N)
  integer(4) :: i, j
  open(1,file='amat.dat', action='read', status='old')
 
  ! reading matrix A from file 'amat.dat'
  do i=1,N
    read(1,*) (A(i,j), j=1,N)
  end do
  close(1)

  do i=1,N
    write(*,101) (A(i,j), j=1,N)
  end do
  write(*,*)
  
  call DGEEV(JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)
!  if(INFO .gt. 0) then
!    write(*,*)' Failed to compute eigenvalues or eigenvectors'
!  else
  
!  print eigenvalues  
    do i=1, N
      write(*, 100), WI(i), WR(i)
    end do
    100 format(2f12.6)

! print Left eigenvectors
    do i=1, N
       write(*,101) (VL(i, j), j=1,N)
    end do
! Print Right eigenvectors
    do i=1, N
       write(*,101) (VR(i, j), j=1,N)
    end do
    101 format(5f12.6)
!  end if
end program main

