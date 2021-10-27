! This program computes all eigenvalues and eigenvectors of 
! a real symmetric matrix A(4,4) using DSYEV subroutine.
! 5.0 4.0 1.0 1.0
! 4.0 5.0 1.0 1.0
! 1.0 1.0 4.0 2.0
! 1.0 1.0 2.0 4.0

! With eigenvalues as below:
! 1.0 2.0 5.0 10.0
! and eigenvectors are:
! -------------  
 
program main
  implicit none
  integer(4), parameter :: N = 4
  integer(4), parameter :: LDA = 4
  integer(4) :: INFO
  character(len=1), parameter :: JOBZ='V', UPLO='U'
  integer(4), parameter :: LWORK=2*N**2+5*N+1
  integer(4) :: WORK(LWORK)
  real(8) :: A(LDA, N), W(N)
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
  
  call DSYEV(JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO)

!  if(INFO .gt. 0) then
!    write(*,*)' Failed to compute eigenvalues or eigenvectors'
!  else
  
!  print eigenvalues  
    do i=1, N
      write(*, 100), w(i)
    end do
    100 format(f10.5)

! print eigenvectors
    do i=1, N
       write(*,101) (A(i, j), j=1,N)
    end do
    101 format(4f10.5)
!  end if
end program main
