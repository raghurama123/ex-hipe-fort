! gfortran -O3 code_01_flop.f90 -o code_01_flop.x
! ./code_01_flop.x | tee benchmark_times.txt

program calc_mflops

  implicit none

  integer, parameter               :: dp = kind(1.0d0)
  integer                          :: M, N, R
  real(dp), allocatable            :: A(:), B(:), C(:), D(:)
  real(dp)                         :: S, E, MFLOPS
  integer                          :: i, j

  R = 100

  do M = 10, 28

    N = 2**M

    allocate(A(1:N), B(1:N), C(1:N), D(1:N))
  
    do i = 1, N
      A(i) = 0.0d0
      B(i) = 1.0d0
      C(i) = 2.0d0
      D(i) = 3.0d0
    enddo
  
    call get_walltime(S)
  
    do j = 1, R
      do i = 1, N
        A(i) = B(i) + C(i) * D(i)
      enddo
    enddo
  
    call get_walltime(E)
  
    MFLOPS = R * N * 2d0 / ((E-S) * 1.0d6)
  
    deallocate(A, B, C, D)
  
    write(*,'(i8, 2e15.8)') M, real(N,dp), MFLOPS

  enddo
  
end program calc_mflops

subroutine get_walltime(wctime)

  use iso_fortran_env, only: int64

  implicit none

  integer, parameter :: dp = kind(1.0d0)
  real(dp)           :: wctime
  integer(int64)     :: tcount, trate

  call system_clock(tcount, trate)

  wctime = real(tcount, dp) / trate

end subroutine get_walltime
