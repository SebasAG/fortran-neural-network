program neuron1
  use class_net
  implicit none

  integer, parameter :: i_size=2, h_size=2, o_size=1
  type(net) :: n1

  integer, parameter :: samples = 10
  real, dimension(samples) :: err2
  integer :: i

  integer, dimension(:), allocatable :: seed
  integer :: seed_size
  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  seed = 1

  call n1%init(i_size, h_size, o_size, 0.1, seed=seed)

  do i = 1, samples
    err2(i) = n1%learn((/1.0,1.0/), (/0.0/)) &
      + n1%learn((/1.0,0.0/), (/1.0/)) &
      + n1%learn((/0.0,1.0/), (/1.0/)) &
      + n1%learn((/0.0,0.0/), (/0.0/))
    print *, err2(i)
  enddo
contains
  
  subroutine save_net_dat
  !! save network data to a file 'n_data.dat'
    integer, parameter :: n_dat=1
    integer :: i, j
    open(n_dat, file='n_data.dat')
  10 format (*(f10.6, 1x))
  20 format (/ *(f10.6, 1x))

    do i = 1, n1%i_size
      write(n_dat, 10) (n1%h_w(j, i), j = 1, h_size)
    enddo
    write(n_dat, 20) (n1%h_b(i), i = 1, h_size)

    write(n_dat, '(A, $)') new_line('a')
    do i = 1, n1%h_size
      write(n_dat, 10) (n1%o_w(j, i), j = 1, o_size)
    enddo
    write(n_dat, 20) (n1%o_b(i), i = 1, o_size)
  end subroutine save_net_dat
end program neuron1

