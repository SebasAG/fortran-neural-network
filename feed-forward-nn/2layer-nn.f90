program neuron1
  use class_net
  implicit none

  integer, parameter :: i_size=2, h_size=2, o_size=1
  type(net) :: n1

  integer, parameter :: samples = 100
  real :: err2
  integer :: i

  integer, dimension(:), allocatable :: seed
  integer :: seed_size
  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  seed = 1

  call n1%init(i_size, h_size, o_size, 0.1, seed=seed)

  open(1, file="error.dat")
  do i = 1, samples
    err2 = n1%learn((/1.0,1.0/), (/0.0/)) &
      + n1%learn((/1.0,0.0/), (/1.0/)) &
      + n1%learn((/0.0,1.0/), (/1.0/)) &
      + n1%learn((/0.0,0.0/), (/0.0/))
    write (1,*) err2
  enddo
  close(1)

  call system('gnuplot -p error.plt')
end program neuron1

