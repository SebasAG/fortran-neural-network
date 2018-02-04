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
end program neuron1

