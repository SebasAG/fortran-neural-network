program neuron1
  use class_net
  implicit none

  integer, parameter :: i_size=2, h_size=2, o_size=1
  type(net) :: n1

  integer, parameter :: samples = 10000
  real :: err, e1, e2, e3, e4
  integer :: i

  integer, dimension(:), allocatable :: seed
  integer :: seed_size
  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  seed = 1

  call n1%init(i_size, h_size, o_size, lrate=0.01)

  open(1, file="i_error.dat")
  do i = 1, samples
    e1 = n1%learn([1.0,1.0], [0.0])
    e2 = n1%learn([1.0,0.0], [1.0])
    e3 = n1%learn([0.0,1.0], [1.0])
    e4 = n1%learn([0.0,0.0], [0.0])
    err = e1 + e2 + e3 + e4
    if (modulo(i, 50) == 0) then
      write (1,*) i, err
    endif
  enddo
  close(1)
  call system('gnuplot -p i_error.plt')

  call n1%feedf((/0.0, 0.0/))
  print *, '0,0: ', n1%o_act
  call n1%feedf((/0.0, 1.0/))
  print *, '0,1: ', n1%o_act
  call n1%feedf((/1.0, 0.0/))
  print *, '1,0: ', n1%o_act
  call n1%feedf((/1.0, 1.0/))
  print *, '1,1: ', n1%o_act
end program neuron1

