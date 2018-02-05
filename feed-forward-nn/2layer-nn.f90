program neuron1
  use class_net
  implicit none

  integer, parameter :: i_size=2, h_size=2, o_size=1
  type(net) :: n1

  integer, parameter :: samples = 10000
  real :: err, e1, e2, e3, e4, inp(2)
  integer :: i, j

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
      write (1, '(i5,1x,f10.8)') i, err
    endif
  enddo
  close(1)

  open(2, file="i_result.dat")
  do i = 0, 20
    do j = 0, 20
      inp = [i, j]*0.05
      call n1%feedf(inp)
      write(2, '(3(f10.8,1x))') inp(1), inp(2), n1%o_act(1)
    enddo
    write(2, '(A,$)') new_line('a')
  enddo
  close(2)

  call system('gnuplot -p i_error.plt')
end program neuron1

