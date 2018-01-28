program neuron1
  use class_net
  implicit none

  integer, parameter :: i_size=2, h_size=2, o_size=1
  type(net) :: n1

  call n1%init(i_size, h_size, o_size)

  print *, n1%i_size, n1%h_size, n1%o_size
  call n1%feedf((/1.0, 0.0/))
  print *, n1%act(1)

  call save_net_dat
contains
  subroutine save_net_dat
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

