module class_net
  implicit none

  type, public :: net
    integer :: i_size, h_size, o_size
    real, allocatable :: h_w(:,:), o_w(:,:), h_b(:), o_b(:), &
      h_z(:), act(:), d_act(:), res(:)
  contains
    procedure :: init => init_net
    procedure :: feedf => feed_forward
  end type net

  ! interface net
  !   module procedure new_net
  ! end interface net
contains
  ! type(net) function new_net(i, h, o)
  subroutine init_net(new_net, i, h, o)
    class(net), intent(inout) :: new_net
    integer, intent(in) :: i, h, o

    new_net%i_size = i
    new_net%h_size = h
    new_net%o_size = o

    allocate(new_net%h_w(h, i))
    allocate(new_net%o_w(o, h))
    allocate(new_net%h_b(h))
    allocate(new_net%o_b(o))

    allocate(new_net%h_z(h))
    allocate(new_net%act(h))
    allocate(new_net%d_act(h))
    allocate(new_net%res(o))

    call random_number(new_net%h_w)
    call random_number(new_net%h_b)
    call random_number(new_net%o_w)
    call random_number(new_net%o_b)
  end subroutine init_net

  subroutine feed_forward(this, inputs)
    class(net), intent(inout) :: this
    real, intent(in) :: inputs(this%i_size)
    this%h_z = matmul(this%h_w, inputs) + this%h_b
    this%act = tanh(this%h_z)
    this%res = matmul(this%o_w, this%act) + this%o_b
  end subroutine feed_forward
end module class_net

