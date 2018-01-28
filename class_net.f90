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

contains
  subroutine init_net(this, i, h, o)
    class(net), intent(inout) :: this
    integer, intent(in) :: i, h, o

    this%i_size = i
    this%h_size = h
    this%o_size = o

    allocate(this%h_w(h, i))
    allocate(this%o_w(o, h))
    allocate(this%h_b(h))
    allocate(this%o_b(o))

    allocate(this%h_z(h))
    allocate(this%act(h))
    allocate(this%d_act(h))
    allocate(this%res(o))

    call random_number(this%h_w)
    call random_number(this%h_b)
    call random_number(this%o_w)
    call random_number(this%o_b)
  end subroutine init_net

  subroutine feed_forward(this, inputs)
    class(net), intent(inout) :: this
    real, intent(in) :: inputs(this%i_size)
    this%h_z = matmul(this%h_w, inputs) + this%h_b
    this%act = tanh(this%h_z)
    this%res = matmul(this%o_w, this%act) + this%o_b
  end subroutine feed_forward
end module class_net

