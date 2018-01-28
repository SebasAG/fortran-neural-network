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
  !! initialize the network with
  !!   'i' input nodes, 'h' hidden neurons and 'o' output nodes
  !! to do: implement in the constructor function
  !!   it seems gfortran doesn's support this at the time (?)
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
  !! feed forward algorithm
    class(net), intent(inout) :: this
    real, intent(in) :: inputs(this%i_size)
    
    !! weighted average of inputs plus bias
    this%h_z = matmul(this%h_w, inputs) + this%h_b
    !! apply activation function (hidden layer)
    this%act = tanh(this%h_z)
    !! result, with output weights plus bias
    this%res = matmul(this%o_w, this%act) + this%o_b
  end subroutine feed_forward

  subroutine net_learn(this, inputs, expected)
  !! learning algorithme from inputs and expected outputs
    class(net), intent(inout) :: this
    real, intent(in) :: inputs(this%i_size), expected(this%o_size)
    real, dimension(this%o_size) :: error
    integer :: i

    call this%feedf(inputs)
    error = expected - this%res

    !! print error for debug
    print '(*(f10.6))', (error(i), i = 1, this%o_size)
  end subroutine net_learn
end module class_net

