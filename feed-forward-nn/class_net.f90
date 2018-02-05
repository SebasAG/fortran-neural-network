module class_net
  implicit none

  type, public :: net
    integer :: i_size, h_size, o_size
    real :: lrate
    real, allocatable :: h_w(:,:), o_w(:,:), h_b(:), o_b(:), &
      h_z(:), o_z(:), h_act(:), o_act(:)
  contains
    procedure :: init => init_net
    procedure :: feedf => feed_forward
    procedure :: learn => net_learn
  end type net

contains
  subroutine init_net(this, i, h, o, lrate, seed)
  !! initialize the network with
  !!   'i' input nodes, 'h' hidden neurons and 'o' output nodes
  !! to do: implement in the constructor function
  !!   it seems gfortran doesn's support this at the time (?)
    class(net), intent(inout) :: this
    integer, intent(in) :: i, h, o
    real, intent(in), optional :: lrate
    integer, dimension(:), intent(in), optional :: seed
    
    !! set size and learning rate of network
    this%i_size = i
    this%h_size = h
    this%o_size = o
    this%lrate = lrate

    !! allocate memory for weights and biases according to size
    allocate(this%h_w(h, i))
    allocate(this%o_w(o, h))
    allocate(this%h_b(h))
    allocate(this%o_b(o))

    allocate(this%h_z(h))
    allocate(this%o_z(o))
    allocate(this%h_act(h))
    allocate(this%o_act(o))

    !! set seed for prng
    if (present(seed)) then
      call random_seed(put=seed)
    else
      call random_seed()
    endif

    !! initialize weights and bias with random values in [-1,1)
    call random_number(this%h_w)
    call random_number(this%h_b)
    call random_number(this%o_w)
    call random_number(this%o_b)

    this%h_w = (this%h_w*2 - 1)/sqrt(real(this%h_size))
    this%h_b = this%h_b*2 - 1
    this%o_w = (this%o_w*2 - 1)/sqrt(real(this%o_size))
    this%o_b = this%o_b*2 - 1
  end subroutine init_net

  function act(z)
    real, dimension(:), intent(in) :: z
    real, dimension(size(z)) :: act
    ! act = tanh(z)
    act = 1.0 / (1 - exp(-1*z))
  end function act

  function d_act(a)
    real, dimension(:), intent(in) :: a
    real, dimension(size(a)) :: d_act
    ! d_act = (1 - a*a)
    d_act = a*(1 - a)
  end function d_act

  subroutine feed_forward(this, inputs)
  !! feed forward algorithm
    class(net), intent(inout) :: this
    real, intent(in) :: inputs(this%i_size)
    
    !! weighted average of inputs plus bias
    this%h_z = matmul(this%h_w, inputs) + this%h_b
    !! apply activation function (hidden layer)
    this%h_act = act(this%h_z)
    !! weiphted average of second layer
    this%o_z = matmul(this%o_w, this%h_act) + this%o_b
    !! result, with output weights plus bias
    this%o_act = act(this%o_z)
  end subroutine feed_forward

  function outer_product(a, b)
    real, dimension(:), intent(in) :: a, b
    real, dimension(size(a), size(b)) :: outer_product
    outer_product = matmul(&
      reshape(a, [size(a), 1]),&
      reshape(b, [1, size(b)])&
    )
  end function outer_product

  function net_learn(this, inputs, expected) result(err2)
  !! learning algorithme from inputs and expected outputs
    class(net), intent(inout) :: this
    real, intent(in) :: inputs(this%i_size), expected(this%o_size)
    real, dimension(this%o_size) :: error, o_del, do_act
    real, dimension(this%h_size) :: h_del, dh_act
    real :: err2

    call this%feedf(inputs)
    error = expected - this%o_act
    dh_act = d_act(this%h_act)
    do_act = d_act(this%o_act)
    o_del = this%lrate * error * do_act
    h_del = matmul(o_del, this%o_w)

    !! outer to inner nodes
    this%o_b = this%o_b - o_del
    this%o_w = this%o_w - outer_product(o_del, this%h_act)
    this%h_b = this%h_b - h_del
    this%h_w = this%h_w - outer_product(h_del, inputs)

    err2 = dot_product(error, error)
  end function net_learn
end module class_net

