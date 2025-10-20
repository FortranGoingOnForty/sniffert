module types
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: file_node, rect, color_pair

  ! Represents a file or directory in the tree
  type :: file_node
    character(len=:), allocatable :: name
    character(len=:), allocatable :: path
    integer(int64) :: size
    logical :: is_directory
    type(file_node), dimension(:), allocatable :: children
    integer :: num_children
  end type file_node

  ! Rectangle for treemap layout
  type :: rect
    integer :: x, y      ! Top-left corner
    integer :: width, height
  end type rect

  ! Color pair for terminal rendering
  type :: color_pair
    integer :: foreground
    integer :: background
    integer :: pair_id
  end type color_pair

end module types
