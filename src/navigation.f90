module navigation
  use types
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: selection_state, init_selection, move_up, move_down
  public :: move_left, move_right, get_selected_node, get_selected_path

  ! Maximum depth for selection path
  integer, parameter :: MAX_DEPTH = 100

  ! Selection state tracks which node is selected via index path
  type :: selection_state
    integer :: depth  ! Current depth in tree
    integer, dimension(MAX_DEPTH) :: indices  ! Path of indices to selected node
    character(len=512) :: current_path  ! Full filesystem path of selected node
  end type selection_state

contains

  ! Initialize selection to root node
  subroutine init_selection(state, root_node)
    type(selection_state), intent(out) :: state
    type(file_node), intent(in) :: root_node

    state%depth = 0
    state%indices(:) = 0
    state%current_path = root_node%path
  end subroutine init_selection

  ! Move selection down (next sibling)
  subroutine move_down(state, root_node)
    type(selection_state), intent(inout) :: state
    type(file_node), intent(in) :: root_node
    type(file_node), pointer :: parent_node
    integer :: current_idx, num_siblings

    if (state%depth == 0) then
      ! At root, can't move down
      return
    end if

    ! Get parent node
    parent_node => get_node_at_path(root_node, state%indices, state%depth - 1)
    if (.not. associated(parent_node)) return

    if (.not. allocated(parent_node%children)) return

    num_siblings = parent_node%num_children
    current_idx = state%indices(state%depth)

    ! Move to next sibling if not at end
    if (current_idx < num_siblings) then
      state%indices(state%depth) = current_idx + 1
      call update_current_path(state, root_node)
    end if
  end subroutine move_down

  ! Move selection up (previous sibling)
  subroutine move_up(state, root_node)
    type(selection_state), intent(inout) :: state
    type(file_node), intent(in) :: root_node
    integer :: current_idx

    if (state%depth == 0) then
      ! At root, can't move up
      return
    end if

    current_idx = state%indices(state%depth)

    ! Move to previous sibling if not at start
    if (current_idx > 1) then
      state%indices(state%depth) = current_idx - 1
      call update_current_path(state, root_node)
    end if
  end subroutine move_up

  ! Move selection right (first child / drill down)
  subroutine move_right(state, root_node)
    type(selection_state), intent(inout) :: state
    type(file_node), intent(in) :: root_node
    type(file_node), pointer :: current_node

    ! Get current node
    current_node => get_node_at_path(root_node, state%indices, state%depth)
    if (.not. associated(current_node)) return

    ! Check if has children
    if (allocated(current_node%children) .and. current_node%num_children > 0) then
      ! Move to first child
      state%depth = state%depth + 1
      if (state%depth <= MAX_DEPTH) then
        state%indices(state%depth) = 1
        call update_current_path(state, root_node)
      else
        state%depth = state%depth - 1  ! Undo if too deep
      end if
    end if
  end subroutine move_right

  ! Move selection left (parent / go up)
  subroutine move_left(state, root_node)
    type(selection_state), intent(inout) :: state
    type(file_node), intent(in) :: root_node

    if (state%depth > 0) then
      state%depth = state%depth - 1
      state%indices(state%depth + 1) = 0  ! Clear child index
      call update_current_path(state, root_node)
    end if
  end subroutine move_left

  ! Get pointer to currently selected node
  function get_selected_node(root_node, state) result(node_ptr)
    type(file_node), intent(in), target :: root_node
    type(selection_state), intent(in) :: state
    type(file_node), pointer :: node_ptr

    node_ptr => get_node_at_path(root_node, state%indices, state%depth)
  end function get_selected_node

  ! Get filesystem path of selected node
  function get_selected_path(state) result(path)
    type(selection_state), intent(in) :: state
    character(len=512) :: path

    path = state%current_path
  end function get_selected_path

  ! Helper: Get node at index path
  function get_node_at_path(root_node, indices, depth) result(node_ptr)
    type(file_node), intent(in), target :: root_node
    integer, dimension(:), intent(in) :: indices
    integer, intent(in) :: depth
    type(file_node), pointer :: node_ptr
    integer :: i

    node_ptr => root_node

    do i = 1, depth
      if (.not. allocated(node_ptr%children)) then
        nullify(node_ptr)
        return
      end if

      if (indices(i) < 1 .or. indices(i) > node_ptr%num_children) then
        nullify(node_ptr)
        return
      end if

      node_ptr => node_ptr%children(indices(i))
    end do
  end function get_node_at_path

  ! Helper: Update current_path after navigation
  subroutine update_current_path(state, root_node)
    type(selection_state), intent(inout) :: state
    type(file_node), intent(in) :: root_node
    type(file_node), pointer :: node_ptr

    node_ptr => get_node_at_path(root_node, state%indices, state%depth)
    if (associated(node_ptr)) then
      state%current_path = node_ptr%path
    end if
  end subroutine update_current_path

end module navigation
