module disk_scanner
  use types
  use file_system
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: scan_directory, build_tree, calculate_sizes

contains

  ! Scan a directory and build a file tree (with optional depth limiting)
  recursive subroutine scan_directory(path, node, current_depth)
    character(len=*), intent(in) :: path
    type(file_node), intent(inout) :: node
    integer, intent(in), optional :: current_depth
    character(len=256), dimension(:), allocatable :: entries
    integer :: num_entries, i, valid_children, depth, max_entries
    character(len=512) :: child_path
    type(file_node), allocatable :: temp_children(:)
    logical :: skip_entry
    integer, parameter :: MAX_DEPTH = 100
    integer, parameter :: MAX_FILES_PER_DIR = 10000

    ! Handle depth parameter
    if (present(current_depth)) then
      depth = current_depth
    else
      depth = 0
    end if

    ! Set node properties
    node%path = path
    node%name = extract_filename(path)
    node%num_children = 0
    node%access_denied = .false.

    ! Check if this is a symbolic link - skip if so
    if (is_symlink(path)) then
      node%is_directory = .false.
      node%size = 0_int64
      return
    end if

    node%is_directory = is_directory(path)

    if (node%is_directory) then
      ! Check depth limit
      if (depth >= MAX_DEPTH) then
        node%access_denied = .true.
        node%size = 0_int64
        return
      end if

      ! Allocate entries array on heap instead of stack
      allocate(entries(MAX_FILES_PER_DIR))

      ! List directory contents (returns 0 on error/permission denied)
      num_entries = list_directory(path, entries, MAX_FILES_PER_DIR)

      ! If we got entries, scan them
      if (num_entries > 0) then
        ! Allocate temporary array for children
        allocate(temp_children(num_entries))
        valid_children = 0

        ! Recursively scan children
        do i = 1, num_entries
          ! Build child path
          child_path = trim(path) // get_path_separator() // trim(entries(i))

          ! Skip symbolic links
          if (is_symlink(child_path)) cycle

          ! Recursively scan child (with depth+1)
          valid_children = valid_children + 1
          call scan_directory(child_path, temp_children(valid_children), depth + 1)
        end do

        ! Copy valid children to node
        if (valid_children > 0) then
          allocate(node%children(valid_children))
          node%children(1:valid_children) = temp_children(1:valid_children)
          node%num_children = valid_children
        end if

        deallocate(temp_children)
      end if

      ! Calculate directory size as sum of children
      node%size = 0_int64
      if (allocated(node%children)) then
        do i = 1, node%num_children
          node%size = node%size + node%children(i)%size
        end do
      end if

      ! Deallocate entries array
      if (allocated(entries)) deallocate(entries)
    else
      ! File - get size directly
      node%size = get_file_size(path)
    end if
  end subroutine scan_directory

  ! Build tree from a root path
  subroutine build_tree(root_path, root_node)
    character(len=*), intent(in) :: root_path
    type(file_node), intent(out) :: root_node

    call scan_directory(root_path, root_node)
  end subroutine build_tree

  ! Calculate cumulative sizes (stub - already done in scan_directory)
  recursive subroutine calculate_sizes(node)
    type(file_node), intent(inout) :: node
    integer :: i

    if (node%is_directory .and. allocated(node%children)) then
      node%size = 0_int64
      do i = 1, node%num_children
        call calculate_sizes(node%children(i))
        node%size = node%size + node%children(i)%size
      end do
    end if
  end subroutine calculate_sizes

  ! Extract filename from path
  function extract_filename(path) result(filename)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: filename
    integer :: last_sep, i
    character(len=1) :: sep

    sep = get_path_separator()
    last_sep = 0

    do i = len_trim(path), 1, -1
      if (path(i:i) == sep) then
        last_sep = i
        exit
      end if
    end do

    if (last_sep > 0 .and. last_sep < len_trim(path)) then
      filename = trim(path(last_sep+1:))
    else
      filename = trim(path)
    end if
  end function extract_filename

end module disk_scanner
