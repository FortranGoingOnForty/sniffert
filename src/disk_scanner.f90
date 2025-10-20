module disk_scanner
  use types
  use file_system
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: scan_directory, build_tree, calculate_sizes

contains

  ! Scan a directory and build a file tree
  recursive subroutine scan_directory(path, node)
    character(len=*), intent(in) :: path
    type(file_node), intent(inout) :: node
    character(len=256), dimension(1000) :: entries
    integer :: num_entries, i
    character(len=512) :: child_path
    type(file_node) :: child_node

    ! Set node properties
    node%path = path
    node%name = extract_filename(path)
    node%is_directory = is_directory(path)
    node%num_children = 0

    if (node%is_directory) then
      ! List directory contents
      num_entries = list_directory(path, entries, 1000)

      ! Allocate children array
      if (num_entries > 0) then
        allocate(node%children(num_entries))

        ! Recursively scan children
        do i = 1, num_entries
          ! Skip . and ..
          if (trim(entries(i)) == '.' .or. trim(entries(i)) == '..') cycle

          ! Build child path
          child_path = trim(path) // get_path_separator() // trim(entries(i))

          ! Recursively scan child
          call scan_directory(child_path, child_node)

          ! Add to children
          node%num_children = node%num_children + 1
          node%children(node%num_children) = child_node
        end do
      end if

      ! Calculate directory size as sum of children
      node%size = 0_int64
      do i = 1, node%num_children
        node%size = node%size + node%children(i)%size
      end do
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
