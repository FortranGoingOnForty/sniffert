module disk_scanner
  use types
  use file_system
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: scan_directory, build_tree, calculate_sizes, dump_tree_debug

  ! Directories to skip (reduce scan time and avoid issues)
  character(len=*), parameter, dimension(7) :: SKIP_DIRS = &
    [character(len=20) :: '.git', '.svn', '.hg', 'node_modules', '__pycache__', 'build', '.claude']

contains

  ! Check if directory should be skipped
  function should_skip_dir(dirname) result(skip)
    character(len=*), intent(in) :: dirname
    logical :: skip
    integer :: i

    skip = .false.
    do i = 1, size(SKIP_DIRS)
      if (trim(dirname) == trim(SKIP_DIRS(i))) then
        skip = .true.
        return
      end if
    end do
  end function should_skip_dir

  ! Scan a directory and build a file tree (with optional depth limiting)
  recursive subroutine scan_directory(path, node, current_depth)
    character(len=*), intent(in) :: path
    type(file_node), intent(inout) :: node
    integer, intent(in), optional :: current_depth
    character(len=256), dimension(:), allocatable :: entries
    integer :: num_entries, i, valid_children, depth
    character(len=512) :: child_path
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
        ! First pass: count valid children
        valid_children = 0
        do i = 1, num_entries
          if (should_skip_dir(entries(i))) cycle
          child_path = trim(path) // get_path_separator() // trim(entries(i))
          if (is_symlink(child_path)) cycle
          valid_children = valid_children + 1
        end do

        ! Allocate exact size needed
        if (valid_children > 0) then
          allocate(node%children(valid_children))
          node%num_children = 0

          ! Second pass: scan children
          do i = 1, num_entries
            if (should_skip_dir(entries(i))) cycle
            child_path = trim(path) // get_path_separator() // trim(entries(i))
            if (is_symlink(child_path)) cycle

            ! Scan directly into node%children
            node%num_children = node%num_children + 1
            call scan_directory(child_path, node%children(node%num_children), depth + 1)
          end do
        end if
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

  ! Debug function to dump tree structure to file
  subroutine dump_tree_debug(node, filename)
    type(file_node), intent(in) :: node
    character(len=*), intent(in) :: filename
    integer :: unit, ios

    open(newunit=unit, file=filename, status='replace', iostat=ios)
    if (ios /= 0) then
      print *, "Warning: Could not open debug file: ", trim(filename)
      return
    end if

    write(unit, '(A)') '=== TREE STRUCTURE DEBUG ==='
    call dump_node_recursive(node, unit, 0)
    close(unit)
  end subroutine dump_tree_debug

  ! Recursive helper for dump_tree_debug
  recursive subroutine dump_node_recursive(node, unit, depth)
    type(file_node), intent(in) :: node
    integer, intent(in) :: unit, depth
    character(len=512) :: indent
    integer :: i
    character(len=20) :: size_str

    ! Build indent string
    indent = ''
    do i = 1, depth * 2
      indent(i:i) = ' '
    end do

    ! Format size
    if (node%size < 1024_int64) then
      write(size_str, '(I0,A)') node%size, 'B'
    else if (node%size < 1024_int64**2) then
      write(size_str, '(F0.2,A)') real(node%size)/1024.0, 'KB'
    else if (node%size < 1024_int64**3) then
      write(size_str, '(F0.2,A)') real(node%size)/(1024.0**2), 'MB'
    else
      write(size_str, '(F0.2,A)') real(node%size)/(1024.0**3), 'GB'
    end if

    ! Write node info
    if (node%is_directory) then
      write(unit, '(A,A,A,A,A,A,I0,A)') trim(indent(1:depth*2)), '[DIR] ', &
        trim(node%name), ' (', trim(size_str), ', ', node%num_children, ' children)'
    else
      write(unit, '(A,A,A,A,A,A)') trim(indent(1:depth*2)), '[FILE] ', &
        trim(node%name), ' (', trim(size_str), ')'
    end if

    ! Recurse into children
    if (allocated(node%children)) then
      do i = 1, node%num_children
        call dump_node_recursive(node%children(i), unit, depth + 1)
      end do
    end if
  end subroutine dump_node_recursive

end module disk_scanner
