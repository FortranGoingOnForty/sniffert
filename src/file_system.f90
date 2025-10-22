module file_system
  use iso_c_binding
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: get_file_size, is_directory, is_symlink, list_directory, get_path_separator, is_running_as_root
  public :: delete_path, get_absolute_path

  ! POSIX stat structure (simplified)
  type, bind(c) :: c_stat
    integer(c_long) :: st_dev
    integer(c_long) :: st_ino
    integer(c_int) :: st_mode
    integer(c_long) :: st_nlink
    integer(c_int) :: st_uid
    integer(c_int) :: st_gid
    integer(c_long) :: st_rdev
    integer(c_long) :: st_size
    integer(c_long) :: st_blksize
    integer(c_long) :: st_blocks
    integer(c_long) :: st_atime
    integer(c_long) :: st_mtime
    integer(c_long) :: st_ctime
  end type c_stat

  ! POSIX dirent structure (simplified, platform-specific)
  ! On macOS, d_name is 256 bytes, but other fields vary
  type, bind(c) :: c_dirent
    integer(c_long) :: d_ino
    integer(c_short) :: d_reclen
    integer(c_int8_t) :: d_type
    integer(c_int8_t) :: d_namlen
    character(kind=c_char) :: d_name(256)
  end type c_dirent

  ! File type constants
  integer(c_int), parameter :: S_IFDIR = int(o'040000', c_int)
  integer(c_int), parameter :: S_IFLNK = int(o'120000', c_int)
  integer(c_int), parameter :: S_IFMT = int(o'170000', c_int)

  ! dirent d_type constants (not currently used, but available)
  integer(c_int8_t), parameter :: DT_DIR = 4
  integer(c_int8_t), parameter :: DT_REG = 8
  integer(c_int8_t), parameter :: DT_LNK = 10

  interface
    ! C helper functions
    function list_dir_helper(path, names, max_entries) bind(c, name="list_dir_helper")
      use iso_c_binding
      type(c_ptr), value :: path
      character(kind=c_char), dimension(*) :: names
      integer(c_int), value :: max_entries
      integer(c_int) :: list_dir_helper
    end function list_dir_helper

    function is_dir_helper(path) bind(c, name="is_dir_helper")
      use iso_c_binding
      type(c_ptr), value :: path
      integer(c_int) :: is_dir_helper
    end function is_dir_helper

    function is_link_helper(path) bind(c, name="is_link_helper")
      use iso_c_binding
      type(c_ptr), value :: path
      integer(c_int) :: is_link_helper
    end function is_link_helper

    function get_size_helper(path) bind(c, name="get_size_helper")
      use iso_c_binding
      type(c_ptr), value :: path
      integer(c_long_long) :: get_size_helper
    end function get_size_helper

    function is_running_as_root_helper() bind(c, name="is_running_as_root")
      use iso_c_binding
      integer(c_int) :: is_running_as_root_helper
    end function is_running_as_root_helper

    function get_absolute_path_helper(path, result, result_len) bind(c, name="get_absolute_path")
      use iso_c_binding
      type(c_ptr), value :: path
      character(kind=c_char), dimension(*) :: result
      integer(c_int), value :: result_len
      integer(c_int) :: get_absolute_path_helper
    end function get_absolute_path_helper
  end interface

contains

  ! Get file size in bytes
  function get_file_size(filepath) result(size)
    character(len=*), intent(in) :: filepath
    integer(int64) :: size
    character(len=len(filepath)+1, kind=c_char), target :: c_path
    integer(c_long_long) :: c_size

    c_path = trim(filepath) // c_null_char
    c_size = get_size_helper(c_loc(c_path))
    size = int(c_size, int64)
  end function get_file_size

  ! Check if path is a directory
  function is_directory(filepath) result(is_dir)
    character(len=*), intent(in) :: filepath
    logical :: is_dir
    character(len=len(filepath)+1, kind=c_char), target :: c_path
    integer(c_int) :: result

    c_path = trim(filepath) // c_null_char
    result = is_dir_helper(c_loc(c_path))
    is_dir = (result /= 0)
  end function is_directory

  ! Check if path is a symbolic link
  function is_symlink(filepath) result(is_link)
    character(len=*), intent(in) :: filepath
    logical :: is_link
    character(len=len(filepath)+1, kind=c_char), target :: c_path
    integer(c_int) :: result

    c_path = trim(filepath) // c_null_char
    result = is_link_helper(c_loc(c_path))
    is_link = (result /= 0)
  end function is_symlink

  ! List directory contents using C helper
  function list_directory(dirpath, entries, max_entries) result(num_entries)
    character(len=*), intent(in) :: dirpath
    character(len=256), dimension(:), intent(out) :: entries
    integer, intent(in) :: max_entries
    integer :: num_entries

    character(len=len(dirpath)+1, kind=c_char), target :: c_path
    character(kind=c_char), dimension(256*max_entries), target :: c_names
    integer(c_int) :: c_count
    integer :: i, j, name_start, name_len

    num_entries = 0
    c_path = trim(dirpath) // c_null_char

    ! Call C helper to get directory names
    c_count = list_dir_helper(c_loc(c_path), c_names, int(max_entries, c_int))
    num_entries = int(c_count)

    ! Extract names from C array
    do i = 1, num_entries
      name_start = (i - 1) * 256 + 1
      entries(i) = ''
      name_len = 0

      ! Copy characters until null terminator
      do j = 0, 255
        if (c_names(name_start + j) == c_null_char) exit
        entries(i)(j+1:j+1) = c_names(name_start + j)
        name_len = j + 1
      end do
    end do
  end function list_directory

  ! Get platform-specific path separator
  function get_path_separator() result(sep)
    character(len=1) :: sep
    ! Unix/Linux/macOS use forward slash
    sep = '/'
  end function get_path_separator

  ! Check if running as root (sudo)
  function is_running_as_root() result(is_root)
    logical :: is_root
    integer(c_int) :: result

    result = is_running_as_root_helper()
    is_root = (result /= 0)
  end function is_running_as_root

  ! Delete a file or directory, trying 'trash' command first, then rm -rf
  function delete_path(path) result(success)
    character(len=*), intent(in) :: path
    logical :: success
    integer :: exit_code
    character(len=1024) :: command

    ! First try using 'trash' command (safer)
    write(command, '(A,A,A)') 'trash "', trim(path), '" 2>/dev/null'
    call execute_command_line(trim(command), exitstat=exit_code)

    if (exit_code == 0) then
      success = .true.
      return
    end if

    ! Trash not available or failed, try rm -rf
    if (is_directory(path)) then
      write(command, '(A,A,A)') 'rm -rf "', trim(path), '"'
    else
      write(command, '(A,A,A)') 'rm -f "', trim(path), '"'
    end if

    call execute_command_line(trim(command), exitstat=exit_code)
    success = (exit_code == 0)
  end function delete_path

  ! Get absolute path from relative or absolute path
  function get_absolute_path(path) result(abs_path)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: abs_path
    character(kind=c_char, len=512), target :: c_path, c_result
    integer(c_int) :: result_code, i, path_len

    ! Convert Fortran string to C string
    c_path = trim(path) // c_null_char

    ! Call C helper
    result_code = get_absolute_path_helper(c_loc(c_path), c_result, 512_c_int)

    if (result_code == 0) then
      ! Failed to resolve, return original path
      abs_path = trim(path)
      return
    end if

    ! Convert C string back to Fortran string
    path_len = 0
    do i = 1, 512
      if (c_result(i:i) == c_null_char) exit
      path_len = path_len + 1
    end do

    allocate(character(len=path_len) :: abs_path)
    abs_path = c_result(1:path_len)
  end function get_absolute_path

end module file_system
