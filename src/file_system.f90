module file_system
  use iso_c_binding
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: get_file_size, is_directory, list_directory, get_path_separator

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

  ! S_IFDIR constant for directory check
  integer(c_int), parameter :: S_IFDIR = int(o'040000', c_int)
  integer(c_int), parameter :: S_IFMT = int(o'170000', c_int)

  interface
    ! stat syscall
    function c_stat_file(path, buf) bind(c, name="stat")
      use iso_c_binding
      import :: c_stat
      type(c_ptr), value :: path
      type(c_stat) :: buf
      integer(c_int) :: c_stat_file
    end function c_stat_file
  end interface

contains

  ! Get file size in bytes
  function get_file_size(filepath) result(size)
    character(len=*), intent(in) :: filepath
    integer(int64) :: size
    type(c_stat) :: stat_buf
    character(len=len(filepath)+1, kind=c_char), target :: c_path
    integer(c_int) :: stat_result

    size = 0_int64
    c_path = trim(filepath) // c_null_char
    stat_result = c_stat_file(c_loc(c_path), stat_buf)

    if (stat_result == 0) then
      size = int(stat_buf%st_size, int64)
    end if
  end function get_file_size

  ! Check if path is a directory
  function is_directory(filepath) result(is_dir)
    character(len=*), intent(in) :: filepath
    logical :: is_dir
    type(c_stat) :: stat_buf
    character(len=len(filepath)+1, kind=c_char), target :: c_path
    integer(c_int) :: stat_result

    is_dir = .false.
    c_path = trim(filepath) // c_null_char
    stat_result = c_stat_file(c_loc(c_path), stat_buf)

    if (stat_result == 0) then
      is_dir = iand(stat_buf%st_mode, S_IFMT) == S_IFDIR
    end if
  end function is_directory

  ! List directory contents (stub - needs proper dirent implementation)
  function list_directory(dirpath, entries, max_entries) result(num_entries)
    character(len=*), intent(in) :: dirpath
    character(len=256), dimension(:), intent(out) :: entries
    integer, intent(in) :: max_entries
    integer :: num_entries

    ! Stub implementation - will need proper dirent.h bindings
    ! For now, return 0 entries
    num_entries = 0

    ! TODO: Implement using opendir/readdir/closedir via C bindings
    ! This will require additional interface definitions for:
    ! - DIR* opendir(const char *name)
    ! - struct dirent* readdir(DIR *dirp)
    ! - int closedir(DIR *dirp)
  end function list_directory

  ! Get platform-specific path separator
  function get_path_separator() result(sep)
    character(len=1) :: sep
    ! Unix/Linux/macOS use forward slash
    sep = '/'
  end function get_path_separator

end module file_system
