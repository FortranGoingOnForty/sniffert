program test_listdir
  use file_system
  implicit none

  character(len=256), dimension(100) :: entries
  character(len=256) :: test_path
  integer :: num_entries, i, nargs
  logical :: is_dir, is_link

  ! Get path from command line
  nargs = command_argument_count()
  if (nargs >= 1) then
    call get_command_argument(1, test_path)
  else
    test_path = 'src'
  end if

  print *, 'Testing directory: ', trim(test_path)
  print *, ''

  ! Check if it's a directory
  is_dir = is_directory(test_path)
  is_link = is_symlink(test_path)

  print *, 'Is directory? ', is_dir
  print *, 'Is symlink? ', is_link
  print *, ''

  ! List directory contents
  num_entries = list_directory(test_path, entries, 100)

  print *, 'Number of entries found: ', num_entries
  print *, ''

  if (num_entries > 0) then
    print *, 'Entries:'
    do i = 1, num_entries
      print *, '  ', i, ': ', trim(entries(i))
    end do
  else
    print *, 'No entries found (or permission denied)'
  end if

end program test_listdir
