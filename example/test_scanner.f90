program test_scanner
  use types
  use disk_scanner
  use iso_fortran_env, only: int64
  implicit none

  type(file_node) :: root
  character(len=256) :: scan_path
  integer :: nargs

  print *, '========================================='
  print *, '  SNIFFERT Directory Scanner Test'
  print *, '========================================='
  print *, ''

  ! Get path from command line or use current directory
  nargs = command_argument_count()
  if (nargs >= 1) then
    call get_command_argument(1, scan_path)
  else
    scan_path = '.'
  end if

  print *, 'Scanning directory: ', trim(scan_path)
  print *, ''

  ! Build the file tree
  call build_tree(scan_path, root)

  ! Print results
  print *, 'Scan complete!'
  print *, ''
  print *, 'Root directory: ', trim(root%name)
  print *, 'Total size: ', format_size(root%size)
  print *, 'Number of entries: ', root%num_children
  print *, ''

  ! Print tree structure
  call print_tree(root, 0)

  print *, ''
  print *, '========================================='
  print *, 'Test complete! Compare with: du -sh ', trim(scan_path)
  print *, '========================================='

contains

  ! Print the tree structure recursively
  recursive subroutine print_tree(node, depth)
    type(file_node), intent(in) :: node
    integer, intent(in) :: depth
    integer :: i
    character(len=200) :: indent, prefix
    character(len=10) :: type_str

    ! Create indentation
    indent = ''
    do i = 1, depth * 2
      indent(i:i) = ' '
    end do

    ! Determine type
    if (node%is_directory) then
      type_str = '[DIR]'
    else
      type_str = '[FILE]'
    end if

    ! Print this node
    print '(A,A,1X,A,1X,A)', trim(indent), trim(type_str), &
          trim(node%name), trim(format_size(node%size))

    ! Print children (limit to avoid excessive output)
    if (allocated(node%children) .and. depth < 3) then
      do i = 1, min(node%num_children, 20)
        call print_tree(node%children(i), depth + 1)
      end do
      if (node%num_children > 20) then
        print '(A,A,I0,A)', trim(indent), '  ... and ', &
              node%num_children - 20, ' more entries'
      end if
    else if (allocated(node%children) .and. node%num_children > 0) then
      print '(A,A,I0,A)', trim(indent), '  (', node%num_children, &
            ' entries - not shown)'
    end if
  end subroutine print_tree

  ! Format file size in human-readable form
  function format_size(bytes) result(str)
    integer(int64), intent(in) :: bytes
    character(len=20) :: str
    real :: size_kb, size_mb, size_gb

    if (bytes < 1024_int64) then
      write(str, '(I0,A)') bytes, ' B'
    else if (bytes < 1024_int64 * 1024_int64) then
      size_kb = real(bytes) / 1024.0
      write(str, '(F8.2,A)') size_kb, ' KB'
    else if (bytes < 1024_int64 * 1024_int64 * 1024_int64) then
      size_mb = real(bytes) / (1024.0 * 1024.0)
      write(str, '(F8.2,A)') size_mb, ' MB'
    else
      size_gb = real(bytes) / (1024.0 * 1024.0 * 1024.0)
      write(str, '(F8.2,A)') size_gb, ' GB'
    end if
  end function format_size

end program test_scanner
