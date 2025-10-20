program test_treemap
  use types
  use disk_scanner
  use treemap_layout
  use iso_fortran_env, only: int64
  implicit none

  type(file_node) :: root
  type(rect) :: screen
  character(len=256) :: scan_path
  integer :: nargs
  character(len=80*40) :: canvas
  integer :: i, j, width, height

  print *, '========================================='
  print *, '  SNIFFERT Treemap Layout Test'
  print *, '========================================='
  print *, ''

  ! Get path from command line or use src directory
  nargs = command_argument_count()
  if (nargs >= 1) then
    call get_command_argument(1, scan_path)
  else
    scan_path = 'src'
  end if

  print *, 'Scanning directory: ', trim(scan_path)
  print *, ''

  ! Build the file tree
  call build_tree(scan_path, root)

  print *, 'Scan complete!'
  print *, 'Root: ', trim(root%name)
  print *, 'Size: ', format_size(root%size)
  print *, 'Entries: ', root%num_children
  print *, ''

  ! Set up "screen" dimensions (80x24 for visualization)
  width = 80
  height = 20
  screen%x = 0
  screen%y = 0
  screen%width = width
  screen%height = height

  print *, 'Calculating treemap layout (', width, 'x', height, ')...'
  print *, ''

  ! Calculate treemap layout
  call calculate_treemap(root, screen)

  print *, 'Layout calculated! Rendering...'
  print *, ''

  ! Render to ASCII canvas
  call init_canvas(canvas, width, height)
  call render_node(root, canvas, width, height, 0)

  ! Print canvas
  call print_canvas(canvas, width, height)

  print *, ''
  print *, '========================================='
  print *, 'Legend: Each rectangle represents a file'
  print *, 'Numbers show first few digits of size'
  print *, '========================================='

contains

  ! Initialize canvas with spaces
  subroutine init_canvas(canvas, w, h)
    character(len=*), intent(out) :: canvas
    integer, intent(in) :: w, h
    integer :: i

    do i = 1, w * h
      canvas(i:i) = ' '
    end do
  end subroutine init_canvas

  ! Render a node to the canvas
  recursive subroutine render_node(node, canvas, canvas_w, canvas_h, depth)
    type(file_node), intent(in) :: node
    character(len=*), intent(inout) :: canvas
    integer, intent(in) :: canvas_w, canvas_h, depth
    integer :: i

    ! Draw this node's border
    call draw_rect(canvas, canvas_w, canvas_h, node%bounds, depth)

    ! Recursively render children
    if (allocated(node%children)) then
      do i = 1, node%num_children
        call render_node(node%children(i), canvas, canvas_w, canvas_h, depth + 1)
      end do
    end if
  end subroutine render_node

  ! Draw a rectangle on the canvas
  subroutine draw_rect(canvas, canvas_w, canvas_h, bounds, depth)
    character(len=*), intent(inout) :: canvas
    integer, intent(in) :: canvas_w, canvas_h, depth
    type(rect), intent(in) :: bounds
    integer :: x, y, pos
    character(len=1) :: border_char

    ! Choose border character based on depth
    select case (mod(depth, 3))
      case (0)
        border_char = '#'
      case (1)
        border_char = '+'
      case (2)
        border_char = '|'
    end select

    ! Draw top and bottom borders
    do x = bounds%x, min(bounds%x + bounds%width - 1, canvas_w - 1)
      ! Top border
      if (bounds%y >= 0 .and. bounds%y < canvas_h) then
        pos = bounds%y * canvas_w + x + 1
        if (pos >= 1 .and. pos <= len(canvas)) then
          canvas(pos:pos) = border_char
        end if
      end if

      ! Bottom border
      if (bounds%y + bounds%height - 1 >= 0 .and. &
          bounds%y + bounds%height - 1 < canvas_h) then
        pos = (bounds%y + bounds%height - 1) * canvas_w + x + 1
        if (pos >= 1 .and. pos <= len(canvas)) then
          canvas(pos:pos) = border_char
        end if
      end if
    end do

    ! Draw left and right borders
    do y = bounds%y, min(bounds%y + bounds%height - 1, canvas_h - 1)
      ! Left border
      if (bounds%x >= 0 .and. bounds%x < canvas_w) then
        pos = y * canvas_w + bounds%x + 1
        if (pos >= 1 .and. pos <= len(canvas)) then
          canvas(pos:pos) = border_char
        end if
      end if

      ! Right border
      if (bounds%x + bounds%width - 1 >= 0 .and. &
          bounds%x + bounds%width - 1 < canvas_w) then
        pos = y * canvas_w + (bounds%x + bounds%width - 1) + 1
        if (pos >= 1 .and. pos <= len(canvas)) then
          canvas(pos:pos) = border_char
        end if
      end if
    end do
  end subroutine draw_rect

  ! Print the canvas
  subroutine print_canvas(canvas, w, h)
    character(len=*), intent(in) :: canvas
    integer, intent(in) :: w, h
    integer :: y, start_pos

    do y = 0, h - 1
      start_pos = y * w + 1
      print '(A)', canvas(start_pos:start_pos+w-1)
    end do
  end subroutine print_canvas

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

end program test_treemap
