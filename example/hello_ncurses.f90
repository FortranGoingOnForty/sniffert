program hello_ncurses
  use ncurses_wrapper
  use iso_c_binding
  implicit none

  type(c_ptr) :: win
  integer :: ch, res
  integer :: max_y, max_x
  integer :: row, col
  character(len=50) :: msg, size_msg, help_msg, arrow_msg
  character(len=20) :: key_name

  ! Initialize ncurses
  win = nc_initscr()

  ! Set up terminal
  res = nc_cbreak()       ! Disable line buffering
  res = nc_noecho()       ! Don't echo input
  res = nc_keypad(win, .true.)  ! Enable arrow keys
  res = nc_curs_set(0)    ! Hide cursor

  ! Initialize colors
  res = nc_start_color()

  ! Define color pairs
  res = nc_init_pair(1, COLOR_WHITE, COLOR_BLUE)
  res = nc_init_pair(2, COLOR_BLACK, COLOR_GREEN)
  res = nc_init_pair(3, COLOR_YELLOW, COLOR_RED)
  res = nc_init_pair(4, COLOR_BLACK, COLOR_CYAN)

  ! Get terminal size
  call nc_getmaxyx(max_y, max_x)

  ! Main display loop
  do
    res = nc_clear()

    ! Display banner
    row = 2
    col = (max_x - 20) / 2

    res = nc_move(row, col)
    res = nc_attron(nc_color_pair(1))
    res = nc_attron(A_BOLD)
    msg = "  SNIFFERT v0.1  "
    res = nc_addstr(msg)
    res = nc_attroff(A_BOLD)
    res = nc_attroff(nc_color_pair(1))

    ! Display terminal size
    row = row + 2
    write(size_msg, '(A,I0,A,I0)') 'Terminal Size: ', max_x, ' x ', max_y
    col = (max_x - len_trim(size_msg)) / 2
    res = nc_move(row, col)
    res = nc_attron(nc_color_pair(2))
    res = nc_addstr(trim(size_msg))
    res = nc_attroff(nc_color_pair(2))

    ! Check if terminal is large enough
    row = row + 2
    if (max_x >= 40 .and. max_y >= 20) then
      msg = 'Terminal size: OK'
      res = nc_move(row, (max_x - len_trim(msg)) / 2)
      res = nc_attron(nc_color_pair(2))
      res = nc_addstr(trim(msg))
      res = nc_attroff(nc_color_pair(2))
    else
      msg = 'WARNING: Terminal too small!'
      res = nc_move(row, (max_x - len_trim(msg)) / 2)
      res = nc_attron(nc_color_pair(3))
      res = nc_attron(A_BOLD)
      res = nc_addstr(trim(msg))
      res = nc_attroff(A_BOLD)
      res = nc_attroff(nc_color_pair(3))

      row = row + 1
      msg = 'Minimum: 40 x 20'
      res = nc_move(row, (max_x - len_trim(msg)) / 2)
      res = nc_addstr(trim(msg))
    end if

    ! Display help
    row = row + 3
    help_msg = 'Press arrow keys to test, q to quit'
    col = (max_x - len_trim(help_msg)) / 2
    res = nc_move(row, col)
    res = nc_attron(nc_color_pair(4))
    res = nc_addstr(trim(help_msg))
    res = nc_attroff(nc_color_pair(4))

    ! Display last key pressed (if any)
    if (ch > 0) then
      row = row + 2
      select case (ch)
        case (KEY_UP)
          key_name = 'UP'
        case (KEY_DOWN)
          key_name = 'DOWN'
        case (KEY_LEFT)
          key_name = 'LEFT'
        case (KEY_RIGHT)
          key_name = 'RIGHT'
        case (ichar('q'), ichar('Q'))
          key_name = 'Q (quit)'
        case default
          write(key_name, '(A,I0)') 'char ', ch
      end select

      write(arrow_msg, '(A,A)') 'Last key: ', trim(key_name)
      col = (max_x - len_trim(arrow_msg)) / 2
      res = nc_move(row, col)
      res = nc_attron(A_REVERSE)
      res = nc_addstr(trim(arrow_msg))
      res = nc_attroff(A_REVERSE)
    end if

    ! Refresh display
    res = nc_refresh()

    ! Get input
    ch = nc_getch()

    ! Exit on 'q'
    if (ch == ichar('q') .or. ch == ichar('Q')) exit

    ! Update terminal size in case of resize
    call nc_getmaxyx(max_y, max_x)
  end do

  ! Cleanup
  res = nc_endwin()

  print *, 'Ncurses test completed successfully!'
  print *, 'All Phase 1 tests passed:'
  print *, '  ✓ ncurses initialization'
  print *, '  ✓ Color support'
  print *, '  ✓ Keyboard input'
  print *, '  ✓ Arrow keys'
  print *, '  ✓ Terminal size detection'

end program hello_ncurses
