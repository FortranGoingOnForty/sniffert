module terminal_ui
  use types
  use ncurses_wrapper
  use iso_c_binding
  implicit none
  private

  public :: init_ui, cleanup_ui, render_treemap, handle_input
  public :: check_terminal_size, draw_box, draw_text

  integer, parameter :: MIN_WIDTH = 40
  integer, parameter :: MIN_HEIGHT = 20

  type(c_ptr) :: stdscr

contains

  ! Initialize the terminal UI
  subroutine init_ui()
    integer :: res
    integer :: max_y, max_x

    ! Initialize ncurses
    stdscr = nc_initscr()

    ! Set up terminal modes
    res = nc_cbreak()       ! Disable line buffering
    res = nc_noecho()       ! Don't echo input
    res = nc_keypad(stdscr, .true.)  ! Enable arrow keys
    res = nc_curs_set(0)    ! Hide cursor

    ! Initialize colors if available
    res = nc_start_color()
    if (res == 0) then
      ! Define color pairs
      res = nc_init_pair(1, COLOR_WHITE, COLOR_BLUE)
      res = nc_init_pair(2, COLOR_BLACK, COLOR_GREEN)
      res = nc_init_pair(3, COLOR_WHITE, COLOR_RED)
      res = nc_init_pair(4, COLOR_BLACK, COLOR_YELLOW)
      res = nc_init_pair(5, COLOR_WHITE, COLOR_MAGENTA)
      res = nc_init_pair(6, COLOR_BLACK, COLOR_CYAN)
    end if

    ! Clear screen
    res = nc_clear()
    res = nc_refresh()
  end subroutine init_ui

  ! Clean up and exit ncurses
  subroutine cleanup_ui()
    integer :: res
    res = nc_endwin()
  end subroutine cleanup_ui

  ! Check if terminal size is adequate
  function check_terminal_size() result(is_adequate)
    logical :: is_adequate
    integer :: max_y, max_x

    call nc_getmaxyx(max_y, max_x)
    is_adequate = (max_x >= MIN_WIDTH .and. max_y >= MIN_HEIGHT)
  end function check_terminal_size

  ! Render the treemap
  subroutine render_treemap(root_node, selected_path)
    type(file_node), intent(in) :: root_node
    character(len=*), intent(in), optional :: selected_path
    integer :: res, max_y, max_x
    type(rect) :: screen_bounds

    res = nc_clear()

    ! Get screen dimensions
    call nc_getmaxyx(max_y, max_x)

    ! Set up screen bounds (leave room for status bar)
    screen_bounds%x = 0
    screen_bounds%y = 0
    screen_bounds%width = max_x
    screen_bounds%height = max_y - 2

    ! Render the tree
    call render_node(root_node, screen_bounds, 0, selected_path)

    ! Render status bar
    call render_status_bar(max_y - 1, max_x, root_node)

    res = nc_refresh()
  end subroutine render_treemap

  ! Render a single node and its children
  recursive subroutine render_node(node, bounds, depth, selected_path)
    type(file_node), intent(in) :: node
    type(rect), intent(in) :: bounds
    integer, intent(in) :: depth
    character(len=*), intent(in), optional :: selected_path
    integer :: i, res
    logical :: is_selected
    integer :: color_pair_num

    ! Check if this node is selected
    is_selected = .false.
    if (present(selected_path)) then
      is_selected = (trim(node%path) == trim(selected_path))
    end if

    ! Choose color based on depth and selection
    color_pair_num = mod(depth, 6) + 1

    ! Draw the box for this node
    if (bounds%width > 2 .and. bounds%height > 2) then
      call draw_box(bounds, color_pair_num, is_selected)
      call draw_text(bounds, node%name, is_selected)
    end if

    ! Render children (if layout is calculated)
    if (allocated(node%children)) then
      do i = 1, node%num_children
        ! Note: Child bounds would be calculated by treemap_layout
        ! For now, this is a placeholder
      end do
    end if
  end subroutine render_node

  ! Draw a box at the given bounds
  subroutine draw_box(bounds, color_pair_num, highlighted)
    type(rect), intent(in) :: bounds
    integer, intent(in) :: color_pair_num
    logical, intent(in) :: highlighted
    integer :: x, y, res
    character(len=1) :: corner, horiz, vert

    corner = '+'
    horiz = '-'
    vert = '|'

    ! Set color
    res = nc_attron(color_pair_num)
    if (highlighted) res = nc_attron(A_REVERSE)

    ! Draw top border
    res = nc_move(bounds%y, bounds%x)
    res = nc_addch(ichar(corner))
    do x = bounds%x + 1, bounds%x + bounds%width - 2
      res = nc_addch(ichar(horiz))
    end do
    res = nc_addch(ichar(corner))

    ! Draw sides
    do y = bounds%y + 1, bounds%y + bounds%height - 2
      res = nc_move(y, bounds%x)
      res = nc_addch(ichar(vert))
      res = nc_move(y, bounds%x + bounds%width - 1)
      res = nc_addch(ichar(vert))
    end do

    ! Draw bottom border
    res = nc_move(bounds%y + bounds%height - 1, bounds%x)
    res = nc_addch(ichar(corner))
    do x = bounds%x + 1, bounds%x + bounds%width - 2
      res = nc_addch(ichar(horiz))
    end do
    res = nc_addch(ichar(corner))

    if (highlighted) res = nc_attroff(A_REVERSE)
    res = nc_attroff(color_pair_num)
  end subroutine draw_box

  ! Draw text inside a box
  subroutine draw_text(bounds, text, highlighted)
    type(rect), intent(in) :: bounds
    character(len=*), intent(in) :: text
    logical, intent(in) :: highlighted
    integer :: res, text_x, text_y
    character(len=:), allocatable :: display_text

    if (bounds%width <= 4 .or. bounds%height <= 2) return

    ! Center the text
    text_y = bounds%y + bounds%height / 2
    text_x = bounds%x + 2

    ! Truncate text if needed
    if (len_trim(text) > bounds%width - 4) then
      display_text = text(1:bounds%width-4)
    else
      display_text = trim(text)
    end if

    res = nc_move(text_y, text_x)
    if (highlighted) res = nc_attron(A_BOLD)
    res = nc_addstr(display_text)
    if (highlighted) res = nc_attroff(A_BOLD)
  end subroutine draw_text

  ! Render status bar
  subroutine render_status_bar(y, width, root_node)
    integer, intent(in) :: y, width
    type(file_node), intent(in) :: root_node
    character(len=256) :: status_text
    integer :: res

    write(status_text, '(A,A,A)') '[q]uit [c]hdir [d]elete | ', &
                                   trim(root_node%path), ' '

    res = nc_move(y, 0)
    res = nc_attron(A_REVERSE)
    res = nc_addstr(trim(status_text))
    res = nc_attroff(A_REVERSE)
  end subroutine render_status_bar

  ! Handle user input
  function handle_input() result(action)
    character(len=1) :: action
    integer :: ch

    ch = nc_getch()

    select case (ch)
      case (ichar('q'), ichar('Q'))
        action = 'q'
      case (ichar('c'), ichar('C'))
        action = 'c'
      case (ichar('d'), ichar('D'))
        action = 'd'
      case (KEY_UP)
        action = 'u'
      case (KEY_DOWN)
        action = 'd'
      case (KEY_LEFT)
        action = 'l'
      case (KEY_RIGHT)
        action = 'r'
      case default
        action = ' '
    end select
  end function handle_input

end module terminal_ui
