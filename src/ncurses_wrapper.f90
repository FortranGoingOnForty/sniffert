module ncurses_wrapper
  use iso_c_binding
  implicit none
  private

  public :: nc_initscr, nc_endwin, nc_refresh, nc_getch, nc_clear
  public :: nc_move, nc_addch, nc_addstr, nc_getmaxyx
  public :: nc_start_color, nc_init_pair, nc_attron, nc_attroff
  public :: nc_keypad, nc_cbreak, nc_noecho, nc_curs_set
  public :: nc_color_pair
  public :: COLOR_BLACK, COLOR_RED, COLOR_GREEN, COLOR_YELLOW
  public :: COLOR_BLUE, COLOR_MAGENTA, COLOR_CYAN, COLOR_WHITE
  public :: KEY_UP, KEY_DOWN, KEY_LEFT, KEY_RIGHT
  public :: A_NORMAL, A_REVERSE, A_BOLD

  ! ncurses constants (actual values may vary by platform)
  integer(c_int), parameter :: COLOR_BLACK = 0
  integer(c_int), parameter :: COLOR_RED = 1
  integer(c_int), parameter :: COLOR_GREEN = 2
  integer(c_int), parameter :: COLOR_YELLOW = 3
  integer(c_int), parameter :: COLOR_BLUE = 4
  integer(c_int), parameter :: COLOR_MAGENTA = 5
  integer(c_int), parameter :: COLOR_CYAN = 6
  integer(c_int), parameter :: COLOR_WHITE = 7

  integer(c_int), parameter :: KEY_UP = 259
  integer(c_int), parameter :: KEY_DOWN = 258
  integer(c_int), parameter :: KEY_LEFT = 260
  integer(c_int), parameter :: KEY_RIGHT = 261

  integer(c_int), parameter :: A_NORMAL = 0
  integer(c_int), parameter :: A_REVERSE = 262144
  integer(c_int), parameter :: A_BOLD = 2097152

  interface
    ! Initialize ncurses
    function initscr() bind(c, name="initscr")
      use iso_c_binding
      type(c_ptr) :: initscr
    end function initscr

    ! End ncurses
    function endwin() bind(c, name="endwin")
      use iso_c_binding
      integer(c_int) :: endwin
    end function endwin

    ! Refresh screen
    function refresh() bind(c, name="refresh")
      use iso_c_binding
      integer(c_int) :: refresh
    end function refresh

    ! Get character
    function getch() bind(c, name="getch")
      use iso_c_binding
      integer(c_int) :: getch
    end function getch

    ! Clear screen
    function clear() bind(c, name="clear")
      use iso_c_binding
      integer(c_int) :: clear
    end function clear

    ! Move cursor
    function move(y, x) bind(c, name="move")
      use iso_c_binding
      integer(c_int), value :: y, x
      integer(c_int) :: move
    end function move

    ! Add character
    function addch(ch) bind(c, name="addch")
      use iso_c_binding
      integer(c_int), value :: ch
      integer(c_int) :: addch
    end function addch

    ! Add string
    function addstr(str) bind(c, name="addstr")
      use iso_c_binding
      type(c_ptr), value :: str
      integer(c_int) :: addstr
    end function addstr

    ! Start color
    function start_color() bind(c, name="start_color")
      use iso_c_binding
      integer(c_int) :: start_color
    end function start_color

    ! Initialize color pair
    function init_pair(pair, f, b) bind(c, name="init_pair")
      use iso_c_binding
      integer(c_short), value :: pair, f, b
      integer(c_int) :: init_pair
    end function init_pair

    ! Turn on attributes
    function attron(attrs) bind(c, name="attron")
      use iso_c_binding
      integer(c_int), value :: attrs
      integer(c_int) :: attron
    end function attron

    ! Turn off attributes
    function attroff(attrs) bind(c, name="attroff")
      use iso_c_binding
      integer(c_int), value :: attrs
      integer(c_int) :: attroff
    end function attroff

    ! Enable keypad
    function keypad(win, bf) bind(c, name="keypad")
      use iso_c_binding
      type(c_ptr), value :: win
      logical(c_bool), value :: bf
      integer(c_int) :: keypad
    end function keypad

    ! Set cbreak mode
    function cbreak() bind(c, name="cbreak")
      use iso_c_binding
      integer(c_int) :: cbreak
    end function cbreak

    ! Disable echo
    function noecho() bind(c, name="noecho")
      use iso_c_binding
      integer(c_int) :: noecho
    end function noecho

    ! Set cursor visibility
    function curs_set(visibility) bind(c, name="curs_set")
      use iso_c_binding
      integer(c_int), value :: visibility
      integer(c_int) :: curs_set
    end function curs_set

    ! Get maximum y and x (helper function for getmaxyx macro)
    subroutine getmaxyx_helper(y, x) bind(c, name="getmaxyx_helper")
      use iso_c_binding
      integer(c_int) :: y, x
    end subroutine getmaxyx_helper
  end interface

contains

  function nc_initscr() result(win)
    type(c_ptr) :: win
    win = initscr()
  end function nc_initscr

  function nc_endwin() result(res)
    integer :: res
    res = endwin()
  end function nc_endwin

  function nc_refresh() result(res)
    integer :: res
    res = refresh()
  end function nc_refresh

  function nc_getch() result(ch)
    integer :: ch
    ch = getch()
  end function nc_getch

  function nc_clear() result(res)
    integer :: res
    res = clear()
  end function nc_clear

  function nc_move(y, x) result(res)
    integer, intent(in) :: y, x
    integer :: res
    res = move(int(y, c_int), int(x, c_int))
  end function nc_move

  function nc_addch(ch) result(res)
    integer, intent(in) :: ch
    integer :: res
    res = addch(int(ch, c_int))
  end function nc_addch

  function nc_addstr(str) result(res)
    character(len=*), intent(in) :: str
    integer :: res
    character(len=len(str)+1, kind=c_char), target :: c_str
    c_str = trim(str) // c_null_char
    res = addstr(c_loc(c_str))
  end function nc_addstr

  subroutine nc_getmaxyx(max_y, max_x)
    integer, intent(out) :: max_y, max_x
    integer(c_int) :: c_y, c_x
    ! Get terminal dimensions using C helper function
    call getmaxyx_helper(c_y, c_x)
    max_y = int(c_y)
    max_x = int(c_x)
  end subroutine nc_getmaxyx

  function nc_start_color() result(res)
    integer :: res
    res = start_color()
  end function nc_start_color

  function nc_init_pair(pair, fg, bg) result(res)
    integer, intent(in) :: pair, fg, bg
    integer :: res
    res = init_pair(int(pair, c_short), int(fg, c_short), int(bg, c_short))
  end function nc_init_pair

  function nc_attron(attrs) result(res)
    integer, intent(in) :: attrs
    integer :: res
    res = attron(int(attrs, c_int))
  end function nc_attron

  function nc_attroff(attrs) result(res)
    integer, intent(in) :: attrs
    integer :: res
    res = attroff(int(attrs, c_int))
  end function nc_attroff

  function nc_keypad(win, bf) result(res)
    type(c_ptr), intent(in) :: win
    logical, intent(in) :: bf
    integer :: res
    res = keypad(win, logical(bf, c_bool))
  end function nc_keypad

  function nc_cbreak() result(res)
    integer :: res
    res = cbreak()
  end function nc_cbreak

  function nc_noecho() result(res)
    integer :: res
    res = noecho()
  end function nc_noecho

  function nc_curs_set(visibility) result(res)
    integer, intent(in) :: visibility
    integer :: res
    res = curs_set(int(visibility, c_int))
  end function nc_curs_set

  ! COLOR_PAIR macro equivalent
  ! Converts a color pair number to an attribute value
  function nc_color_pair(n) result(attr)
    integer, intent(in) :: n
    integer :: attr
    ! COLOR_PAIR(n) is typically implemented as ((n) << 8)
    attr = ishft(n, 8)
  end function nc_color_pair

end module ncurses_wrapper
