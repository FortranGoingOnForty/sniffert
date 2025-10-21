module terminal_ui
  use types
  use ncurses_wrapper
  use iso_c_binding
  implicit none
  private

  public :: init_ui, cleanup_ui, render_treemap, handle_input
  public :: check_terminal_size, draw_box, draw_text, format_size
  public :: get_terminal_dimensions, confirm_action

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

  ! Render the treemap with scroll offset
  subroutine render_treemap(root_node, scroll_offset, total_height, selected_path)
    type(file_node), intent(in) :: root_node
    integer, intent(in) :: scroll_offset, total_height
    character(len=*), intent(in), optional :: selected_path
    integer :: res, max_y, max_x

    res = nc_clear()

    ! Get screen dimensions
    call nc_getmaxyx(max_y, max_x)

    ! Render the tree using calculated bounds with scroll offset
    call render_node(root_node, 0, scroll_offset, selected_path)

    ! Render status bar with scroll info
    call render_status_bar(max_y - 1, max_x, root_node, scroll_offset, total_height, max_y - 1)

    res = nc_refresh()
  end subroutine render_treemap

  ! Render a single node and its children with scroll offset
  recursive subroutine render_node(node, depth, scroll_offset, selected_path)
    type(file_node), intent(in) :: node
    integer, intent(in) :: depth, scroll_offset
    character(len=*), intent(in), optional :: selected_path
    integer :: i, res, max_y, max_x, ios
    integer, save :: debug_unit = 0
    logical :: is_selected, is_leaf
    integer :: color_pair_num
    type(rect) :: adjusted_bounds
    logical, save :: debug_opened = .false.

    ! Check if this node is selected
    is_selected = .false.
    if (present(selected_path)) then
      is_selected = (trim(node%path) == trim(selected_path))
    end if

    ! Skip rendering if this node has zero dimensions
    if (node%bounds%width < 1 .or. node%bounds%height < 1) then
      return
    end if

    ! Apply scroll offset to bounds
    adjusted_bounds = node%bounds
    adjusted_bounds%y = node%bounds%y - scroll_offset

    ! Get screen dimensions for clipping
    call nc_getmaxyx(max_y, max_x)

    ! Check if this is a leaf node (file or empty directory) - needed for viewport culling
    is_leaf = (.not. allocated(node%children)) .or. (node%num_children == 0)

    ! Open debug file BEFORE any viewport checks
    if (.not. debug_opened .and. depth <= 1) then
      open(newunit=debug_unit, file='/tmp/render_debug.log', status='replace', iostat=ios)
      if (ios == 0) then
        debug_opened = .true.
        write(debug_unit, '(A,I4,A,I4)') '=== Render Debug | max_y=', max_y, ' max_x=', max_x
        flush(debug_unit)
      end if
    end if

    ! Log ALL render attempts when scrolling (BEFORE viewport checks)
    ! Include depth=0 (root) and depth=1 (children) to debug recursion issues
    if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
      write(debug_unit, '(A,I1,A,I3,A,I4,A,I4,A,I3,A,I4,A)') &
        'RENDER d=', depth, ' scroll=', scroll_offset, ' orig_y=', node%bounds%y, &
        ' adj_y=', adjusted_bounds%y, ' h=', adjusted_bounds%height, &
        ' max_y=', max_y, ' "' // trim(node%name) // '"'
      flush(debug_unit)
    end if

    ! Skip if completely above viewport - BUT ONLY FOR LEAF NODES
    ! Containers must recurse into children even if the parent box is out of view
    if (adjusted_bounds%y + adjusted_bounds%height <= 0 .and. is_leaf) then
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
        write(debug_unit, '(A,I1,A)') '  → d=', depth, ' SKIPPED: leaf above viewport'
        flush(debug_unit)
      end if
      return
    end if

    ! Skip if completely below viewport - BUT ONLY FOR LEAF NODES
    ! Containers must recurse into children even if the parent box is out of view
    if (adjusted_bounds%y > max_y - 1 .and. is_leaf) then
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
        write(debug_unit, '(A,I1,A,I4,A,I4)') '  → d=', depth, ' SKIPPED: leaf beyond viewport (y=', adjusted_bounds%y, ' > max_y-1=', max_y - 1, ')'
        flush(debug_unit)
      end if
      return
    end if

    ! Clip bounds to viewport (ncurses cannot render at negative coordinates)
    if (adjusted_bounds%y < 0) then
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1) then
        write(debug_unit, '(A,I1,A,I3,A,I3,A,I3,A)') &
          '  d=', depth, ' BEFORE CLIP: y=', adjusted_bounds%y, ' h=', adjusted_bounds%height, &
          ' scroll=', scroll_offset, ' "' // trim(node%name) // '"'
        flush(debug_unit)
      end if

      ! Box starts above viewport, clip the top portion
      adjusted_bounds%height = adjusted_bounds%height + adjusted_bounds%y
      adjusted_bounds%y = 0

      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1) then
        write(debug_unit, '(A,I1,A,I3,A,I3)') &
          '  d=', depth, ' AFTER CLIP: y=', adjusted_bounds%y, ' h=', adjusted_bounds%height
        flush(debug_unit)
      end if
    end if

    ! Clip bottom if extends beyond content area
    ! Status bar at max_y-1, so content must end by max_y-2
    if (adjusted_bounds%y + adjusted_bounds%height > max_y - 1) then
      adjusted_bounds%height = max((max_y - 1) - adjusted_bounds%y, 0)
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
        write(debug_unit, '(A,I1,A,I3)') '  d=', depth, ' BOTTOM CLIP: new_h=', adjusted_bounds%height
        flush(debug_unit)
      end if
    end if

    ! Skip if clipping resulted in invalid dimensions - BUT ONLY FOR LEAF NODES
    ! Containers with invalid dimensions must still recurse into children
    if ((adjusted_bounds%width < 1 .or. adjusted_bounds%height < 1) .and. is_leaf) then
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
        write(debug_unit, '(A,I1,A,I3,A,I3)') '  → d=', depth, ' SKIPPED: leaf with invalid dims after clip (w=', &
          adjusted_bounds%width, ' h=', adjusted_bounds%height, ')'
        flush(debug_unit)
      end if
      return
    end if

    ! For containers with invalid dimensions, don't draw but DO recurse into children
    if (adjusted_bounds%width < 1 .or. adjusted_bounds%height < 1) then
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
        write(debug_unit, '(A,I1,A,I3,A,I3,A)') '  → d=', depth, ' SKIP DRAW (invalid dims w=', &
          adjusted_bounds%width, ' h=', adjusted_bounds%height, ') but WILL RECURSE'
        flush(debug_unit)
      end if
      ! Don't return - continue to recursion below
      ! But skip the drawing section by jumping to recursion
      goto 100
    end if

    ! Choose color based on depth (cycle through available color pairs)
    color_pair_num = mod(depth, 6) + 1

    ! Draw the box using clipped adjusted bounds
    if (adjusted_bounds%y < max_y - 1) then
      if (debug_opened .and. debug_unit /= 0 .and. depth <= 1 .and. scroll_offset > 0) then
        write(debug_unit, '(A,I1,A,I4,A,I3,A)') &
          '  → d=', depth, ' DRAW_BOX at y=', adjusted_bounds%y, ' h=', adjusted_bounds%height, &
          ' "' // trim(node%name) // '"'
        flush(debug_unit)
      end if

      ! For leaf nodes, fill the box. For directories, just draw border
      call draw_box(adjusted_bounds, color_pair_num, is_selected, is_leaf)

      ! Add text if box is big enough (matches layout minimums)
      if (adjusted_bounds%width >= 10 .and. adjusted_bounds%height >= 3) then
        call draw_text_with_size(adjusted_bounds, node%name, node%size, is_selected)
      end if
    end if

    ! Label for skipping draw but continuing to recursion
100 continue

    ! Recursively render children with same scroll offset
    ! Only show children if box is large enough to meaningfully display them
    ! This prevents cluttered rendering in large directories with many small boxes
    if (allocated(node%children) .and. &
        node%bounds%width >= 40 .and. node%bounds%height >= 10) then
      do i = 1, node%num_children
        call render_node(node%children(i), depth + 1, scroll_offset, selected_path)
      end do
    end if
  end subroutine render_node

  ! Draw a box at the given bounds (filled for leaf nodes, border-only for directories)
  subroutine draw_box(bounds, color_pair_num, highlighted, fill)
    type(rect), intent(in) :: bounds
    integer, intent(in) :: color_pair_num
    logical, intent(in) :: highlighted
    logical, intent(in) :: fill
    integer :: x, y, res
    character(len=1) :: corner, horiz, vert

    ! Use different border characters for selected vs unselected
    if (highlighted) then
      corner = '#'
      horiz = '#'
      vert = '#'
    else
      corner = '+'
      horiz = '-'
      vert = '|'
    end if

    ! Set color and attributes
    res = nc_attron(nc_color_pair(color_pair_num))
    if (highlighted) then
      res = nc_attron(A_BOLD)
      res = nc_attron(A_REVERSE)
    end if

    ! Fill the entire box with background color (only for leaf nodes)
    if (fill) then
      do y = bounds%y, bounds%y + bounds%height - 1
        res = nc_move(y, bounds%x)
        do x = bounds%x, bounds%x + bounds%width - 1
          res = nc_addch(ichar(' '))
        end do
      end do
    end if

    ! Special case for height=1 boxes: just draw a single horizontal line
    if (bounds%height == 1) then
      res = nc_move(bounds%y, bounds%x)
      do x = bounds%x, bounds%x + bounds%width - 1
        res = nc_addch(ichar(horiz))
      end do
      ! Turn off attributes
      if (highlighted) then
        res = nc_attroff(A_REVERSE)
        res = nc_attroff(A_BOLD)
      end if
      res = nc_attroff(nc_color_pair(color_pair_num))
      return
    end if

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

    ! Turn off attributes
    if (highlighted) then
      res = nc_attroff(A_REVERSE)
      res = nc_attroff(A_BOLD)
    end if
    res = nc_attroff(nc_color_pair(color_pair_num))
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

  ! Draw text with size information inside a box
  subroutine draw_text_with_size(bounds, text, size, highlighted)
    use iso_fortran_env, only: int64
    type(rect), intent(in) :: bounds
    character(len=*), intent(in) :: text
    integer(int64), intent(in) :: size
    logical, intent(in) :: highlighted
    integer :: res, text_x, text_y, max_len
    character(len=128) :: display_text, size_str

    if (bounds%width <= 4 .or. bounds%height <= 2) return

    max_len = bounds%width - 4

    ! Format size
    size_str = format_size(size)

    ! If we have room for multiple lines, show name and size separately
    if (bounds%height >= 4) then
      ! Show name on top line
      text_y = bounds%y + 1
      text_x = bounds%x + 2

      if (len_trim(text) > max_len) then
        display_text = text(1:max_len)
      else
        display_text = trim(text)
      end if

      res = nc_move(text_y, text_x)
      if (highlighted) res = nc_attron(A_BOLD)
      res = nc_addstr(trim(display_text))
      if (highlighted) res = nc_attroff(A_BOLD)

      ! Show size on next line
      text_y = bounds%y + 2
      res = nc_move(text_y, text_x)
      res = nc_addstr(trim(size_str))
    else
      ! Single line: show name only
      text_y = bounds%y + bounds%height / 2
      text_x = bounds%x + 2

      if (len_trim(text) > max_len) then
        display_text = text(1:max_len)
      else
        display_text = trim(text)
      end if

      res = nc_move(text_y, text_x)
      if (highlighted) res = nc_attron(A_BOLD)
      res = nc_addstr(trim(display_text))
      if (highlighted) res = nc_attroff(A_BOLD)
    end if
  end subroutine draw_text_with_size

  ! Format file size in human-readable form
  function format_size(bytes) result(str)
    use iso_fortran_env, only: int64
    integer(int64), intent(in) :: bytes
    character(len=20) :: str
    real :: size_kb, size_mb, size_gb

    if (bytes < 1024_int64) then
      write(str, '(I0,A)') bytes, 'B'
    else if (bytes < 1024_int64 * 1024_int64) then
      size_kb = real(bytes) / 1024.0
      write(str, '(F6.2,A)') size_kb, 'KB'
    else if (bytes < 1024_int64 * 1024_int64 * 1024_int64) then
      size_mb = real(bytes) / (1024.0 * 1024.0)
      write(str, '(F6.2,A)') size_mb, 'MB'
    else
      size_gb = real(bytes) / (1024.0 * 1024.0 * 1024.0)
      write(str, '(F6.2,A)') size_gb, 'GB'
    end if
  end function format_size

  ! Render status bar
  subroutine render_status_bar(y, width, root_node, scroll_offset, total_height, viewport_height)
    integer, intent(in) :: y, width, scroll_offset, total_height, viewport_height
    type(file_node), intent(in) :: root_node
    character(len=512) :: status_text
    integer :: res, visible_count, total_count, i, text_len
    integer :: view_start, view_end

    ! Count visible vs total files
    total_count = root_node%num_children
    visible_count = count_visible_nodes(root_node)

    ! Calculate visible range for scroll indicator
    view_start = scroll_offset + 1
    view_end = min(scroll_offset + viewport_height, total_height)

    ! Build status message
    if (total_height > viewport_height) then
      ! Show scroll position if content overflows
      if (visible_count < total_count) then
        write(status_text, '(A,A,A,I0,A,I0,A,I0,A,I0,A)') &
          'Arrows/PgUp/PgDn:Navigate [c]hdir [q]uit | ', &
          trim(root_node%path), ' (', visible_count, ' of ', total_count, &
          ' files) [', view_start, '-', view_end, ' lines]'
      else
        write(status_text, '(A,A,A,I0,A,I0,A)') &
          'Arrows/PgUp/PgDn:Navigate [c]hdir [q]uit | ', &
          trim(root_node%path), ' [', view_start, '-', view_end, ' lines]'
      end if
    else
      ! No scrolling needed
      if (visible_count < total_count) then
        write(status_text, '(A,A,A,I0,A,I0,A)') &
          'Arrows:Navigate [c]hdir [d]el [q]uit | ', &
          trim(root_node%path), ' (', visible_count, ' of ', total_count, ' shown)'
      else
        write(status_text, '(A,A,A)') &
          'Arrows:Navigate [c]hdir [d]el [q]uit | ', trim(root_node%path), ' '
      end if
    end if

    ! Pad to full width with spaces
    text_len = len_trim(status_text)
    do i = text_len + 1, min(width, len(status_text))
      status_text(i:i) = ' '
    end do

    res = nc_move(y, 0)
    res = nc_attron(A_REVERSE)
    res = nc_addstr(status_text(1:min(width, len(status_text))))
    res = nc_attroff(A_REVERSE)
  end subroutine render_status_bar

  ! Count how many child nodes have non-zero dimensions (are visible)
  function count_visible_nodes(node) result(count)
    type(file_node), intent(in) :: node
    integer :: count, i

    count = 0
    if (allocated(node%children)) then
      do i = 1, node%num_children
        if (node%children(i)%bounds%width > 0 .and. &
            node%children(i)%bounds%height > 0) then
          count = count + 1
        end if
      end do
    end if
  end function count_visible_nodes

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
        action = 'j'  ! j for down (vim-style, avoids conflict with 'd'elete)
      case (KEY_LEFT)
        action = 'l'
      case (KEY_RIGHT)
        action = 'r'
      case (KEY_PPAGE)
        action = 'p'  ! Page Up
      case (KEY_NPAGE)
        action = 'n'  ! Page Down
      case default
        action = ' '
    end select
  end function handle_input

  ! Show confirmation prompt and wait for y/n response
  function confirm_action(prompt_msg) result(confirmed)
    character(len=*), intent(in) :: prompt_msg
    logical :: confirmed
    integer :: ch, max_y, max_x, res, i
    character(len=512) :: full_prompt

    ! Get screen dimensions
    call nc_getmaxyx(max_y, max_x)

    ! Build prompt with y/n suffix
    write(full_prompt, '(A,A)') trim(prompt_msg), ' (y/n)? '

    ! Pad to full width
    do i = len_trim(full_prompt) + 1, min(max_x, len(full_prompt))
      full_prompt(i:i) = ' '
    end do

    ! Display prompt at bottom of screen
    res = nc_move(max_y - 1, 0)
    res = nc_attron(A_REVERSE)
    res = nc_attron(A_BOLD)
    res = nc_addstr(full_prompt(1:min(max_x, len(full_prompt))))
    res = nc_attroff(A_BOLD)
    res = nc_attroff(A_REVERSE)
    res = nc_refresh()

    ! Wait for y/n input
    confirmed = .false.
    do
      ch = nc_getch()
      if (ch == ichar('y') .or. ch == ichar('Y')) then
        confirmed = .true.
        exit
      else if (ch == ichar('n') .or. ch == ichar('N')) then
        confirmed = .false.
        exit
      end if
      ! Ignore other keys, keep waiting
    end do
  end function confirm_action

  ! Get terminal dimensions
  subroutine get_terminal_dimensions(max_y, max_x)
    integer, intent(out) :: max_y, max_x
    call nc_getmaxyx(max_y, max_x)
  end subroutine get_terminal_dimensions

end module terminal_ui
