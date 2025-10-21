program sniffert
  use types
  use file_system, only: is_directory
  use disk_scanner
  use treemap_layout
  use terminal_ui
  use navigation
  implicit none

  type(file_node) :: root_node
  type(rect) :: screen_bounds
  type(selection_state) :: selection
  character(len=512) :: current_path
  character(len=1) :: action
  logical :: running, size_ok, needs_rescan
  integer :: nargs, max_y, max_x
  integer :: scroll_offset, total_height
  character(len=256) :: arg

  ! Initialize
  running = .true.
  scroll_offset = 0

  ! Get command line argument for starting directory
  nargs = command_argument_count()
  if (nargs >= 1) then
    call get_command_argument(1, arg)

    ! Check for help flag
    if (trim(arg) == '-h' .or. trim(arg) == '--help') then
      call print_usage()
      stop 0
    end if

    current_path = trim(arg)
  else
    ! Default to current directory
    current_path = '.'
  end if

  ! Validate that path exists and is a directory
  if (.not. is_directory(current_path)) then
    print *, "ERROR: '", trim(current_path), "' is not a valid directory"
    print *
    call print_usage()
    stop 1
  end if

  ! Scan BEFORE initializing UI so we can see errors
  print *, "Sniffert - Disk Space Analyzer"
  print *, "Scanning: ", trim(current_path)
  print *, "(This may take a moment for large directories...)"
  print *
  call build_tree(current_path, root_node)
  print *, "Scan complete! Found ", root_node%num_children, " items"
  print *, "Starting interactive view..."
  print *

  ! Initialize terminal UI
  call init_ui()

  ! Check terminal size
  size_ok = check_terminal_size()
  if (.not. size_ok) then
    call cleanup_ui()
    print *, "ERROR: Terminal too small. Minimum 40x20 required."
    stop 1
  end if

  ! NOW we can get terminal dimensions and calculate layout
  call get_terminal_dimensions(max_y, max_x)
  screen_bounds%x = 0
  screen_bounds%y = 0
  screen_bounds%width = max_x
  screen_bounds%height = max_y - 2  ! Leave room for status bar

  call calculate_treemap(root_node, screen_bounds)

  ! Calculate total height of treemap (may exceed screen)
  total_height = calculate_total_height(root_node)

  ! Initialize selection state
  call init_selection(selection, root_node)

  ! Main loop
  needs_rescan = .false.
  do while (running)
    ! Render the current view with selection and scroll offset
    call render_treemap(root_node, scroll_offset, total_height, get_selected_path(selection))

    ! Handle input
    action = handle_input()

    select case (action)
      case ('q')
        ! Quit
        running = .false.

      case ('c')
        ! Change directory - drill down into selected node
        if (selection%depth >= 0) then
          current_path = get_selected_path(selection)
          needs_rescan = .true.
        end if

      case ('d')
        ! Delete (stub - needs confirmation dialog and implementation)
        ! Would show warning and delete selected directory
        continue

      case ('u')
        ! Up arrow - previous sibling
        call move_up(selection, root_node)

      case ('j')
        ! Down arrow - next sibling (j for down since 'd' is delete)
        call move_down(selection, root_node)

      case ('l')
        ! Left arrow - parent
        call move_left(selection, root_node)

      case ('r')
        ! Right arrow - first child
        call move_right(selection, root_node)

      case ('p')
        ! Page Up - scroll up
        scroll_offset = max(0, scroll_offset - (max_y - 2))

      case ('n')
        ! Page Down - scroll down
        scroll_offset = min(max(0, total_height - (max_y - 2)), scroll_offset + (max_y - 2))

      case default
        ! Unknown input, ignore
        continue
    end select

    ! Handle directory change
    if (needs_rescan) then
      ! Re-scan from new directory
      call build_tree(current_path, root_node)
      call init_selection(selection, root_node)

      ! Recalculate layout
      call get_terminal_dimensions(max_y, max_x)
      screen_bounds%x = 0
      screen_bounds%y = 0
      screen_bounds%width = max_x
      screen_bounds%height = max_y - 2
      call calculate_treemap(root_node, screen_bounds)

      ! Recalculate total height
      total_height = calculate_total_height(root_node)
      scroll_offset = 0

      needs_rescan = .false.
    end if
  end do

  ! Cleanup
  call cleanup_ui()

  print *, "Sniffert terminated successfully."

contains

  ! Calculate total height of treemap (including overflow)
  recursive function calculate_total_height(node) result(total)
    type(file_node), intent(in) :: node
    integer :: total, i, max_child_bottom

    if (.not. allocated(node%children) .or. node%num_children == 0) then
      total = node%bounds%y + node%bounds%height
      return
    end if

    ! Find the bottom-most child
    max_child_bottom = 0
    do i = 1, node%num_children
      max_child_bottom = max(max_child_bottom, calculate_total_height(node%children(i)))
    end do

    total = max(node%bounds%y + node%bounds%height, max_child_bottom)
  end function calculate_total_height

  ! Print usage information
  subroutine print_usage()
    print *, "Usage: sniffert [DIRECTORY]"
    print *
    print *, "A terminal-based disk space analyzer with interactive treemap visualization."
    print *
    print *, "Arguments:"
    print *, "  DIRECTORY    Path to analyze (default: current directory)"
    print *, "  -h, --help   Show this help message"
    print *
    print *, "Interactive Controls:"
    print *, "  Arrow Keys   Navigate through files and directories"
    print *, "  ↑/↓          Move to previous/next sibling"
    print *, "  ←/→          Move to parent/child directory"
    print *, "  c            Change directory (drill down into selection)"
    print *, "  q            Quit"
    print *
    print *, "Examples:"
    print *, "  sniffert              # Analyze current directory"
    print *, "  sniffert /var/log     # Analyze /var/log"
    print *, "  sniffert ~/Downloads  # Analyze Downloads folder"
    print *
  end subroutine print_usage

end program sniffert
