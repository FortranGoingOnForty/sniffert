program sniffert
  use types
  use file_system
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
  character(len=256) :: arg

  ! Initialize
  running = .true.

  ! Get command line argument for starting directory
  nargs = command_argument_count()
  if (nargs >= 1) then
    call get_command_argument(1, arg)
    current_path = trim(arg)
  else
    ! Default to current directory
    current_path = '.'
  end if

  ! Scan BEFORE initializing UI so we can see errors
  print *, "Scanning directory: ", trim(current_path)
  call build_tree(current_path, root_node)
  print *, "Scan complete. Found ", root_node%num_children, " items"

  ! Initialize terminal UI
  call init_ui()

  ! Check terminal size
  size_ok = check_terminal_size()
  if (.not. size_ok) then
    call cleanup_ui()
    print *, "ERROR: Terminal too small. Minimum 40x20 required."
    stop 1
  end if

  ! Initialize selection state
  call init_selection(selection, root_node)

  ! Calculate treemap layout for initial screen size
  call get_terminal_dimensions(max_y, max_x)
  screen_bounds%x = 0
  screen_bounds%y = 0
  screen_bounds%width = max_x
  screen_bounds%height = max_y - 2  ! Leave room for status bar
  call calculate_treemap(root_node, screen_bounds)

  ! Main loop
  needs_rescan = .false.
  do while (running)
    ! Render the current view with selection
    call render_treemap(root_node, get_selected_path(selection))

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

      needs_rescan = .false.
    end if
  end do

  ! Cleanup
  call cleanup_ui()

  print *, "Sniffert terminated successfully."

end program sniffert
