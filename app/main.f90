program sniffert
  use types
  use file_system
  use disk_scanner
  use treemap_layout
  use terminal_ui
  implicit none

  type(file_node) :: root_node
  type(rect) :: screen_bounds
  character(len=512) :: current_path, selected_path
  character(len=1) :: action
  logical :: running, size_ok
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

  ! Initialize terminal UI
  call init_ui()

  ! Check terminal size
  size_ok = check_terminal_size()
  if (.not. size_ok) then
    call cleanup_ui()
    print *, "ERROR: Terminal too small. Minimum 40x20 required."
    stop 1
  end if

  ! Scan initial directory
  call build_tree(current_path, root_node)
  selected_path = current_path

  ! Calculate treemap layout for initial screen size
  call get_terminal_dimensions(max_y, max_x)
  screen_bounds%x = 0
  screen_bounds%y = 0
  screen_bounds%width = max_x
  screen_bounds%height = max_y - 2  ! Leave room for status bar
  call calculate_treemap(root_node, screen_bounds)

  ! Main loop
  do while (running)
    ! Render the current view
    call render_treemap(root_node, selected_path)

    ! Handle input
    action = handle_input()

    select case (action)
      case ('q')
        ! Quit
        running = .false.

      case ('c')
        ! Change directory (stub - needs implementation)
        ! Would navigate into the selected directory
        continue

      case ('d')
        ! Delete (stub - needs confirmation dialog and implementation)
        ! Would show warning and delete selected directory
        continue

      case default
        ! Unknown input or arrow keys, ignore for now
        ! Arrow key navigation will be implemented in Phase 5
        continue
    end select
  end do

  ! Cleanup
  call cleanup_ui()

  print *, "Sniffert terminated successfully."

end program sniffert
