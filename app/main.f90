program sniffert
  use types
  use file_system, only: is_directory, is_running_as_root, delete_path, get_absolute_path
  use disk_scanner, only: build_tree, calculate_sizes, dump_tree_debug
  use treemap_layout
  use terminal_ui, only: init_ui, cleanup_ui, render_treemap, handle_input, &
                         check_terminal_size, get_terminal_dimensions, confirm_action
  use navigation
  implicit none

  type(file_node) :: root_node
  type(rect) :: screen_bounds
  type(selection_state) :: selection
  character(len=512) :: current_path, selected_path, prompt_msg, parent_path
  character(len=1) :: action
  logical :: running, size_ok, needs_rescan, confirmed, delete_success
  integer :: nargs, max_y, max_x
  integer :: scroll_offset, total_height, last_slash
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

  ! Convert to absolute path (resolves '.' and relative paths)
  current_path = get_absolute_path(current_path)

  ! Warn if running as root
  if (is_running_as_root()) then
    print *, "=========================================="
    print *, "WARNING: Running as root!"
    print *, "=========================================="
    print *
    print *, "Sniffert does NOT need elevated permissions."
    print *, "Running as root is unnecessary and discouraged."
    print *
    print *, "Press Ctrl+C to cancel, or Enter to continue..."
    read *
    print *
  end if

  ! Scan BEFORE initializing UI so we can see errors
  print *, "Sniffert - Disk Space Analyzer"
  print *, "Scanning: ", trim(current_path)
  print *, "(This may take a moment for large directories...)"
  print *
  call build_tree(current_path, root_node)

  ! DEBUG: Dump tree structure to file
  call dump_tree_debug(root_node, '/tmp/sniffert_tree.log')

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
  screen_bounds%height = max_y - 1  ! Leave room for status bar at bottom line

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
        ! Delete - show confirmation and delete selected file/directory
        if (selection%depth >= 0) then
          ! Get the selected path
          selected_path = get_selected_path(selection)

          ! Build confirmation prompt
          write(prompt_msg, '(A,A)') 'Delete "', trim(selected_path) // '"'

          ! Show confirmation dialog
          confirmed = confirm_action(trim(prompt_msg))

          if (confirmed) then
            ! Attempt to delete (tries trash first, then rm)
            delete_success = delete_path(trim(selected_path))

            if (delete_success) then
              ! Deletion successful - trigger rescan from current directory
              needs_rescan = .true.
            end if
            ! If deletion failed, just continue (user will see file still there)
          end if
        end if

      case ('P')
        ! Go up to parent directory (. key)
        ! Find last slash to get parent directory
        last_slash = index(trim(current_path), '/', back=.true.)

        if (last_slash > 1) then
          ! Not at root, go to parent
          parent_path = current_path(1:last_slash-1)

          ! Handle special case: if parent is empty, we're at root
          if (len_trim(parent_path) == 0) then
            parent_path = '/'
          end if

          ! Check if parent is accessible (try to scan it)
          if (is_directory(trim(parent_path))) then
            current_path = parent_path
            needs_rescan = .true.
          end if
          ! If not accessible, just stay in current directory (silent fail per permissions paradigm)
        else if (last_slash == 1) then
          ! Already at root (/something), can't go higher than /
          continue
        end if

      case ('u')
        ! Up arrow - previous sibling
        call move_up(selection, root_node)
        call auto_scroll_to_selection(selection, root_node, scroll_offset, max_y - 1)

      case ('j')
        ! Down arrow - next sibling (j for down since 'd' is delete)
        call move_down(selection, root_node)
        call auto_scroll_to_selection(selection, root_node, scroll_offset, max_y - 1)

      case ('l')
        ! Left arrow - parent
        call move_left(selection, root_node)
        call auto_scroll_to_selection(selection, root_node, scroll_offset, max_y - 1)

      case ('r')
        ! Right arrow - first child
        call move_right(selection, root_node)
        call auto_scroll_to_selection(selection, root_node, scroll_offset, max_y - 1)

      case ('p')
        ! Page Up - scroll up
        scroll_offset = max(0, scroll_offset - (max_y - 1))

      case ('n')
        ! Page Down - scroll down
        scroll_offset = min(max(0, total_height - (max_y - 1)), scroll_offset + (max_y - 1))

      case default
        ! Unknown input, ignore
        continue
    end select

    ! Handle directory change
    if (needs_rescan) then
      ! Re-scan from new directory
      call build_tree(current_path, root_node)

      ! DEBUG: Dump tree structure to file
      call dump_tree_debug(root_node, '/tmp/sniffert_tree.log')

      call init_selection(selection, root_node)

      ! Recalculate layout
      call get_terminal_dimensions(max_y, max_x)
      screen_bounds%x = 0
      screen_bounds%y = 0
      screen_bounds%width = max_x
      screen_bounds%height = max_y - 1  ! Leave room for status bar at bottom line
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

  ! Auto-scroll to keep selected node visible
  subroutine auto_scroll_to_selection(selection, root_node, scroll_offset, viewport_height)
    type(selection_state), intent(in) :: selection
    type(file_node), intent(in) :: root_node
    integer, intent(inout) :: scroll_offset
    integer, intent(in) :: viewport_height
    type(file_node), pointer :: selected_node
    integer :: node_top, node_bottom

    ! Get the selected node
    selected_node => get_selected_node(root_node, selection)
    if (.not. associated(selected_node)) return

    ! Get node's screen position
    node_top = selected_node%bounds%y
    node_bottom = node_top + selected_node%bounds%height

    ! Check if node is above viewport - scroll up
    if (node_top < scroll_offset) then
      scroll_offset = node_top
    end if

    ! Check if node is below viewport - scroll down
    if (node_bottom > scroll_offset + viewport_height) then
      scroll_offset = node_bottom - viewport_height
    end if

    ! Ensure scroll_offset is not negative
    scroll_offset = max(0, scroll_offset)
  end subroutine auto_scroll_to_selection

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
    print *, "  ↑/↓          Move to previous/next sibling (auto-scrolls)"
    print *, "  ←/→          Move to parent/child directory"
    print *, "  PgUp/PgDn    Scroll view up/down (for large directories)"
    print *, "  c            Change directory (drill down into selection)"
    print *, "  q            Quit"
    print *
    print *, "Examples:"
    print *, "  sniffert              # Analyze current directory"
    print *, "  sniffert /var/log     # Analyze /var/log"
    print *, "  sniffert ~/Downloads  # Analyze Downloads folder"
    print *
    print *, "IMPORTANT - Permissions:"
    print *, "  Do NOT run sniffert with sudo!"
    print *
    print *, "  Sniffert analyzes your accessible files. If a directory requires"
    print *, "  elevated permissions, it will be skipped. This is intentional and safe."
    print *, "  Running as root is unnecessary and discouraged."
    print *
  end subroutine print_usage

end program sniffert
