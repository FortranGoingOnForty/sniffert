module treemap_layout
  use types
  use iso_fortran_env, only: int64, real64
  implicit none
  private

  public :: calculate_treemap

contains

  ! Calculate treemap layout using squarified algorithm
  recursive subroutine calculate_treemap(node, bounds)
    type(file_node), intent(inout) :: node
    type(rect), intent(in) :: bounds
    integer :: i

    ! Set this node's bounds
    node%bounds = bounds

    ! If no children or zero size, nothing to layout
    if (.not. allocated(node%children) .or. node%num_children == 0) then
      return
    end if

    if (node%size == 0) return

    ! Sort children by size (descending) for better aspect ratios
    call sort_by_size(node%children, node%num_children)

    ! Layout children using squarified algorithm
    call squarify(node%children, node%num_children, bounds, node%size)

    ! Recursively layout each child's children
    do i = 1, node%num_children
      if (allocated(node%children(i)%children)) then
        call calculate_treemap(node%children(i), node%children(i)%bounds)
      end if
    end do
  end subroutine calculate_treemap

  ! Squarified treemap layout algorithm (Bruls et al.)
  recursive subroutine squarify(nodes, num_nodes, bounds, total_size)
    type(file_node), dimension(:), intent(inout) :: nodes
    integer, intent(in) :: num_nodes
    type(rect), intent(in) :: bounds
    integer(int64), intent(in) :: total_size

    integer :: i, row_start, row_end
    real(real64) :: remaining_area, row_area, scale_factor, total_pixel_area
    type(rect) :: remaining_bounds, row_bounds
    logical :: layout_horizontal

    ! Only check for invalid inputs, not space constraints (we support scrolling!)
    if (num_nodes == 0 .or. total_size == 0) return
    if (bounds%width <= 0 .or. bounds%height <= 0) return

    ! Calculate scaling factor: maps bytes to pixels²
    total_pixel_area = real(bounds%width, real64) * real(bounds%height, real64)
    scale_factor = total_pixel_area / real(total_size, real64)

    ! Determine layout direction (use shorter dimension for rows)
    layout_horizontal = bounds%width >= bounds%height

    remaining_bounds = bounds
    remaining_area = real(total_size, real64)
    row_start = 1

    do while (row_start <= num_nodes)
      ! Find best row: add items while aspect ratio improves
      row_end = find_best_row(nodes(row_start:num_nodes), &
                             num_nodes - row_start + 1, &
                             remaining_bounds, &
                             layout_horizontal, &
                             scale_factor)
      row_end = row_start + row_end - 1

      ! Calculate row area
      row_area = 0.0_real64
      do i = row_start, row_end
        row_area = row_area + real(nodes(i)%size, real64)
      end do

      ! Layout this row
      if (layout_horizontal) then
        ! Horizontal row (items placed left-to-right)
        call layout_row_horizontal(nodes(row_start:row_end), &
                                   row_end - row_start + 1, &
                                   remaining_bounds, &
                                   row_area, &
                                   scale_factor, &
                                   row_bounds)
        ! Update remaining bounds (move down)
        remaining_bounds%y = remaining_bounds%y + row_bounds%height
        remaining_bounds%height = max(1, remaining_bounds%height - row_bounds%height)

        ! Note: remaining_bounds%height might go to 1 (minimum), but that's OK
        ! The layout_row_horizontal will still calculate proper row heights based on scale_factor
      else
        ! Vertical row (items placed top-to-bottom)
        call layout_row_vertical(nodes(row_start:row_end), &
                                row_end - row_start + 1, &
                                remaining_bounds, &
                                row_area, &
                                scale_factor, &
                                row_bounds)
        ! Update remaining bounds (move right)
        remaining_bounds%x = remaining_bounds%x + row_bounds%width
        remaining_bounds%width = max(1, remaining_bounds%width - row_bounds%width)
      end if

      row_start = row_end + 1
    end do
  end subroutine squarify

  ! Find best row: add items while worst aspect ratio improves
  function find_best_row(nodes, num_nodes, bounds, horizontal, scale_factor) result(row_size)
    type(file_node), dimension(:), intent(in) :: nodes
    integer, intent(in) :: num_nodes
    type(rect), intent(in) :: bounds
    logical, intent(in) :: horizontal
    real(real64), intent(in) :: scale_factor
    integer :: row_size

    real(real64) :: current_area, new_area
    real :: current_worst, new_worst
    integer :: i

    if (num_nodes == 0) then
      row_size = 0
      return
    end if

    row_size = 1
    current_area = real(nodes(1)%size, real64)
    current_worst = calc_worst_aspect_ratio(nodes(1:1), 1, bounds, &
                                            horizontal, current_area, scale_factor)

    ! Try adding items while aspect ratio improves
    do i = 2, num_nodes
      new_area = current_area + real(nodes(i)%size, real64)
      new_worst = calc_worst_aspect_ratio(nodes(1:i), i, bounds, &
                                          horizontal, new_area, scale_factor)

      ! Limit items per row to prevent too many tiny boxes
      ! Check if adding this item would make boxes too small
      if (horizontal .and. bounds%width / i < 10) then
        ! Would make boxes < 10 chars wide, stop here
        exit
      else if (i > 5) then
        ! Max 5 items per row for readability (was 6, reducing further)
        exit
      else if (new_worst <= current_worst * 1.2) then
        ! Aspect ratio acceptable (within 20% tolerance), add to row
        row_size = i
        current_area = new_area
        current_worst = new_worst
      else
        ! Aspect ratio too bad, stop here
        exit
      end if
    end do
  end function find_best_row

  ! Calculate worst aspect ratio for a row of items
  function calc_worst_aspect_ratio(nodes, num_nodes, bounds, horizontal, row_area, scale_factor) result(worst)
    type(file_node), dimension(:), intent(in) :: nodes
    integer, intent(in) :: num_nodes
    type(rect), intent(in) :: bounds
    logical, intent(in) :: horizontal
    real(real64), intent(in) :: row_area, scale_factor
    real :: worst

    real(real64) :: row_dim, other_dim, item_dim, row_pixel_area
    real :: item_aspect
    integer :: i

    worst = 0.0

    if (row_area <= 0.0_real64) then
      worst = huge(1.0)
      return
    end if

    ! Convert row area from bytes to pixels using scale factor
    row_pixel_area = row_area * scale_factor

    if (horizontal) then
      ! Horizontal row: height is fixed, widths vary
      other_dim = real(bounds%width, real64)
      if (other_dim <= 0.0_real64) then
        worst = huge(1.0)
        return
      end if
      row_dim = row_pixel_area / other_dim  ! Height of row in PIXELS

      do i = 1, num_nodes
        ! Convert item size to pixels and calculate width
        item_dim = (real(nodes(i)%size, real64) * scale_factor) / row_dim
        item_aspect = aspect_ratio(int(item_dim), int(row_dim))
        worst = max(worst, item_aspect)
      end do
    else
      ! Vertical row: width is fixed, heights vary
      other_dim = real(bounds%height, real64)
      if (other_dim <= 0.0_real64) then
        worst = huge(1.0)
        return
      end if
      row_dim = row_pixel_area / other_dim  ! Width of row in PIXELS

      do i = 1, num_nodes
        ! Convert item size to pixels and calculate height
        item_dim = (real(nodes(i)%size, real64) * scale_factor) / row_dim
        item_aspect = aspect_ratio(int(row_dim), int(item_dim))
        worst = max(worst, item_aspect)
      end do
    end if
  end function calc_worst_aspect_ratio

  ! Layout a horizontal row (items left-to-right)
  subroutine layout_row_horizontal(nodes, num_nodes, bounds, row_area, scale_factor, row_bounds)
    type(file_node), dimension(:), intent(inout) :: nodes
    integer, intent(in) :: num_nodes
    type(rect), intent(in) :: bounds
    real(real64), intent(in) :: row_area, scale_factor
    type(rect), intent(out) :: row_bounds

    integer :: i, x_offset, item_width, remaining_width
    real(real64) :: row_height, row_pixel_area
    integer, parameter :: MIN_HEIGHT = 3  ! Minimum to show text: 2 content lines + borders

    ! Convert row area from bytes to pixels² using scale factor
    row_pixel_area = row_area * scale_factor

    ! Calculate row height
    if (bounds%width > 0) then
      row_height = row_pixel_area / real(bounds%width, real64)
    else
      row_height = 0.0_real64
    end if

    row_bounds%x = bounds%x
    row_bounds%y = bounds%y
    row_bounds%width = bounds%width
    row_bounds%height = max(MIN_HEIGHT, int(row_height))

    x_offset = bounds%x
    remaining_width = bounds%width

    ! First pass: calculate ideal widths with minimums
    do i = 1, num_nodes
      if (row_height > 0.0_real64) then
        ! Calculate width: (item_size / row_total_size) * row_width
        item_width = int((real(nodes(i)%size, real64) / row_area) * real(bounds%width, real64))
      else
        item_width = 0
      end if

      ! Enforce minimum width for text
      item_width = max(10, item_width)
      nodes(i)%bounds%width = item_width
    end do

    ! Second pass: adjust if total width exceeds bounds
    ! Calculate total width needed
    item_width = 0
    do i = 1, num_nodes
      item_width = item_width + nodes(i)%bounds%width
    end do

    ! If overflow, scale all widths proportionally to fit
    if (item_width > bounds%width) then
      do i = 1, num_nodes
        ! Scale width down proportionally (keep minimum for text)
        nodes(i)%bounds%width = max(10, &
          int((real(nodes(i)%bounds%width, real64) / real(item_width, real64)) * real(bounds%width, real64)))
      end do
    end if

    ! Third pass: assign x positions and handle rounding
    do i = 1, num_nodes
      nodes(i)%bounds%x = x_offset
      nodes(i)%bounds%y = bounds%y
      nodes(i)%bounds%height = row_bounds%height

      if (i == num_nodes) then
        ! Last item gets remaining width to avoid rounding gaps
        remaining_width = bounds%x + bounds%width - x_offset
        ! Ensure last item has at least minimum width
        nodes(i)%bounds%width = max(2, remaining_width)
      end if

      x_offset = x_offset + nodes(i)%bounds%width
    end do
  end subroutine layout_row_horizontal

  ! Layout a vertical row (items top-to-bottom)
  subroutine layout_row_vertical(nodes, num_nodes, bounds, row_area, scale_factor, row_bounds)
    type(file_node), dimension(:), intent(inout) :: nodes
    integer, intent(in) :: num_nodes
    type(rect), intent(in) :: bounds
    real(real64), intent(in) :: row_area, scale_factor
    type(rect), intent(out) :: row_bounds

    integer :: i, y_offset, item_height, remaining_height
    real(real64) :: row_width, row_pixel_area
    integer, parameter :: MIN_WIDTH = 10  ! Minimum to show text: 2 borders + 8 chars

    ! Convert row area from bytes to pixels² using scale factor
    row_pixel_area = row_area * scale_factor

    ! Calculate row width
    if (bounds%height > 0) then
      row_width = row_pixel_area / real(bounds%height, real64)
    else
      row_width = 0.0_real64
    end if

    row_bounds%x = bounds%x
    row_bounds%y = bounds%y
    row_bounds%width = max(MIN_WIDTH, int(row_width))
    row_bounds%height = bounds%height

    y_offset = bounds%y
    remaining_height = bounds%height

    ! First pass: calculate ideal heights with minimums
    do i = 1, num_nodes
      if (row_width > 0.0_real64) then
        ! Calculate height: (item_size / row_total_size) * row_height
        item_height = int((real(nodes(i)%size, real64) / row_area) * real(bounds%height, real64))
      else
        item_height = 0
      end if

      ! Enforce minimum height for text
      item_height = max(3, item_height)
      nodes(i)%bounds%height = item_height
    end do

    ! Second pass: adjust if total height exceeds bounds (allow overflow for scrolling)
    ! Calculate total height needed
    item_height = 0
    do i = 1, num_nodes
      item_height = item_height + nodes(i)%bounds%height
    end do

    ! Note: For vertical, we allow overflow (scrolling), but scale if severely over
    ! to prevent extremely tall boxes
    if (item_height > bounds%height * 10) then
      do i = 1, num_nodes
        ! Scale height down proportionally
        nodes(i)%bounds%height = max(2, &
          int((real(nodes(i)%bounds%height, real64) / real(item_height, real64)) * real(bounds%height * 10, real64)))
      end do
    end if

    ! Third pass: assign y positions
    do i = 1, num_nodes
      nodes(i)%bounds%x = bounds%x
      nodes(i)%bounds%y = y_offset
      nodes(i)%bounds%width = row_bounds%width

      y_offset = y_offset + nodes(i)%bounds%height
    end do
  end subroutine layout_row_vertical

  ! Calculate aspect ratio for a rectangle (always >= 1.0)
  pure function aspect_ratio(width, height) result(ratio)
    integer, intent(in) :: width, height
    real :: ratio

    if (height > 0 .and. width > 0) then
      ratio = real(width) / real(height)
      if (ratio < 1.0) ratio = 1.0 / ratio
    else
      ratio = huge(1.0)
    end if
  end function aspect_ratio

  ! Sort nodes by size (descending) using quicksort
  ! Uses index-based sorting to avoid deep-copying file_node objects
  subroutine sort_by_size(nodes, num_nodes)
    type(file_node), dimension(:), intent(inout) :: nodes
    integer, intent(in) :: num_nodes
    integer, dimension(:), allocatable :: indices
    type(file_node), dimension(:), allocatable :: temp_nodes
    integer :: i

    if (num_nodes <= 1) return

    ! Create index array
    allocate(indices(num_nodes))
    do i = 1, num_nodes
      indices(i) = i
    end do

    ! Quicksort indices by node size (descending)
    call quicksort_indices(nodes, indices, 1, num_nodes)

    ! Reorder nodes according to sorted indices
    ! Use move_alloc to transfer allocatable components without deep copying
    allocate(temp_nodes(num_nodes))
    do i = 1, num_nodes
      ! Move allocatable components (avoids deep copy)
      call move_alloc(nodes(indices(i))%name, temp_nodes(i)%name)
      call move_alloc(nodes(indices(i))%path, temp_nodes(i)%path)
      call move_alloc(nodes(indices(i))%children, temp_nodes(i)%children)
      ! Copy simple types
      temp_nodes(i)%size = nodes(indices(i))%size
      temp_nodes(i)%is_directory = nodes(indices(i))%is_directory
      temp_nodes(i)%access_denied = nodes(indices(i))%access_denied
      temp_nodes(i)%bounds = nodes(indices(i))%bounds
      temp_nodes(i)%num_children = nodes(indices(i))%num_children
    end do

    ! Move back to original array
    do i = 1, num_nodes
      call move_alloc(temp_nodes(i)%name, nodes(i)%name)
      call move_alloc(temp_nodes(i)%path, nodes(i)%path)
      call move_alloc(temp_nodes(i)%children, nodes(i)%children)
      nodes(i)%size = temp_nodes(i)%size
      nodes(i)%is_directory = temp_nodes(i)%is_directory
      nodes(i)%access_denied = temp_nodes(i)%access_denied
      nodes(i)%bounds = temp_nodes(i)%bounds
      nodes(i)%num_children = temp_nodes(i)%num_children
    end do

    deallocate(indices)
    deallocate(temp_nodes)
  end subroutine sort_by_size

  ! Quicksort indices array based on node sizes (descending order)
  recursive subroutine quicksort_indices(nodes, indices, left, right)
    type(file_node), dimension(:), intent(in) :: nodes
    integer, dimension(:), intent(inout) :: indices
    integer, intent(in) :: left, right
    integer :: pivot_idx

    if (left < right) then
      call partition_indices(nodes, indices, left, right, pivot_idx)
      call quicksort_indices(nodes, indices, left, pivot_idx - 1)
      call quicksort_indices(nodes, indices, pivot_idx + 1, right)
    end if
  end subroutine quicksort_indices

  ! Partition for quicksort (sorts in descending order by size)
  subroutine partition_indices(nodes, indices, left, right, pivot_idx)
    type(file_node), dimension(:), intent(in) :: nodes
    integer, dimension(:), intent(inout) :: indices
    integer, intent(in) :: left, right
    integer, intent(out) :: pivot_idx
    integer(int64) :: pivot_size
    integer :: i, j, temp

    ! Use middle element as pivot
    pivot_idx = (left + right) / 2
    pivot_size = nodes(indices(pivot_idx))%size

    ! Move pivot to end
    temp = indices(pivot_idx)
    indices(pivot_idx) = indices(right)
    indices(right) = temp

    ! Partition: larger sizes go to the left
    i = left - 1
    do j = left, right - 1
      if (nodes(indices(j))%size >= pivot_size) then
        i = i + 1
        temp = indices(i)
        indices(i) = indices(j)
        indices(j) = temp
      end if
    end do

    ! Move pivot to its final position
    i = i + 1
    temp = indices(i)
    indices(i) = indices(right)
    indices(right) = temp
    pivot_idx = i
  end subroutine partition_indices

end module treemap_layout
