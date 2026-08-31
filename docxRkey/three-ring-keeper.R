#' Create Three-Ring Equal-Area Keeper Plot
#'
#' Generates a three-ring concentric donut chart with equal-area shading
#' and customizable segments, colors, and rotation.
#'
#' @param n_segments Number of segments per ring (default: 12)
#' @param shaded_outer Vector of segment numbers to shade in outer ring
#' @param shaded_middle Vector of segment numbers to shade in middle ring
#' @param shaded_inner Vector of segment numbers to shade in inner ring
#' @param color_grey Color for shaded segments (default: "#d9d9d9")
#' @param color_white Color for unshaded segments (default: "#ffffff")
#' @param rotation Rotation angle in degrees (default: -15)
#' @param stroke_color Border color (default: "#333333")
#' @param stroke_width Border width (default: 0.8)
#' @param title Plot title (default: NULL)
#' @param equal_area If TRUE, middle and outer rings have equal-area segments
#'
#' @return A ggplot object
#'
#' @examples
#' # Default 12-segment design
#' three_ring_keeper()
#'
#' # Custom shading
#' three_ring_keeper(
#'   n_segments = 12,
#'   shaded_outer = c(2, 3, 4, 5, 8, 9, 10, 11),
#'   shaded_middle = c(2, 3, 4, 5, 8, 9, 10, 11),
#'   shaded_inner = c(6, 7, 8, 9, 12, 1, 2, 3),
#'   color_grey = "#4A90E2",
#'   color_white = "#FFFFFF"
#' )
#'
#' # More segments
#' three_ring_keeper(n_segments = 16)
#'
#' @import ggplot2
#' @import dplyr
#' @export

three_ring_keeper <- function(
  n_segments = 12,
  shaded_outer = c(2, 3, 4, 5, 8, 9, 10, 11),
  shaded_middle = c(2, 3, 4, 5, 8, 9, 10, 11),
  shaded_inner = c(6, 7, 8, 9, 12, 1, 2, 3),
  color_grey = "#d9d9d9",
  color_white = "#ffffff",
  rotation = -15,
  stroke_color = "#333333",
  stroke_width = 0.8,
  title = NULL,
  equal_area = TRUE
) {
  
  require(ggplot2)
  require(dplyr)
  
  # Calculate radii for equal area constraint
  if (equal_area) {
    R_inner_outer <- 100
    R_inner_inner <- 100 / 1.2  # ≈ 83.33
    
    # For equal areas: R_middle_outer² - R_middle_inner² = R_middle_inner² - R_inner_inner²
    # With R_middle_inner = 100:
    # R_middle_outer² - 100² = 100² - 83.33²
    R_middle_outer <- sqrt(10000 + (10000 - (100/1.2)^2))  # ≈ 114.26
    
    # For outer ring:
    # R_outer² - R_middle_outer² = R_middle_outer² - 100²
    R_outer <- sqrt(2 * R_middle_outer^2 - 10000)  # ≈ 126.92
  } else {
    R_outer <- 140
    R_middle_outer <- 120
    R_inner_outer <- 100
    R_inner_inner <- 83.33
  }
  
  R_middle_inner <- R_inner_outer
  R_outer_inner <- R_middle_outer
  
  # Create segment data
  create_segments <- function(n_seg, R_outer, R_inner, shaded_segs, ring_name) {
    angle_step <- 360 / n_seg
    segments <- list()
    
    for (i in 1:n_seg) {
      start_angle <- (i - 1) * angle_step
      end_angle <- i * angle_step
      
      # Convert to radians
      start_rad <- start_angle * pi / 180
      end_rad <- end_angle * pi / 180
      
      # Create arc segment
      angle_seq <- seq(start_rad, end_rad, length.out = 50)
      
      # Outer arc
      x_outer <- R_outer * cos(angle_seq)
      y_outer <- R_outer * sin(angle_seq)
      
      # Inner arc (reversed for proper polygon)
      x_inner <- R_inner * cos(rev(angle_seq))
      y_inner <- R_inner * sin(rev(angle_seq))
      
      # Combine into polygon
      x <- c(x_outer, x_inner)
      y <- c(y_outer, y_inner)
      
      # Determine if shaded
      is_shaded <- i %in% shaded_segs
      
      segments[[i]] <- data.frame(
        x = x,
        y = y,
        segment = i,
        ring = ring_name,
        shaded = is_shaded,
        group = paste0(ring_name, "_", i)
      )
    }
    
    do.call(rbind, segments)
  }
  
  # Create all rings
  outer_segs <- create_segments(n_segments, R_outer, R_outer_inner, shaded_outer, "Outer")
  middle_segs <- create_segments(n_segments, R_middle_outer, R_middle_inner, shaded_middle, "Middle")
  inner_segs <- create_segments(n_segments, R_inner_outer, R_inner_inner, shaded_inner, "Inner")
  
  # Combine
  all_segs <- rbind(outer_segs, middle_segs, inner_segs)
  
  # Apply rotation
  rot_angle <- rotation * pi / 180
  all_segs$x_rot <- all_segs$x * cos(rot_angle) - all_segs$y * sin(rot_angle)
  all_segs$y_rot <- all_segs$x * sin(rot_angle) + all_segs$y * cos(rot_angle)
  
  # Determine colors
  all_segs$fill_color <- ifelse(all_segs$shaded, color_grey, color_white)
  
  # Create plot
  p <- ggplot(all_segs, aes(x = x_rot, y = y_rot, fill = fill_color, group = group)) +
    geom_polygon(color = stroke_color, size = stroke_width) +
    scale_fill_identity() +
    coord_equal() +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "none"
    )
  
  if (!is.null(title)) {
    p <- p + labs(title = title)
  }
  
  return(p)
}

# Example usage:
# three_ring_keeper()
# three_ring_keeper(n_segments = 16, color_grey = "#4A90E2")
# three_ring_keeper(
#   n_segments = 12,
#   shaded_outer = c(1, 2, 3, 4),
#   shaded_middle = c(6, 7, 8, 9),
#   shaded_inner = c(11, 12),
#   color_grey = "#E74C3C",
#   color_white = "#ECF0F1"
# )
