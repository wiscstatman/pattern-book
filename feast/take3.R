library(ggplot2)
library(gganimate)
library(dplyr)

# --- 1. Parameters ---
n <- 8
R_macro <- 7.5
delta <- 3.0              # Juggler disc diameter (feet)
D_target <- 11.5          # Ideal passing distance
total_beats <- 32         # Exactly 32 beats per cycle (1 frame per beat)

# Station coordinates (Clockwise from top)
station_angles <- pi/2 - (2 * pi * (0:(n-1))) / n
station_data <- data.frame(
  ID = 1:n,
  Angle = station_angles,
  StationX = R_macro * cos(station_angles),
  StationY = R_macro * sin(station_angles)
)

# --- 2. Official Verified 8-Beat Feast Passing Table ---
feast_beats_single <- list(
  list(c(1,8), c(2,7), c(3,6), c(4,5)), # Event 1 (Beat 1)
  list(c(2,8), c(3,7), c(4,6)),         # Event 2 (Beat 5)
  list(c(1,2), c(3,8), c(4,7), c(5,6)), # Event 3 (Beat 9)
  list(c(1,3), c(8,4), c(7,5)),         # Event 4 (Beat 13)
  list(c(1,4), c(8,5), c(7,6), c(2,3)), # Event 5 (Beat 17)
  list(c(6,8), c(2,4), c(1,5)),         # Event 6 (Beat 21: J6-J8, J7 rests)
  list(c(4,3), c(7,8), c(1,6), c(2,5)), # Event 7 (Beat 25: J4-J3, J7-J8)
  list(c(1,7), c(2,6), c(3,5))          # Event 8 (Beat 29)
)

passing_beats <- c(1, 5, 9, 13, 17, 21, 25, 29)

# --- 3. Variational Optimization for the Table ---
solve_beat_positions <- function(active_pairs, stations, delta, D_target) {
  n_juggler <- nrow(stations)
  obj_fn <- function(params) {
    coords <- matrix(params, ncol = 2, byrow = TRUE)
    cost <- 0
    
    # Adjustable weight for passing distance precision
    weight_pass <- 10.0
    
    if (length(active_pairs) > 0) {
      for (p in active_pairs) {
        dist_ij <- sqrt(sum((coords[p[1], ] - coords[p[2], ])^2))
        cost <- cost + weight_pass * (dist_ij - D_target)^2
      }
    }
    for (i in 1:n_juggler) {
      station_i <- c(stations$StationX[i], stations$StationY[i])
      cost <- cost + 0.4 * sum((coords[i, ] - station_i)^2)
    }
    for (i in 1:(n_juggler - 1)) {
      for (j in (i + 1):n_juggler) {
        dist_ij <- sqrt(sum((coords[i, ] - coords[j, ])^2))
        if (dist_ij < delta) {
          cost <- cost + 2000 * (delta - dist_ij)^2
        }
      }
    }
    return(cost)
  }
  init_params <- as.vector(t(as.matrix(stations[, c("StationX", "StationY")])))
  res <- optim(par = init_params, fn = obj_fn, method = "Nelder-Mead", control = list(maxit = 400))
  matrix(res$par, ncol = 2, byrow = TRUE)
}

# Solve global positions for all 8 table entries
optimized_solutions <- lapply(feast_beats_single, solve_beat_positions, stations = station_data, delta = delta, D_target = D_target)

# --- 4. Synchronous Position Evaluator with Spatial Rotational Invariance ---
# Each juggler executes the exact same master shape (from J1's optimization), 
# rotated spatially around their unique station angle.
master_pos_matrix <- optimized_solutions[[1]] # Use Entry 1 as reference shape
master_station_x <- station_data$StationX[1]
master_station_y <- station_data$StationY[1]
master_phi <- station_data$Angle[1]

# Compute master local offsets relative to J1's station
master_offsets <- t(sapply(1:8, function(e) {
  pos_e <- optimized_solutions[[e]][1, ]
  dx <- pos_e[1] - master_station_x
  dy <- pos_e[2] - master_station_y
  c(dx * cos(master_phi) + dy * sin(master_phi), 
    -dx * sin(master_phi) + dy * cos(master_phi))
}))

# Evaluate any juggler's position at any global table entry (1 to 8, with smooth interpolation between entries)
get_synchronized_pos <- function(juggler_id, table_time) {
  e1 <- floor(((table_time - 1) %% 8)) + 1
  e2 <- (e1 %% 8) + 1
  frac <- table_time - floor(table_time)
  alpha <- (1 - cos(frac * pi)) / 2
  
  # Interpolate local offset shape
  local_x <- (1 - alpha) * master_offsets[e1, 1] + alpha * master_offsets[e2, 1]
  local_y <- (1 - alpha) * master_offsets[e1, 2] + alpha * master_offsets[e2, 2]
  
  # Rotate spatially into juggler's station orientation
  phi <- station_angles[juggler_id]
  s_x <- station_data$StationX[juggler_id]
  s_y <- station_data$StationY[juggler_id]
  
  c(s_x + (local_x * cos(phi) - local_y * sin(phi)),
    s_y + (local_x * sin(phi) + local_y * cos(phi)))
}

# --- 5. Generate Exactly 32 Frames in Strict Chronological Order ---
sim_data <- bind_rows(lapply(1:total_beats, function(b) {
  # Map beat b (1 to 32) smoothly across the 8 table entries (1.0 to 9.0)
  table_time <- 1 + ((b - 1) / 4)
  
  bind_rows(lapply(1:n, function(i) {
    pt <- get_synchronized_pos(i, table_time)
    data.frame(
      Juggler = paste0("J", i), 
      ID = i, 
      Beat = factor(b, levels = 1:total_beats), 
      X = pt[1], 
      Y = pt[2]
    )
  }))
}))

# --- 6. Permanent Invariant Orbital Traces ---
dense_times <- seq(1, 9, length.out = 150)
static_path_data <- bind_rows(lapply(1:n, function(i) {
  coords <- t(sapply(dense_times, function(tf) get_synchronized_pos(i, tf)))
  data.frame(Juggler = paste0("J", i), ID = i, X = coords[, 1], Y = coords[, 2])
}))

# --- 7. Passing Segments STRICTLY on Beats 1, 5, 9, 13, 17, 21, 25, 29 ---
pass_data <- bind_rows(lapply(passing_beats, function(b) {
  entry_idx <- match(b, passing_beats)
  active_pairs <- feast_beats_single[[entry_idx]]
  frame_sim <- sim_data[sim_data$Beat == factor(b, levels = 1:total_beats), ]
  
  data.frame(
    Beat = factor(b, levels = 1:total_beats),
    x = sapply(active_pairs, function(p) frame_sim$X[frame_sim$ID == p[1]]),
    y = sapply(active_pairs, function(p) frame_sim$Y[frame_sim$ID == p[1]]),
    xend = sapply(active_pairs, function(p) frame_sim$X[frame_sim$ID == p[2]]),
    yend = sapply(active_pairs, function(p) frame_sim$Y[frame_sim$ID == p[2]])
  )
}))

# --- 8. Render in Strict Chronological Order at Controlled Speed ---
p <- ggplot() +
  # Static anchor stations
  geom_point(data = station_data, aes(x = StationX, y = StationY), color = "gray60", size = 2.5, shape = 4) +
  
  # Permanent rotationally invariant orbital traces
  geom_path(data = static_path_data, aes(x = X, y = Y, group = Juggler, color = Juggler), linewidth = 1.0, alpha = 0.6) +
  
  # Transient passing lines (strictly on passing beats 1, 5, 9...)
  geom_segment(data = pass_data, aes(x = x, y = y, xend = xend, yend = yend), color = "firebrick", linewidth = 1.4, alpha = 0.9) +
  
  # Juggler center points and labels
  geom_point(data = sim_data, aes(x = X, y = Y, color = Juggler), size = 4.5) +
  geom_text(data = sim_data, aes(x = X, y = Y, label = ID), color = "white", size = 2.5, fontface = "bold") +
  
  coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Synchronized 32-Beat Feast Rhythm ($n=8$, $\\delta = 3$ ft)",
    subtitle = "Beat: {closest_state} of 32 (Passing Event {floor((closest_state-1)/4)+1} of 8)"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_states(Beat, transition_length = 0, state_length = 1)

# Render explicitly with print() and slow fps
print(animate(p, nframes = 32, fps = 2, width = 600, height = 600))
