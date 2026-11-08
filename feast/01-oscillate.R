library(ggplot2)
library(gganimate)
library(dplyr)

# --- 1. Parameters ---
n <- 8
R_macro <- 7.5
delta <- 3.0              # Juggler disc diameter (feet)
#D_target <- 11.5          # Ideal passing distance
D_target <- 1          # Ideal passing distance
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

# --- 3. Optimization with Anisotropic Wells, Passing Pull, & Separation ---
solve_symmetric_master <- function(feast_table, stations, delta, D_target) {
  obj_fn <- function(params) {
    j1_coords <- matrix(params, ncol = 2, byrow = TRUE)
    cost <- 0
    
    weight_pass    <- 200.0 # Aggressive force driving center infiltration for D_target = 11.5 ft
    weight_radial  <- 0.05  # Very soft spring allowing deep in-and-out radial breathing
    #weight_lateral <- 15.0  # Heavy penalty stopping sideways/tangential sliding (no darting)
    weight_lateral <- 150.0  # Heavy penalty stopping sideways/tangential sliding (no darting)
    
    for (b in 1:8) {
      pos_1_b <- j1_coords[b, ]
      
      coords_b <- matrix(0, nrow = 8, ncol = 2)
      for (i in 1:8) {
        angle_shift <- (i - 1) * (2 * pi / 8)
        cos_s <- cos(angle_shift)
        sin_s <- sin(angle_shift)
        rot_m <- matrix(c(cos_s, sin_s, -sin_s, cos_s), ncol = 2, byrow = TRUE)
        coords_b[i, ] <- as.vector(rot_m %*% pos_1_b)
      }
      
      # 1. Passing Distance Penalty (Drives center infiltration)
      active_pairs <- feast_table[[b]]
      if (length(active_pairs) > 0) {
        for (p in active_pairs) {
          dist_ij <- sqrt(sum((coords_b[p[1], ] - coords_b[p[2], ])^2))
          cost <- cost + weight_pass * (dist_ij - D_target)^2
        }
      }
      
      # 2. Anisotropic Station Wells (Restoring force separated into radial vs. lateral components)
      for (i in 1:8) {
        theta_i <- stations$Angle[i]
        station_i <- c(stations$StationX[i], stations$StationY[i])
        
        # Displacement vector from station
        dx <- coords_b[i, 1] - station_i[1]
        dy <- coords_b[i, 2] - station_i[2]
        
        # Unit vectors for station i (Radial outward, Tangential lateral)
        r_hat_x <- cos(theta_i)
        r_hat_y <- sin(theta_i)
        t_hat_x <- -sin(theta_i)
        t_hat_y <- cos(theta_i)
        
        # Decompose displacement into radial (in/out) and lateral (side-to-side)
        delta_radial  <- dx * r_hat_x + dy * r_hat_y
        delta_lateral <- dx * t_hat_x + dy * t_hat_y
        
        # Anisotropic cost contribution
        cost <- cost + weight_radial * (delta_radial)^2 + weight_lateral * (delta_lateral)^2
      }
      
      # 3. Exponential Separation Penalty (Safety threshold at 2 * delta = 6 ft)
      safe_dist <- 2 * delta
      for (i in 1:7) {
        for (j in (i + 1):8) {
          dist_ij <- sqrt(sum((coords_b[i, ] - coords_b[j, ])^2))
          if (dist_ij < safe_dist) {
            cost <- cost + 10000.0 * exp(safe_dist - dist_ij)
          } else {
            cost <- cost + 25.0 * (exp(dist_ij - safe_dist) - 1)
          }
        }
      }
    }
    return(cost)
  }
  
  init_j1 <- matrix(rep(c(stations$StationX[1], stations$StationY[1]), 8), ncol = 2, byrow = TRUE)
  init_params <- as.vector(t(init_j1))
  
  res <- optim(par = init_params, fn = obj_fn, method = "Nelder-Mead", control = list(maxit = 3000, reltol = 1e-9))
  matrix(res$par, ncol = 2, byrow = TRUE)
}

j1_optimal_trace <- solve_symmetric_master(feast_beats_single, station_data, delta, D_target)

# --- 4. Flawless Linear Mapping Evaluator ---
get_phase_shifted_pos <- function(juggler_id, b_num) {
  table_entry_float <- 1 + ((b_num - 1 - (juggler_id - 1) * 0.25) %% 8)
  
  e1 <- floor(table_entry_float)
  e2 <- (e1 %% 8) + 1
  frac <- table_entry_float - e1
  alpha <- (1 - cos(frac * pi)) / 2
  
  pos_1 <- (1 - alpha) * j1_optimal_trace[e1, ] + alpha * j1_optimal_trace[e2, ]
  
  angle_shift <- (juggler_id - 1) * (2 * pi / 8)
  cos_s <- cos(angle_shift)
  sin_s <- sin(angle_shift)
  rot_m <- matrix(c(cos_s, sin_s, -sin_s, cos_s), ncol = 2, byrow = TRUE)
  
  as.vector(rot_m %*% pos_1)
}

# --- 5. Generate Exactly 32 Frames (1 to 32) ---
sim_data <- bind_rows(lapply(1:total_beats, function(b) {
  bind_rows(lapply(1:n, function(i) {
    pt <- get_phase_shifted_pos(i, b)
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
dense_beats <- seq(1, 33, length.out = 200)
static_path_data <- bind_rows(lapply(1:n, function(i) {
  coords <- t(sapply(dense_beats, function(bf) get_phase_shifted_pos(i, bf)))
  data.frame(Juggler = paste0("J", i), ID = i, X = coords[, 1], Y = coords[, 2])
}))

# --- 7. Passing Segments STRICTLY on Passing Beats ---
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

# --- 8. Render with Robust Subtitle Formatting ---
p <- ggplot() +
  geom_point(data = station_data, aes(x = StationX, y = StationY), color = "gray60", size = 2.5, shape = 4) +
  geom_path(data = static_path_data, aes(x = X, y = Y, group = Juggler, color = Juggler), linewidth = 1.0, alpha = 0.6) +
  geom_segment(data = pass_data, aes(x = x, y = y, xend = xend, yend = yend), color = "firebrick", linewidth = 1.4, alpha = 0.9) +
  geom_point(data = sim_data, aes(x = X, y = Y, color = Juggler), size = 4.5) +
  geom_text(data = sim_data, aes(x = X, y = Y, label = ID), color = "white", size = 2.5, fontface = "bold") +
  coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Anisotropic Radial Feast Rhythm ($n=8$, $\\delta = 3$ ft)",
    subtitle = "Beat: {closest_state} of 32 (Passing Event {floor((as.numeric(closest_state)-1)/4)+1} of 8)"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_states(Beat, transition_length = 0, state_length = 1)

print(animate(p, nframes = 32, fps = 2, width = 600, height = 600, rewind = FALSE))
