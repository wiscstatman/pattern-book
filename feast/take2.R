library(ggplot2)
library(gganimate)
library(dplyr)

# --- 1. Parameters & Geometry Setup ---
n <- 8                    # Number of jugglers
delta <- 3.0              # Juggler body disc diameter (feet)
# Calculate minimum radius R so adjacent chord distance >= delta * 1.2 for safety
min_R <- (delta * 1.2) / (2 * sin(pi / n))
R_macro <- max(6.5, min_R)# Radius of the macro-circle (feet)
r_micro <- 0.75           # Amplitude of the local micro-orbit (feet)

cycles <- 2               # Number of full 8-beat rotation cycles
beats_per_cycle <- n      # 8 beats in the feast cycle for n=8
frames_per_beat <- 15
total_beats <- cycles * beats_per_cycle
total_frames <- total_beats * frames_per_beat

time_steps <- seq(0, 2 * pi * cycles, length.out = total_frames)

# --- 2. Generate Juggler Stations (Clockwise from Top) ---
# Clockwise ordering: J1 at top (pi/2), J2, J3... proceeding clockwise.
station_data <- data.frame(ID = 1:n) %>%
  mutate(
    Angle = pi/2 - (2 * pi * (ID - 1)) / n,
    StationX = R_macro * cos(Angle),
    StationY = R_macro * sin(Angle)
  )

# --- 3. Simulate Trajectories with Micro-Orbits ---
df_list <- list()

for (i in 1:n) {
  s_x <- station_data$StationX[i]
  s_y <- station_data$StationY[i]
  # Phase lag propagating clockwise around the ring
  phase_lag <- - (2 * pi / n) * (i - 1)
  
  for (t_idx in seq_along(time_steps)) {
    t <- time_steps[t_idx]
    # Local orbital adjustment vector
    r_x <- r_micro * cos(t + phase_lag)
    r_y <- r_micro * sin(t + phase_lag)
    
    df_list[[length(df_list) + 1]] <- data.frame(
      Juggler = paste0("J", i),
      ID = i,
      Frame = t_idx,
      X = s_x + r_x,
      Y = s_y + r_y,
      StationX = s_x,
      StationY = s_y
    )
  }
}

sim_data <- bind_rows(df_list)

# --- 4. Define the 8-Beat Parallel Feast Matchings for n=8 ---
# Strictly parallel non-crossing chords/diameters satisfying the clockwise sequence.
feast_beats <- list(
  list(c(1,8), c(2,7), c(3,6), c(4,5)), # Beat 1: 4 parallel pairs (diameters/chords)
  list(c(2,8), c(3,7), c(4,6)),         # Beat 2: 3 parallel pairs (J1 & J5 on phantom)
  list(c(1,2), c(3,8), c(4,7), c(5,6)), # Beat 3: 4 parallel pairs
  list(c(1,3), c(8,4), c(7,5)),         # Beat 4: 3 parallel pairs (J2 & J6 on phantom)
  list(c(1,4), c(8,5), c(7,6), c(2,3)), # Beat 5: 4 parallel pairs
  list(c(1,5), c(2,4), c(6,7)),         # Beat 6: 3 parallel pairs (J3 & J8 on phantom)
  list(c(1,6), c(2,5), c(3,4), c(7,8)), # Beat 7: 4 parallel pairs
  list(c(1,7), c(2,6), c(3,5))          # Beat 8: 3 parallel pairs (J4 & J8 phantom-equivalent)
)

pass_list <- list()

for (t_idx in 1:total_frames) {
  current_beat <- ((t_idx - 1) %/% frames_per_beat) %% beats_per_cycle + 1
  active_pairs <- feast_beats[[current_beat]]
  
  for (p in active_pairs) {
    j1 <- p[1]
    j2 <- p[2]
    
    pos1 <- sim_data %>% filter(ID == j1, Frame == t_idx)
    pos2 <- sim_data %>% filter(ID == j2, Frame == t_idx)
    
    pass_list[[length(pass_list) + 1]] <- data.frame(
      Frame = t_idx,
      Beat = current_beat,
      x = pos1$X,
      y = pos1$Y,
      xend = pos2$X,
      yend = pos2$Y,
      Pair = paste0("J", j1, "-J", j2)
    )
  }
}

pass_data <- bind_rows(pass_list)

# --- 5. Generate Disc Polygons for Visualization (Diameter = delta) ---
# Creates smooth circular polygons representing each juggler's physical footprint (delta = 3ft)
disc_res <- 16
disc_angles <- seq(0, 2 * pi, length.out = disc_res)
disc_list <- list()

for (t_idx in seq(1, total_frames, by = 2)) { # Sample frames for performance
  frame_sim <- sim_data %>% filter(Frame == t_idx)
  for (i in 1:nrow(frame_sim)) {
    row <- frame_sim[i,]
    for (a in disc_angles) {
      disc_list[[length(disc_list) + 1]] <- data.frame(
        Frame = t_idx,
        Juggler = row$Juggler,
        X = row$X + (delta / 2) * cos(a),
        Y = row$Y + (delta / 2) * sin(a)
      )
    }
  }
}
disc_data <- bind_rows(disc_list)

# --- 6. Build the ggplot Animation ---
p <- ggplot() +
  # Draw light-gray physical body discs (delta = 3 ft footprint)
  geom_polygon(data = disc_data, aes(x = X, y = Y, group = interaction(Juggler, Frame)), 
               fill = "lightgray", color = "darkgray", alpha = 0.5, linewidth = 0.4) +
  
  # Draw static anchor stations (crosses)
  geom_point(data = station_data, aes(x = StationX, y = StationY), color = "gray60", size = 2.5, shape = 4) +
  
  # Draw persistent motion traces of the jugglers' center points
  geom_path(data = sim_data, aes(x = X, y = Y, group = Juggler, color = Juggler), alpha = 0.4, linewidth = 0.8) +
  
  # Draw parallel passing segments for the active beat
  geom_segment(data = pass_data, aes(x = x, y = y, xend = xend, yend = yend), color = "firebrick", linewidth = 1.1, alpha = 0.85) +
  
  # Draw juggler center points and labels
  geom_point(data = sim_data, aes(x = X, y = Y, color = Juggler), size = 3) +
  geom_text(data = sim_data, aes(x = X, y = Y, label = ID), color = "white", size = 2.2, fontface = "bold") +
  
  coord_fixed(xlim = c(-10, 10), ylim = c(-10, 10)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Clockwise Feast Pattern ($n=8$, $\\delta = 3$ ft Discs)",
    subtitle = "Beat: {closest_state} (Parallel Non-Crossing Exchanges)",
    x = "X Position (Feet)",
    y = "Y Position (Feet)"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_states(Frame, transition_length = 1, state_length = 1)

print(p)
