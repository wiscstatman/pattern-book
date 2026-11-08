library(ggplot2)
library(gganimate)
library(dplyr)

# --- 1. Simulation Parameters ---
n <- 6                # Number of jugglers
R_macro <- 5          # Radius of the macro-circle (stations)
r_micro <- 0.9        # Amplitude of the local oscillation (orbit)
cycles <- 2           # Number of full oscillation cycles to animate
beats_per_cycle <- 6  # Exactly 6 beats in the full feast rotation
frames_per_beat <- 15
total_beats <- cycles * beats_per_cycle
total_frames <- total_beats * frames_per_beat

time_steps <- seq(0, 2 * pi * cycles, length.out = total_frames)

# --- 2. Generate Juggler Position Data with Phase-Shifted Orbits ---
df_list <- list()

for (i in 1:n) {
  station_angle <- (i - 1) * (2 * pi / n)
  S_x <- R_macro * cos(station_angle)
  S_y <- R_macro * sin(station_angle)
  phase_lag <- (2 * pi / n) * (i - 1)
  
  for (t_idx in seq_along(time_steps)) {
    t <- time_steps[t_idx]
    r_x <- r_micro * cos(t + phase_lag)
    r_y <- r_micro * sin(t + phase_lag)
    
    df_list[[length(df_list) + 1]] <- data.frame(
      Juggler = paste0("J", i),
      ID = i,
      Frame = t_idx,
      X = S_x + r_x,
      Y = S_y + r_y,
      StationX = S_x,
      StationY = S_y
    )
  }
}

sim_data <- bind_rows(df_list)

# --- 3. Define the Rigorously Parallel Feast Matchings ---
# Alternates 2 and 3 exchanges. All pairs on 2-exchange beats are strictly parallel chords/edges.
feast_beats <- list(
  list(c(2,3), c(5,6)),         # Beat 1: 2 exchanges (Parallel Span-1 edges) -> J2 -> J3
  list(c(2,4), c(5,1)),         # Beat 2: 2 exchanges (Parallel Span-2 chords) -> J2 -> J4
  list(c(2,5), c(1,4), c(3,6)), # Beat 3: 3 exchanges (The 3 Diameters)         -> J2 -> J5
  list(c(2,6), c(3,5)),         # Beat 4: 2 exchanges (Parallel Span-2 chords) -> J2 -> J6
  list(c(2,1), c(4,5)),         # Beat 5: 2 exchanges (Parallel Span-1 edges) -> J2 -> J1
  list(c(1,3), c(4,6))          # Beat 6: 2 exchanges (Parallel Span-2 chords) -> J2 break
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

# --- 4. Build the ggplot Animation ---
p <- ggplot() +
  # Draw static anchor stations (gray crosses)
  geom_point(data = sim_data %>% filter(Frame == 1), aes(x = StationX, y = StationY), color = "gray75", size = 3, shape = 4) +
  
  # Draw persistent motion traces of the local orbits
  geom_path(data = sim_data, aes(x = X, y = Y, group = Juggler, color = Juggler), alpha = 0.35, linewidth = 1) +
  
  # Draw strictly parallel passing segments for the active beat
  geom_segment(data = pass_data, aes(x = x, y = y, xend = xend, yend = yend), color = "firebrick", linewidth = 1.2, alpha = 0.9) +
  
  # Draw juggler nodes
  geom_point(data = sim_data, aes(x = X, y = Y, color = Juggler), size = 5) +
  geom_text(data = sim_data, aes(x = X, y = Y, label = Juggler), color = "white", size = 2.5, fontface = "bold") +
  
  coord_fixed(xlim = c(-7, 7), ylim = c(-7, 7)) +
  theme_minimal(base_size = 14) +
  labs(
    title = "True Feast Pattern: Geometric Parallels & Orbits",
    subtitle = "Beat: {closest_state} (Alternating 2 and 3 Parallel Exchanges)",
    x = "X Position",
    y = "Y Position"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_states(Frame, transition_length = 1, state_length = 1)

print(p)
