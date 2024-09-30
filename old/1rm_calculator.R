
weight <- 250
reps   <- 10

one_rep_max <- weight / (1.0278 - (0.0278 * reps))


m_distance          <- 42195                              # Marathon distance
n_rowers            <- 4                                  # Number of rowers
individual_work     <- m_distance / n_rowers              # Meters per rower
rep_distance        <- 500                                # Estimated meters rep
total_reps          <- m_distance / rep_distance          # Total team reps
individual_reps     <- individual_work / rep_distance     # Total individual reps
rep_time            <- 115                                # 1:55 sec rep time
individual_row_time <- (individual_reps * rep_time) / 60  # Row time per rower
total_row_time      <- (total_reps * rep_time) / 60       # Row time total

# If each person rows at a 1:55 pace we will finish in 161.75 minutes.
# Each person will row for 40.44 minutes.

final_time                 <- ((60 * 2) + 31.36) * 60
actual_individual_row_time <- (final_time / 4) / 60
