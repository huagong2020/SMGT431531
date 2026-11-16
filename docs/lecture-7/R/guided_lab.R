# Stage 0. Terminal payoffs from our team's perspective.
terminal <- c(win = 1, loss = 0)

# Stage 1. Rows are final-snap calls and columns are terminal outcomes.
# The opponent's call is supplied at each of its states.
P1 <- list(
  lead = rbind(opponent_attempt = c(win = 0.90, loss = 0.10)),
  trail = rbind(opponent_kneel = c(win = 0, loss = 1)),
  first_down = rbind(
    kick_52 = c(win = 0.65, loss = 0.35),
    end_zone_pass = c(win = 0.12, loss = 0.88)
  )
)

# Stage 2. Rows are fourth-down calls and columns are stage-1 states.
P2 <- rbind(
  kick_57 = c(lead = 0.40, trail = 0.60, first_down = 0),
  go      = c(lead = 0, trail = 0.45, first_down = 0.55),
  punt    = c(lead = 0, trail = 1, first_down = 0)
)

action_values <- function(transition, next_values) {
  stopifnot(
    all(colnames(transition) %in% names(next_values)),
    all(is.finite(transition)), all(transition >= 0),
    all(abs(rowSums(transition) - 1) < 1e-10)
  )
  drop(transition %*% next_values[colnames(transition)])
}

q1 <- lapply(P1, action_values, next_values = terminal)
v1 <- vapply(q1, max, numeric(1))
q2 <- action_values(P2, v1)

print(v1)
print(round(100 * q2, 2))

# Evaluate each fixed policy by selecting the prescribed action at each stage.
policies <- expand.grid(
  first_action = rownames(P2),
  after_conversion = rownames(P1$first_down),
  stringsAsFactors = FALSE
)
policies$win_probability <- vapply(seq_len(nrow(policies)), function(i) {
  fixed_values <- v1
  fixed_values["first_down"] <- q1$first_down[policies$after_conversion[i]]
  fixed_action <- policies$first_action[i]
  action_values(P2[fixed_action, , drop = FALSE], fixed_values)[[1]]
}, numeric(1))
print(policies, row.names = FALSE)

# Change the go row and preserve its total probability of one.
P2_more_conversions <- P2
P2_more_conversions["go", c("trail", "first_down")] <- c(0.44, 0.56)
q2_more_conversions <- action_values(P2_more_conversions, v1)
print(round(100 * q2_more_conversions, 2))

# The figure in Tutorial 7.5 assumes zero win probability after failure.
plot_decision_boundary <- function() {
  conversion_rates <- seq(0.45, 0.65, length.out = 100)
  break_even_make_rate <- v1[["first_down"]] * conversion_rates / v1[["lead"]]

  plot(NA, xlim = c(0.45, 0.65), ylim = c(0.30, 0.50), xaxs = "i", yaxs = "i",
       xlab = "Fourth-down conversion probability",
       ylab = "57-yard field-goal make probability", axes = FALSE)
  polygon(c(conversion_rates, rev(conversion_rates)),
          c(break_even_make_rate, rep(0.50, length(conversion_rates))),
          col = "#e5f4fb", border = NA)
  polygon(c(conversion_rates, rev(conversion_rates)),
          c(break_even_make_rate, rep(0.30, length(conversion_rates))),
          col = "#f9efd8", border = NA)
  x_ticks <- seq(0.45, 0.65, by = 0.05)
  y_ticks <- seq(0.30, 0.50, by = 0.05)
  axis(1, at = x_ticks, labels = paste0(round(100 * x_ticks), "%"))
  axis(2, at = y_ticks, labels = paste0(round(100 * y_ticks), "%"), las = 1)
  box(col = "#9caab3")
  lines(conversion_rates, break_even_make_rate, col = "#374151", lwd = 2)
  text(0.505, 0.462, "Attempt field goal", col = "#104963", font = 2)
  text(0.600, 0.343, "Go for it", col = "#6e490d", font = 2)
  p <- P2["go", "first_down"]
  k <- P2["kick_57", "lead"]
  points(p, k, pch = 21, bg = "#19688c", col = "white", lwd = 1.6, cex = 1.4)
  segments(p, k + 0.006, p, k + 0.021, col = "#19688c")
  text(p, k + 0.024, "Worked example", pos = 3, col = "#104963", cex = 0.85)
}
