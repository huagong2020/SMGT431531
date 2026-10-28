# A statistical figure for review; no lecture sources are modified.
out <- "C:/Users/gongh/Desktop/SGMT431/tmp/gmm-visual-preview"
colors <- c("#22749B", "#B46537")
mu <- list(c(-1, 0), c(1.4, 0.65))
Sigma <- list(matrix(c(1, .45, .45, .7), 2),
              matrix(c(.75, -.35, -.35, 1.2), 2))
pi <- c(.6, .4)
xi <- c(.15, .6)
normal_density <- function(x, center, covariance) {
  delta <- x - center
  as.numeric(exp(-.5 * t(delta) %*% solve(covariance) %*% delta) /
               (2 * base::pi * sqrt(det(covariance))))
}
phi <- vapply(1:2, function(k) normal_density(xi, mu[[k]], Sigma[[k]]), numeric(1))
contribution <- pi * phi
mixture <- sum(contribution)
responsibility <- contribution / mixture
stopifnot(abs(sum(responsibility) - 1) < 1e-12)

draw_figure <- function() {
  layout(matrix(c(1, 2, 3, 4, 4, 4), 2, 3, byrow = TRUE),
         widths = c(1.28, 1, 1), heights = c(4.3, 1.2))
  par(family = "sans", fg = "#222222", col.axis = "#333333",
      col.lab = "#222222", cex = 1, las = 1,
      oma = c(.4, .4, 2.7, .4), mgp = c(2.1, .65, 0), tcl = -.25)

  par(mar = c(5.7, 3.8, 3.5, 1))
  plot(NA, xlim = c(-3.5, 3.9), ylim = c(-2.5, 3.1), asp = 1,
       xlab = "Feature 1", ylab = "Feature 2", bty = "l", cex.axis = .85)
  title("(a) Two Gaussian components", adj = 0, cex.main = 1.12, font.main = 1, line = 2)
  angles <- seq(0, 2 * base::pi, length.out = 300)
  circle <- cbind(cos(angles), sin(angles))
  for (k in 1:2) {
    for (radius in c(1, 2)) {
      ellipse <- sweep(radius * circle %*% chol(Sigma[[k]]), 2, mu[[k]], "+")
      lines(ellipse, col = colors[k], lwd = if (radius == 1) 1.8 else 1.2,
            lty = if (radius == 1) 1 else 2)
    }
    points(mu[[k]][1], mu[[k]][2], pch = 3, cex = 1.1, lwd = 1.7, col = colors[k])
    text(mu[[k]][1] - .08, mu[[k]][2] - .32,
         labels = if (k == 1) expression(mu[1]) else expression(mu[2]),
         col = colors[k], cex = 1.05)
  }
  points(xi[1], xi[2], pch = 19, cex = .95)
  text(xi[1] + .22, xi[2] + .2, expression(x[i]), adj = 0, cex = 1.05)
  legend("topleft", legend = c(expression(pi[1] == 0.60), expression(pi[2] == 0.40)),
         col = colors, lty = 1, lwd = 1.6, bty = "n", cex = .9)
  mtext(expression(mu[k]*": center     "*Sigma[k]*": spread and direction"),
        side = 1, line = 4.4, cex = .78)

  par(mar = c(5.7, 4.3, 3.5, 1))
  bp <- barplot(contribution, names.arg = c("Component 1", "Component 2"),
                col = colors, border = NA, ylim = c(0, max(contribution) * 1.55),
                ylab = "Weighted density", cex.names = .85, cex.axis = .85,
                yaxs = "i", width = .65, space = .7)
  title(expression("(b) Contributions at "*x[i]), adj = 0, cex.main = 1.12,
        font.main = 1, line = 2)
  text(bp, contribution, sprintf("%.4f", contribution), pos = 3, cex = .95)
  mtext(expression(pi[k]*phi(x[i]*" | "*mu[k]*", "*Sigma[k])),
        side = 1, line = 2.4, cex = .92)
  mtext(sprintf("%.2f x %.4f = %.4f", pi[1], phi[1], contribution[1]),
        side = 1, line = 4, cex = .75, col = colors[1])
  mtext(sprintf("%.2f x %.4f = %.4f", pi[2], phi[2], contribution[2]),
        side = 1, line = 5, cex = .75, col = colors[2])

  par(mar = c(5.7, 4, 3.5, 1.2))
  bp <- barplot(responsibility, names.arg = c("Component 1", "Component 2"),
                col = colors, border = NA, ylim = c(0, 1),
                ylab = "Membership probability", cex.names = .85, cex.axis = .85,
                yaxs = "i", width = .65, space = .7)
  title(expression("(c) Memberships at "*x[i]), adj = 0, cex.main = 1.12,
        font.main = 1, line = 2)
  text(bp, responsibility, sprintf("%.1f%%", 100 * responsibility), pos = 3, cex = .95)
  mtext(expression(r[ik] == frac(pi[k]*phi(x[i]*" | "*mu[k]*", "*Sigma[k]), p(x[i]))),
        side = 1, line = 3, cex = .9)
  mtext("Divide each contribution by their total.", side = 1, line = 5, cex = .75)

  par(mar = c(.3, .7, .3, .7))
  plot.new()
  text(.02, .8, bquote(p(x[i]) == .(sprintf("%.4f + %.4f = %.4f",
                        contribution[1], contribution[2], mixture))),
       adj = 0, cex = 1.15)
  text(.02, .32, expression(paste("\u2113", "(", theta, ")") == sum(log(p(x[i])), i == 1, n)),
       adj = 0, cex = 1.12)
  text(.43, .32, "For all players, add one log-density per observation.", adj = 0, cex = .9)
  mtext("Gaussian mixture model: one observation, two components", outer = TRUE,
        side = 3, line = .65, adj = 0, cex = 1.3, font = 1)
}

png(file.path(out, "gaussian-mixture-notation.png"), width = 2100, height = 1200,
    res = 170, type = "cairo")
draw_figure()
dev.off()
write.csv(data.frame(component = 1:2, weight = pi, density = phi,
                     weighted_density = contribution, responsibility),
          file.path(out, "calculated-values.csv"), row.names = FALSE)
cat("Saved statistical figure to", file.path(out, "gaussian-mixture-notation.png"), "\n")
