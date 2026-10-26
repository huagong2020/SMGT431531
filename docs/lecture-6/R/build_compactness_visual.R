# Rebuild the illustrative synthetic-data chart from the lecture root.
library(ggplot2)
theta <- seq(0, 2 * pi, length.out = 15)[-15]
cloud <- do.call(rbind, lapply(1:2, function(g) {
  center <- c(-1.25, 1.25)[g]
  data.frame(x = center + c(0, 0.25 * cos(theta), 0.5 * cos(theta)),
             y = c(0, 0.25 * sin(theta), 0.45 * sin(theta)), group = factor(g))
}))
cloud$panel <- "Compactness: close to a center"
t <- seq(0, pi, length.out = 33)
chains <- rbind(data.frame(x = cos(t) - 0.45, y = sin(t) - 0.25, group = factor(1)),
                data.frame(x = 0.55 - cos(t), y = -sin(t) + 0.25, group = factor(2)))
chains$panel <- "Connectedness: short steps along a chain"
centers <- data.frame(x = c(-1.25, 1.25), y = 0, group = factor(1:2), panel = cloud$panel[1])
chart <- ggplot(rbind(cloud, chains), aes(x, y, color = group)) +
  geom_path(data = chains, aes(group = group), linewidth = 0.7, alpha = 0.6) +
  geom_point(size = 2.5) +
  geom_point(data = centers, shape = 4, size = 6, stroke = 1.7, color = "#172033") +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = c("#0077a8", "#b34d19")) +
  coord_equal(xlim = c(-1.95, 1.95), ylim = c(-1.05, 1.05)) +
  labs(x = "Feature 1 (illustrative units)", y = "Feature 2") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", color = "#003b5c", size = 15),
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.spacing = grid::unit(1.5, "lines"))
ggsave("images/compactness-connectedness.png", chart, width = 12, height = 4.6, dpi = 160)
file.copy("images/compactness-connectedness.png", "slides/images/compactness-connectedness.png", overwrite = TRUE)
