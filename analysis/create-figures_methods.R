##-----------------------------------------------------------------------------#
#' Generate figures for methods section of paper:
##-----------------------------------------------------------------------------#

library(tidyverse)
library(here)
library(ggbeeswarm)
library(glue)
library(patchwork)
library(RColorBrewer)
library(latex2exp)
# devtools::install_github("AckerDWM/gg3D")
library(gg3D)

fig_dir <- "fig"

## related-work data replication ----------------------------------------------#

make_data <- function(n, seed) {
  set.seed(seed)
  x <- runif(n, 0, 1)
  y <- runif(n, 0, 1)
  class <- rep(3, n)
  class[which(x+y < 1.1)] <- 2
  class[which(x+y < 0.76)] <- 1
  
  omit <- abs(x - y) > 0.6
  
  d <- tibble(x1 = x, x2 = y, y = class)
  d <- d[!omit, ]
  d <- expand_grid(d, h = c(0, 1))
  d <- d %>% 
    mutate(yb = 1*((y - h >= 2)),
           yb = 2*yb - 1, 
           across(c(y, yb), as.factor))
  
}

plot_data <- make_data(n = 30, seed = 10)

# x + y - 0.5 * z = 0.75
z <- function(x, y) (0.75 - x - y) / (-0.5)
frame_data <- 
  expand_grid(x1 = seq(0, 1, by = 0.1), 
              x2 = seq(0, 1, by = 0.1)) %>% 
  mutate(h = z(x1, x2)) %>% 
  filter(h >= 0, h <= 1)


my_scales <- list(
  scale_shape_manual(values = 0:2), 
  scale_color_brewer(palette = "Set1")
)

my_scales2 <- list(
  scale_color_manual(name = "Replicated y", 
                     values = brewer.pal(5, "Set1")[4:5]),
  scale_shape_manual(name = "Replicated y", 
                     values = 4:3)
)

theta <- 44
phi <- 0
my_3d_axes <- list(
  axes_3D(theta = theta, phi = phi),
  labs_3D(theta = theta, phi = phi,
          hjust = c(-0.2,-0.2,0.5), vjust = c(0.1,0.1,-0.7),
          labs = c(TeX("$x^{(1)}$"), TeX("$x^{(2)}$"), TeX("$h$"))),
  theme_void()
)


p8.1 <- ggplot(plot_data, aes(x1, x2, shape = y, color = y)) + 
  geom_abline(slope = -1, intercept = c(0.76, 1.13),
              size = 1, color = "grey70") + 
  geom_point() +
  expand_limits(x = c(0,1), y = c(0, 1)) + 
  my_scales + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  coord_fixed() +
  labs(x = TeX("$x^{(1)}$"), y = TeX("$x^{(2)}$"),
       title = TeX("A. Initial $x_i$ and $y_i$"))

p8.2 <- ggplot(plot_data, aes(x=x1, y=x2, z=h)) + 
  my_3d_axes + 
  stat_3D(aes(shape = y, color = y), theta = theta, phi = phi) +
  stat_wireframe(color = "grey70",
                 theta = theta, phi = phi, 
                 data = frame_data) + 
  my_scales + 
  theme(legend.position = "bottom") +
  labs(title = TeX("B. Replicated $x_i$, initial $y_i$ (with $s = 2$)"))

p8.3 <- ggplot(plot_data, aes(x1, x2, z = h)) +
  my_3d_axes + 
  stat_3D(aes(color = yb, shape = yb), theta = theta, phi = phi) +
  stat_wireframe(color = "grey70",
                 theta = theta, phi = phi, 
                 data = frame_data) + 
  my_scales2 + 
  theme(legend.position = "bottom") +
  labs(title = TeX("C. Replicated $x_i$ and $y_i$"))

p8.4 <- plot_data %>% 
  filter(!(y == 1 & h == 1) & !(y == 3 & h == 0)) %>% 
  ggplot(aes(x=x1, y=x2, z=h)) + 
  my_3d_axes + 
  stat_3D(aes(shape = y, color = y), theta = theta, phi = phi) +
  stat_wireframe(color = "grey70",
                 theta = theta, phi = phi, 
                 data = frame_data) + 
  my_scales + 
  theme(legend.position = "bottom") +
  labs(title = TeX("D. Replicated $x_i$, initial $y_i$ (with $s = 1$)"))

p8 <-
  (p8.1 + p8.2 + p8.3 + p8.4) +  
  plot_layout(guides = "collect", widths = 1) &
  theme(legend.position = "bottom", plot.title.position = "plot")

p8[[1]] <- p8[[1]] + theme(legend.position = "none") # stupid fix
p8

ggsave(here(fig_dir, "related-work_data-replication.pdf"), p8, width = 8, height = 6, dpi = 800)


## OMISVM intuition -----------------------------------------------------------#

make_data2 <- function(n, seed) {
  set.seed(seed)
  x <- runif(n, 0, 3)
  bag <- sample(rep(seq_len(n/10), 10))
  class <- rep(3, n)
  class[which(x < 2)] <- 2
  class[which(x < 1)] <- 1
  
  d <- tibble(x1 = x, b = bag, y = class)
  d <- expand_grid(d, h = c(0, 1))
}

remove_close_points <- function(data, thresh = 0.25) {
  continue <- TRUE
  while (continue) {
    removed <- FALSE
    d <- as.matrix(dist(data[, 1:2]))
    
    i <- 1
    while (!removed & i <= nrow(d)) {
      row <- d[i, ]
      
      to_remove <- which(row > 0 & row < thresh)
      if (length(to_remove) > 0) {
        data <- data[-to_remove[1], ]
        removed <- TRUE 
      } 
      i <- i + 1
    }
    
    if (!removed) {
      continue <- FALSE
    }
  }
  return(data)
}

plot_data <- make_data2(n = 50, seed = 10)
plot_data <- remove_close_points(plot_data, thresh = 0.2)
plot_data <- plot_data %>% group_by(b) %>% slice_head(n = 8) %>% ungroup() 

# misc edits to get the points to match what I need 
plot_data[3:4, "y"] <- 3 # bag 1
plot_data[13:14, "y"] <- 1 # bag 2
plot_data[13:14, "x1"] <- 1.10 # bag 2
plot_data[9:10, "x1"] <- 0.396 # bag 2
plot_data[19:20, "x1"] <- 0.80 # bag 3 
plot_data[21:22, "x1"] <- 1.80 # bag 3
plot_data[31:32, "y"] <- 3 # bag 4
plot_data[31:32, "x1"] <- 1.08 # bag 4
plot_data[25:26, "x1"] <- 2.62 # bag 4
plot_data[25:26, "y"] <- 3 # bag 4
plot_data[35:36, "y"] <- 2 # bag 5
plot_data[37:38, "y"] <- 1 # bag 5
plot_data[37:38, "x1"] <- 0.966 # bag 5

# re-order for better flow in text
plot_data <- plot_data %>% 
  mutate(b2 = case_when(
    b == 5 ~ 1, 
    b == 1 ~ 2,
    b == 3 ~ 3,
    b == 4 ~ 4,
    b == 2 ~ 5
  ))

plot_data <- plot_data %>% 
  group_by(b) %>% 
  mutate(
    rep = x1 == max(x1),
    y_bag = max(y)
  ) %>% 
  ungroup()

plot_data <- plot_data %>% 
  mutate(yb = 1*((y - h >= 2)),
         yb = 2*yb - 1, 
         across(c(y, yb), as.factor))



# Frame data 
frame_data <- 
  expand_grid(x1 = seq(0, 3, by = 0.1), 
              b2 = seq(0, 5, by = 0.7)) %>% 
  mutate(h = x1 - 1) %>% 
  filter(h >= 0, h <= 1)

frame_line <- function(x1, x2, h) {
  expand_grid(x1 = x1, b2 = x2, h = h) %>% 
    add_dummy()
}

add_dummy <- function(data) {
  data %>% 
    bind_rows(c(x1 = range(plot_data$x1)[1], b2 = 0.03, h = 0)) %>% 
    bind_rows(c(x1 = range(plot_data$x1)[2], b2 = 0.07, h = 1))
}

my_scales <- list(
  scale_shape_manual(values = c(22, 21, 24)), 
  scale_color_brewer(palette = "Set1")
)

p9.1 <-
  ggplot(plot_data, aes(x1, 5.5-b2, shape = y, color = y)) + 
  geom_vline(xintercept = c(1, 2),
             size = 1, color = "grey70") + 
  geom_vline(xintercept = as.numeric(outer(c(1, 2), 0.2*c(-1, 1), `+`)), 
             color = "grey80") + 
  geom_point(size = 7) +
  geom_point(size = 7, stroke = 1.5, data = plot_data %>% filter(rep)) +
  geom_text(aes(label = b2), color = "black") + 
  scale_x_continuous(
    breaks = as.numeric(outer(0.2*c(-1, 0, 1), c(1, 2), `+`)), 
    labels = TeX(paste0("-", c("b_1 - 1", "b_1", "b_1 + 1", "b_2 - 1", "b_2", "b_2 + 1"))) 
  ) + 
  expand_limits(x = c(0,3), y = c(0, 5)) + 
  my_scales + 
  theme_classic() + 
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank()) +
  # coord_fixed() +
  labs(
    x = expression(f(x) == group(langle, list(w, x), rangle)),
    y = NULL,
    title = "A. Initial data and constraints"
  )

# Add segments
my_arrow <- function(x, x2, y, label, vjust = -0.2, ...) {
  std_ar <- arrow(length = unit(1, "mm"), ends = "both")
  list(
    annotate(x = x, y = y, xend = x2, yend = y, 
             geom = "segment", ..., arrow = std_ar),
    annotate(x = mean(c(x, x2)), y = y,
             geom = "text", label = label, vjust = vjust)  
  )
}
my_segment <- function(x, x2, y, label, vjust = -0.2) {
  list(
    annotate(x = x, y = y, xend = x2, yend = y, 
             geom = "segment", linetype = "dotted")
  )
}
x_eps <- 0.055

p9.1 <-
  p9.1 + 
    my_arrow(0.8, 0.966-x_eps, 5.5-1, TeX("$\\xi_1^1$")) +
    my_arrow(1.8, 2.08-x_eps, 5.5-1, TeX("$\\xi_1^2$")) +
    my_segment(1.2, 1.07+x_eps, 5.5-2, "") +
    my_arrow(2.2, 1.70+x_eps, 5.5-2, TeX("$\\xi_2^{*2}$")) + 
    my_segment(1.2, 1.08+x_eps/2, 5.5-3.85, "") +
    my_segment(2.2, 1.08+x_eps, 5.5-4, "") +
    my_arrow(0.8, 1.1-x_eps, 5.5-5, TeX("$\\xi_5^1$"))
    
  
# plot_data %>% mutate(row = row_number()) %>% arrange(b2, x1) %>% print(n = Inf)

theta <- -5
phi <- 0
my_3d_axes <- list(
  axes_3D(theta = theta, phi = phi),
  labs_3D(theta = theta, phi = phi,
          hjust = c(1,-0.2,2), vjust = c(1.1,0.1,1),
          labs = c(expression(f(x) == group(langle, list(w, x), rangle)), "", TeX("$h$"))),
  theme_void()
)

line_frame <- function(x1, h, theta, phi, ...) {
  x2 <- seq(0, 1, by = 0.1)
  list(stat_wireframe(
    color = "grey70",
    theta = theta, phi = phi, 
    data = frame_line(x1, x2, h), 
    ...
  ))
}

p9.2 <-
  ggplot(plot_data, aes(x=x1, y=5.5-b2, z=h)) + 
  stat_3D(aes(shape = y, color = y), theta = theta, phi = phi,
          size = 5) +
  stat_3D(aes(label = b2), theta = theta, phi = phi,
          geom = "text") +
  stat_wireframe(color = "grey70",
                 theta = theta, phi = phi, 
                 data = add_dummy(frame_data)) + 
  line_frame(x1 = 2, h = 0, theta, phi, size = 1) + 
  line_frame(x1 = 1, h = 1, theta, phi, size = 1) + 
  line_frame(x1 = 0.8, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.2, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.8, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 2.2, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 0.8, h = 0, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.2, h = 0, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.8, h = 0, theta, phi, size = 0.3) + 
  line_frame(x1 = 2.2, h = 0, theta, phi, size = 0.3) + 
  my_3d_axes +
  my_scales + 
  theme(legend.position = "bottom")


my_scales2 <- list(
  scale_color_manual(name = "Replicated y", 
                     values = brewer.pal(5, "Set1")[4:5]),
  scale_shape_manual(name = "Replicated y", 
                     values = 4:3)
)

p9.3 <-
  ggplot(plot_data, aes(x1, 5.5-b2, z = h)) +
  stat_3D(aes(color = yb, shape = yb), theta = theta, phi = phi,
          size = 3) +
  stat_3D(aes(label = b2), theta = theta, phi = phi,
          geom = "text", vjust = 0, hjust = -1) +
  stat_wireframe(color = "grey70",
                 theta = theta, phi = phi, 
                 data = add_dummy(frame_data)) + 
  line_frame(x1 = 2, h = 0, theta, phi, size = 1) + 
  line_frame(x1 = 2, h = 1, theta, phi, size = 1) + 
  line_frame(x1 = 1, h = 0, theta, phi, size = 1) + 
  line_frame(x1 = 1, h = 1, theta, phi, size = 1) + 
  line_frame(x1 = 0.8, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.2, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.8, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 2.2, h = 1, theta, phi, size = 0.3) + 
  line_frame(x1 = 0.8, h = 0, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.2, h = 0, theta, phi, size = 0.3) + 
  line_frame(x1 = 1.8, h = 0, theta, phi, size = 0.3) + 
  line_frame(x1 = 2.2, h = 0, theta, phi, size = 0.3) + 
  my_3d_axes + 
  my_scales2 + 
  theme(legend.position = "bottom") +
  labs(title = "B. Replicated data (with s = 2)")

p9.4 <- 
  p9.3 %+% (
    plot_data %>% filter(!(y_bag == 1 & h == 1) & !(y_bag == 3 & h == 0)) 
  ) + 
  labs(title = "C. Replicated data (with s = 1)")


p9 <-
  (p9.1 + inset_element(p9.2, 0, 0, 0, 0) + p9.3 + p9.4) +  
  plot_layout(guides = "collect", heights = 1, nrow = 3) &
  theme(legend.position = "bottom", plot.title.position = "plot")

p9[[1]] <- p9[[1]] + theme(legend.position = "none") # stupid fix
p9

ggsave(here(fig_dir, "methods_data-replication-or.pdf"), p9, width = 8, height = 10, dpi = 800)
