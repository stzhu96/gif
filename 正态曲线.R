library(ggplot2)
library(patchwork)  # 加载patchwork包

# 创建函数用于添加X轴黑线并突出到刻度
add_xaxis_line <- function(plot) {
  plot +
    geom_segment(aes(x = 2, xend = 18, y = 0, yend = 0), color = "black", size = 1) +
    geom_segment(aes(x = seq(2, 18, by = 2), xend = seq(2, 18, by = 2), y = 0, yend = 0.1), color = "black", size = 1) +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.1)),
      breaks = seq(2, 18, by = 2),
      limits = c(2, 18),
      name = "X轴",
      guide = guide_axis(n.dodge = 2),
      labels = c("2", "4", "6", "8", "10", "12", "14", "16", "18")
    )
}

# 创建第一张图
plot1 <- ggplot() +
  geom_blank() +
  labs(x = NULL, y = NULL, title = NULL) +
  add_xaxis_line() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))

# 创建第二张图
set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p = 1 - alpha/2, df = length(x) - 1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96 * error, ucl = sample.means + 1.96 * error, trial = 1:n.reps)

plot2 <- ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl), color = "green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  labs(x = NULL, y = NULL, title = NULL) +
  add_xaxis_line() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

# 上下合并两张图，使用相同的坐标系和背景网格
combined_plot <- plot1 + plot2 + plot_layout(guides = "collect")

# 显示合并后的图
combined_plot
