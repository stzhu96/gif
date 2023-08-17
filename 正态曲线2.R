library(ggplot2)

set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p=1-alpha/2, df=length(x)-1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96*error, ucl = sample.means +1.96*error, trial = 1:n.reps)


ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl),color="green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(NULL) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

library(ggplot2)

set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p=1-alpha/2, df=length(x)-1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96 * error, ucl = sample.means + 1.96 * error, trial = 1:n.reps)

ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl)) +
  geom_errorbarh(color = "green") +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  xlab("Sample Mean") +  # 添加横坐标标签
  ylab(NULL) +
  ggtitle(NULL) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90", linetype = "dashed"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),  # 旋转横坐标标签
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())




library(ggplot2)

my.mean <- 10
my.sd <- 2
alpha <- 0.05

# 生成一些数据点
x <- seq(my.mean - 3*my.sd, my.mean + 3*my.sd, length.out = 100)
y <- dnorm(x, mean = my.mean, sd = my.sd)

# 计算置信区间上下限
z <- qnorm(1 - alpha/2)
lower_limit <- my.mean - z * my.sd
upper_limit <- my.mean + z * my.sd

# 创建数据框
data <- data.frame(x = x, y = y)

# 绘制正态曲线、置信区间和均值竖线
ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "green", size = 1) +
  geom_vline(xintercept = c(lower_limit, upper_limit), linetype = "dashed", color = "red") +
  geom_vline(xintercept = my.mean, linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, ) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())



set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p=1-alpha/2, df=length(x)-1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96*error, ucl = sample.means +1.96*error, trial = 1:n.reps)


ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl),color="green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(NULL) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())



library(ggplot2)
library(patchwork)  # 加载patchwork包

# 第一张图
plot1 <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "green", size = 1) +
  geom_vline(xintercept = c(lower_limit, upper_limit), linetype = "dashed", color = "red") +
  geom_vline(xintercept = my.mean, linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

# 第二张图
plot2 <- ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl), color = "green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(NULL) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# 合并两张图
combined_plot <- plot1 / plot2

# 显示合并后的图
combined_plot


library(ggplot2)

my.mean <- 10
my.sd <- 2
alpha <- 0.05

# 生成一些数据点
x <- seq(my.mean - 3*my.sd, my.mean + 3*my.sd, length.out = 100)
y <- dnorm(x, mean = my.mean, sd = my.sd)

# 计算置信区间上下限
z <- qnorm(1 - alpha/2)
lower_limit <- my.mean - z * my.sd
upper_limit <- my.mean + z * my.sd

# 创建数据框
data <- data.frame(x = x, y = y)

# 绘制第一张图，正态曲线、置信区间和均值竖线
plot1 <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "green", size = 1) +
  geom_vline(xintercept = c(lower_limit, upper_limit), linetype = "dashed", color = "red") +
  geom_vline(xintercept = my.mean, linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p=1-alpha/2, df=length(x)-1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96*error, ucl = sample.means +1.96*error, trial = 1:n.reps)

# 绘制第二张图，样本均值的误差棒和均值竖线
plot2 <- ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl), color = "green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(NULL) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# 上下合并两张图，使用相同的坐标系
combined_plot <- plot1 + plot2 + plot_layout(guides = "collect")

# 显示合并后的图
combined_plot
library(ggplot2)

my.mean <- 10
my.sd <- 2
alpha <- 0.05

# 生成一些数据点
x <- seq(my.mean - 3*my.sd, my.mean + 3*my.sd, length.out = 100)
y <- dnorm(x, mean = my.mean, sd = my.sd)

# 计算置信区间上下限
z <- qnorm(1 - alpha/2)
lower_limit <- my.mean - z * my.sd
upper_limit <- my.mean + z * my.sd

# 创建数据框
data <- data.frame(x = x, y = y)

# 绘制第一张图，正态曲线、置信区间和均值竖线
plot1 <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "green", size = 1) +
  geom_vline(xintercept = c(lower_limit, upper_limit), linetype = "dashed", color = "red") +
  geom_vline(xintercept = my.mean, linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),  # 去掉背景主要网格线
        panel.grid.minor = element_blank(),  # 去掉背景次要网格线
        axis.text = element_blank(),
        axis.title = element_blank())

set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p=1-alpha/2, df=length(x)-1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96*error, ucl = sample.means +1.96*error, trial = 1:n.reps)

# 绘制第二张图，样本均值的误差棒和均值竖线
plot2 <- ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl), color = "green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle(NULL) +
  theme_minimal() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),  # 去掉背景主要网格线
        panel.grid.minor = element_blank(),  # 去掉背景次要网格线
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())

# 上下合并两张图，使用相同的坐标系
combined_plot <- plot1 + plot2 + plot_layout(guides = "collect")

# 显示合并后的图
combined_plot



library(ggplot2)
library(patchwork)  # 加载patchwork包

# 共享的主题设置，包括背景网格
shared_theme <- theme_minimal() +
  theme(panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90", linetype = "dashed"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16))

my.mean <- 10
my.sd <- 2
alpha <- 0.05

# 生成一些数据点
x <- seq(my.mean - 3*my.sd, my.mean + 3*my.sd, length.out = 100)
y <- dnorm(x, mean = my.mean, sd = my.sd)

# 计算置信区间上下限
z <- qnorm(1 - alpha/2)
lower_limit <- my.mean - z * my.sd
upper_limit <- my.mean + z * my.sd

# 创建数据框
data <- data.frame(x = x, y = y)

# 绘制第一张图，正态曲线、置信区间和均值竖线
plot1 <- ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "green", size = 1) +
  geom_vline(xintercept = c(lower_limit, upper_limit), linetype = "dashed", color = "red") +
  geom_vline(xintercept = my.mean, linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, title = NULL) +
  shared_theme

set.seed(1321)
n <- 10
sd <- 2
n.reps <- 100
my.mean <- 10
alpha <- 0.05

mydata <- matrix(rnorm(n = n.reps * n, mean = my.mean, sd = sd), ncol = n.reps)

sample.means <- apply(mydata, 2, mean)

error <- apply(mydata, 2, function(x) qt(p=1-alpha/2, df=length(x)-1) * sd(x) / sqrt(length(x)))

dfx <- data.frame(sample.means, error, lcl = sample.means - 1.96*error, ucl = sample.means +1.96*error, trial = 1:n.reps)

# 绘制第二张图，样本均值的误差棒和均值竖线
plot2 <- ggplot(dfx, aes(x = sample.means, y = trial, xmin = lcl, xmax = ucl), color = "green") +
  geom_errorbarh() +
  geom_point(pch = 1) +
  geom_vline(aes(xintercept = my.mean), lty = 2) +
  geom_vline(xintercept = c(my.mean - z * sd, my.mean + z * sd), lty = 2, color = "red") +
  labs(x = NULL, y = NULL, title = NULL) +
  shared_theme

# 上下合并两张图，使用相同的坐标系和背景网格
combined_plot <- plot1 + plot2 + plot_layout(guides = "collect")

# 显示合并后的图
combined_plot



