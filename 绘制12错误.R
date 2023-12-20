# 设置随机种子以确保可复现性
set.seed(123)

# 生成数据
x <- seq(-5, 5, length.out = 1000)
mean1 <- 2
mean2 <- 0
sd1 <- 1
sd2 <- 1

curve1 <- dnorm(x, mean = mean1, sd = sd1)
curve2 <- dnorm(x, mean = mean2, sd = sd2)

# 绘制两条正太曲线，去掉横纵坐标和框
plot(x, curve1, type = "n", xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

# 绘制曲线
lines(x, curve1, col = "black", lty = 1, lwd = 2)
lines(x, curve2, col = "black", lty = 1, lwd = 2)

# 添加阈值线
threshold <-(-0.4)

# 计算并绘制第一类错误的区域
x_values <- seq(-5, threshold, length.out = 1000)
y_values_curve1 <- dnorm(x_values, mean = mean1, sd = sd1)
polygon(c(x_values, rev(x_values)), c(y_values_curve1, rep(0, length(x_values))), col = rgb(0, 0, 1, 0.3), border = NA)

# 计算并绘制第二类错误的区域
x_values <- seq(threshold, 5, length.out = 1000)
y_values_curve2 <- dnorm(x_values, mean = mean2, sd = sd2)
polygon(c(x_values, rev(x_values)), c(y_values_curve2, rep(0, length(x_values))), col = rgb(1, 0, 0, 0.3), border = NA)

# 在阈值处绘制一条垂直虚线
abline(v = threshold, col = "green", lty = 2, lwd = 2)
