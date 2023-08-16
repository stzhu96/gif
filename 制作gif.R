install.packages("magick")
library(magick)
# 读取图片
image1 <- image_read("C:\\Users\\Ming.LAPTOP-SJBF2IL0\\Desktop\\动画1\\1.png")
image2 <- image_read("C:\\Users\\Ming.LAPTOP-SJBF2IL0\\Desktop\\动画1\\2.png")
image3 <- image_read("C:\\Users\\Ming.LAPTOP-SJBF2IL0\\Desktop\\动画1\\3.png")
# ... 读取更多图片

# 将图片组合成一个列表
images <- c(image1, image2,image3)  # 添加更多图片到列表

# 创建GIF动画
animation <- image_animate(image_join(images), fps = 2)

# 保存GIF动画
image_write(animation, path = "output.gif")


########################################################
