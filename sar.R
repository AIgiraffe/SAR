library(png)
library(ggplot2)
#library(countcolors)
#library(colordistance)
#norway_raw <- loadImage(path = "/home/jason/Workspace/sar_home_work.png", lower = NULL, upper = NULL)
#plotPixels(norway_raw, lower = NULL, upper = NULL, n = 5000)
img <- readPNG("../sar_home_work1.png",native =T)
img <-abs(img)/(max(abs(img)))
a <- hist(img,freq = F,breaks=seq(0,1,0.02),main="Histogram of urban forest")
a1 <- a$density
dat <-data.frame(densitys=a1,breaks=seq(0,0.98,0.02))
ggplot(dat,aes(x=breaks,y=densitys))+
      geom_bar(stat = "identity",colour="white")+
  stat_function(fun=dgamma,geom ="line",size=1,col="blue",args=list(shape=1,scale=1))+
  stat_function(fun=dgamma,geom ="line",size=1,col="red",args=list(shape=4,scale=1/10))+
  stat_function(fun=dgamma,geom ="line",size=1,col="yellow",args=list(shape=3,scale=1/8))

ggplot(data=data.frame(x=c(0, 7)), aes(x=x)) +
  stat_function(fun=dexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=dexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=dexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Densities")

ggplot(data=data.frame(x=seq(0, 7, length.out = 500)), aes(x=x)) +
  stat_function(fun=pexp, geom = "line", size=2, col="black", args = (mean=1)) +
  stat_function(fun=pexp, geom = "line", size=2, col="red", args = (mean=.5)) +
  stat_function(fun=pexp, geom = "line", size=2, col="blue", args = (mean=2)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Exponential Cumulative Distribution Functions")

ggplot(data=data.frame(x=seq(10^-3, 5, length.out = 500)), aes(x=x)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=dgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Densities")

ggplot(data=data.frame(x=seq(10^-3, 5, length.out = 500)), aes(x=x)) +
  stat_function(fun=pgamma, geom = "line", size=2, col="black", args = list(shape=1, scale=1)) +
  stat_function(fun=pgamma, geom = "line", size=2, col="red", args = list(shape=3, scale=1/3)) +
  stat_function(fun=pgamma, geom = "line", size=2, col="blue", args = list(shape=8, scale=1/8)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("x") + ylab("Gamma Cumulative Distribution Functions")

ggplot(data=data.frame(x=seq(0.001, 1, length.out = 500)), aes(x=x)) +
  stat_function(fun=dbeta, geom = "line", size=2, col="red", args = list(shape1=2, shape2=2)) +
  stat_function(fun=dbeta, geom = "line", size=2, col="blue", args = list(shape1=8, shape2=8)) +
  stat_function(fun=dbeta, geom = "line", size=2, col="black", args = list(shape1=.7, shape2=.7)) +
  theme_classic() +
  theme(text = element_text(size=20)) +
  xlab("u") + ylab("Symmetric Beta densities")
