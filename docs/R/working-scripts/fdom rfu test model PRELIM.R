#looking at chla grant data#
chla<-read.csv("chla_2.1_lks.csv", header = TRUE)
RFU<-chla[,38]
ugL<-chla[,11]
fdom<-chla[,20]
data<-data.frame(RFU, ugL, fdom)

#tests fir assumptions of normality
qplot(data$RFU, geom = "histogram", binwidth = 0.2)
qplot(data$ugL, geom = "histogram", binwidth = 4)
qplot(data$fdom, geom = "histogram")

shapiro.test(data$RFU)
shapiro.test(data$ugL)
shapiro.test(data$fdom)

#ug/L not quite normal

cor(RFU, ugL, method =c("pearson"))
plot(RFU, ugL)
simple<-lm(ugL~RFU, data = chla)
summary(simple)
model2<-lm(RFU~ugL + fdom, data = chla) 
summary(model2)

#fdom a sign addition to linear model

#GLMS with AIC

simpleGLM<-glm(ugL~RFU, data = chla)
model2GLM<-glm(ugL~RFU + fdom, data = chla)
summary(simpleGLM)
summary(model2GLM)

#fdom is sign addition to GLM and lower AIC
#fdom relationship is negative


install.packages("ggplot2")
library(ggplot2)


plot1<- ggplot(data=data, aes(RFU, ugL,)) +
  geom_point(alpha=0.5, size=2) +
  labs(y="Chlorophyll-a (ug/L)", x="Sensor RFU")
  
plot1

plot2<- ggplot(data=data, aes(RFU, ugL, fill=fdom)) +
  geom_point(alpha=0.5, size=3, shape=21) +
  labs(y="Chlorophyll-a (ug/L)", x="Sensor RFU")

plot2

plot3<- plot2+scale_fill_gradient(low="white", high = "brown")
plot3

