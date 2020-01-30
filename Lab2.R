EPI_data <- read.csv("2010EPI_data.csv", skip = 1)

attach(EPI_data)
EPI                    #Prints out Values EPI_dat$EPI
tf <- is.na(EPI)       #Records True for NA
E <- EPI[!tf]          #Filters out NA

plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)
help("qqnorm")
par(pty="s")
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5),x,xlab="Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI), do.points=FALSE,verticals = TRUE)
#Make Points visible
plot(ecdf(EPI_data$EPI), do.points=TRUE, verticals = TRUE)
par(pty="s")
help("qqnorm")
help("qqplot")
qqnorm(EPI_data$EPI)
#Line for QQ Plot
qqline(EPI_data$EPI)

x<-seq(30,95,1)
x
x2<-seq(30,95,1)
x2
x2<-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x,xlab = "Q-Q plot")
qqline(x)


#Exercise 1
DALY
tfd <- is.na(DALY)       #Records True for NA
D <- DALY[!tfd]  

plot(ecdf(EPI_data$DALY), do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_data$DALY), do.points=TRUE,verticals = TRUE)
par(pty="s")
qqnorm(EPI_data$DALY)
qqline(EPI_data$DALY)
help(seq)


WATER_H
tfw <- is.na(WATER_H)
W <- WATER_H[!tfw]       

plot(ecdf(EPI_data$WATER_H), do.points=FALSE,verticals = TRUE)
plot(ecdf(EPI_data$WATER_H), do.points=TRUE, verticals = TRUE)
par(pty="s")
qqnorm(EPI_data$WATER_H)
qqline(EPI_data$WATER_H)

qqplot(EPI,DALY)
qqplot(EPI,WATER_H)
qqplot(DALY,WATER_H)

boxplot(EPI_data$EPI, EPI_data$DALY)
boxplot(EPI_data$EPI, EPI_data$WATER_H)
boxplot(EPI_data$DALY, EPI_data$WATER_H)
boxplot(EPI_data$EPI, EPI_data$ENVHEALTH)
boxplot(EPI_data$EPI, EPI_data$AIR_H)
boxplot(EPI_data$EPI, EPI_data$AIR_E)
boxplot(EPI_data$EPI, EPI_data$WATER_E)

multivariate <- read.csv("multivariate.csv")
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm
head(multivariate)
help(lm)
mm # mm
summary(mm)$coef
plot(Homeowners~Immigrant)
help(abline)
abline(mm)
abline(mm,col=2,lwd=3)
abline(mm,col=3,lwd=5)
abline(mm,col=10,lwd=3)
attributes(mm)
mm$coefficients

#Creating Plots
plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data = mtcars)
ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()
plot(pressure$temperature,pressure$pressure, type = "l")
points(pressure$temperature,pressure$pressure)

lines(pressure$temperature,pressure$pressure/2, col = "red")
points(pressure$temperature,pressure$pressure/2, col = "blue")
library(ggplot2)
qplot(pressure$temperature,pressure$pressure, geom="line")
qplot(temperature,pressure,data = pressure, geom = "line")
ggplot(pressure, aes(x=temperature, y = pressure)) + geom_line() + geom_point()
ggplot(pressure, aes(x=temperature, y = pressure)) + geom_line() + geom_point()

# Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
qplot(factor(cyl), data = mtcars)
ggplot(mtcars,aes(x=factor(cyl))) + geom_bar()

#Histograms
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data = mtcars, binwidth= 4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars,aes(x=mpg)) + geom_histogram(binwidth = 5)

#BoxPlot
plot(ToothGrowth$supp, ToothGrowth$len)
boxpot(len~suppp, data = ToothGrowth)
boxplot(len~supp + dose, data = ToothGrowth)
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=supp,y=len)) + geom_boxplot()
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len,geom = "boxplot")
qplot(interaction(supp,dose), len, data = ToothGrowth, geom = "boxplot")
ggplot(ToothGrowth, aes(x=interaction(supp,dose), y=len)) + geom_boxplot()
