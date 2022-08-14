X <- c(90,86,67,89,81,75)
Y <- c(62,45,40,55,64,53)


#to draw a scatter plot
plot(X,Y,pch=21,col="blue",bg="red")
cor(X,Y)

#to perform a correlation test
cor.test(X,Y)

#assuming and plotting a simple linear model
HeartRateModel <- lm(Y~X)
summary(HeartRateModel)

#plotting the regression line
abline(HeartRateModel,col="red")

#validating the assumptions using graphs
plot(HeartRateModel)

#used to check whether the data points are normally distributed.
shapiro.test(Y)

#constructing the annova table for the model
anova(HeartRateModel)

temperature <- c(53.2,53.2,53.2,53.3,60.0,60.0,70.3,70.3,70.3,70.3,76.9,78.4,88.1,88.1,89.6,89.6,89.6)
wear <- c(2.25,2.19,1.99,2.27,2.68,2.63,2.35,2.42,2.34,2.41,2.22,2.08,2.23,2.07,2.01,2.26,2.01)

plot(temperature,wear,pch=21,col="blue",bg="red")
cor(temperature,wear)
cor.test(temperature,wear)

TireWearModel <- lm(wear~temperature)
summary(TireWearModel)
shapiro.test(wear)
anova(TireWearModel)

X1 <- c(73,93,89,96,73,53,79,69,70,93,79,70,81,88,78,82,86,78)
X2 <- c(80,88,91,98,66,46,70,70,65,95,80,73,90,92,83,86,82,83)
X3 <- c(75,93,90,100,70,55,88,73,74,91,73,78,93,86,77,90,89,85)
Y <-c(152,185,180,196,142,101,164,141,141,184,152,148,183,177,159,177,175,175)
SLR1 <- lm(Y~X1+X2+X3)
summary(SLR1)
anova(SLR1)

X1 <- c(79,69,70,93,79,70)
X2 <- c(70,70,65,95,80,73)
X3 <- c(88,73,74,91,73,78)
Y <-c(164,141,141,184,152,148)
SLR2 <- lm(Y~X1+X2+X3)
summary(SLR2)
anova(SLR2)

X1 <- c(81,88,78,82,86,78)
X2 <- c(90,92,83,86,82,83)
X3 <- c(93,86,77,90,89,85)
Y <-c(183,177,159,177,175,175)
SLR3 <- lm(Y~X1+X2+X3)
summary(SLR3)
