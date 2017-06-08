## Regression Course Project


## executive summary


# exploratory data analyses
# Collect data, and make factor variables
data<-mtcars
str(data)
data$cyl  <- factor(data$cyl)
data$vs   <- factor(data$vs)
data$gear <- factor(data$gear)
data$carb <- factor(data$carb)
data$am   <- factor(data$am,labels=c("Automatic","Manual"))

g = ggplot(data, aes(x = am, y = mpg, colour = wt))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("Transmission") + ylab("Miles/(US) gallon")
g
# Looks like there is a difference in the avg mpg between automatic and manual 
# transmission, however there are a number of cofounding variables. In this plot we see how 
# weight can have an effect.

## fitting multiple models and model selection strategy
# first we fit all the variables as regressors
fitAll<-lm(mpg~.,data = data)
# finding the best model
fitBest <- step(fitAll, direction = "both")
round(summary(fitBest)$coeff,3)
par(mfrow=c(2,2))
plot(fitBest)
# we see no clear patterns in the residual plots
# errors are gaussian



#“Is an automatic or manual transmission better for MPG”
#"Quantify the MPG difference between automatic and manual transmissions"

## looking at the coefficient for manual transmission, we see that changing from automatic
# to manual increases the mpg on average by 1.8 (+/- ...), keeping all other variables 
# constant....looking at the p-value this is not significant...include interaction term...

library("car")
sqrt(vif(fit))



##

