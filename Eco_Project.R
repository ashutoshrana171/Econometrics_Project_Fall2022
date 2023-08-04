#Econometrics Project - Stock Analysis
#include the libraries
library("readxl")
library(memisc)
library(psych)
library(dplyr)
library(lmtest)
library(sjPlot)
library(sgof)
library(ggplot2)
library(foreign)
library(car)
library(hexbin)
library(lmtest)

#read data from excel and store in data variable
data <- read_excel("/Users/ashutosh/Documents/Econometrics/ECO_Project.xlsx")

#create scatter plots for pairs of variables
ggpairs(data)

#modifying column header for better representation
colnames(data) <-c("Date", "x1", "x2", "x3","x4","x5","x6", "x7", "y", "x8", "x9", "x10", "x11", "x12", "x13", 
                   "x14","x15","x16","x17","x18","x19","x20","x21")
glimpse(data)

#preparing multivariate model
model<-lm(data=data, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21)
summary(model)

coef(model)
model_fitted<-fitted(model) #predicted values of y (y_hat)
model_residual<-residuals(model) #difference between observed and fitted
deviance(model)


#SSE
sse <- sum((fitted(model) - data$y)^2)
sse

#SSR
ssr <- sum((fitted(model) - mean(data$y))^2)
ssr

#SST
sst <- ssr + sse
sst

#R-square
r_square <- ssr/sst


#ASSUMPTION ONE: LINEARITY OF THE DATA
#Residual vs Fitted plot would not have a pattern where the red line is approximately horizontal at zero.
plot(model,1)  #there is linear relation between the predictors and outcome variable

#ASSUMPTION TWO: PREDICTORS (X) ARE INDEPENDENT AND OBSERVED WITH NEGLIGIBLE ERROR
#durbin Watson test
#p-value > 0.05, we would fail to reject the null hypothesis.
durbinWatsonTest(model) #we reject H0, therefore the predictors are not independent

#ASSUMPTION THREE: RESIDUAL ERRORS HAVE A MEAN VALUE OF ZERO
#red line on residual vs fitted plot is flat on 0

#ASSUMPTION FOUR: RESIDUAL ERRORS HAVE CONSTANT VARIANCE
#residual points equally spread around the red line, which would indicate constant variance.
plot(model,3) #scale location plot


plot(model)

#histogram of residual errors old model
ggplot(data = data, aes(x = model$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=100) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency') +
  xlim(min(model$residuals), 1e-12)


#check for heteroscedasticity
bptest(model) #there is some heteroscedasticity present in the model

#correction of heteroscedasticity using general least square method
gls <- lm(data = data, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+x21, 
          weights = 1/model$fitted.values^2)
summary(gls)
bptest(gls)
plot(gls)

#histogram of residual errors weighted model
ggplot(data = data, aes(x = gls$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black', bins=100) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency') +
  xlim(min(gls$residuals), 1e-12)

#-------------------------------------------------------------------------------------------------------------------

#new model = model minus insignificant variables
newmodel <- lm(data = data, y~x1+x3+x4+x5+x7+x14+x17)
summary(newmodel)
plot(newmodel)
newmodel_residual <- newmodel$residuals
newmodel_fitted <- newmodel$fitted.values
bptest(newmodel)


#plotting residual plots with variables x1,x3,x4,x5,x7,14,x17
ggplot(data = data, aes(x = x1, y = y)) + 
geom_smooth(method = "lm", se = FALSE, color = "red") + #adding regression line
geom_segment(aes(xend = x1,yend = y),alpha = 0.2) + #adding a vertical predicted value
geom_point(aes(color = abs(newmodel_residual), size = abs(newmodel_residual))) + #color and size of points depends on the absolute of the residual
scale_color_continuous(low = "green", high = "darkred") +
guides() + 
geom_point(aes(y = newmodel_fitted), shape = 1) + theme_bw()
    

#histogram of residual errors new model
ggplot(data = data, aes(x = newmodel$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=100) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency') +
  xlim(min(newmodel$residuals), 1e-12)

#new model with weighted variance
newgls <- lm(data = data, y~x1+x3+x4+x5+x7+x14+x17,weights = 1/newmodel$fitted.values^2)
summary(newgls)
bptest(newgls)

#histogram of residual errors new gls
ggplot(data = data, aes(x = newgls$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black',bins=100) +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency') +
  xlim(min(newgls$residuals), 1e-12)



