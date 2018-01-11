#***************************************************************************************************#
# Data Analysis for Chinese company Geely Auto to enter the US Market. 
# Analysis done based on the data provided in CarPrice_Assignment.csv, along with the 
# data dictionary provided. 
# Author : ANARGHA BISWAS
#***************************************************************************************************#
# libaries used ************************************************************************************#

library(MASS)
library(car)
library(stringr)
library(dplyr)
library(tidyr)
library(stringi)
library(caret)
library(DAAG)

#***************************************************************************************************#
# Reading the data.
#***************************************************************************************************#
cars <- read.csv("CarPrice_Assignment.csv")
View(cars)
str(cars)

#***************************************************************************************************#
# DATA CLEANING
#***************************************************************************************************#
sum(is.na(cars)) # No NA Values present in the data.
summary(cars)

summary(cars$doornumber)
summary(cars$carbody)
summary(cars$enginetype)
summary(cars$fuelsystem)

# The price range of the cars vary from 5118 to 45400
range(cars$price)
boxplot(cars$price)

# There are 26 rows with prices greater than 30000.
length(cars[cars$price>30000, ])

#***************************************************************************************************#
# Spliting the car name into car company and car model
#***************************************************************************************************#
#cars$CarName <- as.character(cars$CarName)
#strsplit(cars$CarName, " ", fixed = TRUE, 1)

# car.1 <- separate(cars, "CarName", c("Car.Company", "car.model"), " ")
# But Audi 100 ls and Audi 100ls are the same model. So spliting should be based on only the 
# first delimiter. 

output <- stri_split_fixed(str = cars$CarName, pattern = " ", n = 2)
cars.df <- data.frame(cars, do.call(rbind, output))

colnames(cars.df)[colnames(cars.df) == 'X1'] <- "Car.Company"
colnames(cars.df)[colnames(cars.df) == 'X2'] <- "Car.Model"
colnames(cars.df)[colnames(cars.df) == 'curbweight'] <- "carweight"

cars.df <- subset(cars.df, select = -c(CarName))

# Looks like there are duplicate values in the data frame. 
length(unique(cars.df$Car.Company))

#***************************************************************************************************#
# Though there are 28 unique companies, there are some spell mistakes, 
# which needs to be taken care of. 
#***************************************************************************************************#

unique(cars.df$Car.Company)
summary(cars.df$Car.Company)
cars.df$Car.Company[cars.df$Car.Company == 'porcshce'] <- "porsche"
cars.df$Car.Company[cars.df$Car.Company == 'maxda'] <- "mazda"
cars.df$Car.Company[cars.df$Car.Company == 'vokswagen'] <- "volkswagen"
cars.df$Car.Company[cars.df$Car.Company == 'vw'] <- "volkswagen"
cars.df$Car.Company[cars.df$Car.Company == 'toyouta'] <- "toyota"

#***************************************************************************************************#
# Upper case - lower case issue for nissan
#***************************************************************************************************#

cars.df$Car.Company = tolower(cars.df$Car.Company)
cars.df$Car.Company = as.factor(cars.df$Car.Company)
length(unique(cars.df$Car.Company))
summary(cars.df$Car.Company)
summary(cars.df)

#***************************************************************************************************#
# Conversion of factors to numeric. We are using DummyVars from the caret package.
# This will take care of the categorical variables at one shot. 
# fullRank = T signifies that we remove the redundant variables. 
# i.e for a variable with 3 categories , we need to split into only 2 columns. 

# Since we dont need to consider the car models
#***************************************************************************************************#
cars.df <- subset(cars.df, select = -c(car_ID, Car.Model))

dummy <- dummyVars("~ .", data = cars.df, fullRank = T)
car.Model <- data.frame(predict(dummy, newdata = cars.df))


#***************************************************************************************************#
# Modelling the data frame
#***************************************************************************************************#

# Divide into training and test data set
set.seed(100)

train.indices <- sample(1:nrow(car.Model), 0.7*nrow(car.Model))
train.df <- car.Model[train.indices, ]

test.df <- car.Model[-train.indices, ]

model.1 <- lm(price~.,data = train.df)
summary(model.1)
#***************************************************************************************************#
# At this point we have 65 columns in all in the cars.model df. 
# Next we need to include only the significant columns, which 
# can be done with the help of stepAIC function. 
#***************************************************************************************************#

step <- stepAIC(model.1, direction="both")
step

temp.df <- subset(car.Model, select = c(symboling, aspiration.turbo,carbody.hardtop, carbody.hatchback, 
                                        carbody.sedan, carbody.wagon, drivewheel.rwd, enginelocation.rear, 
                                        carlength, carwidth, carheight, carweight, enginetype.l, enginetype.ohcv, enginetype.rotor, 
                                        cylindernumber.four, cylindernumber.three, cylindernumber.twelve, enginesize, fuelsystem.spdi, 
                                        boreratio, stroke, peakrpm, fuelsystem.2bbl,
                                          Car.Company.audi, Car.Company.bmw, Car.Company.buick, Car.Company.dodge, Car.Company.honda
                                        ,Car.Company.mazda, Car.Company.mitsubishi, Car.Company.nissan, Car.Company.plymouth, 
                                          Car.Company.porsche, Car.Company.saab, Car.Company.volkswagen))

#***************************************************************************************************#
# Lets prepare a data frame with the relevant variables and prepare a correlation
# matrix out of that. This will help us in understanding the correlation between the variables. 
#***************************************************************************************************#

new_cor <- cor(temp.df)
write.csv(new_cor, "new_cor.csv")

# Lets try to find out a proper model now. !

model.2 <- lm(formula = price ~ symboling + aspiration.turbo + carbody.hardtop + carbody.hatchback +
                carbody.sedan + carbody.wagon + enginelocation.rear + carlength + carwidth + carheight +
                carweight + enginetype.l + enginetype.ohcv + enginetype.rotor + cylindernumber.four +
                cylindernumber.three + cylindernumber.twelve + enginesize + fuelsystem.spdi + boreratio + 
                stroke + peakrpm + fuelsystem.2bbl + 
                Car.Company.audi + Car.Company.bmw + Car.Company.buick + Car.Company.dodge + Car.Company.honda
              + Car.Company.mazda + Car.Company.mitsubishi + Car.Company.nissan + Car.Company.plymouth + 
                Car.Company.porsche + Car.Company.saab + Car.Company.volkswagen, data = train.df)

summary(model.2)
vif(model.2)

#***************************************************************************************************#
# There are quite a number of variables with high VIF values. 
# e.g carweight and carbody.sedan. Should we include them in our analysis? 
# Lets try to find out. 
#***************************************************************************************************#

#***************************************************************************************************#
# While carbody.sedan can be removed , carweight should not be removed 
# since it has low p-value. 

# We remove carbody.sedan and carbody.wagon and Car.Company.volkswagen 
#***************************************************************************************************#

model.3 <- lm(formula = price ~ symboling + aspiration.turbo + carbody.hardtop + carbody.hatchback +
                enginelocation.rear + carlength + carwidth + carheight +
                carweight + enginetype.l + enginetype.ohcv + enginetype.rotor + cylindernumber.four +
                cylindernumber.three + cylindernumber.twelve + enginesize + fuelsystem.spdi + boreratio + 
                stroke + peakrpm + fuelsystem.2bbl + 
                Car.Company.audi + Car.Company.bmw + Car.Company.buick + Car.Company.dodge + Car.Company.honda
              + Car.Company.mazda + Car.Company.mitsubishi + Car.Company.nissan + Car.Company.plymouth + 
                Car.Company.porsche + Car.Company.saab, data = train.df)

summary(model.3)
vif(model.3)

#***************************************************************************************************#
# Next we remove car.company.mazda, Car.Company.nissan, 
# fuelsystem.2bbl and fuelsystem.spdi. 
# This way to keep on removing and adding variables which have high p-values
# and high VIF at the same time. 
#***************************************************************************************************#

model.4 <- lm(formula = price ~ symboling + aspiration.turbo + carbody.hardtop + carbody.hatchback +
                enginelocation.rear + carlength + carwidth + carheight +
                carweight + enginetype.l + enginetype.ohcv + enginetype.rotor + cylindernumber.four +
                cylindernumber.three + cylindernumber.twelve + enginesize + boreratio + 
                stroke + peakrpm +  
                Car.Company.audi + Car.Company.bmw + Car.Company.buick + Car.Company.dodge + Car.Company.honda
              + Car.Company.mitsubishi + Car.Company.plymouth + 
                Car.Company.porsche + Car.Company.saab, data = train.df)

summary(model.4)
vif(model.4)

#***************************************************************************************************#
# Next we remove carheight, carbody.hardtop, symboling and 
# Car.Company.porsche amd build model.5
#***************************************************************************************************#

model.5 <- lm(formula = price ~ aspiration.turbo + carbody.hatchback +
                enginelocation.rear + carlength + carwidth +
                carweight + enginetype.l + enginetype.ohcv + enginetype.rotor + cylindernumber.four +
                cylindernumber.three + cylindernumber.twelve + enginesize + boreratio + 
                stroke + peakrpm +  Car.Company.audi + Car.Company.bmw + Car.Company.buick + 
                Car.Company.dodge +  Car.Company.honda + Car.Company.mitsubishi + Car.Company.plymouth + 
                Car.Company.saab, data = train.df)

summary(model.5)
vif(model.5)


#***************************************************************************************************#
# Next we remove carbody.hatchback , enginetype.l, Car.Company.honda and 
# Car.Company.audi amd build model.6
# Adjusted R-squared:  0.957
#***************************************************************************************************#

model.6 <- lm(formula = price ~ aspiration.turbo + enginelocation.rear + carlength + carwidth +
                carweight + enginetype.ohcv + enginetype.rotor + cylindernumber.four +
                cylindernumber.three + cylindernumber.twelve + enginesize + boreratio + 
                stroke + peakrpm +  Car.Company.bmw + Car.Company.buick + Car.Company.dodge + 
                Car.Company.mitsubishi + Car.Company.plymouth + Car.Company.saab, data = train.df)

summary(model.6)
vif(model.6)

#***************************************************************************************************#
# Removing carweight, carlength, cylindernumber.four, 
# Car.Company.dodge , Car.Company.plymouth, Car.Company.saab
#***************************************************************************************************#

model.7 <- lm(formula = price ~ aspiration.turbo + enginelocation.rear + carwidth +
                enginetype.ohcv + enginetype.rotor + cylindernumber.three + 
                cylindernumber.twelve + enginesize + boreratio + stroke + peakrpm +  
                Car.Company.bmw + Car.Company.buick + Car.Company.mitsubishi, data = train.df)

summary(model.7)
vif(model.7)

#***************************************************************************************************#
# Removing cylindernumber.three, cylindernumber.twelve, Car.Company.mitsubishi
#***************************************************************************************************#

model.8 <- lm(formula = price ~ aspiration.turbo + enginelocation.rear + carwidth +
                enginetype.ohcv + enginetype.rotor + enginesize + boreratio + 
                stroke + peakrpm +  Car.Company.bmw + Car.Company.buick, data = train.df)

summary(model.8)
vif(model.8)


#***************************************************************************************************#
# Removing enginetype.ohcv and boreratio, we get final model model.9
# The adjusted p-value is 0.935. 
# The adjusted p-value is slightly lower than the ajusted p-value for the previous
# model, but in this model, all the variables have low p-values as compared
# to the previous models. 
#***************************************************************************************************#

model.9 <- lm(formula = price ~ aspiration.turbo + enginelocation.rear + carwidth +
                enginetype.rotor + enginesize + 
                stroke + peakrpm +  Car.Company.bmw + Car.Company.buick, data = train.df)

summary(model.9)
vif(model.9)
#***************************************************************************************************#
# Indeed we have achieved indepedant variables with low p-values but if we have a look at the 
# correlation matrix then we see that there is good level of correlation between carwidth 
# and enginesize. So we remove the carwidth in the final model. 

# we have achieved vif values less than 2 for all the indepedant variables now. 
#***************************************************************************************************#


model.10 <- lm(formula = price ~ aspiration.turbo + enginelocation.rear +
                 enginetype.rotor + enginesize + 
                 stroke + peakrpm +  Car.Company.bmw + Car.Company.buick, data = train.df)

summary(model.10)
vif(model.10)

#***************************************************************************************************#
# At this point we have all the variables with low p-value, and low VIF. This indicates that the 
# independant variables are not dependant on each other. and also they are significant in predicting
# the price. 
# From the business perspective, the engine size, turbo, stroke, peakrpm do influence the 
# price of cars. 
#***************************************************************************************************#
coefficients(model.10)

#***************************************************************************************************#
# The coefficients of the variables in the model indicates that increase in stroke
# negatively impacts the price of cars. This we compared with models with other variables, 
# but the sign remains negative. This implies that our model is not unstable. 

# Buick and BMW brands have a high influence on the price of cars, which is justified 
# considering the fact that both of them are luxury cars. 
#***************************************************************************************************#
# Trying to predict the price on the test df. 
#****************************************************************************#
predict.1 <- predict(model.10, subset(test.df, select = -(price)))
test.df$test_price <- predict.1

test.df$residual <- test.df$price - test.df$test_price
range(test.df$residual)

plot(test.df$residual, main = "Plotting of Residuals")
#***************************************************************************************************#
# We can see from plotting the residual values, ie.the errors, that they are completely random, 
# or in other words, they are basically white noise and no pattern is observed. 
#***************************************************************************************************#
scatterplot(test.df$price, test.df$test_price)

#***************************************************************************************************#
# Model Validation.
# 1. Trying to find the r and rsquared values between the real price and the predicted price. 
#***************************************************************************************************#

r <- cor(test.df$price, test.df$test_price)
rsquared <- cor(test.df$price, test.df$test_price)^2 # 0.809

#***************************************************************************************************#
# 2. 3 fold validation. 
# The model is tested with 3 different train datasets and the results are compared, 
# so as to make sure that the model is not an overfitting one. 

# Observation : For all the three models, the predicted line of regression fits pretty well
# for all the three cases. 
#***************************************************************************************************#
cv.lm(data =train.df, model.10, m=3) # 3 fold cross-validation

#***************************************************************************************************#
# 3. Plotting the residuals, predicted and the real price values.  
# Plot 1 : Residual vs Plotted - Though we see an L shape in the beginning, but its mostly linear
# at the end , and the residuals are distributed almost equally on either sides. 

# Plot 2 : Normal Q-Q Graph
# The Residuals follow almost a straight line. i.e they are lined along the regression line. 

# Plot 3 : Scale-Location Graph. 
# The Residuals are distributed along a almost straight line in this case. 

# Plot 4: Residual vs Leverage Graph
# LAstly, we try to find the influential cases, ie outliers which can change the linear regression 
# pattern altogether. The level of influence is calculated based on cook's distance. 
# If points are there beyond the cook's distance then they can be considered outliers, the points 
# which are located on the right upper and lower part of the graph. In our case we dont see any. 
# which is a good indication that our model is not influenced by any particular outlier value. 
#***************************************************************************************************#

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(model.10)

#***************************************************************************************************#
# Conclusion : 
# The Business Perspective : 

# For our company, we should focus on the factors like aspiration.turbo, the enginelocation , 
# the car width, The type of engine, the size of the engine, the stroke, the peakrpm offered. 
# These will be the deciding factors for the chinese company to focus on, to give competition to
# the existing brands in the market. These are the factors that the customers of US look upon, 
# when they plan to buy a car.Company BMW and Buick also has a considerable influence in the prices
# but from the business perspective we cant do anything about it apart from building a good impression
# in people's mind regarding our brand. In a way it is good also, considering the fact that if our chinese 
# company can have a good brand value then people will surely consider to buy our company's cars. 

# Note : We have kept the model simple so that it doenst overfit the data. The Adjusted R-squared 
# is pretty high in comparison to the industry standards, but the parameters that are chosen have 
# pretty low p-values and VIF values which sounds promising. 
#***************************************************************************************************#


