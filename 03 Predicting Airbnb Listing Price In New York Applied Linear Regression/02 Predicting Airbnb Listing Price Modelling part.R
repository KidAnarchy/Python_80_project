#======================================================================
#                      Predicting Airbnb Listing Price
#======================================================================
# Import library
library(tidyverse)
library(psych)
library(ggplot2)
library(corrplot)
library(skimr)
library(plyr)
library(caret) #OHE


#==========================================================================
#            STEP 1: Import data that finished in prepocessing 
#==========================================================================
# Import Data set file(.CSV)
airbnb <- read.csv('airbnb_pre.CSV')
str(airbnb)

# Check column names
colnames(airbnb)

# Overview data
summary(airbnb)

# Drop column "neighbourhood_groupBronx,neighbourhood_groupStaten.Island"
drop <- c("neighbourhood_groupBronx","neighbourhood_groupStaten.Island")
airbnb = airbnb[,!(names(airbnb) %in% drop)]
# **** drop because found Significant code not good

# Ckeck again with missing data
sapply(airbnb,function(airbnb) sum(is.na(airbnb)))


#===================================================================
#                STEP 2: Train and Test split
#===================================================================
# Random sampling
samplesize = 0.75*nrow(airbnb)
set.seed(3)
index = sample(seq_len(nrow(airbnb)), size = samplesize)

# Create training and test set
airbnb_train = airbnb[index,]
airbnb_test = airbnb[-index,]



#===================================================================
#             STEP3: Training  Model
#===================================================================
model = lm(airbnb.price~., data = airbnb_train)
summary(model)

# Checked Prediction Value(Optional)
model$fitted.values

# Checked residuals value that error(Optional)
model$residuals


#====================================================================
#                          STEP4: Prediction
#====================================================================
predictions = predict(model,airbnb_test)


# computing model performance metrics
data.frame( R2 = R2(predictions, airbnb_test$airbnb.price),
            RMSE = RMSE(predictions, airbnb_test$airbnb.price),
            MAE = MAE(predictions, airbnb_test$airbnb.price))
          

#model = lm(price~latitude+longitude+minimum_nights+number_of_reviews+reviews_per_month
           #+calculated_host_listings_count+availability_365+neb_encoded+room_encoded
           #+neighbourhood_groupBronx+neighbourhood_groupBrooklyn+neighbourhood_groupManhattan
           #+neighbourhood_groupQueens+neighbourhood_groupStaten.Island, data = airbnb_train)

# Checked Individaul Linear relationship
plot(airbnb.price~neb_encoded,data=airbnb)




#=============================================================
#                     Variable selection
#=============================================================
null_mod =lm(airbnb.price~1, data=airbnb)
full_mod =lm(airbnb.price ~., data=airbnb)

#Forward
mod_forw = step(null_mod,scope=list(lower=null_mod,upper=full_mod),direction="forward")
summary(mod_forw)

#Backward
mod_back = step(full_mod,data=df.house,direction="backward")
summary(mod_back)

#Stepwise
mod_step = step(full_mod, direction="both", data=df.house)
summary(mod_step)

#======Additional Method in Variable selection=======#

#install.packages("MASS")
library(MASS)
step.model =  stepAIC(full_mod, direction = "backward", 
                      trace = FALSE)
summary(step.model)

#=========Test====================#
predictions1 = predict(mod_back,airbnb_test)


# computing model performance metrics
data.frame( R2 = R2(predictions1, airbnb_test$airbnb.price),
            RMSE = RMSE(predictions1, airbnb_test$airbnb.price),
            MAE = MAE(predictions1, airbnb_test$airbnb.price))

#======================================================================
#                         STEP5: Model checking
#======================================================================
par(mfrow = c(2, 2))
plot(model)

# Visulization
#install.packages("ggResidpanel")
library(ggResidpanel)
resid_panel(model)
resid_xpanel(model)

#install.packages("ggfortify")
library(ggfortify)
fit.f <- fortify(model)
ggplot(fit.f, aes(x = .fitted, y = .resid)) + geom_point()



#=======================================================================
###==================Dealing with multicollinearity==================###
#=======================================================================
car::vif(model)

#=========RECHECK IN STEP3: Linear Regression Model==========#
model1 = lm(airbnb.price~. -neighbourhood_groupBrooklyn , data = airbnb_train)
summary(model1)
# 
predictions2 = predict(model1,airbnb_test)

# computing model performance metrics
data.frame( R2 = R2(predictions2, airbnb_test$airbnb.price),
            RMSE = RMSE(predictions2, airbnb_test$airbnb.price),
            MAE = MAE(predictions2, airbnb_test$airbnb.price))

# ***Result** It can be seen that removing the 'neighbourhood_groupManhattan' variable 
# does not affect very much the model performance metrics.




#====================================================================
##====check the assumption of the regression model======###
#====================================================================
model2 = lm(airbnb.price~latitude+longitude+minimum_nights+reviews_per_month
            +calculated_host_listings_count+availability_365+neb_encoded+room_encoded , data = airbnb_train)
summary(model2)

#which model to be chosen
#say after we've check the assumption of the regression model, one
#can look at the AIC
#the smallest AIC is preferable
AIC(model) # tested, it is the lowest
AIC(model1)
AIC(model2)

predictions3 = predict(model2,airbnb_test)

# computing model performance metrics
data.frame( R2 = R2(predictions3, airbnb_test$airbnb.price),
            RMSE = RMSE(predictions3, airbnb_test$airbnb.price),
            MAE = MAE(predictions3, airbnb_test$airbnb.price))

#====================================================================#

