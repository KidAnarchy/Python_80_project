#========================================================================
#                Data Preprocessing Airbnb Listing Price
#========================================================================
# Import library
library(tidyverse)
library(psych)
library(ggplot2)
library(corrplot)
library(skimr)
library(plyr)
library(caret) #OHE

#========================================================================
#                STEP 1: Initial Exploratory Analysis
#========================================================================
# Import Data set file(.CSV)
airbnb <- read.csv('AB_NYC_2019.CSV')

# Overview Data
view(airbnb)

summary(airbnb)

# Drop some columns
drop <- c("id","name","host_id","host_name","last_review")
airbnb = airbnb[,!(names(airbnb) %in% drop)]

# Deal with missing data
sapply(airbnb,function(airbnb) sum(is.na(airbnb)))

# Replace the NA's in the particcular column with 0
airbnb$reviews_per_month[is.na(airbnb$reviews_per_month)]<-0
summary(airbnb)
#========================================================================
#                   STEP 2: Exploratory Data Analysis
#========================================================================
# Histogram
hist(airbnb$price)

# BoxPlot 
boxplot(airbnb$calculated_host_listings_count)
# 1.Price
# 2.Minimum night
# 3.reviews_per_month
# 4.calculated_host_listings_count

# Categorical Variables(that we need to do OHE)
table(airbnb$neighbourhood_group)
# 5 types
table(airbnb$room_type)
# 3 types

# we will use features to predict are ("neighbourhood_group(OHE),neighbourhood,latitude,
# longtitude, Room type(OHE), minimum_night, number_of_reviews,reviews_per_month,
# calculated_host_listings_count, availability_365) Indepent valiable.

# Check and remove duplicate 
airbnb %>% duplicated()
summary(airbnb)

#========================================================================
#                          Step3: Remove Outliers
#========================================================================
# 3.1.Price
priceOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .90), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
airbnb$price = priceOutlier(airbnb$price)
boxplot(airbnb$price)

# 3.2.Minimum night
MinNightOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .98), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
airbnb$minimum_nights = MinNightOutlier(airbnb$minimum_nights)
boxplot(airbnb$minimum_nights)

# 3.3.calculated_host_listings_count
hostOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}
airbnb$calculated_host_listings_count = hostOutlier(airbnb$calculated_host_listings_count)
boxplot(airbnb$calculated_host_listings_count)
#================================================================================
#          Step4: Handle with Category Feature(Transform to Numerical)
#================================================================================

# 4.1 "neighbourhood" used Bayesian Encoders

encode_target <- function(x, y, sigma = NULL) {
  d <- aggregate(y, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  m <- d[is.na(as.character(d[, 1])), 2]
  l <- d[, 2]
  names(l) <- d[, 1]
  l <- l[x]
  l[is.na(l)] <- m
  if (!is.null(sigma)) {
    l <- l * rnorm(length(l), mean = 1, sd = sigma)
  }
  l
}
new_bnb <- airbnb
new_bnb[["neb_encoded"]] <- encode_target(airbnb[["neighbourhood"]],airbnb[["price"]])
head(new_bnb)


# 4.2 "room_type" used OHE Ordinal type
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
table(new_bnb[["room_type"]],
      encode_ordinal(new_bnb[["room_type"]], order = c("Shared room", "Private room", "Entire home/apt")),
      useNA = "ifany")
#                     1     2     3
# Entire home/apt     0     0 25409
# Private room        0 22326     0
# Shared room      1160     0     0

new_bnb[["room_encoded"]] <- encode_ordinal(new_bnb[["room_type"]])
head(new_bnb)

# 4.3 "neighborhood_group" used OHE Nominal type
new_bnb1 <- new_bnb

new_bnb1$neighbourhood_group <- factor(new_bnb1$neighbourhood_group, exclude = NULL)
new_bnb1$neighbourhood_group <- addNA(new_bnb1$neighbourhood_group)
dv <- caret::dummyVars(" ~ neighbourhood_group", data = new_bnb)

new_bnb1 <- data.frame(predict(dv, newdata = new_bnb1))
head(new_bnb1)

count(new_bnb1)
# Drop column "neighbourhood_group.NA"
drop <- c("neighbourhood_groupNA")
new_bnb1 = new_bnb1[,!(names(new_bnb1) %in% drop)]

#========================================================================
#             STEP5: Data Transformation in Numerical feature
#========================================================================
new_bnb2 <- new_bnb
drop <- c("neighbourhood_group","neighbourhood","room_type","price")
new_bnb2 = new_bnb2[,!(names(new_bnb2) %in% drop)]
# Visual Correlation
corrplot(cor(new_bnb2))

# 5.1 Standardization(Easy way Scaling)
#preproc1 <- preProcess(new_bnb2, method=c("center", "scale"))
#Std_bnb <- predict(preproc1, new_bnb2)

# 5.2 Min-max Scaling
preproc2 <- preProcess(new_bnb2, method=c("range"))
Norm_bnb <- predict(preproc2, new_bnb2)

#========================================================================
#                STEP6: Combine table and export data
#========================================================================
# Class target feature
Target_fea <- data.frame(airbnb$price)
# Take log to 
Target_fea <- log(Target_fea)

hist(Target_fea$airbnb.price)

# Combine table to export .csv file to modelling.
airbnb_pre<- cbind(Norm_bnb,new_bnb1,Target_fea)

write.csv(airbnb_pre,"D:\\airbnb_pre.csv", row.names = FALSE)

#========================================================================#



