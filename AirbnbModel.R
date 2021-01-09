setwd("users/krinalmanakiwala/desktop/RProjects")
airbnb = read.csv("airbnb.csv")

##1. How many observations?
nrow(airbnb)
##2. How many variables?
ncol(airbnb)
colnames(airbnb)
##3. Which columns have NA values?
summary(airbnb)
##OR:
colSums(is.na(airbnb))
##4. What's the percentage of NA values in square feet
round(100*(sum(is.na(airbnb$square_feet))/length(airbnb$square_feet)),2)
##5. Convert zipcode to its numeric form
sum(is.na(airbnb$zipcode))
airbnb$zipcode = as.character(airbnb$zipcode)
sum(is.na(airbnb$zipcode))
airbnb$zipcode = as.numeric(airbnb$zipcode)
sum(is.na(airbnb$zipcode))
##6. Remove the NAs and find the average after they are removed
airbnb = airbnb[!is.na(airbnb$zipcode),]
sum(is.na(airbnb$zipcode))
nrow(airbnb)
round(mean(airbnb$price), 2)
##7. i. There are 17 NA values in beds. Impute the NA values in beds with the median of beds (remember to use na.rm parameter in the function)
airbnb$beds[is.na(airbnb$beds)] = median(airbnb$beds, na.rm = T)
sum(is.na(airbnb$beds))
## ii. There are 5,702 NA values in cleaning_fee. Impute the NA values in cleaning_fee column with 0. This is with the assumption that cleaning_fee is already included in the price for observations where it is NA. 
sum(is.na(airbnb$cleaning_fee))
airbnb$cleaning_fee[is.na(airbnb$cleaning_fee)] = 0
cor(airbnb[,unlist(lapply(airbnb, is.numeric))])
##8. Which variables should not be included in a linear regression model?
model = lm(price~city + zipcode + latitude + longitude + property_type + room_type + accommodates + bathrooms
           + bedrooms + beds+ bed_type  + cleaning_fee + guests_included + extra_people + 
             number_of_reviews + review_scores_rating + reviews_per_month,data=airbnb)
summary(model)
##if this has a lot of factors, it should not be included as part of the linear regression model
  ##9. i. You will now split the data set into train and test using sample function. Set the seed as 123 (Please remember to use the RNGkind function)
RNGkind(sample.kind = 'Rounding')
set.seed(123)
split = sample(1:nrow(airbnb), 0.7*nrow(airbnb))
train = airbnb[split, ]

test = airbnb[-split, ]
nrow(test)
## ii. Run a linear regression model for predicting price based on the following variables - zipcode, latitude, longitude, accommodates, beds, bedrooms, bathrooms, cleaning_fee, bed_type, room_type, guests_included, extra_people, & review_scores_rating 
model1 = lm(price ~ zipcode+latitude+longitude+accommodates+beds+bedrooms+bathrooms+cleaning_fee+bed_type+room_type+guests_included+extra_people+review_scores_rating, data = train)
summary(model1)

pred = predict(model1, newdata = train)
rmse = sqrt(mean((pred - airbnb$price)^2)); rmse

##10. What is the out of sample (test data) R-squared and RMSE? (Round off to 4 decimal points)
##model2 = lm(price ~ zipcode+latitude+longitude+accommodates+beds+bedrooms+bathrooms+cleaning_fee+bed_type+room_type+guests_included+extra_people+review_scores_rating, data = test)
##summary(model2)

pred2 = predict(model1, newdata = test)

sse = sum((pred2 - test$price)^2)
sst = sum(((pred2-test$price)-test$price)^2)
r2 = 1 - sse/sst; round(r2,2)


rmse2 = sqrt(mean((pred2 - airbnb$price)^2)); rmse2

