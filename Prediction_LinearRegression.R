#Load library
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(StatMeasures)
library(MASS)
library(car)

#Load Dataset
carPrice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

str(carPrice)

######################################################################################
############## Data Cleaning
#####################################################################################

## 1: Check for missing values
any(is.na(carPrice))

sapply(carPrice, function(y) sum(length(which(is.na(y)))))

# Data is clean no NA values

##  2: Check unique rows in data
length(unique(carPrice))
length(unique(carPrice$car_ID))

# 205 rows with 26 columns

## 2: Derive CarCompany name from carName
carPrice$CarCompany <- str_split_fixed(carPrice$CarName," ",n=2)[,1]

unique(carPrice$CarCompany)

# remove carname column as not needed anymore
carPrice <- subset(carPrice, select = -c(CarName))

# We can still see some issues with name vw is volkswagen and some spelling errors in toyota and porsche

carPrice$CarCompany <- sapply(carPrice$CarCompany, function(x) ifelse(x == "porcshce","porsche",x))
carPrice$CarCompany <- sapply(carPrice$CarCompany, function(x) ifelse(x == "toyouta","toyota",x))
carPrice$CarCompany <- sapply(carPrice$CarCompany, function(x) ifelse(x == "vw","volkswagen",x))
carPrice$CarCompany <- sapply(carPrice$CarCompany, function(x) ifelse(x == "vokswagen","volkswagen",x))
carPrice$CarCompany <- sapply(carPrice$CarCompany, function(x) ifelse(x == "maxda","mazda",x))
carPrice$CarCompany <- sapply(carPrice$CarCompany, function(x) ifelse(x == "Nissan","nissan",x))

# Convert the categorical variables to factors as we will need to create dummy variables for them
idx <- c(
  "symboling",
  "CarCompany",
  "fueltype",
  "aspiration",
  "doornumber",
  "carbody",
  "drivewheel",
  "enginelocation",
  "enginetype",
  "cylindernumber",
  "fuelsystem"
)

carPrice[, idx] <- lapply(carPrice[, idx], factor)

#Lets change the order of levels for cylinder number to correctly view them on graphs
carPrice$cylindernumber <-
  factor(
    carPrice$cylindernumber,
    levels = c("two", "three", "four", "five", "six","eight","twelve")
  )

levels(carPrice$cylindernumber)

######################################################################################
############## Exploratory Data Analysis
######################################################################################

# Price spread

ggplot(carPrice, aes(x = car_ID, y = price)) +
  geom_point(col = "red") + 
  labs(title = "Car Prices spread", x = "Car ID", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = price)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Car Prices spread", x = "All cars", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

quantile(carPrice$price, seq(0,1,0.01))
outliers(carPrice$price)

# We can see outliers in this set but that is because of the fact that certain luxury brands have a higher price
# We cannot remove them from analysis

############## Univariate Categorical variables

# 1: Symboling
ggplot(carPrice, aes(x = as.factor(symboling))) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of Cars for various Insurance Risk Rating", x = "Insurance Risk Rating", y = "Number of cars") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,face="bold"), axis.text.y = element_text(face="bold"))

ggplot(carPrice, aes(x = as.factor(symboling), y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Prices", x = "Insurance Risk Rating", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# cars with insurance rating 1 have the lowest priced cars.

# 2: Car Company
ggplot(carPrice, aes(x = CarCompany)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +  
  labs(title = "Number of Cars for various brands", x = "Car Company", y = "Number of cars") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,face="bold"), axis.text.y = element_text(face="bold"))

ggplot(carPrice, aes(x = CarCompany, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Prices", x = "Car Company", y = "Car Price") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,face="bold"), axis.text.y = element_text(face="bold"))

# Car brands bmw, buick, jaguar and porche have the costliest cars in the market.
# Whereas toyota has the maximum number of cars 

# 3: Fuel Type
ggplot(carPrice, aes(x = fueltype)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of Cars for different fuel types", x = "Fuel Type", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = fueltype, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Fuel Type", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# very less diesel cars in the market and reason is evident from the prices
# Diesel cars have a higher price compared to gas

# 4: Aspiration
ggplot(carPrice, aes(x = aspiration)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of turbo/std Aspiration cars", x = "Aspiration", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = aspiration, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Aspiration", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# Cars with std Aspiration are much cheaper compared to turbo ones

# 5: DoorNumber
ggplot(carPrice, aes(x = doornumber)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Door Numbers", x = "Door Number", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = doornumber, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Door Number", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# Not much can be concluded from the above for car prices

# 6: Car Body
ggplot(carPrice, aes(x = carbody)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Car Body", x = "Car Body Type", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = carbody, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Car Body Type", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# Car Prices are much less for hatchback sedans and wagons. 
# Converibles and hardtops are much pricier.

# 7: Drivewheel
ggplot(carPrice, aes(x = drivewheel)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Drive wheel", x = "Drive wheel Type", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = drivewheel, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Drive wheel Type", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# Rear wheel drives are clearly the costliest.

# 8: Engine Location
ggplot(carPrice, aes(x = enginelocation)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Engine Location", x = "Engine Location", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = enginelocation, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Engine Location", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# cars with Rear engines are clearly the costliest.

# 9: Engine Type
ggplot(carPrice, aes(x = enginetype)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Engine Types", x = "Engine Types", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = enginetype, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Engine Types", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# Most of the less priced cars have ohc or ohcf engine.

# 10: Cylinder Number
ggplot(carPrice, aes(x = cylindernumber)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Number of cylinders", x = "Number of cylinders", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = cylindernumber, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Number of cylinders", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# Car prices increase with the number of cylinders from three to twelve
# With four cylinder cars being the most in number

# 11: Fuel System
ggplot(carPrice, aes(x = fuelsystem)) +
  geom_bar(stat = "count",fill="dark green") +
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)  +
  labs(title = "Number of cars with different Fuel systems", x = "Fuel system", y = "Number of cars") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = fuelsystem, y= price)) +
  geom_boxplot(fill = "light blue",outlier.colour = "red") +
  labs(title = "Car Price Spread", x = "Fuel systems", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

# cars with mpfi fuel system have the costliest cars with all outliers lying in this category

##  Summary:

# 1: Cars with insurance symboling 1 influence Price
# 2: Car brands bmw, buick, jaguar and porche have the costliest cars in the market.
# 3: Cars with std aspiration clearly have cheaper cars. Hence should affect the price.
# 4: Hatchback, sedans and wagons are the cheaper car body types with most cars. With sedans being the most in number.
# 5: Drivewheel type "rear wheel drive" definitely affect the car prices being the pricier then others.
# 6: Cars with front engine location are the most in quantity and much cheaper.
# 7: Cars with Engine type ohc and ohcf have cheapest cars. With ohc engine type used in most cars.
# 8: four cylinder cars are the most and higher or lower the number of cylinders costlier the cars become


############## Univariate Continuous variables

# Now lets see the effect on car prices due to continuos variables

# 1: Wheel Base
ggplot(carPrice, aes(x = wheelbase, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Wheel Base", x = "Wheel Base", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = wheelbase)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Wheel Base") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$wheelbase,seq(0,1,0.01))
quantile(carPrice$wheelbase,probs = 0.98)
# capping the outlier values above 98 percentile
carPrice$wheelbase[which(carPrice$wheelbase > quantile(carPrice$wheelbase,probs = 0.98))] <- quantile(carPrice$wheelbase,probs = 0.98)

# 2: Car Length
ggplot(carPrice, aes(x = carlength, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Car Length", x = "Car Length", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = carlength)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Car Length") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$carlength,seq(0,1,0.01))
quantile(carPrice$carlength,probs = 0)
# capping the outlier values below 1 percentile
carPrice$carlength[which(carPrice$carlength < quantile(carPrice$carlength,probs = 0.01))] <- quantile(carPrice$carlength,probs = 0.01)

# 3: Car Width
ggplot(carPrice, aes(x = carwidth, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Car Width", x = "Car Width", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = carwidth)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Car Width") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$carwidth,seq(0,1,0.01))
quantile(carPrice$carwidth,probs = 0.98)
outliers(carPrice$carwidth)
# capping the outlier values above 96 percentile
carPrice$carwidth[which(carPrice$carwidth > quantile(carPrice$carwidth,probs = 0.96))] <- quantile(carPrice$carwidth,probs = 0.96)

# 4: Car Height
ggplot(carPrice, aes(x = carheight, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Car Height", x = "Car Height", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = carheight)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Car Height") +
  theme(axis.text = element_text(face="bold"))
# No Outliers

# 5: Curbweight
ggplot(carPrice, aes(x = curbweight, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Car Curbweight", x = "Car Curbweight", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = curbweight)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Curbweight") +
  theme(axis.text = element_text(face="bold"))
# No Outliers

# 6: Engine Size
ggplot(carPrice, aes(x = enginesize, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Car Engine Size", x = "Car Engine Size", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = enginesize)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Engine Size") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$enginesize,seq(0,1,0.01))
quantile(carPrice$enginesize,probs = 0.95)
outliers(carPrice$enginesize)
carPrice$enginesize[outliers(carPrice$enginesize)$idxOutliers]
# capping the outlier values above 95 percentile
carPrice$enginesize[which(carPrice$enginesize > quantile(carPrice$enginesize,probs = 0.95))] <- 
  quantile(carPrice$enginesize,probs = 0.95)

# 7: Bore Ratio
ggplot(carPrice, aes(x = boreratio, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Bore Ratio", x = "Bore Ratio", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = boreratio)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Bore Ratio") +
  theme(axis.text = element_text(face="bold"))
# No Outliers

# 8: Stroke
ggplot(carPrice, aes(x = stroke, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Stroke", x = "Stroke", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = stroke)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "stroke") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$stroke,seq(0,1,0.01))
quantile(carPrice$stroke,probs = 0.98)
outliers(carPrice$stroke)
carPrice$stroke[outliers(carPrice$stroke)$idxOutliers]
# capping the outlier values below 3 percentile
carPrice$stroke[which(carPrice$stroke < quantile(carPrice$stroke,probs = 0.03))] <- 
  quantile(carPrice$stroke,probs = 0.03)
# capping the outlier values above 97 percentile
carPrice$stroke[which(carPrice$stroke > quantile(carPrice$stroke,probs = 0.97))] <- 
  quantile(carPrice$stroke,probs = 0.97)

# 9: Compression ratio
ggplot(carPrice, aes(x = compressionratio, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Compression ratio", x = "Compression ratio", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = compressionratio)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Compression Ratio") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$compressionratio,seq(0,1,0.01))
quantile(carPrice$compressionratio,probs = 0.90)
outliers(carPrice$compressionratio)
carPrice$compressionratio[outliers(carPrice$compressionratio)$idxOutliers]
# capping the outlier values below 4 percentile
carPrice$compressionratio[which(carPrice$compressionratio < quantile(carPrice$compressionratio,probs = 0.04))] <- 
  quantile(carPrice$compressionratio,probs = 0.04)
# capping the outlier values above 90 percentile
carPrice$compressionratio[which(carPrice$compressionratio > quantile(carPrice$compressionratio,probs = 0.90))] <- 
  quantile(carPrice$compressionratio,probs = 0.90)

# 10: Horsepower
ggplot(carPrice, aes(x = horsepower, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Horsepower", x = "Horsepower", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = horsepower)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Horsepower") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$horsepower,seq(0,1,0.01))
quantile(carPrice$horsepower,probs = 0.98)
outliers(carPrice$horsepower)
carPrice$horsepower[outliers(carPrice$horsepower)$idxOutliers]
# capping the outlier values above 97 percentile
carPrice$horsepower[which(carPrice$horsepower > quantile(carPrice$horsepower,probs = 0.97))] <- 
  quantile(carPrice$horsepower,probs = 0.97)

# 11: Peak RPM
ggplot(carPrice, aes(x = peakrpm, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Peak RPM", x = "Peak RPM", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = peakrpm)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Peak RPM") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$peakrpm,seq(0,1,0.01))
quantile(carPrice$peakrpm,probs = 0.98)
outliers(carPrice$peakrpm)
carPrice$peakrpm[outliers(carPrice$peakrpm)$idxOutliers]
# capping the outlier values above 99 percentile
carPrice$peakrpm[which(carPrice$peakrpm > quantile(carPrice$peakrpm,probs = 0.99))] <- 
  quantile(carPrice$peakrpm,probs = 0.99)

# 12: City Mileage
ggplot(carPrice, aes(x = citympg, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs City Mileage", x = "City Mileage", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = citympg)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "City Mileage") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$citympg,seq(0,1,0.01))
quantile(carPrice$citympg,probs = 0.99)
outliers(carPrice$citympg)
carPrice$citympg[outliers(carPrice$citympg)$idxOutliers]
# capping the outlier values above 99 percentile
carPrice$citympg[which(carPrice$citympg > quantile(carPrice$citympg,probs = 0.99))] <- 
  quantile(carPrice$citympg,probs = 0.99)

# 13: Highway Mileage
ggplot(carPrice, aes(x = highwaympg, y= price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Car Price vs Highway Mileage", x = "Highway Mileage", y = "Car Price") +
  theme(axis.text = element_text(face="bold"))

ggplot(carPrice, aes(x = "",y = highwaympg)) +
  geom_boxplot(fill = "light blue" , outlier.colour = "red") +
  labs(title = "Outlier Check", x = "All cars", y = "Highway Mileage") +
  theme(axis.text = element_text(face="bold"))

# Treat the outliers
quantile(carPrice$highwaympg,seq(0,1,0.01))
quantile(carPrice$highwaympg,probs = 0.99)
outliers(carPrice$highwaympg)
carPrice$highwaympg[outliers(carPrice$highwaympg)$idxOutliers]
# capping the outlier values above 98 percentile
carPrice$highwaympg[which(carPrice$highwaympg > quantile(carPrice$highwaympg,probs = 0.98))] <- 
  quantile(carPrice$highwaympg,probs = 0.98)

### Summary
## Positive Linear relationship : 
# car prices increases as the car length, wheel base, car width increases.
# car prices increases as the curbweight, engine size, bore ratio, horsepower.

## Negitive Linear relationship : 
# car prices decreases as the car mileage for city and highway increases.

############## Bivariate Analysis
# Lets see the correlation between the continuous variables.

cor.CarPrice <- carPrice %>%
  dplyr::select(
    wheelbase,
    carlength,
    carwidth,
    carheight,
    curbweight,
    enginesize,
    boreratio,
    stroke,
    compressionratio,
    horsepower,
    peakrpm,
    citympg,
    highwaympg,
    price
  )

cormat <- round(cor(cor.CarPrice,use = "complete.obs"),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

melted_cormat %>%
  filter(value >= 0.8 & Var1 != Var2)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  geom_text(aes(Var2, Var1, label = value), color = "white", size = 3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Summary
# 1:  Price: High positive correlation with enginesize, curbweight, horsepower,carwidth, carlength
#            High negative correlation with city and highway mileage.

# 2:  curbweight, wheelbase, carwidth, enginesize, horsepower and boreratio are highly correlated
#     Multicollinearity might be an issue with above while creating the model

# 3: Overall Mileage (City and highway) has a negative correlation with car dimensions and weight aesthetics.

# 4: Stroke has a slight correlation with enginesize.

# Summary: Findings from correlation matrix confirms the findings from univariate analysis of continuous variables.


######################################################################################
############## Model Preperation
######################################################################################

##  Create dummy variables for all categorical variables
carPrice.model <- carPrice

# Remove car_ID as not needed in modelling
carPrice.model <- subset(carPrice.model, select = -c(car_ID))

# 1: symboling 
levels(carPrice.model$symboling)
dummy_1 <- data.frame(model.matrix( ~symboling, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(symboling)), dummy_1)

# 2: fueltype 
levels(carPrice.model$fueltype)
# only two levels hence
levels(carPrice.model$fueltype)<-c(1,0)
carPrice.model$fueltype <- as.numeric(levels(carPrice.model$fueltype))[carPrice.model$fueltype]
unique(carPrice.model$fueltype)

# 3: aspiration 
levels(carPrice.model$aspiration)
# only two levels hence
levels(carPrice.model$aspiration)<-c(1,0)
carPrice.model$aspiration <- as.numeric(levels(carPrice.model$aspiration))[carPrice.model$aspiration]
unique(carPrice.model$aspiration)


# 4: doornumber 
levels(carPrice.model$doornumber)
# only two levels hence
levels(carPrice.model$doornumber)<-c(1,0)
carPrice.model$doornumber <- as.numeric(levels(carPrice.model$doornumber))[carPrice.model$doornumber]
unique(carPrice.model$doornumber)

# 5: carbody 
levels(carPrice.model$carbody)
dummy_1 <- data.frame(model.matrix( ~carbody, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(carbody)), dummy_1)

# 6: drivewheel 
levels(carPrice.model$drivewheel)
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(drivewheel)), dummy_1)

# 7: enginelocation 
levels(carPrice.model$enginelocation)
# only two levels hence
levels(carPrice.model$enginelocation)<-c(1,0)
carPrice.model$enginelocation <- as.numeric(levels(carPrice.model$enginelocation))[carPrice.model$enginelocation]
unique(carPrice.model$enginelocation)

# 8: enginetype 
levels(carPrice.model$enginetype)
dummy_1 <- data.frame(model.matrix( ~enginetype, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(enginetype)), dummy_1)

# 9: cylindernumber 
levels(carPrice.model$cylindernumber)
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(cylindernumber)), dummy_1)

# 10: fuelsystem 
levels(carPrice.model$fuelsystem)
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(fuelsystem)), dummy_1)

# 11: CarCompany 
levels(carPrice.model$CarCompany)
dummy_1 <- data.frame(model.matrix( ~CarCompany, data = carPrice.model))
dummy_1 <- dummy_1[,-1]
carPrice.model <- cbind(subset(carPrice.model, select = -c(CarCompany)), dummy_1)

## Verify that all columns must be num or int
str(carPrice.model)

## separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carPrice.model), 0.7*nrow(carPrice.model))
carPrice.train = carPrice.model[trainindices,]
carPrice.test = carPrice.model[-trainindices,]

######################################################################################
############## Model Building
######################################################################################

## Build model 1 containing all variables
model_1 <-lm(price~.,data=carPrice.train)
summary(model_1)

## Lets use StepAIC to eliminate insignificant columns before going forward
step <- stepAIC(model_1, direction="both")

# Getting the final model achieved from stepAIC
step

# Running model_2 as obtained from above
model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                enginetypeohc + symboling.1, data = carPrice.train)

summary(model_2)

# Checking for multicollinearrity
vif(model_2)
model.vif <- data.frame(vif(model_2))

# Analysis model_2
# Looking at variables which are most insignifican p > 0.05 ("." or no dots) and high VIF > 2
# "carbodysedan" is the most insignificant and has a high VIF and hence can be removed
# Multiple R-squared:  0.9759,	Adjusted R-squared:  0.9681

# Running model_3 removing carbodysedan
model_3 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                enginetypeohc + symboling.1, data = carPrice.train)

summary(model_3)

# Checking for multicollinearrity
vif(model_3)
model.vif <- data.frame(vif(model_3))

# Analysis model_3
# Looking at variables which are most insignifican p > 0.05 ("." or no dots) and high VIF > 2
# "wheelbase" has a high VIF and is also insignificant.
# Multiple R-squared:  0.9751,	Adjusted R-squared:  0.9672

# Running model_4 removing wheelbase
model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                enginetypeohc + symboling.1, data = carPrice.train)

summary(model_4)

# Checking for multicollinearrity
vif(model_4)
model.vif <- data.frame(vif(model_4))

# Analysis model_4
# Looking at variables which are most insignifican p > 0.05 ("." or no dots) and high VIF > 2
# "enginetypeohc" has a high VIF and is also insignificant.
# Multiple R-squared:  0.9744,	Adjusted R-squared:  0.9666

# Running model_5 removing enginetypeohc
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + CarCompanyhonda + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                symboling.1, data = carPrice.train)

summary(model_5)

# Checking for multicollinearrity
vif(model_5)
model.vif <- data.frame(vif(model_5))

# Analysis model_5
# Looking at variables which are most insignifican p > 0.05 ("." or no dots) and high VIF > 2
# Removing the next most insignificant variable "CarCompanyhonda" with high VIF
# Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9659

# Running model_6 removing CarCompanyhonda
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                symboling.1, data = carPrice.train)

summary(model_6)

# Checking for multicollinearrity
vif(model_6)
model.vif <- data.frame(vif(model_6))

# Analysis model_6
# All variables with high VIF are now significant (p < 0.05)
# Removing the most insignificant variables now: "symboling.1"
# Multiple R-squared:  0.9728,	Adjusted R-squared:  0.9652

# Running model_7 removing symboling.1
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen
                , data = carPrice.train)

summary(model_7)

# Checking for multicollinearrity
vif(model_7)
model.vif <- data.frame(vif(model_7))

# Analysis model_7
# All variables with high VIF are now significant (p < 0.05)
# Removing the most insignificant variables now: "carbodyhardtop"
# Multiple R-squared:  0.9728,	Adjusted R-squared:  0.9655

# Running model_8 removing carbodyhardtop
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + carbodyhatchback + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen
              , data = carPrice.train)

summary(model_8)

# Checking for multicollinearrity
vif(model_8)
model.vif <- data.frame(vif(model_8))

# Analysis model_8
# All variables with high VIF are now significant (p < 0.05)
# Removing the most insignificant variables now: "carbodyhatchback"
# Multiple R-squared:  0.9727,	Adjusted R-squared:  0.9657

# Running model_9 removing carbodyhatchback
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + horsepower + 
                symboling1 + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohcf + 
                cylindernumberfour + cylindernumberfive + cylindernumbersix + 
                CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen
              , data = carPrice.train)

summary(model_9)

# Checking for multicollinearrity
vif(model_9)
model.vif <- data.frame(vif(model_9))

# Analysis model_9
# Removing the most insignificant variables now: "aspiration" VIF > 2
# Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9655

# Running model_10 removing aspiration
model_10 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + symboling1 + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
                 CarCompanytoyota + CarCompanyvolkswagen , data = carPrice.train)

summary(model_10)

# Checking for multicollinearrity
vif(model_10)
model.vif <- data.frame(vif(model_10))

# Analysis model_10
# "CarCompanyrenault", "CarCompanyvolkswagen" least significant
# with "CarCompanyvolkswagen" is the most insignificant
# Multiple R-squared:  0.9713,	Adjusted R-squared:  0.9646

# Running model_11 removing CarCompanyvolkswagen
model_11 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + symboling1 + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanyplymouth + CarCompanyrenault + CarCompanysaab + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_11)

# Checking for multicollinearrity
vif(model_11)
model.vif <- data.frame(vif(model_11))

# Analysis model_11
# "CarCompanyrenault" is the most insignificant hence removing it
# Multiple R-squared:  0.9706,	Adjusted R-squared:  0.9641

# Running model_12 removing CarCompanyrenault
model_12 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + symboling1 + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanynissan + CarCompanyplymouth + CarCompanysaab + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_12)

# Checking for multicollinearrity
vif(model_12)
model.vif <- data.frame(vif(model_12))

# Analysis model_12
# "CarCompanynissan" is the most insignificant hence removing it
# Multiple R-squared:  0.9701,	Adjusted R-squared:  0.9637

# Running model_13 removing CarCompanynissan
model_13 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + symboling1 + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanyplymouth + CarCompanysaab + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_13)

# Checking for multicollinearrity
vif(model_13)
model.vif <- data.frame(vif(model_13))

# Analysis model_13
# "symboling1" is the most insignificant
# Multiple R-squared:  0.9692,	Adjusted R-squared:  0.9629

# Running model_14 removing symboling1
model_14 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanyplymouth + CarCompanysaab + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_14)

# Checking for multicollinearrity
vif(model_14)
model.vif <- data.frame(vif(model_14))

# Analysis model_14
# "CarCompanyplymouth" has now become most insignificant hence removing it
# Multiple R-squared:  0.9683,	Adjusted R-squared:  0.9621

# Running model_15 removing CarCompanyplymouth
model_15 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanysaab + CarCompanytoyota , data = carPrice.train)

summary(model_15)

# Checking for multicollinearrity
vif(model_15)
model.vif <- data.frame(vif(model_15))

# Analysis model_15
# "CarCompanydodge" has now become most insignificant hence removing it
# Multiple R-squared:  0.9674,	Adjusted R-squared:  0.9615

# Running model_16 removing CarCompanydodge
model_16 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymercury + CarCompanymitsubishi + 
                 CarCompanysaab + CarCompanytoyota , data = carPrice.train)

summary(model_16)

# Checking for multicollinearrity
vif(model_16)
model.vif <- data.frame(vif(model_16))

# Analysis model_16
# "CarCompanymercury" has now become most insignificant hence removing it
# Multiple R-squared:  0.9665,	Adjusted R-squared:  0.9607

# Running model_17 removing CarCompanymercury
model_17 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanysaab + CarCompanytoyota , data = carPrice.train)

summary(model_17)

# Checking for multicollinearrity
vif(model_17)
model.vif <- data.frame(vif(model_17))

# Analysis model_17
# "CarCompanysaab" has now become most insignificant hence removing it
# Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9673

# Running model_18 removing CarCompanysaab
model_18 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypel + enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_18)

# Checking for multicollinearrity
vif(model_18)
model.vif <- data.frame(vif(model_18))

# Analysis model_18
# "enginetypel" has now become most insignificant hence removing it
# Multiple R-squared:  0.9649,	Adjusted R-squared:  0.9595

# Running model_19 removing enginetypel
model_19 <- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_19)

# Checking for multicollinearrity
vif(model_19)
model.vif <- data.frame(vif(model_19))

# Analysis model_19
# All variables are now significant. Removing the variables with high VIF and least significance (1 star)
# "enginesize" has the highest VIF with 1 star
# Multiple R-squared:  0.9639,	Adjusted R-squared:  0.9587

# Running model_20 removing enginesize
model_20 <- lm(formula = price ~ enginelocation + carwidth + curbweight + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi + 
                 CarCompanytoyota , data = carPrice.train)

summary(model_20)

# Checking for multicollinearrity
vif(model_20)
model.vif <- data.frame(vif(model_20))

# Analysis model_20
# All variables with high VIF have 3 stars now
# Looking at variables which have highest p-value : "CarCompanytoyota" 
# Multiple R-squared:  0.9622,	Adjusted R-squared:  0.957

# Running model_21 removing CarCompanytoyota
model_21 <- lm(formula = price ~ enginelocation + carwidth + curbweight + 
                 stroke + horsepower + carbodywagon + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi
                 , data = carPrice.train)

summary(model_21)

# Checking for multicollinearrity
vif(model_21)
model.vif <- data.frame(vif(model_21))

# Analysis model_21
# All variables with high VIF have 3 stars now
# Looking at variables which have highest p-value : "carbodywagon" 
# Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9559

# Running model_22 removing carbodywagon
model_22 <- lm(formula = price ~ enginelocation + carwidth + curbweight + 
                 stroke + horsepower + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymazda + CarCompanymitsubishi
               , data = carPrice.train)

summary(model_22)

# Checking for multicollinearrity
vif(model_22)
model.vif <- data.frame(vif(model_22))

# Analysis model_22
# All variables with high VIF have 3 stars now
# Looking at variables which have highest p-value : "CarCompanymazda" 
# Multiple R-squared:  0.9591,	Adjusted R-squared:  0.9543

# Running model_23 removing CarCompanymazda
model_23 <- lm(formula = price ~ enginelocation + carwidth + curbweight + 
                 stroke + horsepower + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar + CarCompanymitsubishi , data = carPrice.train)

summary(model_23)

# Checking for multicollinearrity
vif(model_23)
model.vif <- data.frame(vif(model_23))

# Analysis model_23
# All variables with high VIF have 3 stars now
# Looking at variables which have highest p-value : "CarCompanymitsubishi" 
# Multiple R-squared:  0.9572,	Adjusted R-squared:  0.9526

# Running model_24 removing CarCompanymitsubishi
model_24 <- lm(formula = price ~ enginelocation + carwidth + curbweight + 
                 stroke + horsepower + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar , data = carPrice.train)

summary(model_24)

# Checking for multicollinearrity
vif(model_24)
model.vif <- data.frame(vif(model_24))

# Analysis model_24
# All variables are now extremely significant - 3 stars
# Looking at values with very high VIF : curbweight
# We know from our analysis that curbweight is highly correlated to carwidth (0.87)
# Multiple R-squared:  0.947,	Adjusted R-squared:  0.9416

# Running model_25 removing curbweight
model_25 <- lm(formula = price ~ enginelocation + carwidth + 
                 stroke + horsepower + drivewheelrwd + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar , data = carPrice.train)

summary(model_25)

# Checking for multicollinearrity
vif(model_25)
model.vif <- data.frame(vif(model_25))

# Analysis model_25
# "drivewheelrwd" has now become insignificant
# Multiple R-squared:  0.9481,	Adjusted R-squared:  0.9434 (Adjusted R-squared has slightly increased)

# Running model_26 removing drivewheelrwd
model_26 <- lm(formula = price ~ enginelocation + carwidth + 
                 stroke + horsepower + 
                 enginetypeohcf + cylindernumberfour + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar , data = carPrice.train)

summary(model_26)

# Checking for multicollinearrity
vif(model_26)
model.vif <- data.frame(vif(model_26))

# Analysis model_26
# Looking at variables which have high VIF: "cylindernumberfour"
# Multiple R-squared:  0.947,	Adjusted R-squared:  0.9426

# Running model_27 removing cylindernumberfour
model_27 <- lm(formula = price ~ enginelocation + carwidth + 
                 stroke + horsepower + enginetypeohcf + cylindernumberfive + 
                 cylindernumbersix + CarCompanybmw + CarCompanybuick + 
                 CarCompanyjaguar , data = carPrice.train)

summary(model_27)

# Checking for multicollinearrity
vif(model_27)
model.vif <- data.frame(vif(model_27))

# Analysis model_27
# "cylindernumberfive" and "cylindernumbersix" have both become insignificant
# Multiple R-squared:  0.9393,	Adjusted R-squared:  0.9347 (Adjusted R-squared has slightly increased)

# Running model_28 removing cylindernumbersix
model_28 <- lm(formula = price ~ enginelocation + carwidth + 
                 stroke + horsepower + enginetypeohcf + cylindernumberfive + 
                 CarCompanybmw + CarCompanybuick + CarCompanyjaguar , 
               data = carPrice.train)

summary(model_28)

# Checking for multicollinearrity
vif(model_28)
model.vif <- data.frame(vif(model_28))

# Analysis model_28
# "cylindernumberfive" is insignificant hence removing it
# Multiple R-squared:  0.9391,	Adjusted R-squared:  0.935 (Adjusted R-squared has slightly increased)

# Running model_29 removing cylindernumberfive
model_29 <- lm(formula = price ~ enginelocation + carwidth + 
                 stroke + horsepower + enginetypeohcf + 
                 CarCompanybmw + CarCompanybuick + CarCompanyjaguar , 
               data = carPrice.train)

summary(model_29)

# Checking for multicollinearrity
vif(model_29)
model.vif <- data.frame(vif(model_29))

# Analysis model_29
# All variables are now highly significant 
# VIF is also < 3 which is acceptable considering our model is able to explain 93.5% data.
# Lets try removing "carwidth" which has the heighest VIF
# Multiple R-squared:  0.9386,	Adjusted R-squared:  0.935 

## We have 3 variables with high VIF lets remove them one by one and see the affect on model
# 1: enginesize
# 2: horsepower
# 3: enginetypeohcf

##########

# Running model_30 removing carwidth
model_30_1 <- lm(formula = price ~ enginelocation + 
                 stroke + horsepower + enginetypeohcf + 
                 CarCompanybmw + CarCompanybuick + CarCompanyjaguar , 
               data = carPrice.train)

summary(model_30_1)

# Checking for multicollinearrity
vif(model_30_1)
model.vif <- data.frame(vif(model_30_1))

# Analysis model_30_1
# Adjusted R-squared has dipped more than 5 points which is not correct hence our assumption to remove "enginesize" is wrong
# Multiple R-squared:  0.8945,	Adjusted R-squared:  0.8891

# Running model_30 removing horsepower
model_30_2 <- lm(formula = price ~ enginelocation + carwidth + 
                   stroke + enginetypeohcf + 
                   CarCompanybmw + CarCompanybuick + CarCompanyjaguar , 
                 data = carPrice.train)

summary(model_30_2)

# Checking for multicollinearrity
vif(model_30_2)
model.vif <- data.frame(vif(model_30_2))

# Analysis model_30_2
# Adjusted R-squared has dipped more than 5 points which is not correct hence our assumption to remove "horsepower" is also wrong
# Multiple R-squared:  0.8895,	Adjusted R-squared:  0.8838

# Running model_30 removing enginetypeohcf
model_30_3 <- lm(formula = price ~ enginelocation + carwidth + 
                   stroke + horsepower +  
                   CarCompanybmw + CarCompanybuick + CarCompanyjaguar , 
                 data = carPrice.train)

summary(model_30_3)

# Checking for multicollinearrity
vif(model_30_3)
model.vif <- data.frame(vif(model_30_3))

# Analysis model_30_3
# Adjusted R-squared has not hence  our assumption to remove "horsepower" is correct.
# Multiple R-squared:  0.9333,	Adjusted R-squared:  0.9298

# model_30_3 is the correct model to go further
# "stroke" however has dipped in significance we can try removing it first

##########

# Running model_31 removing stroke
model_31 <- lm(formula = price ~ enginelocation + carwidth + 
                 horsepower + CarCompanybmw + CarCompanybuick + CarCompanyjaguar , 
               data = carPrice.train)

summary(model_31)

# Checking for multicollinearrity
vif(model_31)
model.vif <- data.frame(vif(model_31))

# Analysis model_31
# VIF is also < 3 which is acceptable considering our model is able to explain 92.5% data.
# All variables have a VIF < 2. No collinearity exists.
# Multiple R-squared:  0.9287,	Adjusted R-squared:  0.9256

# We cannot remove the variables further as this the best fit for model

model_final <- model_31

######################################################################################
############## Model testing and Prediction analysis
######################################################################################

Predict_1 <- predict(model_final,carPrice.test[,-1])
carPrice.test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(carPrice.test$price,carPrice.test$test_price)
rsquared <- cor(carPrice.test$price,carPrice.test$test_price)^2
rsquared

# R-squared value achieved on test model is .87 which is good

# Plotting the residuals
par(mfrow = c(2, 2))
plot(model_final)

# Fitting predicted price on actual car price
par(mfrow = c(1, 1))
plot(carPrice.test$price,col = "green", type = "l" )
lines(carPrice.test$test_price,col = "red", type = "l")
