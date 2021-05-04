cars_data = read.csv("cars.csv")
##Examining Data##
dim(cars_data) # Number of observations and columns
head(cars_data) # Take a quick preview of the data
names(cars_data) # Variable/column names 
#Handling Missing Values#
summary(cars_data$state)
cars_data$state = as.factor(cars_data$state)
summary(cars_data$state)

cars_data$state[cars_data$state=="new"] = NA # Not including any new cars 
cars_data$state[cars_data$state=="emergency"] = NA # Not including any damaged cars 

missing = is.na(cars_data)
sum(missing)

for (i in names(cars_data)) {
  print(paste(i,":",sum(is.na(cars_data[i])),sep=" "))
} #To see which variables have missing values 

cars_data = na.omit(cars_data) # Delete missing values 
sum(is.na(cars_data)) # Count missing again
####################
##Cleaning Up Data## 
cars_data$age = 2019 - cars_data$year_produced #Creating new value
features = c("manufacturer_name","transmission","color","odometer_value",
             "age", "engine_fuel","engine_has_gas",
             "engine_capacity","body_type","has_warranty", "price_usd", "drivetrain")   
# Variables we want to focus on
cars_data = cars_data[features] # Creates data-set with chosen variables 
dim(cars_data)
str(cars_data) # Get information about variables

cars_data$manufacturer_name = as.factor(cars_data$manufacturer_name)
cars_data$age = as.integer(cars_data$age)
cars_data$body_type = as.factor(cars_data$body_type)
cars_data$transmission = as.factor(cars_data$transmission)
cars_data$drivetrain = as.factor(cars_data$drivetrain)
cars_data$color = as.factor(cars_data$color)
cars_data$engine_fuel = as.factor(cars_data$engine_fuel)
cars_data$has_warranty = as.logical(cars_data$has_warranty)
cars_data$engine_has_gas = as.logical(cars_data$engine_has_gas)

str(cars_data) # Get information of variables with data type changes

levels(cars_data$engine_fuel) #See levels of factor types 
levels(cars_data$body_type)
levels(cars_data$color)
#Recoding Values##
manufacturers = c("Acura","Alfa Romeo","Audi","BMW","Buick","Cadillac","Chery",
                  "Chevrolet","Chrysler","Citroen","Dacia","Daewoo","Dodge",
                  "Fiat","Ford","Geely","Great Wall","Honda","Hyundai","Infiniti",
                  "Iveco","Jaguar","Jeep","Kia","LADA","Lancia","Land Rover",
                  "Lexus","Lifan","Lincoln","Mazda","Mercedes-Benz","Mini",
                  "Mitsubishi","Nissan","Opel","Peugeot","Pontiac","Porsche",
                  "Renault","Rover","Saab","Seat","Skoda","SsangYong","Subaru",
                  "Suzuki","Toyota","Volkswagen","Volvo")         
cars_data <- droplevels(cars_data[cars_data$manufacturer_name %in% manufacturers,])
cars_data$manufacturer_name
cars_data$engine_fuel = as.character(cars_data$engine_fuel) 
cars_data$engine_fuel[cars_data$engine_fuel=="gas"] = "gasoline" 
cars_data$engine_fuel = as.factor(cars_data$engine_fuel)
levels(cars_data$engine_fuel)
###EDA##
summary(cars_data$drivetrain)
sd(cars_data$price_usd) #Standard Deviation 
sd(cars_data$odometer_value)
sd(cars_data$age)
sd(cars_data$engine_capacity)

##############################################
##############Visuals#########################
#Continuous Univariate Variables# 
#histogram of car prices
hist(cars_data$price_usd, xlab = "Price ($)", ylab = "# of Cars", 
     main = "Histogram of Used Car Prices in USD", breaks = 100,xlim=c(0,30000))
#histogram of odometer value
hist(cars_data$odometer_value,xlab="Odometer Value (Kilometers)",ylab="# of cars",
     main="Histogram of Odometer Values",breaks=100,xlim=c(0,800000))
#histogram of car age
hist(cars_data$age,xlab="Age (Years)",ylab="# of cars",
     main="Histogram of Car Years",breaks=100,xlim=c(0,40))
#Scatterplot of Age and Price
x=cars_data$age
y=cars_data$price_usd
plot(x,y, xlab = "Age (Years)", ylab = "Price ($)", main = "Price of Used Cars by Age")
plot(x,y, xlab = "Age (Years)", ylab = "Price ($)", main = "Price of Used Cars by Age")

#histogram of engine capacity
hist(cars_data$engine_capacity,xlab="Engine Capacity (Liters)",ylab="# of cars",
     main="Histogram of Engine Capacity",breaks=100,xlim=c(0,6))
#Categorical Univariate Variables: 
plot(cars_data$manufacturer_name,main="Cars by Manufacturer Name",xlab="Manufacturer",ylab="# of cars", las=2)
plot(cars_data$transmission,main="Cars by Transmission",xlab="Transmission",ylab="# of cars", las=2)
plot(cars_data$engine_fuel,main="Cars by Engine Fuel",xlab="Engine Fuel Type",ylab="# of cars", las=2)
plot(cars_data$body_type,main="Cars by Body Type",xlab="Body Type",ylab="# of cars", las=2)
plot(cars_data$color,main="Cars by Color",xlab="Color",ylab="# of cars", las=2)
plot(cars_data$price_usd ~ cars_data$drivetrain, main="Cars by Drivetrain",xlab="Drivetrain",ylab="Price")
box_x = cars_data$color
box_y = cars_data$price_usd
plot(box_x,box_y, xlab = "Color", ylab = "Price ($)", ylim = c(0,30000), main = "Boxplot of Price by Color")