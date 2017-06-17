setwd("/Documents/9.UBC/stat306/R- project/")

# need to place the data file in the working folder - currently Documents
dt<-read.csv("data_train_competition.csv", header=TRUE)

# Steps 1 and 2 


dt$revenue_class <- 1

# get date drom time stamp
dt$date <- as.POSIXct(as.numeric(as.character(dt$starting_timestamp)),origin="1970-01-01",tz="EET")

# extract hour from the date
dt$hour <- as.POSIXlt(dt$date)$hour
dt$exactdate <- as.Date(dt$date)
dt$weekday <- weekdays(as.Date(dt$date))


table(dt$hour, dt$weekday)
table(dt$hour, dt$holiday)

#rgdal and sp

library(sp)
library(rgeos)
library(rgdal)
LongLat <- data.frame(dt$starting_longitude, dt$starting_latitude)
#x longitude, y latitude
names(LongLat) <- c("x","y")

# convert to SP object
coordinates(LongLat)<-~x+y

# Add a coordinate reference system
# World Geodetic System 1984
proj4string(LongLat) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# zone = 34
UTM <- spTransform(LongLat, CRS("+proj=utm +zone=34  +north ellps=WGS84"))

air_x = 667454.2812 
air_y = 4487818.8696
down_x = 663264.5608 
down_y = 4500987.8518

dist_air_km <- sqrt((UTM$x-air_x)^2+(UTM$y-air_y)^2)/1000
dist_down_km <- sqrt((UTM$x-down_x)^2+(UTM$y-down_y)^2)/1000

dt$dist_air_km <- dist_air_km
dt$dist_down_km <- dist_down_km

summary(dt$dist_down_km)

summary(dt$dist_air_km)

#categories for Distance

dt$radius_down_km <- "0-1"
dt[1 <= dt$dist_down_km & dt$dist_down_km < 2, "radius_down_km"] <- "1-2"
dt[3 <= dt$dist_down_km & dt$dist_down_km < 6, "radius_down_km"] <- "3-6"
dt[4 <= dt$dist_down_km & dt$dist_down_km < 5, "radius_down_km"] <- "4-5"
dt[5 <= dt$dist_down_km & dt$dist_down_km < 6, "radius_down_km"] <- "5-6"
dt[6 <= dt$dist_down_km, "radius_down_km"] <- "6+"

dt$dist_air_km <- NULL
dt$dist_down_km <- NULL

# public holidays

holiday <- read.table("holidays.txt", header = TRUE)
c<-holiday[,1]

dt$holiday <- 0
dt[(as.numeric(as.Date(dt$date)) %in% (as.numeric(as.Date(c)))), "holiday"] <- 1


# importing weather data and merge with original one   
weather <- read.csv("weather.txt", header=TRUE)   
dt$numDate <- as.numeric(as.Date(dt$date))
weather$numEEST <- as.numeric(as.Date(weather$EEST))
dmt <- merge(dt, weather, by.x = "numDate", by.y = "numEEST")
#clean up the data

#delete useless vars
dmt$EEST <- NULL
dmt$Max.TemperatureC <- NULL
dmt$Mean.TemperatureC <-NULL
dmt$Min.TemperatureC <-NULL
dmt$Dew.PointC <-NULL
dmt$MeanDew.PointC <-NULL
dmt$Min.DewpointC <-NULL
dmt$Mean.Humidity <-NULL
dmt$Max.Humidity <-NULL
dmt$Min.Humidity <-NULL
dmt$Max.Sea.Level.PressurehPa <-NULL
dmt$Mean.Sea.Level.PressurehPa <-NULL
dmt$Min.Sea.Level.PressurehPa <-NULL
dmt$Max.VisibilityKm <-NULL
dmt$Mean.VisibilityKm <-NULL
dmt$Min.VisibilitykM <-NULL
dmt$Max.Gust.SpeedKm.h <-NULL
dmt$Precipitationmm <-NULL
dmt$CloudCover <-NULL
dmt$Precipitationmm <-NULL
dmt$WindDirDegrees <-NULL
dmt$Max.Wind.SpeedKm.h <- NULL
dmt$Mean.Wind.SpeedKm.h <- NULL

dmt$ID <- NULL
dmt$taxi_id <- NULL

dmt$starting_longitude <- NULL
dmt$starting_latitude <- NULL
dmt$numDate <- NULL
dmt$date <- NULL
dmt$starting_timestamp <- NULL


# combine all weather events into one
library(stringi)
# 
# dmt$weather <- "good"
# dmt[dmt$Events ==  "Rain", "weather"] <- "bad"
# dmt[dmt$Events ==  "Snow", "weather"] <- "bad"
# dmt[dmt$Events ==  "Rain-Thunderstorm", "weather"] <- "bad"
# dmt[dmt$Events ==  "Rain-Snow", "weather"] <- "bad"
# dmt[dmt$Events ==  "Fog-Rain", "weather"] <- "bad"
# #dmt[dmt$Events ==  "Fog", "weather"] <- "Fog"


dmt$Events <- NULL



# factorize



# data aggregation: compute the total number of transactions for two revenue categories (low, high) 
# in each one hour interval. This will be the new response variable, representing two types of taxi demand

dmt$count <- 1
dmtagg <- aggregate(count ~ exactdate + hour + radius_down_km , dmt, FUN = sum)
dmtagg$logcount <- log(dmtagg$count)

dmtagg$weekday <- weekdays(as.Date(dmtagg$exactdate))

holiday <- read.table("holidays.txt", header = TRUE)
c<-holiday[,1]
dmtagg$holiday <- 0
dmtagg[(as.numeric(as.Date(dmtagg$exactdate)) %in% (as.numeric(as.Date(c)))), "holiday"] <- 1

dmtagg$hour <- as.factor(dmtagg$hour)
dmtagg$weekday <- as.factor(dmtagg$weekday)
dmtagg$weekday<-factor(dmtagg$weekday, c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))
dmtagg$holiday <- as.factor(dmtagg$holiday)
# dmtagg$weather <- as.factor(dmtagg$weather)
dmtagg$radius_down_km <- as.factor(dmtagg$radius_down_km)

# Step 3 - split into training and holdout sets

ntot<-nrow(dmtagg)

set.seed(1234)
n<-10000

iperm<-sample(ntot,ntot) # random permutation of 1...ntot
train<-dmtagg[iperm[1:n],]
hold<-dmtagg[iperm[(n+1):ntot],]

options(digits=4)

# Step 4: correlations and side-by-side boxplots and tabular summaries


# nine box plots
boxplot(exp(dmtagg$count),ylab="Count")
par(mfrow=c(4,1))


par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(dmtagg$weekday, dmtagg$count, ylab="Count",xlab="Weekday")
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(dmtagg$hour, dmtagg$count, ylab="Count",xlab="Hour")
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(dmtagg$holiday, dmtagg$count, ylab="Count", xlab = "Holiday")
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(dmtagg$radius_down_km, dmtagg$count, ylab="Count", xlab="Range from Downtown in km")
dev.off()

par(mfrow=c(4,1))


dmtagg$sqrtcount <- sqrt(dmtagg$count)
par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(dmtagg$weekday, dmtagg$sqrtcount, ylab="Sqrt of Count",xlab="Weekday")

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(dmtagg$hour, dmtagg$sqrtcount, ylab="Sqrt of Count",xlab="Hour")

par(mar=c(4.5, 4.5, 0.5, 0.5))

plot(dmtagg$holiday, dmtagg$sqrtcount, ylab="Sqrt of Count", xlab = "Holiday")
par(mar=c(4.5, 4.5, 0.5, 0.5))

plot(dmtagg$radius_down_km, dmtagg$sqrtcount, ylab="Sqrt of Count", xlab="Range from Downtown in km")
# Step 5: lm


model <- lm(logcount~weekday+hour+radius_down_km+holiday, train)
predhold1 <- predict(model,newdata = hold)
predtrain1 <- predict(model)

model2 <- lm(logcount~(weekday+hour+holiday)^2+radius_down_km, train)
predhold2 <- predict(model2,newdata = hold)
predtrain2 <- predict(model2)

model3 <- lm(logcount~(weekday+hour+holiday)^2+radius_down_km+I(hour:radius_down_km), train)
predhold3 <- predict(model3,newdata = hold)
predtrain3 <- predict(model3)

model4 <- lm(logcount~(weekday+hour+holiday)^2+radius_down_km+I(hour:radius_down_km)+I(weekday:radius_down_km), train)
predhold4 <- predict(model4,newdata = hold)
predtrain4 <- predict(model4)


model5 <- lm(logcount~(weekday+hour+holiday+radius_down_km)^2, train)
predhold5 <- predict(model5,newdata = hold)
predtrain5 <- predict(model5)

m1 <- summary(model)
m2 <- summary(model2)
m3 <- summary(model3)
m4 <- summary(model4)
m5 <- summary(model5)


MSEhold1 <- sqrt(mean((predhold1-hold$logcount)^2))
MSEhold2 <- sqrt(mean((predhold2-hold$logcount)^2))
MSEhold3 <- sqrt(mean((predhold3-hold$logcount)^2))
MSEhold4 <- sqrt(mean((predhold4-hold$logcount)^2))
MSEhold5 <- sqrt(mean((predhold5-hold$logcount)^2))

MSEtrain1 <- sqrt(mean((predtrain1-train$logcount)^2))
MSEtrain2 <- sqrt(mean((predtrain2-train$logcount)^2))
MSEtrain3 <- sqrt(mean((predtrain3-train$logcount)^2))
MSEtrain4 <- sqrt(mean((predtrain4-train$logcount)^2))
MSEtrain5 <- sqrt(mean((predtrain5-train$logcount)^2))

result <- c(model$rank,model2$rank,model3$rank,model4$rank,model5$rank,
            m1$adj.r.squared,m2$adj.r.squared,m3$adj.r.squared,m4$adj.r.squared,m5$adj.r.squared,
            AIC(model),AIC(model2), AIC(model3),AIC(model4),AIC(model5),
            MSEtrain1,MSEtrain2,MSEtrain3,MSEtrain4,MSEtrain5,
            MSEhold1,MSEhold2,MSEhold3,MSEhold4,MSEhold5)

x1 <- matrix(data = result,nrow = 5,ncol = 5)

pred = predict(model3)
res <- model3$residuals
rests <- ts(res)
sigma <- m3$sigma

#step6: residual analysis
summary(res)

par(mfrow=c(2,1))
plot(res,ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Residuals plot",cex.main=1)

plot(res[1000:2000],ylab="residual[1000:2000]")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Parts of Residuals",cex.main=1)

#residual plots
par(mfrow=c(3,2)) # 3x2 grid of subplots

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(pred,res,xlab="predicted value",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0); abline(h=-10*sigma)
title("Residuals VS Predicted Value",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$hour,res,xlab="Hour",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Residuals VS Hour",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$weekday,res,xlab="Weekday",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Residuals VS Weekday",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$holiday,res,xlab="Holiday",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Residuals VS Holiday",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$radius_down_km,res,xlab="Radius_down_km",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Residuals VS Radius_down_km",cex.main=0.8)
dev.off()

#stu.res residuals plots
r = rstudent(model3)

par(mfrow=c(3,2)) # 3x2 grid of subplots

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(pred,r,xlab="predicted value",ylab="studentized residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0); abline(h=-10*sigma)
title("Studentized Residuals VS Predicted Value",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$hour,r,xlab="Hour",ylab="studentized residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Studentized Residuals VS Hour",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$weekday,r,xlab="Weekday",ylab="studentized residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Studentized Residuals VS Weekday",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$holiday,r,xlab="Holiday",ylab="studentized residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Studentized Residuals VS Holiday",cex.main=0.8)

par(mar=c(4.5, 4.5, 0.5, 0.5))
plot(train$radius_down_km,r,xlab="Radius_down_km",ylab="studentized residual")
abline(h=2*sigma); abline(h=-2*sigma); abline(h=0)
title("Studentized Residuals VS Radius_down_km",cex.main=0.8)

#cook distance plot
which(grepl(-1.4078,res))   #most deviation potin at trainset row 5697
r[5697] 
prednew = predict(model3,train[5697,])

cook = cooks.distance(model5)
plot(cook,ylab="Cooks distances")
points(5697,cook[5697],col='red')

#outlier
summary(res)
sort(res)[2:length(res)]  #also find out largest res in dmtagg[565,]

new_xvec= dmtagg[-565,]

set.seed(1234)
new_iperm<-sample(ntot-1,ntot-1) # random permutation of 1...ntot-1
no_outlier_train<-new_xvec[new_iperm[1:n],]

model_no_outlier = lm(logcount~(weekday+hour+holiday)^2+radius_down_km+I(hour:radius_down_km), no_outlier_train)
summary(model_no_outlier)
summary(model_no_outlier$residuals)

#QQ and histogram plots
par(mfrow=c(1,2)) # 1x2 grid of subplots 

par(mar=c(4.5, 4.5, 0.5, 0.5))
qqnorm(res,main="normal Q-Q plot of residuals")
qqline(res)
hist(res)

par(mfrow=c(2,1))
acf(rests)
pacf(rests)




print(x1)
