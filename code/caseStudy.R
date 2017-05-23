####### 1. DATA PREPARATION ######
# Run this if you already have an existing session
# load("sessions/flights.session")

# Read datasets
airlines <- read.csv("datasets/airlines.csv")
airports <- read.csv("datasets/airports.csv")
flights  <- read.csv("datasets/flights.csv")

# Explore datasets
head(airlines)
head(airports)
head(flights)

colnames(airlines)
colnames(airports)
colnames(flights)

sum(flights[, "CANCELLED"] == 0)

# Function to fix inconsistent airport codes
# Thanks to Scott A. Miller on Kaggle
# install.packages("dplyr")
id.to.iata <- function (x) {
  #  Step 0: Initial setup 
  #  Load the incoming vector into a data frame, so we can use dplyr functions to join things up.
  df <- data.frame(ID = as.character (x), 
                   stringsAsFactors = FALSE)
  #  Store the number of records - used to make sure joins do not introduce duplicates
  num_records = nrow (df)
  
  #  Step 1: Add the Description to the base data.
  dfAirportID <- read.csv("https://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_AIRPORT_ID",
                          colClasses = c("character", "character"), 
                          col.names = c("AirportID", "Description"))
  df <- dplyr::left_join(df, dfAirportID, by=c("ID" = "AirportID"))
  #  Step 2: Use Description to add the IATA_CODE to the base data.
  dfAirport <- read.csv ("https://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_AIRPORT",
                         colClasses = c("character", "character"), 
                         col.names = c("IATA_CODE", "Description"))
  #
  #  There are duplicated airports. To solve this problem, clear out codes discontinued before 2015.
  #  BSM was discontinued in 1999.
  #  The IATA does not use NYL for Yuma, it uses YUM. So remove NYL.
  dfAirport <- dfAirport[! (dfAirport$IATA_CODE %in% c('BSM', 'NYL')),]
  df <- dplyr::left_join(df, dfAirport, by="Description")
  
  #  Step 3: Make sure we have the same number of rows that we started with
  #          If this error is triggered, steps will need to be made to eliminate 
  #          duplicate key values.
  if (num_records != nrow (df)) {
    stop ("Due to duplicates in the data, the number of records has changed.")
  }
  
  #  Step 4: In cases where we didn't get a matching IATA_CODE, copy over the original value
  df$ID <- dplyr::coalesce (df$IATA_CODE, df$ID)
  
  #  Step 5: We are all done. Return the results. 
  return (df$ID)
}

# Fix airport codes in flights dataset using id.to.iata function
flights$ORIGIN_AIRPORT <- id.to.iata(flights$ORIGIN_AIRPORT)
flights$DESTINATION_AIRPORT <- id.to.iata(flights$DESTINATION_AIRPORT)

# Merge data frames in one
# We need to change the name of a column in airlines dataset
#  or it will cause a duplicate column and will not merge
# Need to merge with airports twice because of origin and 
#  destination airports
colnames(airlines) <- c("IATA_CODE", "AIRLINE_NAME")
flights <- merge(flights, airports, by.x = "ORIGIN_AIRPORT", by.y = "IATA_CODE")
flights <- merge(flights, airports, by.x = "DESTINATION_AIRPORT", by.y = "IATA_CODE")
flights <- merge(flights, airlines, by.x = "AIRLINE", by.y = "IATA_CODE")

head(flights)
colnames(flights)

# Remove NA values and replace by 0
# We assume that if there is no record of delay, there was no delay
flights[is.na(flights)] <- 0
head(flights)

# Select time and delay attributes from flights dataset
time.att <- c("SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME","ARRIVAL_DELAY",
              "AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY", "DEPARTURE_DELAY",
              "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")
delay.att <- time.att[4:length(time.att)]
summary(flights[ ,time.att])

# Add DELAY and DELAYED attribute to dataset
# DELAY represents the sum of all delay attributes
# DELAYED represents if the flight as been delayed or not (positive delay)
flights[, "DELAY"] <- rowSums(flights[, delay.att])
flights[, "DELAYED"] <- ifelse(flights$DELAY > 0, 0, 1)

# Save the current R session.
save.image("sessions/flights.session")

####### 2. EXPLORATIVE ANALISYS ######
# install.packages("maps")
library("maps")

# Show airports location on a map 
# Thanks to miquar on Kaggle
png("plots/map.airports.png")
map("usa")
title("Airports")
points(airports$LONGITUDE,
       airports$LATITUDE,
       col = "red",
       cex = 0.75)
dev.off()

# Number of Flights per Airline
png("plots/flights.byAirline.png",
    w = 1413,
    h = 1080)
barplot(sort(table(flights$AIRLINE)),
        las = 0,
        col = "orange",
        xlab = "Airline Code",
        ylab = "Number of Flights",
        main = "Number of Flights per Airline")
dev.off()

# Boxplot of delay metrics
png("plots/time.metrics.png",
    w = 1413,
    h = 1080)
boxplot(flights[ ,time.att],
        col = 4:length(time.att),
        main = "Time Attributes Distribution")
dev.off()

# Info of delay times
png("plots/times.info.png",
    w = 1413,
    h = 1080)
par(mfrow = c(4,3))
for (att in time.att){
  hist(flights[, att],
       main=att,
       col="orange")
}
hist(flights[, "DELAY"],
     main="DELAY",
     col="orange")
barplot(table(flights[, "DELAYED"]),
        main="DELAYED FLIGHTS",
        col="orange")
dev.off()

####### 3. KNOWLEDGE EXTRACTION OBJECTIVES ######
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
# install.packages("caret")
library(caret)
# install.packages("rminer")
library(rminer)
# install.packages("MASS")
library(MASS)
# install.packages("e1071")
library(e1071)
# install.packages("cluster")
library(cluster)
# install.packages("arules")
library(arules)
# install.packages("arulesViz")
library(arulesViz)

###### 3.1. Best time of the year to travel #####
# Calculate mean delays by month
# Get the mean of all delays in all months
months <- seq(1, 12, 1)
months.delays <- rep(0, 12)
for (month in months) {
  x <- flights[flights[, "MONTH"] == month, delay.att]
  months.delays[month] <- sum(x)/nrow(x)
}

# Plot the resulting data in a barplot
png("questions/times.monthly.png",
    w = 1413,
    h = 1080)
barplot(months.delays, 
        las = 0,
        names.arg = months,
        col = "orange",
        xlab = "Months",
        ylab = "Delay Mean",
        main = "Delays by Month")
dev.off()

###### 3.2. Best airline to travel within #####
# Get total dispatch time and delays by airline
# Get the sum of all delay times
airline.codes <- airlines$IATA_CODE
airline.times <- rep(0, length(airline.codes))
for (i in 1:length(airline.codes)) {
  x <- flights[flights[, "AIRLINE"] == airline.codes[i], delay.att]
  airline.times[i] <- sum(x)/nrow(x)
}

# Plot the resulting data in a barplot
png("questions/airlines.delays.png",
    w = 1413,
    h = 1080)
barplot(airline.times, 
        las = 0,
        names.arg = airline.codes,
        col = "orange",
        xlab = "Airline Codes",
        ylab = "Delay Mean",
        main = "Airline Mean Delay Times")
dev.off()

# Get the sum of all times
airline.times <- rep(0, length(airline.codes))
for (i in 1:length(airline.codes)) {
  x <- flights[flights[, "AIRLINE"] == airline.codes[i], time.att]
  airline.times[i] <- sum(x)/nrow(x)
}

# Plot the resulting data in a barplot
png("questions/airlines.times.png",
    w = 1413,
    h = 1080)
barplot(airline.times, 
        las = 0,
        names.arg = airline.codes,
        col = "orange",
        xlab = "Airline Codes",
        ylab = "Mean Dispatch Time",
        main = "Airline Mean Flight Dispatch Times")
dev.off()

###### 3.3. Predict if a flight will be delayed and the delay #####
# Get attributes to use for prediction
pred.att <- c("AIRLINE", "DESTINATION_AIRPORT", "ORIGIN_AIRPORT", "MONTH", "DAY", "DAY_OF_WEEK", "SCHEDULED_DEPARTURE", "SCHEDULED_TIME", "SCHEDULED_ARRIVAL")

# Take a sample of the dataset to speed up models construction 
# (20%) ~ 1M records
set.seed(123456)
sample <- sample(1:nrow(flights),
                 size = ceiling(0.2 * (nrow(flights))),
                 replace = FALSE)
flights.sample <- flights[sample, c(pred.att, "DELAY", "DELAYED")]

# Generate train and test datasets
# 2/3 train, 1/3 test
set.seed(123456)
train <- createDataPartition(flights.sample$DELAYED,
                             p = 2/3,
                             list = FALSE)
flights.train <- flights.sample[train, ]
flights.test <- flights.sample[-train, ]

##### 3.3.1. Predict if a flight will be delayed ####

#### 3.3.2.1. Regression model ####
# Build regression model
flights.regr <- lm(DELAYED ~ .,
                   data = flights.train[, c(pred.att, "DELAYED")])

# Get model predictions
flights.regr.probs <- predict(flights.regr,
                              flights.test[, pred.att])
flights.regr.pred = rep(0, 
                        nrow(flights.test))
flights.regr.pred[flights.regr.probs > 0.5] = 1

# Build and print confusion matrix
flights.regr.cm <- confusionMatrix(flights.regr.pred, 
                                   flights.test[, "DELAYED"],
                                   positive = '1')
print(flights.regr.cm)

# Plot the ROC curve
png("questions/delayed.predictions.regr.png",
    w = 1013,
    h = 720)
mgraph(flights.test[, "DELAYED"],
       flights.regr.pred,
       graph = "ROC",
       Grid = 10)
dev.off()

#### 3.3.1.2. Decision Tree ####
# Create the tree model
flights.tree = rpart(DELAYED ~ ., 
                     data = flights.train[, c(pred.att, "DELAYED")],
                     method = "anova")

# Plot the tree
png("questions/delayed.tree.png",
    w = 1013,
    h = 720)
rpart.plot(flights.tree)
dev.off()

# Get tree predictions
flights.tree.probs <- predict(flights.tree, 
                              flights.test[, pred.att])
flights.tree.pred = rep(0, 
                        nrow(flights.test))
flights.tree.pred[flights.tree.probs > 0.6] = 1

# Build and print confusion matrix
flights.tree.cm <- confusionMatrix(flights.tree.pred, 
                                   flights.test[, "DELAYED"],
                                   positive = '1')
print(flights.tree.cm)

# Plot the ROC curve
png("questions/delayed.predictions.tree.png",
    w = 1013,
    h = 720)
mgraph(flights.test[, "DELAYED"],
       flights.tree.pred,
       graph = "ROC",
       Grid = 10)
dev.off()

#### 3.3.1.3. Naive Bayes ####
# Convert all the categorical features to factor
flights.train[sapply(flights.train, is.character)] <- lapply(flights.train[sapply(flights.train, is.character)], as.factor)
flights.test[sapply(flights.test, is.character)] <- lapply(flights.test[sapply(flights.test, is.character)], as.factor)

str(flights.train)
str(flights.test)

# Create the probabilistic model
# DELAYED needs to be converted to factor because of the Naive Bayes definition
# "Computes the conditional a-posterior probabilities of a **categorical** class variable 
#   given independent predictor variables using the Bayes rule."
flights.nb = naiveBayes(as.factor(DELAYED) ~ .,
                        data = flights.train[, c(pred.att, "DELAYED")])

# Get the probabilistic predictions
flights.nb.pred <- predict(flights.nb, 
                           flights.test[, pred.att])

# Build and print confusion matrix
flights.nb.cm <- confusionMatrix(flights.nb.pred, 
                                 as.factor(flights.test[, "DELAYED"]),
                                 positive = '1')
print(flights.nb.cm)

# Plot the ROC curve
png("questions/delayed.predictions.nb.png",
    w = 1013,
    h = 720)
mgraph(as.numeric(flights.test[, "DELAYED"]),
       as.numeric(flights.nb.pred),
       graph = "ROC",
       Grid = 10)
dev.off()

##### 3.3.2. Predict the delay of a flight ####

#### 3.3.2.1. Regression model ####
# Build regression model
flights.delayed.regr <- lm(DELAY ~ .,
                           data = flights.train[, c(pred.att, "DELAY")])

# Get model predictions
flights.delayed.regr.pred <- as.numeric(predict(flights.delayed.regr, 
                                        flights.test[, pred.att]))

# Calculate some error metrics
print(mmetric(flights.test[, "DELAY"],
              flights.delayed.regr.pred,
              c("SSE","RMSE","MAE")))

# Plot the REC curve
png("questions/delay.predictions.png",
    w = 1013,
    h = 720)
mgraph(flights.test[, "DELAY"],
       flights.delayed.regr.pred,
       graph = "REC")
dev.off()

###### 3.4. Group airports by delays #####
# Start by group origin airports
# Start by doing hierarchical clustering
# Prepare dataset
airports.origin.times <- data.frame(AIRPORT = integer(),
                                    ARRIVAL_DELAY = integer(),
                                    AIR_SYSTEM_DELAY = integer(), SECURITY_DELAY = integer(),
                                    AIRLINE_DELAY = integer(), DEPARTURE_DELAY = integer(),
                                    LATE_AIRCRAFT_DELAY = integer(), WEATHER_DELAY = integer())
i = 1
for (airport in airports$IATA_CODE) {
  x <- colMeans(flights[flights[, "ORIGIN_AIRPORT"] == airport, delay.att])
  airports.origin.times[i, ] <- c(airport, x)
  i <- i+1
}

# Build distance matrices
airports.origin.euclidean.dist = dist(airports.origin.times[, delay.att], 
                                      method = "euclidean")
airports.origin.euclidean.dist.full = as.matrix(dist(airports.origin.times[, delay.att], 
                                                     method = "euclidean", 
                                                     diag = TRUE, 
                                                     upper = TRUE))

# Build hierarchical cluster
airports.origin.hclust <- hclust(airports.origin.euclidean.dist, 
                                 method="complete")

# Plot the dendrogram
png("questions/airports.origin.dendro.png", 
    w = 3040,
    h = 2160)
plot(airports.origin.hclust, 
     airports.origin.times$AIRPORT,
     cex = 0.6)
dev.off()

# Plot the heatmap
png("questions/airports.origin.heatmap.png", 
    w = 3040,
    h = 2160)
heatmap(airports.origin.euclidean.dist.full, 
        labRow = airports.origin.times$AIRPORT, 
        labCol = airports.origin.times$AIRPORT, 
        cexRow = 0.6, 
        cexCol = 0.6)
dev.off()

# Now try K-Means
# Set seed to get always the same results
set.seed(123456)

# Run kmeans function with:
# 3 centers - BAD, AVERAGE, GOOD
# 5 centers - HORRIBLE, BAD, AVERAGE, GOOD, EXCELLENT
airports.origin.kmeans.3 <- kmeans(airports.origin.times[, delay.att], 
                                   centers = 3, 
                                   nstart = 50)
airports.origin.kmeans.5 <- kmeans(airports.origin.times[, delay.att], 
                                   centers = 5, 
                                   nstart = 50)

# Plot clusters
png("questions/airports.origin.kmeans.3.png", 
    w = 1413,
    h = 1080)
clusplot(airports.origin.times[, delay.att], 
         airports.origin.kmeans.3$cluster, 
         color = TRUE, 
         shade = TRUE, 
         labels = 2, 
         lines = 0)
dev.off()
png("questions/airports.origin.kmeans.5.png", 
    w = 1413,
    h = 1080)
clusplot(airports.origin.times[, delay.att], 
         airports.origin.kmeans.5$cluster, 
         color = TRUE, 
         shade = TRUE, 
         labels = 2, 
         lines = 0)
dev.off()

# Then group destination airports
# Start by a hierarchical cluster
# Prepare dataset
airports.dest.times <- data.frame(AIRPORT = integer(),
                                  ARRIVAL_DELAY = integer(),
                                  AIR_SYSTEM_DELAY = integer(), SECURITY_DELAY = integer(),
                                  AIRLINE_DELAY = integer(), DEPARTURE_DELAY = integer(),
                                  LATE_AIRCRAFT_DELAY = integer(), WEATHER_DELAY = integer())
i = 1
for (airport in airports$IATA_CODE) {
  x <- colMeans(flights[flights[, "DESTINATION_AIRPORT"] == airport, delay.att])
  airports.dest.times[i, ] <- c(airport, x)
  i <- i+1
}

# Build distance matrices
airports.dest.euclidean.dist = dist(airports.dest.times[, delay.att], 
                                    method = "euclidean")
airports.dest.euclidean.dist.full = as.matrix(dist(airports.dest.times[, delay.att], 
                                                   method = "euclidean", 
                                                   diag = TRUE, 
                                                   upper = TRUE))

# Build hierarchical cluster
airports.dest.hclust <- hclust(airports.dest.euclidean.dist, 
                               method="complete")

# Plot the dendrogram
png("questions/airports.dest.dendro.png", 
    w = 3040,
    h = 2160)
plot(airports.dest.hclust, 
     airports.dest.times$AIRPORT,
     cex = 0.6)
dev.off()

# Plot the heatmap
png("questions/airports.dest.heatmap.png", 
    w = 3040,
    h = 2160)
heatmap(airports.dest.euclidean.dist.full, 
        labRow = airports.origin.times$AIRPORT, 
        labCol = airports.origin.times$AIRPORT, 
        cexRow = 0.6, 
        cexCol = 0.6)
dev.off()

# Now try K-Means
# Set seed to get always the same results
set.seed(123456)

# Run kmeans function with:
# 3 centers - BAD, AVERAGE, GOOD
# 5 centers - HORRIBLE, BAD, AVERAGE, GOOD, EXCELLENT
airports.dest.kmeans.3 <- kmeans(airports.dest.times[, delay.att], 
                                 centers = 3, 
                                 nstart = 50)
airports.dest.kmeans.5 <- kmeans(airports.dest.times[, delay.att], 
                                 centers = 5, 
                                 nstart = 50)

# Plot clusters
png("questions/airports.dest.kmeans.3.png", 
    w = 1413,
    h = 1080)
clusplot(airports.dest.times[, delay.att], 
         airports.dest.kmeans.3$cluster, 
         color = TRUE, 
         shade = TRUE, 
         labels = 2, 
         lines = 0)
dev.off()
png("questions/airports.dest.kmeans.5.png", 
    w = 1413,
    h = 1080)
clusplot(airports.dest.times[, delay.att], 
         airports.dest.kmeans.5$cluster, 
         color = TRUE, 
         shade = TRUE, 
         labels = 2, 
         lines = 0)
dev.off()

###### 3.5. Check patterns in airports and airlines #####
# Select airport, airline and delay attributes
flights.association.data <- flights[, c("ORIGIN_AIRPORT", "DESTINATION_AIRPORT", "AIRLINE", "DELAY")]

# Discretize DELAY attribute in 5 intervals of equal frequency (None, Small, Medium, Large & Huge)
flights.association.data[, "DELAY"] <- discretize(flights.association.data[, "DELAY"],
                                                  method = "frequency",
                                                  categories = 5,
                                                  labels = c("None",
                                                             "Small",
                                                             "Medium",
                                                             "Large",
                                                             "Huge"))

# Transform data frame to transactions
flights.association.data[, c(1,2)] <- lapply(flights.association.data[, c(1,2)],
                                             as.factor)
flights.association.data <- as(flights.association.data,
                               "transactions")

# Run Apriori algorithm
# We need to use a small support because of the dataaset size
flights.rules <- apriori(flights.association.data,
                         parameter = list(support = 0.000001, 
                                          confidence = 0.4))

# Analyze the first 10 rules 
summary(flights.rules)
inspect(flights.rules[1:10])
inspect(sort(flights.rules, 
             by = "lift")[1:10])

# Select rules that present great delay and analyze them
for (att in c("Large","Huge")) {
  flights.rules.sub = subset(flights.rules, 
                             subset = (rhs %in% paste("DELAY=", att, sep="")))
  inspect(sort(flights.rules.sub, 
               by = "lift")[1:10])
  # Plot rules
  png(paste("questions/flights.rules.delays.", att, ".png", sep=""),
      w = 1013,
      h = 720)
  plot(sort(flights.rules.sub, 
            by = "lift")[1:10], 
       method = "graph", 
       control = list(type = "items"))
  dev.off()
  # Write delay rules to file
  write(sort(flights.rules.sub, 
             by = "lift"), 
        file = paste("questions/flights.rules.delays.", att, ".csv", sep=""), 
        sep = "\t", 
        quote = TRUE, 
        row.names = FALSE) 
}
