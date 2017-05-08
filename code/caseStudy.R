####### UTILITIES ######
is.not.na <- function(x) ! is.na(x)
# load("sessions/flights.session")

####### DATA PREPARATION ######
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
Airport_ID_to_IATA <- function (x) {
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

# Fix airport codes in flights dataset
flights$ORIGIN_AIRPORT <- Airport_ID_to_IATA(flights$ORIGIN_AIRPORT)
flights$DESTINATION_AIRPORT <- Airport_ID_to_IATA(flights$DESTINATION_AIRPORT)

# Merge datasets
# We need to change the name of a column in airlines dataset
# or it will cause a duplicate column and will not merge
colnames(airlines) <- c("IATA_CODE", "AIRLINE_NAME")
flights <- merge(flights, airports, by.x = "ORIGIN_AIRPORT", by.y = "IATA_CODE")
flights <- merge(flights, airports, by.x = "DESTINATION_AIRPORT", by.y = "IATA_CODE")
flights <- merge(flights, airlines, by.x = "AIRLINE", by.y = "IATA_CODE")

head(flights)
colnames(flights)

# Remove NA values and replace by 0
flights[is.na(flights)] <- 0
head(flights)

# Select time attributes from flights dataset
time.att <- c("SCHEDULED_TIME","ELAPSED_TIME","AIR_TIME","ARRIVAL_DELAY",
              "AIR_SYSTEM_DELAY","SECURITY_DELAY","AIRLINE_DELAY", "DEPARTURE_DELAY",
              "LATE_AIRCRAFT_DELAY","WEATHER_DELAY")
delay.att <- time.att[4:length(time.att)]
summary(flights[ ,time.att])

# Add DELAY and DELAYED attribute to dataset
# DELAY represents the sum of all delay attributes
# DELAYED represents if the flight as been delayed or not (positive delay)
flights[, "DELAY"] <- rowSums(flights[, delay.att])
flights[, "DELAYED"] <- flights[, "DELAY"] > 0

# Save the current R session.
save.image("sessions/flights.session")

####### EXPLORATIVE ANALISYS ######
# install.packages("maps")
library("maps")

# Show airports location on a map 
# Thanks to miquar on Kaggle
png("plots/map.airports.png")
map("usa")
title("Airports")
points(airports$LONGITUDE,
       airports$LATITUDE,
       col="red",
       cex=0.75)
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
par(mfrow=c(4,3))
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

# Check correlation between attributes
png("plots/pairs.times.png",
    w = 1413,
    h = 1080)
pairs(flights[ ,time.att], 
      lower.panel = panel.smooth)
dev.off()

####### KNOWLEDGE EXTRACTION OBJECTIVES ######
# install.packages("rpart")
library(rpart)
# install.packages("rpart.plot")
library(rpart.plot)
# install.packages("caret")
library(caret)
# install.packages("randomForest")
library(randomForest)
# install.packages("neuralnet")
library(neuralnet)
# install.packages("cluster")
library(cluster)
# install.packages("arules")
library(arules)
# install.packages("arulesViz")
library(arulesViz)

###### 1. Best time of the year to travel #####
# Get the mean of all delays in all months
months <- seq(1, 12, 1)
months.delays <- rep(0, 12)
for (month in months) {
  x <- flights[flights[, "MONTH"] == month, delay.att]
  months.delays[month] <- sum(x)/nrow(x)
}

# Plot the resulting graph in a barplot
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

###### 2. Best airline to travel within #####
# Get the sum of all delay times
airline.codes <- airlines$IATA_CODE
airline.times <- rep(0, length(airline.codes))
for (i in 1:length(airline.codes)) {
  x <- flights[flights[, "AIRLINE"] == airline.codes[i], delay.att]
  airline.times[i] <- sum(x)/nrow(x)
}

# Plot the resulting graph in a barplot
png("questions/airlines.times.png",
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

###### 3. Predict if a flight will be delayed and the delay #####
# Set seed to get always the same results
set.seed(123456)

# Get attributes to use for prediction
pred.att <- c("AIRLINE", "DESTINATION_AIRPORT", "ORIGIN_AIRPORT", "MONTH", "DAY", "DAY_OF_WEEK", "SCHEDULED_DEPARTURE", "SCHEDULED_TIME", "SCHEDULED_ARRIVAL")

##### 3.1. Predict if a flight will be delayed ####
# Generate train and test datasets
train <- sample(1:nrow(flights),
                size = ceiling(0.7 * nrow(flights)),
                replace = FALSE)
flights.train <- flights[train, c(pred.att, "DELAYED")]
flights.test <- flights[-train, c(pred.att, "DELAYED")]

#### Logistic Regression ####
# Build logistic regression model
flights.logit <- glm(DELAYED ~ .,
                     family = binomial(link = 'logit'), 
                     data = flights.train)

# Evaluate the logistic model
flights.logit.probs <- predict(flights.logit, 
                               flights.test[, pred.att])
flights.logit.pred <- rep(FALSE, 
                          nrow(flights.test))
flights.logit.pred[flights.logit.probs > 0.4] <- TRUE
flights.logit.cm <- confusionMatrix(flights.logit.pred, 
                                    flights.test[, "DELAYED"])
print(flights.logit.cm)

#### Decision Tree ####
# Create the tree model
flights.tree = rpart(DELAYED ~ ., 
                     data = flights.train)

# Plot the tree
png("questions/delayed.tree.png",
    w = 1013,
    h = 720)
rpart.plot(flights.tree)
dev.off()

# Evaluate the tree predictions
flights.tree.probs <- predict(flights.tree, 
                              flights.test[, pred.att])
flights.tree.pred <- rep(FALSE, 
                         nrow(flights.test))
flights.tree.pred[flights.tree.probs > 0.4] <- TRUE
flights.tree.cm <- confusionMatrix(flights.tree.pred, 
                                   flights.test[, "DELAYED"])
print(flights.tree.cm)

#### Random Forests ####
# Create the random forest model
flights.rf = randomForest(DELAYED ~ ., 
                          data = flights.train, 
                          mtry = floor(sqrt(ncol(flights))), 
                          ntree = 50)

# Evaluate the forest predictions
flights.rf.prob <- predict(flights.rf, 
                           flights.test[, pred.att])
flights.rf.pred <- rep(FALSE, 
                       nrow(flights.test))
flights.rf.pred[flights.rf.probs > 0.4] <- TRUE
flights.rf.cm <- confusionMatrix(flights.rf.pred, 
                                 flights.test[, "DELAYED"])
print(flights.rf.cm)

#### Neural Network ####
# Build network model
flights.net <- neuralnet(DELAYED ~ ., 
                         data = flights.train, 
                         hidden = c(4,4), 
                         lifesign = "minimal", 
                         linear.output = FALSE, 
                         threshold = 0.1)

# Plot the net
plot(flights.net, 
     rep = "best")

# Evaluate the net predictions
flights.net.prob <- predict(flights.net, 
                            flights.test[, pred.att])
flights.net.pred <- rep(FALSE, 
                        nrow(flights.test))
flights.net.pred[flights.net.probs > 0.4] <- TRUE
flights.net.cm <- confusionMatrix(flights.net.pred, 
                                  flights.test[, "DELAYED"])
print(flights.net.cm)

##### 3.2. Predict the delay of a flight ####
# Build regression model
flights.regression <- lm(DELAY ~ AIRLINE + DESTINATION_AIRPORT +
                           ORIGIN_AIRPORT + MONTH + DAY + DAY_OF_WEEK + SCHEDULED_DEPARTURE + 
                           SCHEDULED_TIME + SCHEDULED_ARRIVAL, 
                         data = flights.train)

# Test regression model
flights.regression.pred = predict(flights.regression,
                                  flights.test, 
                                  type = "response")
# Confusion matrix
table(flights.regression.pred, 
      flights.test[, "DELAY"]) 

###### 4. Group airports by delays #####
# Start by group origin airports
# Start by a hierarchical cluster
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

# Run kmeans function with 3/5 centers
airports.origin.kmeans.3 <- kmeans(airports.origin.times[, delay.att], 
                                   centers = 3, 
                                   nstart = 50)
airports.origin.kmeans.5 <- kmeans(airports.origin.times[, delay.att], 
                                   centers = 5, 
                                   nstart = 50)

# Plot cluster
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

# Run kmeans function with 3/5 centers
airports.dest.kmeans.3 <- kmeans(airports.dest.times[, delay.att], 
                                 centers = 3, 
                                 nstart = 50)
airports.dest.kmeans.5 <- kmeans(airports.dest.times[, delay.att], 
                                 centers = 5, 
                                 nstart = 50)

# Plot cluster
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

###### 5. Check patterns in airports and airlines #####
# ...
