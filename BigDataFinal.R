#install.packages("data.table")
library(data.table)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("xts")
library(xts)
#install.packages("chron")
library(chron)
#install.packages("scales")
library(scales)
#install.packages("devtools")
library(devtools)
#install_github("Rforecastio", "hrbrmstr")
library(Rforecastio)
library(plyr)
library(ggplot2)
library(reshape2)
library(ggmap)
library(zoo)

plotmap <- function(data) {
  data_2016_ORIG <- subset(completeDataTable, DATE > "2015-12-31")
  data_2016 <- subset(data_2016_ORIG, data_2016_ORIG$BOROUGH != "");
  data_2016 <- subset(data_2016, !is.na(data_2016$LATITUDE));

  # getting the map
  map <- get_map(location = c(lon = mean(data_2016$LONGITUDE),
                              lat = mean(data_2016$LATITUDE)),
                 zoom = "auto",
                 scale = "auto",
                 maptype = "roadmap");
  # plotting the map with some points on it
  map_df <- data.frame(lon = data_2016$LONGITUDE, lat = data_2016$LATITUDE);
  ggmap(map) +
    geom_point(data = map_df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE);

  data_2016_manhattan <- subset(data_2016, data_2016$BOROUGH == "MANHATTAN");
  # getting the map
  map_manhattan <- get_map(location = c(lon = mean(data_2016_manhattan$LONGITUDE),
                                        lat = mean(data_2016_manhattan$LATITUDE)),
                           zoom = 13,
                           scale = "auto",
                           maptype = "roadmap");
  # plotting the map with some points on it
  ggmap(map_manhattan) +
    geom_point(data = data_2016_manhattan, aes(x = data_2016_manhattan$LONGITUDE, y = data_2016_manhattan$LATITUDE, fill = "red", alpha = 0.8), size = 5, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE);


}

predictiveAnalytics <- function(data) {
  data1 <- completeDataTable;
  data1$YEAR <- format(data1$DATE,'%Y');
  data1$YEAR.MONTH <- format(data1$DATE,'%Y-%m');
  collisionByYr <- ddply(data1, ~YEAR.MONTH, summarise,
                         noOfColl = sum(CUSTOM.WEIGHT),
                         avgInjured = mean(NUMBER.OF.PERSONS.INJURED),
                         avgKilled = mean(NUMBER.OF.PERSONS.KILLED));
  collisionByYr$year <- format.Date(as.yearmon(collisionByYr$YEAR.MONTH),"%Y");
  collisionByYr$month <- format.Date(as.yearmon(collisionByYr$YEAR.MONTH),"%m");

  xlab <- paste("Year (", format.Date(min(data1$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(data1$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  #collisionByYr$YEAR.MONTH <- as.Date(as.yearmon(collisionByYr$YEAR.MONTH,"%Y-%m"))

  ggplot(collisionByYr, aes(as.yearmon(collisionByYr$YEAR.MONTH,"%Y-%m"), avgInjured)) +
    geom_point() +
    geom_smooth() +
    xlab(xlab) + ylab("Average no. of people injured") +
    ggtitle("Collisions History");

  ggplot(collisionByYr, aes(as.Date(YEAR,"%Y"), avgKilled)) +
    geom_point() +
    geom_smooth() +
    xlab(xlab) + ylab("Average no. of people killed") +
    ggtitle("Collisions History");

  trainingPositions <- sample(nrow(collisionByYr),
                              size = floor(nrow(collisionByYr) * 0.7))
  training <- collisionByYr[trainingPositions,]
  test <- collisionByYr[-trainingPositions,]
  model <- lm(avgInjured~year + month, training)

  prediction <- predict(model, test)
  print(prediction)

  result <- as.data.frame(cbind(as.yearmon(test$YEAR.MONTH, "%Y-%m"), test$avgInjured, prediction))
  colnames(result) <- c("Year", "Actual", "Prediction")
  print(result)

  resultMelted <- melt(result, id="Year")
  ggplot(data=resultMelted, aes(x=Year, y=value, colour=variable)) + geom_line()

  ggplot(collisionByYr, aes(YEAR, noOfColl)) +
    geom_point() +
    geom_smooth() +
    xlab(xlab) + ylab("Total no. of collisions") +
    ggtitle("Collisions History");

  manhattan <- data1[data1$BOROUGH == "MANHATTAN",];
  manhattanCollisionByYr <- ddply(manhattan, ~YEAR, summarise,
                                  noOfColl = sum(CUSTOM.WEIGHT),
                                  avgInjured = mean(NUMBER.OF.PERSONS.INJURED),
                                  avgKilled = mean(NUMBER.OF.PERSONS.KILLED));
  ggplot(manhattanCollisionByYr, aes(YEAR, noOfColl)) +
    geom_point() + geom_smooth() +
    xlab(xlab) + ylab("Total no. of collisions") +
    ggtitle("Collisions History");

}

predictAvgInjuredByMonthYear <- function(data) {
  predictData <- data;
  predictData$YEAR.MONTH <- format(predictData$DATE,'%Y-%m');
  collisionByMonthYr <- ddply(predictData, ~YEAR.MONTH, summarise,
                              #noOfColl = sum(CUSTOM.WEIGHT),
                              avgInjured = mean(NUMBER.OF.PERSONS.INJURED),
                              avgKilled = mean(NUMBER.OF.PERSONS.KILLED));

  collisionByMonthYr$YEAR.MONTH <- as.yearmon(collisionByMonthYr$YEAR.MONTH,"%Y-%m")
  collisionByMonthYr2 <- melt(collisionByMonthYr, id="YEAR.MONTH");
  levels(collisionByMonthYr2$variable) <- c("Average no. of people injured",
                                            "Average no. of people killed");

  xlab <- paste("Year (", format.Date(min(predictData$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(predictData$DATE), "%b-%y"))
  xlab <- paste(xlab, ")");

  plot.dir.pred <- "/Users/kbhandari/ms/spring_16/IS-690Q - Big Data/project/plots/prediction_graphs/";
  png(filename=paste(plot.dir.pred,"avg_ppl_injured_killed_by_yr.png"))
  plot(ggplot(collisionByMonthYr2, aes(YEAR.MONTH, value)) +
         geom_point() + geom_smooth() +
         facet_wrap(~variable, ncol = 1, scales = "free_y") +
         xlab(xlab) + ylab(""));
  dev.off()

  collisionByMonthYr$year <- format.Date(as.yearmon(collisionByMonthYr$YEAR.MONTH),"%Y");
  collisionByMonthYr$month <- format.Date(as.yearmon(collisionByMonthYr$YEAR.MONTH),"%m");

  trainingPositions <- sample(nrow(collisionByMonthYr),
                              size = floor(nrow(collisionByMonthYr) * 0.7))
  training <- collisionByMonthYr[trainingPositions,]
  test <- collisionByMonthYr[-trainingPositions,]
  model <- lm(avgInjured~year + month, training)
  print(model)

  prediction <- predict(model, test)
  print(prediction)

  result <- as.data.frame(cbind(as.yearmon(test$YEAR.MONTH, "%Y-%m"), test$avgInjured, prediction))
  colnames(result) <- c("Year", "Actual", "Prediction")
  print(result)

  resultMelted <- melt(result, id="Year");
  png(filename=paste(plot.dir.pred,"avg_ppl_injured_pred_vs_actual.png"))
  plot(ggplot(data=resultMelted, aes(x=Year, y=value, colour=variable)) +
         geom_line() +
         xlab(xlab) + ylab("Average no. of people injured") +
         ggtitle("Prediction - average no. of people injured"));
  dev.off()

  trainingPositions <- sample(nrow(collisionByMonthYr),
                              size = floor(nrow(collisionByMonthYr) * 0.7))
  training <- collisionByMonthYr[trainingPositions,]
  test <- collisionByMonthYr[-trainingPositions,]
  model <- lm(avgKilled~year + month, training)
  print(model)

  prediction <- predict(model, test)
  print(prediction)

  result <- as.data.frame(cbind(as.yearmon(test$YEAR.MONTH, "%Y-%m"), test$avgKilled,
                                prediction))
  colnames(result) <- c("Year", "Actual", "Prediction")
  print(result)

  resultMelted <- melt(result, id="Year");
  png(filename=paste(plot.dir.pred,"avg_ppl_killed_pred_vs_actual.png"))
  plot(ggplot(data=resultMelted, aes(x=Year, y=value, colour=variable)) +
         geom_line() +
         xlab(xlab) + ylab("Average no. of people killed") +
         ggtitle("Prediction - average no. of people killed"));
  dev.off()
}

classifyLocation <- function(inputData) {
  final_data <- completeDataTable

  # filer data for 2015 and Manhattan
  final_data <- getFilteredDataAsDataTable(final_data, 2015, 'MANHATTAN')

  # consider data from 08:00 to 21:00, since maximum no. of accidents happens during that time
  final_data <- subset(final_data, format(DATE.TIME, "%m") == 12 )
  final_data <- subset(final_data, format(DATE.TIME, "%H:%M:%S") > '08:00:00' )
  final_data <- subset(final_data, format(DATE.TIME, "%H:%M:%S") < '21:00:00' )

  # Take random sample of data
  sampleData <- final_data[sample(nrow(final_data), size = 900),]
  sampleData$WEATHER <- "TBA"

  options(timeout = 3600)

  for (i in seq(1,900)) {
    my.latitude = sampleData[i,LATITUDE]
    my.longitude = sampleData[i,LONGITUDE]
    DATE.TIME = sampleData[i,DATE.TIME]
    #my.date = ISOdatetime(format(DATE.TIME, "%Y"),format(DATE.TIME, "%m"),format(DATE.TIME, "%d"),format(DATE.TIME, "%H"),format(DATE.TIME, "%M"),format(DATE.TIME, "%S"), tz="EST")

    my.date = as.numeric(DATE.TIME)

    #fio.list <- get_current_forecast(my.latitude, my.longitude)
    fio.list <- get_forecast_for(my.latitude, my.longitude, my.date)

    sampleData[i,"WEATHER"] <- fio.list$currently[[2]]
  }

  # Lets use decision tree now
  library(rpart)
  #install.packages('rattle')
  #install.packages('rpart.plot')
  #install.packages('RColorBrewer')
  library(rattle)
  library(rpart.plot)
  library(RColorBrewer)

  trainingPositions <- sample(nrow(sampleData),
                              size = floor(nrow(sampleData) * 0.7))
  training <- sampleData[trainingPositions,]
  test <- sampleData[-trainingPositions,]

  fit <- rpart(LOCATION ~ WEATHER + DATE.TIME +
                 CONTRIBUTING.FACTOR.VEHICLE.1 + CONTRIBUTING.FACTOR.VEHICLE.2 +
                 CONTRIBUTING.FACTOR.VEHICLE.3 + CONTRIBUTING.FACTOR.VEHICLE.4 +
                 CONTRIBUTING.FACTOR.VEHICLE.5 +
                 VEHICLE.TYPE.CODE.1 + VEHICLE.TYPE.CODE.2 +
                 VEHICLE.TYPE.CODE.3 + VEHICLE.TYPE.CODE.4 +
                 VEHICLE.TYPE.CODE.5, data=training, method="class")


  plot(fit)

  library("party")

  model <- ctree(LOCATION ~ WEATHER + DATE.TIME +
                   CONTRIBUTING.FACTOR.VEHICLE.1 + CONTRIBUTING.FACTOR.VEHICLE.2 +
                   CONTRIBUTING.FACTOR.VEHICLE.3 + CONTRIBUTING.FACTOR.VEHICLE.4 +
                   CONTRIBUTING.FACTOR.VEHICLE.5 +
                   VEHICLE.TYPE.CODE.1 + VEHICLE.TYPE.CODE.2 +
                   VEHICLE.TYPE.CODE.3 + VEHICLE.TYPE.CODE.4 +
                   VEHICLE.TYPE.CODE.5, data=training)

}

getFilteredDataAsDataTable <- function(data, year, borough) {
  filteredData <- data;

  # Filter based on input params
  if (!is.null(year)) {
    filteredData <- subset(filteredData, format.Date(DATE, "%Y") == year);
  }

  if (!is.null(borough) && borough != "") {
    filteredData <- subset(filteredData, BOROUGH == borough);
  }

  # Create a data table
  filteredData <- as.data.table(filteredData)

  # Check data formats, dates are strings
  str(filteredData)

  filteredData$DATE <- as.Date(filteredData$DATE, "%m/%d/%Y")
  filteredData$DATE.TIME <- as.POSIXct(paste(filteredData$DATE, filteredData$TIME),
                                       format="%Y-%m-%d %H:%M", tz="EST")
  filteredData$CUSTOM.WEIGHT <- 1

  return(filteredData);
}

hourWiseCollision <- function(inputData) {
  final_data <- inputData

  # use data table to aggregate on hours
  # First lets add a field plot time
  final_data[, PlotTime := format(DATE.TIME, "%H:%M:%S")]

  # key by this plot time
  setkeyv(final_data, "PlotTime")

  # aggregate the data for each minute
  plotdata <- final_data[, .(Count.monthly = sum(as.integer(CUSTOM.WEIGHT))), by = PlotTime]

  # aggregate the data for each hour
  plotdata$PlotTime <- as.POSIXct(plotdata$PlotTime, format="%H:%M:%S", tz="EST")
  plotdata.xts <- xts(plotdata$Count.monthly, plotdata$PlotTime)
  ep <- endpoints(plotdata.xts, 'hours', 1)
  plot.xts <- period.apply(plotdata.xts,ep,sum)
  plot.ggplot.df <- data.frame(time=index(plot.xts), count=coredata(plot.xts))
  plot.ggplot <- as.data.table(plot.ggplot.df)
  plot.ggplot[, dtTime := format(time, "%H:%M")]

  xlab <- paste("Hour of day (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plot.ggplot, aes(dtTime,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           scale_x_discrete(labels=c("00:59" = "00:00 - 01:00",
                                     "01:59" = "01:00 - 02:00",
                                     "02:59" = "02:00 - 03:00",
                                     "03:58" = "03:00 - 04:00",
                                     "03:59" = "03:00 - 04:00",
                                     "04:59" = "04:00 - 05:00",
                                     "05:58" = "05:00 - 06:00",
                                     "05:59" = "05:00 - 06:00",
                                     "06:59" = "06:00 - 07:00",
                                     "07:59" = "07:00 - 08:00",
                                     "08:59" = "08:00 - 09:00",
                                     "09:59" = "09:00 - 10:00",
                                     "10:59" = "10:00 - 11:00",
                                     "11:59" = "11:00 - 12:00",
                                     "12:59" = "12:00 - 13:00",
                                     "13:59" = "13:00 - 14:00",
                                     "14:59" = "14:00 - 15:00",
                                     "15:59" = "15:00 - 16:00",
                                     "16:59" = "16:00 - 17:00",
                                     "17:59" = "17:00 - 18:00",
                                     "18:59" = "18:00 - 19:00",
                                     "19:59" = "19:00 - 20:00",
                                     "20:59" = "20:00 - 21:00",
                                     "21:59" = "21:00 - 22:00",
                                     "22:59" = "22:00 - 23:00",
                                     "23:59" = "23:00 - 24:00")) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

monthWiseCollision <- function(inputData) {
  final_data <- inputData
  # use data table to aggregate on hours
  # First lets add a field plot time
  final_data[, PlotTime := format(DATE.TIME, "%m")]

  # key by this plot time
  setkeyv(final_data, "PlotTime")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = PlotTime]

  xlab <- paste("Month of day (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(PlotTime,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           scale_x_discrete(labels=c("01" = "January",
                                     "02" = "February",
                                     "03" = "March",
                                     "04" = "April",
                                     "05" = "May",
                                     "06" = "June",
                                     "07" = "July",
                                     "08" = "August",
                                     "09" = "September",
                                     "10" = "October",
                                     "11" = "November",
                                     "12" = "December")) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

boroughWiseCollision <- function(inputData) {
  final_data <- inputData
  # use data table to aggregate on borough
  setkeyv(final_data, "BOROUGH")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = BOROUGH]

  # Remove missing borough data
  plotdata <- plotdata[which(plotdata$BOROUGH != ""),]

  xlab <- paste("Borough (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(BOROUGH,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

injuredKilledCount <- function(inputData) {
  final_data <- inputData
  pedestriansInjured <- sum(final_data$NUMBER.OF.PEDESTRIANS.INJURED, na.rm = TRUE)
  pedestriansKilled <- sum(final_data$NUMBER.OF.PEDESTRIANS.KILLED, na.rm = TRUE)

  cyclistInjured <- sum(final_data$NUMBER.OF.CYCLIST.INJURED, na.rm = TRUE)
  cyclistKilled <- sum(final_data$NUMBER.OF.CYCLIST.KILLED, na.rm = TRUE)

  motoristInjured <- sum(final_data$NUMBER.OF.MOTORIST.INJURED, na.rm = TRUE)
  motoristKilled <- sum(final_data$NUMBER.OF.MOTORIST.KILLED, na.rm = TRUE)

  injuredKilledData <- data.frame(reason = character(), count = numeric(), stringsAsFactors = FALSE)
  injuredKilledData[1, ] <-  c("Pedestrians Injured", pedestriansInjured)
  injuredKilledData[2, ] <-  c("Cyclist Injured", cyclistInjured)
  injuredKilledData[3, ] <-  c("Motorist Injured", motoristInjured)

  injuredKilledData[4, ] <-  c("Pedestrians Killed", pedestriansKilled)
  injuredKilledData[5, ] <-  c("Cyclist Killed", cyclistKilled)
  injuredKilledData[6, ] <-  c("Motorist Killed", motoristKilled)

  xlab <- paste("Category (", format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")
  return(ggplot(injuredKilledData, aes(reason,count,fill=count,width=1)) +
           xlab(xlab) + ylab("Count") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));

}

boroughWiseOnStreetCollisionCount <- function(inputData) {
  final_data <- inputData
  # use data table to aggregate on hours
  setkeyv(final_data, "ON.STREET.NAME")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = ON.STREET.NAME]

  # Remove missing on street data
  plotdata <- plotdata[which(plotdata$ON.STREET.NAME != ""),]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  xlab <- paste("Streets of", unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(ON.STREET.NAME,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

boroughWiseOnStreetCrossStreetCollisionCount <- function(inputData) {
  final_data <- inputData
  final_data$ON.STREET.CROSS.STREET <- paste(final_data$ON.STREET.NAME, " # ", final_data$CROSS.STREET.NAME);

  # use data table to aggregate on hours
  setkeyv(final_data, "ON.STREET.CROSS.STREET")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = ON.STREET.CROSS.STREET]

  # Remove missing on street data
  plotdata <- plotdata[which(plotdata$ON.STREET.CROSS.STREET != "  #  "),]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  xlab <- paste("On Street # Cross Street of", unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(ON.STREET.CROSS.STREET,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

top10CollisionCount <- function(inputData) {
  final_data <- inputData

  final_data <- final_data[which(!is.na(final_data$LATITUDE)),]
  final_data <- final_data[which(!is.na(final_data$LONGITUDE)),]

  final_data$LATITUDE.LONGITUDE <- paste(final_data$LATITUDE, " # ", final_data$LONGITUDE);

  # use data table to aggregate on hours
  setkeyv(final_data, "LATITUDE.LONGITUDE")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = LATITUDE.LONGITUDE]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  plotdata$LATITUDE <- sapply(strsplit(plotdata$LATITUDE.LONGITUDE, " # "),"[",1)
  plotdata$LONGITUDE <- sapply(strsplit(plotdata$LATITUDE.LONGITUDE, " # "),"[",2)

  plotdata$LATITUDE <- gsub(" ", "", plotdata$LATITUDE, fixed = TRUE)
  plotdata$LONGITUDE <- gsub(" ", "", plotdata$LONGITUDE, fixed = TRUE)

  plotdata$LATITUDE <- as.numeric(plotdata$LATITUDE)
  plotdata$LONGITUDE <- as.numeric(plotdata$LONGITUDE)

  print(plotdata)

  xlab <- "Top 10 places of collisions in"
  if (length(unique(final_data[which(final_data$BOROUGH != ""), BOROUGH])) == 1) {
    xlab <- paste(xlab, unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  } else {
    xlab <- "Top 10 places of collisions "
  }
  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  # getting the map
  map <- get_map(location = c(lon = mean(plotdata$LONGITUDE),
                              lat = mean(plotdata$LATITUDE)),
                 zoom = 12,
                 scale = "auto",
                 maptype = "roadmap");

  # plotting the map with some points on it
  return(ggmap(map) +
           geom_point(data = plotdata,
                      aes(x = plotdata$LONGITUDE,
                          y = plotdata$LATITUDE, fill = count, alpha = 0.8),
                      size = 5, shape = 21) +
           ggtitle(xlab) +
           guides(fill=FALSE, alpha=FALSE, size=FALSE));
}


contributingFactorOfCollision <- function(inputData) {
  final_data <- inputData

  # Filter Unspecified contributing factors
  final_data <- final_data[which(final_data$CONTRIBUTING.FACTOR.VEHICLE.1 != ""),]
  final_data <- final_data[which(final_data$CONTRIBUTING.FACTOR.VEHICLE.1 != "Unspecified"),]
  final_data$CONTRIBUTING.FACTOR <- final_data[,CONTRIBUTING.FACTOR.VEHICLE.1]

  # use data table to aggregate on hours
  setkeyv(final_data, "CONTRIBUTING.FACTOR")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = CONTRIBUTING.FACTOR]

  # Remove missing on street data
  plotdata <- plotdata[which(plotdata$CONTRIBUTING.FACTOR != "  #  "),]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  if (length(unique(final_data[which(final_data$BOROUGH != ""), BOROUGH])) == 1) {
    xlab <- paste("Contributing Factors in", unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  } else {
    xlab <- "Contributing Factors"
  }

  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(CONTRIBUTING.FACTOR,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

top10CollisionDays <- function(inputData) {
  final_data <- inputData
  #final_data <- completeDataTable[which(BOROUGH=='STATEN ISLAND'),]

  # use data table to aggregate on hours
  setkeyv(final_data, "DATE")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = DATE]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  if (length(unique(final_data[which(final_data$BOROUGH != ""), BOROUGH])) == 1) {
    xlab <- paste("Top 10 days of collision in", unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  } else {
    xlab <- "Top 10 days of collision"
  }

  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(DATE,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

contributingFactor_1_2_Combined <- function(inputData) {
  final_data <- inputData

  # Filter Unspecified contributing factors
  final_data <- final_data[which(final_data$CONTRIBUTING.FACTOR.VEHICLE.1 != ""),]
  final_data <- final_data[which(final_data$CONTRIBUTING.FACTOR.VEHICLE.1 != "Unspecified"),]
  final_data$CONTRIBUTING.FACTOR <- final_data[,CONTRIBUTING.FACTOR.VEHICLE.1]

  toConcat2 <- which(final_data$CONTRIBUTING.FACTOR.VEHICLE.2 != "Unspecified" & final_data$CONTRIBUTING.FACTOR.VEHICLE.2 != "" & final_data$CONTRIBUTING.FACTOR.VEHICLE.2 != final_data$CONTRIBUTING.FACTOR.VEHICLE.1)
  final_data[toConcat2,"CONTRIBUTING.FACTOR"] <- paste(final_data[toConcat2,CONTRIBUTING.FACTOR], " # ", final_data[toConcat2,CONTRIBUTING.FACTOR.VEHICLE.2])

  # use data table to aggregate on hours
  setkeyv(final_data, "CONTRIBUTING.FACTOR")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = CONTRIBUTING.FACTOR]

  # Remove missing on street data
  plotdata <- plotdata[which(plotdata$CONTRIBUTING.FACTOR != "  #  "),]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  if (length(unique(final_data[which(final_data$BOROUGH != ""), BOROUGH])) == 1) {
    xlab <- paste("Contributing Factors in ", unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  } else {
    xlab <- "Contributing Factors"
  }

  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(CONTRIBUTING.FACTOR,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

vehicleTypesCollision <- function(inputData) {
  final_data <- inputData

  # Filter Unspecified contributing factors
  final_data <- final_data[which(final_data$VEHICLE.TYPE.CODE.1 != ""),]
  final_data$VEHICLE.TYPE <- final_data[,VEHICLE.TYPE.CODE.1]

  toConcat <- which(final_data$VEHICLE.TYPE.CODE.2 != "" )
  final_data[toConcat,"VEHICLE.TYPE"] <- paste(final_data[toConcat,VEHICLE.TYPE], " # ", final_data[toConcat,VEHICLE.TYPE.CODE.2])

  toConcat <- which(final_data$VEHICLE.TYPE.CODE.3 != "" )
  final_data[toConcat,"VEHICLE.TYPE"] <- paste(final_data[toConcat,VEHICLE.TYPE], " # ", final_data[toConcat,VEHICLE.TYPE.CODE.3])

  toConcat <- which(final_data$VEHICLE.TYPE.CODE.4 != "" )
  final_data[toConcat,"VEHICLE.TYPE"] <- paste(final_data[toConcat,VEHICLE.TYPE], " # ", final_data[toConcat,VEHICLE.TYPE.CODE.4])

  toConcat <- which(final_data$VEHICLE.TYPE.CODE.5 != "" )
  final_data[toConcat,"VEHICLE.TYPE"] <- paste(final_data[toConcat,VEHICLE.TYPE], " # ", final_data[toConcat,VEHICLE.TYPE.CODE.5])

  # use data table to aggregate on hours
  setkeyv(final_data, "VEHICLE.TYPE")

  # aggregate the data for each minute
  plotdata <- final_data[, .(count = sum(as.integer(CUSTOM.WEIGHT))), by = VEHICLE.TYPE]

  # Remove missing on street data
  plotdata <- plotdata[which(plotdata$VEHICLE.TYPE != "  #  "),]

  # Only plot top 10 street data
  plotdata <- plotdata[rev(order(plotdata$count)),][1:10]

  if (length(unique(final_data[which(final_data$BOROUGH != ""), BOROUGH])) == 1) {
    xlab <- paste("Contributing Factors in ", unique(final_data[which(final_data$BOROUGH != ""), BOROUGH]))
  } else {
    xlab <- "Contributing Factors"
  }

  xlab <- paste(xlab, "(")
  xlab <- paste(xlab, format.Date(min(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, "till")
  xlab <- paste(xlab, format.Date(max(final_data$DATE), "%b-%y"))
  xlab <- paste(xlab, ")")

  return(ggplot(plotdata, aes(VEHICLE.TYPE,count,fill=count,width=1)) +
           xlab(xlab) + ylab("No. of vehicle collisions") +
           geom_bar(stat="identity", position = "identity", alpha = .6) +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)));
}

file = "/Users/kbhandari/ms/spring_16/IS-690Q - Big Data/project/NYPD_Motor_Vehicle_Collisions.csv"
data <- read.csv(file)
data$DATE <- as.Date(data$DATE, "%m/%d/%Y")

plot.dir <- "/Users/kbhandari/ms/spring_16/IS-690Q - Big Data/project/plots/yearly_graphs/";
allBorough <- unique(data[which(data$BOROUGH != ""), "BOROUGH"]);

printYearly = FALSE;

if (printYearly) {
  # Plot the graphs for various years
  startYear <- format.Date(min(data$DATE), "%Y");
  endYear <- format.Date(max(data$DATE), "%Y");
  for (year in startYear:endYear) {
    # Plot the hour wise collision graph for various years
    print(paste("Plotting graph for year: ", year))
    file.name <- paste(paste("plot_hourWiseCollision_",year),".png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    filteredData <- getFilteredDataAsDataTable(data, year, NULL)

    png(filename=paste(plot.dir,file.name))
    plot(hourWiseCollision(filteredData))
    dev.off()

    # Plot the month wise collision graph for various years
    file.name <- paste(paste("plot_monthWiseCollision_",year),".png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    png(filename=paste(plot.dir,file.name))
    plot(monthWiseCollision(filteredData))
    dev.off()

    # Plot the borough wise collision graph for various years
    file.name <- paste(paste("plot_boroughWiseCollision_",year),".png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    png(filename=paste(plot.dir,file.name))
    plot(boroughWiseCollision(filteredData))
    dev.off()

    # Plot the month wise injured & killed count for various categories
    file.name <- paste(paste("plot_injuredKilledCount_",year),".png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    png(filename=paste(plot.dir,file.name))
    plot(injuredKilledCount(filteredData))
    dev.off()

    # Plot the top 10 collision days
    file.name <- paste(paste("plot_top10CollisionDays_",year),".png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    png(filename=paste(plot.dir,file.name))
    plot(top10CollisionDays(filteredData))
    dev.off()

    # Plot the on street data for all the borough
    for (borough in allBorough) {
      file.name <- paste(paste(paste(paste("plot_on_street_",borough),"_"),year),".png");
      file.name <- gsub(" ", "", file.name, fixed = TRUE);
      png(filename=paste(plot.dir,file.name))
      plot(boroughWiseOnStreetCollisionCount(getFilteredDataAsDataTable(filteredData, NULL, borough)))
      dev.off()

      file.name <- paste(paste(paste(paste("plot_on_street_cross_street_",borough),"_"),year),".png");
      file.name <- gsub(" ", "", file.name, fixed = TRUE);
      png(filename=paste(plot.dir,file.name))
      plot(boroughWiseOnStreetCrossStreetCollisionCount(getFilteredDataAsDataTable(filteredData, NULL, borough)))
      dev.off()
    }
  }
}

completeDataTable <- getFilteredDataAsDataTable(data, NULL, NULL);
plot.dir.agg <- "/Users/kbhandari/ms/spring_16/IS-690Q - Big Data/project/plots/aggregated_graphs/";

# Plot the hour wise collision graph for the complete dataset
file.name <- "plot_hourWiseCollision_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(hourWiseCollision(completeDataTable))
dev.off()

# Plot the contributing factors collision graph for the complete dataset
file.name <- "plot_contributing_factors_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(contributingFactorOfCollision(completeDataTable))
dev.off()

# Plot the contributing factors collision graph for the complete dataset
file.name <- "plot_vehicle_types_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(vehicleTypesCollision(completeDataTable))
dev.off()

# Plot the month wise collision graph for the complete dataset
file.name <- "plot_monthWiseCollision_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(monthWiseCollision(completeDataTable))
dev.off()

# Plot the borough wise collision graph for complete dataset
file.name <- "plot_boroughWiseCollision_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(boroughWiseCollision(completeDataTable))
dev.off()

# Plot the no. of injured & killed graph for the complete dataset
file.name <- "plot_injuredKilledCount_complete_dataset.png";
file.name <- gsub(" ", "", file.name, fixed = TRUE);
png(filename=paste(plot.dir.agg,file.name))
plot(injuredKilledCount(completeDataTable))
dev.off()

# Plot the borough wise collision graph for the complete dataset
for (borough in allBorough) {
  print(paste("Printing plot for", borough))
  file.name <- paste(paste("plot_on_street_",borough),"_complete_dataset.png");
  file.name <- gsub(" ", "", file.name, fixed = TRUE);
  png(filename=paste(plot.dir.agg,file.name))
  plot(boroughWiseOnStreetCollisionCount(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
  dev.off()

  print(paste("Printing plot for", borough))
  file.name <- paste(paste("plot_on_street_cross_street_",borough),"_complete_dataset.png");
  file.name <- gsub(" ", "", file.name, fixed = TRUE);
  png(filename=paste(plot.dir.agg,file.name))
  plot(boroughWiseOnStreetCrossStreetCollisionCount(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
  dev.off()

  print(paste("Printing hourwise collision plot for", borough))
  file.name <- paste(paste("plot_hourWiseCollision_",borough),"_complete_dataset.png");
  file.name <- gsub(" ", "", file.name, fixed = TRUE);
  png(filename=paste(plot.dir.agg,file.name))
  plot(hourWiseCollision(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
  dev.off()

  print(paste("Printing contributing factors of collision plot for", borough))
  file.name <- paste(paste("plot_contributing_factors_",borough),"_complete_dataset.png");
  file.name <- gsub(" ", "", file.name, fixed = TRUE);
  png(filename=paste(plot.dir.agg,file.name))
  plot(contributingFactorOfCollision(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
  dev.off()

  print(paste("Printing contributing factors of collision plot for", borough))
  file.name <- paste(paste("plot_vehicle_types_",borough),"_complete_dataset.png");
  file.name <- gsub(" ", "", file.name, fixed = TRUE);
  png(filename=paste(plot.dir.agg,file.name))
  plot(vehicleTypesCollision(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
  dev.off()

  if (FALSE) {
    print(paste("Printing top 10 of collision plot for", borough))
    file.name <- paste(paste("plot_top_10_collision_",borough),"_complete_dataset.png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    png(filename=paste(plot.dir.agg,file.name))
    plot(top10CollisionCount(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
    dev.off()

    print(paste("Printing top 10 collision days for", borough))
    file.name <- paste(paste("plot_top10CollisionDays_",borough),"_complete_dataset.png");
    file.name <- gsub(" ", "", file.name, fixed = TRUE);
    png(filename=paste(plot.dir.agg,file.name))
    plot(top10CollisionDays(getFilteredDataAsDataTable(completeDataTable, NULL, borough)))
    dev.off()
  }
}

# Prediction analytics for average no. of people injured
predictAvgInjuredByMonthYear(completeDataTable)
