library(plyr)
library(ggplot2)

## Load and Clean the Data

    activityData <- read.csv("activity.csv",head=TRUE, na.strings="NA", colClasses=c("integer","character","integer"))
    
## Summarize the steps taken per day. Any day for which the total steps are 0 are
## removed from the analysis. It is assumed that the device was not properly worn
## on days where it indicates the person never moved.

    #Calculate steps per day, then remove all days with zero steps
    stepsPerDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
    stepsPerDay <- stepsPerDay[which(stepsPerDay > 0)]
    
    #Generate historgram
    outfile <- "figure/StepsPerDay.png"
    png(outfile)
    hist(stepsPerDay, col="blue", main="Total Steps in a Day (when device worn)", 
         xlab="Total Steps in the Day", ylab="Frequency")
    dev.off()
    message(paste("Chart Complete. Stored as ",getwd(),"/",outfile,sep=""))
    rm(outfile)
    
    #Calculate averages
    stepsPerDayAvg <- c(mean(stepsPerDay), median(stepsPerDay))
    names(stepsPerDayAvg) <- c('Mean', 'Median')
    print(stepsPerDayAvg)
    rm(stepsPerDayAvg)
    rm(stepsPerDay)
    
## Summarize the average steps taken throughout the day. 
    
    #Calculate steps per day, then remove all days with zero steps
    stepsPerTime <- tapply(activityData$steps, activityData$interval, mean, na.rm=TRUE)
    
    #Generate historgram
    outfile <- "figure/StepsThruDay.png"
    png(outfile)
    plot(stepsPerTime, type="l", col="black", main="Avg. Steps Throughout the Day", 
         xlab="Daily Time Interval", ylab="Avgerage (mean) Steps")
    dev.off()
    message(paste("Chart Complete. Stored as ",getwd(),"/",outfile,sep=""))
    rm(outfile)
    
    #Calculate averages
    maxSteps <- as.list(c(names(stepsPerTime)[which(stepsPerTime == max(stepsPerTime))],max(stepsPerTime)))
    names(maxSteps) <- c('Time Interval when Max. Avg. Steps Occur', 'Maximum Average Steps')
    print(maxSteps)
    rm(maxSteps)
    rm(stepsPerTime)

## Missing Data Handling. Will impute missing data with average steps for the time interval.
    message("Indicator table for rows with steps data missing (TRUE = missing)")
    t <- table(is.na(activityData$steps))
    print(t)
    rm(t)

    ##Calculate interval averages and map onto original data
        stepsPerTime <- ddply(activityData, .(interval), summarize, 
                              avgSteps = mean(steps, na.rm=TRUE))
        activityData2 <- join(activityData, stepsPerTime, by = "interval", type = "left", 
                              match = "all")
        rm(stepsPerTime)

    ##Replace missing steps with average for interval
        activityData2$steps[is.na(activityData$steps)] <- 
            activityData2$avgSteps[is.na(activityData$steps)]

    ##tidy up dataset
        activityData2 <- activityData2[ , 1:4]

## Replot Imputed Data to Summarize the average steps taken throughout the day. 

    #Calculate steps per day, then remove all days with zero steps
    stepsPerTime <- tapply(activityData2$steps, activityData2$interval, mean, na.rm=TRUE)
    
    #Generate historgram
    outfile <- "figure/StepsThruDay.png"
    png(outfile)
    plot(stepsPerTime, type="l", col="black", main="Avg. Steps Throughout the Day (imputed data)", 
         xlab="Daily Time Interval", ylab="Avgerage (mean) Steps")
    dev.off()
    message(paste("Chart Complete. Stored as ",getwd(),"/",outfile,sep=""))
    rm(outfile)
    
    #Calculate averages
    maxSteps <- as.list(c(names(stepsPerTime)[which(stepsPerTime == max(stepsPerTime))],max(stepsPerTime)))
    names(maxSteps) <- c('Time Interval when Max. Avg. Steps Occur', 'Maximum Average Steps')
    print(maxSteps)
    rm(maxSteps)
    rm(stepsPerTime)

## Compare Weekend and Weekdays with imputed data
    
    ##Create Identifier for Weekdays

        #Transform character date into date field
        daysRecorded <- as.POSIXct(activityData2$date, format="%Y-%m-%d")
        
        ##Get Name of days
        dayNames <- weekdays(daysRecorded)
    
        ##Initialize array for weekportion inicator to weekday
        weekPortion <- rep("Weekday",length(dayNames))
    
        ##Recode sunday and saturday to weekend
        weekPortion[dayNames == "Sunday" | dayNames == "Saturday"] <- "Weekend"
    
        ## Map back onto dataset
        activityData2 <- cbind(activityData, weekPortion)

        rm(weekPortion)
        rm(dayNames)
        rm(daysRecorded)

    ##Summarize average time interval steps by week portion
    stepsPerTime <- ddply(activityData2, .(interval, weekPortion), summarize, 
                          avgSteps = mean(steps, na.rm=TRUE))
    
    ##Plot Weekend vs Weekday
        outfile <- "figure/StepsThruDaybyWeekPortion"
        png(outfile)
        
            g <- ggplot(stepsPerTime, aes(interval,avgSteps)) 
            g <- g + facet_grid(weekPortion ~ .) 
            g <- g + geom_line(color="goldenrod4")
            g <- g + xlab("Time Interval")
            g <- g + ylab("Avg. Steps during Time Interval")
            g <- g + ggtitle(expression("Comparison of Avg. Steps per Time by Weekday and Weekend"))
            g <- g + theme_bw()
            print(g)
        
        dev.off()
        message(paste("Chart Complete. Stored as ",getwd(),"/",outfile,sep=""))
        rm(outfile)
        rm(stepsPerTime)