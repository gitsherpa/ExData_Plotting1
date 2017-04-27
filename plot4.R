
# download and unzip file from given url
data2explore.source.get <- function(...) {
    source.url <-  'http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
    source.zip <-  'household_power_consumption.zip'
    data2explore.source <-  'household_power_consumption.txt'
    
    # assumption: source file located in working directory, otherwise:
    # setwd(".....")
    if (!file.exists(data2explore.source)) {
        download.file(source.url, destfile = source.zip, mode='wb')
        unzip(source.zip)
    }
    data2explore.source
}

# generation of clean data from existing (downloaded and unzipped) file
data2explore.get <- function() {
    # import data source from file
    data2explore <-  read.csv(
        data2explore.source.get(),
        sep = ';',
        dec = '.',
        na.strings = '?'
    )
    # 
    data2explore$Date <-  as.Date(data2explore$Date, "%d/%m/%Y")
    data2explore <-  subset(data2explore,Date == "2007-02-01" | Date == "2007-02-02")
    
    data2explore$Time <-  strptime(data2explore[,2], '%H:%M:%S')


    # evaluate data source
    #str(data2explore)
    #head(data2explore)
    #summary(data2explore)

    data2explore
}

# check, whether clean data is available, otherwise generation of data source
if (!exists("data2explore")) {
    data2explore <- data2explore.get()
} 




par(mfrow=c(2,2), oma=c(0,1,2,0))

# plot1
plot(data2explore$Global_active_power
     , type="l"
     , xlab=""
     , xaxt="n"
     , ylab="Global Active Power")
axis(1, at=c(0, length(data2explore$Global_active_power)/2, length(data2explore$Global_active_power))
     , labels=c("Thu", "Fri", "Sat"))

# plot2
plot(data2explore$Voltage
     , type="l"
     , xlab="datetime"
     , xaxt="n"
     , ylab="Voltage")
axis(1, at=c(0, length(data2explore$Global_active_power)/2, length(data2explore$Global_active_power))
     , labels=c("Thu", "Fri", "Sat"))


# plot3
plot(data2explore$Sub_metering_1, xaxt="n", type="l", xlab="", ylab="Energy sub metering", col="black")
lines(data2explore$Sub_metering_2, xaxt="n", type="l", col="red")
lines(data2explore$Sub_metering_3, xaxt="n", type="l", col="blue")
axis(1
     , at=c(0, length(data2explore$Sub_metering_1)/2, length(data2explore$Sub_metering_1))
     , labels=c("Thu", "Fri", "Sat")
)
legend("topright"
       , col=c("black","red","blue")
       , c("Sub_metering_1  ","Sub_metering_2  ", "Sub_metering_3  ")
       , lty=c(1,1)
       , bty="n"
       , cex=0.5)

# plot4
plot(data2explore$Global_reactive_power
     , type="l"
     , xlab="datetime"
     , xaxt="n"
     , ylab="Global_reactive_power")
axis(1, at=c(0, length(data2explore$Global_active_power)/2, length(data2explore$Global_active_power))
     , labels=c("Thu", "Fri", "Sat"))


dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()
