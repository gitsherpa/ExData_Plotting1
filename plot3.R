
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
    str(data2explore)
    head(data2explore)
    summary(data2explore)

    data2explore
}

# check, whether clean data is available, otherwise generation of data source
if (!exists("data2explore")) {
    data2explore <- data2explore.get()
} 

## quantitative exploration of variable
print(summary(data2explore$Sub_metering_1, data2explore$Sub_metering_2, data2explore$Sub_metering_3))

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
       , lwd=c(1,1))


dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()
