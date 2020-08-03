housepower <- read.table("file:///Users/janhavivaswani/Desktop/Coursera/household_power_consumption.txt", sep = ";", na.strings = "?", header = TRUE)
options(max.print = .Machine$integer.max)

housepower$Date <- as.Date(housepower$Date, format = "%d/%m/%Y")
housepowerSub <- subset(housepower, Date >= "2007-02-01" & Date <= "2007-02-02")

list(housepowerSub)

png(file="file:///Users/janhavivaswani/Desktop/Coursera/plot1.png",
    width=480, height=480)
hist(housepowerSub$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (killowatts", col = "red", )
dev.off()
Date_Time <- paste(housepowerSub$Date, housepowerSub$Time)
housepowerSub$DateTime <- as.POSIXct(Date_Time)
housepower2 <- housepowerSub[, c(10, 3:9)]

png(file="file:///Users/janhavivaswani/Desktop/Coursera/plot2.png",
    width=480, height=480)
plot(housepower2$Global_active_power ~ housepower2$DateTime, ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")
dev.off()

png(file="file:///Users/janhavivaswani/Desktop/Coursera/plot3.png",
    width=480, height=480)
plot(housepower2$Sub_metering_1 ~ housepower2$DateTime, ylab = "Energy sub metering", xlab = "", type = "l")
lines(housepower2$Sub_metering_2 ~ housepower2$DateTime, col = 'Red')
lines(housepower2$Sub_metering_3 ~ housepower2$DateTime, col = 'Blue')
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)
dev.off()

png(file="file:///Users/janhavivaswani/Desktop/Coursera/plot4.png",
    width=480, height=480)
par(mfrow = c(2,2))
plot(housepower2$Global_active_power ~ housepower2$DateTime, ylab = "Global Active Power", xlab = "", type = "l")
plot(housepower2$Voltage ~ housepower2$DateTime, ylab = "Voltage", xlab = "datetime", type = "l")
plot(housepower2$Sub_metering_1 ~ housepower2$DateTime, ylab = "Energy sub metering", xlab = "", type = "l")
lines(housepower2$Sub_metering_2 ~ housepower2$DateTime, col = 'Red')
lines(housepower2$Sub_metering_3 ~ housepower2$DateTime, col = 'Blue')
legend("topright", col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)
plot(housepower2$Global_reactive_power ~ housepower2$DateTime, ylab = "Global_reactive_power", xlab = "datetime", type = "l")
dev.off()
