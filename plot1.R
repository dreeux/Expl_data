dataset <- fread("household_power_consumption.txt", sep=";", na.strings=c("?","NA",""), colClasses="character")

b <- dataset[(dataset$Date == "1/2/2007"| dataset$Date == "2/2/2007")]

plot1 <- as.numeric(b$Global_active_power)

hist(plot1, col = "red", main = "Global Active Power", xlab = "Global Active Power(kilowatts)")
