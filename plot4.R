par(mfrow = c(2, 2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))

plot(totime,b$Global_active_power,type = "l",ylab = "Global Active Power(kilowatts)",xlab = "")

plot (x, b$Voltage, type = "l", ylab ="Voltage", xlab = "datetime")

plot(x,y0,type = "l", col = 1, ylab = "Global Active Power(kilowatts)",xlab = "")

lines(x,y1,col=2)

lines(x,y2,col=4)

legend("topright", col = c(1,2,4), bty = "n",lwd=2, cex=0.2, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

plot(totime,b$Global_reactive_power,type = "l",ylab = "Global reactive Power",xlab = "datetime")