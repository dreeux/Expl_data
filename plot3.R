> x <- totime
> y0 <- b$Sub_metering_1
> y1 <- b$Sub_metering_2
> y2 <- b$Sub_metering_3
> plot(x,y0,type = "l", col = 1, ylab = "Global Active Power(kilowatts)",xlab = "")
> lines(x,y1,col=2)
> lines(x,y2,col=4)
legend("topright", col = c(1,2,4), bty = "n",lwd=2, cex=1, legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
> 