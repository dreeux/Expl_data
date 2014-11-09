b[,Date := as.Date(Date,format = "%d/%m/%Y")]

concat = paste(b$Date,b$Time,sep=' ')

totime = strptime(concat,"%Y-%m-%d %H:%M:%S")

plot(totime,b$Global_active_power,type = "l",ylab = "Global Active Power(kilowatts)",xlab = "")
