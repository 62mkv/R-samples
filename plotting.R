# run this using 
# source("C:\\Develop\\R\\health\\plotting.R")

# picking up a file
filename = "C:\\Develop\\R\\health\\sample1.txt"

# read data.frame from a file
ds = read.table(filename, header=TRUE)

# converting Date column as POSIXlt class (this is necessary to manipulate Date properties, i.e. years)
ds$Date <- as.POSIXlt(ds$Date, format = '%d.%m.%Y')

ds$Year <- ds$Date$year+1900

ds$Month <- ds$Date$mon + 1

# constructing span of years
years <- c((min(ds$Date)$year+1900):(max(ds$Date)$year+1900))

# calculate "symptoms power" (as opposed to health rate)
ds$SymptPower <- 10-ds$HealthRate

# adding separate columns for each year
for (year in years) { ds[,as.character(year)] <- ifelse(ds$Date$year + 1900 == year, ds$SymptPower, NA) }

# zeroing all the years in Date
ds$Date$year <- 0

# converting Date column as Date class (this is necessary for proper xlim)
ds$Date <- as.Date(ds$Date)

# this will be a range of X-axis limits
xlims = c(as.Date("1900-01-01"), as.Date("1900-12-31"))

# calculating colors for plotting
cv <- rainbow(length(years))

# plot a graph ('xaxs' is used for more pretty display of X axis marks for this case)
# NB: we cannot use plot(ds,..) because it does not display 2 set of Y values properly in that case
# TODO: add every month tick on x axis 
# TODO: add legend for years
plot(ds$Date, 
     ds[,as.character(years[1])], 
     xaxp = c(as.Date("1900-01-01"), as.Date("1900-12-31"), 22), 
     xlim = xlims, ylim = c(0,12),
     xlab="Day of year", ylab = "Symptoms severity",
     xaxs = "i", 
     col=cv[1])
i = 2
while (i <= length(years)) {
 points(ds$Date, ds[,as.character(years[i])], col=cv[i])
 i <- i+1
}

# adding a "week number" factor (to use in "group by" operations later)
ds$Week <- as.numeric(strftime(ds$Date,"%U"))

