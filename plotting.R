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

# calculate severity (aka "symptoms power") (as opposed to health rate)
ds$Severity <- 10-ds$HealthRate

# adding separate columns for each year
for (year in years) { ds[,as.character(year)] <- ifelse(ds$Date$year + 1900 == year, ds$Severity, NA) }

# zeroing all the years in Date
ds$Date$year <- 0

# converting Date column as Date class (this is necessary for proper xlim)
ds$Date <- as.Date(ds$Date)

# adding a "week number" factor (to use in "group by" operations later)
ds$Week <- as.numeric(strftime(ds$Date,"%U"))

# calculating colors for plotting
cv <- rainbow(length(years))

plot_severity_by_yearday <- function() {

 # this will be a range of X-axis limits
 xlims = c(as.Date("1900-01-01"), as.Date("1900-12-31"))

 # plot a graph ('xaxs' is used for more pretty display of X axis marks for this case)
 # NB: we cannot use plot(ds,..) because it does not display 2 set of Y values properly in that case
 # TODO: add every month tick on x axis 
 # TODO: add legend for years
 plot(366,10,
      xaxp = c(xlims, 12), 
      xlim = xlims, ylim = c(0,12),
      xaxs = "i",
      axes = FALSE,
      xlab="Day of year", ylab = "Symptoms severity", main = "Severity per day"
      )
 i = 1
 while (i <= length(years)) {
  points(ds$Date, ds[,as.character(years[i])], col=cv[i])
  i <- i+1
 }
}

barplot_per_year <- function(period) {
# preparing for aggregation
 dsa <- ds[c(period, "Year", "Severity")]

# do the aggregation
 sdsa <- aggregate(x = dsa, by=list(period = dsa[, period], year = dsa$Year), FUN = sum)

# removing erroneously summed-up columns
 sdsa[, period] <- NULL
 sdsa[, "Year"] <- NULL

# transforming the aggregated by "week, year" data.frame to a matix for barplot
 period.matrix <- xtabs(Severity ~ year+period, data=sdsa)
# and plotting it
 dev.new()
#TODO: make sure X axis values are spread evenly
 barplot(period.matrix, col = cv, legend = unique(sdsa[, "year"]), 
         main = paste ("Sum of severity per",tolower(period),"and year"),
         axes = FALSE, axisnames = FALSE
 )
}

barplot_per_year("Week")
#barplot_per_year("Month")
