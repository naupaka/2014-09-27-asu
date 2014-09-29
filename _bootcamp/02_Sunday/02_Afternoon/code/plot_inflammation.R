# Author: Naupaka Zimmerman September 27, 2014 
# Script to make figure with ggplot from
# command line argument for SWC bootcamp
# Arizona State University - Tempe, AZ
# Takes an inflammation csv file as a single 
# command line argument and plots it

# Takes one command line argument as input data set
options <- commandArgs(trailingOnly = TRUE)
 
# load that csv from command line arg
data.in <- read.csv(options[1])
 
# save png based on input from file name
savename <- paste0(options[1],".png")
# write figure to a png image file 
png(filename=savename)
plot(data.in$Day13, data.in$Day14, main = options[1], xlab = "Day 13", ylab = "Day 14", cex = 2)
dev.off()
