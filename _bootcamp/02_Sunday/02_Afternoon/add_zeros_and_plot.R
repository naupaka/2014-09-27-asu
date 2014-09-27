# Author: Naupaka Zimmerman June 16, 2014
# Script to make figure with ggplot from
# command line argument for SWC bootcamp
# UC Davis - Davis, CA

# Load libraries
require(ggplot2)
require(reshape2)

# Function to add leading zero to patients with single-digit ID numbers
add.zero <- function(patient.id.in){
    split.chars <- strsplit(as.character(patient.id.in),split="")
    if (length(split.chars[[1]]) == 2){
        joined.chars <- paste(split.chars[[1]][1],"0",split.chars[[1]][2],sep="")
        joined.chars
    }
    else {
        patient.id.in
    }
}

# Takes one command line argument as input data set
options <- commandArgs(trailingOnly = TRUE)

# load that csv from command line arg
data.in <- read.csv(options[1])

# melt with defaults
data.in.melted <- melt(data.in)

# apply add.zeros function to each element in the PatientID column
data.in.melted$PatientID <- sapply(as.character(data.in.melted$PatientID),add.zero)

# plot figure with dynamically generated title
out.plot <- ggplot(data.in.melted,(aes(x=PatientID, y=value))) + 
    geom_point() + 
    stat_sum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(paste("Patient Inflammation figure for ", options[1], sep=""))

# save pdf based on input file name
savename <- paste0(options[1],".pdf")
ggsave(out.plot, file=savename, width = 12, height = 8)
