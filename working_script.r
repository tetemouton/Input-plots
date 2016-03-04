library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(R4MFCL)
library(scales)
library(grid)
library(stringr)  

# Source in the functions needed
    source("C:/Users/SamM/Desktop/GitHub_Desk/Input-plots/Input-plots/plot_tag_summary.r")

# Read in key files
    fd <- read.table("C:/skj/2016/assessment/Setup/fdesc.txt", header=TRUE)
    skjtag <- read.tag("C:/skj/2016/assessment/Model_Runs/run2014_1/skj.tag")

# Plot a summary of the .tag file
    plot.tag.summary(fdesc = fd, tag = skjtag, time.scl = "year")
