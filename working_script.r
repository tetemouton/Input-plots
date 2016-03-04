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
    source("C:/Users/SamM/Desktop/GitHub_Desk/Input-plots/Input-plots/plot_sizes_movement.tags.r")
    source("C:/Users/SamM/Desktop/GitHub_Desk/Input-plots/Input-plots/plot_compare_tags.r")

# Read in key files
    fd <- read.table("C:/skj/2016/assessment/Setup/fdesc.txt", header=TRUE)
    skjtag <- read.tag("C:/skj/2016/assessment/Model_Runs/run2014_1/skj.tag")
    skjalt <- read.tag("//penguin/assessments/skj/2011/Model_runs/ref_t6fixgr/skj.tag")

# Plot a summary of the .tag file
    windows(2200,2200)   
        plot_tag_summary(tag = skjtag, time.scl = "year")
    dev.off()

# Plot movement matrix and sizes of tagged fish
    windows(2300,2300) 
        plot_sizes_movement_tags(fdesc = fd, tag = skjtag)
    dev.off()

# Plot the comparison between two different .tag files
    windows(2000,2400)   
        plot_compare_tags(tag1 = skjalt, tag2 = skjtag, fln = c("2011", "2014"), prg.fm = c(" JPN","m JP"), prg.to = "JPTP")
    dev.off()

