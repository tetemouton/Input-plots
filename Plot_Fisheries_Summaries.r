library(splines)
library(scales)
library(ggplot2)
library(MASS)
library(reshape2)
library(plyr)
library(grid)
library(ggmap)
library(maps)
library(mapplots)
library(mapproj)
library(mapdata)
library(rgl)
library(magrittr)
library(dplyr)

# Set the drive that is being worked off, if on my compter work off the C:/ else work off penguin
    drv <- ifelse(Sys.info()["nodename"] == "SPC122114", "C:/skj/2016", "//penguin/assessments/skj/2016")

    setwd(paste0(drv, "/assessment/Data_preperation/Fisheries_Structure"))
    source("support.functions.r")
    
    theme_set(theme_bw())
    
    setInternet2(TRUE) # Sometimes permissions prevent generating the map unless this is set

# Get a map to be used for plotting the extent of the individual fisheries
    reg.map <- get_map(location = c(160,15), zoom = 3, maptype = 'satellite')# maptype = 'roadmap')

# Need to define the subregions for each fishery unfortunately - will try to automate via extraction of regions from muffie at some stage
    reg_defs <- read.csv("Assessment_Region_Coordinates_SKJ.csv", header=TRUE, stringsAsFactors=FALSE)
#    reg_defs <- read.csv("Assessment_Region_Coordinates_SKJ_alt.csv", header=TRUE, stringsAsFactors=FALSE)

    regKeep <- reg_defs$MufArea 

# List below explicitely links the 33 fisheries to the regions they are in. Note some in R7 don't cover the whole region so have to state thier subregions
    #fsh.dat <- data.frame(reg = c(1,1,1,2,2,2,2,5,5,5,5,3,3,3,3,4,4,4,4,4,4,4,4))
    fsh.reg = c(1,1,1,2,2,2,2,5,5,5,5,3,3,3,3,4,4,4,4,4,4,4,4)
#    fsh.reg = c(1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,NA,4,5,5,5,5,6,6,6,6,6,7,7,7,4,4)


    regHigh <- lapply(1:length(fsh.reg), function(x) reg_defs$MufArea[fsh.reg[x] == reg_defs$reg])


# Exclusion lists
# There are some cases where size data exists for a fleet that does not have catch in that fishery - might be a scale problem, data entry etc. etc. - all very minor so removed
#     alb.exc = list(F1 = 'NONE', F2 = 'NONE', F3 = 'NONE', F4 = 'NONE', F5 = 'NONE', F6 = 'NONE', F7 = 'NONE', F8 = 'NONE', F9 = 'NONE', F10 = 'NONE',
#                    F11 = 'NONE', F12 = 'NONE', F13 = 'NONE', F14 = 'NONE', F15 = 'NONE', F16 = 'NONE', F17 = 'NONE', F18 = 'NONE', F19 = 'NONE', F20 = 'NONE',
#                    F21 = 'NONE', F22 = 'NONE', F23 = 'NONE', F24 = 'NONE', F25 = 'NONE', F26 = 'NONE', F27 = 'NONE', F28 = 'NONE', F29 = 'NONE', F30 = 'NONE',
#                    F31 = 'NONE', F32 = 'NONE', F33 = 'NONE')

    fsh.exc <- as.list(rep("NONE", length(fsh.reg)))

# Define a couple of species specific parameters
    mod="SKJ_1_detail"
#    mod="SKJ_3_detail"
    fisheries=1:length(fsh.reg)
    Lrange=c(2,110)
    fyr=1975
    lyr=2015

# Structure all the data
    cfile = paste(mod, "_CE.csv", sep='') # Defines the name of the file containing the catch data for the different fisheries
    lfile = paste(mod, "_LF.csv", sep='') # Defines the name of the file containing the length data for the different fisheries

    Cdat = read.csv(cfile, header=TRUE)          # load catch data
    Cdat = rename(Cdat, c("yy"="yr"))        # rename variables to make things easier for functions later on
    Cdat = ExpandDat(dat=Cdat, year.lab="yr", fisheries=fisheries, first.yr=fyr, last.yr=lyr)    # calls function that cleans and reshapes the data

    Ldat = read.csv(lfile, header=TRUE)                                         # Loads length data
    Ldat = ExpandDat(dat=Ldat, year.lab="yr", fisheries=fisheries, first.yr=fyr, last.yr=lyr)   # calls function that cleans and reshapes the data
    Ldat$len = as.numeric(as.character(Ldat$len))
    Ldat = Ldat[Ldat$len >= Lrange[1] & Ldat$len < Lrange[2],]                           # culls data outside size range used in assessment
    Ldat = rename(Ldat, c('lf_samples' = 'freq'))
    L.avail = ifelse(is.na(match(fisheries, Ldat$fsh))=='TRUE',0,1)     # creates variable which indicates the availability of data, prevents crashing if there's no data

# Defines some colours for the different fleets
  # The list below started as just a few fleets for LL, but increased and increased - should have just chosen a palette in retrospect...
  # This list sets the colours for the shitload of fleets in the dataset, if two major fleets are too similar delete one between the two fleets and it will change one of them
    fullist = c('#FFCC00', '#330099', '#CCFF99', '#000000', '#FF00CC', '#FFFF00', '#FFFFCC', '#99CC99', '#33FF00', '#FF6600', '#3366FF', '#663333', '#6699FF', '#9966CC',
                '#990000', '#CCFFCC', '#003200', '#CCFFFF', '#3333FF', '#666666', '#FF6600', '#99FF33', '#CC00FF', '#660000', '#FF6699', '#3300CC', '#00CCFF', '#00FF99', '#CCCC33', '#FFFF66',
                '#9999FF', '#330000', '#6633FF', '#FF99FF', '#0066FF', '#6600FF', '#333366', '#99FF00', '#33FF66', '#FF9933', '#CCCC00', '#FF00FF', '#FF0066', '#993300', '#999999', '#333333',
                '#CC0000', '#FF3366', '#CCCC66', '#CCCC99', '#669933', '#3399FF', '#99CCFF', '#00CC66', '#009966', '#33FF99', '#0099FF', '#0033FF', '#CC9966', '#CC6600', '#CC9999', '#CCFF33', '#FF9999', '#FFCCCC',
                '#9933FF', '#FFFF33', '#FFFF99')
    collist = fullist[1:length(unique(c(Cdat$FlgFlt,Ldat$FlgFlt)))]   # Pulls out the actual number of colours needed based on the fleets in all the data
    names(collist) = sort(unique(c(Cdat$FlgFlt,Ldat$FlgFlt)))         # Gives the colours names so that the fleets can be matched to a colour and the colour can be kept consistent between plots

# The actual function that does it all and produces the plots
    generate.fsh.plot = function(fishery=1, prelim.filenm="//penguin/assessments/skj/2016/assessment/Data_preperation/Fisheries_Structure/5Reg/FshPlot_F")
    {
        Fsh.plot <- plot.fishery(reg.keep=regKeep, reg.highlight=regHigh, fishy=fishery, reg_defs=reg_defs)   # makes the map of the fishery
        
        C.plot <- Catch.Plot(dat=Cdat, fishery=fishery, collist=collist, all.yrs = 1970:2013, brwidth = 1)                              # makes the plot of the number or weight of the fish caught
        
        if(L.avail[fishery] == 1)   # if there are length samples then plot the number of samples and the median size of the samples
        {
          L.N.tab <- create.N.tab(dat=Ldat, fishery=fishery, exc.list=fsh.exc[[fishery]], all.yrs=seq(1970,2015,1), by_yr=TRUE)
          L.N.plot <- Sz.N.Plot(tab.dat=L.N.tab, y.lab="No. fish measured", collist=collist, xbrks=seq(1970, 2010, 10), brwidth = 1)
    
          L.Med.tab <- create.Med.tab(dat=Ldat, sz.lab="len", fishery=fishery, exc.list=fsh.exc[[fishery]], all.yrs=seq(1970,2015,1), by_yr=TRUE)
          L.Med.plot <- Sz.Med.Plot(tmp.med=L.Med.tab, y.lab="Median length (cm)", collist=collist)
          
          size.plt <- size.dist.plot(dat=Ldat, sz.lab="len", fishery=fishery, exc.list=fsh.exc[[fishery]], all.yrs = seq(1970, 2010, 10), by_yr=TRUE, rnding=2) 
        }
    
    # Set up full plot    
        windows(2000,1400)
    
            pushViewport(viewport(layout = grid.layout(3,2)))   # make a grid of 2 rows by 2 columns
            print(C.plot, vp = viewport(layout.pos.row=1, layout.pos.col=1))   # put the catch plot in row 1 column 1 etc. etc.
            print(Fsh.plot, vp = viewport(layout.pos.row=1, layout.pos.col=2))
            
            if(L.avail[fishery] == 1)
            {
              print(L.N.plot, vp = viewport(layout.pos.row=2, layout.pos.col=1))
              print(L.Med.plot, vp = viewport(layout.pos.row=2, layout.pos.col=2))
            }
    
            print(size.plt, vp=viewport(layout.pos.row=3, layout.pos.col=1:2))
        
        savePlot(paste0(prelim.filenm, fishery),type='png')
        dev.off()
    }
    
    
    sil <- lapply(1:length(fsh.reg), generate.fsh.plot)



# Alternative plots to check the JP only P fishery in R2 assumption
    for(i in 1:length(fsh.reg)) generage.fsh.plot(i, prelim.filenm="//penguin/assessments/skj/2016/assessment/Data_preperation/Fisheries_Structure/5Reg_Alt/FshPlot_F")


# Alternative spatial structure plots
    for(i in 27:32) generate.fsh.plot(i, prelim.filenm=paste0(drv, "/assessment/Data_preperation/Fisheries_Structure/5Reg_Alt/FshPlot_F"))

    





