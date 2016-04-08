
ExpandDat = function(dat=cdat, year.lab="yr", fisheries=1:30, first.yr=1960, last.yr=2015)   # year.lab now effectively redundant but left it in out of laziness
{
    dat <- dat[dat$fishery_no %in% fisheries,]                                   # Excludes data not in fisheries
    dat <- dat[dat[,year.lab] >= first.yr & dat[,year.lab] <= last.yr,]               # Excludes data before 1960
    dat$yrqtr <- dat[,year.lab] + dat$qq/4 - 0.125
    dat$FlgFlt <- paste(dat$flag_id, dat$fleet_id)
    rename(dat, fsh = fishery_no)
}

create.N.tab = function(dat=Ldat, fishery=1, exc.list=exc.list, all.yrs=seq(1960.125,2015.875,0.25), by_yr=FALSE)   # Gets data into the right form to produce the number of samples plots
{                                               # makes sure that if there is no data in a certain year then there is an NA and hence ggplot plots a zero
    tmp.dat <- dat[dat$fsh == fishery,]
    tmp.dat <- tmp.dat[!(tmp.dat$FlgFlt %in% exc.list),]   # Excludes size samples for fleets where there are size samples but no catch - decides which to exclude based on the exc.list list in the working script
    
    if(by_yr)
    {
        tab.dat <- tapply(tmp.dat$freq, list(tmp.dat$yr,tmp.dat$FlgFlt), sum)  
    } else {
        tab.dat <- tapply(tmp.dat$freq, list(tmp.dat$yrqtr,tmp.dat$FlgFlt), sum)      
    }
    
    yr.match <- match(all.yrs, rownames(tab.dat))
    tab.dat <- data.frame("yr"=as.character(all.yrs), tab.dat[yr.match,],row.names=NULL); names(tab.dat) = c("yr",sort(unique(tmp.dat$FlgFlt)))
    tab.dat <- melt(tab.dat, id.vars="yr", measure.vars=dimnames(tab.dat)[[2]][-1]); names(tab.dat) = c("yr","Fleet","Nsamples")  # Put into long format for plot
              
    return(tab.dat)
}


create.Med.tab = function(dat=Ldat, sz.lab="len", fishery=1, exc.list=exc.list, all.yrs = seq(1960.125,2015.875,0.25), by_yr=FALSE)   # Calculates median size for each year there is data and puts into format that can be plotted
{
    tmp.dat <- dat[dat$fsh == fishery,]
    tmp.dat <- tmp.dat[!(tmp.dat$FlgFlt %in% exc.list),]   # Excludes size samples for fleets where there are size samples but no catch - decides which to exclude based on the exc.list list in the working script
    
    if(by_yr)
    {
      tab.dat <- aggregate(tmp.dat$freq, by=list(yr = tmp.dat[,"yr"], Fleet = tmp.dat[,"FlgFlt"], Size = tmp.dat[,sz.lab]), FUN=sum) # aggregates number of samples at each size by year and fleet  
    } else {
      tab.dat <- aggregate(tmp.dat$freq, by=list(yr = tmp.dat[,"yrqtr"], Fleet = tmp.dat[,"FlgFlt"], Size = tmp.dat[,sz.lab]), FUN=sum) # aggregates number of samples at each size by year and fleet     
    }        
    
    tab.dat$ind <- 1:dim(tab.dat)[1]

    new.ind <- rep(tab.dat$ind,tab.dat$x)   # Makes a variable that indicates how many samples there are for each size in that year/fleet
    new.dat <- tab.dat[match(new.ind, tab.dat$ind),]   # Expands the dataset from tab.dat which is aggregated by length to a full dataset where each row is an individual fish

    tmp.med <- tapply(new.dat[,"Size"], list(new.dat[,"yr"], new.dat[,"Fleet"]), FUN=function(x) quantile(x, 0.5))   # Now take the median over the expanded data
    yr.match <- match(all.yrs, rownames(tmp.med))   # Make sure there is a median or an NA for each year
    tmp.med <- data.frame("yr"=as.character(all.yrs), tmp.med[yr.match,],row.names=NULL); names(tmp.med) = c("yr",sort(unique(tmp.dat$FlgFlt)))
    tmp.med <- melt(tmp.med, id.vars="yr", measure.vars=dimnames(tmp.med)[[2]][-1]); names(tmp.med) = c("yr","Fleet","Size")   # put into long format for plotting
      
    return(tmp.med)
}



Catch.Plot = function(dat=dat, fishery=1, collist=collist, all.yrs = 1970:2015, brwidth = 0.9)   # Calculates and plots catch of a fishery by year and fleet
{
    tmp.dat <- dat[dat$fsh == fishery,]    

    if(tmp.dat[1,"gr_id"] %in% c("L","T"))   # If the gear of the fishery is longline or troll then calculate catch in numbers of fish (sp_n)
    {
        tab.dat <- tapply(tmp.dat$sp_n, list(tmp.dat$yr,tmp.dat$FlgFlt), FUN=sum)
    } else {                        # If the gear of the fishery is something else then calculate catch in weight (sp_c)
        tab.dat <- tapply(tmp.dat$sp_c, list(tmp.dat$yr,tmp.dat$FlgFlt), FUN=sum)
    }
    
    yr.match <- match(all.yrs, rownames(tab.dat))
      tab.dat <- data.frame("yr"=as.character(all.yrs), tab.dat[yr.match,],row.names=NULL); names(tab.dat) = c("yr",sort(unique(tmp.dat$FlgFlt)))
        tab.dat <- melt(tab.dat, id.vars="yr", measure.vars=dimnames(tab.dat)[[2]][-1]); names(tab.dat) = c("yr","Fleet","Nsamples")  # put into long format for plotting
          Ncol <- ifelse(length(unique(tab.dat$Fleet)) > 14, 2, 1)   # This determines how many columns the legend is - if there are heaps of fleets (e.g. some ps fisheries) then the legend is 2 columns, otherwise just one
    
      p <- ggplot(data=tab.dat, aes(x=yr, y=Nsamples/1000, fill=Fleet)) +   # Divide by 1000 to keep the y scale managable
                  geom_bar(stat="identity", colour="black", width=brwidth) +
                  scale_fill_manual(name="Fleet", values=collist) 
          if(tmp.dat[1,"gr_id"] == "L")
          {
              p <- p + xlab("Year") + ylab("Catch (fish x 1000)")   # If longline fishery axis label in no. of fish
          } else {
              p <- p + xlab("Year") + ylab("Catch (MT x 1000)")     # If something else then axis label in metric tonnes
          }

          p <- p + scale_x_discrete(breaks=pretty_breaks(n=5)) + # manually insert breaks and labels for consistency across fisheries
                   theme(legend.key.size = unit(0.08,"cm"), legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1),
                         legend.background = element_rect(fill="transparent"), panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank()) +  # make legend unit size small else it takes up too much room
                   ggtitle(paste("Fishery",fishery)) +        # plot the fishery name as the title - in future might be nice to give the actual fishery name e.g. all_flag_3 etc.
                   guides(fill=guide_legend(ncol=Ncol))       # insert legend for fleets, and determine columns from Ncol
      p
}



Sz.N.Plot <- function(tab.dat=L.N.tab, y.lab="No. fish measured", collist=collist, xbrks=seq(1970, 2010, 10), brwidth = 0.9)   # Makes a plot of the number of size samples for a fishery - bar plot
{
    tab.dat$yr <- as.character(tab.dat$yr)
    Ncol <- ifelse(length(unique(tab.dat$Fleet)) > 14, 2, 1)   # This determines how many columns the legend is - if there are heaps of fleets (e.g. some ps fisheries) then the legend is 2 columns, otherwise just one
    
        p <- ggplot(data=tab.dat, aes(x=yr, y=Nsamples, fill=Fleet)) +
                    geom_bar(stat="identity", colour="black", width=brwidth) +
                    scale_fill_manual(name="Fleet", values=collist) +
                    xlab("Year") + ylab(y.lab) +
                    scale_x_discrete(breaks=xbrks, labels=xbrks) +
                    theme(legend.key.size = unit(0.08,"cm"), legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1),
                          legend.background = element_rect(fill="transparent"), panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank()) +
                    guides(fill=guide_legend(ncol=Ncol))       # insert legend for fleets, and determine columns from Ncol
        p
}

Sz.Med.Plot <- function(tmp.med=L.Med.tab, y.lab="Median length (cm)", collist=collist)   # Makes a plot of the median sizes for a fishery - line plot
{
    tmp.med$yr <- as.numeric(as.character(tmp.med$yr))
    Ncol <- ifelse(length(unique(tmp.med$Fleet)) > 14, 2, 1)   # This determines how many columns the legend is - if there are heaps of fleets (e.g. some ps fisheries) then the legend is 2 columns, otherwise just one
    
        p <- ggplot(data=tmp.med, aes(x=yr, y=Size, colour=Fleet)) +
                    geom_point() +
                    geom_line() +
                    scale_colour_manual(name="Fleet", values=collist) +
                    xlab("Year") + ylab(y.lab) +
                    scale_x_continuous(breaks=pretty_breaks(n=5)) +
                    theme(legend.key.size = unit(0.08,"cm"), legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1),
                          legend.background = element_rect(fill="transparent"), panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(), legend.key=element_rect(colour="transparent")) +
                    guides(colour=guide_legend(ncol=Ncol))       # insert legend for fleets, and determine columns from Ncol
    p
}

# This function plots the map of the spatial extent of the individual fisheries
plot.fishery = function(reg.keep=reg.keep, reg.highlight=reg.highlight, fishy=1, reg_defs=reg_defs) # Remove the getmap step and things will speed up immensely
{

   reg.bounds.x <- cbind(reg_defs$lon1n, reg_defs$lon2n, reg_defs$lon2n, reg_defs$lon1n, reg_defs$lon1n) 
   reg.bounds.y <- cbind(reg_defs$lat1n, reg_defs$lat1n, reg_defs$lat2n, reg_defs$lat2n, reg_defs$lat1n) 
  
      p <- ggmap(reg.map, fullpage=TRUE)  # pull in the map (reg.map) that was extracted in the working script, you could put the get map step in here if different base maps were needed for different fisheries
      #p = ggmap(reg.map)

      for(i in 1:length(reg.highlight[[fishy]]))   # this loop determines which region/s or subregion/s are highlighted - appear as a shaded polygon - determined by the reg.highlight list in the working script
      {
          ind <- match(reg.highlight[[fishy]][i], reg_defs$MufArea)
          dat <- data.frame(x=reg.bounds.x[ind,], y=reg.bounds.y[ind,])  # matches the highlight list with the bounds given above to determine the polygon to be plotted
          p <- p + geom_polygon(data=dat, mapping=aes(x=x, y=y), fill=alpha("red", 0.5))   # plot a semi-transparent polygon
          
          if(i == 1)
          {
              txtdat <- data.frame(x=mean(c(reg_defs[ind,"lon1n"],reg_defs[ind,"lon2n"])),
                                   y=mean(c(reg_defs[ind,"lat1n"],reg_defs[ind,"lat2n"])),
                                   r=reg_defs[ind,"reg"])  
              p <- p + geom_text(data=txtdat, aes(x=x, y=y, label=r, colour="white"), hjust=0, vjust=1, size=5)
          }
      }
      
      for(i in 1:length(reg.keep))   # this loop determines which regions/subregions are plotted as outlines - based on a list of regions to "keep" and their bounds
      {
          dat <- data.frame(x=reg.bounds.x[i,],y=reg.bounds.y[i,])
          #dat.txt <- reg.txt[reg.txt$reg == i,]
          p <- p + geom_polygon(data=dat, mapping=aes(x=x, y=y), fill=alpha("red", 0.25))
          #p <- p + geom_text(data=dat.txt, aes(x=x, y=y, label=r, colour="red"), hjust=0, vjust=0, size=3)
      }

      p <- p + theme(legend.position="none")
      p
}


size.dist.plot = function(dat=Ldat, sz.lab="len", fishery=1, exc.list=exc.list, all.yrs = seq(1960.125,2015.875,0.25), by_yr=FALSE, rnding=2, relativeP=TRUE)   # Calculates median size for each year there is data and puts into format that can be plotted
{
  
  tmp.dat <- dat[dat$fsh == fishery,]
  tmp.dat <- tmp.dat[!(tmp.dat$FlgFlt %in% exc.list),]   # Excludes size samples for fleets where there are size samples but no catch - decides which to exclude based on the exc.list list in the working script
  
  tmp.dat$Size <- floor(tmp.dat[,sz.lab]/rnding)*rnding
  
  if(by_yr)
  {
    tab.dat <- aggregate(tmp.dat$freq, by=list(yr = tmp.dat[,"yr"], Size = tmp.dat$Size), FUN=sum) # aggregates number of samples at each size by year and fleet  
  } else {
    tab.dat <- aggregate(tmp.dat$freq, by=list(yr = tmp.dat[,"yrqtr"], Size = tmp.dat$Size), FUN=sum) # aggregates number of samples at each size by year and fleet     
  }        
  
#   tab.dat$ind <- 1:dim(tab.dat)[1]
#   
#   new.ind <- rep(tab.dat$ind,tab.dat$x)   # Makes a variable that indicates how many samples there are for each size in that year/fleet
#   new.dat <- tab.dat[match(new.ind, tab.dat$ind),]   # Expands the dataset from tab.dat which is aggregated by length to a full dataset where each row is an individual fish
#   
#   #new.dat$JSize <- jitter(new.dat$Size, 3)
#   new.dat$RSize <- floor(new.dat$Size/rnding)*rnding
#   
#   p <- ggplot(data=new.dat, aes(x=yr, y=RSize, group=yr)) +
#               # geom_jitter(height=0, colour=alpha("black",0.002)) +
#               geom_violin(colour="transparent", fill=alpha("black",0.5), scale="width") +
#               xlab("Year") + ylab("Length (cm)") + xlim(fyr-2, lyr) +
#               scale_x_continuous(breaks=pretty_breaks(n=5)) +
#               theme(legend.key.size = unit(0.08,"cm"), legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1),
#                     legend.background = element_rect(fill="transparent"), panel.grid.major=element_blank(),
#                     panel.grid.minor=element_blank(), legend.key=element_rect(colour="transparent"))
#   

  if(relativeP) tab.dat %<>% group_by(yr) %>% mutate(Nfish = sum(x), x = x/Nfish) 

  yrseq <- seq(fyr-2, lyr, 1)
  which.mis <- which(!yrseq %in% unique(tab.dat$yr))

  for(i in which.mis)
  {
    tmp <- tab.dat[nrow(tab.dat), ]
    tmp$yr <- yrseq[i]
    tmp$x <- 0
    tmp$Size <- 999
    tab.dat <- rbind(tab.dat, tmp)
  }  
    
  p <- ggplot(data=tab.dat, aes(x=yr, y=Size)) + geom_tile(aes(fill = x)) +
              xlab("Year") + ylab("Length (cm)") + xlim(fyr-2, lyr) + ylim(10,100) +
              #scale_x_continuous(breaks=pretty_breaks(n=5)) + scale_fill_gradientn(colours = terrain.colors(6), trans = "sqrt") +
              scale_x_continuous(breaks=pretty_breaks(n=5)) + scale_fill_gradient2(low="grey", mid="blue", high="white", trans = "sqrt") +
              theme(legend.key.size = unit(0.6,"cm"), legend.title=element_blank(), legend.justification=c(0,1), legend.position=c(0,1),
                    legend.background = element_rect(fill="transparent"), panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(), legend.key=element_rect(colour="transparent"))
  
  
  p
}



