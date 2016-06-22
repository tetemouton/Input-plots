# Function that plots a crude summary of a .tag file - No. of releases, recaptures and recapture rate
plot_tag_summary = function(tag = read.tag("//penguin/assessments/skj/2016/assessment/Data_preperation/MFDGR/SKJ_tempory.tag"), time.scl = "year")
{
    theme_set(theme_bw())  
  
    # Take the .tag file, extract release lengths and add column names
    relmat <- tag$rel.lens %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint)) %>%
                               as.data.frame()
    
    # Set up a big data.frame that has info on release details
    tagdat <- data.frame(group = 1:tag$hd$nrel,
                         Program = tag$tagprog,
                         region = tag$rel$reg,
                         year = tag$rel$y,
                         mon = tag$rel$m,
                         qtr = (tag$rel$m + 1)/3,
                         Nrel = apply(relmat, 1, sum))
    
    # Aggregate recapture and add them to the tagdat data.frame
    nrec <- tag$rel.recov %>% group_by(grp) %>% summarise(recN = sum(n))   
    tagdat$Nrec <- nrec$recN[match(tagdat$group, nrec$grp)]               # Total number of recaptures for a release event
    tagdat$yrqtr <- tagdat$year + tagdat$qtr/4 - 0.125
    tagdat$Prec <- tagdat$Nrec/tagdat$Nrel                                # Proportion of releases that are recaptured
    tagdat$Region <- paste("Region", tagdat$region)
    
    # Define time scale to be plotted based on the argument list
    tagdat$time <- tagdat[, time.scl]
    
    # Aggregate to the correct scale
    regdat <- tagdat %>% group_by(time, Region) %>% summarise(Nrel = sum(Nrel)) %>% as.data.frame()
    
    reg.pl <- ggplot(data=regdat, aes(x=time, y=Nrel/1000)) + geom_bar(stat="identity", width=1, colour="black", fill="#99CCFF") + ggtitle("Releases by region") +
                     scale_colour_brewer(palette="Set1") + facet_wrap(~ Region, ncol=1, scales = "free_y") + ylab("No. fish (x 1000)") +
                     theme(legend.position="none",
                           plot.title=element_text(colour=grey(0.3), size = 12),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank())
    
    
    tmpdat <- tagdat %>% group_by(Program, yrqtr) %>% summarise(Trel = sum(Nrel), Trec = sum(Nrec)) %>%
                         mutate(Prec = Trec/Trel)
    
    
    rel.pl <- ggplot(data=tmpdat, aes(x=yrqtr, y=Trel/1000)) + geom_bar(stat="identity", width=0.25, colour="black", fill="#6699CC") +
                     scale_fill_brewer(palette="Set2") + facet_wrap(~ Program, scales = "free_y") + ylab("No. fish (x 1000)") +
                     scale_x_continuous(breaks=pretty_breaks(n=3)) + scale_y_continuous(breaks=pretty_breaks(n=3)) + ggtitle("Releases") +
                     theme(legend.position="none",
                           plot.title=element_text(colour=grey(0.3), size = 12),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
                           strip.text.x = element_text(size = 10, lineheight=3),
                           axis.title.x = element_blank())
    
    rec.pl <- ggplot(data=tmpdat, aes(x=yrqtr, y=Trec/1000)) + geom_bar(stat="identity", width=0.25, colour="black", fill="#6699CC") +
                     scale_fill_brewer(palette="Set2") + facet_wrap(~ Program, scales = "free_y") + ylab("No. fish (x 1000)") +
                     scale_x_continuous(breaks=pretty_breaks(n=3)) + scale_y_continuous(breaks=pretty_breaks(n=3)) + ggtitle("Recaptures") +
                     theme(legend.position="none",
                           plot.title=element_text(colour=grey(0.3), size = 12),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
                           strip.text.x = element_text(size = 10, lineheight=3),
                           axis.title.x = element_blank())
    
    Prec.pl <- ggplot(data=tmpdat, aes(x=yrqtr, y=Prec)) + #geom_hline(yintercept=1, colour=alpha("grey",0.5), size=1) +
                      scale_colour_brewer(palette="Set2") + geom_bar(stat="identity", width=0.25, colour="black", fill="#6699CC") +
                      facet_wrap(~ Program, scales = "free_y") + ylab("P(recaptured)") + ggtitle("Recapture rate") +
                      scale_x_continuous(breaks=pretty_breaks(n=3)) + scale_y_continuous(breaks=pretty_breaks(n=3)) +
                      theme(legend.position="none",
                            plot.title=element_text(colour=grey(0.3), size = 12),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
                            strip.text.x = element_text(size = 10, lineheight=3),
                            axis.title.x = element_blank())
 
        pushViewport(viewport(layout = grid.layout(3,6)))   # make a grid of 3 rows by 2 columns
        print(rel.pl, vp = viewport(layout.pos.row=1, layout.pos.col=1:4))
        print(rec.pl, vp = viewport(layout.pos.row=2, layout.pos.col=1:4))
        print(Prec.pl, vp=viewport(layout.pos.row=3, layout.pos.col=1:4))        
        print(reg.pl, vp=viewport(layout.pos.row=1:3, layout.pos.col=5:6))
    
}
