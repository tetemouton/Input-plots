# Function that plots a summary of the length compositions of released fish by program and region and
# A movement matrix showing observed movement transition rates between regions for tagged fish
    plot_sizes_movement_tags = function(fdesc = read.table("C:/skj/2016/assessment/Setup/fdesc.txt", header=TRUE),
                                        tag = read.tag("//penguin/assessments/skj/2016/assessment/Data_preperation/MFDGR/SKJ_tempory.tag"))
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
        recmat <- tag$rel.recov %>% group_by(grp, len) %>% summarise(recN = sum(n)) %>%
                                    dcast(grp ~ len, value.var="recN")
        
        tmpmat <- matrix(NA, nrow=tag$hd$nrel, ncol=tag$hd$nint) %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint))
        
        tmpmat[recmat$grp, match(colnames(recmat)[-1], colnames(tmpmat))] <- as.matrix(recmat[,-1])
        
        tagdat <- cbind(tagdat, relmat, tmpmat)
        
        tagdat[is.na(tagdat)] <- 0
        tagdat$Region <- paste("Region", tagdat$region)
    
        tmprelL <- tagdat[, 1:(tag$hd$nint + 12)] %>% melt(id.vars=1:12, variable.name="len") %>%
                                                      group_by(Program, region, len) %>% 
                                                      summarise(N = sum(value)) %>% mutate(Category = "Release") %>%
                                                      as.data.frame()
    
        tmprecL <- tagdat[, c(1:12,(tag$hd$nint + 13):dim(tagdat)[2])] %>% melt(id.vars=1:12, variable.name="len") %>%
                                                                           group_by(Program, region, len) %>%
                                                                           summarise(N = sum(value)) %>%
                                                                           mutate(Category = "Recapture") %>% as.data.frame
    
        tmprelL <- rbind(tmprelL, tmprecL) %>% mutate(len2 = as.numeric(as.character(len)))
    
   
        relL.pl2 <- ggplot(data=tmprelL, aes(x=len2, y=N, fill=Category)) + geom_vline(xintercept=50, colour=alpha("grey",0.7), size=0.8) +
                           geom_density(stat="identity") + scale_fill_manual(values=c(alpha("blue",0.7),alpha("red",0.3))) +#geom_line() + geom_point() + #geom_point(aes(size=Nrel)) +
                           facet_grid(region ~ Program, scales="free_y") + scale_y_continuous(breaks=pretty_breaks(n=2)) +  
                           xlab("Length (cm)") + ylab("No. fish") +
                           theme(legend.position="none",
                                 legend.background = element_rect(fill="transparent"),
                                 panel.grid.major=element_blank(),
                                 panel.grid.minor=element_blank())
    
        movmat <- tag$rel.recov %>% mutate(relreg = tag$rel$reg[grp], recreg = fdesc$region[fsh]) %>% group_by(relreg, recreg) %>% summarise(recN = sum(n))
    
        regtots <- movmat %>% group_by(relreg) %>% summarise(recN = sum(recN))
    
        movmat$Proportion <- movmat$recN/regtots$recN[movmat$relreg]
    
        mat.pl <- ggplot(movmat, aes(x=relreg, y=recreg)) + geom_tile(aes(fill=Proportion), colour = "black") + 
                         scale_fill_gradient(low="white", high="red", trans="sqrt") +
                         geom_text(aes(label=recN)) + coord_equal() +
                         xlab("Release region") + ylab("Recapture region") +
                         theme(panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               axis.ticks = element_blank(),
                               panel.border = element_blank(),
                               axis.ticks.length = unit(-0.2, "cm"))
    
  
            pushViewport(viewport(layout = grid.layout(2,5)))   # make a grid of 2 rows by 2 columns
            print(mat.pl, vp = viewport(layout.pos.row=1, layout.pos.col=1:4))
            print(relL.pl2, vp = viewport(layout.pos.row=2, layout.pos.col=1:5))
}








