# Function to broadly compare two .tag files
plot_compare_tags = function(tag1 = read.tag("C:/skj/2016/assessment/Model_Runs/Old/run2014_1/skj.tag"),
                             tag2 = read.tag("//penguin/assessments/skj/2016/assessment/Data_preperation/MFDGR/SKJ_tempory.tag"),
                             fln = c("2014", "2016"), prg.fm = c("JPTP","m JP"), prg.to = "JPTP")
{
    theme_set(theme_bw())
  
    if(!is.null(prg.fm))
    {
        tag1$tagprog[tag1$tagprog %in% prg.fm] <- prg.to
        tag2$tagprog[tag2$tagprog %in% prg.fm] <- prg.to
    }
    
    
    process.tag = function(tag = tag1, flname = fln[1])
    {
        relmat <- tag$rel.lens %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint)) %>%
                                   as.data.frame()
        
        tagdat <- data.frame(group = 1:tag$hd$nrel,
                             Program = tag$tagprog,
                             region = tag$rel$reg,
                             year = tag$rel$y,
                             mon = tag$rel$m,
                             qtr = (tag$rel$m + 1)/3,
                             Nrel = apply(relmat, 1, sum))
        
        tagdat$Program <- as.character(tagdat$Program)
        
        nrec <- tag$rel.recov %>% group_by(grp) %>% summarise(recN = sum(n))
        
        tagdat$Nrec <- nrec$recN[match(tagdat$group, nrec$grp)]
        tagdat$yrqtr <- tagdat$year + tagdat$qtr/4 - 0.125
        tagdat$Prec <- tagdat$Nrec/tagdat$Nrel

        tagdat$File <- flname
        
        return(tagdat)
    }
    
    tagdat <- process.tag(tag = tag1, flname = fln[1])
    tagdat1 <- process.tag(tag = tag2, flname = fln[2])
    
    tagdat <- rbind(tagdat, tagdat1)
    
    # Plots
    # Releases
    relcom <- tagdat %>% group_by(yrqtr, File, Program) %>% summarise(Nrel = sum(Nrel)) %>%
      ggplot(aes(x=yrqtr, y=Nrel, colour=File)) + geom_line(size=0.5, aes(linetype=File)) +
      facet_wrap(~ Program, scales="free") + geom_point() + scale_colour_manual(values=c("#003399","#CC3366")) +
      xlab("") + ylab("No. releases") + scale_y_continuous(breaks=pretty_breaks(n=3)) +
      theme(legend.justification = c(0,1),
            legend.key.size = unit(0.5, "cm"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            legend.background = element_rect(fill="transparent"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))                     
    
    # Recaptures
    reccom <-tagdat %>% group_by(yrqtr, File, Program) %>% summarise(Nrec = sum(Nrec)) %>%
      ggplot(aes(x=yrqtr, y=Nrec, colour=File)) + geom_line(size=0.5, aes(linetype=File)) +
      facet_wrap(~ Program, scales="free") + geom_point() + scale_colour_manual(values=c("#003399","#CC3366")) +
      xlab("") + ylab("No. recaptures") + scale_y_continuous(breaks=pretty_breaks(n=3)) +
      theme(legend.justification = c(0,1),
            legend.key.size = unit(0.5, "cm"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            legend.background = element_rect(fill="transparent"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) 
    
    # Recaptures
    prccom <-tagdat %>% group_by(yrqtr, File, Program) %>% summarise(Nrel = sum(Nrel), Nrec = ifelse(is.na(sum(Nrec)), 0, sum(Nrec)), prec = Nrec/Nrel) %>%
      ggplot(aes(x=yrqtr, y=prec, colour=File)) + geom_line(size=0.5, aes(linetype=File)) +
      facet_wrap(~ Program, scales="free") + geom_point() + scale_colour_manual(values=c("#003399","#CC3366")) +
      xlab("") + ylab("P(recaptured)") + scale_y_continuous(breaks=pretty_breaks(n=3)) +
      theme(legend.justification = c(0,1),
            legend.key.size = unit(0.5, "cm"),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            legend.background = element_rect(fill="transparent"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm")) 
    
    # Composite figure 
    pushViewport(viewport(layout = grid.layout(3,1)))   # make a grid of 3 rows by 1 columns
    print(relcom, vp = viewport(layout.pos.row=1, layout.pos.col=1))
    print(reccom, vp = viewport(layout.pos.row=2, layout.pos.col=1))
    print(prccom, vp = viewport(layout.pos.row=3, layout.pos.col=1))
    
}








