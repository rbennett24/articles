# Before running this you need to run
# loess_plots_Irish_JPhon.R
# all the way up to (and including) the definition of the function plotPolarFits().

library(nabor) # For getting nearest neighbor

CbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#########
# Function for adding polar coordinates to raw tracings in X,Y space,
# assuming that the origin has already been determined (as in e.g. edgetrak.minmax)
add_polar_coords <- function(df){
  
  # Re-center data w.r.t. origin
  X.new <- df$origin.X - df$X
  Y.new <- df$Y - df$origin.Y # Y-axis is inverted
  
  # Transform to polar coordinates and substitute.
  pi <- 3.14159265359
  df$th<- pi + atan2(Y.new, X.new) # Angle
  df$r<- sqrt(X.new^2 + Y.new^2) # Radius length
  return(df)
}


#########
# Define a function for computing RMS distance over pairs of raw tracings belonging to a particular comparison condition.
# only works for columns/factors with exactly two levels
# Note that knn(a,b) actually asks: for each point in b, what's its nearest neighbor in a?
rmssd.allpaired.samples<-function(df,keycol,trim=T){
  
  df<-data.frame(df)
  flevels <- unique(df[,keycol])
  
  v1.reps<-unique(df[df[,keycol]==flevels[1],]$rep)
  v2.reps<-unique(df[df[,keycol]==flevels[2],]$rep)
  
  RMSSD<-data.frame(matrix(nrow=0,ncol=2))
  
  for (v1.r in v1.reps){
    for (v2.r in v2.reps){
      
      v1<-df[df[,keycol]==flevels[1],]
      v2<-df[df[,keycol]==flevels[2],]
      
      v1.rep.data<-subset(v1,rep==v1.r)
      v2.rep.data<-subset(v2,rep==v2.r)

      # If you only want to compute nearest-neighbor comparisons over regions in which 
      # the two curves overlap in polar space.
      # Assumes input has a column th for polar angle.
      if (trim==T){
        
        low.angle.limit <- max(min(v1.rep.data$th,na.rm=T),
                               min(v2.rep.data$th,na.rm=T),na.rm=T)

        high.angle.limit <- min(max(v1.rep.data$th,na.rm=T),
                                max(v2.rep.data$th,na.rm=T),na.rm=T)
        
        v1.rep.data<-subset(v1.rep.data,
                            th <= high.angle.limit & th >= low.angle.limit)
        
        v2.rep.data<-subset(v2.rep.data,
                            th <= high.angle.limit & th >= low.angle.limit)

      }
      
      abneigh<-knn(v1.rep.data[,c("X","Y")], v2.rep.data[,c("X","Y")], k=1)
      abdist2sum<-sum(abneigh$nn.dists^2)
      
      baneigh<-knn(v2.rep.data[,c("X","Y")], v1.rep.data[,c("X","Y")], k=1)
      badist2sum<-sum(baneigh$nn.dists^2)
      
      RMSSD.paired<-sqrt((abdist2sum+badist2sum)/(nrow(v1.rep.data)+nrow(v2.rep.data)))

      RMSSD.paired.vec<-c(RMSSD.paired,paste0(v1.r,".",v2.r))
      
      RMSSD<-rbind(RMSSD,RMSSD.paired.vec)
      
    }
  }
  
  colnames(RMSSD)<-c("RMSSD","Pair.Index")
  RMSSD$RMSSD<-as.numeric(RMSSD$RMSSD)
  RMSSD$Pair.Index<-as.factor(RMSSD$Pair.Index)
  return(RMSSD)
}





#########
# Function for making a list of nearest-neighbor coordinates, so you can plot the method.
# Only works in one direction at a time.
# Note that knn(a,b) actually asks: for each point in b, what's its nearest neighbor in a?
rmssdRaw.neigh.abonly<-function(v1,v2){
  
  # Get b-to-a neighbor indices
  abneigh<-knn(v1[,c("X","Y")], v2[,c("X","Y")], k=1)$nn.idx
  
  # Get b-coordinates
  x.start<-v2$X
  y.start<-v2$Y
  
  # Get nearest neighbors in a
  x.end<-v1[abneigh,]$X
  y.end<-v1[abneigh,]$Y

  return(data.frame(cbind("b.x"=x.start,"b.y"=y.start,"a.x"=x.end,"a.y"=y.end)))
}



###############
# Add polar coordinates to your raw tracings.
edgetrak.polar<-add_polar_coords(edgetrak.minmax)


# Generate RMSSD values based on *trimmed raw* data that only includes comparisons of points in overlapping regions (determined by polar angle).

# This gets complicated, because you generate up to (# of reps) ^ 2 new observations (comparisons) per condition.
# do() will store this comparisons as a column of lists, where the list includes all comparisons you generated for 
# each condition/row.
# You then need to unpack that list of comparisons using unnest() below.
edgetrak.polar <- tibble(edgetrak.polar)
rmssd.raw.polar.trim <- edgetrak.polar %>% group_by(subj,dialect,c.place,v,syll.pos,frm.pos) %>%
                                           do(sec.art.diff=rmssd.allpaired.samples(.,"sec.art")[c("RMSSD")],
                                              diff.pair=rmssd.allpaired.samples(.,"sec.art")[c("Pair.Index")])

head(rmssd.raw.polar.trim)
head(unnest(rmssd.raw.polar.trim,cols=c(sec.art.diff,diff.pair)))
rmssd.raw.polar.trim<-unnest(rmssd.raw.polar.trim,cols=c(sec.art.diff,diff.pair))
head(rmssd.raw.polar.trim)

# Split apart the Pair.Index column to get the repetition number of the velarized and palatalized
# stops in each comparison.
rmssd.raw.polar.trim <- rmssd.raw.polar.trim %>% mutate(vel.rep=substr(Pair.Index,1,1),
                                                        pal.rep=substr(Pair.Index,3,3))

# Add voicing back in as a factor.
rmssd.raw.polar.trim <- rmssd.raw.polar.trim %>% mutate(vel.voicing = case_when(
                                c.place == "labial" & v == "i" & syll.pos=="Onset" ~ "voiced",
                                #
                                c.place == "labial" & v == "i" & syll.pos=="Coda" ~ "voiced",
                                c.place == "coronal" & v == "i" & syll.pos=="Coda" ~ "voiced",
                                c.place == "labial" & v == "u" & syll.pos=="Coda" ~ "voiced",
                                c.place == "coronal" & v == "u" & syll.pos=="Coda" ~ "voiced",
                                c.place == "labial" & v == "a" & syll.pos=="Coda" ~ "voiced",
                                c.place == "dorsal" & v == "a" & syll.pos=="Coda" ~ "voiced",
                                .default = "voiceless"
                                  ),
                                #
                                #
                                pal.voicing = case_when(
                                c.place == "coronal" & v == "i" & syll.pos=="Onset" ~ "voiced",
                                c.place == "labial" & v == "u" & syll.pos=="Onset" ~ "voiced",
                                #
                                c.place == "labial" & v == "i" & syll.pos=="Coda" ~ "voiced",
                                c.place == "coronal" & v == "i" & syll.pos=="Coda" ~ "voiced",
                                c.place == "labial" & v == "u" & syll.pos=="Coda" ~ "voiced",
                                c.place == "coronal" & v == "u" & syll.pos=="Coda" ~ "voiced",
                                c.place == "labial" & v == "a" & syll.pos=="Coda" ~ "voiced",
                                c.place == "dorsal" & v == "a" & syll.pos=="Coda" ~ "voiced",
                                .default = "voiceless"
                                  )
                                )


# rmssd.raw.polar.trim <- rmssd.raw.polar.trim %>% mutate(vel.voicing=substr(Pair.Index,1,1),
#                                                         pal.voicing=substr(Pair.Index,3,3))




# ################################
# # Make a plot illustrating how RMMSD works over overlapping regions only.
# # We'll use sec.art as our comparison dimension.
# 
# 
# ######
# # Sample trimmed raw tracing plot
# 
# # Instead of filter, add coding.
# # Note the angle is called th
# sample.raw.trimmed.plot.data <- edgetrak.polar %>% group_by(subj,dialect,c.place,v,syll.pos,frm.pos) %>%
#                         mutate(low.angle.limit = max(min(th[sec.art=="Vel"],na.rm=T),
#                                                      min(th[sec.art=="Pal"],na.rm=T),na.rm=T),
#                                high.angle.limit = min(max(th[sec.art=="Vel"],na.rm=T),
#                                                       max(th[sec.art=="Pal"],na.rm=T),na.rm=T)
#                         ) %>% mutate(sec.art = case_when(!(th <= high.angle.limit & th >= low.angle.limit) ~ "Trimmed",
#                                                          TRUE ~ sec.art)) # Otherwise, leave it alone.
# 
# 
# # Check that trimming occurred
# summary(as.factor(sample.raw.trimmed.plot.data$sec.art))
# 
# 
# # Get sample comparison
# trim.sample<-subset(sample.raw.trimmed.plot.data,
#                     subj=="S1" & dialect == "Connacht" &
#                       c.place=="labial" &
#                       v == "i" &
#                       syll.pos == "Onset" & frm.pos == "end" & rep==3)
# 
# summary(as.factor(trim.sample$sec.art))
# 
# # Create nearest neighbor dataframe
# v.to.p<-rmssdRaw.neigh.abonly(subset(trim.sample,sec.art=="Pal"),
#                               subset(trim.sample,sec.art=="Vel")
# )
# 
# # Rename and reorder factors to match the example you're using.
# trim.sample$sec.art <- trim.sample$sec.art %>% fct_relevel("Vel", after = 0)
# trim.sample$sec.art <- trim.sample$sec.art %>% fct_recode("Pʲ (rep 3)" = "Pal","Pˠ (rep 3)" = "Vel")
# 
# ###
# # Plot
# 
# 
# # We redefine this in a hacky way to plot sample raw data instead.
# plotPolarFitsRaw <- function(inputdata,
#                           # The plottype value shouldn't need to be changed, but in principle it could be any
#                           # column name (e.g. "Consonant"), depending on the condition you are plotting.
#                           # Below, we define a function that produces a column called "obs" which includes information
#                           # about the number of repetitions for each kind of comparison being carried out.
#                           # This function is basically parasitic on that function, which mostly serves to smuggle in
#                           # the count of observations into the labels used for each comparison type.
#                           # Still, the plottype parameter is still useful if you want to group data by some other column,
#                           # and can be used for troubleshooting code or dataframes (for example).
#                           plottype="obs",
#                           plotmaintitle="",
#                           plotsubtitle="",
#                           dialectFacet=F,
#                           radialGrid=T){
# 
#   # Create base plot, without any data on it.
#   ultPlot <- ggplot(data=inputdata,
#                     aes_string(x="X",
#                                y="Y",
#                                group=plottype,
#                                col=plottype,
#                                lty=plottype)
#   )
# 
# 
#   # Plot origin and spokes if desired.
#   # We do this first so that it sits *under* any data we decide to plot.
#   # You don't necessarily need to call origins.df here --- the same information is now in inputdata = plotting.data
#   # But since origins.df just has one row per speaker it's a bit neater to use it here, since it already exists.
# 
#   if (radialGrid==T){
#     # Add point showing hypothesized origin.
#     if (dialectFacet==F){
#       spk.origin<-subset(origins.df,
#                          subj==unique(inputdata$subj) &
#                            dialect==unique(inputdata$dialect))
#     } else if (dialectFacet==T) {
#       spk.origin<-origins.df
#     }
# 
#     ultPlot <- ultPlot + geom_point(inherit.aes = F, # Add the origin.
#                                     data=spk.origin,
#                                     aes(x=origin.X,
#                                         y=origin.Y),
#                                     na.rm=TRUE,
#                                     alpha=0.5)+
#       # Right edge of polar angle
#       geom_segment(inherit.aes = F,
#                    data=spk.origin,
#                    aes(x=origin.X,
#                        y=origin.Y,
#                        xend=r.edge.x,
#                        yend=r.edge.y),
#                    na.rm=TRUE,
#                    alpha=0.5)+
#       # Left edge of polar angle
#       geom_segment(inherit.aes = F,
#                    data=spk.origin,
#                    aes(x=origin.X,
#                        y=origin.Y,
#                        xend=l.edge.x,
#                        yend=l.edge.y),
#                    na.rm=TRUE,
#                    alpha=0.5)+
#       # Vertical line parallel to Y-axis
#       # geom_spoke(inherit.aes = F,
#       #            data=spk.origin,
#       #            lwd=1.25,
#       #            lty="dashed",
#       #            aes(x=origin.X,
#       #                y=origin.Y,
#       #                radius = spokeLength,
#       #                angle = 90*pi/180), # Need to convert from degrees to radians
#       #                na.rm=TRUE,
#       #                alpha=0.5)+
#     geom_spoke(inherit.aes = F,data=spk.origin,lty="dashed",
#                aes(x=origin.X,y=origin.Y,radius=spokeLength,angle=angleWidth/4+angleStart),
#                na.rm=TRUE,
#                alpha=0.3)+
#       geom_spoke(inherit.aes = F,data=spk.origin,lty="dashed",
#                  aes(x=origin.X,y=origin.Y,radius=spokeLength,angle=2*angleWidth/4+angleStart),
#                  na.rm=TRUE,
#                  alpha=0.3)+
#       geom_spoke(inherit.aes = F,data=spk.origin,lty="dashed",
#                  aes(x=origin.X,y=origin.Y,radius=spokeLength,angle=3*angleWidth/4+angleStart),
#                  na.rm=TRUE,
#                  alpha=0.3)
# 
#   }
# 
#   ######
#   # Add data plots
#   #
#   # Add confidence intervals.
#   # You've tried to mimic the default aesthetics of ggplot's
#   # geom_smooth(method=loess) (derived from geom_ribbon() for CIs), which is what you'd
#   # be relying on if you didn't have to do all of this polar <=> Cartesian inter-translation
#   # here.
#   # If you want you could add color = plottype to the aesthetic call here and remove color="grey60" if you want
#   # the CI lines to be the same color as the main line for the smoothed estimate.
#   # If you do that you probably need to use aes_string, and enclose the x/y values in aes_string() in quotes.
#   #
#   # We don't want confidence intervals for conditions with only one repetition, so we replaced those with NA above.
#   # Here we add na.rm = T so that we don't get warnings when we plot such conditions.
#   ultPlot <- ultPlot +
#     # geom_path(aes(x=X.fit,y=CI.hi),lwd=0.75,alpha=0.4,color="grey60", show.legend = F, na.rm=T, lty = "solid")+
#     # geom_path(aes(x=X.fit,y=CI.lo),lwd=0.75,alpha=0.4,color="grey60", show.legend = F, na.rm=T, lty = "solid")+
#     #
#     # Add main fit line on top of confidence lines so that it stands out more than the CIs
#     # when the two are heavily overlapping.
#     geom_path(lwd=2,na.rm=T)
# 
# 
#   #####
#   # Add aesthetics
#   ultPlot <- ultPlot +
#     ggtitle(plotmaintitle)+
#     labs(subtitle=plotsubtitle)+
#     xlab("Backness (normalized)")+
#     ylab("Height (normalized)")+
# 
#     theme_bw(base_size = 22)+
#     theme(legend.key.width=unit(4,"line"),
#           legend.key.height=unit(1,"line"),
#           text = element_text(family = "Charis SIL"),
#           legend.title = element_blank(),
#           legend.text = element_text(size=20)
#     )+
# 
#     scale_color_manual(values=CbbPalette)+
#     coord_equal() # This is really important --- it guarantees that the X/Y axis will be plotted at the same scale.
# 
#   # facet_wrap below is what gets the all-speaker charts to come out together.
# 
#   # TO DO:
#   # Adjust these options if you end up re-centering each speaker's curves in the same space.
#   # You probably want to simplify/merge these options, and/or shift them upward?
#   if (dialectFacet==F){
#     # Reposition the legend.
#     ultPlot <- ultPlot+theme(
#       legend.position = c(.01, .99),
#       legend.justification = c("left", "top")
#     )+
#       # Set plot limits
#       scale_y_reverse(limits=c(0.75,-0.2),breaks=seq(-0.2,0.75,0.1))+
#       scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
#       # Add big labels
#       annotate("text",x=0.05,y=0.75,label="BACK",size=10,family="Charis SIL",fontface="bold")+
#       annotate("text",x=0.9,y=0.75,label="FRONT",size=10,family="Charis SIL",fontface="bold")
# 
#   } else if (dialectFacet==T){
#     # Add faceting
#     ultPlot <- ultPlot+facet_wrap(dialect~subj, nrow=2)+theme(strip.text = element_text(face = "bold",size=32))+
#       # Set plot limits
#       scale_y_reverse(limits=c(0.85,-0.1),breaks=seq(-0.1,0.75,0.1))+
#       scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+
#       # Add small labels
#       annotate("text",x=0.2,y=0.8,label="BACK",size=10,family="Charis SIL",fontface="bold")+
#       annotate("text",x=0.75,y=0.8,label="FRONT",size=10,family="Charis SIL",fontface="bold")
#   }
# 
#   return(ultPlot)
# 
# }
# 
# 
# 
# p<-plotPolarFitsRaw(subset(trim.sample,sec.art!="Trimmed"),plottype = "sec.art")
# p
# sample.trim<-p+geom_point(inherit.aes = F, data=subset(trim.sample,sec.art=="Trimmed"),color="firebrick",shape=4,aes(x=X,y=Y),size=4)+geom_segment(inherit.aes = F, data=v.to.p,aes(x=b.x,y=b.y,xend=a.x,yend=a.y),color="skyblue3")
# sample.trim
# 
# cairo_pdf(file="trim_raw_example.pdf",
#           width=10,height=8)
#   print(sample.trim)
# dev.off()
# 
# 
# 
# # Show nearest neighbors in sample going in the other direction
# # Create nearest neighbor dataframe
# v.to.p<-rmssdRaw.neigh.abonly(subset(trim.sample,sec.art=="Pˠ (rep 3)"),
#                               subset(trim.sample,sec.art=="Pʲ (rep 3)")
# )
# 
# 
# p<-plotPolarFitsRaw(subset(trim.sample,sec.art!="Trimmed"),plottype = "sec.art")
# p
# sample.trim.2<-p+geom_point(inherit.aes = F, data=subset(trim.sample,sec.art=="Trimmed"),color="firebrick",shape=4,aes(x=X,y=Y),size=4)+geom_segment(inherit.aes = F, data=v.to.p,aes(x=b.x,y=b.y,xend=a.x,yend=a.y),color="skyblue3")
# sample.trim.2
# 
# cairo_pdf(file="trim_raw_example_2.pdf",
#           width=10,height=8)
# print(sample.trim.2)
# dev.off()
  



###########
# Start making plots with trimmed data.

# Raw data
rawRMSS.plotdata<-rmssd.raw.polar.trim

# Change factor names if you want
rawRMSS.plotdata$c.place <- rawRMSS.plotdata$c.place %>% fct_recode("Labial" = "labial",
                                                                  "Coronal" = "coronal",
                                                                  "Dorsal" = "dorsal")

rawRMSS.plotdata$v <- rawRMSS.plotdata$v %>% fct_recode("i\u02D0" = "i",
                                                        "u\u02D0" = "u",
                                                        "\u0254\u02D0" = "a")


# Recode transitions
rawRMSS.plotdata<-rawRMSS.plotdata %>% mutate(trans = case_when(syll.pos == "Coda" & frm.pos == "start" ~ "C release/VC transition",
                                                                syll.pos == "Onset" & frm.pos == "end" ~ "C release/VC transition",
                                                                TRUE ~ "no"))

transpoints.raw<-subset(rawRMSS.plotdata,trans=="C release/VC transition")


countlabels <- transpoints.raw %>% group_by(c.place,syll.pos) %>% summarise(N=n())

rms.raw.faceted<-ggplot(data=transpoints.raw)+
  geom_violin(aes(y=RMSSD,x=syll.pos,fill=syll.pos), show.legend = FALSE)+
  geom_boxplot(aes(y=RMSSD,x=syll.pos),width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(aes(y=RMSSD,x=syll.pos),fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(aes(y=RMSSD,x=syll.pos),fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos), y=0,color="black",size=10,fontface= "bold") +
  facet_grid(trans~c.place)+
  ggtitle("Difference scores for matching Cʲ vs. Cˠ pairs")+
  ylab("RMSSD scores")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=36)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_text(size=28,face="bold"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.205),breaks=seq(0,0.20,0.05))

rms.raw.faceted
cairo_pdf(file="rmssd_raw_faceted.pdf",
          width=12,height=8)
  print(rms.raw.faceted)
dev.off()


# By speaker
# Change speaker/dialect labels
transpoints.raw <- transpoints.raw %>% mutate(subjectCode = paste0(substr(transpoints.raw$dialect,0,1),
                                                                   substr(transpoints.raw$subj,2,2)
                                                                   )
                                              )

countlabels <- transpoints.raw %>% group_by(c.place,syll.pos,subjectCode,subj,dialect) %>% summarise(N=n())

rms.raw.faceted.byspkr<-ggplot(data=transpoints.raw)+
  geom_violin(aes(y=RMSSD,x=syll.pos,fill=syll.pos), show.legend = FALSE)+
  geom_boxplot(aes(y=RMSSD,x=syll.pos),width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(aes(y=RMSSD,x=syll.pos),fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(aes(y=RMSSD,x=syll.pos),fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos), y=0.005,color="black",size=6,fontface= "bold") +
  facet_grid(subjectCode~c.place)+
  ggtitle("Difference scores for matching Cʲ vs. Cˠ pairs")+
  ylab("RMSSD scores")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=36)+
  theme(strip.text = element_text(family = "Doulos SIL",size=22,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_text(size=28,face="bold"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.205),breaks=seq(0,0.20,0.05))

rms.raw.faceted.byspkr
cairo_pdf(file="rmssd_raw_faceted_byspeaker.pdf",
          width=12,height=16)
print(rms.raw.faceted.byspkr)
dev.off()





# Include vowel context
countlabels <- transpoints.raw %>% group_by(c.place,syll.pos,v) %>% summarise(N=n())

rms.raw.faceted.v<-ggplot(data=transpoints.raw)+
  geom_violin(aes(y=RMSSD,x=v,fill=v), show.legend = FALSE)+
  geom_boxplot(aes(y=RMSSD,x=v),width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(aes(y=RMSSD,x=v),fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(aes(y=RMSSD,x=v),fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=v), y=0,color="black",size=12,fontface= "bold") +
  facet_grid(syll.pos~c.place)+
  ggtitle("Difference scores for matching Cʲ vs. Cˠ pairs")+
  ylab("RMSSD scores")+xlab("Vowel context")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=48)+
  theme(strip.text = element_text(family = "Doulos SIL",size=48,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_text(size=28,face="bold"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.205),breaks=seq(0,0.20,0.05))

rms.raw.faceted.v
cairo_pdf(file="rmssd_raw_faceted_byv.pdf",
          width=20,height=14)
print(rms.raw.faceted.v)
dev.off()




##########
# Run a regression.
library(lmerTest)
library(emmeans)
library(performance) # For model statistics

# Add unique speaker codes
transpoints.raw <- transpoints.raw %>% mutate(speaker=as.factor(paste(dialect,subj,sep=".")))
head(transpoints.raw)

# Order factors to reflect desired reference levels.
transpoints.raw$c.place <- transpoints.raw$c.place %>% fct_relevel("Labial", after = length(transpoints.raw$c.place))
transpoints.raw$v <- transpoints.raw$v %>% fct_relevel("i\u02D0", after = length(transpoints.raw$v))
transpoints.raw$syll.pos <- transpoints.raw$syll.pos %>% fct_relevel("Onset", after = length(transpoints.raw$syll.pos))

# Sum code factors to reduce collinearity.
contrasts(transpoints.raw$c.place)
contrasts(transpoints.raw$c.place)<-contr.sum(length(levels(transpoints.raw$c.place))) # Sum coding
contrasts(transpoints.raw$c.place)

contrasts(transpoints.raw$v)
contrasts(transpoints.raw$v)<-contr.sum(length(levels(transpoints.raw$v))) # Sum coding
contrasts(transpoints.raw$v)

contrasts(transpoints.raw$syll.pos)
contrasts(transpoints.raw$syll.pos)<-contr.sum(length(levels(transpoints.raw$syll.pos))) # Sum coding
contrasts(transpoints.raw$syll.pos)

# Add a column for token, so that it can be used as a random effect.
transpoints.raw <- transpoints.raw %>% mutate(vel.token.code = paste(sep=".",
                                                                subjectCode,
                                                                c.place,v,syll.pos,
                                                                vel.rep
                                                                ),
                                              pal.token.code = paste(sep=".",
                                                                    subjectCode,
                                                                    c.place,v,syll.pos,
                                                                    pal.rep
                                                                    )
                                              )

# Add factor for same/different voicing, and sum code it
transpoints.raw <- transpoints.raw %>% mutate(same.voicing = case_when(pal.voicing==vel.voicing ~ "Same",
                                                                       .default = "Different"))
transpoints.raw$same.voicing<-factor(transpoints.raw$same.voicing)
contrasts(transpoints.raw$same.voicing)
transpoints.raw$same.voicing <- transpoints.raw$same.voicing %>% fct_relevel("Different", after = length(transpoints.raw$same.voicing))
contrasts(transpoints.raw$same.voicing)<-contr.sum(length(levels(transpoints.raw$same.voicing))) # Sum coding
contrasts(transpoints.raw$same.voicing)


# Center and scale repetition
transpoints.raw <- transpoints.raw %>% mutate(pal.rep = scale(as.numeric(pal.rep)),
                                              vel.rep = scale(as.numeric(vel.rep))
                                              )

# Fit a mixed effects model
base.m<-lmer(data = transpoints.raw, RMSSD ~ (c.place + syll.pos + v)^3 +
                                          (vel.rep + pal.rep) +
                                          (1+syll.pos|speaker) + 
                                          (1|pal.token.code) + (1|vel.token.code),
                                          REML = F)
# Use REML=F for log-likelihood comparison in step-down model reduction.
# Adding (1+c.place|speaker) or (1+v|speaker) leads to failures in model convergence.

anova(base.m)
performance::check_collinearity(base.m)

# Model reduction
# Reduce fixed-effects predictors
an.rev<-function(m){anova(m)[order(anova(m)[5]),]} # Shorthand for sorting anova by F-value (increasing)
# coef(summary(base.m))[order(abs(coef(summary(base.m))[,1]),decreasing=T),]

an.rev(base.m)
m.r1<-update(base.m,~.-c.place:syll.pos:v) # Significant
performance::test_performance(base.m,m.r1)

an.rev(base.m)
m.r1<-update(base.m,~.-pal.rep) # Dropped
performance::test_performance(base.m,m.r1)

an.rev(m.r1)
m.r2<-update(m.r1,~.-vel.rep) # Significant, but just *barely*!
performance::test_performance(base.m,m.r1,m.r2)

an.rev(m.r1)
m.r2<-update(m.r1,~.-c.place:syll.pos:v) # Still significant
performance::test_performance(base.m,m.r1,m.r2)

# Store final model
fm <- m.r1
performance::check_collinearity(fm) # Low collinearity...but *very* wide CIs in some cases (vel.rep, c.place)


###########
# Check normality of residuals
plot(performance::check_normality(fm),type = "density")

cor(qqnorm(residuals(fm))$x,
    residuals(fm))
qqnorm(residuals(fm))
qqline(residuals(fm))

summary(residuals(fm))
ggplot(data=data.frame("resid"=residuals(fm)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)

# Check collinearity and other model statistics
performance::check_collinearity(fm)  # Low collinearity
# performance::check_model(fm) # Slow, generates massive plot.
performance::model_performance(fm,estimator="ML") # r^2 etc.


#########
# Prep model results for LaTeX
# Coding and reference levels
contrasts(transpoints.raw$c.place)
contrasts(transpoints.raw$syll.pos)
contrasts(transpoints.raw$v)

# Get model summary
anova(fm)
summary(transpoints.raw$RMSSD)
sd(transpoints.raw$RMSSD)

library(xtable)
res.table <- as.data.frame(round(coef(summary(fm)),6))
res.table

# Replace column names
res.table <- res.table%>%rownames_to_column()
colnames(res.table)<-c("Predictor","Estimate","SE","df","t","p.numeric")

# Replace predictor values with more informative names
# Coding and reference levels
contrasts(transpoints.raw$c.place)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'c.place1', 'Coronal'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'c.place2', 'Dorsal'))
res.table

contrasts(transpoints.raw$syll.pos)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'syll.pos1', 'Coda'))
res.table

contrasts(transpoints.raw$v)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v1', '\\ipa{[u:]}'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v2', '\\ipa{[O:]}'))
res.table

res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'vel.rep', 'Repetition ([C\\vel])'))
res.table

# Clean up p-values
res.table <- res.table %>% mutate(p = case_when((p.numeric < 0.05 & p.numeric >= 0.01) ~ "< .05*",
                                                (p.numeric < 0.01 & p.numeric >= 0.001) ~ "< .01*",
                                                (p.numeric < 0.001) ~ "< .001*",
                                                .default = substr(round(p.numeric,2),2,4)
                                                )
                                                )

res.table <- res.table %>% select(-c(p.numeric,df))

# Adjust significant digits
res.table <- res.table %>% mutate(t = as.character(round(t,2)))


print(xtable(res.table,digits=4,
             sanitize.colnames.function = identity,
             sanitize.rownames.function = identity,
             sanitize.text.function = identity),
      include.rownames=FALSE)


###
# Use emmeans for post-hoc comparisons.
# https://cran.r-project.org/web/packages/emmeans/index.html
# Interaction plots:
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

# Simple terms
c.place.emm <- emmeans(fm,"c.place")
c.place.emm
contrast(c.place.emm)
pairs(c.place.emm)

syll.pos.emm <- emmeans(fm,"syll.pos")
syll.pos.emm
contrast(syll.pos.emm)
pairs(syll.pos.emm)

v.emm <- emmeans(fm,"v")
v.emm
contrast(v.emm)
pairs(v.emm)

# Interaction terms (2-way)
cplace.syllpos.int.emm <- emmeans(fm,c("c.place","syll.pos"))
cplace.syllpos.int.emm
contrast(cplace.syllpos.int.emm)
pairs(cplace.syllpos.int.emm,by="c.place") # Pairwise tests by group.
pairs(cplace.syllpos.int.emm,by="syll.pos")

syllpos.v.int.emm <- emmeans(fm,c("syll.pos","v"))
syllpos.v.int.emm
contrast(syllpos.v.int.emm)
pairs(syllpos.v.int.emm,by="v")
# emmeans(fm,c("v","syll.pos"))
pairs(syllpos.v.int.emm,by="syll.pos")

cplace.v.int.emm <- emmeans(fm,c("c.place","v"))
cplace.v.int.emm
contrast(cplace.v.int.emm)
pairs(cplace.v.int.emm,by="c.place") # Pairwise tests by group.
pairs(cplace.v.int.emm,by="v")

# More complex groupings
cplace.v.syllpos.int.emm <- emmeans(fm,c("v","c.place","syll.pos"))
cplace.v.syllpos.int.emm
contrast(cplace.v.syllpos.int.emm)
pairs(cplace.v.syllpos.int.emm,by=c("c.place","syll.pos")) # Pairwise tests by group.
# pairs(cplace.v.syllpos.int.emm,by=c("c.place","v")) # Pairwise tests by group.


###########
# Check some relative variances.
library(broom)
library(HH) # For Brown-Forsythe test. There are lots of function name clashes here, so be careful. Shouldn't be an issue, though.
            # The Brown-Forsythe test is supposed to be e.g. more robust to violations of normality assumptions than Levene's F-test.

# We compute 14 BF tests in the paper, so the adjusted alpha following stepwise Holm-Bonferroni correction would be:
ntests <- 14
alpha <- 0.05
hm.alpha <- 0.05/seq(ntests,1)
hm.alpha
min(hm.alpha)

transpoints.raw %>% group_by(c.place) %>% summarize(SD=sd(RMSSD),
                                                    mean=mean(RMSSD))

hov(RMSSD~c.place, data=subset(transpoints.raw,c.place!="Coronal"))
hov(RMSSD~c.place, data=subset(transpoints.raw,c.place!="Dorsal"))
hov(RMSSD~c.place, data=subset(transpoints.raw,c.place!="Labial"))

transpoints.raw %>% group_by(syll.pos,c.place) %>% summarize(SD=sd(RMSSD),
                                                             mean=mean(RMSSD))

subset(transpoints.raw,c.place!="Coronal") %>% group_by(syll.pos) %>% do(tidy(hov(RMSSD~c.place, data=.)))
subset(transpoints.raw,c.place!="Dorsal") %>% group_by(syll.pos) %>% do(tidy(hov(RMSSD~c.place, data=.)))
subset(transpoints.raw,c.place!="Labial") %>% group_by(syll.pos) %>% do(tidy(hov(RMSSD~c.place, data=.)))

detach(package:HH)


######################
# Some additional analyses of inter-speaker variation
# See also: https://rpkgs.datanovia.com/rstatix/reference/t_test.html
# https://www.datanovia.com/en/blog/how-to-perform-t-test-for-multiple-groups-in-r/

# Speaker, across onset vs. coda
# Get descriptive statistics
transpoints.raw %>% group_by(speaker) %>% summarize(onset = mean(RMSSD[syll.pos=="Onset"]),
                                                    coda = mean(RMSSD[syll.pos=="Coda"]),
                                                    syll.pos.diff = onset - coda
                                                   )
# Run t-tests (where estimates reflect coda - onset instead of onset - coda)
ts <- transpoints.raw %>% group_by(speaker) %>% do(tidy(t.test(RMSSD~syll.pos, data=.)))
ts <- ts %>% dplyr::select(c("speaker","estimate","p.value")) %>% mutate(p.value = round(p.value,4),
                                                                                     sig = case_when(p.value < 0.05 ~ "*",
                                                                                                     .default = ""))
ts

# Speaker, across syllable position, within each C place
# Get descriptive statistics
transpoints.raw %>% group_by(speaker,c.place) %>% summarize(onset = mean(RMSSD[syll.pos=="Onset"]),
                                                    coda = mean(RMSSD[syll.pos=="Coda"]),
                                                    syll.pos.diff = onset - coda
                                                    )
# Run t-tests (where estimates reflect coda - onset instead of onset - coda)
ts.cpl <- transpoints.raw %>% group_by(speaker,c.place) %>% do(tidy(t.test(RMSSD~syll.pos, data=.)))
ts.cpl <- ts.cpl %>% dplyr::select(c("speaker","c.place","estimate","p.value")) %>% mutate(p.value = round(p.value,4),
                                                                                     sig = case_when(p.value < 0.05 ~ "*",
                                                                                                     .default = ""))
ts.cpl
ts.cpl %>% arrange(c.place,speaker)


# Speaker, across C place, within each syllable position
# Filter to get only those speakers who have data from all of the C places you're comparing

# Coronals vs. labials
# Get descriptive statistics
transpoints.raw %>% group_by(speaker,syll.pos) %>% summarize(cor = mean(RMSSD[c.place=="Coronal"]),
                                                            lab = mean(RMSSD[c.place=="Labial"]),
                                                            cpl.diff = cor - lab
                                                            ) %>% filter(!is.na(cpl.diff))

syll.spk <- transpoints.raw %>% filter(speaker %in% c("Connacht.S2","Munster.S1", "Munster.S2","Ulster.S1","Ulster.S2") & c.place != "Dorsal")
# Run t-tests (where estimates reflect coronal - labial)
ts.syll <- syll.spk %>% group_by(speaker,syll.pos) %>% do(tidy(t.test(RMSSD~c.place, data=.)))
ts.syll <- ts.syll %>% dplyr::select(c("speaker","syll.pos","estimate","p.value")) %>% mutate(p.value = round(p.value,4),
                                                                                 sig = case_when(p.value < 0.05 ~ "*",
                                                                                                 .default = ""))
ts.syll
ts.syll %>% arrange(syll.pos,speaker)

detach(package:broom)


###################
# Re-make some plots looking at C releases rather than C release (onset) vs. C start (coda).
###################

# Recode transitions
rawRMSS.plotdata<-rawRMSS.plotdata %>% mutate(trans = case_when(frm.pos == "end" ~ "C release",
                                                                TRUE ~ "no"))

transpoints.raw<-subset(rawRMSS.plotdata,trans=="C release")


countlabels <- transpoints.raw %>% group_by(c.place,syll.pos) %>% summarise(N=n())

rms.raw.faceted.rel<-ggplot(data=transpoints.raw)+
  geom_violin(aes(y=RMSSD,x=syll.pos,fill=syll.pos), show.legend = FALSE)+
  geom_boxplot(aes(y=RMSSD,x=syll.pos),width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(aes(y=RMSSD,x=syll.pos),fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(aes(y=RMSSD,x=syll.pos),fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos), y=0,color="black",size=10,fontface= "bold") +
  facet_grid(trans~c.place)+
  ggtitle("Difference scores for matching Cʲ vs. Cˠ pairs")+
  ylab("RMSSD scores")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=36)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_text(size=28,face="bold"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.205),breaks=seq(0,0.20,0.05))

rms.raw.faceted.rel
cairo_pdf(file="rmssd_raw_faceted_release.pdf",
          width=12,height=8)
print(rms.raw.faceted.rel)
dev.off()





# Include vowel context
countlabels <- transpoints.raw %>% group_by(c.place,syll.pos,v) %>% summarise(N=n())

rms.raw.faceted.v.rel<-ggplot(data=transpoints.raw)+
  geom_violin(aes(y=RMSSD,x=v,fill=v), show.legend = FALSE)+
  geom_boxplot(aes(y=RMSSD,x=v),width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(aes(y=RMSSD,x=v),fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(aes(y=RMSSD,x=v),fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=v), y=0,color="black",size=12,fontface= "bold") +
  facet_grid(syll.pos~c.place)+
  ggtitle("Difference scores for matching Cʲ vs. Cˠ pairs")+
  ylab("RMSSD scores")+xlab("Vowel context")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=48)+
  theme(strip.text = element_text(family = "Doulos SIL",size=48,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_text(size=28,face="bold"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.205),breaks=seq(0,0.20,0.05))

rms.raw.faceted.v.rel
cairo_pdf(file="rmssd_raw_faceted_byv_release.pdf",
          width=20,height=14)
print(rms.raw.faceted.v.rel)
dev.off()
