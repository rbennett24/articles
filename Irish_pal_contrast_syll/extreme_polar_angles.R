library(tidyverse)

# Set directory for saving PDFs, reading in data frames
setwd("")

# Read in the data we'll use to find the point of maximum extension from the polar origin. 
# edgetrak.minmax is the data frame
load("edgetrak_output_normalized_Irish_JPhon.RData")

# Define a function for adding polar coordinates to a data frame:
savePolarCoords <- function(df){
  
  # Make a data frame storing polar origins for each speaker.
  origins.df <- df %>% group_by(Speaker) %>% 
    summarize(# origin.X = min(X)+(max(X)-min(X))/2,
      origin.X = X[which.min(Y)], # This is what Mielke does.
      # It might be more robust to X-axis variability/outliers.
      origin.Y = max(Y,na.rm=T) + diff(range(Y,na.rm = T),na.rm = T)*0.01,
      spokeLength = abs(min(Y,na.rm=T) - origin.Y),
      r.edge.y = Y[which.max(X)],
      r.edge.x = max(X,na.rm=T),
      l.edge.y = Y[which.min(X)],
      l.edge.x = min(X,na.rm=T),
      dialect=unique(dialect),
      subj=unique(subj),
      # Note that whether or not the origin comes first here again differs for X,Y
      # coordinates because the Y scale is inverted.
      angleWidth = atan2(origin.Y - l.edge.y, l.edge.x - origin.X)-
        atan2(origin.Y - r.edge.y, r.edge.x - origin.X),
      angleStart = atan2(origin.Y - r.edge.y,
                         r.edge.x - origin.X)
    )
 
  # Add origin data back in to the original dataframe.
  # This is important for the polar transformation defined below.
  df<-merge(df,
            select(origins.df,-c("dialect","subj")),
            by="Speaker")
  
  # Re-center data w.r.t. origin
  X.new <- df$origin.X - df$X
  Y.new <- df$Y - df$origin.Y # Y-axis is inverted
  
  # Transform to polar coordinates and substitute.
  pi <- 3.14159265359
  df$th<- pi + atan2(Y.new, X.new) # Angle
  df$r<- sqrt(X.new^2 + Y.new^2) # Radius length
  
  return(df)
}

polar.pts<-savePolarCoords(edgetrak.minmax)


#########
# For each token, get the point of maximum extension from origin.

# There is a tendency for the most 'extreme' point to be the first or last point on the tongue surface.
# These points frequently do not correspond to the dorsal peak in any meaningful way.
longestRadiusPts.firstpass <- polar.pts %>% group_by(dialect,subj,Consonant,sec.art.IPA,syll.pos,Vowel,rep,frm.pos) %>% 
                                            slice_max(r)

ggplot(data=longestRadiusPts.firstpass)+geom_density(aes(x=pt.num))
ggplot(data=longestRadiusPts.firstpass)+geom_density(aes(x=pt.num,fill=sec.art),alpha=0.2)


# However, some of these extreme points *do* correspond to points on the dorsum, specifically in the case of partially traced contours.
# To distinguish these cases from misleading cases, we limit our search to candidate points which are in the upper 1/3 of each speaker's data.
polar.pts.trim <- polar.pts %>% group_by(dialect,subj) %>% filter(Y < min(Y) + 0.33333333*range(Y))

longestRadiusPts <- polar.pts.trim %>% group_by(dialect,subj,Consonant,sec.art.IPA,syll.pos,Vowel,rep,frm.pos) %>%
                                                 slice_max(r)

ggplot(data=longestRadiusPts)+geom_density(aes(x=pt.num))
ggplot(data=longestRadiusPts)+geom_density(aes(x=pt.num,fill=sec.art),alpha=0.2)



# Rename backness column
longestRadiusPts <- longestRadiusPts %>% rename(X.polarpeak = X,
                                                th.polarpeak = th)


# At this point, or near it, do a left join on longestRadiusPts to add any corresponding dorsal peak backness values. That should guarantee each point has a comparator.
load("tongue_peaks_Irish_JPhon.RData")
summary(Tongue.peaks$X)

Tongue.peaks <- Tongue.peaks %>% rename(X.ypeak = X,
                                        Y.ypeak = Y)

# Note that NA values for X.ypeak correspond to points which were excluded from the 
# original backness analysis.
nrow(Tongue.peaks)
nrow(longestRadiusPts)

corr.DF <- left_join(longestRadiusPts,Tongue.peaks,by=c("Speaker", "c.place", "sec.art", "v", "syll.pos", "rep", "frm.pos"))
corr.DF

ggplot(data=corr.DF)+geom_density(aes(x=Y.ypeak)) # Check the distribution of peak points.
ggplot(data=corr.DF)+geom_density(aes(x=Y.ypeak,fill=sec.art),alpha=0.2)


# For Munster 2, it looks like the highest point of the tongue is at the tongue tip instead of the tongue body for [ta], [tu] and [at]. And maybe some others. So we exclude it to be safe.
corr.DF <- corr.DF %>% filter(!(Speaker == "Munster, Speaker S2" & c.place=="coronal"))
nrow(corr.DF)

summary(corr.DF$X.ypeak)
summary(corr.DF$X.polarpeak)
summary(corr.DF$th.polarpeak)


# Correlate backness of 'peak' position as computed by both methods
corr.vals <- corr.DF %>% group_by(dialect,subj) %>% summarize(corr = round(cor(X.ypeak, X.polarpeak),2))
corr.vals

corr.vals.all <- corr.vals

backness.corr.plot<-ggplot(data=corr.DF) + 
                    geom_point(aes(x=X.ypeak,y=X.polarpeak),alpha=0.75) +
                    geom_smooth(aes(x=X.ypeak,y=X.polarpeak),method="lm") +
                    facet_grid(dialect~subj) +
                    theme_bw(base_size = 30) +
                    labs(x="Backness of dorsal peak", y="Backness of point farthest from polar origin") +
                    geom_label(data=corr.vals,aes(label=corr),x=0.225,y=0.9,size=8.5)

backness.corr.plot

output_file<-"backness_polar_cart_corr.pdf"
cairo_pdf(file=output_file,
          width=12,height=8)
  backness.corr.plot
dev.off()


# Correlate backness of 'peak' position as computed by dorsal peak, against angle of point furthest from origin.
corr.vals <- corr.DF %>% group_by(dialect,subj) %>% summarize(corr = round(cor(X.ypeak, th.polarpeak),2))
corr.vals

corr.vals.all <- rbind(corr.vals.all,corr.vals)
corr.vals.all
range(abs(corr.vals.all$corr))
mean(abs(corr.vals.all$corr))
median(abs(corr.vals.all$corr))
sd(abs(corr.vals.all$corr))

backness.angle.corr.plot <- ggplot(data=corr.DF) +
                            geom_point(aes(x=X.ypeak,y=th.polarpeak),alpha=0.75) +
                            geom_smooth(aes(x=X.ypeak,y=th.polarpeak),method="lm") +
                            facet_grid(dialect~subj)+theme_bw(base_size = 30) +
                            labs(x="Backness of dorsal peak", y="Angle of point farthest from polar origin") +
                            geom_label(data=corr.vals,aes(label=corr),x=0.225,y=0.725,size=8.5)

backness.angle.corr.plot

output_file<-"backness_polarangle_corr.pdf"
cairo_pdf(file=output_file,
          width=12,height=8)
  backness.angle.corr.plot
dev.off()
