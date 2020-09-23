######################################################
# Preliminaries and data preparation
######################################################

##################
# Load packages


library(plyr) # for data organization and manipulation
library(tidyverse)

# Load ggplot2
library(ggplot2)
library(grid)
library(scales)
library(ggpubr) # If you want to plot convex hulls
# https://www.rdocumentation.org/packages/ggpubr/versions/0.2/topics/stat_chull
# https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

##################
# Try to make plots using the DoulosSIL IPA font
library(extrafont)

##########
# This only needs to be done once on each computer, I think.
# font_import(prompt = F,pattern="DoulosSIL-R") # Import system fonts -- this can take awhile if you import them all
# loadfonts(device = "win") # I think you only need to do this once so that R imports all the needed files to a place it can draw on them?
# windowsFonts() # This will just show you which fonts are available to R -- it may be a long list!
##########


# Select color scheme for plotting.
# Color-blind friendly color palate.
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPaletteX <- c("#56B4E9", "#009E73", "#000000", "#E69F00", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
newpall<-c("#000000","#018571","#a6611a","#dfc27d","#80cdc1")
newpallx<-c("gray78","#a6611a","#018571","#80cdc1","#dfc27d")

showColor<-function(pal){
  
  hist(seq(0,length(pal)),col=pal,breaks=length(pal))
  
}

showColor(cbPalette)
showColor(cbPaletteX)
showColor(newpall)
showColor(newpallx)

yelCol = "#F0E442" # cbPalette[6]
lightBlue = "#56B4E9" # cbPalette[4]
orng  = "#E69F00" # cbPalette[3]

# Define a simple density plot function for data exploration.
simpleDensity<-function(df,datacol,title){
  ggplot(data=df)+
    geom_density(aes(x=datacol),
                 fill=lightBlue,alpha=0.25,color="black",lwd=1.25,na.rm=T)+
    theme_bw(base_size=24)+
    xlab(title)
  
}


#################
# Set some directories.
computer <- "Tiamat"
basedir<-paste0("C:\\Users\\",
                computer,
                "\\Dropbox\\Research\\Mayan\\Uspanteko\\Uspanteko_NSF_project\\")

setwd(paste0(basedir,"\\Recordings\\JIPA_forced_alignment\\To_analyze\\Measures\\"))

outdir<-paste0(basedir,"\\Articles\\JIPA_description\\Images\\")

scriptdir<-paste0(basedir,"\\Recordings\\Scripts\\Data analysis\\R_scripts\\JIPA_wordlist_2018\\")


###############################
# Formant analysis
###############################

################
# Load the text file of formant measurements.
dataDateV = "Sep22"

formant.tracks.raw<-read.csv(paste0("Usp_formant_tracks_all_spkrs_",
                                   dataDateV,
                                   ".txt"),
                            header=T,sep="\t",
                            encoding="UTF-8")



# Load in some convenience functions, including functions for Lobanov normalization.
source(paste0(scriptdir,"Usp_convenience_functions.R"))


##################################################
# Eliminate words that you do not want to plot, e.g. various
# kinds of function words, disfluencies, etc.
##################################################
write.csv(sort(unique(formant.tracks.raw$word)),"wordtypes.csv")

source(paste0(scriptdir,"Usp_cutwords.R"))

formant.tracks.cut<-subset(formant.tracks.raw,!(word %in% cutwords))

# Get rid of unstressed long vowels -- in this case, by recoding them as stressed, because the relevant example has an unstressed enclitic
subset(formant.tracks.cut,segment%in%c("AL0","IL0","EL0","OL0","UL0"))
formant.tracks.cut$segment<-revalue(formant.tracks.cut$segment,c("AL0"="AL1","EL0"="EL1","IL0"="IL1","OL0"="OL1","UL0"="UL1"))
subset(formant.tracks.cut,segment%in%c("AL0","IL0","EL0","OL0","UL0"))

# Focus on midpoint data exclusively.
formant.tracks.cut$step<-as.factor(formant.tracks.cut$step)
levels(formant.tracks.cut$step)
midpoint = 3
usp.v.mdpt<-subset(formant.tracks.cut,
                   step==midpoint
                    )


# We do Lobanov normalization here, not because we plan to use Lobanov-normalized values (though we could),
# but rather because this custom function does a good job of reshaping data in an R-friendly way (though I'm sure the code could be better, I wrote this function a *long* time ago...).
#
# This can take some time.
#
usp.v.mdpt<-lob.v.trans.onestep(usp.v.mdpt)

# The Lobanov function does strip away information about duration, so we add that back in.
durframe<-subset(formant.tracks.cut,
                 step==midpoint &
                   formant=="F1")

usp.v.mdpt$dur<-durframe$dur[match(durframe$token.code,
                                   usp.v.mdpt$token.code)]


# Add F1/F3 and F2/F3 as normalization measures -- this is what you'll actually use, in part because
# it's a vowel-by-vowel normalization measure, and thus not sensitive to the distribution of 
# vowel types in the data.
#
# This method *is*, of course, sensitive to how accurately F3 is tracked in each example.
# Earlier exploration comparing this method to Lobanov normalization suggested that this approach
# provided slightly tighter clustering, and showed the same basic outliers, suggesting that outliers
# are really measurement errors from Praat.
#
# If you really want to check normalization you can fit an lmer with and without normalization and check random slopes
# for speaker, as done in Monahan & Idsardi (2009)
# https://www.tandfonline.com/doi/full/10.1080/01690965.2010.490047?scroll=top&needAccess=true
#
usp.v.mdpt$F1.F3<-usp.v.mdpt$F1/usp.v.mdpt$F3
usp.v.mdpt$F2.F3<-usp.v.mdpt$F2/usp.v.mdpt$F3
head(usp.v.mdpt)


# Add some factors to the data frame using the independently-defined
# function usp.add.factors()
#
# This can take some time with big data frames, so it's good you're just looking at the midpoint data by this point.
#
source(paste0(scriptdir,"Usp_add_factors.R"))
usp.v.mdpt<-usp.add.factors(usp.v.mdpt)
head(usp.v.mdpt)

# Check out what the general distributions of formant values are.
ggplot(usp.v.mdpt,aes(x=F1))+geom_density(fill="gray50")+theme_bw(base_size=24)+scale_x_reverse()+coord_flip()
ggplot(usp.v.mdpt,aes(x=F2))+geom_density(fill="gray50")+theme_bw(base_size=24)+scale_x_reverse()
ggplot(usp.v.mdpt,aes(x=F3))+geom_density(fill="gray50")+theme_bw(base_size=24)

form.col<-gather(usp.v.mdpt,key = "Formant",value = "Hz",F1,F2,F3)
ggplot(form.col,aes(x=Hz,color=Formant,fill=Formant))+geom_density(alpha=0.5)+theme_bw(base_size=24)+facet_grid(v.qual~.)+scale_x_continuous(breaks=seq(0,5000,200))
rm(form.col)

# summary(usp.v.mdpt$F1)
# summary(usp.v.mdpt$F2)
# summary(usp.v.mdpt$F3)


# If you're going to normalize by F3, you should verify that it's pretty consistent within speakers, unlike F1 and F2
ggplot(usp.v.mdpt,aes(x=F3))+geom_density(fill="gray50")+facet_grid(.~speaker)+theme_bw(base_size=24)+ylim(0,0.003)+scale_x_reverse()
ggplot(usp.v.mdpt,aes(x=F1))+geom_density(fill="gray50")+facet_grid(.~speaker)+theme_bw(base_size=24)+ylim(0,0.003)+scale_x_reverse()
ggplot(usp.v.mdpt,aes(x=F2))+geom_density(fill="gray50")+facet_grid(.~speaker)+theme_bw(base_size=24)+ylim(0,0.003)+scale_x_reverse()


# Double check that F3 isn't itself overly-sensitive to vowel quality.
# To the extent that there are trends, they seem to be consistent across speakers?  
ggplot(usp.v.mdpt,aes(x=F3))+
  geom_density(aes(color=v.qual,
                   fill=v.qual),
               alpha=0.25)+
  facet_grid(.~speaker)+
  theme_bw(base_size=24)+ylim(0,0.003)

# Now check out how the normalized values are distributed.
ggplot(usp.v.mdpt,aes(x=F1.F3))+geom_density(fill="gray50")+facet_grid(.~speaker)+theme_bw(base_size=24)+ylim(0,3)+scale_x_reverse()
ggplot(usp.v.mdpt,aes(x=F2.F3))+geom_density(fill="gray50")+facet_grid(.~speaker)+theme_bw(base_size=24)+ylim(0,3)+scale_x_reverse()

# The patterns here are what you'd expect, and F1/F2 seem to be pretty well-separated, even for /u/, suggesting that Praat tracking + normalization is doing good work (though with some errors that look like F2/F3 confusions?)
form.col<-gather(usp.v.mdpt,key = "Formant",value = "Hz",F1.F3,F2.F3)
ggplot(form.col,aes(x=Hz,color=Formant,fill=Formant))+geom_density(alpha=0.5)+theme_bw(base_size=24)+facet_grid(v.qual~.)
rm(form.col)



#####################################
# Compare intrinsic F3 normalization with other normalization techniques.

# Check how correlated your F3 normalization is with the outcome of Lobanov normalization.
attach(usp.v.mdpt)
  cor.test(F1.lob,F1.F3) # Highly correlated
  cor.test(F2.lob,F2.F3) # Highly correlated
detach(usp.v.mdpt)

  
# Check how correlated your F3 normalization is with Bark and ERB transformations (which really *shouldn't* be considered normalization techniques)
attach(usp.v.mdpt)
  cor.test(hzToBark(F1),F1.F3) # Highly correlated
  cor.test(hzToBark(F2),F2.F3) # Highly correlated
  
  cor.test(hzToERB(F1),F1.F3) # Highly correlated
  cor.test(hzToERB(F2),F2.F3) # Highly correlated
detach(usp.v.mdpt)
  

# What's the dispersion like across speakers, anyway? Not huge it seems, and swamped by vowel quality variation.
sd(usp.v.mdpt$F1)
sd(usp.v.mdpt$F2)


##############
# Log-additive regression normalization
#
# The idea here is simple: some inter-talker variability in F1/F2 reflects physiology, i.e. the size of the vocal tract. If you want to normalize away from that variation specifically, you just need to scale speaker vowel spaces so that they are in the same space. This can be done by predicting observed formant values from vowel quality + speaker in a regression analysis, then transforming the original values by subtracting the estimated effect of speaker produced by the model (in essence re-scaling/sizing the vowel space---recall too that subtraction of log-transformed values is like division, i.e. ratio normalization)
#
# There are lots of apparent advantages to this method, e.g. when the data is not balanced across vowel qualities
# https://asa.scitation.org/doi/10.1121/1.5047742
# https://assta.org/proceedings/ICPhS2019/papers/ICPhS_1604.pdf
# "Log-additive regression normalization was applied to
# reduce interspeaker variation"
# Barreda, S., Nearey, T. 2017. A regression approach to
# vowel normalization for missing and unbalanced data.
# J. Acoust. Soc. Am. 142(4), 2583.


# Recode data in long format
usp.v.mdpt.long<-gather(usp.v.mdpt,
                        key = "Formant",
                        value = "log.Fx",
                        F1,F2)

usp.v.mdpt.long$log.Fx<-log(usp.v.mdpt.long$log.Fx)


# An assumption of the method as implemented here is that F1/F2 should be scaled equally by speaker physiology/vocal tract size. This is explicitly defended on other grounds by Barreda & Nearey too.
#
# Create vqual x formant interaction factor
usp.v.mdpt.long$N = factor(interaction(usp.v.mdpt.long$v.qual,
                                       usp.v.mdpt.long$Formant))

M = lm(data=usp.v.mdpt.long,
       log.Fx~0+speaker+N,
       contrasts=list(N=contr.sum))

# The estimated speaker means formant k can be extracted
# as S=dummy.coef(M)$S, and can then be used to normalize
# FF by subtracting each subjects S coefficient from
# the log formant frequencies produced by that speaker for
# that formant.

spknorms <- dummy.coef(M)$speaker
# vnorms <- dummy.coef(M)$N

# Create column with normalization constant for each observation
usp.v.mdpt.long$spk.adjust<-rep(NA,nrow(usp.v.mdpt.long))

for (currRow in 1:nrow(usp.v.mdpt.long)){
  spk <- usp.v.mdpt.long[currRow,]$speaker
  adj<-spknorms[spk]
  usp.v.mdpt.long[currRow,]$spk.adjust<-adj
  
}
unique(usp.v.mdpt.long$spk.adjust)
spknorms

# Apply speaker normalization as estimated from logistic regression
head(usp.v.mdpt.long)
usp.v.mdpt.long$log.Fx.norm<-usp.v.mdpt.long$log.Fx-usp.v.mdpt.long$spk.adjust

# Recode data in wide(r) format
usp.v.mdpt.long<-subset(usp.v.mdpt.long,
                        select=-c(N,log.Fx,spk.adjust))

x<-spread(usp.v.mdpt.long,
          Formant,
          log.Fx.norm)

summary(usp.v.mdpt$token.code==x$token.code)
usp.v.mdpt$F1.lognorm<-x$F1
usp.v.mdpt$F2.lognorm<-x$F2

remove(usp.v.mdpt.long,x)


# Check how correlated your F3 normalization is with the outcome of log-additive regression normalization
attach(usp.v.mdpt)
  cor.test(F1.lognorm,F1.F3) # Highly correlated
  cor.test(F2.lognorm,F2.F3) # Highly correlated
detach(usp.v.mdpt)

  
# Probably, Fx/F3 is perfectly fine, and definitely has the advantage of being token-dependent rather than sensitive to properties of the dataset as a whole or parameter estimates. But it of course depends on F3 being tracked well, and on the assumption that F3 doesn't itself vary in a systematic way with different vowel qualities.

# Check how the different normalization measures correlate with the original data (or log-transformed for log normalization)
attach(usp.v.mdpt)

  cor.test(F1.F3,F1) # Highly correlated
  cor.test(F1.lob,F1) # Highly correlated
  cor.test(F1.lognorm,log(F1)) # Highly correlated
  cor.test(hzToBark(F1),F1) # Highly correlated
  
  cor.test(F2.F3,F2) # Highly correlated
  cor.test(F2.lob,F2) # Highly correlated
  cor.test(hzToBark(F2),F2) # Highly correlated
  
detach(usp.v.mdpt)
  
# It's surprising *how* correlated normalizations are with the original data. Could this reflect the lack of substantial inter-speaker variation in vocal tract length/vowel space size in this data set?


##########################
# Examine the distribution of vowel qualities, and cut outliers if you like.

# Now start plotting normalized vowels in a vowel space.
# Facet by vowel type (length x stressed/unstressed)
# 
form.clean.mdpt<-subset(usp.v.mdpt,v.qual!="NA")

# Add another factor combining vowel quality and length.
# This isn't in your Usp_add_factors.R script, because it depends on plyr,
# but it probably should be part of that script.
# 
form.clean.mdpt$v.qual.len<-interaction(form.clean.mdpt$v.qual,form.clean.mdpt$v.len)
form.clean.mdpt$v.qual.len<-droplevels(form.clean.mdpt$v.qual.len)
form.clean.mdpt$v.qual.len<-revalue(form.clean.mdpt$v.qual.len,
                                    c("a.Short"="a","a.Long"="a\u2D0",
                                      "e.Short"="e","e.Long"="e\u2D0",
                                      "i.Short"="i","i.Long"="i\u2D0",
                                      "o.Short"="o","o.Long"="o\u2D0",
                                      "u.Short"="u","u.Long"="u\u2D0"))
summary(form.clean.mdpt$v.qual.len)


# Do the same for vowel quality, stress, and length.
form.clean.mdpt$v.qual.len.str<-droplevels(interaction(form.clean.mdpt$v.qual,form.clean.mdpt$v.len,form.clean.mdpt$stress))
summary(form.clean.mdpt$v.qual.len.str)
form.clean.mdpt$v.qual.len.str<-revalue(form.clean.mdpt$v.qual.len.str,c(
                                                           "a.Long.1"="a\u2D0",
                                                           "e.Long.1"="e\u2D0",
                                                           "i.Long.1"="i\u2D0",
                                                           "o.Long.1"="o\u2D0",
                                                           "u.Long.1"="u\u2D0",
                                                           "a.Short.1"="a",
                                                           "e.Short.1"="e",
                                                           "i.Short.1"="i",
                                                           "o.Short.1"="o",
                                                           "u.Short.1"="u",
                                                           "a.Short.0"="a\u306",
                                                           "e.Short.0"="e\u306",
                                                           "i.Short.0"="\u0131\u306",
                                                           "o.Short.0"="o\u306",
                                                           "u.Short.0"="u\u306"
                                                            ))
levels(form.clean.mdpt$v.qual.len.str)


# Make a table showing vowel quality, stress, and length.
form.clean.mdpt$v.qual.len.str<-as.factor(paste0(form.clean.mdpt$v.qual.len.str))
summary(form.clean.mdpt$v.qual.len.str)
summary(form.clean.mdpt$v.qual)

# Count the different types of vowels that you have in the dataset:
plyr::count(form.clean.mdpt,.(v.qual,v.str.len))

form.clean.mdpt$v.str.tone<-revalue(form.clean.mdpt$v.str.tone,c("0"="V\u306","1"="\u2C8V","2"="\u2C8V-H"))
plyr::count(form.clean.mdpt,.(v.qual,v.str.tone))
plyr::count(form.clean.mdpt,.(v.qual,v.str.tone,v.len))



##########################
# Visualize distributions and look for outliers.

########
# Make a density plot / contour map

v.F3norm.v.density<-
  # Select data frame.
  ggplot(form.clean.mdpt,
         # X-axis is Lobanov-normed F2.
         aes(x=F2.F3,
             # Y-axis is Lobanov-normed F1.
             y=F1.F3,
             group=v.qual,
             color=v.qual))+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse()+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title=element_text(size=24,face="bold"))+
  
  # Set the axis limits.
  # coord_cartesian() is used because it restricts the plotting
  # space without eliminating data points.
  # coord_cartesian(xlim = c(1.1,0),ylim = c(0.45,0.05))+
  
  # Initiate density plot
  geom_density2d(lwd=0.75) +
  
  
  ggtitle("Density of F1 and F2")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3,size=24))+
  theme(axis.title.x = element_text(vjust=-0.1,size=24))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Set color palette
  scale_color_manual(values=cbPalette,
                     name="")+
  
  # Increase size of lines in the legend. 
  theme(legend.key.width = unit(6, "lines"),legend.key.height = unit(2, "lines"))+
  guides(colour = guide_legend(override.aes = list(size=7)))+
  theme(panel.spacing = unit(0, "lines"))


v.F3norm.v.density

v.str.F3norm.v.dens.facet<-v.F3norm.v.density+facet_grid(.~v.str.len)+theme(strip.text = element_text(family = "Doulos SIL"))
v.str.F3norm.v.dens.facet

# Even without removing outliers, the distributions here look pretty good. /u/ is maybe wandering a bit too much, but perhaps we don't need to worry about that.


########
# Plot raw points

v.F3norm.points<-
  # Select data frame.
  ggplot(form.clean.mdpt,
         # X-axis is Lobanov-normed F2.
         aes(x=F2.F3,
             # Y-axis is Lobanov-normed F1.
             y=F1.F3,
             group=v.qual,
             color=v.qual))+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse()+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title=element_text(size=24,face="bold"))+
  
  # Set the axis limits.
  # coord_cartesian() is used because it restricts the plotting
  # space without eliminating data points.
  # coord_cartesian(xlim = c(1.1,0),ylim = c(0.45,0.05))+
  
  # Initiate density plot
  geom_text(aes(label=v.qual),size=6)+

  ggtitle("Density of F1 and F2")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3,size=24))+
  theme(axis.title.x = element_text(vjust=-0.1,size=24))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Set color palette
  scale_color_manual(values=cbPalette,
                     name="")+
  
  # Increase size of lines in the legend. 
  theme(legend.key.width = unit(6, "lines"),legend.key.height = unit(2, "lines"))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  theme(panel.spacing = unit(0, "lines"))


v.F3norm.points

v.str.F3norm.points.facet<-v.F3norm.points+facet_grid(.~v.str.len)+theme(strip.text = element_text(family = "Doulos SIL"))
v.str.F3norm.points.facet



######
# There are a handful of points here that are obvious measurement errors. We should eventually remove them.
ggplot(data=form.clean.mdpt,aes(x=F1.F3))+geom_density(fill="gray50")+facet_grid(v.qual~v.str.len)+theme_bw(base_size=24)+scale_x_reverse()
ggplot(data=form.clean.mdpt,aes(x=F2.F3))+geom_density(fill="gray50")+facet_grid(v.qual~v.str.len)+theme_bw(base_size=24)+scale_x_reverse()



########################
# Calculating means and SDs
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_%28ggplot2%29/

f1data.sum <- ddply(form.clean.mdpt, c("v.qual"), summarise,
                    N    = length(F1.F3),
                    mean = mean(F1.F3),
                    sd   = sd(F1.F3),
                    thresh.hi = mean + 2.5*sd,
                    thresh.lo = mean - 2.5*sd
                    )
f1data.sum

f2data.sum <- ddply(form.clean.mdpt, c("v.qual"), summarise,
                    N    = length(F2.F3),
                    mean = mean(F2.F3),
                    sd   = sd(F2.F3),
                    thresh.hi = mean + 2.5*sd,
                    thresh.lo = mean - 2.5*sd
                    )
f2data.sum

F3norm.mean.data<-cbind(f1data.sum[,1:2],
                        "F1.F3.mean"=f1data.sum[,"mean"],
                        "F1.F3.thresh.lo"=f1data.sum[,"thresh.lo"],"F1.F3.thresh.hi"=f1data.sum[,"thresh.hi"],
                        "F2.F3.mean"=f2data.sum[,"mean"],
                        "F2.F3.thresh.lo"=f2data.sum[,"thresh.lo"],"F2.F3.thresh.hi"=f2data.sum[,"thresh.hi"]
                        )

F3norm.mean.data


### 
# Filter data here:

get_CIs<-function(V){
  vdata<-subset(F3norm.mean.data,v.qual==V)
  f1.lo<-vdata$F1.F3.thresh.lo
  f1.hi<-vdata$F1.F3.thresh.hi
  f2.lo<-vdata$F2.F3.thresh.lo
  f2.hi<-vdata$F2.F3.thresh.hi
  return(c("f1.lo"=f1.lo,"f1.hi"=f1.hi,"f2.lo"=f2.lo,"f2.hi"=f2.hi))
}

outlier.rows<-c()
for (r in 1:nrow(form.clean.mdpt)){
  rowdata<-form.clean.mdpt[r,] # It would probably be much faster to pull out the data in blocks by vowel quality rather than by row. But this works and I don't feel like recoding it.
  theV<-unique(rowdata$v.qual)
  thresh<-get_CIs(theV)
    if (rowdata$F1.F3 < thresh["f1.lo"] | rowdata$F1.F3 > thresh["f1.hi"] | rowdata$F2.F3 < thresh["f2.lo"] | rowdata$F2.F3 > thresh["f2.hi"]){
      #print(paste("Outlier!",r))
      outlier.rows<-c(outlier.rows,r)      
    }
  }
length(outlier.rows)
nrow(form.clean.mdpt)


# Drop vowels that you want to treat as outliers in some respect:
form.trimmed<-form.clean.mdpt[-(outlier.rows),]
(1-nrow(form.trimmed)/nrow(form.clean.mdpt))*100


##############################################
# If you decide NOT to plot the formant values of post-tonic unstressed vowels:
# form.trimmed<-subset(form.trimmed,!(v.len=="Short"&stress==0&v.pos=="Final"))
##############################################


############
# Replot distributions to check for outcome of outlier trimming.
ggplot(data=form.trimmed,aes(x=F1.F3))+geom_density(fill="gray50")+facet_grid(v.qual~v.str.len)+theme_bw(base_size=24)+scale_x_reverse()
ggplot(data=form.trimmed,aes(x=F2.F3))+geom_density(fill="gray50")+facet_grid(v.qual~v.str.len)+theme_bw(base_size=24)+scale_x_reverse()

########
# Make a density plot / contour map

v.F3norm.v.density<-
  # Select data frame.
  ggplot(form.trimmed,
         # X-axis is Lobanov-normed F2.
         aes(x=F2.F3,
             # Y-axis is Lobanov-normed F1.
             y=F1.F3,
             group=v.qual,
             color=v.qual))+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse()+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title=element_text(size=24,face="bold"))+
  
  # Set the axis limits.
  # coord_cartesian() is used because it restricts the plotting
  # space without eliminating data points.
  # coord_cartesian(xlim = c(1.1,0),ylim = c(0.45,0.05))+
  
  # Initiate density plot
  geom_density2d(lwd=0.75) +
  
  
  ggtitle("Density of F1 and F2")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3,size=24))+
  theme(axis.title.x = element_text(vjust=-0.1,size=24))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Set color palette
  scale_color_manual(values=cbPalette,
                     name="")+
  
  # Increase size of lines in the legend. 
  theme(legend.key.width = unit(6, "lines"),legend.key.height = unit(2, "lines"))+
  guides(colour = guide_legend(override.aes = list(size=7)))+
  theme(panel.spacing = unit(0, "lines"))


v.F3norm.v.density

v.str.F3norm.v.dens.facet<-v.F3norm.v.density+facet_grid(.~v.str.len)+theme(strip.text = element_text(family = "Doulos SIL"))

v.str.F3norm.v.dens.facet

cairo_pdf(file=paste(outdir,"Outlier_checking/f1_f2_dens_F3_str_x_len.pdf",sep=""),
          width=14,height=8)
  print(v.str.F3norm.v.dens.facet)
dev.off()


# Even without removing outliers, the distributions here look pretty good. /u/ is maybe wandering a bit too much, but perhaps we don't need to worry about that.


########
# Plot raw points

v.F3norm.points<-
  # Select data frame.
  ggplot(form.trimmed,
         # X-axis is Lobanov-normed F2.
         aes(x=F2.F3,
             # Y-axis is Lobanov-normed F1.
             y=F1.F3,
             group=v.qual,
             color=v.qual))+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse()+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title=element_text(size=24,face="bold"))+
  
  # Set the axis limits.
  # coord_cartesian() is used because it restricts the plotting
  # space without eliminating data points.
  # coord_cartesian(xlim = c(1.1,0),ylim = c(0.45,0.05))+
  
  # Initiate density plot
  geom_text(aes(label=v.qual),size=6)+
  
  ggtitle("Density of F1 and F2")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3,size=24))+
  theme(axis.title.x = element_text(vjust=-0.1,size=24))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Set color palette
  scale_color_manual(values=cbPalette,
                     name="")+
  
  # Increase size of lines in the legend. 
  theme(legend.key.width = unit(6, "lines"),legend.key.height = unit(2, "lines"))+
  guides(colour = guide_legend(override.aes = list(size=7)))+
  theme(panel.spacing = unit(0, "lines"))


v.F3norm.points

v.str.F3norm.points.facet<-v.F3norm.points+facet_grid(.~v.str.len)+theme(strip.text = element_text(family = "Doulos SIL"))
v.str.F3norm.points.facet

cairo_pdf(file=paste(outdir,"Outlier_checking/f1_f2_points_F3_str_x_len.pdf",sep=""),
          width=14,height=8)
  print(v.str.F3norm.points.facet)
dev.off()




######################
# Filled ellipses with superimposed labels.

f1data.sum <- ddply(form.trimmed, c("v.qual", "v.str.len"), summarise,
                    N    = length(F1.F3),
                    mean = mean(F1.F3),
                    sd   = sd(F1.F3),
                    se   = sd / sqrt(N),
                    ci = qt(0.975/2+0.5, N-1),
                    f1.ci=ci*se
)
f1data.sum

f2data.sum <- ddply(form.trimmed, c("v.qual", "v.str.len"), summarise,
                    N    = length(F2.F3),
                    mean = mean(F2.F3),
                    sd   = sd(F2.F3),
                    se   = sd / sqrt(N),
                    ci = qt(0.975/2+0.5, N-1),
                    f2.ci=ci*se
)
f2data.sum

F3norm.mean.data<-cbind(f1data.sum[,1:2],
                        "F1.F3.mean"=f1data.sum[,"mean"],
                        f1.ci = f1data.sum[,"f1.ci"],
                        "F2.F3.mean"=f2data.sum[,"mean"],
                        f2.ci = f2data.sum[,"f2.ci"]
)

F3norm.mean.data

# Reorder factors + data in so that geom_polygon will plot the shape of the vowel space correctly, along the outside.
vorder<-c("a","e","i","u","o")
F3norm.mean.data$v.qual<-factor(F3norm.mean.data$v.qual,levels=vorder)
F3norm.mean.data<-arrange(F3norm.mean.data,v.qual)
F3norm.mean.data

# Plot the ellipses
v.F3norm.mean.CI.ellipse<-
  ggplot(form.trimmed,
         aes(x=F2.F3,
             y=F1.F3,
             # Plot formants grouped by vowel quality, using colors.
             col=v.qual,
             fill=v.qual
         )
  )+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse(limits=c(0.975,0.125),breaks=rev(seq(0.2,0.9,0.1)))+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=28)+
  theme(plot.title=element_text(size=24,face="bold"))+
  
  # Select color palettes.
  scale_color_manual(values=newpallx,
                     name="")+
  scale_fill_manual(values=newpallx,
                    name="")+
  
  # Set linetypes if desired.
  # scale_linetype_manual(values=c("solid","longdash","longdash","solid","solid"),name="")+
  
  # Hide legends.
  guides(fill=FALSE,color=FALSE)+
  
  coord_fixed(ratio=2)+ # Change dimensions so that F1 changes are visually more important than F2.
  
  # Add main plot title
  # Hide main title using plot.title=element_blank()
  #ggtitle("F3-normalized F1 and F2 values")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3))+
  theme(axis.title.x = element_text(vjust=-0.1))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Connect means to show overall vowel space
  geom_polygon(data=F3norm.mean.data,inherit.aes = FALSE,aes(group=v.str.len,x=F2.F3.mean,y=F1.F3.mean),color="grey50",alpha=0.1,size=1.15)+
  
  # Plot data ellipses first, so you can plot other things on top of them.
  # Note that these ellipses are NOT confidence ellipses -- they are data ellipses
  # (https://github.com/tidyverse/ggplot2/issues/2776)
  #
  # Setting level = 0.68 includes 68% of observations in each condition within the ellipse (about 1 standard deviation)
  # It's sort of like an inter-quantile interval.
  # 
  stat_ellipse(geom="polygon", lwd=1.25,alpha=0.5,level=0.68)+
  
  # If you'd rather fit a polygon (convex hull) showing vowel distributions in a bit more detail...
  # Can be useful for finding outliers, broadly speaking.
  # stat_chull(alpha=0.5) +
  
  
  # Plot error bars
  # geom_errorbar(data=F3norm.mean.data,aes(ymin=F1.F3.mean-f1.ci,
  #                                         ymax=F1.F3.mean+f1.ci,
  #                                         x=F2.F3.mean),
  #               width=0.015,lwd=1,color="gray65",inherit.aes = FALSE)+
  # 
  # geom_errorbarh(data=F3norm.mean.data,aes(xmin=F2.F3.mean-f2.ci,
  #                                          xmax=F2.F3.mean+f2.ci,
  #                                          y=F1.F3.mean),
  #                height=0.012,lwd=1,color="gray65",inherit.aes = FALSE)+
  # 

  # Plot means labeled with vowel quality values.
  geom_text(data=F3norm.mean.data,cex=12,aes(label=v.qual,x=F2.F3.mean,y=F1.F3.mean),
            color="black",inherit.aes = FALSE,family="Doulos SIL")+
  theme(plot.title=element_text(size=30,face="bold"))

v.F3norm.mean.CI.ellipse

v.str.F3norm.ellip<-v.F3norm.mean.CI.ellipse+facet_grid(.~v.str.len)+theme(strip.text = element_text(family = "Doulos SIL",face="bold",size=40)) # Rotate facet labels and change style
v.str.F3norm.ellip

cairo_pdf(file=paste(outdir,"f1_f2_ellipse_mean_CI_F3_str_x_len.pdf",sep=""),
          width=14,height=6)
  print(v.str.F3norm.ellip)
dev.off()



###############
# Now plot formant means + CIs in a single space to show centralization patterns.

########################
# Calculating means and SDs
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_%28ggplot2%29/

f1data.sum <- ddply(form.trimmed, c("v.qual.len.str","v.qual"), summarise,
                    N    = length(F1.F3),
                    mean = mean(F1.F3),
                    sd   = sd(F1.F3),
                    se   = sd / sqrt(N),
                    ci = qt(0.975/2+0.5, N-1),
                    f1.ci=ci*se
)
f1data.sum

f2data.sum <- ddply(form.trimmed, c("v.qual.len.str","v.qual"), summarise,
                    N    = length(F2.F3),
                    mean = mean(F2.F3),
                    sd   = sd(F2.F3),
                    se   = sd / sqrt(N),
                    ci = qt(0.975/2+0.5, N-1),
                    f2.ci=ci*se
)
f2data.sum

F3norm.mean.data<-cbind(f1data.sum[,1:2],
                        "F1.F3.mean"=f1data.sum[,"mean"],
                        f1.ci = f1data.sum[,"f1.ci"],
                        "F2.F3.mean"=f2data.sum[,"mean"],
                        f2.ci = f2data.sum[,"f2.ci"]
)

F3norm.mean.data



######################
# Fire up plot of F3 normalized values.
v.F3norm.mean.CI<-
  ggplot(form.trimmed,
         aes(x=F2.F3,
             y=F1.F3,
             # Plot formants grouped by vowel quality, using colors.
             col=v.qual,
             # lty=v.height,
             group=v.qual.len.str
         )
  )+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse()+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title = element_text(size = 30,face="bold"),
        plot.subtitle = element_text(size = 20))+
  
  # Select color palettes.
  scale_color_manual(values=newpall,
                     name="")+
  
  # Set linetypes if desired.
  scale_linetype_manual(values=c("solid","longdash","longdash","solid","solid"),name="")+
  
  # Hide legends.
  guides(fill=FALSE,color=FALSE,lty=FALSE)+

  # Add main plot title
  # Hide main title using plot.title=element_blank()
  #ggtitle("Unstressed V ~ \u2C8V ~ \u2C8V\u2D0")+
  #labs(subtitle = "F3-normalized F1 and F2 values")+
  #     caption = "9 speakers")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3))+
  theme(axis.title.x = element_text(vjust=-0.1))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Plot error bars
  # geom_errorbar(data=F3norm.mean.data,aes(ymin=F1.F3.mean-f1.ci,
  #                                         ymax=F1.F3.mean+f1.ci,
  #                                         x=F2.F3.mean),
  #               width=0.015,lwd=1,color="gray65",inherit.aes = FALSE)+
  # 
  # geom_errorbarh(data=F3norm.mean.data,aes(xmin=F2.F3.mean-f2.ci,
  #                                          xmax=F2.F3.mean+f2.ci,
  #                                          y=F1.F3.mean),
  #                height=0.012,lwd=1,color="gray65",inherit.aes = FALSE)+

  # Plot means labeled with vowel quality values.
  geom_text(data=F3norm.mean.data,cex=15,aes(label=v.qual.len.str,x=F2.F3.mean,y=F1.F3.mean,col=v.qual,group=v.qual.len.str),inherit.aes = FALSE,show.legend = F,family="Doulos SIL")+
  #geom_path(data=F3norm.mean.data,aes(group=v.qual,x=F2.F3.mean,y=F1.F3.mean),inherit.aes = FALSE,color="black",size=1.15)+
  stat_ellipse(lwd=1.25,level=0.68,aes(group=v.qual))

v.F3norm.mean.CI


cairo_pdf(file=paste(outdir,"f1_f2_mean_CI_F3_str_x_len_joint.pdf",sep=""),
          width=9.5,height=7.5)
  print(v.F3norm.mean.CI)
dev.off()



#######
# Stress vs. tone
form.trimmed$tone.qual<-interaction(form.trimmed$v.qual,
                                       form.trimmed$v.len,
                                       form.trimmed$stress,
                                       form.trimmed$v.tone)

# Replace symbols:
form.trimmed$tone.qual<-revalue(form.trimmed$tone.qual,c(
  # Long: Stressed, non-tonal
  "a.Long.1.0"="a\u2D0",
  "e.Long.1.0"="e\u2D0",
  "i.Long.1.0"="i\u2D0",
  "o.Long.1.0"="o\u2D0",
  "u.Long.1.0"="u\u2D0",
  # Short: Stressed, non-tonal
  "a.Short.1.1"="a",
  "e.Short.1.1"="e",
  "i.Short.1.1"="i",
  "o.Short.1.1"="o",
  "u.Short.1.1"="u",
  # Long: Stressed, tonal
  "a.Long.1.1"="a\u0301\u2D0",
  "e.Long.1.1"="e\u0301\u2D0",
  "i.Long.1.1"="i\u0301\u2D0",
  "o.Long.1.1"="o\u0301\u2D0",
  "u.Long.1.1"="u\u0301\u2D0",
  # Short: Stressed, tonal
  "a.Short.1.0"="a\u0301",
  "e.Short.1.0"="e\u0301",
  "i.Short.1.0"="i\u0301",
  "o.Short.1.0"="o\u0301",
  "u.Short.1.0"="u\u0301",
  # Unstressed
  "a.Short.0.0"="a\u306",
  "e.Short.0.0"="e\u306",
  "i.Short.0.0"="\u0131\u306",
  "o.Short.0.0"="o\u306",
  "u.Short.0.0"="u\u306"
))

# Resummarize
f1data.sum <- ddply(form.trimmed, c("tone.qual","v.qual","v.len"), summarise,
                    N    = length(F1.F3),
                    mean = mean(F1.F3),
                    sd   = sd(F1.F3),
                    se   = sd / sqrt(N),
                    ci = qt(0.975/2+0.5, N-1),
                    f1.ci=ci*se
)

f1data.sum


f2data.sum <- ddply(form.trimmed, c("tone.qual","v.qual","v.len"), summarise,
                    N    = length(F2.F3),
                    mean = mean(F2.F3),
                    sd   = sd(F2.F3),
                    se   = sd / sqrt(N),
                    ci = qt(0.975/2+0.5, N-1),
                    f2.ci=ci*se
)
f2data.sum

F3norm.mean.data<-cbind(f1data.sum[,1:3],
                        "F1.F3.mean"=f1data.sum[,"mean"],
                        f1.ci = f1data.sum[,"f1.ci"],
                        "F2.F3.mean"=f2data.sum[,"mean"],
                        f2.ci = f2data.sum[,"f2.ci"]
)

F3norm.mean.data



######################
# Fire up plot of F3 normalized values.
v.F3norm.mean.CI<-
  ggplot(form.trimmed,
         aes(x=F2.F3,
             y=F1.F3,
             # Plot formants grouped by vowel quality, using colors.
             col=v.qual,
             # lty=v.height,
             group=tone.qual
         )
  )+
  
  # Invert the y-axis so that higher F1 corresponds to a lower vowel.
  # Invert x-axis so front vowels (high F2) are to the left.
  scale_y_reverse()+
  scale_x_reverse()+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title = element_text(size = 30,face="bold"),
        plot.subtitle = element_text(size = 20))+
  
  # Select color palettes.
  scale_color_manual(values=newpall,
                     name="")+
  
  # Set linetypes if desired.
  scale_linetype_manual(values=c("solid","longdash","longdash","solid","solid"),name="")+
  
  # Hide legends.
  guides(fill=FALSE,color=FALSE,lty=FALSE)+
  
  # Add main plot title
  # Hide main title using plot.title=element_blank()
  #ggtitle("Unstressed V ~ \u2C8V ~ \u2C8V\u2D0")+
  #labs(subtitle = "F3-normalized F1 and F2 values")+
  #     caption = "9 speakers")+
  
  # Adjust the positioning of the minor x-axis and y-axis labels
  theme(axis.title.y = element_text(vjust=0.3))+
  theme(axis.title.x = element_text(vjust=-0.1))+
  
  # Change x- and y-axis labels.
  ylab("F1/F3")+
  xlab("F2/F3")+
  
  # Plot error bars
  # geom_errorbar(data=F3norm.mean.data,aes(ymin=F1.F3.mean-f1.ci,
  #                                         ymax=F1.F3.mean+f1.ci,
  #                                         x=F2.F3.mean),
  #               width=0.015,lwd=1,color="gray65",inherit.aes = FALSE)+
  # 
  # geom_errorbarh(data=F3norm.mean.data,aes(xmin=F2.F3.mean-f2.ci,
  #                                          xmax=F2.F3.mean+f2.ci,
  #                                          y=F1.F3.mean),
  #                height=0.012,lwd=1,color="gray65",inherit.aes = FALSE)+

# Plot means labeled with vowel quality values.
geom_text(data=F3norm.mean.data,cex=15,aes(label=tone.qual,x=F2.F3.mean,y=F1.F3.mean,col=v.qual,group=v.len),inherit.aes = FALSE,show.legend = F,family="Doulos SIL")+
  #geom_path(data=F3norm.mean.data,aes(group=v.qual,x=F2.F3.mean,y=F1.F3.mean),inherit.aes = FALSE,color="black",size=1.15)+
  stat_ellipse(lwd=1.25,level=0.68,aes(group=v.qual))

v.F3norm.mean.CI+facet_grid(.~v.len)
# v.F3norm.mean.CI


  
  
###############################
# Duration analysis
###############################
  
# We care about duration here primarily as it relates to stress and tone.
# Do we want to filter outliers?
  
head(form.clean.mdpt)
summary(form.clean.mdpt$dur)
quantile(form.clean.mdpt$dur,probs=seq(0,1,0.01))

# Trim duration outliers
dur.clean<-subset(form.clean.mdpt, dur < 301 )
100-nrow(na.omit(dur.clean))/nrow(na.omit(form.clean.mdpt))*100
nrow(na.omit(form.clean.mdpt))-nrow(na.omit(dur.clean))

dur.clean$accent<-droplevels(interaction(dur.clean$v.tone,dur.clean$stress))
summary(dur.clean$accent)

dur.clean$accent<-revalue(dur.clean$accent,c("0.0"="Unstressed\n","0.1"="Stressed\n","1.1"="Stressed\nwith H tone"))
dur.clean$v.len<-revalue(dur.clean$v.len,c("Short" = "V", "Long" = "V\u2D0"))

# Make duration plot
tone.dur.plot<-ggplot(data=dur.clean)+
  geom_density(aes(x=dur,
                   fill=accent,
                   lty=accent),
               alpha=0.3,color="black",lwd=2.5)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
                                   ),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Vowel duration (ms)")+
  scale_fill_manual(values=cbPalette[c(3,2,1)])+
  ylab("Density")+
  labs(fill = "Accent type",lty="Accent type")+
  facet_grid(.~v.len)+
  scale_linetype_manual(values=c("dashed","solid","dotted"))

tone.dur.plot

cairo_pdf(file=paste(outdir,"duration_by_accent_type.pdf",sep=""),
          width=16,height=6)
  print(tone.dur.plot)
dev.off()


###
# Let's try a ggridges version
library(ggridges)
tone.dur.plot.ridges<-ggplot(data=dur.clean)+
  geom_density_ridges(aes(x=dur,y=accent,
                   fill=accent),
               alpha=0.3,color="black",lwd=1.5)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        axis.title.y = element_blank(),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Vowel duration (ms)")+
  scale_fill_manual(values=cbPalette[c(3,2,1)],guide=F)+
  ylab("Density")+
  labs(fill = "Accent type",lty="Accent type")+
  facet_grid(.~v.len)+
  scale_linetype_manual(values=c("dashed","solid","dotted"))

tone.dur.plot.ridges

cairo_pdf(file=paste(outdir,"duration_by_accent_type_ridges.pdf",sep=""),
          width=10,height=6)
  print(tone.dur.plot.ridges)
dev.off()



# Does lengthening of short vowels under tone lead to neutralization with long vowels?
# It appears not, though there is substantial overlap.
short.str<-subset(dur.clean,accent=="Stressed\n" & v.len =="V")
short.tone<-subset(dur.clean,accent=="Stressed\nwith H tone" & v.len =="V")
t.test(short.str$dur,short.tone$dur)

long<-subset(dur.clean,v.len =="V\u2D0")
t.test(short.tone$dur,long$dur)

# Short vs. long vowels under stress
short.stress.or.tone<-subset(dur.clean,accent!="Unstressed\n" & v.len =="V")
t.test(short.stress.or.tone$dur,long$dur)

# Short stressed vs. unstressed vowels
short.unstressed<-subset(dur.clean,accent=="Unstressed\n")
t.test(short.unstressed$dur,short.stress.or.tone$dur)

# Different kinds of unstressed vowels
post.tonic<-subset(dur.clean,accent=="Unstressed\n" & v.pos =="Final")
pre.tonic<-subset(dur.clean,accent=="Unstressed\n" & v.pos =="Non-final")

t.test(post.tonic$dur,pre.tonic$dur)


rm(short.stress.or.tone,short.tone,long,short.unstressed,pre.tonic,post.tonic)




###############################
# Pitch analysis
###############################

################
# Load the text file of pitch measurements.
dataDateV = "Sep22"
pitch.tracks.raw<-read.csv(paste0("Usp_pitch_tracks_all_spkrs_",
                                    dataDateV,
                                    ".txt"),
                             header=T,sep="\t",
                             encoding="UTF-8")

head(pitch.tracks.raw)

# Get some demographic info in the dataframe.
pitch.tracks.raw$gender<-rep(NA,nrow(pitch.tracks.raw))
males<-c("S02","S04","S06")
females<-c("S01","S03","S05","S07","S08","S09")

# Ages
ages<-c(30,34,30,27,29,50,44,23,47)
summary(ages)
sd(ages)
# 1:30-f - San Miguel Uspantan
# 2:34-m - San Miguel Uspantan 
# 3:30-f - San Miguel Uspantan
# 4:27-m - La Lagunita
# 5:29-f - San Miguel Uspantan (Jacubi)
# 6:50-m - San Miguel Uspantan
# 7:44-f - San Miguel Uspantan
# 8:23-f - San Miguel Uspantan
# 9:47-f - San Miguel Uspantan


pitch.tracks.raw[pitch.tracks.raw$speaker%in%males,]$gender<-"M"
pitch.tracks.raw[pitch.tracks.raw$speaker%in%females,]$gender<-"F"
head(pitch.tracks.raw)
pitch.tracks.cut<-pitch.tracks.raw

# There's one speaker who sometimes produces items with exaggerated, unnatural f0 on the target words. We can remove her data if we are concern.
# pitch.tracks.cut<-subset(pitch.tracks.cut,!(speaker %in% c("S07")))

# Remove words that you don't want to analyze.
pitch.tracks.cut<-subset(pitch.tracks.cut,!(word %in% cutwords))

# Get rid of unstressed long vowels -- in this case, by recoding them as stressed, because the relevant example has an unstressed enclitic
subset(pitch.tracks.cut,segment%in%c("AL0","IL0","EL0","OL0","UL0"))
pitch.tracks.cut$segment<-revalue(pitch.tracks.cut$segment,c("AL0"="AL1","EL0"="EL1","IL0"="IL1","OL0"="OL1","UL0"="UL1"))
subset(pitch.tracks.cut,segment%in%c("AL0","IL0","EL0","OL0","UL0"))



# Get list of # of steps used
step.list<-as.character(seq(1,max(as.numeric(pitch.tracks.raw$step)),1))

# Get *unique* vowels
pitch.tracks.unique<-subset(pitch.tracks.cut,step==1)

###########
# Add some factors to the pitch data frame
# Note that you can't do this with data frames that have more than one observation per vowel (e.g. the
# original pitch track data frame). You have to have unique observations. 
pitch.tracks.unique<-usp.add.factors(pitch.tracks.unique)

######
# Mean pitch 
pitch.NOTNA<-subset(pitch.tracks.unique,!(is.na(mean.pitch.Hz)))

# You'll want to do some form of speaker-specific pitch normalization here. We check a few different methods and correlate them.
pitch.z<-z.score(pitch.NOTNA,"mean.pitch.Hz")

# Look at distribution of z-scores
simpleDensity(pitch.z,
              pitch.z$mean.pitch.Hz.zscore,
              "Hz (z-scored)"
)


# Remove outliers
zthresh = 2.5

quantile(pitch.z$mean.pitch.Hz.zscore,
         prob=seq(0,1,0.01),
         na.rm = T) # How much data is going to be trimmed?

pitch.z.trimmed<-subset(pitch.z,
                     abs(mean.pitch.Hz.zscore) < zthresh)
rm(zthresh)

# It might appear that you're losing tons of data here,
# but that's mostly because NA values are being automatically stripped out.
nrow(pitch.z.trimmed)
nrow(pitch.z)
100-nrow(na.omit(pitch.z.trimmed))/nrow(na.omit(pitch.z))*100
nrow(na.omit(pitch.z))-nrow(na.omit(pitch.z.trimmed))

# Look at distribution of z-scores
simpleDensity(pitch.z.trimmed,
              pitch.z.trimmed$mean.pitch.Hz.zscore,
              "Hz (z-scored)"
)


# We check out some other normalization methods.
# For a general citation, see Ladd (2008:192-202).

# Normalization by estimated pitch range (de Looze & Rauzy)
spkrange<-pitch.z$pitch.range.max-pitch.z$pitch.range.min

range.norm.Hz<-pitch.z$mean.pitch.Hz/spkrange

cor.test(range.norm.Hz,
       pitch.z$mean.pitch.Hz.zscore)

qqplot(range.norm.Hz,
       pitch.z$mean.pitch.Hz.zscore)
abline(lm(pitch.z$mean.pitch.Hz.zscore~range.norm.Hz),col="red")

rm(range.norm.Hz)

# Check correlation after outlier removal -- surprisingly, not any better.
spkrange<-pitch.z.trimmed$pitch.range.max-pitch.z.trimmed$pitch.range.min

range.norm.Hz<-pitch.z.trimmed$mean.pitch.Hz/spkrange

cor.test(range.norm.Hz,
       pitch.z.trimmed$mean.pitch.Hz.zscore)

qqplot(range.norm.Hz,
       pitch.z.trimmed$mean.pitch.Hz.zscore)
abline(lm(pitch.z.trimmed$mean.pitch.Hz.zscore~range.norm.Hz),col="red")

rm(range.norm.Hz)


############
# "Normalisation des contours intonatifs et étude de la variation régionale en français" Alice Bardiaux, Piet Mertens
# normalized px = 100 * ((px-pbottom) / (ptop-pbottom))

# Need to calculate pitch range for each speaker.
range.norm.Hz<-c()
for (spk in unique(pitch.z$speaker)){
  spkdata<-subset(pitch.z,speaker==spk)
  
  # Get speaker pitch range
  spkqs<-quantile(spkdata$mean.pitch.Hz,seq(0,1,0.01))
  ceiling <- spkqs["98%"]
  floor <- spkqs["2%"]
  speaknormed <- 100 * ( (spkdata$mean.pitch.Hz-floor)/(ceiling-floor) )
  
  range.norm.Hz<-c(range.norm.Hz,speaknormed)
}

summary(range.norm.Hz)

cor.test(range.norm.Hz,
       pitch.z$mean.pitch.Hz.zscore)

qqplot(range.norm.Hz,
       pitch.z$mean.pitch.Hz.zscore)
abline(lm(pitch.z$mean.pitch.Hz.zscore~range.norm.Hz),col="red")

rm(range.norm.Hz)


############
# Now do it in semitones

# Formula for converting between semitones and Hz
# http://www.fon.hum.uva.nl/praat/manual/Formulas_5__Mathematical_functions.html
# https://praat-users.yahoogroups.co.narkive.com/053Lww1a/semitone
hzTOst<-function(x,ref=100){
  (12/log(2,base=10))*log(x/ref,base=10)
}

pitch.z$ST<-hzTOst(pitch.z$mean.pitch.Hz)

range.norm.ST<-c()
for (spk in unique(pitch.z$speaker)){
  spkdata<-subset(pitch.z,speaker==spk)
  
  # Get speaker pitch range
  spkqs<-quantile(spkdata$ST,seq(0,1,0.01))
  ceiling <- spkqs["98%"]
  floor <- spkqs["2%"]
  speaknormed <- 100 * ( (spkdata$ST-floor)/(ceiling-floor) )
  
  range.norm.ST<-c(range.norm.ST,speaknormed)
}

summary(range.norm.ST)

cor.test(range.norm.ST,
       pitch.z$mean.pitch.Hz.zscore)

qqplot(range.norm.ST,
       pitch.z$mean.pitch.Hz.zscore)
abline(lm(pitch.z$mean.pitch.Hz.zscore~range.norm.ST),col="red")


#################
# A Comparison of Tone Normalization Methods for Language Variation
# Jingwei Zhang

# Method 9: ST-AvgF0 ref=AvgF0 (i.e. each
#                               speaker's average
#                               pitch)

norm.ST.mean<-c()
for (spk in unique(pitch.z$speaker)){
  spkdata<-subset(pitch.z,speaker==spk)
  
  # Get speaker pitch mean
  spkmean<-mean(spkdata$mean.pitch.Hz)
  speaknormed <- hzTOst(spkdata$mean.pitch.Hz,ref=spkmean)
  
  norm.ST.mean<-c(norm.ST.mean,speaknormed)
}

summary(norm.ST.mean)

cor.test(norm.ST.mean,
       pitch.z$mean.pitch.Hz.zscore)

qqplot(norm.ST.mean,
       pitch.z$mean.pitch.Hz.zscore)
abline(lm(pitch.z$mean.pitch.Hz.zscore~norm.ST.mean),col="red")

# Pretty good agreement across the board for normalization methods, except perhaps the first. Much of the disagreement also concerns the treatment of outliers, apparently.


#################
# Plot mean vowel pitch
pitch.z.trimmed$accent<-droplevels(interaction(pitch.z.trimmed$v.tone,pitch.z.trimmed$stress))
summary(pitch.z.trimmed$accent)

pitch.z.trimmed$accent<-revalue(pitch.z.trimmed$accent,c("0.0"="Unstressed\n","0.1"="Stressed\n","1.1"="Stressed\nwith H tone"))
pitch.z.trimmed$v.len<-revalue(pitch.z.trimmed$v.len,c("Short" = "V", "Long" = "V\u2D0"))

tone.plot<-ggplot(data=pitch.z.trimmed)+
  geom_density(aes(x=mean.pitch.Hz.zscore,
                   fill=accent,
                   lty=accent),
               alpha=0.3,color="black",lwd=2.5)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Mean pitch on vowels (z-scores over Hz)")+
  scale_fill_manual(values=cbPalette[c(3,2,1)])+
  ylab("Density")+
  labs(fill = "Accent type",lty="Accent type")+
  facet_grid(.~v.len)+
  scale_linetype_manual(values=c("dashed","solid","dotted"))

tone.plot

cairo_pdf(file=paste(outdir,"f0_mean_by_accent_type.pdf",sep=""),
          width=16,height=6)
  print(tone.plot)
dev.off()


# How do speakers vary, if at all?
# It does indeed seem like some speakers have larger tone differences than others.
tone.plot+facet_grid(v.len~speaker)

# Does this correlate with gender?
tone.plot+facet_grid(gender~v.len)+theme(strip.text.y = element_text(angle=0))


# Use t-tests to check for significant differences:
# Technique inspired here:
#https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories
stressed.mean.pitch<-subset(pitch.z.trimmed,stress==1)
summary(stressed.mean.pitch$mean.pitch.Hz.zscore)

library(broom)
stressed.mean.pitch %>% group_by(v.len) %>% do(tidy(t.test(mean.pitch.Hz.zscore~v.tone, data=.)))



#################################
# Should you look at the variances of unstressed vowels too?

#################################
# Variability check -- are variances different for tonal vs. non-tonal vowels?

# Tone vs. toneless, collapsed over vowel length
#

# What in fact are the variances of the different groups?
# They seem really similar, so we shouldn't expect a significant result, I don't think.
var(subset(stressed.mean.pitch,v.tone==1)$mean.pitch.Hz.zscore)
var(subset(stressed.mean.pitch,v.tone==0)$mean.pitch.Hz.zscore)
var(subset(pitch.z.trimmed,stress==0)$mean.pitch.Hz.zscore) # Less variance than stressed vowels? How odd...


# There's a concern here because different ways of measuring variance, all purportedly using the Brown-Forsythe or the related (but dispreferred) Levene's F-test, seem to turn up *really* different results.

library(car)
leveneTest(mean.pitch.Hz.zscore~v.tone, data=stressed.mean.pitch) # Levene, non.sig.
detach(package:car)

library(HH) # For brown-forsythe test
hov(mean.pitch.Hz.zscore~v.tone, data=stressed.mean.pitch) # bf test, non.sig.
hov.bf(x=stressed.mean.pitch$mean.pitch.Hz.zscore,group=stressed.mean.pitch$v.tone) # bf test but just checking that syntax doesn't matter

hov(mean.pitch.Hz~v.tone, data=stressed.mean.pitch) # bf test, non.sig., over non-z-scored data.
detach(package:HH)

library(onewaytests)
homog.test(mean.pitch.Hz.zscore~v.tone, data=stressed.mean.pitch,method="Levene",na.rm=T) # Levene, non.sig
homog.test(mean.pitch.Hz.zscore~v.tone, data=stressed.mean.pitch,method="Bartlett",na.rm=T) # Bartlett, non.sig

bf.test(mean.pitch.Hz.zscore~v.tone, data=stressed.mean.pitch,na.rm=T) # bf test, but sig!!! Is it just more sensitive to the number of values included? Doesn't matter if na is included or not.
detach(package:onewaytests)

# We stick here with HH for the bf test because the bf test is more robust to skews and deviations from normality, and this test in this package seems to give results for the bf test which are consistent with other tests/packages.


####################
# Check f0 variance by tone across v-lengths
# Technique inspired here:
#https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories

# library(broom)
library(HH) # For brown-forsythe test
stressed.mean.pitch %>% group_by(v.len) %>% do(tidy(hov(mean.pitch.Hz.zscore~v.tone, data=.)))




######
# Maximum pitch

# Check how correlated mean and maximum pitch are before bothering to analyze maximum pitch separately.
cor.test(na.omit(pitch.tracks.unique)$mean.pitch.Hz,na.omit(pitch.tracks.unique)$max.pitch.Hz)

# Pretty highly correlated...

# Look at distribution of z-scores
pitch.max.z<-z.score(pitch.tracks.unique,"max.pitch.Hz")
simpleDensity(pitch.max.z,
              pitch.max.z$max.pitch.Hz.zscore,
              "Hz (z-scored)"
)


# Remove outliers
zthresh = 2.5

quantile(pitch.max.z$max.pitch.Hz.zscore,
         prob=seq(0,1,0.01),
         na.rm = T) # How much data is going to be trimmed?

pitch.max.z.trimmed<-subset(pitch.max.z,
                        abs(max.pitch.Hz.zscore) < zthresh)
rm(zthresh)

# It might appear that you're losing tons of data here,
# but that's mostly because NA values are being automatically stripped out.
nrow(pitch.max.z.trimmed)
nrow(pitch.max.z)
100-nrow(na.omit(pitch.max.z.trimmed))/nrow(na.omit(pitch.max.z))*100
nrow(na.omit(pitch.max.z))-nrow(na.omit(pitch.max.z.trimmed))

# Look at distribution of z-scores
simpleDensity(pitch.max.z.trimmed,
              pitch.max.z.trimmed$max.pitch.Hz.zscore,
              "Hz (z-scored)"
)

# Trimming outliers doesn't really affect the strength of the mean/max correlation.
cor.test(na.omit(pitch.max.z.trimmed)$mean.pitch.Hz,na.omit(pitch.max.z.trimmed)$max.pitch.Hz)



############
# Compare with other normalization methods:
# "Normalisation des contours intonatifs et étude de la variation régionale en français" Alice Bardiaux, Piet Mertens
# normalized px = 100 * ((px-pbottom) / (ptop-pbottom))

pitch.max.z$ST<-hzTOst(pitch.max.z$max.pitch.Hz)

range.norm.ST<-c()
for (spk in unique(pitch.max.z$speaker)){
  spkdata<-subset(pitch.max.z,speaker==spk)
  
  # Get speaker pitch range
  spkqs<-quantile(spkdata$ST,seq(0,1,0.01),na.rm=T)
  ceiling <- spkqs["98%"]
  floor <- spkqs["2%"]
  speaknormed <- 100 * ( (spkdata$ST-floor)/(ceiling-floor) )
  
  range.norm.ST<-c(range.norm.ST,speaknormed)
}

summary(range.norm.ST)

cor.test(range.norm.ST,
         pitch.max.z$max.pitch.Hz.zscore)

qqplot(range.norm.ST,
       pitch.max.z$max.pitch.Hz.zscore)
abline(lm(pitch.max.z$max.pitch.Hz.zscore~range.norm.ST),col="red")


#################
# A Comparison of Tone Normalization Methods for Language Variation
# Jingwei Zhang

# Method 9: ST-AvgF0 ref=AvgF0 (i.e. each
#                               speaker's average
#                               pitch)

norm.ST.max<-c()
for (spk in unique(pitch.max.z$speaker)){
  spkdata<-subset(pitch.max.z,speaker==spk)
  
  # Get speaker pitch mean
  spkmean<-mean(spkdata$max.pitch.Hz,na.rm=T)
  speaknormed <- hzTOst(spkdata$max.pitch.Hz,ref=spkmean)
  
  norm.ST.max<-c(norm.ST.max,speaknormed)
}

summary(norm.ST.max)

cor.test(norm.ST.max,
         pitch.max.z$max.pitch.Hz.zscore)

qqplot(norm.ST.max,
       pitch.max.z$max.pitch.Hz.zscore)
abline(lm(pitch.max.z$max.pitch.Hz.zscore~norm.ST.max),col="red")

# Pretty good agreement across the board for normalization methods, except perhaps the first. Much of the disagreement also concerns the treatment of outliers, apparently.



#########################
# Make plot
pitch.max.z.trimmed$accent<-droplevels(interaction(pitch.max.z.trimmed$v.tone,pitch.max.z.trimmed$stress))
summary(pitch.max.z.trimmed$accent)

pitch.max.z.trimmed$accent<-revalue(pitch.max.z.trimmed$accent,c("0.0"="Unstressed\n","0.1"="Stressed\n","1.1"="Stressed\nwith H tone"))
pitch.max.z.trimmed$v.len<-revalue(pitch.max.z.trimmed$v.len,c("Short" = "V", "Long" = "V\u2D0"))

tone.plot<-ggplot(data=pitch.max.z.trimmed)+
  geom_density(aes(x=max.pitch.Hz.zscore,
                   fill=accent,
                   lty=accent),
               alpha=0.3,color="black",lwd=2.5)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Maximum pitch on vowels (z-scores over Hz)")+
  scale_fill_manual(values=cbPalette[c(3,2,1)])+
  ylab("Density")+
  labs(fill = "Accent type",lty="Accent type")+
  facet_grid(.~v.len)+
  scale_linetype_manual(values=c("dashed","solid","dotted"))

tone.plot

cairo_pdf(file=paste(outdir,"f0_max_by_accent_type.pdf",sep=""),
          width=16,height=6)
  print(tone.plot)
dev.off()


# Use t-tests to check for significant differences:
# Technique inspired here:
#https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories
stressed.max.pitch<-subset(pitch.max.z.trimmed,stress==1)
summary(stressed.max.pitch$max.pitch.Hz.zscore)

stressed.max.pitch %>% group_by(v.len) %>% do(tidy(t.test(max.pitch.Hz.zscore~v.tone, data=.)))


############
# Variability check -- are variances different for tonal vs. non-tonal vowels?
# The answer appears to be no.
stressed.max.pitch<-subset(pitch.max.z.trimmed,stress==1)
summary(stressed.max.pitch$max.pitch.Hz.zscore)

hov(max.pitch.Hz.zscore~v.tone, data=stressed.max.pitch)
stressed.max.pitch %>% group_by(v.len) %>% do(tidy(hov(max.pitch.Hz.zscore~v.tone, data=.)))



#############################################
# Plot vowel pitch by position
head(pitch.tracks.cut)

pitch.tracks.z<-z.score(pitch.tracks.cut,"pitch.Hz")

# Look at distribution of z-scores
simpleDensity(pitch.tracks.z,
              pitch.tracks.z$pitch.Hz.zscore,
              "Hz (z-scored)"
)


# Remove outliers
zthresh = 2.5

quantile(pitch.tracks.z$pitch.Hz.zscore,
         prob=seq(0,1,0.01),
         na.rm = T) # How much data is going to be trimmed?

pitch.tracks.z.trimmed<-subset(pitch.tracks.z,
                        abs(pitch.Hz.zscore) < zthresh)
rm(zthresh)

# It might appear that you're losing tons of data here,
# but that's mostly because NA values are being automatically stripped out.
nrow(pitch.tracks.z.trimmed)
nrow(pitch.tracks.z)
100-nrow(na.omit(pitch.tracks.z.trimmed))/nrow(na.omit(pitch.tracks.z))*100
nrow(na.omit(pitch.tracks.z))-nrow(na.omit(pitch.tracks.z.trimmed))

# Look at distribution of z-scores
simpleDensity(pitch.tracks.z.trimmed,
              pitch.tracks.z.trimmed$pitch.Hz.zscore,
              "Hz (z-scored)"
)


############
# Compare with other normalization methods:
# "Normalisation des contours intonatifs et étude de la variation régionale en français" Alice Bardiaux, Piet Mertens
# normalized px = 100 * ((px-pbottom) / (ptop-pbottom))

pitch.tracks.z$ST<-hzTOst(pitch.tracks.z$pitch.Hz)

range.norm.ST<-c()
for (spk in unique(pitch.tracks.z$speaker)){
  spkdata<-subset(pitch.tracks.z,speaker==spk)
  
  # Get speaker pitch range
  spkqs<-quantile(spkdata$ST,seq(0,1,0.01),na.rm=T)
  ceiling <- spkqs["98%"]
  floor <- spkqs["2%"]
  speaknormed <- 100 * ( (spkdata$ST-floor)/(ceiling-floor) )
  
  range.norm.ST<-c(range.norm.ST,speaknormed)
}

summary(range.norm.ST)

cor.test(range.norm.ST,
            pitch.tracks.z$pitch.Hz.zscore)

qqplot(range.norm.ST,
       pitch.tracks.z$pitch.Hz.zscore)
abline(lm(pitch.tracks.z$pitch.Hz.zscore~range.norm.ST),col="red")


#################
# A Comparison of Tone Normalization Methods for Language Variation
# Jingwei Zhang

# Method 9: ST-AvgF0 ref=AvgF0 (i.e. each
#                               speaker's average
#                               pitch)

norm.ST.mean<-c()
for (spk in unique(pitch.tracks.z$speaker)){
  spkdata<-subset(pitch.tracks.z,speaker==spk)
  
  # Get speaker pitch mean
  spkmean<-mean(spkdata$pitch.Hz,na.rm=T)
  speaknormed <- hzTOst(spkdata$pitch.Hz,ref=spkmean)
  
  norm.ST.mean<-c(norm.ST.mean,speaknormed)
}

summary(norm.ST.mean)

cor.test(norm.ST.mean,
            pitch.tracks.z$pitch.Hz.zscore)

qqplot(norm.ST.mean,
       pitch.tracks.z$pitch.Hz.zscore)
abline(lm(pitch.tracks.z$pitch.Hz.zscore~norm.ST.mean),col="red")

# Pretty good agreement across the board for normalization methods, except perhaps the first. Much of the disagreement also concerns the treatment of outliers, apparently.



#########
# Vowel length and tone are already coded for vowels in Praat TextGrids, but we want to reparse them into factors.
head(pitch.tracks.z.trimmed)
summary(pitch.tracks.z.trimmed$segment)

# Tone coding:
# Use regular expressions to treat any vowel code with T in it as a tonal vowel, otherwise non-tonal
pitch.tracks.z.trimmed$tone<-pitch.tracks.z.trimmed$segment
pitch.tracks.z.trimmed$tone<-gsub(pitch.tracks.z.trimmed$tone,
                  pattern=".*T.*",
                  replacement="Tonal")

pitch.tracks.z.trimmed$tone[pitch.tracks.z.trimmed$tone!="Tonal"]<-"Non-tonal"

pitch.tracks.z.trimmed$tone<-as.factor(pitch.tracks.z.trimmed$tone)
summary(pitch.tracks.z.trimmed$tone)


# Vowel length coding:
# Use regular expressions to treat any vowel code with L as long, otherwise short
pitch.tracks.z.trimmed$vlen<-pitch.tracks.z.trimmed$segment
pitch.tracks.z.trimmed$vlen<-gsub(pitch.tracks.z.trimmed$vlen,
                  pattern=".*L.*",
                  replacement="Long")

pitch.tracks.z.trimmed$vlen[pitch.tracks.z.trimmed$vlen!="Long"]<-"Short"

pitch.tracks.z.trimmed$vlen<-as.factor(pitch.tracks.z.trimmed$vlen)
summary(pitch.tracks.z.trimmed$vlen)


# Stress coding:
# Use regular expressions to treat any vowel code with 1 as stressed, otherwise unstressed 
pitch.tracks.z.trimmed$stress<-pitch.tracks.z.trimmed$segment
pitch.tracks.z.trimmed$stress<-gsub(pitch.tracks.z.trimmed$stress,
                    pattern=".*1$",
                    replacement="Stressed")

pitch.tracks.z.trimmed$stress[pitch.tracks.z.trimmed$stress!="Stressed"]<-"Unstressed"

pitch.tracks.z.trimmed$stress<-as.factor(pitch.tracks.z.trimmed$stress)  
summary(pitch.tracks.z.trimmed$stress)


# Check output
head(pitch.tracks.z.trimmed)
tail(pitch.tracks.z.trimmed)
attach(pitch.tracks.z.trimmed)
  table(stress,tone)
  table(vlen,tone)
  table(vlen,stress)
detach(pitch.tracks.z.trimmed)



#######
# Code positional factors

# First, you need to split the token code up so that the interval is separate from the word-level coding. There's probably a smart way to do this, I did it with some hack-and-slash regular expressions I worked out by trial and error.
library(stringi)
pitch.tracks.z.trimmed$word.code<-stri_extract_last_regex(pitch.tracks.z.trimmed$token.code,".+(?=-[:digit:]+$)")
pitch.tracks.z.trimmed$interval<-as.numeric(stri_extract_last_regex(pitch.tracks.z.trimmed$token.code,"[:digit:]+$"))

head(pitch.tracks.z.trimmed)


# We want to code vowels by their position in the word, and position relative to stress as well.
#
# This is straightforward for stressed/tonal vowels, but harder to work out for unstressed vowels, which
# can be final/non-final or pre/post-tonic.

summary(pitch.tracks.z.trimmed$act.wd) # This won't be totally straightforward, as words range from 1-3 vowels/syllables


# Coding for position (and opacity because of post-tonic syncope) isn't straightforward. We write a complex function for it, which could probably be simpler or more elegant. It also runs slowly.

# Create columns for storing information about vowel position.
dummy.vec<-rep(NA,nrow(pitch.tracks.z.trimmed))
  pitch.tracks.z.trimmed$v.pos.wd<-dummy.vec
  pitch.tracks.z.trimmed$v.pos.str<-dummy.vec
  pitch.tracks.z.trimmed$opacity<-dummy.vec
rm(dummy.vec)


##########
# Add codes.
# This takes a long time and there is surely a better way to do this.
for (wd.token in unique(pitch.tracks.z.trimmed$word.code)){
  
  # For each word, figure out which vowel is the final vowel, and which the penultimate.
  wd.data = subset(pitch.tracks.z.trimmed,word.code==wd.token)
  intervals = unique(wd.data$interval)
  finalfirst = rev(sort(intervals))

  finalCode = as.numeric(finalfirst[1])
  penultCode = as.numeric(finalfirst[2])

  # Now iterate through the vowels in each word, adding factor values based on position.
  for (rownum in 1:nrow(wd.data)){
    vRow = wd.data[rownum,]
    vInt = as.numeric(vRow$interval)
    
    # Get word position code
    if (vInt==finalCode){
      posCode <-"Ultima"
    } else if (vInt==penultCode){
      posCode <-"Penult"
    } else {
      posCode <-"Pre-penult"
    }

    # Get opacity coding
    if (posCode=="Ultima" & vRow$vlen == "Short" & vRow$tone == "Tonal"){
      opqCode <-"Opaque"
    } else {
      opqCode <-"Transparent"
    }
    
    # Get stress position coding
    if (posCode=="Ultima" & vRow$stress == "Unstressed"){
      strposCode <-"Post-tonic"
    } else if (vRow$stress == "Stressed"){
      strposCode <-"Stressed"
    } else {
      strposCode <-"Pre-tonic"
    }
    
    # Get row number of the current row.
    # This should match the row number in the original dataframe
    rowIdentifier = rownames(vRow)

    # Save values
    pitch.tracks.z.trimmed[rowIdentifier,"v.pos.wd"]<-posCode
    pitch.tracks.z.trimmed[rowIdentifier,"v.pos.str"]<-strposCode
    pitch.tracks.z.trimmed[rowIdentifier,"opacity"]<-opqCode

    rm(posCode,strposCode,opqCode)
  }
}
rm(wd.data,vRow,finalCode,finalfirst,intervals,rowIdentifier,rownum,vInt,wd.token)


pitch.tracks.z.trimmed$v.pos.wd<-factor(pitch.tracks.z.trimmed$v.pos.wd)
pitch.tracks.z.trimmed$v.pos.str<-factor(pitch.tracks.z.trimmed$v.pos.str)
pitch.tracks.z.trimmed$opacity<-factor(pitch.tracks.z.trimmed$opacity)


# Check the output
head(pitch.tracks.z.trimmed)
summary(pitch.tracks.z.trimmed$opacity)
summary(pitch.tracks.z.trimmed$v.pos.str)
summary(pitch.tracks.z.trimmed$v.pos.wd)
attach(pitch.tracks.z.trimmed)
  table(v.pos.wd,stress)
  table(v.pos.wd,tone)
  table(v.pos.wd,opacity)
  table(v.pos.wd,vlen)
detach(pitch.tracks.z.trimmed)

# Order levels for word-internal position.
pitch.tracks.z.trimmed$v.pos.wd<-factor(pitch.tracks.z.trimmed$v.pos.wd,
                        levels=c("Pre-penult","Penult","Ultima"))

# Create code for interaction of stress and tone
pitch.tracks.z.trimmed$accent<-factor(interaction(pitch.tracks.z.trimmed$stress,pitch.tracks.z.trimmed$tone))
levels(pitch.tracks.z.trimmed$accent)
pitch.tracks.z.trimmed$accent<-revalue(pitch.tracks.z.trimmed$accent,
                       c("Stressed.Non-tonal"="Stressed",
                         "Stressed.Tonal"="Tonal",
                         "Unstressed.Non-tonal"="Unstressed"))
summary(pitch.tracks.z.trimmed$accent)
attach(pitch.tracks.z.trimmed)
  table(v.pos.wd,accent)
detach(pitch.tracks.z.trimmed)

head(pitch.tracks.z.trimmed)
tail(pitch.tracks.z.trimmed)


####
# Adjust position coding so that opaque final, tonal, short vowels are treated as penultimate instead
pitch.tracks.z.trimmed$v.pos.wd.adj<-pitch.tracks.z.trimmed$v.pos.wd

for (row in 1:nrow(pitch.tracks.z.trimmed)){
  if (pitch.tracks.z.trimmed[row,"opacity"]=="Opaque" & pitch.tracks.z.trimmed[row,"vlen"]=="Short"){
    pitch.tracks.z.trimmed[row,"v.pos.wd.adj"]<-"Penult"
  }
}



# Some words have long vowels followed by enclitics, so their position needs to be adjusted:
pitch.tracks.z.trimmed[pitch.tracks.z.trimmed$vlen=="Long" & pitch.tracks.z.trimmed$v.pos.wd.adj=="Penult",]$v.pos.wd.adj<-"Ultima"

##############
# Sort factors you'll be plotting/faceting
# Word position
pitch.tracks.z.trimmed$v.pos.wd<-as.factor(pitch.tracks.z.trimmed$v.pos.wd)
levels(pitch.tracks.z.trimmed$v.pos.wd)
pitch.tracks.z.trimmed$v.pos.wd<-factor(pitch.tracks.z.trimmed$v.pos.wd,
                        levels=rev(levels(pitch.tracks.z.trimmed$v.pos.wd)))
levels(pitch.tracks.z.trimmed$v.pos.wd)

# Accent type
pitch.tracks.z.trimmed$accent<-as.factor(pitch.tracks.z.trimmed$accent)
levels(pitch.tracks.z.trimmed$accent)
pitch.tracks.z.trimmed$accent<-factor(pitch.tracks.z.trimmed$accent,
                      levels=c("Tonal","Stressed","Unstressed"))
levels(pitch.tracks.z.trimmed$accent)


summary(pitch.tracks.z.trimmed$v.pos.wd)
summary(pitch.tracks.z.trimmed$v.pos.wd.adj)
# Order levels for word-internal position.
pitch.tracks.z.trimmed$v.pos.wd.adj<-factor(pitch.tracks.z.trimmed$v.pos.wd.adj,
                            levels=c("Pre-penult","Penult","Ultima"))

# Subset by word length
pitch.tracks.short<-droplevels(subset(pitch.tracks.z.trimmed,vlen=="Short"))
pitch.tracks.long<-droplevels(subset(pitch.tracks.z.trimmed,vlen=="Long"))

head(pitch.tracks.z.trimmed)



# Plot pitch tracks
pitch.tracks.z.trimmed$vlen<-revalue(pitch.tracks.z.trimmed$vlen,c("Short" = "V", "Long" = "V\u2D0"))
pitch.tracks.z.trimmed$vlen<-factor(pitch.tracks.z.trimmed$vlen,levels=c("V","V\u2D0"))
plotdata<-subset(pitch.tracks.z.trimmed,v.pos.wd.adj!="Pre-penult")


v.plot<-ggplot(plotdata,
               aes(x=step,
                   y=pitch.Hz.zscore,
                   color=accent,
                   linetype=accent))+
  
  geom_smooth(method="loess",lwd=3)+
  
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=c("solid","dashed","dotted"))+
  
  theme_bw()+
  
  theme(text=element_text(size=48),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=44),
        legend.key.height = unit(3, "lines"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
        
        # Use these options if you want a more B&W print-friendly plot.
        #panel.background = element_rect(fill="white"),
        #panel.grid.major = element_line(color = "#80808033",linetype="dashed"),
        #panel.grid.minor = element_line(color = "#80808033",linetype="dotted")
  )+
  
  coord_cartesian(ylim=c(-1.5,1.5))+
  
  scale_x_continuous("Time (normalized)", breaks=as.numeric(step.list), labels=waiver())+
  scale_y_continuous("Pitch (z-scores over Hz)", labels=waiver())+
  labs(linetype = "Accent type")+
  labs(color = "Accent type")+
  
  #ggtitle("Vowel pitch contours")+
  facet_grid(vlen~v.pos.wd.adj)+theme(strip.text.y = element_text(angle=0))

v.plot


output_file<-paste0(outdir,"vowel_pitch_tracks.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  v.plot
dev.off()



########################################################
# Let's see if there's any relationship between the length of the accented penult and the application of post-tonic vowel deletion.
########################################################

# The idea: vowel length should be correlated with whether the vowel is coded in Praat as the "last" vowel of the word.
dur.tone.short<-subset(dur.clean,accent=="Stressed\nwith H tone" & v.len=="V")

##########
# Rather than repeating all of the code used to add factors to the pitch trajectory data, we use a lookup technique to copy over those factors. This should be cleaner and faster. 
addcols<-c("v.pos.wd","v.pos.wd.adj",
           "token.code")

dur.tone.short.factors<-merge(dur.tone.short,
                              pitch.tracks.z.trimmed[,addcols],by="token.code")


# Merging creates lots of duplicate rows.
head(dur.tone.short.factors)
head(duplicated(dur.tone.short.factors))

# We drop duplicate rows.
dur.tone.short<-dur.tone.short.factors[!duplicated(dur.tone.short.factors),]
head(dur.tone.short)
head(duplicated(dur.tone.short))
summary(duplicated(dur.tone.short))

summary(dur.tone.short$v.pos.wd)
dur.tone.short[1:30,c("word","v.pos.wd")]

sync.count<-summary(dur.tone.short$v.pos.wd)[1]
nosync.count<-summary(dur.tone.short$v.pos.wd)[2]

dur.tone.short$v.pos.wd<-revalue(dur.tone.short$v.pos.wd,
                                 c("Ultima"=paste0("Syncope\n(n = ",sync.count,")"),
                                   "Penult"=paste0("No syncope\n(n = ",nosync.count,")")
                                 ))
summary(dur.tone.short$v.pos.wd)

sync.dur.plot<-ggplot(dur.tone.short,
                      aes(x=dur,
                          group=v.pos.wd,
                          fill=v.pos.wd,
                          linetype=v.pos.wd))+
  
  # Set the overall theme to black & white.
  # Also sets the base font size at 32pt, and the plot title at 30pt bold.
  theme_bw(base_size=32)+
  theme(plot.title=element_text(size=24,face="bold"))+
  
  # Initiate density plot
  geom_density(lwd=3,alpha=0.5) +
  
  # Set color palette
  scale_fill_manual(values=cbPalette,
                    name="")+
  scale_linetype_manual(values=c("solid","dashed"),
                        name="")+
  
  
  # Increase size of lines in the legend. 
  theme(legend.key.width = unit(6, "lines"),legend.key.height = unit(2, "lines"))+
  guides(colour = guide_legend(override.aes = list(size=7)))+
  theme(panel.spacing = unit(0, "lines"))+
  ylab("Density")+
  xlab("Duration (ms)")+
  scale_x_continuous(limits=c(0,300))

sync.dur.plot

cairo_pdf(file=paste0(basedir,
                      "Articles\\Syncope\\Images\\JIPA_study\\",
                      "deletion_v_tone_del.pdf"),
          width=12,height=6)
  print(sync.dur.plot)
dev.off()




######################
# We fit an lmer for mean pitch (raw pitch, not z-scored, since we have random effects for speaker anyway)
library(lme4)
source(paste0(scriptdir,"lmer_collinearity_tools.R")) # For assessing model collinearity
library(piecewiseSEM) # For computing marginal r^2; https://github.com/jslefche/piecewiseSEM
# https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/


lmerdata.mean<-pitch.z.trimmed
lmerdata.max<-pitch.max.z.trimmed

lmerdata.mean<-droplevels(lmerdata.mean)
lmerdata.max<-droplevels(lmerdata.max)

colnames(lmerdata.mean)
summary(as.factor(lmerdata.mean$v.pos))
lmerdata.mean$v.pos.wd<-as.factor(lmerdata.mean$v.pos)

for (row in 1:nrow(lmerdata.mean)){
  if (lmerdata.mean[row,"v.pos"]=="Final" & lmerdata.mean[row,"v.len"]=="Short" & lmerdata.mean[row,"v.tone"]==1){
    pitch.tracks.z.trimmed[row,"v.pos.wd"]<-"Non-final"
  }
}
summary(as.factor(lmerdata.mean$v.pos))
summary(as.factor(lmerdata.mean$v.pos.wd))



##################
# Reorder levels and use backward/forward difference coding for accent
##################
# Accent type
# For accent type, we have a three-way factor and may want to know how each level changes relative to the
# *previous level*. For that, we would use backward difference coding instead.
# https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/contr.sdif.html
# Order levels
levels(lmerdata.mean$accent)

# library(MASS)
# contrasts(lmerdata.mean$accent)
# contrasts(lmerdata.mean$accent)<-contr.sdif(length(levels(lmerdata.mean$accent)))
# summary(lmerdata.mean$accent)
# contrasts(lmerdata.mean$accent)


lmerdata.mean$v.pos.wd<-factor(lmerdata.mean$v.pos.wd,levels=c("Non-final","Final"))


##############
# Let's look at speaker means for some predictors to see if random slopes might be justified.
# https://web.stanford.edu/class/psych252/section/Mixed_models_tutorial.html

# Tone and stress
accent.means<-ddply(lmerdata.mean,
                    .(speaker,accent,v.tone,stress,v.len),
                    summarize,
                    mean_pitch = mean(mean.pitch.Hz),
                    sd_pitch = sd(mean.pitch.Hz)
)


# Tone
tone.means<-subset(accent.means,accent!="Unstressed")

# Some variation in tonal slopes, though not an enormous amount relative to baseline pitch differences.
tone.means.plot<-ggplot(tone.means, aes(x=accent, y=mean_pitch)) +
  geom_point(size=4, aes(color = speaker,group=speaker))+
  geom_line(size=2, aes(color = speaker,group=speaker))+
  geom_label(data=subset(tone.means,v.tone==0),aes(label=speaker,x=0.9),size=8)+
  #scale_y_continuous(limits=c(3.25,6.25))+
  theme_bw(base_size = 48)+
  theme(legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32))+
  xlab("Tone")+
  ylab("Mean pitch (z-scores over Hz)")+
  labs(color = "Speaker")+
  theme(legend.position = "none")


tone.means.plot
tone.means.plot+facet_grid(.~v.len)

# There are definitely (noisy) inter-speaker differences to think about, some of which probably reflect the relatively small # of items in each condition in this study.


# Fit an lmer
tail(count(lmerdata.mean,word)) # Some words only have one repetition, so you can't actually estimate a random effect for word without doing some more data restructuring.

full.m<-lmer(data=lmerdata.mean,
             # Use REML=F for log-likelihood comparison in step-down model reduction.
             REML=F,
             #
             # Increase the number of iterations to convergence if needed.
             # https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022084.html
             control=lmerControl(optCtrl=list(maxfun=1e5),optimizer = "bobyqa"),
             #
             # Dependent variable:
             mean.pitch.Hz~
               #
               # Fixed effects: all required by theoretical interest, and allowed by convergence/factor crossing
               stress+
               v.tone*v.len+
               v.pos.wd+
               v.height+
               # 
               # Random effects
               # Simple random intercepts
               # (1|word)+
               (1|speaker)+
               # By speaker random slopes for tone/stress, correlated with by-speaker random intercepts
               (1+v.tone|speaker)
               # (1+stress|speaker)
)

# Collinearity
max(vif.mer(full.m))
kappa.mer(full.m) # Moderate collinearity

summary(full.m)
anova(full.m)

# Model reduction
anova(full.m)[order(anova(full.m)[4]),] # Shorthand for sorting anova by F-value (decreasing)
coef(summary(full.m))[order(abs(coef(summary(full.m))[,1]),decreasing=T),]

# m2<-update(full.m,.~.-v.len:v.tone) # Sig
m2<-update(full.m,.~.-stress)
anova(full.m,m2)

# Step 2
anova(m2)[order(anova(m2)[4]),] # Shorthand for sorting anova by F-value (decreasing)
coef(summary(m2))[order(abs(coef(summary(m2))[,1]),decreasing=T),]

# m3<-update(m2,.~.-v.len:v.tone) # Sig
# m3<-update(m2,.~.-v.height) # No convergence
# m3<-update(m2,.~.-v.pos.wd) # No convergence
# anova(full.m,m2,m3)

anova(m2)[order(anova(m2)[4],decreasing=T),] # Shorthand for sorting anova by F-value (decreasing)
coef(summary(m2))[order(abs(coef(summary(m2))[,1]),decreasing=T),]

# Collinearity
max(vif.mer(m2))
kappa.mer(m2) # Low collinearity


# Get some p-values
length(fixef(m2)) # Get the number of fixed effects predictors
nrow(lmerdata.mean) # Get the number of observation
df.ub<-nrow(lmerdata.mean)-length(fixef(m2)) # upper bound
df.ub

for (x in rownames(coef(summary(m2)))){cat(x,"\n")}
for (x in (coef(summary(m2))[ , "Estimate"])){cat(round(x,6),"\n")}
for (x in (coef(summary(m2))[ , "Std. Error"])){cat(round(x,6),"\n")}
for (x in (coef(summary(m2))[ , "t value"])){cat(round(x,3),"\n")}
tvals<-round(coef(summary(m2))[ , "t value"],3)

pval.maker<-function(t){
  2 * (1 - pt(abs(t), df.ub))
}

library(stringr)
# http://stackoverflow.com/questions/13353531/format-numeric-without-leading-zero

for (t in tvals){
  trd<-round(pval.maker(t),3)
  tout<-str_replace(as.character(trd), "^0\\.", ".")    
  cat(tout,"\n")
}

# Check normality of residuals --- not too bad!
cor(qqnorm(residuals(m2))$x,
    residuals(m2))
qqnorm(residuals(m2)) # Not ideal, but not too bad.
qqline(residuals(m2))

summary(residuals(m2))
ggplot(data=data.frame("resid"=residuals(m2)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)

coef(summary(m2))[order(abs(coef(summary(m2))[,1]),decreasing=T),]
# Compare to full model
coef(summary(full.m))[order(abs(coef(summary(m2))[,1]),decreasing=T),]

# Verify that dropping stress is better than dropping the correlated vowel position predictor (by AIC)
m2.alt<-update(full.m,.~.-v.pos.wd)
anova(m2,m2.alt)



##############################
# Phonation plots for accent type
##############################

################
# Load the text file of phonation measurements.
dataDateV = "Sep22"

phon.tracks.raw<-read.csv(paste0("Usp_phonation_all_spkrs_",
                                    dataDateV,
                                    ".txt"),
                             header=T,sep="\t",
                             encoding="UTF-8")


##################################################
# Eliminate words that you do not want to plot, e.g. various
# kinds of function words, disfluencies, etc.
##################################################
phon.tracks.cut<-subset(phon.tracks.raw,!(word %in% cutwords))

# Get rid of unstressed long vowels -- in this case, by recoding them as stressed, because the relevant example has an unstressed enclitic
subset(phon.tracks.cut,segment%in%c("AL0","IL0","EL0","OL0","UL0"))
phon.tracks.cut$segment<-revalue(phon.tracks.cut$segment,c("AL0"="AL1","EL0"="EL1","IL0"="IL1","OL0"="OL1","UL0"="UL1"))
subset(phon.tracks.cut,segment%in%c("AL0","IL0","EL0","OL0","UL0"))

# Focus on one particular step if you'd like.
phon.tracks.cut$step<-as.factor(phon.tracks.cut$step)
levels(phon.tracks.cut$step)
# slicenum = 2
# phon.slice<-subset(phon.tracks.cut,
#                    step==slicenum
# )
phon.slice<-phon.tracks.cut


#########
# Vowel length and tone are already coded for vowels in Praat TextGrids, but we want to reparse them into factors.
head(phon.slice)
summary(phon.slice$segment)

# Tone coding:
# Use regular expressions to treat any vowel code with T in it as a tonal vowel, otherwise non-tonal
phon.slice$tone<-phon.slice$segment
phon.slice$tone<-gsub(phon.slice$tone,
                                  pattern=".*T.*",
                                  replacement="Tonal")

phon.slice$tone[phon.slice$tone!="Tonal"]<-"Non-tonal"

phon.slice$tone<-as.factor(phon.slice$tone)
summary(phon.slice$tone)


# Vowel length coding:
# Use regular expressions to treat any vowel code with L as long, otherwise short
phon.slice$vlen<-phon.slice$segment
phon.slice$vlen<-gsub(phon.slice$vlen,
                                  pattern=".*L.*",
                                  replacement="Long")

phon.slice$vlen[phon.slice$vlen!="Long"]<-"Short"

phon.slice$vlen<-as.factor(phon.slice$vlen)
summary(phon.slice$vlen)


# Stress coding:
# Use regular expressions to treat any vowel code with 1 as stressed, otherwise unstressed 
phon.slice$stress<-phon.slice$segment
phon.slice$stress<-gsub(phon.slice$stress,
                                    pattern=".*1$",
                                    replacement="Stressed")

phon.slice$stress[phon.slice$stress!="Stressed"]<-"Unstressed"

phon.slice$stress<-as.factor(phon.slice$stress)  
summary(phon.slice$stress)


# Check output
head(phon.slice)
tail(phon.slice)
attach(phon.slice)
  table(stress,tone)
  table(vlen,tone)
  table(vlen,stress)
detach(phon.slice)

###########
# Add some coding for vowel quality
phon.slice$v.qual<-phon.slice$segment
phon.slice$v.qual<-gsub(phon.slice$v.qual, # Use regular expressions to strip out coding
                     pattern="T|S|L|1|0",
                     replacement="")

phon.slice$v.qual<-factor(phon.slice$v.qual)
summary(phon.slice$v.qual)


###########
# Add glottalization of flanking segments
phon.slice$prec.seg.glot.wd<-gsub(phon.slice$prec.seg.wd, # Use regular expressions
                               pattern=".*G.*",
                               replacement="Glottalized")

phon.slice[is.na(phon.slice$prec.seg.glot.wd),]$prec.seg.glot.wd<-"Other"
phon.slice[phon.slice$prec.seg.glot.wd!="Glottalized",]$prec.seg.glot.wd<-"Other"
phon.slice$prec.seg.glot.wd<-factor(phon.slice$prec.seg.glot.wd)
summary(phon.slice$prec.seg.glot.wd)

phon.slice$next.seg.glot.wd<-gsub(phon.slice$next.seg.wd, # Use regular expressions
                               pattern=".*G.*",
                               replacement="Glottalized")

phon.slice[is.na(phon.slice$next.seg.glot.wd),]$next.seg.glot.wd<-"Other"
phon.slice[phon.slice$next.seg.glot.wd!="Glottalized",]$next.seg.glot.wd<-"Other"
phon.slice$next.seg.glot.wd<-factor(phon.slice$next.seg.glot.wd)
summary(phon.slice$next.seg.glot.wd)



##################
# Normalize and clean vowel data
colnames(phon.slice)
phon.slice.hnr<-z.score(phon.slice,"hnr")
phon.slice.h1h2<-z.score(phon.slice,"H1.H2")


# Look at distribution of z-scores --- not bad, actually.
simpleDensity(phon.slice.hnr,
              phon.slice.hnr$hnr.zscore,
              "hnr (z-scored)"
)

simpleDensity(phon.slice.h1h2,
              phon.slice.h1h2$H1.H2.zscore,
              "H1-H2 (z-scored)"
)


# Remove outliers
zthresh = 2.5

quantile(phon.slice.hnr$hnr.zscore,
         prob=seq(0,1,0.05),
         na.rm = T) # How much data is going to be trimmed?

phon.slice.hnr<-subset(phon.slice.hnr,
                    abs(hnr.zscore) < zthresh)

quantile(phon.slice.h1h2$H1.H2.zscore,
         prob=seq(0,1,0.05),
         na.rm = T) # How much data is going to be trimmed?

phon.slice.h1h2<-subset(phon.slice.h1h2,
                     abs(H1.H2.zscore) < zthresh)


simpleDensity(phon.slice.hnr,
              phon.slice.hnr$hnr.zscore,
              "hnr (z-scored)"
)

simpleDensity(phon.slice.h1h2,
              phon.slice.h1h2$H1.H2.zscore,
              "H1-H2 (z-scored)"
)


#########
# HNR analysis
give.n <- function(x,ypos=-2.75){
  #  return(c(y = 40, label = length(x)))
  #
  # Setting the value of y here affects the vertical positioning of the labels.
  data.frame(y = ypos, label = paste0("n=",length(x)))
}



####
# All vowels
phon.slice.hnr.STRESSED<-subset(phon.slice.hnr,stress=="Stressed")

phon.slice.hnr.STRESSED$step.char<-phon.slice.hnr.STRESSED$step
phon.slice.hnr.STRESSED$step.char<-revalue(phon.slice.hnr.STRESSED$step.char,c("1"="First 1/2","2"="Second 1/2"))
phon.slice.hnr.STRESSED$vlen<-revalue(phon.slice.hnr.STRESSED$vlen,c("Short" = "V", "Long" = "V\u2D0"))
phon.slice.hnr.STRESSED$vlen<-factor(phon.slice.hnr.STRESSED$vlen,levels=c("V","V\u2D0"))

hnr.phon<-ggplot(data=phon.slice.hnr.STRESSED)+
  geom_density(aes(x=hnr.zscore,
                   fill=tone),
               alpha=0.3,color="black",lwd=1.25)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Voice quality in stressed vowels (z-scores over HNR)")+
  scale_fill_manual(values=cbPalette[c(2,1)])+
  ylab("Density")+
  labs(fill = "Tone")+
  
  facet_grid(vlen~step.char)+
  theme(strip.text.y = element_text(angle=0))

hnr.phon


###############
# DO YOU WANT TO ADD P-VALUES? Prob. not. But fit an lmer?
###############

output_file<-paste0(outdir,"stressed_v_hnr.pdf")
cairo_pdf(file=output_file,
          width=16,height=7)
  print(hnr.phon)
dev.off()




####
# All vowels, h1h2
phon.slice.h1h2.STRESSED<-subset(phon.slice.h1h2,stress=="Stressed")

phon.slice.h1h2.STRESSED$step.char<-phon.slice.h1h2.STRESSED$step
phon.slice.h1h2.STRESSED$step.char<-revalue(phon.slice.h1h2.STRESSED$step.char,c("1"="First 1/2","2"="Second 1/2"))
phon.slice.h1h2.STRESSED$vlen<-revalue(phon.slice.h1h2.STRESSED$vlen,c("Short" = "V", "Long" = "V\u2D0"))
phon.slice.h1h2.STRESSED$vlen<-factor(phon.slice.h1h2.STRESSED$vlen,levels=c("V","V\u2D0"))

h1h2.phon<-ggplot(data=phon.slice.h1h2.STRESSED)+
  geom_density(aes(x=H1.H2.zscore,
                   fill=tone),
               alpha=0.3,color="black",lwd=1.25)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Voice quality in stressed vowels (z-scores over H1*-H2*)")+
  scale_fill_manual(values=cbPalette[c(2,1)])+
  ylab("Density")+
  labs(fill = "Tone")+
  
  facet_grid(vlen~step.char)+
  theme(strip.text.y = element_text(angle=0))

h1h2.phon


###############
# DO YOU WANT TO ADD P-VALUES? Prob. not. But fit an lmer?
###############

output_file<-paste0(outdir,"stressed_v_h1h2.pdf")
cairo_pdf(file=output_file,
          width=16,height=7)
  print(h1h2.phon)
dev.off()




########################################
# HERE HERE HERE HERE --- TO DO
########################################

#######
# Finalize phon analysis

#######
# Window size filtering for data?

######
# Estimate the actual range of the f0 effects using CIs around beta etc., as in usp_NSF_QAtask_alldata.R? May not be necessary here...

######
# Pitch excursion size...you'd need to re-run Praat to get this.

