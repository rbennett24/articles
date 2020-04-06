###################################
#
# Consider looking at non-normalized contours for tone, especially on short vowels, to see if something like rate of increase is the best correlate of the tonal contrast rather than absolute value.
#
# Alternatively, plot max peak position relative to segment start
#
# Linear discriminant analysis

######################################################
# Preliminaries and data preparation
######################################################

##################
# Load packages
library(ggplot2)
library(plyr)
library(tidyverse) # Load after plyr so there aren't any conflicts


##################
# Colorblind friendly color palette
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#000000","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#cbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
yelCol = "#F0E442" # cbPalette[6]
lightBlue = "#56B4E9" # cbPalette[4]
orng  = "#E69F00" # cbPalette[3]

# See what color scheme looks like.
showColor<-function(pal){
  hist(seq(0,length(pal)),col=pal,breaks=length(pal))
}

showColor(cbPalette)

cbBW<-rev(cbPalette[c(6,4,7,3,8)])
showColor(cbBW)


##################
# Try to make plots using the DoulosSIL IPA font
library(extrafont)

##########
# This only needs to be done once on each computer, I think.
# font_import(prompt = F,pattern="DoulosSIL-R") # Import system fonts -- this can take awhile if you import them all
# loadfonts(device = "win") # I think you only need to do this once so that R imports all the needed files to a place it can draw on them?
# windowsFonts() # This will just show you which fonts are available to R -- it may be a long list!
##########

# Check that desired fonts are loaded.
# subset(fonttable(),FontName=="DoulosSIL")


##################
# Set working directory as location where raw pitch tracks are located.
user = "Tiamat"

basedir = paste0("C:/Users/",user,"/Dropbox/Research/Mayan/Uspanteko/Uspanteko_NSF_project/")
# datadir = paste0(basedir,"Recordings/QA_data_for_analysis/FA_input/Files_for_Praat_analysis/Praat_measures/")
datadir = paste0(basedir,"Recordings/QA_data_for_analysis/FA_output/FA_textgrids/Praat_measures/")
paperdir = paste0(basedir,"Articles/Intonation/")

setwd(datadir)

# Set script directory
scriptDir<-paste0(basedir,"Recordings/Scripts/Data analysis/R_scripts/QA_task/Article/")

# Set directories for saving plots
imageDir<-paste0(paperdir,"Images/Study_plots/")
if (dir.exists(imageDir)){
  # Do nothing
} else {
  dir.create(imageDir)
}

if (dir.exists(paste0(imageDir,"Loess/"))){
  # Do nothing
} else {
  dir.create(paste0(imageDir,"Loess/"))
}

if (dir.exists(paste0(imageDir,"PCA/"))){
  # Do nothing
} else {
  dir.create(paste0(imageDir,"PCA/"))
}


##################
# Load functions which may be useful down the road.
source(paste0(scriptDir,"Usp_convenience_functions.R"))


##################
# Import demographic data
spk.demo<-read.csv(header=T,"demographics.csv",encoding="UTF-8")
head(spk.demo)
colnames(spk.demo)[1]<-"Speaker"
head(spk.demo)

spk.demo<-subset(spk.demo,
                 Speaker!="Sxxx")

spk.demo

# We didn't actually work with speaker 9, right?
spk.demo<-subset(spk.demo,Speaker!="S09")

nrow(spk.demo)
summary(spk.demo$Age)
sd(spk.demo$Age)
summary(spk.demo$Gender)
summary(spk.demo$Born)
summary(spk.demo$Lives)

spk.demo


##################
# Read in the QA data for vowel-level analysis.

dataDateV = "Apr06"

QA.v.raw<-read.csv(paste0("Usp_QA_vowels_pitch_all_spkrs_",
                          dataDateV,
                          ".txt"),
                   header=T,sep="\t",
                   encoding="UTF-8")

head(QA.v.raw)
rm(dataDateV)


#############
# Define a simple density plot function for quick visualizations.
simpleDensity<-function(df,datacol,title){
  ggplot(data=df)+
    geom_density(aes(x=datacol),
                 fill=lightBlue,alpha=0.25,color="black",lwd=1.25)+
    theme_bw(base_size=24)+
    xlab(title)
  
}


##################
# How many tokens per-speaker?
spk.token.count<-ddply(QA.v.raw,
                       .(speaker),
                       summarize,
                       n_unique = n_distinct(token.code)
)

spk.token.count
sd(spk.token.count$n_unique)
summary(spk.token.count)
simpleDensity(spk.token.count,
              spk.token.count$n_unique,
              "Unique tokens")
summary(spk.token.count)
sum(spk.token.count$n_unique)

# How many unique vowels?
attach(QA.v.raw)
  length(unique(interaction(token.code,interval.num)))
detach(QA.v.raw)


#####################
# Check Hz~ERB correlations
#####################
cor(QA.v.raw$mean.pitch.Hz,
    QA.v.raw$mean.pitch.ERB,
    use="pairwise.complete.obs")

qqplot(QA.v.raw$pitch.Hz,
       QA.v.raw$pitch.ERB)
abline(lm(QA.v.raw$pitch.ERB~QA.v.raw$pitch.Hz),col="red")

#####################
# Begin cleaning the data
#####################

# How many steps were there per vowel?
unique(QA.v.raw$step)

# Look at distribution of step sizes.
simpleDensity(QA.v.raw,
              QA.v.raw$step.size*1000,
              "Average size (in ms) of pitch measurement window")

quantile((QA.v.raw$step.size*1000),
         prob=seq(0,1,0.01)
)

# Recall that the step-sizes here are windows for averaging over estimated pitch values, and NOT the windows for estimating pitch itself,
# which is done over the full utterance, and is determined separately by the speaker-specific estimated pitch floors.
# http://www.fon.hum.uva.nl/praat/manual/Sound__To_Pitch___.html
# http://www.fon.hum.uva.nl/praat/manual/Sound__To_Pitch__ac____.html

# So, we should only really worry about super-low or super-high window sizes, which plausibly reflect alignment errors (unless the alignments have been hand-corrected.). At the same time it's worth bearing in mind that longer windows involve averaging over more measurements/points than shorter windows --- this is the nature of time-normalization.

hithresh = 40
lothresh = 4
max(QA.v.raw$step)*hithresh # What would vowel length be at thresholds?
max(QA.v.raw$step)*lothresh

# In this case, we feel pretty comfortable with all of the window sizes, because the corresponding vowel lengths are sensible. So we won't end up trimming any data.

QA.v.windowed<-subset(QA.v.raw,
                      step.size*1000<hithresh & # Trim data
                        step.size*1000>lothresh)
rm(hithresh,lothresh)

# Look at distribution of step sizes in words with reasonable windows
simpleDensity(QA.v.windowed,
              QA.v.windowed$step.size*1000,
              "Size (in ms) of pitch measurement windows")

# How much data was lost?
100-nrow(na.omit(QA.v.windowed))/nrow(na.omit(QA.v.raw))*100


####################################
# z-score normalize to remove outliers and pool data.
####################################

QA.v<-QA.v.windowed
rm(QA.v.windowed)

colnames(QA.v)
QA.v.z<-z.score(QA.v,"pitch.Hz")

# Look at distribution of z-scores
simpleDensity(QA.v.z,
              QA.v.z$pitch.Hz.zscore,
              "Hz (z-scored)"
              )


# Get list of # of steps used
step.list<-as.character(seq(1,max(QA.v.z$step),1))


##############################
# We check out some other normalization methods.
# For a general citation, see Ladd (2008:192-202). Cite also the two papers below which you're working from

QA.v.z.NOTNA<-subset(QA.v.z,!is.na(pitch.Hz))

###############
# Log and semitone transformations aren't normalization techniques, so they should correlate with z-scores about as well as the original data (which turns out to be true).

r<-cor(QA.v.z.NOTNA$pitch.Hz,
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(QA.v.z.NOTNA$pitch.Hz,
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~QA.v.z.NOTNA$pitch.Hz),col="red")

###############
# Log-transformation
r<-cor(log(QA.v.z.NOTNA$pitch.Hz),
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(log(QA.v.z.NOTNA$pitch.Hz),
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~log(QA.v.z.NOTNA$pitch.Hz)),col="red")

###############
# Semitone transformation

# We first need functions for converting between semitones and Hz
# http://www.fon.hum.uva.nl/praat/manual/Formulas_5__Mathematical_functions.html
# https://praat-users.yahoogroups.co.narkive.com/053Lww1a/semitone
hzTOst<-function(x,ref=100){
  (12/log(2,base=10))*log(x/ref,base=10)
}
hzTOst(120)
hzTOst(115)
hzTOst(100,ref=80)

# stTOhz<-function(x,ref=100){
#   e = 2.7182818284590452353602874713527
#   ref*e^(x*log(2)/12)
# }
# stTOhz(hzTOst(120))
# stTOhz(hzTOst(150))

QA.v.z.NOTNA$ST<-hzTOst(QA.v.z.NOTNA$pitch.Hz)

# Now test correlation
r<-cor(QA.v.z.NOTNA$ST,
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(QA.v.z.NOTNA$ST,
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~QA.v.z.NOTNA$ST),col="red")



###############
# Check to see whether normalization by z-score transformation correlates at all with the (automatically selected) pitch ranges used for pitch extraction, which are themselves calculated from speaker range.

# Extract pitch range
spkrange<-QA.v.z.NOTNA$pitch.range.max-QA.v.z.NOTNA$pitch.range.min

range.norm.Hz<-QA.v.z.NOTNA$pitch.Hz/spkrange

r<-cor(range.norm.Hz,
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(range.norm.Hz,
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~range.norm.Hz),col="red")

# n = length(range.norm.Hz)
# t = (r/sqrt(1-r^2))*sqrt(n-2)
# 2*pt(-abs(t),df=n-1) # Significant correlation...
# rm(r,t,n)

# They're well-correlated (r = 0.96, p =0), so I suspect that varying the normalization method won't resolve data noisiness problems downstream...especially after outliers are cut, since it seems like a lot of the non-linearity is at the extremes.

# Check correlation with original data
r<-cor(range.norm.Hz,
       QA.v.z.NOTNA$pitch.Hz)
r

rm(range.norm.Hz)

############
#(1) Normalisation des contours intonatifs et étude de la variation régionale en français Alice Bardiaux, Piet Mertens

# normalized px = 100 * ((px-pbottom) / (ptop-pbottom))
#
# px is the height (ST rel 1Hz) at a given point,
# pbottom to the floor of the speaker's tessitura and ptop to the ceiling of
# this range. This formula joins the method of standardization of the
# f0 proposed by Earle (1975) and reported by Ladd (2008). note that
# this normalization according to the range can in principle
# apply to values of f0 expressed on any scale
# (Hertz, semitones, ERB, OME, etc.).
#
# The floor and ceiling of the speaker's range were
# determined by the percentiles 2 and 98 of the tonal targets detected by
# Prosogramme (Mertens 2003, 2004). The height values obtained at
# following this transformation therefore correspond to a proportion
# and are included, with the addition of the coefficient 100, between 0
# (floor) and 100 (ceiling). Therefore, the values of f0 obtained are
# directly comparable between the speakers studied, independently
# their morphological, social or
# situational parameters of production (see 


# Need to calculate pitch range for each speaker.
range.norm.Hz<-c()
for (spk in unique(QA.v.z.NOTNA$speaker)){
  spkdata<-subset(QA.v.z.NOTNA,speaker==spk)
  
  # Get speaker pitch range
  spkqs<-quantile(spkdata$pitch.Hz,seq(0,1,0.01))
  ceiling <- spkqs["98%"]
  floor <- spkqs["2%"]
  speaknormed <- 100 * ( (spkdata$pitch.Hz-floor)/(ceiling-floor) )
  
  range.norm.Hz<-c(range.norm.Hz,speaknormed)
}

summary(range.norm.Hz)

r<-cor(range.norm.Hz,
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(range.norm.Hz,
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~range.norm.Hz),col="red")

n = length(range.norm.Hz)
t = (r/sqrt(1-r^2))*sqrt(n-2)
2*pt(-abs(t),df=n-1) # Significant correlation...

# These are VERY well correlated (r = 0.99), even at the extremes.
# This makes me feel like different normalization methods depending on different kinds of underlying parameters are unlikely to have a big effect on the data.

# Check correlation with original data
r<-cor(range.norm.Hz,
       QA.v.z.NOTNA$pitch.Hz)
r

rm(range.norm.Hz,n,t,r)

############
# Now do it in semitones
range.norm.ST<-c()
for (spk in unique(QA.v.z.NOTNA$speaker)){
  spkdata<-subset(QA.v.z.NOTNA,speaker==spk)
  
  # Get speaker pitch range
  spkqs<-quantile(spkdata$ST,seq(0,1,0.01))
  ceiling <- spkqs["98%"]
  floor <- spkqs["2%"]
  speaknormed <- 100 * ( (spkdata$ST-floor)/(ceiling-floor) )
  
  range.norm.ST<-c(range.norm.ST,speaknormed)
}

summary(range.norm.ST)

r<-cor(range.norm.ST,
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(range.norm.ST,
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~range.norm.ST),col="red")

# n = length(range.norm.ST)
# t = (r/sqrt(1-r^2))*sqrt(n-2)
# 2*pt(-abs(t),df=n-1) # Significant correlation...


# Check correlation with original data
r<-cor(range.norm.ST,
       QA.v.z.NOTNA$pitch.Hz)
r


# Double-check that choice of semitone formula doesn't matter
hzTOstFrench<-function(x,ref=1){
  12*log(base=2,(x/ref))
}
# Perfect correlation, no need to investigate further.
cor(QA.v.z.NOTNA$ST,
    hzTOstFrench(QA.v.z.NOTNA$pitch.Hz))
rm(hzTOstFrench)

rm(floor,ceiling,range.norm.ST,spk)

#################
# A Comparison of Tone Normalization Methods for Language Variation
# Jingwei Zhang

# Method 9: ST-AvgF0 ref=AvgF0 (i.e. each
#                               speaker's average
#                               pitch)

norm.ST.mean<-c()
for (spk in unique(QA.v.z.NOTNA$speaker)){
  spkdata<-subset(QA.v.z.NOTNA,speaker==spk)
  
  # Get speaker pitch mean
  spkmean<-mean(spkdata$pitch.Hz)
  speaknormed <- hzTOst(spkdata$pitch.Hz,ref=spkmean)
  
  norm.ST.mean<-c(norm.ST.mean,speaknormed)
}

summary(norm.ST.mean)

r<-cor(norm.ST.mean,
       QA.v.z.NOTNA$pitch.Hz.zscore)
r

qqplot(norm.ST.mean,
       QA.v.z.NOTNA$pitch.Hz.zscore)
abline(lm(QA.v.z.NOTNA$pitch.Hz.zscore~norm.ST.mean),col="red")

# These are VERY well correlated, though they break apart somewhat at the extremes, where outliers are anyway.

# Check correlation with original data
r<-cor(norm.ST.mean,
       QA.v.z.NOTNA$pitch.Hz)
r


################################
# Having assessed various normalization methods, NOW we remove outliers.

# Remove outliers
zthresh = 2.5

quantile(QA.v.z$pitch.Hz.zscore,
         prob=seq(0,1,0.05),
         na.rm = T) # How much data is going to be trimmed?

QA.v.trimmed<-subset(QA.v.z,
             abs(pitch.Hz.zscore) < zthresh)
rm(zthresh)

# It might appear that you're losing tons of data here,
# but that's mostly because NA values are being automatically stripped out.
nrow(QA.v.trimmed)
nrow(QA.v.z)
100-nrow(na.omit(QA.v.trimmed))/nrow(na.omit(QA.v.z))*100

simpleDensity(QA.v.trimmed,
              QA.v.trimmed$pitch.Hz.zscore,
              "Hz (z-scored)"
              )

QA.v.z<-QA.v.trimmed
rm(QA.v.trimmed)

# Given the small amount of data being removed here, there's not much point in re-normalizing the data after outlier subtraction. When you do, the correlation is very high, like > 0.99
#
# # Recalculate z-scores for cleaned data
# colnames(QA.v.z)
# QA.v.z<-subset(QA.v.z,select=-c(pitch.Hz.zscore)) # Drop existing z-scores
# colnames(QA.v.z)
# 
# QA.v.z<-z.score(QA.v.z,"pitch.Hz")
# colnames(QA.v.z)
# 
# simpleDensity(QA.v.z,
#               QA.v.z$pitch.Hz.zscore,
#               "Hz (z-scored)"
# )


############
# Log-normalizing before doing a z-score transformation doesn't affect much, which makes sense.
# x<-QA.v.z
# x$log<-log(x$pitch.Hz)
# x<-z.score(x,"log")
# cor(QA.v.z$pitch.Hz.zscore,
#     x$log.zscore)
# rm(x)


##################
# If you want to reduce the number of points you intend to plot/analyze by sub-sampling from
# steps, this would be a good place to do it. This can make plotting faster.
#
# QA.v.z<-subset(QA.v.z,
#                 step%in%seq(0,max(QA.v.z$step)-1,2)
#                 )


##################
# How many tokens per-speaker, following normalization and outlier removal?
spk.token.count<-ddply(QA.v.z,
                       .(speaker),
                       summarize,
                       n_unique = n_distinct(token.code)
)

spk.token.count
summary(spk.token.count)
simpleDensity(spk.token.count,
              spk.token.count$n_unique,
              "Unique tokens")
summary(spk.token.count)
sum(spk.token.count$n_unique)


####################################
# Start recoding factors from raw Praat output

# Vowel data
dummy.vec<-rep(NA,nrow(QA.v.z))
targ.wd<-dummy.vec
act.wd<-dummy.vec
disc.fnc<-dummy.vec
position<-dummy.vec
rm(dummy.vec)

counter<-1
for (cell in QA.v.z$code){
  fact.list<-unlist(strsplit(cell,"-"))
  targ.wd[counter]<-fact.list[1]
  act.wd[counter]<-fact.list[2]
  disc.fnc[counter]<-fact.list[3]
  position[counter]<-fact.list[4]
  counter<-counter+1
}
rm(counter,cell,fact.list)

QA.v.z<-cbind(QA.v.z,
              targ.wd,act.wd,disc.fnc,position)
rm(targ.wd,act.wd,disc.fnc,position)
colnames(QA.v.z)
head(QA.v.z)


##################
# Check conditions for garbage, and repair them.

summary(QA.v.z$disc.fnc)
summary(QA.v.z$position)

# I'm assuming here that some people used 'f' instead of 'l' when coding items in final position, so I merge them.
QA.v.z$position<-revalue(QA.v.z$position,
                         c("f"="l")
)

QA.v.z<-droplevels(QA.v.z)
summary(QA.v.z$disc.fnc)
summary(QA.v.z$position)


##################
# Recode discourse and position factors for clarity.

QA.v.z$disc.fnc<-revalue(QA.v.z$disc.fnc,
                         c("f"="Focused","g"="Given")) # Plain English titles
QA.v.z$disc.fnc<-relevel(QA.v.z$disc.fnc,
                         "Given") # Make "Given" the first factor level

QA.v.z$position<-revalue(QA.v.z$position,
                         c("l"="Final","m"="Medial")) # Plain English titles
QA.v.z$position<-relevel(QA.v.z$position,
                         "Medial") # Make "Medial" the first factor level



#########
# Vowel length and tone are already coded for vowels in Praat TextGrids, but we want to reparse them into factors.
head(QA.v.z)
summary(QA.v.z$segment)

# Tone coding:
# Use regular expressions to treat any vowel code with T in it as a tonal vowel, otherwise non-tonal
QA.v.z$tone<-QA.v.z$segment
QA.v.z$tone<-gsub(QA.v.z$tone,
                  pattern=".*T.*",
                  replacement="Tonal")

QA.v.z$tone[QA.v.z$tone!="Tonal"]<-"Non-tonal"

QA.v.z$tone<-as.factor(QA.v.z$tone)
summary(QA.v.z$tone)


# Vowel length coding:
# Use regular expressions to treat any vowel code with L as long, otherwise short
QA.v.z$vlen<-QA.v.z$segment
QA.v.z$vlen<-gsub(QA.v.z$vlen,
                  pattern=".*L.*",
                  replacement="Long")

QA.v.z$vlen[QA.v.z$vlen!="Long"]<-"Short"

QA.v.z$vlen<-as.factor(QA.v.z$vlen)
summary(QA.v.z$vlen)


# Stress coding:
# Use regular expressions to treat any vowel code with 1 as stressed, otherwise unstressed 
QA.v.z$stress<-QA.v.z$segment
QA.v.z$stress<-gsub(QA.v.z$stress,
                    pattern=".*1$",
                    replacement="Stressed")

QA.v.z$stress[QA.v.z$stress!="Stressed"]<-"Unstressed"

QA.v.z$stress<-as.factor(QA.v.z$stress)  
summary(QA.v.z$stress)


# Check output
head(QA.v.z)
tail(QA.v.z)
attach(QA.v.z)
  table(stress,tone)
  table(vlen,tone)
  table(vlen,stress)
detach(QA.v.z)

# We want to code vowels by their position in the word, and position relative to stress as well.
#
# This is straightforward for stressed/tonal vowels, but harder to work out for unstressed vowels, which
# can be final/non-final or pre/post-tonic.

summary(QA.v.z$act.wd) # This won't be totally straightforward, as words range from 1-3 vowels/syllables


# Coding for position (and opacity because of post-tonic syncope) isn't straightforward. We write a complex function for it, which could probably be simpler or more elegant. It also runs slowly.

# Create columns for storing information about vowel position.
dummy.vec<-rep(NA,nrow(QA.v.z))
QA.v.z$v.pos.wd<-dummy.vec
QA.v.z$v.pos.str<-dummy.vec
QA.v.z$opacity<-dummy.vec
rm(dummy.vec)


# Code positional factors
for (wd.token in unique(QA.v.z$token.code)){
  
  # For each word, figure out which vowel is the final vowel, and which the penultimate.
  wd.data = subset(QA.v.z,token.code==wd.token)
  intervals = unique(wd.data$interval.num)
  finalfirst = rev(sort(intervals))
  
  finalCode = finalfirst[1]
  penultCode = finalfirst[2]
  
  # Now iterate through the vowels in each word, adding factor values based on position.
  for (rownum in 1:nrow(wd.data)){
    vRow = wd.data[rownum,]
    vInt = vRow$interval.num
    
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
    QA.v.z[rowIdentifier,"v.pos.wd"]<-posCode
    QA.v.z[rowIdentifier,"v.pos.str"]<-strposCode
    QA.v.z[rowIdentifier,"opacity"]<-opqCode
  }
}
rm(posCode,strposCode,opqCode,wd.data,vRow,finalCode,finalfirst,intervals,rowIdentifier,rownum,vInt,wd.token)

QA.v.z$v.pos.wd<-factor(QA.v.z$v.pos.wd)
QA.v.z$v.pos.str<-factor(QA.v.z$v.pos.str)
QA.v.z$opacity<-factor(QA.v.z$opacity)


# Check the output
head(QA.v.z)
summary(QA.v.z$opacity)
summary(QA.v.z$v.pos.str)
summary(QA.v.z$v.pos.wd)
attach(QA.v.z)
  table(v.pos.wd,stress)
  table(v.pos.wd,tone)
  table(v.pos.wd,opacity)
detach(QA.v.z)

# Order levels for word-internal position.
QA.v.z$v.pos.wd<-factor(QA.v.z$v.pos.wd,
                        levels=c("Pre-penult","Penult","Ultima"))

# Create code for interaction of stress and tone
QA.v.z$accent<-factor(interaction(QA.v.z$stress,QA.v.z$tone))
levels(QA.v.z$accent)
QA.v.z$accent<-revalue(QA.v.z$accent,
                       c("Stressed.Non-tonal"="Stressed",
                         "Stressed.Tonal"="Tonal",
                         "Unstressed.Non-tonal"="Unstressed"))
summary(QA.v.z$accent)
attach(QA.v.z)
  table(v.pos.wd,accent)
detach(QA.v.z)

head(QA.v.z)
tail(QA.v.z)


####
# Adjust position coding so that opaque final, tonal, short vowels are treated as penultimate instead
QA.v.z$v.pos.wd.adj<-QA.v.z$v.pos.wd

for (row in 1:nrow(QA.v.z)){
  if (QA.v.z[row,"opacity"]=="Opaque" & QA.v.z[row,"vlen"]=="Short"){
    QA.v.z[row,"v.pos.wd.adj"]<-"Penult"
  }
}

##############
# Sort factors you'll be plotting/faceting
# Word position
QA.v.z$v.pos.wd<-as.factor(QA.v.z$v.pos.wd)
levels(QA.v.z$v.pos.wd)
QA.v.z$v.pos.wd<-factor(QA.v.z$v.pos.wd,
                        levels=rev(levels(QA.v.z$v.pos.wd)))
levels(QA.v.z$v.pos.wd)

# Accent type
QA.v.z$accent<-as.factor(QA.v.z$accent)
levels(QA.v.z$accent)
QA.v.z$accent<-factor(QA.v.z$accent,
                      levels=c("Tonal","Stressed","Unstressed"))
levels(QA.v.z$accent)


summary(QA.v.z$v.pos.wd)
summary(QA.v.z$v.pos.wd.adj)
# Order levels for word-internal position.
QA.v.z$v.pos.wd.adj<-factor(QA.v.z$v.pos.wd.adj,
                            levels=c("Pre-penult","Penult","Ultima"))


# Subset by word length
QA.v.z.short<-subset(QA.v.z,vlen=="Short")
QA.v.z.short<-droplevels(QA.v.z.short)

QA.v.z.long<-subset(QA.v.z,vlen=="Long")
QA.v.z.long<-droplevels(QA.v.z.long)




####################################
# Start analyzing the basic pitch patterns.
####################################


################
# Violin plots
################

# Get a dataframe which has exactly one unique row per vowel.
head(QA.v.raw)
plyr::count(QA.v.raw,vars=c("step")) # Steps are balanced so you can get unique vowels easily.
QA.v.unique<-subset(QA.v.raw,step==1)
QA.v.unique<-z.score(QA.v.unique,"mean.pitch.Hz")
QA.v.unique<-z.score(QA.v.unique,"max.pitch.Hz")

head(QA.v.unique)


##########
# Rather than repeating all of the code used to add factors to the pitch trajectory data, we use a lookup technique to copy over those factors. This should be cleaner and faster. 
QA.v.unique$v.code<-interaction(QA.v.unique$token.code,QA.v.unique$interval.num)
head(plyr::count(QA.v.unique,vars=c("v.code")))
max(plyr::count(QA.v.unique,vars=c("v.code"))[,"freq"]) # Verify that the codes are truly unique.

QA.v.z$v.code<-interaction(QA.v.z$token.code,QA.v.z$interval.num)
head(plyr::count(QA.v.z,vars=c("v.code")))
max(plyr::count(QA.v.z,vars=c("v.code"))[,"freq"]/7) # Verify that the codes are truly unique.


colnames(QA.v.unique)
colnames(QA.v.z)
addcols<-c("targ.wd","act.wd",
           "disc.fnc","position",
           "tone","vlen","stress",
           "v.pos.wd","v.pos.str","opacity", "accent","v.pos.wd.adj",
           "v.code")

QA.v.unique.factors<-merge(QA.v.unique,QA.v.z[,addcols],by="v.code")

# Merging creates lots of duplicate rows.
head(QA.v.unique.factors)
head(duplicated(QA.v.unique.factors))

# We drop duplicate rows.
QA.v.unique<-QA.v.unique.factors[!duplicated(QA.v.unique.factors),]
head(QA.v.unique)
head(duplicated(QA.v.unique))
summary(duplicated(QA.v.unique))

# Check that merged data doesn't have gaps -- looks good.
summary(QA.v.unique$position)
summary(QA.v.unique$disc.fnc)



##########
# Outlier removal
simpleDensity(QA.v.unique,
              QA.v.unique$mean.pitch.Hz.zscore,
              "Unique tokens")

simpleDensity(QA.v.unique,
              QA.v.unique$max.pitch.Hz.zscore,
              "Unique tokens")


# Trim outliers
zthresh = 2.5

# Mean f0
quantile(QA.v.unique$mean.pitch.Hz.zscore,
         prob=seq(0,1,0.01),
         na.rm = T) # How much data is going to be trimmed?

QA.v.unique.mean.trim<-subset(QA.v.unique,
                              abs(mean.pitch.Hz.zscore) < zthresh)

nrow(QA.v.unique.mean.trim)
nrow(QA.v.unique)
100-nrow(na.omit(QA.v.unique.mean.trim))/nrow(na.omit(QA.v.unique))*100

simpleDensity(QA.v.unique.mean.trim,
              QA.v.unique.mean.trim$mean.pitch.Hz.zscore,
              "Hz (z-scored)"
)


# Max f0
quantile(QA.v.unique$max.pitch.Hz.zscore,
         prob=seq(0,1,0.01),
         na.rm = T) # How much data is going to be trimmed?

QA.v.unique.max.trim<-subset(QA.v.unique,
                             abs(max.pitch.Hz.zscore) < zthresh)

nrow(QA.v.unique.max.trim)
nrow(QA.v.unique)
100-nrow(na.omit(QA.v.unique.max.trim))/nrow(na.omit(QA.v.unique))*100

simpleDensity(QA.v.unique.max.trim,
              QA.v.unique.max.trim$max.pitch.Hz.zscore,
              "Hz (z-scored)"
)


rm(zthresh)



###########
# Do some further subsetting:
QA.v.unique$vlen<-factor(QA.v.unique$vlen,levels=c("Short","Long")) # Order levels

stressed.unique<-subset(QA.v.unique,stress=="Stressed")
stressed.unique.v<-subset(stressed.unique,vlen=="Short")
stressed.unique.vv<-subset(stressed.unique,vlen=="Long")


#######
# Set plotting parameters
give.n <- function(x,ypos=-2.5){
  #  return(c(y = 40, label = length(x)))
  #
  # Setting the value of y here affects the vertical positioning of the labels.
  data.frame(y = ypos, label = paste0("n=",length(x)))
}




####
# Accented short vowels by discourse function x position
currdata<-subset(QA.v.unique.mean.trim,vlen=="Short" & stress=="Stressed")

V.plot<-ggplot(currdata,
               aes(x=tone,
                   y=mean.pitch.Hz.zscore,
                   fill=accent))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  geom_violin(alpha=0.75)+
  theme_bw()+
  theme(text=element_text(size=36),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
  )+
  scale_fill_manual(values=cbPalette,
                    guide=F)+
  
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = NA,fill="white")+
  stat_summary(fun="median",geom='point',size=6,pch=18)+
  stat_summary(fun.data = give.n, geom = "text", size=7,
               col="black",fontface="bold")+
  stat_summary(fun="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  
  ylab("Mean pitch (z-scores over Hz)")+
  xlab("Tone type")+
  labs(fill = "Accent type")+
  #coord_cartesian(ylim=c(-1.5,1.5))+
  ggtitle("Short vowels")



# All data
short.v.violin<-V.plot+facet_grid(position~disc.fnc)
short.v.violin

output_file<-paste0(imageDir,"Means/short_v_stressed_mean_f0_disc_x_pos.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
short.v.violin
dev.off()


####
# Accented long vowels by discourse function x position
currdata<-subset(QA.v.unique.mean.trim,vlen=="Long" & stress=="Stressed")

V.plot<-ggplot(currdata,
               aes(x=tone,
                   y=mean.pitch.Hz.zscore,
                   fill=accent))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  geom_violin(alpha=0.75)+
  theme_bw()+
  theme(text=element_text(size=36),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
  )+
  scale_fill_manual(values=cbPalette,
                    guide=F)+
  
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = NA,fill="white")+
  stat_summary(fun="median",geom='point',size=6,pch=18)+
  stat_summary(fun.data = give.n, geom = "text", size=7,
               col="black",fontface="bold")+
  stat_summary(fun="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  
  ylab("Mean pitch (z-scores over Hz)")+
  xlab("Tone type")+
  labs(fill = "Accent type")+
  #coord_cartesian(ylim=c(-1.5,1.5))+
  ggtitle("Long vowels")


# All data
long.v.violin<-V.plot+facet_grid(position~disc.fnc)
long.v.violin

output_file<-paste0(imageDir,"Means/long_v_stressed_mean_f0_disc_x_pos.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
long.v.violin
dev.off()




#############
# Plot pitch maximum
####
# Accented short vowels by discourse function x position
currdata<-subset(QA.v.unique.max.trim,vlen=="Short" & stress=="Stressed")

V.plot<-ggplot(currdata,
               aes(x=tone,
                   y=max.pitch.Hz.zscore,
                   fill=accent))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  geom_violin(alpha=0.75)+
  theme_bw()+
  theme(text=element_text(size=36),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
  )+
  scale_fill_manual(values=cbPalette,
                    guide=F)+
  
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = NA,fill="white")+
  stat_summary(fun="median",geom='point',size=6,pch=18)+
  stat_summary(fun.data = give.n, geom = "text", size=7,
               col="black",fontface="bold")+
  stat_summary(fun="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  
  ylab("Maximum pitch (z-scores over Hz)")+
  xlab("Tone type")+
  labs(fill = "Accent type")+
  #coord_cartesian(ylim=c(-1.5,1.5))+
  ggtitle("Short vowels")



# All data
short.v.violin<-V.plot+facet_grid(position~disc.fnc)
short.v.violin

output_file<-paste0(imageDir,"Means/short_v_stressed_max_f0_disc_x_pos.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
short.v.violin
dev.off()


####
# Accented long vowels by discourse function x position
currdata<-subset(QA.v.unique.max.trim,vlen=="Long" & stress=="Stressed")

V.plot<-ggplot(currdata,
               aes(x=tone,
                   y=max.pitch.Hz.zscore,
                   fill=accent))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  geom_violin(alpha=0.75)+
  theme_bw()+
  theme(text=element_text(size=36),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
  )+
  scale_fill_manual(values=cbPalette,
                    guide=F)+
  
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = NA,fill="white")+
  stat_summary(fun="median",geom='point',size=6,pch=18)+
  stat_summary(fun.data = give.n, geom = "text", size=7,
               col="black",fontface="bold")+
  stat_summary(fun="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  
  ylab("Maximum pitch (z-scores over Hz)")+
  xlab("Tone type")+
  labs(fill = "Accent type")+
  #coord_cartesian(ylim=c(-1.5,1.5))+
  ggtitle("Long vowels")


# All data
long.v.violin<-V.plot+facet_grid(position~disc.fnc)
long.v.violin

output_file<-paste0(imageDir,"Means/long_v_stressed_max_f0_disc_x_pos.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  long.v.violin
dev.off()


############
# Variability check -- are variances different for tonal vs. non-tonal vowels?

# # You just took this method from this site, you don't actually understand broom::tidy etc.
# #https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories
library(broom)
library(HH) # For brown-forsythe test. There are lots of function name clashes here, so be careful. Shouldn't be an issue, though.

summary(stressed.unique)

summary(stressed.unique$mean.pitch.Hz)
hov(mean.pitch.Hz~tone, data=stressed.unique)
stressed.unique %>% group_by(vlen,disc.fnc,position) %>% do(tidy(hov(mean.pitch.Hz~tone, data=.)))

summary(stressed.unique$mean.pitch.Hz.zscore)
hov(mean.pitch.Hz.zscore~tone, data=stressed.unique)
stressed.unique %>% group_by(vlen,disc.fnc,position) %>% do(tidy(hov(mean.pitch.Hz.zscore~tone, data=.)))


# Looks like, with limited exceptions, that tonal and non-tonal vowels have similar variances (only Long x Given x Medial shows a robust difference, and also Long x Focused x Medial when over z-scores.)
detach(package:broom)
detach(package:HH)



##################################
# lmer analysis of mean pitch
##################################

# Recode some factors and order levels
mean.clean<-QA.v.unique.mean.trim
mean.clean$vlen<-revalue(mean.clean$vlen,c("Short" = "V", "Long" = "V\u2D0"))
mean.clean$vlen<-factor(mean.clean$vlen,levels=c("V","V\u2D0"))

mean.clean$stress<-factor(mean.clean$stress,levels=c("Unstressed","Stressed")) # Order levels
mean.clean$accent<-factor(mean.clean$accent,levels=c("Unstressed","Stressed","Tonal"))

###########
# Add some coding for vowel quality
mean.clean$v.qual<-mean.clean$segment
mean.clean$v.qual<-gsub(mean.clean$v.qual, # Use regular expressions to strip out coding
                        pattern="T|S|L|1|0",
                        replacement="")

mean.clean$v.qual<-factor(mean.clean$v.qual)
summary(mean.clean$v.qual)


###################
# Add an effect for vowel height 
mean.clean$vheight<-mean.clean$v.qual
mean.clean$vheight<-revalue(mean.clean$vheight,
                            c("A"="Non-high",
                              "E"="Non-high",
                              "O"="Non-high",
                              "I"="High",
                              "U"="High"
                            ))
summary(mean.clean$vheight)



# See what interactions are even possible.
attach(mean.clean)
  table(v.pos.wd.adj)
  # Using tone and stress separately
  table(tone,vlen) # Fine
  table(stress,vlen) # Can't be used
  
  table(tone,vheight) # Fine
  table(stress,vheight) # Fine
  
  table(tone,v.pos.wd.adj) # Can't be used without dropping unstressed
  table(stress,v.pos.wd.adj) # Can't be used without dropping unstressed
  
  table(tone,disc.fnc) # Fine
  table(stress,disc.fnc) # Fine
  
  table(tone,position) # Fine
  table(stress,position) # Fine
  
  table(disc.fnc,position) # Fine
  table(v.pos.wd.adj,position) # Sort of fine, but really unbalanced for pre-penult...
  
  table(stress,v.pos.wd.adj)# Can't be used without dropping unstressed
  table(tone,v.pos.wd.adj) # Can't be used without dropping unstressed
  
  
  # Using composite accent factor
  table(accent,vlen) # Can't be used, unless you drop all unstressed vowels from the analysis (which is an option)
  table(accent,vheight) # Fine
  table(accent,v.pos.wd.adj) # Can't be used
  table(accent,disc.fnc) # Fine
  table(accent,position) # Fine
  table(disc.fnc,position) # Fine
  table(v.pos.wd.adj,position) # Sort of fine, but really unbalanced for pre-penult...
detach(mean.clean)

  
# Remove all pre-penultimate vowels
mean.clean<-subset(mean.clean,v.pos.wd.adj!="Pre-penult")
    
# Clean up factors
mean.clean<-droplevels(mean.clean)


#######
# Add some information about speaker demographics
# https://sapa-project.org/blog/2013/06/28/vlookup-in-R/
mean.clean$Age<-spk.demo$Age[match(mean.clean$speaker,spk.demo$Speaker)]
mean.clean$Gender<-spk.demo$Gender[match(mean.clean$speaker,spk.demo$Speaker)]
mean.clean$Born<-spk.demo$Born[match(mean.clean$speaker,spk.demo$Speaker)]
mean.clean$Lives<-spk.demo$Lives[match(mean.clean$speaker,spk.demo$Speaker)]
table(mean.clean$speaker,mean.clean$Age)



##############
# Let's look at speaker means for some predictors to see if random slopes might be justified.
# https://web.stanford.edu/class/psych252/section/Mixed_models_tutorial.html

# Tone and stress
accent.means<-ddply(mean.clean,
                    .(speaker,accent,tone,stress,Age,Gender,Born,Lives),
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
  geom_label(data=subset(tone.means,tone=="Non-tonal"),aes(label=speaker,x=0.9),size=8)+
  scale_y_continuous(limits=c(100,260))+
  theme_bw(base_size = 48)+
  theme(legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=48),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32))+
  xlab("Tone")+
  ylab("Mean pitch (Hz)")+
  labs(color = "Speaker")+
  theme(legend.position = "none")

tone.means.plot

output_file<-paste0(imageDir,"Means/Usp_QA_speaker_pitch_by_tone_means.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  tone.means.plot
dev.off()



# Check by vowel length
accent.means<-ddply(mean.clean,
                    .(speaker,accent,tone,stress,Age,Gender,Born,Lives,vlen),
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
  geom_label(data=subset(tone.means,tone=="Non-tonal"),aes(label=speaker,x=0.9),size=8)+
  scale_y_continuous(limits=c(100,260))+
  theme_bw(base_size = 48)+
  theme(legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=48),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32))+
  xlab("Tone")+
  ylab("Mean pitch (Hz)")+
  labs(color = "Speaker")+
  theme(legend.position = "none")+
  facet_grid(.~vlen)

tone.means.plot

output_file<-paste0(imageDir,"Means/Usp_QA_speaker_pitch_by_tone_means_by_vlen.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  tone.means.plot
dev.off()




# Plot which differences are significant
summary(mean.clean)
summary(mean.clean$mean.pitch.Hz)
library(broom)
pitch.ttests<- subset(mean.clean,stress=="Stressed") %>% group_by(speaker) %>% do(tidy(t.test(mean.pitch.Hz~tone, data=.,alternative="less")))
pitch.ttests
detach(package:broom)

# Add p-values and difference to frame:
# https://stackoverflow.com/questions/54596514/lookup-value-based-on-multiple-columns-in-data-frame
pitch.ttests
pitch.means.str = tone.means %>% left_join(pitch.ttests[,c("speaker","estimate","p.value")], by = c("speaker"))

head(pitch.means.str)

pitch.means.str$result.type<-pitch.means.str$p.value
pitch.means.str[pitch.means.str$estimate<0 & pitch.means.str$p.value<=0.05,]$result.type<-"Tonal\nhigher\n"
# pitch.means.str[pitch.means.str$estimate>0 & pitch.means.str$p.value<=0.05,]$result.type<-"Tonal\nlower\n"
pitch.means.str[pitch.means.str$p.value>0.05,]$result.type<-"n.s. (p\u003E.05)"

pitch.means.str$result.type<-factor(pitch.means.str$result.type,
                                    levels=c("Tonal\nhigher\n","Tonal\nlower\n","n.s. (p\u003E.05)")
)



pitch.means.str.plot.sig<-ggplot(pitch.means.str, aes(x=tone, y=mean_pitch)) +
  geom_point(size=4, aes(color = result.type,group=speaker))+
  geom_line(size=2, aes(color = result.type,group=speaker,lty=result.type))+
  geom_label(data=subset(pitch.means.str,tone=="Non-tonal"),aes(label=speaker,x=0.7),size=8)+
  geom_label(data=subset(pitch.means.str,tone=="Non-tonal"),aes(label=speaker,x=0.7),size=8)+
  theme_bw(base_size = 48)+
  theme(legend.key.width=unit(6,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=48),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        axis.title.x = element_blank()
        )+
  scale_y_continuous(limits=c(100,260))+
  ylab("Mean pitch (Hz)")+
  facet_grid(.~vlen)+
  labs(color = "",lty="")+
  scale_color_manual(values=c(lightBlue,"grey",orng))


pitch.means.str.plot.sig

output_file<-paste0(imageDir,"Means/Usp_QA_speaker_pitch_by_tone_means_signif.pdf")
cairo_pdf(file=output_file,
          width=18,height=8)
  pitch.means.str.plot.sig
dev.off()


# Let's see if demographic factors have anything to do with it.
tone.means$Age<-spk.demo$Age[match(tone.means$speaker,spk.demo$Speaker)]
tone.means$Gender<-spk.demo$Gender[match(tone.means$speaker,spk.demo$Speaker)]
tone.means$Born<-spk.demo$Born[match(tone.means$speaker,spk.demo$Speaker)]
tone.means$Lives<-spk.demo$Lives[match(tone.means$speaker,spk.demo$Speaker)]

tone.means.plot.gender<-
  pitch.means.str.plot.sig<-ggplot(pitch.means.str, aes(x=tone, y=mean_pitch)) +
  geom_point(size=4, aes(color = Gender,group=speaker))+
  geom_line(size=2, aes(color = Gender,group=speaker))+
  geom_label(data=subset(pitch.means.str,tone=="Non-tonal"),aes(label=speaker,x=0.7),size=8)+
  geom_label(data=subset(pitch.means.str,tone=="Non-tonal"),aes(label=speaker,x=0.7),size=8)+
  theme_bw(base_size = 48)+
  theme(legend.key.width=unit(6,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=48),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32))+
  scale_y_continuous(limits=c(100,260))+
  xlab("Tone")+
  ylab("Mean pitch (Hz)")+
  facet_grid(.~vlen)+
  labs(color = "Gender")
  
tone.means.plot.gender

output_file<-paste0(imageDir,"Means/Usp_QA_speaker_pitch_by_tone_means_gender.pdf")
cairo_pdf(file=output_file,
          width=18,height=8)
  tone.means.plot.gender
dev.off()





###################################
# Make sure that you're happy with the levels of 'item'
summary(mean.clean$targ.wd)
length(unique(mean.clean$targ.wd))

mean.clean$targ.wd<-revalue(mean.clean$targ.wd,
                                c("jcháaj"="cháaj",
                                  "jch7úuk7"="ch7úuk7",
                                  "jkaach7"="kaach7",
                                  "jkinaq7"="kinaq7",
                                  "qálq"="qálaq",
                                  "rixóql"="rixóqil",
                                  "jóoq7"="qajóoq7",
                                  "xoot"="qaxoot",
                                  "jtélb7"="téleb7",
                                  "jtéleb7"="téleb7",
                                  "jxajab7"="xajab7",
                                  "jxájab7"="xájab7"
                                ))


summary(mean.clean$targ.wd)
length(unique(mean.clean$targ.wd))

# You can't estimate item-level random effects for items that only occur once in the data.
mean.clean<-subset(mean.clean,targ.wd!="ch7aat")
mean.clean<-droplevels(mean.clean)
summary(mean.clean$targ.wd)


# Create some data subsets
mean.clean.str<-subset(mean.clean,stress=="Stressed")
mean.clean.v<-subset(mean.clean.str,vlen=="V")
mean.clean.vv<-subset(mean.clean.str,vlen=="V\u02D0")

mean.clean.str<-droplevels(mean.clean.str)
mean.clean.v<-droplevels(mean.clean.v)
mean.clean.vv<-droplevels(mean.clean.vv)


##########
# Fit a model

# Packages
library(lme4)
library(lmerTest)
# detach(package:lmerTest)
source(paste0(scriptDir,"lmer_collinearity_tools.R")) # For assessing model collinearity
library(piecewiseSEM) # For computing marginal r^2; https://github.com/jslefche/piecewiseSEM
                      # https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/


# Include unstressed vowels to assess the effect of stress on tone? See how many unstressed vowels you actually have.
summary(mean.clean$stress) # It's a decent proportion of the data, no? 

full.m<-lmer(data=mean.clean,
             # Use REML=F for log-likelihood comparison in step-down model reduction.
             # If you use lmerTest for doing step-down model reduction, you want to use REML=T:
             # https://link.springer.com/article/10.3758/s13428-016-0809-y
             REML=F,
             #
             # Increase the number of iterations to convergence.
             # https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022084.html
             control=lmerControl(optCtrl=list(maxfun=1e5),optimizer = "bobyqa"),
             #
             # Dependent variable:
             mean.pitch.Hz~
               #
               # Fixed effects: all required by theoretical interest, and allowed by convergence/factor crossing
               tone*position*disc.fnc+
               stress*position*disc.fnc+ # If you are including unstressed vowel data...
               vheight+
               vlen*tone+
               # Random effects
               # Simple random intercepts
               (1|targ.wd)+
               (1|speaker)
               # By speaker random slopes for tone/stress, correlated with by-speaker random intercepts
               # The model fails to converge when these are included -- probably not enough per-speaker data.
               # (1+tone|speaker)
               # (1+stress|speaker)
          )


# Collinearity
max(vif.mer(full.m))
kappa.mer(full.m) # Large collinearity

# Model
summary(full.m)
anova(full.m)

# Model reduction
anova(full.m)[order(anova(full.m)[5]),] # Shorthand for sorting anova by F-value (decreasing) (use 4 if lmerTest NOT loaded)
coef(summary(full.m))[order(abs(coef(summary(full.m))[,1]),decreasing=T),]

m2<-update(full.m,.~.-tone:position:disc.fnc)
anova(full.m,m2)
anova(m2)[order(anova(m2)[5]),]

m3<-update(m2,.~.-position:disc.fnc:stress)
anova(full.m,m2,m3)
anova(m3)[order(anova(m3)[5]),]

m4<-update(m3,.~.-tone:disc.fnc)
anova(full.m,m2,m3,m4)
anova(m4)[order(anova(m4)[5]),]

m5<-update(m4,.~.-position:stress)
anova(full.m,m2,m3,m4,m5)
anova(m5)[order(anova(m5)[5]),]

# m6<-update(m5,.~.-tone:position) # Sig.
m6<-update(m5,.~.-position:disc.fnc) # Sig.
anova(full.m,m2,m3,m4,m5,m6)
anova(m5)[order(anova(m5)[5]),]


final.m <- m5

# Collinearity
max(vif.mer(final.m))
kappa.mer(final.m) # Moderate collinearity

# Model
summary(final.m)
anova(final.m)

anova(final.m)[order(anova(final.m)[5],decreasing=T),] # Shorthand for sorting anova by F-value (decreasing) (use 4 if lmerTest NOT loaded)
options(scipen=999)
coef(summary(final.m))[order(abs(coef(summary(final.m))[,1]),decreasing=T),]
options(scipen=0)


# Check r2
rsquared(final.m)

# Check normality of residuals --- not too bad!
cor(qqnorm(residuals(final.m))$x,
    residuals(final.m))
qqnorm(residuals(final.m)) # Not ideal, but not too bad.
qqline(residuals(final.m))

summary(residuals(final.m))
ggplot(data=data.frame("resid"=residuals(final.m)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)



#############
# Let's fit *just* the stressed vowels.

full.m<-lmer(data=mean.clean.str,
             # Use REML=F for log-likelihood comparison in step-down model reduction.
             # If you use lmerTest for doing step-down model reduction, you want to use REML=T:
             # https://link.springer.com/article/10.3758/s13428-016-0809-y
             REML=F,
             #
             # Increase the number of iterations to convergence.
             # https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022084.html
             control=lmerControl(optCtrl=list(maxfun=1e5),optimizer = "bobyqa"),
             #
             # Dependent variable:
             mean.pitch.Hz~
               #
               # Fixed effects: all required by theoretical interest, and allowed by convergence/factor crossing
               tone*position*disc.fnc+
               vlen*tone*position+
               vlen*tone*disc.fnc+
               vlen*disc.fnc*position+
               vheight+
               # Random effects
               # Simple random intercepts
               (1|targ.wd)+
               (1|speaker)
             # By speaker random slopes for tone/stress, correlated with by-speaker random intercepts
             # The model fails to converge when these are included -- probably not enough per-speaker data.
             # (1+tone|speaker)
)


# Collinearity
max(vif.mer(full.m))
kappa.mer(full.m) # Large collinearity

# Model
summary(full.m)
anova(full.m)

# Model reduction
anova(full.m)[order(anova(full.m)[5]),] # Shorthand for sorting anova by F-value (decreasing) (use 4 if lmerTest NOT loaded)
coef(summary(full.m))[order(abs(coef(summary(full.m))[,1]),decreasing=T),]

# If you want to do this semi-automatically, use the lmerTest drop1() function. Pretty handy!
drop1(full.m)
drop1(full.m)[order(drop1(full.m)[6]),]

m1<-update(full.m,.~.-position:disc.fnc:vlen)
anova(full.m,m1)
anova(m1)[order(anova(m1)[5]),]

m2<-update(m1,.~.-tone:disc.fnc:vlen)
anova(full.m,m1,m2)
anova(m1)[order(anova(m1)[5]),]

m3<-update(m2,.~.-tone:position:disc.fnc)
anova(full.m,m1,m2,m3)
anova(m3)[order(anova(m3)[5]),]

m4<-update(m3,.~.-tone:position:vlen)
anova(full.m,m1,m2,m3,m4)
anova(m4)[order(anova(m4)[5]),]

m5<-update(m4,.~.-tone:vlen)
anova(full.m,m1,m2,m3,m4,m5)
anova(m5)[order(anova(m5)[5]),]

m6<-update(m5,.~.-tone:disc.fnc)
anova(full.m,m1,m2,m3,m4,m5,m6)
anova(m6)[order(anova(m6)[5]),]

m7<-update(m6,.~.-disc.fnc:vlen)
anova(full.m,m1,m2,m3,m4,m5,m6,m7)
anova(m7)[order(anova(m7)[5]),]

m8<-update(m7,.~.-tone:position)
anova(full.m,m1,m2,m3,m4,m5,m6,m7,m8)
anova(m8)[order(anova(m8)[5]),]

m9<-update(m8,.~.-vheight)
anova(full.m,m1,m2,m3,m4,m5,m6,m7,m8,m9) # Sig. at p < 0.1
anova(m9)[order(anova(m9)[5]),]

m10<-update(m9,.~.-position:vlen) # Sig. at p < 0.1
anova(full.m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
anova(m10)[order(anova(m10)[5]),]

m11<-update(m10,.~.-vlen) # Sig.
anova(full.m,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11)
anova(m11)[order(anova(m11)[5]),]

final.m<-m11

# Collinearity
max(vif.mer(final.m))
kappa.mer(final.m) # Moderate collinearity

# Model
summary(final.m)
anova(final.m)

anova(final.m)[order(anova(final.m)[5],decreasing=T),] # Shorthand for sorting anova by F-value (decreasing) (use 4 if lmerTest NOT loaded)
options(scipen=999)
coef(summary(final.m))[order(abs(coef(summary(final.m))[,1]),decreasing=T),]
options(scipen=0)


# Check r2
rsquared(final.m)

# Check normality of residuals --- not too bad!
cor(qqnorm(residuals(final.m))$x,
    residuals(final.m))
qqnorm(residuals(final.m)) # Not ideal, but not too bad.
qqline(residuals(final.m))

summary(residuals(final.m))
ggplot(data=data.frame("resid"=residuals(final.m)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)




###########
# Now we fit an lmer for the focused, medial condition specifically.
focmed<-subset(mean.clean,disc.fnc=="Focused" & position=="Medial")
dplyr::count(focmed,stress,v.pos.wd.adj)

full.m<-lmer(data=focmed,
             # Use REML=F for log-likelihood comparison in step-down model reduction.
             # If you use lmerTest for doing step-down model reduction, you want to use REML=T:
             # https://link.springer.com/article/10.3758/s13428-016-0809-y
             REML=F,
             #
             # Increase the number of iterations to convergence.
             # https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022084.html
             control=lmerControl(optCtrl=list(maxfun=1e5),optimizer = "bobyqa"),
             #
             # Dependent variable:
             mean.pitch.Hz~
               #
               # Fixed effects: all required by theoretical interest, and allowed by convergence/factor crossing
               tone*vlen+
               stress+
               #v.pos.wd.adj+
               vheight+
               start.time+
               # Random effects
               # Simple random intercepts
               (1|targ.wd)+
               (1|speaker)
             # By speaker random slopes, correlated with by-speaker random intercepts
             # The model fails to converge when these are included -- probably not enough per-speaker data.
             # (1+tone|speaker)
             # (1+start.time|speaker)
)


# Collinearity
max(vif.mer(full.m))
kappa.mer(full.m)

# Model
summary(full.m)
anova(full.m)

drop1(full.m)[order(drop1(full.m)[6]),]
m1<-update(full.m,.~.-tone:vlen)
anova(full.m,m1)

anova(m1)[order(anova(m1)[5]),]
drop1(m1)[order(drop1(m1)[6]),]
m2<-update(m1,.~.-stress)
anova(full.m,m1,m2)

anova(m2)[order(anova(m2)[5]),]
drop1(m2)[order(drop1(m2)[6]),]
m3<-update(m2,.~.-vheight)
anova(full.m,m1,m2,m3)

anova(m3)[order(anova(m3)[5]),]
drop1(m3)[order(drop1(m3)[6]),]
m4<-update(m3,.~.-vlen) # Sig. at p < .1
anova(full.m,m1,m2,m3,m4)

anova(m4)[order(anova(m4)[5]),]
drop1(m4)[order(drop1(m4)[6]),]
m5<-update(m4,.~.-start.time) # Sig. at p < .05
anova(full.m,m1,m2,m3,m4,m5)

final.m<-m4

# Collinearity
max(vif.mer(final.m))
kappa.mer(final.m) # Low collinearity

# Model
summary(final.m)
anova(final.m)

anova(final.m)[order(anova(final.m)[5],decreasing=T),] # Shorthand for sorting anova by F-value (decreasing) (use 4 if lmerTest NOT loaded)
options(scipen=999)
coef(summary(final.m))[order(abs(coef(summary(final.m))[,1]),decreasing=T),]
options(scipen=0)


# Check r2
rsquared(final.m)

# Check normality of residuals --- not too bad!
cor(qqnorm(residuals(final.m))$x,
    residuals(final.m))
qqnorm(residuals(final.m)) # Not ideal, but not too bad.
qqline(residuals(final.m))

summary(residuals(final.m))
ggplot(data=data.frame("resid"=residuals(final.m)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)






################
# Loess plots 
################

# Check for declination
t.test(data=subset(QA.v.unique,stress=="Stressed"),mean.pitch.Hz.zscore~position, alternative="greater")

# Get p-values for mean and max pitch by t-tests
library(broom)
mean.f0.test<-subset(QA.v.unique,stress=="Stressed") %>% group_by(vlen,disc.fnc,position) %>% do(tidy(t.test(mean.pitch.Hz.zscore~tone, data=.,alternative="less")))
mean.f0.test

# Check general mean f0 differences across conditions.
mean.f0.test$p.char<-format(mean.f0.test$p.value, scientific = FALSE) # Don't use scientific notation
mean.f0.test[mean.f0.test$p.char>0.05,]$p.char<-"n.s."
mean.f0.test[mean.f0.test$p.char<0.001,]$p.char<-"p\u003C.001"
mean.f0.test[mean.f0.test$p.char<0.01,]$p.char<-"p\u003C.01"
mean.f0.test[mean.f0.test$p.char<0.05,]$p.char<-"p\u003C.05"# Nothing meets this condition.
mean.f0.test
# summary(subset(mean.f0.test,vlen=="Short")$estimate)
summary(subset(mean.f0.test,vlen=="Long")$estimate)


# See whether the raising effect of focus applies equally to all conditions.
mean.f0.test.focus<-subset(QA.v.unique,stress=="Stressed") %>% group_by(vlen,position,tone) %>% do(tidy(t.test(mean.pitch.Hz.zscore~disc.fnc, data=.,alternative="less")))
mean.f0.test.focus$p.char<-format(mean.f0.test.focus$p.value, scientific = FALSE) # Don't use scientific notation
mean.f0.test.focus
mean.f0.test.focus[mean.f0.test.focus$p.char>0.05,]$p.char<-"n.s."
mean.f0.test.focus[mean.f0.test.focus$p.char<0.001,]$p.char<-"p\u003C.001"
mean.f0.test.focus[mean.f0.test.focus$p.char<0.01,]$p.char<-"p\u003C.01"
#mean.f0.test.focus[mean.f0.test.focus$p.char<0.05,]$p.char<-"p\u003C.05"# Nothing meets this condition.
mean.f0.test.focus


# Check general max f0 differences across conditions.
max.f0.test<-subset(QA.v.unique,stress=="Stressed") %>% group_by(vlen,disc.fnc,position) %>% do(tidy(t.test(max.pitch.Hz.zscore~tone, data=.,alternative="less")))
max.f0.test

max.f0.test$p.char<-format(max.f0.test$p.value, scientific = FALSE) # Don't use scientific notation
max.f0.test[max.f0.test$p.char>0.05,]$p.char<-"n.s."
max.f0.test[max.f0.test$p.char<0.001,]$p.char<-"p\u003C.001"
max.f0.test[max.f0.test$p.char<0.01,]$p.char<-"p\u003C.01"
max.f0.test[max.f0.test$p.char<0.05,]$p.char<-"p\u003C.05"# Nothing meets this condition.
max.f0.test

detach(package:broom) # I don't know why I always detach this package after using it?

########
# Create a dataframe for dataset means
meanPitch.df<-ddply(QA.v.z,
                    .(step,disc.fnc,position,vlen),
                    summarize,
                    meanHz=mean(pitch.Hz.zscore)
)

meanPitch.df$step<-as.numeric(meanPitch.df$step)


####
# Accented short vowels by discourse function x position
currdata<-droplevels(subset(QA.v.z.short,
                            stress=="Stressed"))

V.plot<-ggplot(currdata,
               aes(x=step,
                   y=pitch.Hz.zscore,
                   color=accent,
                   linetype=accent))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  # Add means over data set  
  # geom_line(data=subset(meanPitch.df,
  #                       vlen=="short"),
  #           aes(x=step,y=meanHz),
  #           inherit.aes=F,
  #           lwd=3,color=lightBlue,alpha=0.5)+
  
  geom_smooth(method="loess",lwd=3)+
  
  scale_color_manual(values=cbPalette
                     #,guide = "none"
                     )+
  scale_linetype_manual(values=c("solid","dashed","dotted")
                        #,guide = "none"
                        )+
  
  theme_bw()+
  
  theme(text=element_text(size=42),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
        
        # Use these options if you want a more B&W print-friendly plot.
        #panel.background = element_rect(fill="white"),
        #panel.grid.major = element_line(color = "#80808033",linetype="dashed"),
        #panel.grid.minor = element_line(color = "#80808033",linetype="dotted")
  )+
  
  coord_cartesian(ylim=c(-1.5,1.5))+
  
  scale_x_continuous("Time (normalized)", breaks=as.numeric(step.list), labels=waiver())+
  scale_y_continuous("Pitch (z-scores over Hz)", breaks=seq(-1.5,1.5,0.5), labels=waiver())+
  labs(linetype = "Short vowels:\nAccent type")+
  labs(color = "Short vowels:\nAccent type")
  
  #ggtitle("Short vowel pitch contours")


# All data
short.v.loess<-V.plot+facet_grid(position~disc.fnc)
short.v.loess

# Add p-values to the plot.
short.v.loess<-short.v.loess+geom_label(inherit.aes = F,
                         data=subset(mean.f0.test,vlen=="Short"),
                         aes(x=7.75,y=1.4,
                             label=p.char),
                         size=10)

short.v.loess

output_file<-paste0(imageDir,"Loess/Usp_QA_shortV_disc_by_pos_loess.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  short.v.loess
dev.off()



####
# Accented long vowels by discourse function x position
currdata<-droplevels(subset(QA.v.z.long,
                            stress=="Stressed"))

V.plot<-ggplot(currdata,
               aes(x=step,
                   y=pitch.Hz.zscore,
                   color=accent,
                   linetype=accent))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  # Add means over data set  
  # geom_line(data=subset(meanPitch.df,
  #                       vlen=="Long"),
  #           aes(x=step,y=meanHz),
  #           inherit.aes=F,
  #           lwd=3,color=lightBlue,alpha=0.5)+
  
  geom_smooth(method="loess",lwd=3)+
  
  scale_color_manual(values=cbPalette)+
  scale_linetype_manual(values=c("solid","dashed","dotted"))+
  
  theme_bw()+
  
  theme(text=element_text(size=42),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
        #axis.title.y = element_blank()
        
        # Use these options if you want a more B&W print-friendly plot.
        #panel.background = element_rect(fill="white"),
        #panel.grid.major = element_line(color = "#80808033",linetype="dashed"),
        #panel.grid.minor = element_line(color = "#80808033",linetype="dotted")
  )+
  
  coord_cartesian(ylim=c(-1.5,1.5))+
  
  scale_x_continuous("Time (normalized)", breaks=as.numeric(step.list), labels=waiver())+
  scale_y_continuous("Pitch (z-scores over Hz)",breaks=seq(-1.5,1.5,0.5), labels=waiver())+
  labs(linetype = "Long vowels:\nAccent type")+
  labs(color = "Long vowels:\nAccent type")
  
  #ggtitle("Long vowel pitch contours")


# All data
long.v.loess<-V.plot+facet_grid(position~disc.fnc)
long.v.loess

# Add p-values to the plot.
long.v.loess<-long.v.loess+geom_label(inherit.aes = F,
                                        data=subset(mean.f0.test,vlen=="Long"),
                                        aes(x=7.75,y=1.4,
                                            label=p.char),
                                        size=10)
long.v.loess

output_file<-paste0(imageDir,"Loess/Usp_QA_longV_disc_by_pos_loess.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  long.v.loess
dev.off()






##################
# Plot sequential pitch tracks
##################

QA.v.z$vlen<-revalue(QA.v.z$vlen,c("Short" = "V", "Long" = "V\u2D0"))
QA.v.z$vlen<-factor(QA.v.z$vlen,levels=c("V","V\u2D0"))
plotdata<-subset(QA.v.z,v.pos.wd.adj!="Pre-penult")

plotdata.foc<-subset(plotdata,disc.fnc=="Focused")
plotdata.giv<-subset(plotdata,disc.fnc=="Given")
plotdata.med<-subset(plotdata,position == "Medial")
plotdata.fin<-subset(plotdata,position == "Final")

plotdata.fm<-subset(plotdata,disc.fnc=="Focused" & position == "Medial")
plotdata.gm<-subset(plotdata,disc.fnc=="Given" & position == "Medial")
plotdata.ff<-subset(plotdata,disc.fnc=="Focused" & position == "Final")
plotdata.gf<-subset(plotdata,disc.fnc=="Given" & position == "Final")


seq.track.plot<-function(df){
  v.plot<-ggplot(df,
                 aes(x=step,
                     y=pitch.Hz.zscore,
                     color=accent,
                     linetype=accent))+
    
    geom_smooth(method="loess",lwd=6)+
    
    scale_color_manual(values=cbPalette)+
    scale_linetype_manual(values=c("solid","dashed","dotted"))+
    
    theme_bw()+
    
    theme(text=element_text(size=42),
          legend.key.width=unit(14,"line"),
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
  
  return(v.plot)
}


output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_pooled.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata)+coord_cartesian(ylim=c(-1,1))
dev.off()


output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_focused.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.foc)
dev.off()


output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_given.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.giv)
dev.off()


output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_medial.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.med)
dev.off()

output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_final.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.fin)
dev.off()


output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_focused_x_medial.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.fm)
dev.off()

output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_focused_x_final.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.ff)
dev.off()


output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_given_x_medial.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.gm)
dev.off()

output_file<-paste0(imageDir,"Sequential_tracks/sequential_f0_tracks_given_x_final.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  seq.track.plot(plotdata.gf)
dev.off()





############
# PCA
############

# You can't run PCA with missing values, and na.omit() may cause a massive loss of input data (every row/vowel that has an NA would be dropped).
#
# One way to deal with just a few NAs would be to replace NAs with some principled value (e.g. mean, interpolated value, etc.).
#
# See also:
# https://stats.stackexchange.com/questions/35561/imputation-of-missing-values-for-pca
# https://stackoverflow.com/questions/12078291/r-function-prcomp-fails-with-nas-values-even-though-nas-are-allowed
# https://link.springer.com/article/10.1007/s11258-014-0406-z
#
# In principle, the NAs could be resolved with functional PCA, which first smooths the signal (see below). BUt for our purposes this seems like overkill. If you want, you can try to implement FPCA using the original code from Gubian et al.:
# https://github.com/uasolo/FDA-DH/blob/master/scripts/FDA.R
#
# For the functional approach to work you may need to start with contours which have not already been time-normalized.


######################################
# Some alternative analytical techniques to consider:
#
# (i) Periograms
# https://osf.io/28ea5/
# https://assta.org/proceedings/ICPhS2019/papers/ICPhS_621.pdf
# https://www.isca-speech.org/archive/SpeechProsody_2018/abstracts/220.html
#
# (ii) SLAM+: An enhanced automatic stylizer for pitch (contour) of speech corpora derived from SLAM
# https://github.com/vieenrose/SLAMplus
#
# (iii) Functional PCA
# https://assta.org/proceedings/ICPhS2019/papers/ICPhS_750.pdf
# https://assta.org/proceedings/ICPhS2019/papers/ICPhS_50.pdf
# https://www.sciencedirect.com/science/article/pii/S009544701400076X
# https://pure.mpg.de/pubman/faces/ViewItemOverviewPage.jsp?itemId=item_2058393
# Gubian, M.,Boves,L.,&Cangemi,F.(2011).Jointanalysisof f0 and speechratewithFunctionalDataAnalysis.In Proceedings ofICASSP2011 (pp. 4972-4975), Prague,CzechRepublic.
######################################

library(stats) # for PCA and k-means
library(imputeTS) # for imputing (i.e. replacing) missing values (NAs) in time series data with one dependent measure

# You don't have any NA-containing rows --- they were all stripped out by an earlier call to subset()!
nrow(QA.v.z)-nrow(na.omit(QA.v.z))

QA.v.z$step<-as.factor(QA.v.z$step)
summary(QA.v.z$step) # You can see that steps were not dropped evenly across the board -- unsurprisingly, more data was lost at edges.
ggplot()+stat_count(data=QA.v.z,
                    aes(x=as.factor(step))
)


# Putting the data in wide format, which you need for PCA anyway, will put all of these NAs (and any other dropped data where less than the full observation was dropped) back where we need them as NAs, if you want to do data imputation.
# http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/

# We really just want to focus, for the moment, on the *accented* vowels.
vdata<-subset(QA.v.z,
              #stress=="Stressed" & disc.fnc == "Focused" & position == "Medial")
              stress=="Stressed")
nrow(vdata)

# Reduce dataframe size so things are easier to work with.
colnames(QA.v.z)
vdata.lean<-subset(vdata,
                   select = c(segment,
                              token.code,
                              interval.num,
                              step,
                              speaker,
                              pitch.Hz.zscore,
                              targ.wd,
                              act.wd,
                              disc.fnc,
                              position,
                              tone,
                              vlen,
                              stress,
                              v.pos.wd.adj,
                              accent
                   ))

head(vdata.lean)
tail(vdata.lean)

v.wide.PCA<-spread(vdata.lean,
                   step,
                   pitch.Hz.zscore)

head(v.wide.PCA)
tail(v.wide.PCA)



# First, let's try dropping peripheral steps 1 and 9 to eliminate a lot of NAs before we drop entire rows.
v.wide.PCA.mid<-dplyr::select(v.wide.PCA,
                              -one_of(c("1","9"))) # subset(select=-c()) was failing, probably because column names are #s
head(v.wide.PCA.mid)


# It looks like imputation could be important here --- you'll lose a lot of data if you ditch every observation with at least one NA in it.
nrow(v.wide.PCA)
nrow(v.wide.PCA.mid)
100*(nrow(v.wide.PCA)-nrow(na.omit(v.wide.PCA.mid)))/nrow(v.wide.PCA)


# But let's try a PCA anyway.
v.wide.PCA.mid.nona<-na.omit(v.wide.PCA.mid) # Drop NA containing rows.
pcinput<-v.wide.PCA.mid.nona[,as.character(seq(2,8,1))]
head(pcinput)


# The input to PCA should be pairs of time points (steps) and pitch values.
# Why scale first? So that relative variance explained is interpretable. See:
# https://stats.stackexchange.com/questions/69157/why-do-we-need-to-normalize-data-before-principal-component-analysis-pca
v.mid.PCA<-prcomp(pcinput,
                  scale=T)

# Most of the variance is explained by PC1, and almost all of it is explained by PC1+PC2
summary(v.mid.PCA)
cumulative.var.exp<-summary(v.mid.PCA)$importance[3,]
c.var.exp.df<-data.frame(cumulative.var.exp)
c.var.exp.df

c.var.exp<-gather(c.var.exp.df,
                  "PC",
                  "Cumulative variance explained"
)
c.var.exp.df
c.var.exp.df<-cbind(c.var.exp.df,"PC"=row.names(c.var.exp.df))
c.var.exp.df

# https://stackoverflow.com/questions/15043956/connecting-points
var.exp.plot<-ggplot(data=c.var.exp.df,
                     aes(x=PC,y=cumulative.var.exp,
                         group=1))+
  geom_point(size=5)+
  geom_line(lwd=2)+
  theme_bw(base_size=24)+
  ylab("Cumulative variance explained")+
  ggtitle("PCA over accented vowels, initial/final steps excluded")+
  scale_y_continuous(breaks=seq(0,1,0.05),limits = c(0.75,1))

var.exp.plot

output_file<-paste0(imageDir,"PCA/PCA_var_explained_no_imputation_steps2-8.pdf")
cairo_pdf(file=output_file,
          width=14,height=10)
  var.exp.plot
dev.off()


# For visualization, plot the principle components in
# the space defined by the original input dimensions (see Johnson 2008:100-2).

# From Johnson (2008:99-102) (but adapted)
# "PCA loadings are z-scores, so to visualize them you take
# loading times standard deviation, then add that to the variable's
# mean value to put the loading back into the original measurement
# units."

pcframe<-data.frame("sd"=v.mid.PCA$sd) # Get standard deviations for each PC
pcframe$PC<-as.factor(paste0("PC",as.character(seq(1,7,1))))

# Get variance explained
pcframe$Var.Exp<-paste0(round(
  summary(v.mid.PCA)$importance[2,],
  3)*100,
  "%")
pcframe

centers<-data.frame(cbind("Pitch"=v.mid.PCA$center,"Step"=seq(2,8,1))) # Get centers (i.e. means) for the data
centers

loads<-data.frame(v.mid.PCA$rotation) # Get loadings for each PC
loads<-cbind("Step"=seq(2,8,1),loads)
loads$centers<-centers$Pitch
loads


# We decide we only care about PCs 1-3, so we drop all others
pcframe<-droplevels(subset(pcframe,PC%in%c("PC1","PC2","PC3")))
pcframe
loads<-dplyr::select(loads,-c("PC4","PC5","PC6","PC7"))
loads


######
# Start plotting deviations

# Number of standard deviations for plotting:
stddev.set <- 1

# Create frame holding deviations
PCdeviations<-data.frame()
for (currPC in unique(pcframe$PC)){
  upper <- loads$centers + loads[,currPC] * (stddev.set*subset(pcframe,PC==currPC)$sd)
  upper.frame <- data.frame(cbind("PC"=currPC,
                                  "coords"=upper,
                                  "type"=rep("Positive",length(upper)),
                                  "Step"=loads$Step
  ))
  
  lower <- loads$centers - loads[,currPC] * (stddev.set*subset(pcframe,PC==currPC)$sd)
  lower.frame <- data.frame(cbind("PC"=currPC,
                                  "coords"=lower,
                                  "type"=rep("Negative",length(lower)),
                                  "Step"=loads$Step
  ))
  
  PCdeviations<- rbind(PCdeviations,upper.frame,lower.frame)
}
PCdeviations
PCdeviations$coords<-as.numeric(as.character(PCdeviations$coords))
PCdeviations$Step<-as.numeric(as.character(PCdeviations$Step))
summary(PCdeviations$coords)


# Start plotting
pc.mids<-ggplot(data=centers,
                aes(x=Step,
                    y=Pitch))+
  geom_point(size=4,color="grey")+
  geom_line(lwd=2,color="grey")+
  theme_bw(base_size=24)+
  theme(strip.text = element_text(face = "bold"),
        legend.key.width=unit(4,"line"),
        legend.text = element_text(face = "bold"))+
  geom_label(inherit.aes = F,
             data=pcframe,
             aes(x=7,y=1.15,
                 label=Var.Exp),
             size=10)+
  ggtitle("Dimensions of variation of each PC")+
  labs(subtitle="PCA over accented vowels, initial/final steps excluded")+
  ylab(paste0("Deviations (\u00B1",stddev.set,"\u03C3)"))+
  # Deviations
  geom_point(data=PCdeviations,
             inherit.aes = F,
             aes(y=coords,
                 x=Step,
                 group=type,
                 color=type),
             size=4)+
  geom_line(data=PCdeviations,
            inherit.aes = F,
            aes(y=coords,
                x=Step,
                group=type,
                color=type),
            lwd=2)+
  # theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(2,8,1))+
  facet_grid(.~PC)+
  scale_color_manual(values=cbPalette[3:length(cbPalette)],name="Direction")

pc.mids

output_file<-paste0(imageDir,"PCA/PCA_deviation_no_imputation_steps2-8.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  pc.mids
dev.off()


###############
# Now do PCA with data imputation

# Recode data in long format
summary(vdata.lean$step)
stepnames<- as.character(sort(unique(vdata.lean$step)))
# c("1","2","3","4","5","6","7","8","9")

v.PCA.long<-gather(v.wide.PCA,
                   key = "step",
                   value = "pitch.Hz.zscore",
                   stepnames # This tells gather() which columns are going to be eliminated. Their names are under the column 'key' (="step") and the values are under the column 'value' (=pitch.Hz.zscore)
)

# Looks like the long->wide->long translation has successfully put back NA for missing measurements and outliers.
nrow(v.PCA.long)
nrow(na.omit(v.PCA.long))
nrow(vdata.lean)

# Create unique ID code for each vowel so you can do data imputation on f0 trajectories of each vowel.
v.PCA.long$vowel.code<-interaction(v.PCA.long$token.code,
                                   v.PCA.long$interval.num)


# Re-sort the data frame so that it's easier to match imputed data to original data.
attach(v.PCA.long)
  v.PCA.long<-v.PCA.long[order(token.code,interval.num,step),]
detach(v.PCA.long)
head(v.PCA.long)

# How many forms need imputation?
toImpute<-subset(plyr::count(na.omit(v.PCA.long)$vowel.code),freq<9)
toImpute
nrow(toImpute)



# By-vowel data imputation:
pitch.imputed<-c()
bad.tokens<-c()
for (vtoken in unique(v.PCA.long$vowel.code)){
  
  vtoken.data<-subset(v.PCA.long,
                      vowel.code==vtoken)
  
  vtoken.pitch<-vtoken.data$pitch.Hz.zscore
  
  # Get number of NAs in the vector
  na.count<-sum(is.na(vtoken.pitch))
  
  # Imputation requires at least 2 non-NA values in the vector (or 3 for Kalman interpolation, which fails here)
  if ( na.count > (length(vtoken.pitch)-2) ){
    print(paste(vtoken,"won't work! All measurements are NA."))
    bad.tokens<-c(bad.tokens,vtoken)
    pitch.imputed<-c(pitch.imputed,rep(NA,length(vtoken.pitch)))
    
  } else {
    # https://cran.r-project.org/web/packages/imputeTS/readme/README.html
    
    vtoken.imputed<-na_interpolation(vtoken.pitch,
                                     option="stine") # You're using stine because its more conservative than spline,
                                                     # and generates fewer 'wobbly' interpolations at edges.
                                                     # But this mostly produces linear interpolations anyway.
                                                     # na_mean (mean replacement) and na_ma (moving average)
                                                     # seem to produce qualitatively similar results.
    
    pitch.imputed<-c(pitch.imputed,vtoken.imputed)
  }
}
summary(pitch.imputed)
nrow(v.PCA.long)
v.PCA.long$pitch.Hz.z.imputed<-pitch.imputed
nrow(v.PCA.long)


# Visualize imputations
# This isn't that useful as currently implemented because it only operates on single contours.
impdata<-subset(v.PCA.long,vowel.code=="S13-83.19")
plotNA.gapsize(impdata$pitch.Hz.zscore)
plotNA.imputations(impdata$pitch.Hz.zscore,impdata$pitch.Hz.z.imputed)

impdata<-subset(v.PCA.long,vowel.code=="S11-75.18")
plotNA.imputations(impdata$pitch.Hz.zscore,impdata$pitch.Hz.z.imputed)

impdata<-subset(v.PCA.long,vowel.code=="S13-23.18")
plotNA.imputations(impdata$pitch.Hz.zscore,impdata$pitch.Hz.z.imputed)

impdata<-subset(v.PCA.long,vowel.code=="S04-26.18")
plotNA.imputations(impdata$pitch.Hz.zscore,impdata$pitch.Hz.z.imputed)

impdata<-subset(v.PCA.long,vowel.code=="S01-40.15")
plotNA.imputations(impdata$pitch.Hz.zscore,impdata$pitch.Hz.z.imputed)

# Remove all-NA rows
v.PCA.long<-subset(v.PCA.long,!(vowel.code%in%bad.tokens))
# nrow(v.PCA.long)+18

attach(v.PCA.long)
  summary(pitch.Hz.zscore==pitch.Hz.z.imputed) # should return only TRUE and na
  sum(is.na(pitch.Hz.zscore))
  sum(is.na(pitch.Hz.z.imputed))
detach(v.PCA.long)





# Reduce dataframe size so things are easier to work with.
vdata.lean<-subset(v.PCA.long,
                   select = c(segment,
                              token.code,
                              interval.num,
                              step,
                              speaker,
                              pitch.Hz.z.imputed,
                              targ.wd,
                              act.wd,
                              disc.fnc,
                              position,
                              tone,
                              vlen,
                              stress,
                              v.pos.wd.adj,
                              accent
                   ))

head(vdata.lean)
tail(vdata.lean)

v.wide.PCA.imputed<-spread(vdata.lean,
                           step,
                           pitch.Hz.z.imputed)


head(v.wide.PCA.imputed)



# The input to PCA should be pairs of time points (steps) and pitch values.
# Why scale first? So that relative variance explained is interpretable. See:
# https://stats.stackexchange.com/questions/69157/why-do-we-need-to-normalize-data-before-principal-component-analysis-pca
v.PCA.imputed<-prcomp(v.wide.PCA.imputed[,stepnames],
                      scale=T)

# Most of the variance is explained by PC1, and almost all of it is explained by PC1+PC2
summary(v.PCA.imputed)
cumulative.var.exp<-summary(v.PCA.imputed)$importance[3,]
c.var.exp.df<-data.frame(cumulative.var.exp)
c.var.exp.df

c.var.exp<-gather(c.var.exp.df,
                  "PC",
                  "Cumulative variance explained"
)
c.var.exp.df
c.var.exp.df<-cbind(c.var.exp.df,"PC"=row.names(c.var.exp.df))
c.var.exp.df

# https://stackoverflow.com/questions/15043956/connecting-points
var.exp.plot<-ggplot(data=c.var.exp.df,
                     aes(x=PC,y=cumulative.var.exp,
                         group=1))+
  geom_point(size=5)+
  geom_line(lwd=2)+
  theme_bw(base_size=24)+
  ylab("Cumulative variance explained")+
  ggtitle("PCA over accented vowels, with data imputation (Stineman)")+
  scale_y_continuous(breaks=seq(0,1,0.05),limits = c(0.75,1))

var.exp.plot

output_file<-paste0(imageDir,"PCA/PCA_var_explained_imputation_stineman.pdf")
cairo_pdf(file=output_file,
          width=14,height=10)
  var.exp.plot
dev.off()



# For visualization, plot the principle components in
# the space defined by the original input dimensions (see Johnson 2008:100-2).

pcframe<-data.frame("sd"=v.PCA.imputed$sd) # Get standard deviations for each PC
pcframe$PC<-as.factor(paste0("PC",seq(1,length(v.PCA.imputed$sd),1)))

# Get variance explained
pcframe$Var.Exp<-paste0(round(
  summary(v.PCA.imputed)$importance[2,],
  3)*100,
  "%")
pcframe

centers<-data.frame(cbind("Pitch"=v.PCA.imputed$center,
                          "Step"=seq(1,length(v.PCA.imputed$center),1))) # Get centers (i.e. means) for the data
centers

loads<-data.frame(v.PCA.imputed$rotation) # Get loadings for each PC
loads<-cbind("Step"=seq(1,length(v.PCA.imputed$center),1),loads)
loads$centers<-centers$Pitch
loads


# We decide we only care about PCs 1-3, so we drop all others
pcframe<-droplevels(subset(pcframe,PC%in%c("PC1","PC2","PC3")))
pcframe
loads<-dplyr::select(loads,-c("PC4","PC5","PC6","PC7","PC8","PC9"))
loads


######
# Start plotting deviations

# Number of standard deviations for plotting:
stddev.set <- 1

# Create frame holding deviations
PCdeviations<-data.frame()
for (currPC in unique(pcframe$PC)){
  upper <- loads$centers + loads[,currPC] * (stddev.set*subset(pcframe,PC==currPC)$sd)
  upper.frame <- data.frame(cbind("PC"=currPC,
                                  "coords"=upper,
                                  "type"=rep("Positive",length(upper)),
                                  "Step"=loads$Step
  ))
  
  lower <- loads$centers - loads[,currPC] * (stddev.set*subset(pcframe,PC==currPC)$sd)
  lower.frame <- data.frame(cbind("PC"=currPC,
                                  "coords"=lower,
                                  "type"=rep("Negative",length(lower)),
                                  "Step"=loads$Step
  ))
  
  PCdeviations<- rbind(PCdeviations,upper.frame,lower.frame)
}
PCdeviations
PCdeviations$coords<-as.numeric(as.character(PCdeviations$coords))
PCdeviations$Step<-as.numeric(as.character(PCdeviations$Step))
summary(PCdeviations$coords)


# Start plotting
pc.imputed<-ggplot(data=centers,
                   aes(x=Step,
                       y=Pitch))+
  geom_point(size=4,color="grey")+
  geom_line(lwd=2,color="grey")+
  theme_bw(base_size=42)+
  theme(strip.text = element_text(face = "bold",size=32),
        legend.key.width=unit(4,"line"),
        legend.text = element_text(face = "bold",size=32),
        legend.title = element_text(face = "bold",size=32))+
  geom_label(inherit.aes = F,
             data=pcframe,
             aes(x=7.5,y=1.15,
                 label=Var.Exp),
             size=10)+
  #ggtitle("Dimensions of variation of each PC")+
  #labs(subtitle="PCA over accented vowels, with data imputation (Stineman)")+
  ylab(paste0("Deviations (\u00B1",stddev.set,"\u03C3)"))+
  xlab("Time (normalized)")+
  # Deviations
  geom_point(data=PCdeviations,
             inherit.aes = F,
             aes(y=coords,
                 x=Step,
                 group=type,
                 color=type),
             size=4)+
  geom_line(data=PCdeviations,
            inherit.aes = F,
            aes(y=coords,
                x=Step,
                group=type,
                color=type),
            lwd=2)+
  theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(1,9,1))+
  facet_grid(.~PC)+
  scale_color_manual(values=cbPalette[3:length(cbPalette)],name="Direction")

pc.imputed

output_file<-paste0(imageDir,"PCA/PCA_deviations_imputation_stineman.pdf")
cairo_pdf(file=output_file,
          width=16,height=6)
  pc.imputed
dev.off()



###############
# NB: you tried recomputing the PCA over the raw, non-normalized data (with imputation)
# and basically all the variance gets assigned to PC1, since, sensibly, most
# of the variance in a non-normalized data set will probably reflect inter-speaker
# differences in pitch range along with inter-token differences in pitch height.
#
# Interestingly, the *qualitative shape* of the PCs remains basically unchanged. 
#
# See:
# PCA_deviations_RAWINPUT.pdf
# PCA_var_explained_RAWINPUT.pdf




###############
# Now analyze the results of the PCA, their relationship with data variables, etc.

# Add PC information for each observation back into the original data frame.
# To capture the most data, we use the PCA with spline-based NA imputation
summary(v.PCA.imputed) # The original PCA object
summary(v.PCA.imputed$x) # The by-observation PC values
nrow(v.PCA.imputed$x)

summary(v.wide.PCA.imputed) # The input to PCA, in wide form.
nrow(v.wide.PCA.imputed)

obs.coding <- dplyr::select(v.wide.PCA.imputed,-stepnames) # Drop the input to PCA
summary(obs.coding)

v.PCA.out <- cbind(obs.coding,v.PCA.imputed$x)
summary(v.PCA.out)
v.PCA.out<-dplyr::select(v.PCA.out, -c("PC4","PC5","PC6","PC7","PC8","PC9")) # Only the first 3 PCs are interesting
summary(v.PCA.out)

# Recode the data in long format
v.PCA.out<-gather(v.PCA.out,
                  key="PC",
                  value="PC.score",
                  c("PC1","PC2","PC3"))


summary(v.PCA.out)
v.PCA.out$PC<-factor(v.PCA.out$PC)
v.PCA.out<-droplevels(v.PCA.out)
summary(v.PCA.out)


############
# Explore tone
# Some broad trends here, but not much to see without factoring out other things, like focus and position.
ggplot(data=v.PCA.out,
       aes(x=accent,
           y=PC.score,
           fill=accent),
       color="black",
       alpha=0.5)+
  geom_violin()+
  theme_bw(base_size=24)+
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32))+
  facet_grid(PC~vlen)+
  scale_fill_manual(values=cbPalette)+
  scale_y_reverse() # Sometimes needed to help make PCs more interpretable in terms of Hz


# Seems that tonal vowels tend to have slightly higher, slightly more falling.
# We should run this same analysis on our wordlist data.


################
# Use combined PC1+PC2 values to see how different categories typically vary across these two parameters.
# It's not going to be all that informative, though, because there is a *lot* of variance even across just
# these two PCs.

sds<-v.PCA.imputed$sd # Get standard deviations for each PC

centers<-data.frame(cbind("Pitch"=v.PCA.imputed$center,
                          "Step"=seq(1,length(v.PCA.imputed$center),1))) # Get centers (i.e. means) for the data
centers

loads<-data.frame(v.PCA.imputed$rotation) # Get loadings for each PC
loads<-cbind("Step"=seq(1,length(v.PCA.imputed$center),1),loads)
loads$centers<-centers$Pitch
loads


# We decide we only care about PCs 1-2, so we drop all others
pcframe<-droplevels(subset(pcframe,PC%in%c("PC1","PC2","PC3")))
pcframe
loads<-dplyr::select(loads,-c("PC4","PC5","PC6","PC7","PC8","PC9"))
loads


# See how shapes typically differ for tonal vs. stressed vowels
colnames(v.PCA.out)
v.PCA.1.2.3<-v.PCA.out
#v.PCA.1.2<-subset(v.PCA.out,
#                  PC!=("PC3"))

PC.summaries<-ddply(v.PCA.1.2.3,
                    .(PC,accent,vlen,disc.fnc,position),
                    summarize,
                    mean.PCscore = mean(PC.score),
                    sd.PCscore = sd(PC.score)
)


PC.summaries
PC.summaries<-droplevels(PC.summaries)

stepframe<-data.frame()
for (num in seq(1,9,1)){
  currFrame<-PC.summaries
  
  currFrame$step<-rep(num,nrow(currFrame))
  stepframe<-rbind(stepframe,currFrame)
  
}
head(stepframe)
summary(stepframe)


stepframe$PC.loaded<-rep(NA,nrow(stepframe))
stepframe$PC.typical<-rep(NA,nrow(stepframe))
for (rw in 1:nrow(stepframe)){
  datasubset<-stepframe[rw,]
  stp<-datasubset$step
  pc<-as.character(datasubset$PC)
  
  pcloading <- as.numeric(subset(loads,Step==stp)[,pc])
  loaded.val <- pcloading*datasubset$mean.PCscore
  
  stepframe[rw,]$PC.loaded<-loaded.val
  
  centerval<-subset(centers,Step==stp)$Pitch
  stepframe[rw,]$PC.typical<-loaded.val+centerval
}

head(stepframe)

pc1data<-subset(stepframe,PC=="PC1")
pc1data<-dplyr::select(pc1data,-PC)
attach(pc1data)
  pc1data<-pc1data[order(accent,vlen,disc.fnc,position,step),]
detach(pc1data)

pc2data<-subset(stepframe,PC=="PC2")
pc2data<-dplyr::select(pc2data,-PC)
attach(pc2data)
  pc2data<-pc2data[order(accent,vlen,disc.fnc,position,step),]
detach(pc2data)

pc3data<-subset(stepframe,PC=="PC3")
pc3data<-dplyr::select(pc3data,-PC)
attach(pc3data)
  pc3data<-pc3data[order(accent,vlen,disc.fnc,position,step),]
detach(pc3data)

head(pc1data)
head(pc2data)
head(pc3data)

# Create combination PC1+PC2 value
pcmerged<-pc1data
pcmerged$PC.typical<-pc1data$PC.typical+pc2data$PC.typical+pc3data$PC.typical
# Not 100% sure this is the right way to pool variance, but...
# http://www.khanacademy.org/math/ap-statistics/random-variables-ap/combining-random-variables/a/combining-random-variables-article
# http://apcentral.collegeboard.org/courses/ap-statistics/classroom-resources/why-variances-add-and-why-it-matters#anchor2
pcmerged$PC.typical.sd<-sqrt(pc1data$sd.PCscore^2+pc2data$sd.PCscore^2+pc3data$sd.PCscore^2)

head(pcmerged)



######################
# To COMBINE PC1+PC2+...
# loads$centers + loads$PC1 * mean.PCscore + loads$PC2 * mean.PCscore ...

# Start plotting
currdata <- subset(pcmerged,vlen=="V")
pc.typical<-ggplot(data=currdata,
                   aes(x=step,
                       y=PC.typical,
                       color=accent,
                       group=accent))+
  
  # I think this is still wrong, the deviations don't seem centered around the category means...
  # geom_ribbon(aes(x=step,
  #   ymax=0.25*(PC.typical+PC.typical.sd),
  #   ymin=0.25*(PC.typical-PC.typical.sd),
  #   fill=accent),
  #   alpha=0.15
  # )+ # Add standard deviations
  geom_point(size=4)+
  geom_line(lwd=2)+
  theme_bw(base_size=24)+
  theme(strip.text = element_text(face = "bold",size=32),
        legend.key.width=unit(4,"line"),
        legend.text = element_text(face = "bold",size=32),
        legend.title = element_text(face = "bold",size=32))+
  ggtitle("Typical deviations for tonal vs. non-tonal short vowels")+
  labs(subtitle="(Mean PC1 + mean PC2 + mean PC3  for each condition)")+
  # ylab("Hz (z-scored; bands = \u00B10.25\u03C3)")+
  ylab("Hz (z-scored)")+
  
  # Add center -- we remove these because declination makes them seem a bit misleading.
  geom_point(data=centers,
             inherit.aes = F,
             aes(y=Pitch,
                 x=Step),
             color="grey",
             size=4)+
  geom_line(data=centers,
            inherit.aes = F,
            aes(y=Pitch,
                x=Step),
            color="grey",
            lwd=2)+
  scale_x_continuous(breaks=seq(1,9,1))+
  facet_grid(position~disc.fnc)+
  scale_color_manual(values=cbPalette)+
  # scale_fill_manual(values=cbPalette)+
  coord_cartesian(ylim=c(-2,2))+
  # labs(color = "Accent type",fill = "Accent type")
  labs(color = "Accent type")

pc.typical

output_file<-paste0(imageDir,"PCA/PCA_typical_shortV.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  pc.typical
dev.off()



# Start plotting
currdata <- subset(pcmerged,vlen=="V\u02D0")
pc.typical<-ggplot(data=currdata,
                   aes(x=step,
                       y=PC.typical,
                       color=accent,
                       group=accent))+
  # I think this is still wrong, the deviations don't seem centered around the category means...
  # geom_ribbon(data=currdata,aes(
  #   ymax=0.25*(PC.typical+sd.PCscore),
  #   ymin=0.25*(PC.typical-sd.PCscore),
  #   fill=accent),
  #   alpha=0.15
  # )+ # Add standard deviations
  geom_point(size=4)+
  geom_line(lwd=2)+
  theme_bw(base_size=24)+
  theme(strip.text = element_text(face = "bold",size=32),
        legend.key.width=unit(4,"line"),
        legend.text = element_text(face = "bold",size=32),
        legend.title = element_text(face = "bold",size=32))+
  ggtitle("Typical deviations for tonal vs. non-tonal long vowels")+
  labs(subtitle="(Mean PC1 + mean PC2 + mean PC3 for each condition)")+
  # ylab("Hz (z-scored; bands = \u00B10.25\u03C3)")+
  ylab("Hz (z-scored)")+
  
  # Add center -- we remove these because declination makes them seem a bit misleading.
  geom_point(data=centers,
             inherit.aes = F,
             aes(y=Pitch,
                 x=Step),
             color="grey",
             size=4)+
  geom_line(data=centers,
            inherit.aes = F,
            aes(y=Pitch,
                x=Step),
            color="grey",
            lwd=2)+
  # theme(legend.position = "none")+
  scale_x_continuous(breaks=seq(1,9,1))+
  facet_grid(position~disc.fnc)+
  scale_color_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  coord_cartesian(ylim=c(-2,2))+
  labs(color = "Accent type")

pc.typical

output_file<-paste0(imageDir,"PCA/PCA_typical_longV.pdf")
cairo_pdf(file=output_file,
          width=16,height=10)
  pc.typical
dev.off()



#####################
# k-means exploration
#####################

# Get the full output of PCA again
form.kmeans.pca <- v.PCA.imputed
summary(form.kmeans.pca)

form.kmeans<-form.kmeans.pca$x
form.kmeans<-form.kmeans[,2:9] # Drop PC1 if you don't really care about the overall level

# Check correlations
cor(form.kmeans)
options(scipen=999)
max(apply(cor(form.kmeans),1,function(x) max(x[x!=1]))) # Find largest non-1 correlation -- PC correlations are low, as expected.
options(scipen=0)

maxgroups<-15

kmeansABIC = function(fit){
  
  m = ncol(fit$centers)
  n = length(fit$cluster)
  k = nrow(fit$centers)
  D = fit$tot.withinss
  return(data.frame(AIC = D + 2*m*k,
                    BIC = D + log(n)*m*k))
}

frm.holder<-matrix(ncol=4,nrow=maxgroups)
variance.frame<-data.frame(frm.holder)
colnames(variance.frame)<-c("Groups","Variance","AIC","BIC")
rm(frm.holder)

grouping.data<-form.kmeans

for (k in (1:maxgroups)){
  
  kmodel<-kmeans(grouping.data,centers=k)
  kvar<-kmodel$betweenss/kmodel$totss
  
  # Calculate AIC and BIC for each k-means model.
  aic<-as.numeric(kmeansABIC(kmodel)["AIC"])
  bic<-as.numeric(kmeansABIC(kmodel)["BIC"])
  
  variance.frame[k,]<-c(k,round(kvar,5),aic,bic)
  
}


head(variance.frame)
summary(variance.frame)

elbow.plot<-qplot(data=variance.frame,
                  x=Groups,
                  y=Variance,
                  size=I(9)
  )+
  geom_line()+
  scale_x_continuous(breaks=seq(1,maxgroups,1))+
  scale_y_continuous(breaks=seq(0,1,0.1),labels=scales::percent)+
  ylab("Variance")+
  xlab("Groups")+
  theme_bw(base_size=32)
  #geom_point(aes(x=4, y=variance.frame[4,"Variance"]), size=60, shape="\u25E6", color="red",lwd=40)

elbow.plot


# Now we inspect AIC/BIC scores.
# We're still looking for elbows
#
# "In general you want to choose AIC and BIC to be closest to negative infinity."
# "For non-tiny data sets, BIC more harshly penalizes free parameters"
# https://mailman.ucsd.edu/pipermail/ling-r-lang-l/2011-August/000282.html
#
aic.plot<-qplot(data=variance.frame,
                x=Groups,
                y=AIC,
                # log(AIC) can be easier to read.
                #y=log(AIC),
                size=I(5))+
  geom_line()+
  scale_y_reverse()+
  scale_x_continuous(breaks=seq(1,maxgroups,1))

aic.plot


bic.plot<-qplot(data=variance.frame,
                x=Groups,
                y=BIC,
                # log(BIC) can be easier to read.
                #y=log(BIC),
                size=I(9))+
  geom_line()+
  scale_x_continuous(breaks=seq(1,maxgroups,1))+
  scale_y_reverse()+
  xlab("Groups")+
  ylab("BIC")+
  theme_bw(base_size=32)
  # geom_point(aes(x=6, y=variance.frame[6,"BIC"]), size=60, shape="\u25E6", color="red",lwd=40)

bic.plot


# Manually double-check location of local AIC/BIC minima (turning points):
#
for (val in (2:nrow(variance.frame))){
  if (variance.frame[val,"AIC"] > variance.frame[val-1,"AIC"]){
    cat(val,"groups has an AIC bump!","\n")
  }
  if (variance.frame[val,"BIC"] > variance.frame[val-1,"BIC"]){
    cat(val,"groups has a BIC bump!","\n")
  }
}

# Check global minima
#
which(variance.frame$AIC==min(variance.frame$AIC))
which(variance.frame$BIC==min(variance.frame$BIC))


# Plot canonical shapes for each group
k3<-kmeans(form.kmeans,centers=3)
k4<-kmeans(form.kmeans,centers=4)
k5<-kmeans(form.kmeans,centers=5)

optimal.k<-k4

optimal.k$centers
optimal.k$size

kmeans.out.groups<-data.frame(optimal.k$cluster)

# Add groups to original data frame
nrow(kmeans.out.groups)
nrow(v.wide.PCA.imputed)
nrow(v.PCA.imputed$x)

v.wide.PCA.imputed$kgroup<-as.factor(kmeans.out.groups[,1]) # Add groups to the dataframe with the original Hz values
head(v.wide.PCA.imputed)

v.PCA.imputed.long<-gather(v.wide.PCA.imputed,
                                key = "step",
                                value = "pitch.Hz.zscore",
                                stepnames
                                )

v.PCA.imputed.long$step<-as.factor(v.PCA.imputed.long$step)
# sapply(v.PCA.imputed.long,is)

v.PCA.unique<-dplyr::select(v.wide.PCA.imputed,-c(all_of(step.list)))

attach(v.PCA.imputed.long)
  v.PCA.imputed.long<-v.PCA.imputed.long[order(token.code,interval.num,step),]
detach(v.PCA.imputed.long)
head(v.PCA.imputed.long)


# What's the basic distribution of k-means groups?
ggplot(data=v.PCA.unique,aes(fill=kgroup,x=kgroup))+
      geom_bar()+
      scale_fill_manual(values=cbBW,guide=F)





####################
# Plot contours by group assignments

# Revalue group names if you'd like 
v.PCA.imputed.long$kgroup<-revalue(v.PCA.imputed.long$kgroup,c("4"="Rise","3"="Flat","1"="Slight fall", "2"="Fall"))
v.PCA.unique$kgroup<-revalue(v.PCA.unique$kgroup,c("4"="Rise","3"="Flat","1"="Slight fall", "2"="Fall"))

v.PCA.imputed.long$kgroup<-factor(v.PCA.imputed.long$kgroup,
                        levels=c("Flat","Rise","Slight fall","Fall"))
v.PCA.unique$kgroup<-factor(v.PCA.unique$kgroup,
                                  levels=c("Flat","Rise","Slight fall","Fall"))


v.PCA.unique$accent<-revalue(v.PCA.unique$accent,c("Stressed"="Non-tonal"))
v.PCA.unique$accent<-factor(v.PCA.unique$accent,
                            levels=c("Non-tonal","Tonal"))


kmeans.contour.plot<-ggplot(data=v.PCA.imputed.long,
       aes(x=step,
           y=pitch.Hz.zscore,
           color=kgroup,
           linetype=kgroup))+
  
  # Add reference line at zero.
  geom_hline(yintercept=0,lwd=1.25,lty="dashed",color="darkgrey")+
  
  # Add means over data set  
  # geom_line(data=subset(meanPitch.df,
  #                       vlen=="short"),
  #           aes(x=step,y=meanHz),
  #           inherit.aes=F,
  #           lwd=3,color=lightBlue,alpha=0.5)+
  
  geom_smooth(aes(group=kgroup),method="loess",lwd=3)+
  
  scale_color_manual(values=cbBW
                     #,guide = "none"
  )+
  scale_linetype_manual(values=c("solid","dashed","dotted","dotdash","twodash")
                        #,guide = "none"
  )+
  
  theme_bw(base_size = 48)+
  theme(text=element_text(size=42),
        legend.key.width=unit(10,"line"),
        plot.title = element_text(size = 32)
        #axis.title = element_text(size=32)
        
        # Use these options if you want a more B&W print-friendly plot.
        #panel.background = element_rect(fill="white"),
        #panel.grid.major = element_line(color = "#80808033",linetype="dashed"),
        #panel.grid.minor = element_line(color = "#80808033",linetype="dotted")
  )+
  
  # coord_cartesian(ylim=c(-2,2))+
  coord_cartesian(ylim=c(-1.5,1.5))+
  
  scale_x_discrete("Time (normalized)", breaks=step.list, labels=waiver())+
  scale_y_continuous("Pitch (z-scores over Hz)", breaks=seq(-2,2,0.5), labels=waiver())+
  labs(linetype = "k-means group")+
  labs(color = "k-means group")

kmeans.contour.plot

# 
# output_file<-paste0(imageDir,"PCA/PCA_kmeans_contours.pdf")
# cairo_pdf(file=output_file,
#           width=16,height=10)
#   kmeans.contour.plot
# dev.off()
# 

# Counts
# https://sebastiansauer.github.io/percentage_plot_ggplot2_V2/
kmeans.contour.barplots<-ggplot(data=v.PCA.unique,aes(fill=kgroup,x=kgroup,group=accent),color="black")+
  # geom_bar(alpha=0.75)+
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count",alpha=0.75) + 
  scale_fill_manual(values=cbBW,guide=F)+
  theme_bw(base_size = 42)+
  theme(text=element_text(size=42),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=44),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
        
        # Use these options if you want a more B&W print-friendly plot.
        #panel.background = element_rect(fill="white"),
        #panel.grid.major = element_line(color = "#80808033",linetype="dashed"),
        #panel.grid.minor = element_line(color = "#80808033",linetype="dotted")
  )+
  theme(strip.text.y = element_text(angle=0))+
  facet_grid(vlen~accent)+
  # xlab("k-means group")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  

kmeans.contour.barplots

# output_file<-paste0(imageDir,"PCA/PCA_kmeans_group_counts.pdf")
# cairo_pdf(file=output_file,
#           width=16,height=10)
#   kmeans.contour.barplots
# dev.off()
# 


#######    
# Use  multinomial regression to explore k-groups.

# Make sure that you're happy with the levels of 'item'
v.PCA.unique$targ.wd<-revalue(v.PCA.unique$targ.wd,
                            c("jcháaj"="cháaj",
                              "jch7úuk7"="ch7úuk7",
                              "jkaach7"="kaach7",
                              "jkinaq7"="kinaq7",
                              "qálq"="qálaq",
                              "rixóql"="rixóqil",
                              "jóoq7"="qajóoq7",
                              "xoot"="qaxoot",
                              "jtélb7"="téleb7",
                              "jtéleb7"="téleb7",
                              "jxajab7"="xajab7",
                              "jxájab7"="xájab7"
                            ))


summary(v.PCA.unique$targ.wd)
length(unique(v.PCA.unique$targ.wd))

# You can't estimate item-level random effects for items that only occur once in the data.
v.PCA.unique<-subset(v.PCA.unique,targ.wd!="ch7aat")
v.PCA.unique<-droplevels(v.PCA.unique)
summary(v.PCA.unique$targ.wd)

# Multinomial mixed-effects regression isn't really appropriate for glmer, so we use the brms package instead,
# which has familiar lme4-type syntax. This is time-intensive so it's commented out until we really explicitly want to re-run it.
# https://github.com/paul-buerkner/brms
# https://thinkinator.com/2016/01/12/r-users-will-now-inevitably-become-bayesians/
# https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/counting-and-classification.html#other-count-regressions
# https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html
# https://github.com/paul-buerkner/brms/tree/master/vignettes
# library(brms)
# brm.m<-brm(data=v.PCA.unique,
#     formula=kgroup~accent*vlen+(1|speaker)+(1|targ.wd),
#     family = categorical())
# 
# brm.m






####################################
# Duration analysis
####################################

###############
# Double check that there aren't any duration x accent type correlations that might muck up your use of time-normalized contours.

head(stressed.unique)
summary(stressed.unique$seg.dur)

stressed.unique$vlen<-revalue(stressed.unique$vlen,c("Short" = "V", "Long" = "V\u2D0"))
stressed.unique$vlen<-factor(stressed.unique$vlen,levels=c("V","V\u2D0"))

###################
# Get unique segment durations and plot.
tone.dur.plot<-ggplot(data=stressed.unique)+
  geom_density(aes(x=seg.dur,
                   fill=tone),
               alpha=0.45,color="black",lwd=1.25)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(size=28,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(6,"line"),
        legend.text = element_text(size=28,face="bold"),
        legend.title = element_text(size=28,face="bold"))+
  xlab("Accented vowel duration (ms)")+
  facet_grid(.~vlen)+
  scale_fill_manual(values=cbPalette[c(2,1)])+
  ylab("Density")+
  labs(fill = "Tone")
  

#geom_vline(xintercept = density(stressed.unique$seg.dur)$x[which.max(density(stressed.unique$seg.dur)$y)])

tone.dur.plot

# Let's see if/how this varies across speakers, assuming there's enough data to estimate this.
# If there isn't enough data, we can check it by demographic...
# tone.dur.plot+facet_grid(.~speaker)
# tone.dur.plot+facet_grid(vlen~speaker)
# It does seem, impressionistically, to be more pronounced for some speakers than others...


# Hmm, it's true that tonal vowels are somewhat longer. But is that significant? Apparently so.
# Run some t-tests and plot results on duration density plots.
short.t<-t.test(subset(stressed.unique.v,tone=="Tonal")$seg.dur,
                subset(stressed.unique.v,tone=="Non-tonal")$seg.dur
)

short.t
short.p<-round(short.t$p.value,4)

long.t<-t.test(subset(stressed.unique.vv,tone=="Tonal")$seg.dur,
               subset(stressed.unique.vv,tone=="Non-tonal")$seg.dur
)

long.t
long.p<-round(long.t$p.value,4)

# Pull out p-values for plotting as text
p.frame<-data.frame(cbind("vlen"=levels(stressed.unique$vlen),
                          "p.val"=c(short.p,long.p))
)
p.frame

p.frame$p.val<-format(p.frame$p.val, scientific = FALSE)
p.frame[p.frame$p.val>0.05,]$p.val<-"n.s."
p.frame[p.frame$p.val<0.001,]$p.val<-"p\u003C.001"
#p.frame[p.frame$p.val<0.01,]$p.val<-"p\u003C.01"
#p.frame[p.frame$p.val<0.05,]$p.val<-"p\u003C.05"# Nothing meets this condition.

p.frame



# Get peaks in density
# https://stackoverflow.com/questions/23641882/showing-major-peaks-in-densities-across-facets-using-r
densMode <- function(x){
  td <- density(x)
  maxDens <- which.max(td$y)
  list(x=td$x[maxDens], y=td$y[maxDens])
}
xdat <- ddply(stressed.unique,c("vlen","tone"), transform,
              durmean = signif(densMode(seg.dur)$x,3),
              med.x = signif(densMode(seg.dur)$x,3),
              med.y = signif(densMode(seg.dur)$y,3))



# Add p-values to the plot.
tone.dur.plot.pval<-tone.dur.plot+geom_label(inherit.aes = F,
                                             data=p.frame,
                                             aes(x=max(stressed.unique$seg.dur)-40,
                                                 # x=215,
                                                 #y=0.014,
                                                 y=max(xdat$med.y)-0.001,
                                                 label=p.val),
                                             size=10)


tone.dur.plot.pval

output_file<-paste0(imageDir,"Non-tonal_measures/Usp_QA_V_dur_by_tone.pdf")
  cairo_pdf(file=output_file,
            width=14,height=6)
  tone.dur.plot.pval
dev.off()



# By accent
QA.v.unique$vlen<-revalue(QA.v.unique$vlen,c("Short" = "V", "Long" = "V\u2D0"))
QA.v.unique$vlen<-factor(QA.v.unique$vlen,levels=c("V","V\u2D0"))

tone.dur.plot<-ggplot(data=QA.v.unique)+
  geom_density(aes(x=seg.dur,
                   fill=accent,
                   lty=accent),
               alpha=0.3,color="black",lwd=2.5)+
  theme_bw(base_size=42)+
  theme(strip.text = element_text(face = "bold",family = "Doulos SIL",size=48),
        legend.key.width=unit(8,"line"),
        legend.key.height=unit(4,"line"),
        legend.title = element_text(face="bold")
        )+
  xlab("Accented vowel duration (ms)")+
  facet_grid(.~vlen)+
  scale_fill_manual(values=cbPalette[c(2,1,3)])+
  ylab("Density")+
  labs(fill = "Accent",lty="Accent")+
  scale_linetype_manual(values=c("solid","dashed","dotted")
                        #,guide = "none"
  )
  
tone.dur.plot

output_file<-paste0(imageDir,"Non-tonal_measures/Usp_QA_V_dur_by_accent.pdf")
cairo_pdf(file=output_file,
          width=18,height=6)
  tone.dur.plot
dev.off()


##########
# Plot durations for long and short vowels across contexts.

#######
# Set plotting parameters
give.n <- function(x,ypos=0){
  #  return(c(y = 40, label = length(x)))
  #
  # Setting the value of y here affects the vertical positioning of the labels.
  data.frame(y = ypos, label = paste0("n=",length(x)))
}


# Get p-values for duration by discourse function
library(broom)
mean.dur.test<-stressed.unique %>% group_by(vlen) %>% do(tidy(t.test(seg.dur~disc.fnc, data=.,alternative="less")))
mean.dur.test
detach(package:broom)


mean.dur.test$p.char<-format(mean.dur.test$p.value, scientific = FALSE) # Don't use scientific notation
# mean.dur.test[mean.dur.test$p.char>0.05,]$p.char<-"n.s." # Nothing meets this condition.
mean.dur.test[mean.dur.test$p.char<0.001,]$p.char<-"p\u003C.001"
# mean.dur.test[mean.dur.test$p.char<0.01,]$p.char<-"p\u003C.01" # Nothing meets this condition.
# mean.dur.test[mean.dur.test$p.char<0.05,]$p.char<-"p\u003C.05" # Nothing meets this condition.
mean.dur.test



dur.plot<-ggplot(data=stressed.unique,
                    aes(x=disc.fnc,
                        y=seg.dur),
                        color="black")+
  geom_violin(alpha=0.6,
              aes(fill=disc.fnc))+
  theme_bw(base_size = 48)+
  theme(text=element_text(size=42),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=44),
        legend.key.height = unit(3, "lines"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
  )+
  scale_fill_manual(values=cbPalette[c(5,6)])+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = NA,fill="white")+
  stat_summary(fun="median",geom='point',size=6,pch=18)+
  stat_summary(fun.data = give.n, geom = "text", size=10,
               col="black",fontface="bold")+
  stat_summary(fun="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  
  
  ylab("Vowel duration (ms)")+
  xlab("Discourse condition")+
  scale_y_continuous(breaks=seq(0,400,40),limits=c(0,280))+
  theme(legend.position = "none")+
  facet_grid(.~vlen)
  # ggtitle("Stressed vowel durations by length and discourse condition")

dur.plot

# Add p-values to the plot.
dur.plot<-dur.plot+geom_label(inherit.aes = F,
                              data=subset(mean.dur.test),
                              aes(x=1.5,y=260,label=p.char),size=10)
  
dur.plot



output_file<-paste0(imageDir,"Non-tonal_measures/Usp_QA_V_dur_by_disc.pdf")
cairo_pdf(file=output_file,
          width=14,height=8)
  dur.plot
dev.off()



# Get p-values for duration by position
library(broom)
mean.dur.test<-stressed.unique %>% group_by(vlen) %>% do(tidy(t.test(seg.dur~position, data=.,alternative="less")))
mean.dur.test
detach(package:broom)


mean.dur.test$p.char<-format(mean.dur.test$p.value, scientific = FALSE) # Don't use scientific notation
mean.dur.test[mean.dur.test$p.char>0.05,]$p.char<-"n.s." # Nothing meets this condition.
# mean.dur.test[mean.dur.test$p.char<0.001,]$p.char<-"p\u003C.001"
# mean.dur.test[mean.dur.test$p.char<0.01,]$p.char<-"p\u003C.01" # Nothing meets this condition.
# mean.dur.test[mean.dur.test$p.char<0.05,]$p.char<-"p\u003C.05" # Nothing meets this condition.
mean.dur.test



dur.plot<-ggplot(data=stressed.unique,
                 aes(x=position,
                     y=seg.dur),
                 color="black")+
  geom_violin(alpha=0.6,
              aes(fill=position))+
  theme_bw(base_size = 48)+
  theme(text=element_text(size=42),
        legend.key.width=unit(8,"line"),
        strip.text = element_text(face = "bold",family = "Doulos SIL",size=44),
        legend.key.height = unit(3, "lines"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32)
  )+
  scale_fill_manual(values=cbPalette[c(7,8)])+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = NA,fill="white")+
  stat_summary(fun="median",geom='point',size=6,pch=18)+
  stat_summary(fun.data = give.n, geom = "text", size=10,
               col="black",fontface="bold")+
  stat_summary(fun="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  
  
  ylab("Vowel duration (ms)")+
  xlab("Sentential position")+
  scale_y_continuous(breaks=seq(0,400,40),limits=c(0,280))+
  theme(legend.position = "none")+
  facet_grid(.~vlen)
# ggtitle("Stressed vowel durations by length and discourse condition")

dur.plot

# Add p-values to the plot.
dur.plot<-dur.plot+geom_label(inherit.aes = F,
                              data=subset(mean.dur.test),
                              aes(x=1.5,y=260,label=p.char),size=10)

dur.plot



output_file<-paste0(imageDir,"Non-tonal_measures/Usp_QA_V_dur_by_pos.pdf")
cairo_pdf(file=output_file,
          width=14,height=8)
  dur.plot
dev.off()



#######
# Some light cleaning...
rm(short.t,short.p,long.t,long.p,p.frame,tone.dur.plot)


############
# lmer analysis of duration

# We analyze unstressed and stressed vowels

# Add some coding for vowel quality and height
QA.v.unique$v.qual<-QA.v.unique$segment
QA.v.unique$v.qual<-gsub(QA.v.unique$v.qual, # Use regular expressions to strip out coding
                        pattern="T|S|L|1|0",
                        replacement="")
QA.v.unique$v.qual<-as.factor(QA.v.unique$v.qual)
summary(QA.v.unique$v.qual)


full.m<-lmer(data=QA.v.unique,
             # Use REML=F for log-likelihood comparison in step-down model reduction.
             # If you use lmerTest for doing step-down model reduction, you want to use REML=T:
             # https://link.springer.com/article/10.3758/s13428-016-0809-y
             REML=F,
             #
             # Increase the number of iterations to convergence.
             # https://stat.ethz.ch/pipermail/r-sig-mixed-models/2014q2/022084.html
             control=lmerControl(optCtrl=list(maxfun=1e5),optimizer = "bobyqa"),
             #
             # Dependent variable:
             seg.dur~
               #
               # Fixed effects: all required by theoretical interest, and allowed by convergence/factor crossing
               tone*vlen+
               stress+
               #v.pos.wd.adj+
               v.qual+
               # Random effects
               # Simple random intercepts
               (1|targ.wd)+
               (1|speaker)
             # By speaker random slopes, correlated with by-speaker random intercepts
             # The model fails to converge when these are included -- probably not enough per-speaker data.
             # (1+tone|speaker)
             # (1+start.time|speaker)
)


# Collinearity
max(vif.mer(full.m))
kappa.mer(full.m)

# Model
summary(full.m)
anova(full.m)

drop1(full.m)[order(drop1(full.m)[6]),]
m1<-update(full.m,.~.-tone:vlen)
anova(full.m,m1)
 
anova(m1)[order(anova(m1)[5]),]
drop1(m1)[order(drop1(m1)[6]),]
m2<-update(m1,.~.-tone)
anova(full.m,m1,m2)

final.m<-m1

# Collinearity
max(vif.mer(final.m))
kappa.mer(final.m) # Low collinearity

# Model
summary(final.m)
anova(final.m)

anova(final.m)[order(anova(final.m)[5],decreasing=T),] # Shorthand for sorting anova by F-value (decreasing) (use 4 if lmerTest NOT loaded)
options(scipen=999)
coef(summary(final.m))[order(abs(coef(summary(final.m))[,1]),decreasing=T),]
options(scipen=0)


# Check r2
rsquared(final.m)

# Check normality of residuals --- not too bad!
cor(qqnorm(residuals(final.m))$x,
    residuals(final.m))
qqnorm(residuals(final.m)) # Not ideal, but not too bad.
qqline(residuals(final.m))

summary(residuals(final.m))
ggplot(data=data.frame("resid"=residuals(final.m)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)
