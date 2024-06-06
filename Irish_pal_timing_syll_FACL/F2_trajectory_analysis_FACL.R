######################################
# TO DO --- SPEED UP WITH .rdata FILES
######################################


######################################
# Packages
######################################
library(tidyverse)
library(stringr)


######################################
# Set working directory
######################################
computer = ""
setwd(paste0("C:/Users/",computer,""))


######################################
# Choose a colorblind friendly palette
######################################
colorSetCB=palette.colors(n = 8,palette = "R4")
colorSet<-colorSetCB[c(1,2,4,3,8,6)]

showColor<-function(pal){
  
  hist(seq(0,length(pal)),col=pal,breaks=length(pal))
  
}

# Check color schemes if desired.
# showColor(colorSetCB)
# showColor(colorSet)


######################################
# Read in data
######################################

# Formant measurements produced by a handmade script:
# formants.homebrewed <- read.csv("FACL_F2_tracks_all_spkrs.txt",sep="\t")


######################################
# Compare FastTrak data to the hand-extracted formant data, and merge as desired.
######################################
# This is time-intensive, so unless you want to actually work with the original CSV files for some reason, just load this:
load("FastTrack_formant_data.Rdata")

############################################################################
# Note that the dataframe which is loaded here (<formants>) includes the log-additive regression
# normalized formant values as well.

# library(vroom)
# formant.csv.files <- fs::dir_ls(path = "csvs/",glob="*.csv")
# # formant.csv.files
# csvs<-vroom(formant.csv.files,id="filename",col_select=c(time,f1,f2,f3))
# 
# # Add normalized time
# csvs <- csvs %>% group_by(filename) %>% mutate(step = ((time-min(time))/(max(time)-min(time))*100))
# 
# # Pivot longer
# csvs <- csvs %>% pivot_longer(cols=c(f1,f2,f3),names_to = "formant",values_to="freq")
# csvs <- csvs %>% mutate(formant = toupper(formant))
# csvs
# 
# # Clean up filenames
# csvs <- csvs %>% mutate(filename = gsub("_[[:digit:]]*$","",fs::path_ext_remove(basename(filename))))
# csvs
# 
# # Get interval code from file info CVS
# fileInfo <- vroom("file_information.csv")
# 
# fileInfo <- fileInfo %>% mutate(file = gsub("_[[:digit:]]*.wav$","",file)) %>% rename("filename" = "file")
# 
# csvs <- left_join(csvs,fileInfo %>% select(filename,label),by="filename") %>%
#                                     rename("vowel.code" = "label") %>%
#                                     mutate(token.code = paste0(vowel.code,"-",filename),
#                                            speaker = gsub("_list[[:digit:]]*_sent[[:digit:]]*$","",filename)
#                                            )
# csvs


# # Compare formant measurements across methods
# nrow(csvs)
# nrow(formants.homebrewed)
# qqplot(csvs$freq,formants.homebrewed$freq)


# # Use the Fast Track data:
# formants <- csvs
############################################################################



######################################
# Clean up some errors in TextGrid coding
######################################
# The following is only necessary if you *don't* call load("FastTrack_formant_data.Rdata") above.

############################################################################
# 
# bad.codes<-unique(formants$vowel.code[!(str_detect(formants$vowel.code, '[0jw][aiu][ptkbdg][jw][[:digit:]]'))])
# bad.codes
# 
# # Go clean up these TextGrids
# formants %>% filter(vowel.code %in% bad.codes) %>% distinct(token.code)
# 
# # Ulster 1, 2,3, say pˠibˠ instead of pʲibˠ
# # jibw => wibw
# formants.UL <- formants %>% filter(speaker %in% c("UL-subject1","UL-subject2","UL-subject3")) %>% 
#              mutate(vowel.code = str_replace(vowel.code,'jibw', 'wibw')) %>% 
#              mutate(token.code = str_replace(token.code,'jibw', 'wibw'))
# 
# formants.notUL <- formants %>% filter(!(speaker %in% c("UL-subject1","UL-subject2","UL-subject3")))
# 
# formants <- rbind(formants.notUL,formants.UL)
# 
# rm(formants.notUL,formants.UL)
# 
# 
# 
# ######################################
# # Clean up factors
# ######################################
# 
# # Split label codes into conditions.
# sl <- seq_len(nchar(formants$vowel.code[1]))
# formants<-separate(formants, vowel.code, paste0('X', sl), sep = sl,remove=F)
# 
# # Rename factors
# formants <- tibble(formants) %>% rename(prec.c = X1,v = X2, c.place = X3, sec.art = X4, rep = X5)
# 
# # Make sure all character vectors are actually treated as factors.
# formants <- formants %>% mutate_if(is.character,as.factor)
# 
# # Revalue factor levels.
# summary(formants$prec.c)
# formants <- formants %>% mutate(prec.c = case_when(prec.c == "j" ~ "Cʲ", prec.c == "w" ~ "Cˠ", prec.c == "0" ~ "V-initial"))
# 
# summary(formants$v)
# formants <- formants %>% mutate(v = case_when(v == "i" ~ "iː", v == "u" ~ "uː", v == "a" ~ "ɔː"))
# formants$v<-factor(formants$v,levels=c("iː","ɔː","uː"))
# 
# summary(formants$c.place)
# formants <- formants %>% mutate(c.place = case_when(c.place == "b" ~ "P", c.place == "p" ~ "P",
#                                               c.place == "d" ~ "T", c.place == "t" ~ "T",
#                                               c.place == "g" ~ "K", c.place == "k" ~ "K"
#                                               ))
# formants$c.place<-factor(formants$c.place,levels=c("P","T","K"))
# 
# summary(formants$sec.art)
# formants <- formants %>% mutate(sec.art = case_when(sec.art == "j" ~ "Cʲ", sec.art == "w" ~ "Cˠ"))
# 
# 
# ############
# # Correct speaker numbering to be consistent with how we use numbering in the text in our paper
# formants <- formants %>% mutate(token.code = str_replace_all(token.code,'CM-subject4', 'CM-subject3'))
# formants <- formants %>% mutate(vowel.code = str_replace_all(vowel.code,'CM-subject4', 'CM-subject3'))
# 
# formants <- formants %>% mutate(token.code = str_replace_all(token.code,'MU-subject2', 'MU-subject1'))
# formants <- formants %>% mutate(vowel.code = str_replace_all(vowel.code,'MU-subject2', 'MU-subject1'))
# 
# formants <- formants %>% mutate(token.code = str_replace_all(token.code,'MU-subject5', 'MU-subject2'))
# formants <- formants %>% mutate(vowel.code = str_replace_all(vowel.code,'MU-subject5', 'MU-subject2'))
# 
# formants <- formants %>% mutate(token.code = str_replace_all(token.code,'subject', ''))
# formants <- formants %>% mutate(speaker = str_replace_all(speaker,'subject', ''))
# 
# formants$speaker <- as.factor(formants$speaker)
# formants$token.code <- as.factor(formants$token.code)
# formants$vowel.code <- as.factor(formants$vowel.code)
# 
# 
# ############
# # Ulster 3 is not in our analysis anymore.
# formants <- subset(formants,speaker != "UL-3")
# 
############################################################################

######################################
# Get some summary stats
######################################
formants %>% group_by(formant) %>% summarize(mean=mean(freq,na.rm=T),
                                             median=median(freq,na.rm=T))

formants %>% group_by(v,formant) %>% summarize(mean=mean(freq,na.rm=T),
                                             median=median(freq,na.rm=T))

# subset(formants,formant == "F1" & freq > 800)$c.place


######################################
# Plot raw formants to make sure things look okay, grouped by token and speaker.
######################################
raw.formants.spk<-ggplot(data = formants)+
  geom_path(aes(x=step,y=freq,color=formant,
                group=interaction(token.code,formant)),alpha=0.5)+
  scale_color_manual(values=colorSet)+
  theme_bw(base_size = 24)+
  theme(axis.text = element_text(size=12))+
  guides(color="none")+
  scale_x_continuous(breaks = seq(min(formants$step,na.rm=T),max(formants$step,na.rm=T),20))+
  xlab("Normalized time (%)")+
  scale_y_continuous(breaks = seq(0,max(formants$freq,na.rm=T),200))

raw.formants.spk+facet_grid(v~speaker)
# raw.formants.spk+facet_grid(sec.art~speaker)


######################################
# Log-additive regression normalization
######################################
# The following is only necessary if you *don't* call load("FastTrack_formant_data.Rdata") above.

############################################################################
# 
##############
# NOTE: if you choose to use formants produced by Fast Track, this will take a *very* long time,
# because there are many, many more tightly-spaced formant measurements in that data.
##############
# #
# # The idea here is simple: some inter-talker variability in F1/F2 reflects physiology, i.e. the size of the vocal tract. If you want to normalize away from that variation specifically, you just need to scale speaker vowel spaces so that they are in the same space. This can be done by predicting observed formant values from vowel quality + speaker in a regression analysis, then transforming the original values by subtracting the estimated effect of speaker produced by the model (in essence re-scaling/sizing the vowel space---recall too that subtraction of log-transformed values is like division, i.e. ratio normalization)
# #
# # There are lots of apparent advantages to this method, e.g. when the data is not balanced across vowel qualities
# # https://asa.scitation.org/doi/10.1121/1.5047742
# # https://assta.org/proceedings/ICPhS2019/papers/ICPhS_1604.pdf
# # "Log-additive regression normalization was applied to
# # reduce interspeaker variation"
# # Barreda, S., Nearey, T. 2017. A regression approach to
# # vowel normalization for missing and unbalanced data.
# # J. Acoust. Soc. Am. 142(4), 2583.
# 
# 
# # An assumption of the method as implemented here is that F1/F2 should be scaled equally by speaker physiology/vocal tract size. This is explicitly defended on other grounds by Barreda & Nearey too.
# 
#  
# formants$log.Fx<-log(formants$freq)
#  
# # Create vqual x formant interaction factor
# formants$N = factor(interaction(formants$v,formants$formant))
# 
# M = lm(data=formants,
#        log.Fx~0+speaker+N,
#        contrasts=list(N=contr.sum))
# 
# # The estimated speaker means formant k can be extracted
# # as S=dummy.coef(M)$S, and can then be used to normalize
# # FF by subtracting each subjects S coefficient from
# # the log formant frequencies produced by that speaker for
# # that formant.
# 
# spknorms <- dummy.coef(M)$speaker
# # vnorms <- dummy.coef(M)$N
# 
# # Create column with normalization constant for each observation
# formants$spk.adjust<-rep(NA,nrow(formants))
# 
# # This is the main bottleneck in processing speed here.
# for (currRow in 1:nrow(formants)){
#   spk <- formants[currRow,]$speaker
#   adj<-spknorms[spk]
#   formants[currRow,]$spk.adjust<-adj
# 
# }
# unique(formants$spk.adjust)
# spknorms
# 
# # Apply speaker normalization as estimated from logistic regression
# head(formants)
# formants$log.Fx.norm<-formants$log.Fx-formants$spk.adjust
############################################################################



######################################
# Plot normalized formants to make sure things look okay, grouped by token and speaker.
######################################

ggplot(data = formants)+
  geom_path(aes(x=step,y=log.Fx.norm,color=formant,
                group=interaction(token.code,formant)),alpha=0.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~speaker)+
  theme_bw(base_size = 24)+
  theme(axis.text = element_text(size=12))+
  guides(color="none")+
  scale_x_continuous(breaks = seq(min(formants$step,na.rm=T),max(formants$step,na.rm=T),20))+
  xlab("Normalized time (%)")



######################################
# F3 normalization
######################################
formants.wider <- formants %>% group_by(token.code,step) %>% pivot_wider(names_from = formant,values_from = freq) %>% fill(everything(), .direction = "downup") %>% slice(1)

formants.wider$F1.F3<-formants.wider$F1/formants.wider$F3
formants.wider$F2.F3<-formants.wider$F2/formants.wider$F3

formants.wider <- formants.wider %>% select(-c(F1,F2,F3))

formants.F3.norm<-formants.wider %>% pivot_longer(cols=c(F1.F3,F2.F3),names_to = "formant.F3",values_to = "freq")

# Plot F3 normalized formants
ggplot(data = formants.F3.norm)+
  geom_path(aes(x=step,y=freq,color=formant.F3,
                group=interaction(token.code,formant.F3)),alpha=0.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~speaker)+
  theme_bw(base_size = 24)+
  theme(axis.text = element_text(size=12))+
  guides(color="none")+
  scale_x_continuous(breaks = seq(min(formants$step,na.rm=T),max(formants$step,na.rm=T),20))+
  xlab("Normalized time (%)")


nrow(subset(formants,formant!="F3"))
nrow(formants.F3.norm)


# Fairly high correlation between normalization methods.
cor(arrange(subset(formants,formant!="F3"),token.code,step,formant)$log.Fx.norm,
    arrange(formants.F3.norm,token.code,step,formant.F3)$freq)

# Fairly high correlation between normalized data and original data.
cor(formants$log.Fx.norm,formants$freq)

cor(arrange(subset(formants,formant!="F3"),token.code,step,formant)$freq,
    arrange(formants.F3.norm,token.code,step,formant.F3)$freq)



######################################
# Plot pooled and normalized data, faceting appropriately
######################################


######################################
# Filter out any data we think is too noisy
######################################

# Subset out bad conditions, speakers, etc.

# Get rid of extreme values
formants.trimmed <- formants %>% filter(formant != "F3") %>% group_by(formant) %>% 
                                 filter(log.Fx.norm <= (mean(log.Fx.norm, na.rm=T) + 2 * sd(log.Fx.norm, na.rm=T)) &
                                        log.Fx.norm >= (mean(log.Fx.norm, na.rm=T) - 2 * sd(log.Fx.norm, na.rm=T))
                                        )

formants.F3.norm.trimmed <- formants.F3.norm %>% group_by(formant.F3) %>% 
                                 filter(freq <= mean(freq) + 2 * sd(freq) &
                                        freq >= mean(freq) - 2 * sd(freq))

# How much F2 data does this eliminate?
nrow(subset(formants.trimmed,formant=="F2"))
nrow(subset(formants,formant=="F2"))
nrow(subset(formants,formant=="F2")) - nrow(subset(formants.trimmed,formant=="F2"))
nrow(subset(formants.trimmed,formant=="F2"))/nrow(subset(formants,formant=="F2"))

# Mean # of measurements per formant per vowel:
meas.count<-formants.trimmed %>% filter(formant=="F2") %>% group_by(token.code,formant) %>% summarize(count = n()) %>% ungroup()
meas.count
mean(meas.count$count)

nrow(meas.count)

##########
# Normalized log frequency

# Outlier removal doesn't fully clean up formant tracking errors, e.g. F1 errors for the Ulster speakers (due to preaspiration?)

ggplot(data = formants)+
  geom_path(aes(x=step,y=log.Fx.norm,color=formant,
                group=interaction(token.code,formant)),alpha=0.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~speaker)+
  theme_bw(base_size = 24)+
  theme(axis.text = element_text(size=12))+
  guides(color="none")+
  scale_x_continuous(breaks = seq(min(formants$step,na.rm=T),max(formants$step,na.rm=T),20))+
  xlab("Normalized time (%)")


lognorm.F2<-ggplot(data = subset(formants.trimmed,formant=="F2"))+
  geom_vline(xintercept=median(formants.trimmed$step),lwd=1.5,color="grey60")+
  geom_smooth(aes(x=step,y=log.Fx.norm,color=sec.art,lty=sec.art),lwd=2.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~c.place)+
  theme_bw(base_size = 24)+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, 'cm')
        )+
  scale_x_continuous(breaks = seq(min(formants$step,na.rm=T),max(formants$step,na.rm=T),20))+
  xlab("Normalized time (%)")+
  ylab("Normalized F2")
lognorm.F2

# Save w/ cairo
setwd("")
cairo_pdf(file="lognorm_F2.pdf",
          width=10,height=6)
  lognorm.F2
dev.off()

UL.data <- formants.trimmed %>% filter(str_detect(speaker,"UL"))

lognorm.F2.UL<-ggplot(data = subset(UL.data,formant=="F2"))+
  geom_vline(xintercept=median(formants.trimmed$step),lwd=1.5,color="grey60")+
  geom_smooth(aes(x=step,y=log.Fx.norm,color=sec.art,lty=sec.art),lwd=2.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~c.place)+
  theme_bw(base_size = 24)+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, 'cm')
  )+
  scale_x_continuous(breaks = seq(min(formants.trimmed$step,na.rm=T),max(formants.trimmed$step,na.rm=T),1))+
  xlab("Normalized time (%)")+
  ylab("Normalized F2")
lognorm.F2.UL

# # Save w/ cairo
# setwd("")
# cairo_pdf(file="lognorm_F2_UL.pdf",
#           width=10,height=6)
#   lognorm.F2.UL
# dev.off()
# 

# F3 Normalized frequency
ggplot(data = subset(formants.F3.norm.trimmed,formant.F3=="F2.F3"))+
  geom_vline(xintercept=median(formants.F3.norm.trimmed$step),lwd=1.5,color="grey60")+
  geom_smooth(aes(x=step,y=freq,color=sec.art,lty=sec.art),lwd=2.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~c.place)+
  theme_bw(base_size = 24)+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, 'cm')
  )+
  scale_x_continuous(breaks = seq(min(formants.F3.norm.trimmed$step,na.rm=T),max(formants.F3.norm.trimmed$step,na.rm=T),20))+
  xlab("Normalized time (%)")+
  ylab("F3-normalized frequency")


# Remove Ulster speakers
not.UL.data <- formants.trimmed %>% filter(!str_detect(speaker,"UL"))

lognorm.F2.not.UL<-ggplot(data = subset(not.UL.data,formant=="F2"))+
  geom_vline(xintercept=median(formants.trimmed$step),lwd=1.5,color="grey60")+
  geom_smooth(aes(x=step,y=log.Fx.norm,color=sec.art,lty=sec.art),lwd=2.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~c.place)+
  theme_bw(base_size = 24)+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, 'cm')
  )+
  scale_x_continuous(breaks = seq(min(formants.trimmed$step,na.rm=T),max(formants.trimmed$step,na.rm=T),20))+
  xlab("Normalized time (%)")+
  ylab("Normalized F2")
lognorm.F2.not.UL


#################
# Plot F2 transitions in physical time


formants.ms.f2 <- formants.trimmed %>% filter(formant=="F2") %>% group_by(token.code) %>% mutate(ms.before = (max(time) - time)*1000)
formants.ms.f2

lognorm.F2.ms<-ggplot(data = formants.ms.f2 %>% filter(ms.before <= 200))+
  # geom_vline(xintercept=median(formants.trimmed$step),lwd=1.5,color="grey60")+
  geom_smooth(aes(x=ms.before,y=log.Fx.norm,color=sec.art,lty=sec.art),lwd=2.5)+
  scale_color_manual(values=colorSet)+
  facet_grid(v~c.place)+
  theme_bw(base_size = 24)+
  theme(strip.text.y = element_text(angle = 0),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(size=12),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(2, 'cm')
  )+
  scale_x_reverse(breaks = seq(min(formants.ms.f2$ms.before,na.rm=T),max(formants.ms.f2$ms.before,na.rm=T),50))+
  xlab("Time (in ms) preceding coda C")+
  ylab("Normalized F2")
lognorm.F2.ms

  # Save w/ cairo
setwd("")
cairo_pdf(file="lognorm_F2_ms.pdf",
          width=10,height=6)
  lognorm.F2.ms
dev.off()
