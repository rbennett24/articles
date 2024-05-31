library(car)
library(tidyverse)
library(broom)
library(moments) # For skewness, kurtosis
# library(onewaytests)
library(diptest)

outdir <- "C:\\Users\\Tiamat\\Dropbox\\Research\\Irish\\Irish_ultrasound_shared\\Scripts\\R scripts\\Carnie_volume\\"
setwd(outdir)

# Choose a colorblind friendly palette
colorSet=palette.colors(n = 8,palette = "R4")

# If you'd like to load the raw data, you can do that here:
# load("raw_EdgeTrak_data_FACL.RData")

# To reproduce our procedure for range normalization of the raw data, do the following:
# edgetrak.minmax <- raw.EdgeTrak.data %>% group_by(Speaker) %>% mutate(Y = (Y-min(Y,na.rm=T))/(max(X,na.rm=T)-min(X,na.rm=T)),
#                                                                       X = (X-min(X,na.rm=T))/(max(X,na.rm=T)-min(X,na.rm=T))
#                                                                       ) %>% ungroup()


# To reproduce our procedure for finding peak dorsal points, do the following:
# load("edgetrak_output_normalized_Irish_FACL.RData")

# Tongue.peaks <- edgetrak.minmax %>% group_by(Speaker,c.place,v,sec.art,syll.pos,rep,frm.pos) %>%
#                                     # Lower Y values are physically higher, so we look for minimum values.
#                                     slice_min(Y) %>%
#                                     summarize(X = mean(X), # Take mean X value in case of ties/plateaus
#                                               Y = unique(Y)) %>% 
#                                     ungroup()


# Read in data frame of peak dorsal points, which is what we actually analyze.
load("tongue_peaks_Irish_FACL.RData")

# Check factor types, and correct if needed.
lapply(Tongue.peaks,is)

# # load(paste0(outdir,"tongue.peaks.OLD.Rdata"))
# # tp.old<-Tongue.peaks
# 
# load(paste0(outdir,"tongue.peaks.RData"))
# tp.raw<-Tongue.peaks

# No midpoint data
# tp.raw <- subset(tp.raw,frm.pos != "midpoint")

# No Munster 2 coronal data
# tp.raw <- subset(tp.raw,!(Speaker == "Munster, Speaker S2" & c.place == "T"))

head(tp.raw)
nrow(tp.raw)


# Rename some factor values.
tp.raw$sec.art<-factor(tp.raw$sec.art,levels=c("Pal","Vel"))
tp.raw$sec.art<-dplyr::recode(tp.raw$sec.art,Pal = "Cʲ",Vel="Cˠ")
tp.raw$frm.pos<-dplyr::recode(tp.raw$frm.pos,start = "C start",end="C end",midpoint = "C midpoint")
tp.raw$c.place<-dplyr::recode(tp.raw$c.place,labial = "P",coronal="T",dorsal="K")

# Remove any observations that have < n rows for n landmarks, e.g. it doesn't make sense to ask
# about gestural magnitude at C start vs. C end if you only have one of those data points.
# lndmrk.types<-length(unique(tp.raw$frm.pos)) # How many landmarks are we working with
# lndmrk.types
# 
# tp <- tp.raw %>% group_by(Speaker,syll.pos,c.place,v,sec.art,rep) %>% filter(n() == lndmrk.types)
# nrow(tp)

tp <- tp.raw


tp %>% group_by(Speaker,c.place) %>% summarize(count=n())


# Plot request from Jaye:
# could you create line plots showing TB backness over start-midpoint-end? This might be a mess, but maybe something would seem clear if we separated by syllable position and secondary art. But maybe not, since things may also depend on place, vowel, and subject...
# Plot counts of gestural peaks. 

# Add token-level identifier
tp <- tp %>% mutate(token = paste0(Speaker,"-",syll.pos,"-",c.place,"-",v,"-",sec.art,"-",rep))
 
tp.means<-tp %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% summarize(mean = mean(X))


# Add vowel height coding
tp <- tp %>% mutate(v.height = case_when(v == "a" ~ "/ɔː/",
                                          TRUE ~ "/iː uː/"))
# 
tp$v.height<-factor(tp$v.height)



# TB.back.by.landmark<-ggplot(tp,aes(x=frm.pos,y=X,color=frm.pos))+
#   geom_violin()+
#   geom_boxplot(width=0.25)+
#   geom_point(data = tp.means, 
#              mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   geom_line(data = tp.means, 
#             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   facet_grid(syll.pos~c.place + sec.art)+
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   # scale_color_manual(values=rev(colorSet))+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank())+
#   guides(color="none")
# 
# TB.back.by.landmark
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
#   cairo_pdf(file="across_landmarks.pdf",
#             width=24,height=8.5)
#   TB.back.by.landmark
# dev.off()


# TB.back.by.landmark.reps<-ggplot(tp,aes(x=frm.pos,y=X))+
#   # geom_violin()+
#   # geom_boxplot(width=0.25)+
#   geom_line(aes(group=token),color="gray50",alpha=0.6)+
#   geom_point(data = tp.means, 
#              mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   geom_line(data = tp.means, 
#             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   #geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X),inherit.aes = F)+
#   facet_grid(syll.pos~c.place + sec.art)+
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   # scale_color_manual(values=rev(colorSet))+
#   theme_bw(base_size = 48)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank())+
#   guides(color="none")
# 
# TB.back.by.landmark.reps
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
# cairo_pdf(file="across_landmarks_byrep.pdf",
#           width=24,height=8.5)
#   TB.back.by.landmark.reps
# dev.off()
# 
# 
# 
# TB.back.by.landmark.reps.spk<-ggplot(tp,aes(x=frm.pos,y=X))+
#   # geom_violin()+
#   # geom_boxplot(width=0.25)+
#   geom_line(aes(group=token,color=Speaker),alpha=0.2)+
#   geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X),inherit.aes = F)+
#   geom_point(data = tp.means, 
#              mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   geom_line(data = tp.means, 
#             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   facet_grid(syll.pos~c.place + sec.art)+
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   scale_color_manual(values=rev(colorSet))+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank())+
#   guides(color="none")
# 
# TB.back.by.landmark.reps.spk
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
# cairo_pdf(file="across_landmarks_byrep_spk.pdf",
#           width=24,height=8.5)
#   TB.back.by.landmark.reps.spk
# dev.off()









#######################################
# For Carnie volume:
# Look at trajectories, but subtract out starting value for X

# Remove tokens that don't have a start value
tp.startnorm <- tp %>% filter("C start" %in% frm.pos)
nrow(tp)
nrow(tp.startnorm)

tp.startnorm <- tp.startnorm %>% group_by(token) %>% mutate(X.startnorm = X-X[frm.pos=="C start"])

tp.startnorm$frm.pos <- recode(tp.startnorm$frm.pos,"C start" = "Start", "C midpoint" = "Mid", "C end" = "End")

# tp.startnorm$frm.pos <- factor(tp.startnorm$frm.pos,levels = c("Start","Mid","End"))

tp.means<-tp.startnorm %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% summarize(mean = mean(X.startnorm))



TB.back.by.landmark<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm,color=frm.pos))+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  # geom_hline(aes(yintercept=0.0),color="black",lwd=1)+
  geom_violin()+
  geom_boxplot(width=0.25)+
  geom_jitter(alpha=0.2,width=0.25)+
  geom_point(data = tp.means, 
             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1.25,alpha=0.8)+
  geom_line(data = tp.means, 
            mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1.25,alpha=0.8)+
  facet_grid(syll.pos~c.place + sec.art)+
  scale_size(range = c(1, 15))+
  # scale_size_area()+
  # scale_color_manual(values=rev(colorSet))+
  theme_bw(base_size = 22)+
  theme(strip.text = element_text(face = "bold",size=22),
        plot.title = element_text(size = 16),
        axis.title=element_blank())+
  guides(color="none")

TB.back.by.landmark

# Save w/ cairo
setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
cairo_pdf(file="across_landmarks_startnorm.pdf",
          width=12,height=9)
  TB.back.by.landmark
dev.off()


# 
# TB.back.by.landmark.vert<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm,color=frm.pos))+
#   geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
#   geom_violin()+
#   geom_boxplot(width=0.25)+
#   geom_jitter(alpha=0.2,width=0.25)+
#   geom_point(data = tp.means, 
#              mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   geom_line(data = tp.means, 
#             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
#   facet_grid(sec.art + c.place ~ syll.pos)+
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   # scale_color_manual(values=rev(colorSet))+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank(),
#         axis.text=element_text(size=12),
#         strip.text.y = element_text(angle = 0))+
#   guides(color="none")
# 
# TB.back.by.landmark.vert
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
# cairo_pdf(file="across_landmarks_startnorm_vert.pdf",
#           width=4.5,height=12)
# TB.back.by.landmark.vert
# dev.off()
# 
# 


TB.back.by.landmark.reps<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm))+
  # geom_violin()+
  # geom_boxplot(width=0.25)+
  geom_line(aes(group=token),color="gray75",alpha=0.5)+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_point(data = tp.means, 
             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1.25,alpha=0.8)+
  geom_smooth(method="loess",data=tp.startnorm,aes(x=frm.pos,y=X.startnorm,group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9,fill="cornflowerblue")+
  # geom_line(data = tp.means, 
            # mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1.25,alpha=0.8)+
  #geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X),inherit.aes = F)+
  facet_grid(syll.pos~c.place + sec.art)+
  scale_size(range = c(1, 15))+
  # scale_size_area()+
  # scale_color_manual(values=rev(colorSet))+
  theme_bw(base_size = 22)+
  theme(strip.text = element_text(face = "bold", size = 22),
        plot.title = element_text(size = 16),
        axis.title=element_blank())+
  guides(color="none")

TB.back.by.landmark.reps

# Save w/ cairo
setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
cairo_pdf(file="across_landmarks_byrep_startnorm.pdf",
          width=12,height=9)
  TB.back.by.landmark.reps
dev.off()


# TB.back.by.landmark.reps.vert<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm))+
#   # geom_violin()+
#   # geom_boxplot(width=0.25)+
#   geom_line(aes(group=token),color="gray65",alpha=0.5)+
#   geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
#   # geom_point(data = tp.means, 
#   #            mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9)+
#   # geom_line(data = tp.means, 
#   #           mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9)+
#   geom_smooth(method="loess",data=tp.startnorm,aes(x=frm.pos,y=X.startnorm,group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9)+
#   facet_grid(sec.art + c.place ~ syll.pos)+
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   # scale_color_manual(values=rev(colorSet))+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank(),
#         axis.text=element_text(size=12),
#         strip.text.y = element_text(angle = 0),
#         panel.grid.major = element_line(size = 0.5))+
#   guides(color="none")
# 
# TB.back.by.landmark.reps.vert
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
# cairo_pdf(file="across_landmarks_byrep_startnorm_vert.pdf",
#           width=4.5,height=12)
#   TB.back.by.landmark.reps.vert
# dev.off()




TB.back.by.landmark.reps.spk<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm))+
  # geom_violin()+
  # geom_boxplot(width=0.25)+
  # geom_line(aes(group=token,color=Speaker),alpha=0.2)+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X.startnorm),inherit.aes = F,se=F)+
  # geom_point(data = tp.means, 
             # mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
  # geom_line(data = tp.means, 
            # mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
  facet_grid(syll.pos~c.place + sec.art)+
  scale_size(range = c(1, 15))+
  # scale_size_area()+
  scale_color_manual(values=rev(colorSet))+
  theme_bw(base_size = 22)+
  theme(strip.text = element_text(face = "bold",size=22),
        plot.title = element_text(size = 16),
        axis.title=element_blank())+
  guides(color="none")

TB.back.by.landmark.reps.spk

# Save w/ cairo
setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
cairo_pdf(file="across_landmarks_byrep_spk_startnorm.pdf",
          width=12,height=9)
TB.back.by.landmark.reps.spk
dev.off()




# TB.back.by.landmark.reps.spk.vert<-ggplot(tp.startnorm,aes(color=Speaker,group=Speaker,x=frm.pos,y=X.startnorm))+
#   # geom_violin()+
#   # geom_boxplot(width=0.25)+
#   # geom_line(aes(group=token,color=Speaker),alpha=0.2)+
#   geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
#   geom_smooth(method="loess",aes(color=Speaker,lty=Speaker,group=Speaker,x=frm.pos,y=X.startnorm),inherit.aes = F,se=F)+
#   # geom_point(data = tp.means, 
#   #            mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9)+
#   # geom_line(data = tp.means, 
#   #           mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9)+
#   # geom_smooth(method="loess",data=tp.startnorm,aes(x=frm.pos,y=X.startnorm,group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9)+
#   facet_grid(sec.art + c.place ~ syll.pos)+
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   # scale_color_manual(values=rev(colorSet))+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank(),
#         axis.text=element_text(size=12),
#         strip.text.y = element_text(angle = 0),
#         panel.grid.major = element_line(size = 0.5))+
#   guides(color="none",lty="none")
# 
# TB.back.by.landmark.reps.spk.vert
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
# cairo_pdf(file="across_landmarks_byrep_spk_startnorm_vert.pdf",
#           width=4.5,height=12)
# TB.back.by.landmark.reps.spk.vert
# dev.off()



###############
# Get some descriptive statistics
descript.tab<-tp.startnorm %>% group_by(syll.pos,c.place,sec.art) %>%
              summarize(mean = mean(X.startnorm,na.rm = T),
              # median = median(X.startnorm,na.rm = T),
              range = paste0("[",round(range(X.startnorm,na.rm=T)[1],3),",",round(range(X.startnorm,na.rm=T)[2],3),"]"),
              drange = diff(range(X.startnorm,na.rm=T)),
              sd = sd(X.startnorm,na.rm = T),
              iq = paste0("[",round(summary(X.startnorm)[2],3),",",round(summary(X.startnorm)[5],3),"]"),
              diq = IQR(X.startnorm,na.rm=T)
              # cov = sd(X.startnorm,na.rm = T)/abs(mean(X.startnorm,na.rm = T))
              ) %>% ungroup()

descript.tab$type<-paste0(descript.tab$syll.pos," ",descript.tab$c.place,str_replace(descript.tab$sec.art,"C",""))

descript.tab <- descript.tab %>% relocate(type)
descript.tab <- descript.tab %>% select(-c(syll.pos,c.place,sec.art)) 
descript.tab$type<-paste0(c("*","*","*","","*","","*","*","*","","",""),descript.tab$type)
descript.tab

# Send the table to LaTeX
library(xtable)
print(xtable(descript.tab,digits=3), include.rownames=FALSE)


# Get some more descriptive statistics for the overall backness value set
tp.startnorm %>% ungroup() %>%
  summarize(mean = mean(X,na.rm = T),
            median = median(X,na.rm = T),
            range = paste0("[",round(range(X,na.rm=T)[1],4),",",round(range(X,na.rm=T)[2],4),"]"),
            drange = diff(range(X,na.rm=T)),
            sd = sd(X,na.rm = T),
            iq = paste0("[",round(summary(X)[2],4),",",round(summary(X)[5],4),"]"),
            diq = IQR(X,na.rm=T)
  )


###############
# Do a fun little grouped t-test,
# just looking at C end values.
t.test.outputs <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% do(tidy(t.test(.$X.startnorm))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))

t.test.outputs

nrow(t.test.outputs)

pthresh <- 0.0500000000000000000001/nrow(t.test.outputs)

t.sig<-subset(t.test.outputs,p.value < pthresh)
nrow(t.sig)
arrange(t.sig,syll.pos,c.place,sec.art)


# Just to get some average values for plain/pal comparisons
secartmeans <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(c.place,syll.pos) %>% do(tidy(t.test(.$X ~ .$sec.art))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))

secartmeans <- secartmeans %>% select(c.place,syll.pos,estimate)
secartmeans

secartmeans <- tp.startnorm  %>% ungroup() %>% group_by(syll.pos) %>% filter(frm.pos == "End") %>% do(tidy(t.test(.$X ~ .$sec.art))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))

secartmeans <- secartmeans %>% select(syll.pos,estimate)
secartmeans


###########
# See if bootstrapping changes anything
library(Hmisc)
# https://stackoverflow.com/a/76310732


# Start with Bonferroni-corrected p-value,
# just looking at C end values
bs.group<-subset(tp.startnorm,frm.pos=="End") %>% 
  dplyr::group_by(syll.pos,c.place,sec.art) %>% 
  dplyr::summarize(result = Hmisc::smean.cl.boot(X.startnorm,B=100000,conf.int=1-pthresh) %>% t %>% as.data.frame) %>% 
  tidyr::unnest(result)

detach(package:Hmisc) # Leads to wonky stuff with mean(), n() and other functions apparently

nrow(bs.group)
bs.group<- bs.group %>% mutate(result = case_when(
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
  TRUE ~ "NS"
)
)

bs.group.sig<-subset(bs.group,Lower > 0 | Upper < 0)
nrow(bs.group.sig)
print(arrange(bs.group.sig,syll.pos,c.place,sec.art),n=nrow(bs.group.sig))

bs.group %>% group_by(syll.pos,c.place,sec.art) %>% summarize(count=n())
bs.group.sig %>% group_by(syll.pos,c.place,sec.art,result) %>% summarize(count=n())



##############
# Add p-values
t.test.outputs <- t.test.outputs %>% mutate(plabs = as.character(case_when(p.value < pthresh & estimate > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",                                                                   p.value < pthresh & estimate > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",
                                                                           
                                                                           p.value < pthresh & estimate > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
                                                                           p.value < pthresh & estimate > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
                                                                           TRUE ~ ""
)))


t.test.outputs


TB.back.by.landmark.labeled <- TB.back.by.landmark + geom_label(data=t.test.outputs %>% filter(plabs!=""),aes(label=plabs,x=frm.pos,y=0.35),nudge_x=-0.15,size=8,color="black")

TB.back.by.landmark.labeled

# Save w/ cairo
setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
cairo_pdf(file="across_landmarks_startnorm_labeled.pdf",
          width=12,height=9)
  TB.back.by.landmark.labeled
dev.off()



##############
# What if we do Bayesian t-tests instead?

library(Bolstad)
bayes.t.test(tp.startnorm$X.startnorm)

t.test.outputs <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% do(tidy(bayes.t.test(.$X.startnorm))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))

t.test.outputs

nrow(t.test.outputs)

pthresh <- 0.0500000000000000000001/nrow(t.test.outputs)

t.sig<-subset(t.test.outputs,p.value < pthresh)
nrow(t.sig)
arrange(t.sig,syll.pos,c.place,sec.art)

library(BayesFactor)
library(bayestestR)
ttestBF(tp.startnorm$X.startnorm)
describe_posterior(ttestBF(tp.startnorm$X.startnorm),ci = 1)

# If the HDI is completely outside the ROPE, the “null hypothesis” for this parameter is “rejected”. If the ROPE completely covers the HDI, i.e., all most credible values of a parameter are inside the region of practical equivalence, the null hypothesis is accepted. Else, it’s unclear whether the null hypothesis should be accepted or rejected.
#
#If the full ROPE is used (i.e., 100% of the HDI), then the null hypothesis is rejected or accepted if the percentage of the posterior within the ROPE is smaller than to 2.5% or greater than 97.5%. Desirable results are low proportions inside the ROPE (the closer to zero the better).

t.test.outputs <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% do(describe_posterior(ttestBF(.$X.startnorm),ci = 1)) 

# t.test.outputs <- t.test.outputs %>% mutate(XXXXX = round(as.numeric(format(XXXX, scientific = FALSE)),5))
 
t.test.outputs

nrow(t.test.outputs)

pthresh <- 0.0500000000000000000001/nrow(t.test.outputs)

# this conversion is only valid when the lowest possible values of pd is 0.5 - i.e., when the posterior represents continuous parameter space 
# https://easystats.github.io/bayestestR/reference/pd_to_p.html?q=p-val#null
min(t.test.outputs$pd)
t.test.outputs <- t.test.outputs %>% mutate(p.value = 1 -pd)

t.sig<-subset(t.test.outputs,p.value < pthresh)
nrow(t.sig)
arrange(t.sig,syll.pos,c.place,sec.art)





####################################
TB.back.by.landmark.reps.spk.noplace<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm))+
  # geom_violin()+
  # geom_boxplot(width=0.25)+
  # geom_line(aes(group=token,color=Speaker),alpha=0.2)+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X.startnorm),inherit.aes = F,se=F)+
  # geom_point(data = tp.means, 
  # mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
  # geom_line(data = tp.means, 
  # mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.8)+
  facet_grid(syll.pos ~ sec.art)+
  scale_size(range = c(1, 15))+
  # scale_size_area()+
  scale_color_manual(values=rev(colorSet))+
  theme_bw(base_size = 28)+
  theme(strip.text = element_text(face = "bold",size=28),
        plot.title = element_text(size = 24),
        axis.title=element_blank())+
  guides(color="none")

TB.back.by.landmark.reps.spk.noplace

# Save w/ cairo
setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
cairo_pdf(file="across_landmarks_byrep_spk_startnorm_NOPLACE.pdf",
          width=8,height=6)
TB.back.by.landmark.reps.spk.noplace
dev.off()




###############
# Fit an lmer.
library(lme4)
# library(lmerTest)
library(performance) # For collinearity
scriptdir<-"C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/HISPhonCog/"
source(paste0(scriptdir,"lmer_collinearity_tools.R"))
library(piecewiseSEM) # For computing marginal r^2; https://github.com/jslefche/piecewiseSEM
                      # https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/


# Data: tp.startnorm
lmerdata <- subset(tp.startnorm,frm.pos == "End")
lmerdata <- droplevels(lmerdata)

# Order factors to reflect desired reference levels.
lmerdata


# Order factors to reflect desired reference levels.
lmerdata$c.place <- lmerdata$c.place %>% fct_relevel("P", after = length(lmerdata$c.place))

lmerdata$v <- lmerdata$v %>% fct_relevel("i", after = length(lmerdata$v))

lmerdata$syll.pos <- lmerdata$syll.pos %>% fct_relevel("Onset", after = length(lmerdata$syll.pos))

lmerdata$sec.art <- lmerdata$sec.art %>% fct_relevel("Cʲ", after = length(lmerdata$sec.art))


contrasts(lmerdata$c.place)
contrasts(lmerdata$c.place)<-contr.sum(length(levels(lmerdata$c.place))) # Sum coding
contrasts(lmerdata$c.place)

contrasts(lmerdata$v)
contrasts(lmerdata$v)<-contr.sum(length(levels(lmerdata$v))) # Sum coding
contrasts(lmerdata$v)

contrasts(lmerdata$syll.pos)
contrasts(lmerdata$syll.pos)<-contr.sum(length(levels(lmerdata$syll.pos))) # Sum coding
contrasts(lmerdata$syll.pos)

contrasts(lmerdata$sec.art)
contrasts(lmerdata$sec.art)<-contr.sum(length(levels(lmerdata$sec.art))) # Sum coding
contrasts(lmerdata$sec.art)


fm<-lmer(data = lmerdata, X.startnorm ~ (c.place + syll.pos + sec.art)^3 + (c.place + syll.pos + v + sec.art)^2 + (1+syll.pos|Speaker),REML = F) # Use REML=F for log-likelihood comparison in step-down model reduction.
anova(fm)
summary(fm)
check_collinearity(fm)  # Low collinearity
max(vif.mer(fm))
kappa.mer(fm)

# Model reduction
anova(fm)[order(anova(fm)[4]),] # Shorthand for sorting anova by F-value (decreasing)
coef(summary(fm))[order(abs(coef(summary(fm))[,1]),decreasing=T),]

m.r1<-update(fm,~.-c.place:syll.pos:sec.art)
anova(fm,m.r1) # Significant
m.r1<-update(fm,~.-sec.art:v)
anova(fm,m.r1) # Significant
m.r1<-update(fm,~.-c.place:v)
anova(fm,m.r1) # Significant
m.r1<-update(fm,~.-syll.pos:v)
anova(fm,m.r1) # Significant

# So we're stuck with the full model!

# Get some p-values
length(fixef(fm)) # Get the number of fixed effects predictors
nrow(lmerdata) # Get the number of observation
df.ub<-nrow(lmerdata)-length(fixef(fm)) # upper bound
df.ub

for (x in rownames(coef(summary(fm)))){cat(x,"\n")}
for (x in (coef(summary(fm))[ , "Estimate"])){cat(round(x,6),"\n")}
for (x in (coef(summary(fm))[ , "Std. Error"])){cat(round(x,6),"\n")}
for (x in (coef(summary(fm))[ , "t value"])){cat(round(x,3),"\n")}
tvals<-round(coef(summary(fm))[ , "t value"],3)

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

pvals<-c()
for (t in tvals){
  trd<-round(pval.maker(t),3)
  tout<-str_replace(as.character(trd), "^0\\.", ".")    
  cat(tout,"\n")
  pvals<-c(pvals,tout)
}
pvals<-as.numeric(pvals)
pvals


# Check normality of residuals --- not too bad!
cor(qqnorm(residuals(fm))$x,
    residuals(fm))
qqnorm(residuals(fm))
qqline(residuals(fm))

summary(residuals(fm))
ggplot(data=data.frame("resid"=residuals(fm)))+
  geom_density(aes(x=resid),fill="darkgrey",alpha=0.75)+
  theme_bw(base_size = 48)


# Repeat model stats
anova(fm)
summary(fm)
check_collinearity(fm)  # Low collinearity
max(vif.mer(fm))
kappa.mer(fm)

# Coding and reference levels
contrasts(lmerdata$c.place)
contrasts(lmerdata$syll.pos)
contrasts(lmerdata$v)
contrasts(lmerdata$sec.art)

# Check r^2 -- not great, but in a sense that's okay, you don't want to overfit.
rsquared(fm)


summary(lmerdata$X)
sd(lmerdata$X)


# Get model summary
res.table <- as.data.frame(round(coef(summary(fm)),6))
res.table$pvalue<-pvals
res.table


# Drop t value and sort on estimate
res.table <- res.table %>% dplyr::select(-c("t value")) %>% dplyr::arrange(desc(abs(Estimate)))
res.table

# Replace column names
res.table<-res.table%>%rownames_to_column()
colnames(res.table)<-c("Predictor","Estimate","SE","p-value")


# Replace predictor values with more informative names
# Coding and reference levels
contrasts(lmerdata$c.place)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'c.place1', 'Coronal'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'c.place2', 'Dorsal'))
res.table


contrasts(lmerdata$syll.pos)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'syll.pos1', 'Coda'))
res.table

contrasts(lmerdata$v)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v1', 'u'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v2', 'a'))
res.table

contrasts(lmerdata$sec.art)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'sec.art1', 'C-vel'))
res.table

print(xtable(res.table,digits=5), include.rownames=FALSE)




###############
# Do a fun little grouped t-test,
# just looking at C end values...for each participant.
t.test.outputs <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(syll.pos,c.place,sec.art,frm.pos,Speaker) %>% do(tidy(t.test(.$X.startnorm))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))
t.test.outputs

nrow(t.test.outputs)

pthresh <- 0.0500000000000000000001/nrow(t.test.outputs)

t.sig<-subset(t.test.outputs,p.value < pthresh)
nrow(t.sig)
arrange(t.sig,syll.pos,c.place,sec.art,Speaker)

# Add p-values
t.test.outputs <- t.test.outputs %>% mutate(plabs = as.character(case_when(p.value < pthresh & estimate > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
                                                                          p.value < pthresh & estimate < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",                                                                   p.value < pthresh & estimate > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
                                                                          p.value < pthresh & estimate < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",

                                                                          p.value < pthresh & estimate > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
                                                                          p.value < pthresh & estimate < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
                                                                          p.value < pthresh & estimate > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
                                                                          p.value < pthresh & estimate < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
                                                                          TRUE ~ ""
)))


t.test.outputs




############
# Try a little bootstrapping...for each participant.
library(Hmisc)
# https://stackoverflow.com/a/76310732


# Start with Bonferroni-corrected p-value,
# just looking at C end values
bs<-subset(tp.startnorm,frm.pos=="End") %>% 
    dplyr::group_by(Speaker,syll.pos,c.place,sec.art) %>% 
    dplyr::summarize(result = Hmisc::smean.cl.boot(X.startnorm,B=10000,conf.int=1-pthresh) %>% t %>% as.data.frame) %>% 
    tidyr::unnest(result)

detach(package:Hmisc) # Leads to wonky stuff with mean(), n() and other functions apparently

nrow(bs)
bs<- bs %>% mutate(result = case_when(
                                (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
                                (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",
                                (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
                                (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
                                (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
                                (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",
                                (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
                                (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
                                TRUE ~ "NS"
                                )
    )


bs.sig<-subset(bs,Lower > 0 | Upper < 0)
nrow(bs.sig)
print(arrange(bs.sig,syll.pos,c.place,sec.art,Speaker),n=nrow(bs.sig))

bs %>% group_by(syll.pos,c.place,sec.art) %>% summarize(count=n())
bs.sig %>% group_by(syll.pos,c.place,sec.art,result) %>% summarize(count=n())


# Now without Bonferroni correction,
# just looking at C end values
library(Hmisc)
bs.uncorr<-subset(tp.startnorm,frm.pos=="End") %>% 
  dplyr::group_by(Speaker,syll.pos,c.place,sec.art) %>% 
  dplyr::summarize(result = Hmisc::smean.cl.boot(X.startnorm,B=10000,conf.int=0.95) %>% t %>% as.data.frame) %>% 
  tidyr::unnest(result)

detach(package:Hmisc) # Leads to wonky stuff with mean() and other functions apparently

nrow(bs.uncorr)
bs.uncorr<- bs.uncorr %>% mutate(result = case_when(
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",
  (Lower > 0 | Upper < 0) & Mean < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
  (Lower > 0 | Upper < 0) & Mean > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
  TRUE ~ "NS"
)
)


bs.uncorr.sig<-subset(bs.uncorr,Lower > 0 | Upper < 0)
nrow(bs.uncorr.sig)
print(arrange(bs.uncorr.sig,syll.pos,c.place,sec.art,Speaker),n=nrow(bs.uncorr.sig))

bs.uncorr %>% group_by(syll.pos,c.place,sec.art) %>% summarize(count=n())
bs.uncorr.sig %>% group_by(syll.pos,c.place,sec.art,result) %>% summarize(count=n())

# Summarize counts
c <- tp.startnorm %>% filter(frm.pos=="End") %>% group_by(Speaker,syll.pos,c.place,sec.art) %>% summarize(count=n())
c
range(c$count)
mean(c$count)
median(c$count)


#####################
# Bayesian t-tests by participant.

# If the HDI is completely outside the ROPE, the “null hypothesis” for this parameter is “rejected”. If the ROPE completely covers the HDI, i.e., all most credible values of a parameter are inside the region of practical equivalence, the null hypothesis is accepted. Else, it’s unclear whether the null hypothesis should be accepted or rejected.
#
#If the full ROPE is used (i.e., 100% of the HDI), then the null hypothesis is rejected or accepted if the percentage of the posterior within the ROPE is smaller than to 2.5% or greater than 97.5%. Desirable results are low proportions inside the ROPE (the closer to zero the better).

t.test.outputs <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(syll.pos,c.place,sec.art,frm.pos,Speaker) %>% do(describe_posterior(ttestBF(.$X.startnorm),ci = 1)) 

# t.test.outputs <- t.test.outputs %>% mutate(XXXXX = round(as.numeric(format(XXXX, scientific = FALSE)),5))

t.test.outputs

nrow(t.test.outputs)

pthresh <- 0.0500000000000000000001

# this conversion is only valid when the lowest possible values of pd is 0.5 - i.e., when the posterior represents continuous parameter space 
# https://easystats.github.io/bayestestR/reference/pd_to_p.html?q=p-val#null
min(t.test.outputs$pd)
t.test.outputs <- t.test.outputs %>% mutate(p.value = 1 -pd)

t.sig<-subset(t.test.outputs,p.value < pthresh)
nrow(t.sig)
arrange(t.sig,syll.pos,c.place,sec.art)

bayes.uncorr<- t.test.outputs %>% mutate(result = case_when(
  p.value < pthresh & Median > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
  p.value < pthresh & Median < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",
  p.value < pthresh & Median < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
  p.value < pthresh & Median > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
  p.value < pthresh & Median > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
  p.value < pthresh & Median < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",
  p.value < pthresh & Median < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
  p.value < pthresh & Median > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
  TRUE ~ "NS"
)
)

bayes.uncorr %>% group_by(syll.pos,c.place,sec.art) %>% summarize(count=n())
bayes.uncorr %>% group_by(syll.pos,c.place,sec.art,result) %>% summarize(count=n()) %>% filter(result!="NS")


#######################
# TO DO:
#######################
# - Add the labeling to the group plots indicating Rel/VC/C start, whatever, a la ICPhs?
# - Add text and table to the individual variation data.

######################
# Hide --- this was from ICPhS
######################

# #############
# # Now you might want to exclude midpoint data.
# # No midpoint data
# tp <- subset(tp,frm.pos != "C midpoint")
# 
# 
# # Subset the data
# tp.pal<-subset(tp,sec.art=="Cʲ")
# tp.vel<-subset(tp,sec.art=="Cˠ")
# 
# # Find the gestural peak for Cʲ and Cˠ separately.
# palmax <- tp.pal %>% group_by(Speaker,syll.pos,c.place,v,rep) %>% slice_max(order_by=X,n=1)
# velmin <- tp.vel %>% group_by(Speaker,syll.pos,c.place,v,rep) %>% slice_min(order_by=X,n=1)
# 
# tp.peaks<-rbind(palmax,velmin)
# 
# tp.peaks.recode<-tp.peaks
# 
# 
# # Plot counts of gestural peaks. 
# peaks.count<-ggplot(tp.peaks.recode,aes(x=frm.pos,y=sec.art,color=frm.pos))+
#   geom_count()+facet_grid(syll.pos~c.place)+ # geom_bar might be better?
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   scale_color_manual(values=colorSet)+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank())+
#   guides(color="none")+
#   ggtitle("Temporal landmark with most extreme backness value for each token")
# 
# peaks.count
# 
# # Save w/ cairo
# setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/Carnie_volume/")
# cairo_pdf(file="c_peak_counts.pdf",
#           width=14,height=8.5)
#   peaks.count
# dev.off()
# 
# 
# # Same plot, but with proportions rather than counts.
# # https://ggplot2.tidyverse.org/reference/geom_count.html
# peaks.prop<-ggplot(tp.peaks.recode,aes(x=frm.pos,y=sec.art,color=frm.pos,size = after_stat(prop), group = 1))+
#   geom_count()+facet_grid(syll.pos~c.place)+ # geom_bar might be better?
#   scale_size(range = c(1, 15))+
#   # scale_size_area()+
#   scale_color_manual(values=colorSet)+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank())+
#   guides(color="none")+
#   labs(size="Proportion\nin each cell")+
#   ggtitle("Temporal landmark with most extreme backness value for each token")
# 
# peaks.prop
# 
# 
# # Save w/ cairo
# cairo_pdf(file="c_peak_props.pdf",
#           width=14,height=8.5)
#   peaks.prop
# dev.off()
# 
# 
# # Do binomial mixed effects regression?
# nrow(tp.peaks.recode)
# 
# 
# 
# 
# 
# 
# #########################
# # A different approach: look at the magnitude of the difference between start vs. end
# # 
# tp.diffs <- tp %>% group_by(Speaker,syll.pos,c.place,v,sec.art,rep) %>% mutate(diff = X - lag(X, default = first(X), order_by = frm.pos)) %>% filter(frm.pos=="C end") %>% select(-c(frm.pos))
# 
# tp.diffs
# summary(tp.diffs)
# summary(tp.diffs$diff)
# 
# 
# peaks.diffs<-ggplot(tp.diffs,aes(x=sec.art,y=diff,color=sec.art))+
#   geom_hline(yintercept = 0,size=2,color="grey50")+
#   geom_point(alpha=0.65)+facet_grid(syll.pos~c.place)+
#   scale_color_manual(values=colorSet)+
#   theme_bw(base_size = 16)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank())+
#   guides(color="none")+
#   ggtitle("Tongue peak backness: C end - C start")+
#   coord_cartesian(ylim=c(-0.3,0.22))+
#   scale_y_continuous(breaks=seq(-0.5,0.5,0.1))
# 
# peaks.diffs
# 
# # Save w/ cairo
# cairo_pdf(file="c_peak_diffs.pdf",
#           width=14,height=8.5)
#   peaks.diffs
# dev.off()
# 
# 
# summary(tp.diffs$diff)
# 
# peaks.diffs.jitter<-ggplot(tp.diffs,aes(x=sec.art,y=diff,color=sec.art))+
#   geom_hline(yintercept = 0,size=2,color="grey50")+
#   geom_jitter(width=0.125,alpha=0.55)+
#   facet_grid(c.place~syll.pos)+
#     scale_color_manual(values=colorSet)+
#   theme_bw(base_size = 28)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title.x=element_blank(),
#         axis.title.y=element_text(size=18),
#         strip.text.y = element_text(angle=0),
#         axis.text.y = element_text(size=16))+
#   guides(color="none")+
#   # ggtitle("Tongue peak backness: C end - C start")+
#   coord_cartesian(ylim=c(-0.3,0.22))+
#   scale_y_continuous(breaks=seq(-0.5,0.5,0.1))+
#   ylab("Backness of dorsal peak: C release - C start")
# 
# peaks.diffs.jitter
# 
# 
# ############### 
# # Run some t-tests
# t.test.outputs <- tp.diffs %>% group_by(syll.pos,c.place,sec.art) %>% do(tidy(t.test(.$diff))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))
# t.test.outputs
# pthresh <- 0.0500000000000000000001/12
# subset(t.test.outputs,p.value < pthresh)
# 
# 
# # Add p-values
# t.test.outputs <-t.test.outputs %>% mutate(plabs = as.character(case_when(p.value < pthresh & estimate > 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "Rel",
#                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cʲ" & syll.pos == "Onset" ~ "C start",                                                                   p.value < pthresh & estimate > 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "Rel",
#                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cʲ" & syll.pos == "Coda" ~ "VC",
# 
#                                                                           p.value < pthresh & estimate > 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "C start",
#                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cˠ" & syll.pos == "Onset" ~ "Rel",
#                                                                           p.value < pthresh & estimate > 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "VC",
#                                                                           p.value < pthresh & estimate < 0 & sec.art == "Cˠ" & syll.pos == "Coda" ~ "Rel",
#                                                                           TRUE ~ ""
# )))
# 
# # t.test.outputs <-t.test.outputs %>% mutate(plabs = as.character(case_when(p.value < pthresh & estimate > 0 ~ "+",
# #                                                                   p.value < pthresh & estimate < 0 ~ "\u2212",
# #                                                                   TRUE ~ ""
#                                                                     # )))
# t.test.outputs
# 
# 
# 
# peaks.diffs.jitter<-peaks.diffs.jitter+geom_label(data=t.test.outputs %>% filter(plabs!=""),aes(label=plabs,x=sec.art,y=0.18),nudge_x=-0.35,size=5.25)
# peaks.diffs.jitter
# 
# 
# 
# # # Do pairwise variability checks across syllable position
# # bf<-tp.diffs %>% group_by(c.place,sec.art) %>% do(tidy(leveneTest(diff~syll.pos,data=.,na.rm=T)))
# # bf
# # 
# # bf <- bf %>% mutate(plabs = as.character(case_when(p.value < pthresh * 2 ~ "variance \u2260",
# #                                                   TRUE ~ ""))) 
# # 
# 
# # # Plot...if you think this is the right way to think about variance...
# # peaks.diffs.jitter <- peaks.diffs.jitter+geom_label(data=bf %>% filter(plabs!="") ,aes(x=1.5,y=0,label=plabs),size=3,inherit.aes = F)
# # peaks.diffs.jitter
# 
# 
# 
# # Explore kurtosis
# # https://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/Simon
# kurtosis <- tp.diffs %>% group_by(c.place,sec.art,syll.pos) %>% summarize(kurt = kurtosis(diff))
# kurtosis$excess.kurt<-kurtosis$kurt-3 # Positive values indicate more data in the tails, and so a wider spread than normal distribution  (≈ more spreading). Negative values indicate less data in the tails (≈ more clumping)
# kurtosis
# kurtosis %>% filter(abs(excess.kurt) > 2)
# 
# 
# # Test for skewness + kurtosis values signficantly different from normal = 3
# # https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test
# kurt.tests<-tp.diffs %>% group_by(c.place,sec.art,syll.pos) %>% do(tidy(jarque.test(x=.$diff)))
# kurt.tests %>% filter(p.value<pthresh)
# # kurt.tests %>% filter(p.value<0.0500000000000000000000001)
# 
# # https://en.wikipedia.org/wiki/D%27Agostino%27s_K-squared_test
# ag.tests<-tp.diffs %>% group_by(c.place,sec.art,syll.pos) %>% do(tidy(agostino.test(x=.$diff)))
# ag.tests %>% filter(p.value<pthresh)
# # ag.tests %>% filter(p.value<0.0500000000000000000000001)
# 
# # https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
# sw.tests<-tp.diffs %>% group_by(c.place,sec.art,syll.pos) %>% do(tidy(shapiro.test(x=.$diff)))
# sw.tests %>% filter(p.value<pthresh)
# # sw.tests %>% filter(p.value<0.0500000000000000000000001)
# 
# 
# # Test for bimodality with Hartigan's dip test
# # https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test
# dip.tests<-tp.diffs %>% group_by(c.place,sec.art,syll.pos) %>% do(tidy(dip.test(.$diff)))
# dip.tests %>% filter(p.value<pthresh)
# # sw.tests %>% filter(p.value<0.0500000000000000000000001)
# 
# 
# # Save w/ cairo
# cairo_pdf(file="c_peak_diffs_jitter.pdf",
#           width=6,height=7)
#   peaks.diffs.jitter
# dev.off()
# 
# 
# 
# 
# 
# ###############
# # By v height
# 
# peaks.diffs.jitter.height<-ggplot(tp.diffs,aes(x=syll.pos,y=diff,color=syll.pos))+
#   geom_hline(yintercept = 0,size=2,color="grey50")+
#   geom_jitter(width=0.125,alpha=0.55)+
#   facet_grid(c.place~sec.art+v.height)+
#   scale_color_manual(values=colorSet)+
#   theme_bw(base_size = 28)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank(),
#         strip.text.y = element_text(angle=0),
#         axis.text.y = element_text(size=16))+
#   guides(color="none")+
#   # ggtitle("Tongue peak backness: C end - C start")+
#   coord_cartesian(ylim=c(-0.3,0.22))+
#   scale_y_continuous(breaks=seq(-0.5,0.5,0.1))
# 
# peaks.diffs.jitter.height
# 
# 
# ############### 
# # Run some t-tests
# t.test.outputs <- tp.diffs %>% group_by(syll.pos,c.place,sec.art,v.height) %>% do(tidy(t.test(.$diff))) %>% mutate(p.value = round(as.numeric(format(p.value, scientific = FALSE)),5))
# t.test.outputs
# pthresh <- 0.0500000000000000000001/12
# subset(t.test.outputs,p.value < pthresh)
# 
# 
# # Add p-values
# t.test.outputs <-t.test.outputs %>% mutate(plabs = as.character(case_when(p.value < pthresh & estimate > 0 ~ "+",
#                                                                           p.value < pthresh & estimate < 0 ~ "\u2212",
#                                                                           TRUE ~ ""
# )))
# t.test.outputs
# 
# 
# 
# peaks.diffs.jitter.height<-peaks.diffs.jitter.height+geom_label(data=t.test.outputs %>% filter(plabs!=""),aes(label=plabs,x=syll.pos,y=0.18),nudge_x=-0.35,size=8)
# peaks.diffs.jitter.height
# 
# 
# 
# 
# #########
# 
# 
# # Save w/ cairo
# cairo_pdf(file="c_peak_diffs_jitter_byv.pdf",
#           width=10,height=7)
#   peaks.diffs.jitter.height
# dev.off()
# 
# 
# 
# #############
# peaks.diffs.jitter.wav<-ggplot(tp.diffs,aes(x=diff,fill=syll.pos))+
#   geom_hline(yintercept = 0,size=2,color="grey50")+
#   geom_density(alpha=0.55)+
#   facet_grid(c.place~sec.art+syll.pos)+
#   scale_color_manual(values=colorSet)+
#   theme_bw(base_size = 14)+
#   theme(strip.text = element_text(face = "bold"),
#         plot.title = element_text(size = 16),
#         axis.title=element_blank(),
#         strip.text.y = element_text(angle=0),
#         axis.text.y = element_text(size=16))+
#   guides(color="none")
# 
#   # ggtitle("Tongue peak backness: C end - C start")+
#   # coord_cartesian(ylim=c(-0.3,0.22))+
#   #scale_y_continuous(breaks=seq(-0.5,0.5,0.1))
# 
# 
# peaks.diffs.jitter.wav
# 
# # Save w/ cairo
# cairo_pdf(file="c_peak_diffs_density.pdf",
#           width=10,height=7)
#   peaks.diffs.jitter.wav
# dev.off()
# 
#