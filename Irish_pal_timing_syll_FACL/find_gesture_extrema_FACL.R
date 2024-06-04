library(car)
library(tidyverse)
library(broom) # For tidy()

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
#
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

# For Munster 2, it looks like the highest point of the tongue is at the tongue tip instead of the tongue body for [ta], [tu] and [at]. And maybe some others. So we exclude it to be safe.
tp.raw <- Tongue.peaks %>% filter(!(Speaker == "Munster, Speaker S2" & c.place=="coronal"))

head(tp.raw)
nrow(tp.raw)


# Rename some factor values.
tp.raw$sec.art<-factor(tp.raw$sec.art,levels=c("Pal","Vel"))
tp.raw$sec.art<-dplyr::recode(tp.raw$sec.art,Pal = "Cʲ",Vel="Cˠ")
tp.raw$frm.pos<-dplyr::recode(tp.raw$frm.pos,start = "C start",end="C end",midpoint = "C midpoint")
tp.raw$c.place<-dplyr::recode(tp.raw$c.place,labial = "P",coronal="T",dorsal="K")

tp <- tp.raw

tp %>% group_by(Speaker,c.place) %>% summarize(count=n())

# Add token-level identifier
tp <- tp %>% mutate(token = paste0(Speaker,"-",syll.pos,"-",c.place,"-",v,"-",sec.art,"-",rep))
 
tp.means<-tp %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% summarize(mean = mean(X))

# Add vowel height coding
tp <- tp %>% mutate(v.height = case_when(v == "a" ~ "/ɔː/",
                                          TRUE ~ "/iː uː/"))

tp$v.height<-factor(tp$v.height)



#######################################
# Look at trajectories, but subtract out starting value for X

# Remove tokens that don't have a start value
tp.startnorm <- tp %>% group_by(token) %>% filter("C start" %in% frm.pos)
nrow(tp)
nrow(tp.startnorm)

tp.startnorm <- tp.startnorm %>% group_by(token) %>% mutate(X.startnorm = X-X[frm.pos=="C start"])

tp.startnorm$frm.pos <- recode(tp.startnorm$frm.pos,"C start" = "Start", "C midpoint" = "Mid", "C end" = "End")

tp.means<-tp.startnorm %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% summarize(mean = mean(X.startnorm))



TB.back.by.landmark<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm,color=frm.pos))+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_violin()+
  geom_boxplot(width=0.25)+
  geom_jitter(alpha=0.2,width=0.25)+
  geom_point(data = tp.means, 
             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1.25,alpha=0.8)+
  geom_line(data = tp.means, 
            mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",linewidth=1.25,alpha=0.8)+
  facet_grid(syll.pos~c.place + sec.art)+
  scale_size(range = c(1, 15))+
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


TB.back.by.landmark.reps<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm))+
  geom_line(aes(group=token),color="gray75",alpha=0.5)+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_point(data = tp.means, 
             mapping = aes(x = frm.pos, y = mean, group=interaction(syll.pos,c.place,sec.art)),color="black",size=1.25,alpha=0.8)+
  geom_smooth(method="loess",data=tp.startnorm,aes(x=frm.pos,y=X.startnorm,group=interaction(syll.pos,c.place,sec.art)),color="black",size=1,alpha=0.9,fill="cornflowerblue")+
  facet_grid(syll.pos~c.place + sec.art)+
  scale_size(range = c(1, 15))+
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



TB.back.by.landmark.reps.spk<-ggplot(tp.startnorm,aes(x=frm.pos,y=X.startnorm))+
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X.startnorm),inherit.aes = F,se=F)+
  facet_grid(syll.pos~c.place + sec.art)+
  scale_size(range = c(1, 15))+
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



#######################################
# QUESTION:IS THIS REDUNDANT GIVEN FOLLOW-UP EMMEANS() TESTS?
#######################################

###############
# Do a grouped t-test, just looking at C end values.
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
library(BayesFactor)
library(bayestestR)
ttestBF(tp.startnorm$X.startnorm)
describe_posterior(ttestBF(tp.startnorm$X.startnorm),ci = 1)

# If the HDI is completely outside the ROPE, the “null hypothesis” for this parameter is “rejected”. If the ROPE completely covers the HDI, i.e., all most credible values of a parameter are inside the region of practical equivalence, the null hypothesis is accepted. Else, it’s unclear whether the null hypothesis should be accepted or rejected.
#
#If the full ROPE is used (i.e., 100% of the HDI), then the null hypothesis is rejected or accepted if the percentage of the posterior within the ROPE is smaller than to 2.5% or greater than 97.5%. Desirable results are low proportions inside the ROPE (the closer to zero the better).

t.test.outputs <- tp.startnorm %>% filter(frm.pos == "End") %>% group_by(syll.pos,c.place,sec.art,frm.pos) %>% do(describe_posterior(ttestBF(.$X.startnorm),ci = 1)) 

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
  geom_hline(aes(yintercept=0.0),color="grey20",lwd=1,lty="dashed",alpha=0.8)+
  geom_smooth(method="loess",aes(color=Speaker,group=Speaker,x=frm.pos,y=X.startnorm),inherit.aes = F,se=F)+
  facet_grid(syll.pos ~ sec.art)+
  scale_size(range = c(1, 15))+
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




##########
# Run a regression.
library(lmerTest)
library(emmeans)
library(performance) # For model statistics


# Data: tp.startnorm
lmerdata <- subset(tp.startnorm,frm.pos == "End")
lmerdata <- droplevels(lmerdata)

# Some summary statistics
lmerdata %>% ungroup() %>%
  summarize(mean = mean(X.startnorm,na.rm = T),
            median = median(X.startnorm,na.rm = T),
            range = paste0("[",round(range(X.startnorm,na.rm=T)[1],6),",",round(range(X.startnorm,na.rm=T)[2],6),"]"),
            drange = diff(range(X.startnorm,na.rm=T)),
            sd = sd(X.startnorm,na.rm = T),
            iq = paste0("[",round(summary(X.startnorm)[2],6),",",round(summary(X.startnorm)[5],6),"]"),
            diq = IQR(X.startnorm,na.rm=T)
  )



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


base.m<-lmer(data = lmerdata, X.startnorm ~ (c.place + syll.pos + sec.art)^3 + (c.place + syll.pos + v + sec.art)^2 + (1+syll.pos|Speaker),REML = F) # Use REML=F for log-likelihood comparison in step-down model reduction.

# Model reduction
# Reduce fixed-effects predictors
an.rev<-function(m){anova(m)[order(anova(m)[5]),]} # Shorthand for sorting anova by F-value (increasing)
# coef(summary(base.m))[order(abs(coef(summary(base.m))[,1]),decreasing=T),]

an.rev(base.m)
m.r1<-update(base.m,~.-c.place:syll.pos:sec.art)
anova(base.m,m.r1) # Significant

m.r1<-update(base.m,~.-sec.art:v)
anova(base.m,m.r1) # Significant

m.r1<-update(base.m,~.-c.place:v)
anova(base.m,m.r1) # Significant

m.r1<-update(base.m,~.-syll.pos:v)
anova(base.m,m.r1) # Significant

# So we're stuck with the full model!

# Store final model
fm <- base.m
performance::check_collinearity(fm) # Low collinearity...but *very* wide CIs in some cases (syll.pos:v,sec.art:v)


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
contrasts(lmerdata$c.place)
contrasts(lmerdata$syll.pos)
contrasts(lmerdata$v)

# Get model summary
anova(fm)
summary(lmerdata$X)
sd(lmerdata$X)

library(xtable)
res.table <- as.data.frame(round(coef(summary(fm)),6))
res.table

# Replace column names
res.table <- res.table%>%rownames_to_column()
colnames(res.table)<-c("Predictor","Estimate","SE","df","t","p.numeric")

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
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v1', '\\textnormal{\\ipa{/u:/}}'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v2', '\\textnormal{\\ipa{/ɔ:/}}'))
res.table

contrasts(lmerdata$sec.art)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'sec.art1', '\\textnormal{\\ipa{/Cˠ/}}'))
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



###############
# Do grouped t-tests,
# looking at C end values...for each participant.
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