library(tidyverse)


# Where are the .Rdata files you're loading?
computer<-""
setwd(paste0("C:/Users/",computer,""))

# To reproduce our procedure for finding peak dorsal points, do the following.
# load("edgetrak_output_normalized_Irish_JPhon.RData")

# Tongue.peaks <- edgetrak.minmax %>% group_by(Speaker,c.place,v,sec.art,syll.pos,rep,frm.pos) %>%
#                                     # Lower Y values are physically higher, so we look for minimum values.
#                                     slice_min(Y) %>%
#                                     summarize(X = mean(X), # Take mean X value in case of ties/plateaus
#                                               Y = unique(Y)) %>% 
#                                     ungroup()



# Read in data frame of peak dorsal points, which is what we actually analyze.
load("tongue_peaks_Irish_JPhon.RData")

# Check factor types, and correct if needed.
lapply(Tongue.peaks,is)

# For Munster 2, it looks like the highest point of the tongue is at the tongue tip instead of the tongue body for [ta], [tu] and [at]. And maybe some others. So we exclude it to be safe.
dorsal.peaks.filtered <- Tongue.peaks %>% filter(!(Speaker == "Munster, Speaker S2" & c.place=="coronal"))

summary(Tongue.peaks$Speaker)
summary(dorsal.peaks.filtered$Speaker)

summary(dorsal.peaks.filtered)


# Change factor names if you want
dorsal.peaks.filtered$c.place <- dorsal.peaks.filtered$c.place %>% fct_recode("Labial" = "labial",
                                                                    "Coronal" = "coronal",
                                                                    "Dorsal" = "dorsal")

dorsal.peaks.filtered$sec.art <- dorsal.peaks.filtered$sec.art %>% fct_recode("Cˠ" = "Vel",
                                                                              "Cʲ" = "Pal")

dorsal.peaks.filtered$v <- dorsal.peaks.filtered$v %>% fct_recode("i\u02D0" = "i",
                                                                  "u\u02D0" = "u",
                                                                  "\u0254\u02D0" = "a")

summary(dorsal.peaks.filtered)
head(dorsal.peaks.filtered)


# Recode transitions
dorsal.peaks.filtered<-dorsal.peaks.filtered %>% mutate(trans = case_when(syll.pos == "Coda" & frm.pos == "start" ~ "C release/VC transition",
                                                                syll.pos == "Onset" & frm.pos == "end" ~ "C release/VC transition",
                                                                TRUE ~ "no"))


dorsal.peaks.filtered$trans<-as.factor(dorsal.peaks.filtered$trans)

transpoints.backness<-subset(dorsal.peaks.filtered,trans=="C release/VC transition")
summary(transpoints.backness)

# Get counts
countlabels <- transpoints.backness %>% group_by(c.place,syll.pos,sec.art) %>% summarise(N=n())


# Too much of a hassle...it works, but isn't any better than nested faceting.
# # Try it with fill
# dorsal.peaks.plot.faceted<-ggplot(data=transpoints.backness,
#                                   aes(y=X,x=syll.pos,group=interaction(sec.art,syll.pos)))+
#   geom_violin(aes(fill=sec.art),position = position_dodge(width = 0.9))+
#   geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white",position = position_dodge(width = 0.9))+
#   stat_summary(fun.y="median",geom='point',size=6,pch=18,position = position_dodge(width = 0.9))+
#   stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
#                 geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black",position = position_dodge(width = 0.9))+
#   geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos,group=interaction(sec.art,syll.pos),y=0),color="black",size=6,fontface= "bold",position = position_dodge(width = 0.9)) +
#   facet_grid(trans~c.place)+
#   ggtitle("Fronting of dorsal peak for each raw tracing")+
#   ylab("Fronting in normalized [0,1] units")+xlab("Syllable position")+
#   # scale_color_manual(values=CbbPalette)+
#   theme_bw(base_size=32)+
#   theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
#         plot.title = element_text(size = 32),
#         axis.title = element_text(size=32),
#         legend.key.width=unit(5,"line"),
#         legend.key.height=unit(1.25,"line"),
#         legend.title = element_blank(),
#         legend.text = element_text(size=28,face="bold"
#                                    #family = "Doulos SIL"
#         ))+
#   scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))
# 
# dorsal.peaks.plot.faceted


# Try it with faceting
dorsal.peaks.plot.faceted<-ggplot(data=transpoints.backness,
                                  aes(y=X,x=syll.pos))+
  geom_violin(aes(fill=syll.pos),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos,y=0),color="black",size=10,fontface= "bold") +
  facet_grid(trans~c.place*sec.art)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=32)+
  theme(strip.text.x = element_text(family = "Doulos SIL",size=36,face = "bold"),
        strip.text.y = element_text(family = "Doulos SIL",size=28,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted


cairo_pdf(file="peak_backness_place.pdf",
          width=16,height=8)
  print(dorsal.peaks.plot.faceted)
dev.off()


# By speaker
# Temporarily re-label speakers
transpoints.backness.relab<-transpoints.backness

transpoints.backness.relab$Speaker <- transpoints.backness.relab$Speaker %>% fct_recode("S1.Connacht" = "Connacht, Speaker S1",
                                                                                "S2.Connacht" = "Connacht, Speaker S2",
                                                                                "S3.Connacht" = "Connacht, Speaker S3",
                                                                                "S1.Munster" = "Munster, Speaker S1",
                                                                                "S2.Munster" = "Munster, Speaker S2",
                                                                                "S3.Munster" = "Munster, Speaker S3",
                                                                                "S1.Ulster" = "Ulster, Speaker S1",
                                                                                "S2.Ulster" = "Ulster, Speaker S2",
                                                                                "S3.Ulster" = "Ulster, Speaker S3"
                                                                                )


countlabels <- transpoints.backness.relab %>% group_by(c.place,syll.pos,sec.art,Speaker) %>% summarise(N=n())

dorsal.peaks.plot.faceted.byspkr<-ggplot(data=transpoints.backness.relab,
                                  aes(y=X,x=syll.pos))+
  geom_violin(aes(fill=syll.pos),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos,y=0.005),color="black",size=6,fontface= "bold") +
  facet_grid(Speaker~c.place*sec.art)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=22,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted.byspkr


cairo_pdf(file="peak_backness_place_byspeaker.pdf",
          width=16,height=18)
print(dorsal.peaks.plot.faceted.byspkr)
dev.off()




# Try it with less faceting
countlabels <- transpoints.backness %>% group_by(syll.pos,sec.art) %>% summarise(N=n())
dorsal.peaks.plot.faceted.simple<-ggplot(data=transpoints.backness,
                                  aes(y=X,x=syll.pos))+
  geom_violin(aes(fill=syll.pos),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos,y=0),color="black",size=6,fontface= "bold") +
  facet_grid(trans~sec.art)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted.simple


# Check v context
countlabels <- transpoints.backness %>% group_by(syll.pos,sec.art,v) %>% summarise(N=n())

dorsal.peaks.plot.faceted.vcontext<-ggplot(data=transpoints.backness,
                                  aes(y=X,x=v))+
  geom_violin(aes(fill=v),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=v,y=0),color="black",size=10,fontface= "bold") +
  facet_grid(trans~sec.art*syll.pos)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Vowel context")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=36)+
  theme(strip.text.x = element_text(family = "Doulos SIL",size=36,face = "bold"),
        strip.text.y = element_text(family = "Doulos SIL",size=28,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted.vcontext

cairo_pdf(file="peak_backness_vcontext.pdf",
          width=16,height=8)
  print(dorsal.peaks.plot.faceted.vcontext)
dev.off()


# Check v context by place
countlabels <- transpoints.backness %>% group_by(syll.pos,sec.art,v,c.place) %>% summarise(N=n())

dorsal.peaks.plot.faceted.vcontext.place<-ggplot(data=transpoints.backness,
                                           aes(y=X,x=v))+
  geom_violin(aes(fill=v),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=v,y=0.05),color="black",size=10,fontface= "bold") +
  facet_grid(c.place~sec.art*syll.pos)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=32)+
  theme(strip.text = element_text(family = "Doulos SIL",size=36,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted.vcontext.place

cairo_pdf(file="peak_backness_vcontext_place.pdf",
          width=16,height=14)
  print(dorsal.peaks.plot.faceted.vcontext.place)
dev.off()



##########
# Run a regression.
library(lmerTest)
library(emmeans)
library(performance) # For model statistics

# Add voicing back in as a factor.
transpoints.backness <- transpoints.backness %>% mutate(voicing = case_when(
                        sec.art=="Cˠ" & c.place == "Labial" & v == "iː" & syll.pos=="Onset" ~ "voiced",
                        #
                        sec.art=="Cˠ" & c.place == "Labial" & v == "iː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cˠ" & c.place == "Coronal" & v == "iː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cˠ" & c.place == "Labial" & v == "uː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cˠ" & c.place == "Coronal" & v == "uː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cˠ" & c.place == "Labial" & v == "ɔː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cˠ" & c.place == "Dorsal" & v == "ɔː" & syll.pos=="Coda" ~ "voiced",
                        #
                        #
                        sec.art=="Cʲ" & c.place == "Coronal" & v == "iː" & syll.pos=="Onset" ~ "voiced",
                        sec.art=="Cʲ" & c.place == "Labial" & v == "uː" & syll.pos=="Onset" ~ "voiced",
                        #
                        sec.art=="Cʲ" & c.place == "Labial" & v == "iː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cʲ" & c.place == "Coronal" & v == "iː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cʲ" & c.place == "Labial" & v == "uː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cʲ" & c.place == "Coronal" & v == "uː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cʲ" & c.place == "Labial" & v == "ɔː" & syll.pos=="Coda" ~ "voiced",
                        sec.art=="Cʲ" & c.place == "Dorsal" & v == "ɔː" & syll.pos=="Coda" ~ "voiced",
                        .default = "voiceless"
                      )
                      )

transpoints.backness$voicing<-factor(transpoints.backness$voicing)

# Order factors to reflect desired reference levels.
transpoints.backness$c.place <- transpoints.backness$c.place %>% fct_relevel("Labial", after = length(transpoints.backness$c.place))
transpoints.backness$v <- transpoints.backness$v %>% fct_relevel("i\u02D0", after = length(transpoints.backness$v))
transpoints.backness$syll.pos <- transpoints.backness$syll.pos %>% fct_relevel("Onset", after = length(transpoints.backness$syll.pos))
transpoints.backness$sec.art <- transpoints.backness$sec.art %>% fct_relevel("Cʲ", after = length(transpoints.backness$sec.art))
transpoints.backness$voicing <- transpoints.backness$voicing %>% fct_relevel("voiceless", after = length(transpoints.backness$voicing))

# Sum code factors to reduce collinearity.
contrasts(transpoints.backness$c.place)
contrasts(transpoints.backness$c.place)<-contr.sum(length(levels(transpoints.backness$c.place))) # Sum coding
contrasts(transpoints.backness$c.place)

contrasts(transpoints.backness$v)
contrasts(transpoints.backness$v)<-contr.sum(length(levels(transpoints.backness$v))) # Sum coding
contrasts(transpoints.backness$v)

contrasts(transpoints.backness$syll.pos)
contrasts(transpoints.backness$syll.pos)<-contr.sum(length(levels(transpoints.backness$syll.pos))) # Sum coding
contrasts(transpoints.backness$syll.pos)

contrasts(transpoints.backness$sec.art)
contrasts(transpoints.backness$sec.art)<-contr.sum(length(levels(transpoints.backness$sec.art))) # Sum coding
contrasts(transpoints.backness$sec.art)

contrasts(transpoints.backness$voicing)
contrasts(transpoints.backness$voicing)<-contr.sum(length(levels(transpoints.backness$voicing))) # Sum coding
contrasts(transpoints.backness$voicing)

# It doesn't make any sense to add item as a random effect, because you have one item/condition,
# but you can play around with it if you like.
# transpoints.backness <- transpoints.backness %>% mutate(item=paste0(Speaker,syll.pos,c.place,v,sec.art,voicing))


# Center and scale repetition
transpoints.backness$rep.scaled <-scale(as.numeric(transpoints.backness$rep))


# Fit a mixed effects model
base.m<-lmer(data = transpoints.backness, X ~ (c.place + syll.pos + v + sec.art)^2 + 
                                               sec.art:syll.pos:c.place +
                                               (rep.scaled + sec.art)^2 +
                                               voicing + 
                                               (1+syll.pos|Speaker),
             REML = F)
# Use REML=F for log-likelihood comparison in step-down model reduction.
# Adding (1+c.place|speaker) or (1+v|speaker) leads to failures in model convergence.

anova(base.m)
performance::check_collinearity(base.m)  # Low collinearity

# Model reduction
an.rev<-function(m){anova(m)[order(anova(m)[5]),]} # Shorthand for sorting anova by F-value (increasing)
# coef(summary(base.m))[order(abs(coef(summary(base.m))[,1]),decreasing=T),]

an.rev(base.m)
m.r1<-update(base.m,~.-c.place:syll.pos:sec.art) # Dropped
performance::test_performance(base.m,m.r1)

an.rev(m.r1)
m.r2<-update(m.r1,~.-rep.scaled:sec.art) # Dropped
performance::test_performance(base.m,m.r1,m.r2)

an.rev(m.r2)
m.r3<-update(m.r2,~.-rep.scaled) # Dropped
performance::test_performance(base.m,m.r1,m.r2,m.r3)

an.rev(m.r3)
m.r4<-update(m.r3,~.-voicing) # Significant
performance::test_performance(base.m,m.r1,m.r2,m.r3,m.r4)

an.rev(m.r3)
m.r4<-update(m.r3,~.-v:sec.art) # Significant
performance::test_performance(base.m,m.r1,m.r2,m.r3,m.r4)

# Store final model
fm <- m.r3
performance::check_collinearity(fm) # Low collinearity...but *slightly* wide CI for syll.pos:sec.art.


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

# Get model summary
anova(fm)
summary(transpoints.backness$X)
sd(transpoints.backness$X)

library(xtable)
res.table <- as.data.frame(round(coef(summary(fm)),6))
res.table

# Replace column names
res.table <- res.table%>%rownames_to_column()
colnames(res.table)<-c("Predictor","Estimate","SE","df","t","p.numeric")

# Replace predictor values with more informative names
# Coding and reference levels
contrasts(transpoints.backness$c.place)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'c.place1', 'Coronal'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'c.place2', 'Dorsal'))
res.table

contrasts(transpoints.backness$syll.pos)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'syll.pos1', 'Coda'))
res.table

contrasts(transpoints.backness$v)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v1', '\\ipa{[u:]}'))
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'v2', '\\ipa{[O:]}'))
res.table

contrasts(transpoints.backness$sec.art)
res.table
res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'sec.art1', '/C\\vel/'))
res.table

res.table <- res.table %>%  mutate(across('Predictor', str_replace, 'voicing1', 'Voiced'))
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
secart.emm <- emmeans(fm,"sec.art")
secart.emm
contrast(secart.emm)
pairs(secart.emm)

v.emm <- emmeans(fm,"v")
v.emm
contrast(v.emm)
pairs(v.emm)


# Interaction terms (2-way)
cplace.secart.int.emm <- emmeans(fm,c("c.place","sec.art"))
cplace.secart.int.emm
contrast(cplace.secart.int.emm)
pairs(cplace.secart.int.emm,by="c.place") # Pairwise tests by group.
pairs(cplace.secart.int.emm,by="sec.art")

syllpos.secart.int.emm <- emmeans(fm,c("syll.pos","sec.art"))
syllpos.secart.int.emm
contrast(syllpos.secart.int.emm)
pairs(syllpos.secart.int.emm,by="sec.art") # Pairwise tests by group.

v.secart.emm <- emmeans(fm,c("v","sec.art"))
v.secart.emm
contrast(v.secart.emm)
pairs(v.secart.emm,by="sec.art")

v.syllpos.emm <- emmeans(fm,c("v","syll.pos"))
v.syllpos.emm
contrast(v.syllpos.emm)
pairs(v.syllpos.emm,by="syll.pos")

# More complex groupings
syllpos.secart.cplace.int.emm <- emmeans(fm,c("syll.pos","c.place","sec.art"))
syllpos.secart.cplace.int.emm
contrast(syllpos.secart.cplace.int.emm)
pairs(syllpos.secart.cplace.int.emm,by=c("c.place","sec.art"))

syllpos.secart.v.int.emm <- emmeans(fm,c("sec.art","syll.pos","v"))
syllpos.secart.v.int.emm
contrast(syllpos.secart.v.int.emm)
pairs(syllpos.secart.v.int.emm,by=c("syll.pos","sec.art"))
pairs(syllpos.secart.v.int.emm,by=c("v","sec.art"))

# Note how emmeans groups/branches from right-to-left rather than expected left-to-right
emmeans(fm,c("v","sec.art","syll.pos"))
contrast(emmeans(fm,c("v","sec.art","syll.pos")))


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

transpoints.backness %>% group_by(sec.art) %>% summarize(SD=sd(X),
                                                         mean=mean(X))
hov(X~sec.art, data=transpoints.backness)


transpoints.backness %>% group_by(sec.art,c.place) %>% summarize(SD=sd(X),
                                                                 mean=mean(X))

subset(transpoints.backness,c.place!="Coronal") %>% group_by(sec.art) %>% do(tidy(hov(X~c.place, data=.)))
subset(transpoints.backness,c.place!="Dorsal") %>% group_by(sec.art) %>% do(tidy(hov(X~c.place, data=.)))
subset(transpoints.backness,c.place!="Labial") %>% group_by(sec.art) %>% do(tidy(hov(X~c.place, data=.)))


transpoints.backness %>% filter(syll.pos=="Coda") %>% group_by(sec.art) %>% summarize(SD=sd(X),
                                                                 mean=mean(X))

transpoints.backness %>% filter(syll.pos=="Coda") %>% hov(X~sec.art, data=.)

detach(package:HH)
detach(package:broom)



###################
# Re-make some plots looking at C releases rather than C release (onset) vs. C start (coda).
###################
# Recode transitions
dorsal.peaks.filtered<-dorsal.peaks.filtered %>% mutate(trans = case_when(frm.pos == "end" ~ "C release",
                                                                          TRUE ~ "no"))

dorsal.peaks.filtered$trans<-as.factor(dorsal.peaks.filtered$trans)

transpoints.backness<-subset(dorsal.peaks.filtered,trans=="C release")
summary(transpoints.backness)

# Get counts
countlabels <- transpoints.backness %>% group_by(c.place,syll.pos,sec.art) %>% summarise(N=n())

dorsal.peaks.plot.faceted.rel<-ggplot(data=transpoints.backness,
                                  aes(y=X,x=syll.pos))+
  geom_violin(aes(fill=syll.pos),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=syll.pos,y=0),color="black",size=10,fontface= "bold") +
  facet_grid(trans~c.place*sec.art)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Syllable position")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=32)+
  theme(strip.text.x = element_text(family = "Doulos SIL",size=36,face = "bold"),
        strip.text.y = element_text(family = "Doulos SIL",size=28,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted.rel

cairo_pdf(file="peak_backness_place_release.pdf",
          width=16,height=8)
print(dorsal.peaks.plot.faceted.rel)
dev.off()



countlabels <- transpoints.backness %>% group_by(syll.pos,sec.art,v) %>% summarise(N=n())

dorsal.peaks.plot.faceted.vcontext.rel<-ggplot(data=transpoints.backness,
                                           aes(y=X,x=v))+
  geom_violin(aes(fill=v),show.legend = F)+
  geom_boxplot(width=0.2,color="black",coef=0,outlier.colour = "red",outlier.shape=NA,fill="white")+
  stat_summary(fun.y="median",geom='point',size=6,pch=18)+
  stat_summary(fun.y="mean",fun.min="mean",fun.max="mean",
               geom="crossbar",width=0.75,lwd=.4,lty="solid",col="black")+
  geom_text(inherit.aes=F,data=countlabels,aes(label=N,x=v,y=0),color="black",size=10,fontface= "bold") +
  facet_grid(trans~sec.art*syll.pos)+
  ggtitle("Fronting of dorsal peak for each raw tracing")+
  ylab("Fronting in normalized [0,1] units")+xlab("Vowel context")+
  # scale_color_manual(values=CbbPalette)+
  theme_bw(base_size=36)+
  theme(strip.text.x = element_text(family = "Doulos SIL",size=36,face = "bold"),
        strip.text.y = element_text(family = "Doulos SIL",size=28,face = "bold"),
        plot.title = element_text(size = 32),
        axis.title = element_text(size=32),
        legend.key.width=unit(5,"line"),
        legend.key.height=unit(1.25,"line"),
        legend.title = element_blank(),
        legend.text = element_text(size=28,face="bold"
                                   #family = "Doulos SIL"
        ))+
  scale_y_continuous(limits = c(0,0.8),breaks=seq(0,0.8,0.2))

dorsal.peaks.plot.faceted.vcontext.rel

cairo_pdf(file="peak_backness_vcontext_release.pdf",
          width=16,height=8)
print(dorsal.peaks.plot.faceted.vcontext.rel)
dev.off()