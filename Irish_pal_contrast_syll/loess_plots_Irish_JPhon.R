setwd("C:/Users/Tiamat/Dropbox/Research/Irish/Irish_ultrasound_shared/Scripts/R scripts/HISPhonCog/")

# Load the range normalized data we actually analyze.
load("edgetrak_output_normalized_Irish_JPhon.RData")

# If you'd like to load the raw data, you can do that here:
# load("raw_EdgeTrak_data_JPhon.RData")


##################
# Load packages
##################

library(tidyverse)
library(ggplot2)
library(data.table)


##################
# Range normalization of raw data
##################
# edgetrak.minmax <- raw.EdgeTrak.data %>% group_by(Speaker) %>% mutate(Y = (Y-min(Y,na.rm=T))/(max(X,na.rm=T)-min(X,na.rm=T)),
#                                                                       X = (X-min(X,na.rm=T))/(max(X,na.rm=T)-min(X,na.rm=T))
#                                                                       ) %>% ungroup()


 
##################
# Make plots using the CharisSIL IPA font.
##################
library(extrafont)
# library(remotes)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# extrafont::font_import()

 
##########
# This only needs to be done once on each computer.
# font_import(prompt = F,pattern="DoulosSIL-R") # Import system fonts -- this can take a while if you import them all
# loadfonts(device = "win") # I think you only need to do this once so that R imports all the needed files to a place it can draw on them?
windowsFonts() # This will just show you which fonts are available to R -- it may be a long list!



##################
# Compute the number of observations for each type, grouped by dialect and speaker.
##################
# What combination of factors is used to uniquely define each item type, i.e.
# how do you define one condition?
# The code a few lines below will determine the number of observations per condition
# and append that to edgetrak.minmax.

uniqueItemVars <- c("dialect", "subj", "syll.pos", "cmerge", "v", "frm.pos") 

# Compute the number of unique repetitions for each item type.
# We use .dots to pass the list of factors as a vector with quoted entries, e.g. as <"sec.art"> instead of <sec.art>

# The below says split the data up by cross classifying variables above under "uniqueItemVars".
obs.lookup <- edgetrak.minmax %>% group_by(.dots = uniqueItemVars) %>% summarise(obs.raw = length(unique(rep))) %>% ungroup()
edgetrak.minmax <- left_join(edgetrak.minmax,obs.lookup,by=uniqueItemVars) # https://dplyr.tidyverCI.org/reference/mutate-joins.html
rm(obs.lookup)
#
# You might consider an alternative approach using add_count(), but note that it would be harder to ignore pt.num
# when doing this (i.e. all of the counts would be too big by a factor of 100, the number of points EdgeTrak spits
# out for each tracing),
# https://dplyr.tidyverCI.org/reference/tally.html



##############################################################
# Prepare a data frame containing data fits over POLAR representations of the data.
# This is advocated by Mielke (2015). Need Data.table package.
##############################################################

###############################
# Locate a hypothesized origin point for each speaker, following Mielke (2015)  

# Create a dataframe collecting origin points grouped by speaker
# Recall that Y-axis is flipped, which affects how we call max and min here,
# and angle width/start, which is very confusing.
origins.df <- edgetrak.minmax %>% group_by(Speaker) %>% 
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
edgetrak.minmax<-merge(edgetrak.minmax,
           select(origins.df,-c("dialect","subj")),
           by="Speaker")


###############################
# For plotting loess/ssanova within a polar space rather than a Cartesian space (as Mielke 2015 argues is appropriate),
# define a function which will:
# (i) Convert Cartesian coordinates to polar coordinates
# (ii) Fit a model and extract model output (estimate of curve and SEs)
# (iii) Convert model output back to Cartesian space
#
# The function polarize_and_fit() is intended to be used for fitting several repetitions of a single item. To get
# fits for multiple items in the same dataframe, you have to do something like the following:
#
#     inputdata %>% group_by(.dots = uniqueItemVars) %>% do(polarize_and_fit(.))
#
# Where uniqueItemVars is a vector listing the column names of the factors that can be used to define each item uniquely.
# (See the definition of uniqueItemVars above and its use elsewhere.)
#
# Note also that this function can produce some outputs which won't arise for models fit over Cartesian data,
# e.g. it can produce curves that curl over/under themselves, such that a single X-value might in principle
# have multiple Y-values. This is a potential issue when plotting.

polarize_and_fit <- function(df,outputVars = uniqueItemVars){
  
  # Re-center data w.r.t. origin
  X.new <- df$origin.X - df$X
  Y.new <- df$Y - df$origin.Y # Y-axis is inverted
  
  # Transform to polar coordinates and substitute.
  pi <- 3.14159265359
  df$th<- pi + atan2(Y.new, X.new) # Angle
  df$r<- sqrt(X.new^2 + Y.new^2) # Radius length
  
  # Fit a loess model over polar coordinates, and extract details of the model specification.
  # Since the angle (theta) is unique, it's like our X values.
  # Since r values (radius length) are not necessarily unique, they are like our Y values.
  #
  # If you'd rather fit an SSANOVA or another kind of model fit instead of a loess model,
  # this is where you should make those changes.
  polar.fit.model<-predict(loess(r~th,df),se=T)
  polar.fit<-data.frame(cbind("th" = df$th,"r.smooth" = polar.fit.model$fit,"SE.polar" = polar.fit.model$se))
  
  # Before reconverting from polar coordinates back to Cartesian space, create a vector for keeping track
  # of the order of the points in the model fit.
  polar.fit$pt.order <- seq(1,nrow(polar.fit),1)
  
  polar.fit <- polar.fit %>% arrange(polar.fit,pt.order)
  
  # Reconvert the model output from polar space to Cartesian space.
  # This will also translate the results back into the original space, which is not
  # centered w.r.t. the origin.
  # If you *wanted* to center all of the fits with respect to the same point (or their respective origins), you could do that here.
  x.cart <- df$origin.X + polar.fit$r.smooth * cos(polar.fit$th)
  y.cart <- df$origin.Y - polar.fit$r.smooth * sin(polar.fit$th) # Different sign because Y axis is reversed.
  CI.hi.cart <- df$origin.Y - (polar.fit$r.smooth+1.96*polar.fit$SE.polar) * sin(polar.fit$th) # Different sign because Y axis is reversed.
  CI.lo.cart <- df$origin.Y - (polar.fit$r.smooth-1.96*polar.fit$SE.polar) * sin(polar.fit$th) # Different sign because Y axis is reversed.
  pt.order <- polar.fit$pt.order
  # Create data frame output, and return it.
  df.pol.smooth <- data.frame(cbind(
                              "X.fit"=x.cart,"Y.fit"=y.cart,"CI.hi"=CI.hi.cart,"CI.lo"=CI.lo.cart, est.pt = pt.order # This is output of the model.
                                    )
                              )
  
  # Add classifying factors of interest. Adds factors back to data frame.
  # This assumes, again, that this function is only applying to
  # fit a model over repetitions of the same item type.
  df.pol.smooth <- cbind(df.pol.smooth,df[1,outputVars],"polar.r"=polar.fit$r.smooth,"polar.th"=polar.fit$th)

  return(df.pol.smooth)
}


# Generate a data frame containing polar loess fits for each item type, using the function just defined above.
polar.fit.df <- edgetrak.minmax %>% group_by(.dots = uniqueItemVars) %>% do(polarize_and_fit(.)) %>% ungroup()



# Add back in any factors of interest from the original dataframe.
# We do this by focusing only on those factors that uniquely define each item type.
edgetrak.minmax.conditions<-unique(select(edgetrak.minmax,
                                -c("X","Y","pt.num","rep")))

# This is the dataframe that will serve as the input to all plotting of smoothed curves below.
plotting.data <- left_join(polar.fit.df,
                           edgetrak.minmax.conditions)


# For any conditions that have just one repetition, get rid of the confidence intervals, which are meaningless.
# I'm not sure why the code above even produces CIs for such cases --- it really should be producing NAs in the first place.
# Instead, it looks like one of the CIs is just equal to the model estimate, and I'm not sure where the other comes from.
plotting.data[plotting.data$obs.raw==1,]$CI.hi<-NA
plotting.data[plotting.data$obs.raw==1,]$CI.lo<-NA




###############################
# Plot smoothed contours
###############################

# We define a standard, semi-generic function for plotting across different comparison types.
# Below, we will use different instances of dplyr::group_by to plot across different conditions.
# using the function makePlotGroups. This helps us avoid very ugly, and slow, nested for statements
# to iterate through all of the factor combinations we're interested in.

# On the use of aes_string rather than aes below: it's needed to pass variables around when
# using ggplot() inside a function. Basically, the fact that <plottype> is used as both a function
# parameter and a grouping variable withing aes() in ggplot requires this.
# https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot

# This plotting function expects a data frame which already includes the (loess) fit line and
# associated confidence intervals as lower/upper lines. This data frame was already generated above.


#############
# Define base plotting function
# 
plotPolarFits <- function(inputdata,
                            # The plottype value shouldn't need to be changed, but in principle it could be any
                            # column name (e.g. "Consonant"), depending on the condition you are plotting.
                            # Below, we define a function that produces a column called "obs" which includes information
                            # about the number of repetitions for each kind of comparison being carried out.
                            # This function is basically parasitic on that function, which mostly serves to smuggle in 
                            # the count of observations into the labels used for each comparison type.
                            # Still, the plottype parameter is still useful if you want to group data by some other column,
                            # and can be used for troubleshooting code or dataframes (for example).
                            plottype="obs", 
                            plotmaintitle="",
                            plotsubtitle="",
                            dialectFacet=F,
                            radialGrid=T){

    # Create base plot, without any data on it.
    ultPlot <- ggplot(data=inputdata,
                      aes_string(x="X.fit",
                                 y="Y.fit",
                                 group=plottype,
                                 col=plottype,
                                 lty=plottype)
                      )
      
  
  # Plot origin and spokes if desired.
  # We do this first so that it sits *under* any data we decide to plot.
  # You don't necessarily need to call origins.df here --- the same information is now in inputdata = plotting.data
  # But since origins.df just has one row per speaker it's a bit neater to use it here, since it already exists.
  
  if (radialGrid==T){
    # Add point showing hypothesized origin.
    if (dialectFacet==F){
        spk.origin<-subset(origins.df,
                           subj==unique(inputdata$subj) &
                           dialect==unique(inputdata$dialect))
    } else if (dialectFacet==T) {
      spk.origin<-origins.df
    }
    
    ultPlot <- ultPlot + geom_point(inherit.aes = F, # Add the origin.
                                    data=spk.origin,
                                    aes(x=origin.X,
                                        y=origin.Y),
                                        na.rm=TRUE,
                                        alpha=0.5)+
                        # Right edge of polar angle
                        geom_segment(inherit.aes = F,
                                     data=spk.origin,
                                     aes(x=origin.X,
                                         y=origin.Y,
                                         xend=r.edge.x,
                                         yend=r.edge.y),
                                         na.rm=TRUE,
                                         alpha=0.5)+
                        # Left edge of polar angle
                        geom_segment(inherit.aes = F,
                                     data=spk.origin,
                                     aes(x=origin.X,
                                         y=origin.Y,
                                         xend=l.edge.x,
                                         yend=l.edge.y),
                                         na.rm=TRUE,
                                         alpha=0.5)+
                        # Vertical line parallel to Y-axis
                        # geom_spoke(inherit.aes = F,
                        #            data=spk.origin,
                        #            lwd=1.25,
                        #            lty="dashed",
                        #            aes(x=origin.X,
                        #                y=origin.Y,
                        #                radius = spokeLength,
                        #                angle = 90*pi/180), # Need to convert from degrees to radians
                        #                na.rm=TRUE,
                        #                alpha=0.5)+
                        geom_spoke(inherit.aes = F,data=spk.origin,lty="dashed",
                                   aes(x=origin.X,y=origin.Y,radius=spokeLength,angle=angleWidth/4+angleStart),
                                   na.rm=TRUE,
                                   alpha=0.3)+
                        geom_spoke(inherit.aes = F,data=spk.origin,lty="dashed",
                                   aes(x=origin.X,y=origin.Y,radius=spokeLength,angle=2*angleWidth/4+angleStart),
                                   na.rm=TRUE,
                                   alpha=0.3)+
                        geom_spoke(inherit.aes = F,data=spk.origin,lty="dashed",
                                   aes(x=origin.X,y=origin.Y,radius=spokeLength,angle=3*angleWidth/4+angleStart),
                                   na.rm=TRUE,
                                   alpha=0.3)

  }
  
  ######  
  # Add data plots
  #
  # Add confidence intervals.
  # You've tried to mimic the default aesthetics of ggplot's
  # geom_smooth(method=loess) (derived from geom_ribbon() for CIs), which is what you'd 
  # be relying on if you didn't have to do all of this polar <=> Cartesian inter-translation
  # here.
  # If you want you could add color = plottype to the aesthetic call here and remove color="grey60" if you want
  # the CI lines to be the same color as the main line for the smoothed estimate.
  # If you do that you probably need to use aes_string, and enclose the x/y values in aes_string() in quotes.
  #
  # We don't want confidence intervals for conditions with only one repetition, so we replaced those with NA above.
  # Here we add na.rm = T so that we don't get warnings when we plot such conditions.
  ultPlot <- ultPlot + geom_path(aes(x=X.fit,y=CI.hi),lwd=0.75,alpha=0.4,color="grey60", show.legend = F, na.rm=T, lty = "solid")+
                       geom_path(aes(x=X.fit,y=CI.lo),lwd=0.75,alpha=0.4,color="grey60", show.legend = F, na.rm=T, lty = "solid")+
  #
  # Add main fit line on top of confidence lines so that it stands out more than the CIs
  # when the two are heavily overlapping.
  geom_path(lwd=2,na.rm=T)
    
  
  #####
  # Add aesthetics
  ultPlot <- ultPlot + 
           ggtitle(plotmaintitle)+
           labs(subtitle=plotsubtitle)+
           xlab("Backness (normalized)")+
           ylab("Height (normalized)")+
      
           theme_bw(base_size = 22)+
           theme(legend.key.width=unit(4,"line"),
                 legend.key.height=unit(1,"line"),
                 text = element_text(family = "Charis SIL"),
                 legend.title = element_blank(),
                 legend.text = element_text(size=20)
                 )+
    
           scale_color_manual(values=CbbPalette)+
           coord_equal() # This is really important --- it guarantees that the X/Y axis will be plotted at the same scale.

  if (dialectFacet==F){
    # Reposition the legend.
    ultPlot <- ultPlot+theme(
          legend.position = c(.01, .99),
          legend.justification = c("left", "top")
          )+
      # Set plot limits
      scale_y_reverse(limits=c(0.75,-0.2),breaks=seq(-0.2,0.75,0.1))+
      scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
      # Add big labels
      annotate("text",x=0.05,y=0.75,label="BACK",size=10,family="Charis SIL",fontface="bold")+
      annotate("text",x=0.9,y=0.75,label="FRONT",size=10,family="Charis SIL",fontface="bold")
    
  } else if (dialectFacet==T){
    # Add faceting
    ultPlot <- ultPlot+facet_wrap(dialect~subj, nrow=2)+theme(strip.text = element_text(face = "bold",size=32))+
      # Set plot limits
      scale_y_reverse(limits=c(0.85,-0.1),breaks=seq(-0.1,0.75,0.1))+
      scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+
      # Add small labels
      annotate("text",x=0.2,y=0.8,label="BACK",size=10,family="Charis SIL",fontface="bold")+
      annotate("text",x=0.75,y=0.8,label="FRONT",size=10,family="Charis SIL",fontface="bold")
  }
  
  return(ultPlot)
  
}




#########
# Define a function for grouping your data into desired plotting groups,
# and adding # of observations per plotted condition.
# 
# This function also produces a GROUPED tibble which can then be used as the *input* to the plotting
# function plotPolarFits() to produce comparisons for a specific condition type.
#
# CAUTIONARY NOTES:
# - This function assumes that the number of observations per condition has already been computed in the input
#   data frame, and is listed in the column obs.raw.
# - This function has as a default parameter value the vector groupingVars, which is defined repeatedly below prior to calling the plotting functions
#   for each comparison type.
# - The parameter conditionType defines the comparison you care about for plotting.
#   It should correspond to a column name of the input data frame.

makePlotGroups<-function(inputdf,
                         conditionType="cmerge",
                         plotgroups = groupingVars,
                         eliminateSingObs = T
                         ){

  # We use interaction() instead of paste() to put together the number of observations and the original factor.
  # We do this to maintain the order of levels in the original factor as set above.
  # https://stackoverflow.com/questions/26633028/the-paste-function-and-ordering-levels-of-variables
  counts.par<-paste0("(", inputdf$obs.raw, ")")
  inputdf$obs<-interaction(data.frame(inputdf)[,conditionType], # If the input dataframe is a tibble, change that, or else it will cause issues.
                           counts.par,
                           sep = " ",
                           lex.order = T, # Make sure that the order of levels doesn't depend on the count of observations
                           drop = T) # Get rid of any factor combinations that aren't in the data.

  # Use tidyverse/dplyr to group data into a grouped tibble for the purposes of
  # plotting subgroups.
  conditionGroups <- setdiff(plotgroups,
                             conditionType) # All other grouping variables.
  
  # .dots = is how you convert list of column names in character/string form
  # to symbolic objects group_by() accepts
  outDF <- inputdf %>% group_by(.dots = conditionGroups)
  
  # Eliminate trivial groups with only one contour -- we don't care much about these
  # because it's hard to interpret them anyway without palate tracings, and they aren't
  # being compared to anything because there's only one contour in the plot.
  # You can turn this off if you like.
  if (eliminateSingObs==T){
    outDF <- outDF %>% mutate(ncurve = n_distinct(obs))
    outDF <- subset(outDF,ncurve > 1)
    outDF <- select(outDF,-ncurve)
  }
  
  return(outDF)
}





##########################
# Create plots
##########################

# FYI, there's a lot of clunky/awkward code below which should at some point be
# simplified and streamlined. For the moment it works, though.

# TO DO:
# - The call to makePlotGroups() here probably could and should be 
#   folded into the call to plotPolarFits().
# - The repeated redefinition of groupingVars is pretty clunky here. At a minimum
#   this should be folded into the call to makePlotGroups(plotgroups = c()). I'm sure there's
#   also a lot of repetition that could be minimized or avoided with the base vector uniqueItemVars
#   defined above.

###############
# Plot by secondary articulation. 
###############

groupingVars = c("dialect", "subj", "syll.pos", "cmerge", "v", "frm.pos","cbasicmerge")

c.sec.comps <- makePlotGroups(plotting.data,
                              conditionType="cmerge"
                              )

plotSet <- c.sec.comps %>%
  do(ultrasoundOutput = plotPolarFits(inputdata = .,
                    plottype="obs", # This has to be called as a string; see aes_string above
                    plotmaintitle=paste0(#"By secondary art.: ",
                                         unique(.$syll.pos), " ",
                                         "/", unique(.$cbasicmerge), "/, ",
                                         unique(.$Vowel),"context,",        # V context
                                         " C ",unique(.$frm.pos)              # Frame
                                         ),
                                         plotsubtitle=unique(.$Speaker))
     )

# Show plots 
# plotSet$ultrasoundOutput # This will take a long time, so don't uncomment without selecting some small subset you want to see.
# subset(plotSet,dialect=="Connacht" & subj =="3" & v =="a" & frm.pos=="end" & syll.pos =="coda")$ultrasoundOutput




############
# Define a function for generating folders for saving PDFs, and for actually saving the PDFs.
# Assumes the input is a unique row of a grouped tibble with a column containing ggplots
# This won't work if you use a tibble/dataframe as the input, as currently defined.
# Also assumes that the variable outdir was set above, as the base directory for storing 
# results.

makeultPDF<-function(inputRow,
                     fileOutName, # What's the filename (XXXXXXXXXX.pdf)?
                     comparisonClass="pal", # What kind of comparison is involved?
                     dynamics=F, # Are you plotting temporal dynamics or comparisons of segment types?
                     plotClass="loess", # Plotting loess, raw data, or something else?
                     allspeakers=F # Are you making faceted plots showing data from all speakers?
                     ){
  
  # Set folder for saving plot
  if (allspeakers==F){
    localOutdir <- paste0(outdir,inputRow$dialect, # Dialect
                          "/",plotClass,"/", # Plot type
                          inputRow$dialect,"_",inputRow$subj,"_","loess", # Speaker folder 
                          "/",comparisonClass # Comparison type
                          )
  } else if (allspeakers==T) {
    localOutdir <- paste0(outdir,
                          "All_speakers_","loess", # Speaker folder 
                          "/",comparisonClass # Comparison type
                          )
    
  }
  
  # Create temporal landmark subfolders if needed
  if (dynamics==F) {
    localOutdir <- paste0(localOutdir,'/',
                          inputRow$frm.pos)
  } else {
    # Do nothing
  }

  # Create the desired subfolder if necessary, including folders farther up the tree.
  if (dir.exists(localOutdir) == T){
    # Do nothing
  } else {
    dir.create(localOutdir,recursive=T)
  }
  
  outname<-paste0(localOutdir,"/",fileOutName,".pdf")
  
  if (allspeakers==F){
  cairo_pdf(file=outname,
            width=10,height=8)
    print(inputRow$ultrasoundOutput)
  dev.off()
  } else if (allspeakers==T){
    cairo_pdf(file=outname,
              width=24,height=10)
      print(inputRow$ultrasoundOutput)
    dev.off()
  }
}


######################################################
# TO DO: GENERALIZE THIS FILE SAVING LOOP INTO A FUNCTION
######################################################
# The free parameters to be factored out are filename and localOutdir, 
# which themselves only really have limited points of customization (just comparison type?)
# filename is the tough one, when it comes to generalization across comparison types...
#
# One possibility is to define "filename" as a variable which is then passed to 
# makeultPDF(), which would then incorporate the full dataframe as input rather
# than a single row?
# https://stackoverflow.com/questions/14046195/r-pass-function-in-as-variable
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste(currplotRow$dialect,
                  currplotRow$subj,
                  currplotRow$syll.pos,
                  currplotRow$cbasicmerge,
                  "pal",
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="pal")
  
  print(filename)
}
rm(k)



###############
# Plot, save secondary articulation using faceted plots to show all speakers at once
###############

# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add .facet tag to output of makePlotGroups() call, and input to do()
# - Add eliminateSingObs=F to makePlotGroups() call.
# - Add dialectFacet=T to plotPolarFits() call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - If you don't want to count the observations in the legend, set plottype to desired grouping variable instead of obs in plotPolarFits() call
# - Add allspeakers = T to makeultPDF() call.

# The code below leaves out cmerge, though it's in ths analogous code for individual plots above.
# Is that for a reason?
groupingVars <- c("syll.pos", "v", "frm.pos","cbasicmerge") # Reset grouping variables.

c.sec.comps.facet <- makePlotGroups(plotting.data,
                                    conditionType="cmerge",
                                    eliminateSingObs=F) # Leave in trivial comparisons

plotSet <- c.sec.comps.facet %>%
  do(ultrasoundOutput = plotPolarFits(dialectFacet=T,
                                        inputdata = .,
                                        plottype="cmerge", # This has to be called as a string; see aes_string above
                                        plotmaintitle=paste0(#"By secondary art.: ",
                                                             unique(.$syll.pos), " ",
                                                             unique(.$cbasicmerge), ", ",
                                                             unique(.$Vowel)," context,",         # V context
                                                             " C ",unique(.$frm.pos)              # Frame
                                        ),
                                        plotsubtitle="By dialect and speaker")
  )

# subset(plotSet,v =="i" & frm.pos=="end" & syll.pos =="onset")$ultrasoundOutput

for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste("Phase1_allspeakers",
                  currplotRow$cbasicmerge,
                  "pal",
                  currplotRow$syll.pos,
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="pal",
             allspeakers = T)
  
  print(filename)
}
rm(k)




###############
# Plot by consonant dynamics (i.e. variation across temporal landmarks)
###############

# Set groupings
# We need to use "cbasicpal" instead of "cbasic" for filenaming, apart from the secondary articulation plots above.
groupingVars <- c("dialect", "subj", "syll.pos", "cmerge", "v", "frm.pos","cbasicpal") # Reset grouping variables.
c.dynam.comps <- makePlotGroups(plotting.data,
                                conditionType="frm.pos")

# Set plot template (mostly, set title format)
plotSet <- c.dynam.comps %>%
  do(
    ultrasoundOutput = plotPolarFits(inputdata = .,
                                       plotmaintitle=paste0(#"By timecourse: ",
                                                            unique(.$syll.pos),
                                                            " /",unique(.$cmerge),"/", # Consonant
                                                            ", ", unique(.$Vowel)," context" # V context
                                                            ),
                                       plotsubtitle=unique(.$Speaker))

  )


# Save PDFs
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste(currplotRow$dialect,
                  currplotRow$subj,
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  currplotRow$syll.pos,
                  currplotRow$v,
                  "dynamics",
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="dynamics",
             dynamics=T)
  
  print(filename)
}
rm(k)


###############
# Plot by consonant dynamics (i.e. variation across temporal landmarks), faceting by dialect and speaker
###############

# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add .facet tag to output of makePlotGroups() call, and input to do()
# - Add eliminateSingObs=F to makePlotGroups() call.
# - Add dialectFacet=T to plotPolarFits() call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - If you don't want to count the observations in the legend, set plottype to desired grouping variable instead of obs in plotPolarFits() call
# - Add allspeakers = T to makeultPDF() call.


# Set groupings
groupingVars <- c("syll.pos", "cmerge", "v", "frm.pos","cbasicpal") # Reset grouping variables.
c.dynam.comps.facet <- makePlotGroups(plotting.data,
                                conditionType="frm.pos",
                                eliminateSingObs=F)  # Leave in trivial comparisons

# Set plot template (mostly, set title format)
plotSet <- c.dynam.comps.facet %>%
  do(
    ultrasoundOutput = plotPolarFits(dialectFacet=T,
                                       inputdata = .,
                                       plottype="frm.pos",
                                       plotmaintitle=paste0(#"By timecourse: ",
                                                            unique(.$syll.pos),
                                                            " /",unique(.$cmerge),"/", # Consonant
                                                            ", ", unique(.$Vowel)," context" # V context
                                       ),
                                       plotsubtitle="By dialect and speaker")
    
  )


# Save PDFs
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste("Phase1_allspeakers",
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  currplotRow$v,
                  "dynamics",
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="dynamics",
             dynamics=T,
             allspeakers = T)
  
  print(filename)
}
rm(k)




###############
# Plot by syllable position
###############

# Set groupings
groupingVars <- c("dialect", "subj", "cmerge", "syll.pos", "v", "frm.pos","cbasicpal") # Reset grouping variables.
c.syll.comps <- makePlotGroups(plotting.data,
                                conditionType="syll.pos")

# Set plot template (mostly, set title format)
plotSet <- c.syll.comps %>%
  do(
    ultrasoundOutput = plotPolarFits(inputdata = .,
                                       plotmaintitle=paste0("\u03C3 position: ",
                                                             "/",unique(.$cmerge),"/", # Consonant
                                                             ", ", unique(.$Vowel)," context", # V context
                                                             ", C ",unique(.$frm.pos)              # Frame
                                        ),
                                        plotsubtitle=unique(.$speaker))
     
   )
# 
# 
# # Save PDFs
 for (k in 1:nrow(plotSet)){
   currplotRow<-plotSet[k,]
   
   filename<-paste(currplotRow$dialect,
                   currplotRow$subj,
                   currplotRow$cbasicpal,
                   currplotRow$v,
                   "syllpos",
                   currplotRow$frm.pos,
                   sep="_")
#   
   makeultPDF(currplotRow,
              filename,
              comparisonClass="syllpos")
#   
   print(filename)
 }
rm(k)
# 

###############
# Plot by syllable position, faceting by dialect and speaker
###############

# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add .facet tag to output of makePlotGroups() call, and input to do()
# - Add eliminateSingObs=F to makePlotGroups() call.
# - Add dialectFacet=T to plotPolarFits() call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - If you don't want to count the observations in the legend, set plottype to desired grouping variable instead of obs in plotPolarFits() call
# - Add allspeakers = T to makeultPDF() call.


# Set groupings
groupingVars <- c("cmerge", "syll.pos", "v", "frm.pos","cbasicpal") # Reset grouping variables.
c.syll.comps.facet <- makePlotGroups(plotting.data,
                                conditionType="syll.pos",
                                eliminateSingObs=F)
 
# # Set plot template (mostly, set title format)
plotSet <- c.syll.comps.facet %>%
   do(
     ultrasoundOutput = plotPolarFits(dialectFacet=T,
                                        inputdata = .,
                                        plottype="syll.pos",
                                        plotmaintitle=paste0("\u03C3 position: ",
                                                             "/",unique(.$cmerge),"/", # Consonant
                                                             ", ", unique(.$Vowel)," context ", # V context
                                                             ", ", unique(.$frm.pos)              # Frame
                                        ),
                                        plotsubtitle="By dialect and speaker")
     
   )
# 
# 
# # Save PDFs
for (k in 1:nrow(plotSet)){
   currplotRow<-plotSet[k,]
   
   filename<-paste("Phase2_allspeakers",
                   currplotRow$cbasicpal,
                   currplotRow$v,
                   "syllpos",
                   currplotRow$frm.pos,
                   sep="_")
   
   makeultPDF(currplotRow,
              filename,
              comparisonClass="syllpos",
              allspeakers = T)
   
   print(filename)
}
rm(k)



###############
# Plot by vowel context
###############

# Set groupings
groupingVars <- c("dialect", "subj", "syll.pos", "cmerge", "Vowel","frm.pos","cbasicpal") # Reset grouping variables.
c.v.comps <- makePlotGroups(plotting.data,
                            conditionType="Vowel")

# Set plot template (mostly, set title format)
plotSet <- c.v.comps %>%
  do(
    ultrasoundOutput = plotPolarFits(inputdata = .,
                                       plotmaintitle=paste0(#"By vowel context: ",
                                                            unique(.$syll.pos),
                                                            " /",unique(.$cmerge),"/", # Consonant
                                                            ", C ", unique(.$frm.pos)              # Frame
                                       ),
                                       plotsubtitle=unique(.$Speaker))
    
  )


# Save PDFs
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste(currplotRow$dialect,
                  currplotRow$subj,
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  "vowelcon",
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="vowelcon")
  
  print(filename)
}
rm(k)


###############
# Plot by vowel context, faceting by dialect and speaker
###############

# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add .facet tag to output of makePlotGroups() call, and input to do()
# - Add eliminateSingObs=F to makePlotGroups() call.
# - Add dialectFacet=T to plotPolarFits() call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - If you don't want to count the observations in the legend, set plottype to desired grouping variable instead of obs in plotPolarFits() call
# - Add allspeakers = T to makeultPDF() call.

# Set groupings
groupingVars <- c("syll.pos", "cmerge", "Vowel","frm.pos","cbasicpal") # Reset grouping variables.
c.v.comps.facet <- makePlotGroups(plotting.data,
                            conditionType="Vowel",
                            eliminateSingObs=F)

# Set plot template (mostly, set title format)
plotSet <- c.v.comps.facet %>%
  do(
    ultrasoundOutput = plotPolarFits(dialectFacet=T,
                                       inputdata = .,
                                       plottype = "Vowel",
                                       plotmaintitle=paste0(#"By vowel context: ",
                                                            unique(.$syll.pos),
                                                            " s/",unique(.$cmerge),"/", # Consonant
                                                            ", C ",unique(.$frm.pos)              # Frame
                                       ),
                                       plotsubtitle="By dialect and speaker")
    
  )


# Save PDFs
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste("Phase1_allspeakers",
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  "vowelcon",
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="vowelcon",
             allspeakers = T)
  
  print(filename)
}
rm(k)



###############
# Plot by C-place
###############

# Set groupings
groupingVars <- c("dialect", "subj", "syll.pos", "c.place", "sec.art.IPA","sec.art", "v", "frm.pos") # Reset grouping variables.
c.place.comps <- makePlotGroups(plotting.data,
                                conditionType="c.place")

# Set plot template (mostly, set title format)
plotSet <- c.place.comps %>%
  do(
    ultrasoundOutput = plotPolarFits(inputdata = .,
                                       plotmaintitle=paste0(#"By place: ",
                                                            unique(.$syll.pos)," ",
                                                            "/C",unique(.$sec.art.IPA),"/, ", # secondary articulation
                                                            unique(.$Vowel)," context, ",
                                                            "C ", unique(.$frm.pos)              # Frame
                                                            ),
                                       plotsubtitle=unique(.$Speaker))

  )


# subset(plotSet,dialect=="Connacht" & subj =="3" & v =="a" & frm.pos=="mdpt" & syll.pos =="coda")$ultrasoundOutput

 
# Save PDFs
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]

  filename<-paste(currplotRow$dialect,
                  currplotRow$subj,
                  currplotRow$syll.pos,
                  currplotRow$sec.art,
                  "cplace",
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")

  makeultPDF(currplotRow,
             filename,
             comparisonClass="cplace")

  print(filename)
}
rm(k)


###############
# Plot by C-place, faceting by dialect and speaker
###############

# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add .facet tag to output of makePlotGroups() call, and input to do()
# - Add eliminateSingObs=F to makePlotGroups() call.
# - Add dialectFacet=T to plotPolarFits() call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - If you don't want to count the observations in the legend, set plottype to desired grouping variable instead of obs in plotPolarFits() call
# - Add allspeakers = T to makeultPDF() call.


# Set groupings
groupingVars <- c("syll.pos", "c.place", "sec.art.IPA","sec.art", "v", "frm.pos") # Reset grouping variables.
c.place.comps.facet <- makePlotGroups(plotting.data,
                                conditionType="c.place",
                                eliminateSingObs=F)

# Set plot template (mostly, set title format)
plotSet <- c.place.comps.facet %>%
  do(
    ultrasoundOutput = plotPolarFits(dialectFacet=T,
                                       inputdata = .,
                                       plottype = "c.place",
                                       plotmaintitle=paste0(#"By place: ",
                                                            unique(.$syll.pos)," ",
                                                            "/C",unique(.$sec.art.IPA),"/, ", # secondary articulation
                                                            unique(.$Vowel)," context, ",
                                                            "C ", unique(.$frm.pos)              # Frame
                                       ),
                                       plotsubtitle="By dialect and speaker")
    
  )


# subset(plotSet,dialect=="Connacht" & subj =="3" & v =="a" & frm.pos=="mdpt" & syll.pos =="coda")$ultrasoundOutput


# Save PDFs
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste("Phase1_allspeakers",
                  currplotRow$syll.pos,
                  currplotRow$sec.art,
                  "cplace",
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultPDF(currplotRow,
             filename,
             comparisonClass="cplace",
             allspeakers = T)
  
  print(filename)
}
rm(k)




###############################
# Plot raw contours
# This can be done after running the code up to creation of final edgetrak data frame.
###############################

# We define a standard, semi-generic function for plotting raw ultrasound tracings.
# This is very similar to plotPolarFits() defined above, but for the moment at least 
# we don't bother trying to combine them into a single function.

ultrasoundRaw <- function(inputdata,
                          plotmaintitle="",
                          plotsubtitle="",
                          dialectFacet=F){
  
  # Current plotting parameters below make this pointless, but 
  # this makes the title of the legend look nicer when the title is included.
  inputdata<-inputdata %>% rename(Repetition=rep)
  
  ultPlot <- ggplot(data=inputdata,
                    aes(x=X,
                        y=Y,
                        group=Repetition,
                        col=Repetition,
                        shape=Repetition)
                    )+
    
    
    # Using geom_path rather than geom_line preserves the order of points as they appear in the input dataframe.
    geom_path(lwd=1,na.rm=TRUE,alpha=0.5)+
    geom_point(size=2,na.rm=TRUE,alpha=0.5)+

    ggtitle(plotmaintitle)+
    labs(subtitle=plotsubtitle)+
    xlab("Backness (normalized)")+
    ylab("Height (normalized)")+
 
    theme_bw(base_size = 22)+
    theme(legend.key.width=unit(4,"line"),
          legend.key.height=unit(1,"line"),
          text = element_text(family = "Charis SIL"),
          legend.title = element_blank(),
          legend.text = element_text(size=20)
          )+

    scale_color_manual(values=CbbPalette)+
    coord_equal()
  
    if (dialectFacet==F){
      ultPlot <- ultPlot+theme(
      legend.position = c(.01, .99),
      legend.justification = c("left", "top"),
      )+

# BE CAREFUL WITH THE NEXT COMMANDS. I'VE HAD TO SWITCH THE AXIS RANGES DEPENDING
# ON WHETHER THE DATA IS RAW, Z-SCORED, OR NORMALIZED.
        
    scale_y_reverse(limits=c(0.75,-0.2),breaks=seq(-0.2,0.75,0.1))+
    scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
      
    annotate("text",x=0.05,y=0.75,label="BACK",size=10,family="Charis SIL",fontface="bold")+
    annotate("text",x=0.9,y=0.75,label="FRONT",size=10,family="Charis SIL",fontface="bold")
    
  } else if (dialectFacet==T){
    # Add faceting
    ultPlot <- ultPlot+facet_wrap(dialect~subj, nrow=3)+theme(strip.text = element_text(face = "bold",size=32))+

      scale_y_reverse(limits=c(0.75,-0.2),breaks=seq(-0.2,0.75,0.1))+
      scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+

            # Add small labels
      annotate("text",x=0.2,y=0.8,label="BACK",size=10,family="Charis SIL",fontface="bold")+
      annotate("text",x=0.75,y=0.8,label="FRONT",size=10,family="Charis SIL",fontface="bold")
    
  }
  
  return(ultPlot)
  
}


############
# Define a function for generating folders for saving PDFs, and for actually saving the PDFs.
# Assumes the input is a unique row of a grouped tibble with a column containing ggplots
# This won't work if you use a tibble/dataframe as the input, as currently defined.
# Also assumes that the variable outdir was set above, as the base directory for storing 
# results.
#
# It would be cleaner if this were folded into the makeultPDF() function above, but
# we go the quick-and-dirty route for now.
makeultRawPDF<-function(inputRow,fileOutName,allspeakers=F){

  # Set folder for saving plot
  if (allspeakers==F){
    # Set folder for saving plot
    localOutdir <- paste0(outdir,inputRow$dialect, # Dialect
                          "/Raw_tracings/",
                          inputRow$dialect,"_",inputRow$subj,"_","raw/",
                          inputRow$frm.pos) # Speaker folder 
    
  } else if (allspeakers==T) {
    localOutdir <- paste0(outdir,"/All_speakers_raw/"
    )
    
  }
  
  if (dir.exists(localOutdir) == T){
    # Do nothing
  } else {
    dir.create(localOutdir,recursive=T) # Create the desired subfolder if necessary, including folders farther up the tree.
  }
  
  outname<-paste0(localOutdir,"/",fileOutName,".pdf")
  
  if (allspeakers==F){
    cairo_pdf(file=outname,
              width=10,height=8)
    print(inputRow$ultrasoundOutput)
    dev.off()
  } else if (allspeakers==T){
    cairo_pdf(file=outname,
              width=24,height=10)
    print(inputRow$ultrasoundOutput)
    dev.off()
  }

}



# Set plot template (mostly, set title format, and set data groupings used for producing individual plots --- see groupingVars variable comments above for more information).
plotSet <- edgetrak.minmax %>% group_by(.dots = c("dialect", "subj", "syll.pos", "cmerge","v", "frm.pos","cbasicpal","Consonant")) %>%
  do(
    ultrasoundOutput = ultrasoundRaw(inputdata = .,
                                     plotmaintitle=paste0(unique(.$syll.pos),
                                                          " /",unique(.$cmerge),"/", # Consonant type
                                                          " (=", .$Consonant,")",
                                                          ", ", unique(.$Vowel)," context",
                                                          ", C ",unique(.$frm.pos)
                                     ),
                                     plotsubtitle=paste0("Raw EdgeTrak tracings: ", unique(.$Speaker))
    )
    
  )

# subset(plotSet,dialect=="Connacht" & subj =="3" & v =="a" & frm.pos=="mdpt" & syll.pos =="coda")$ultrasoundOutput


#########
# Save PDFs of raw tracings
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste(currplotRow$dialect,
                  currplotRow$subj,
                  "raw",
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultRawPDF(currplotRow,
                filename)
  
  print(filename)
}
rm(k)


#############################
# Make pooled raw tracings
#
# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add dialectFacet=T to ultrasoundRaw call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - Add allspeakers = T to makeultRawPDF() call.


# Set plot template (mostly, set title format, and set data groupings used for producing individual plots --- see groupingVars variable comments above for more information).
plotSet <- edgetrak.minmax %>% group_by(.dots = c("syll.pos", "cmerge","v", "frm.pos","cbasicpal","Consonant")) %>%
  do(
    ultrasoundOutput = ultrasoundRaw(inputdata = .,
                                     dialectFacet=T,
                                     plotmaintitle=paste0(unique(.$syll.pos),
                                                          " /",unique(.$cmerge),"/", # Consonant type
                                                          " (=", .$Consonant,")",
                                                          ", ", unique(.$Vowel)," context",
                                                          ", C ",unique(.$frm.pos)
                                     ),
                                     plotsubtitle="By dialect and speaker"
    )
    
  )


#########
# Save PDFs of raw tracings
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste("Phase2_allspeakers_raw",
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultRawPDF(currplotRow,
                filename,
                allspeakers = T)
  
  print(filename)
}


##########################
#########################
# Make raw tracings one rep at a time and mark highest point of tongue
# This is a modified copy of the above, with mods noted in *** commments
# This can be done after running the code up to creation of final edgetrak data frame.
##########################
##########################

# We define a standard, semi-generic function for plotting raw ultrasound tracings.
# This is very similar to plotPolarFits() defined above, but for the moment at least 
# we don't bother trying to combine them into a single function.

ultrasoundRaw <- function(inputdata,
                          plotmaintitle="",
                          plotsubtitle="",
                          dialectFacet=F){   
  
  # Current plotting parameters below make this pointless, but 
  # this makes the title of the legend look nicer when the title is included.

# ***Commented out next line and removed the 3 lines using Repetition in aes below.
  
#  inputdata<-inputdata %>% rename(Repetition=rep)

      ultPlot <- ggplot(data=inputdata,
                    aes(x=X,
                        y=Y)
                    )+
                    
# *** The key line added is the second geom_point line below.                      
                      
                      # Using geom_path rather than geom_line preserves the order of points as they appear in the input dataframe.
                      geom_path(lwd=1,na.rm=TRUE,alpha=0.5)+
                      geom_point(size=2,na.rm=TRUE,alpha=0.5)+
#                      geom_point(aes(x=X[Y==min(Y)], y=min(Y)), col="black", shape=4, size=7)+
                      geom_line(aes(y=min(Y)), col="black")+
                      
                      ggtitle(plotmaintitle)+
                      labs(subtitle=plotsubtitle)+
                      xlab("Backness (normalized)")+
                      ylab("Height (normalized)")+
                      
                      theme_bw(base_size = 22)+
                      theme(legend.key.width=unit(4,"line"),
                            legend.key.height=unit(1,"line"),
                            text = element_text(family = "Charis SIL"),
                            legend.title = element_blank(),
                            legend.text = element_text(size=20)
                      )+
                      
                      scale_color_manual(values=CbbPalette)+
                      coord_equal()
                    
                    if (dialectFacet==F){
                      ultPlot <- ultPlot+theme(
                        legend.position = c(.01, .99),
                        legend.justification = c("left", "top"),
                      )+
                        
                        # BE CAREFUL WITH THE NEXT COMMANDS. I'VE HAD TO SWITCH THE AXIS RANGES DEPENDING
                        # ON WHETHER THE DATA IS RAW, Z-SCORED, OR NORMALIZED.
                        
                        scale_y_reverse(limits=c(0.75,-0.2),breaks=seq(-0.2,0.75,0.1))+
                        scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.1))+
                        
                        annotate("text",x=0.05,y=0.75,label="BACK",size=10,family="Charis SIL",fontface="bold")+
                        annotate("text",x=0.9,y=0.75,label="FRONT",size=10,family="Charis SIL",fontface="bold")
                      
                    } else if (dialectFacet==T){
                      # Add faceting
                      ultPlot <- ultPlot+facet_wrap(dialect~subj, nrow=2)+theme(strip.text = element_text(face = "bold",size=32))+
                        
                        #      scale_y_reverse(limits=c(0.85,-0.1),breaks=seq(-0.1,0.75,0.1))+
                        #      scale_x_continuous(limits=c(0,1),breaks=seq(0,1,0.2))+
                        # Add small labels
                        annotate("text",x=0.2,y=0.8,label="BACK",size=10,family="Charis SIL",fontface="bold")+
                        annotate("text",x=0.75,y=0.8,label="FRONT",size=10,family="Charis SIL",fontface="bold")
                      
                    }
                    
                    return(ultPlot)
                    
}


############
# Define a function for generating folders for saving PDFs, and for actually saving the PDFs.
# Assumes the input is a unique row of a grouped tibble with a column containing ggplots
# This won't work if you use a tibble/dataframe as the input, as currently defined.
# Also assumes that the variable outdir was set above, as the base directory for storing 
# results.
#
# It would be cleaner if this were folded into the makeultPDF() function above, but
# we go the quick-and-dirty route for now.
makeultRawPDF<-function(inputRow,fileOutName,allspeakers=F){
  
  # Set folder for saving plot
  if (allspeakers==F){
    # Set folder for saving plot
    localOutdir <- paste0(outdir,inputRow$dialect, # Dialect
                          "/Raw_tracings_highpoint/",
                          inputRow$dialect,"_",inputRow$subj,"_","raw/",
                          inputRow$frm.pos)
    
  } else if (allspeakers==T) {
    localOutdir <- paste0(outdir,"/All_speakers_raw/tracings_highpoint"
    )
    
  }
  
  if (dir.exists(localOutdir) == T){
    # Do nothing
  } else {
    dir.create(localOutdir,recursive=T) # Create the desired subfolder if necessary, including folders farther up the tree.
  }
  
  outname<-paste0(localOutdir,"/",fileOutName,".pdf")
  
  if (allspeakers==F){
    cairo_pdf(file=outname,
              width=10,height=8)
    print(inputRow$ultrasoundOutput)
    dev.off()
  } else if (allspeakers==T){
    cairo_pdf(file=outname,
              width=24,height=10)
    print(inputRow$ultrasoundOutput)
    dev.off()
  }
  
}



# Set plot template (mostly, set title format, and set data groupings used for producing individual plots --- see groupingVars variable comments above for more information).
# *** Added line for rep below in plot title
plotSet <- edgetrak.minmax %>% group_by(.dots = c("dialect", "subj", "cmerge","v", "frm.pos","cbasicpal","Consonant", "rep", "syll.pos")) %>%
  do(
    ultrasoundOutput = ultrasoundRaw(inputdata = .,
                                     plotmaintitle=paste0(.$syll.pos, " /",unique(.$cmerge),"/", # Consonant type
                                                          " (=", .$Consonant,")",
                                                          ", ", unique(.$Vowel)," context",
                                                          ", C ",unique(.$frm.pos),
                                                          ", rep ",unique(.$rep)
                                     ),
                                     plotsubtitle=paste0("Raw EdgeTrak tracings: ", unique(.$Speaker))
    )
    
  )

# subset(plotSet,dialect=="Connacht" & subj =="3" & v =="a" & frm.pos=="mdpt" & syll.pos =="coda")$ultrasoundOutput


#########
# Save PDFs of raw tracings
 for (k in 1:nrow(plotSet)){
# for (k in 1:20){
  currplotRow<-plotSet[k,]

# ***Added rep below. Also changed "raw" to "minmax". This should be done
  # for regular code above for raw tracings too, if using edgetrak.minmax.
  
  filename<-paste(currplotRow$dialect,
                  currplotRow$subj,
                  currplotRow$syll.pos,
                  "minmax",
                  currplotRow$cbasicpal,
                  currplotRow$v,
                  currplotRow$frm.pos,
                  currplotRow$rep,
                  sep="_")
  
  makeultRawPDF(currplotRow,
                filename)
  
  print(filename)
}
rm(k)


#############################
# Make pooled raw tracings
# This hasn't been altered since it was copied from normal raw tracing code above.
# Things to do, relative to by-speaker plots showing the same comparisons.
# - Remove "dialect" and "subj" from groupingVars
# - Add dialectFacet=T to ultrasoundRaw call.
# - Edit plotsubtitle in plotPolarFits() call (if you want) to "By dialect and speaker"
# - Add allspeakers = T to makeultRawPDF() call.


# Set plot template (mostly, set title format, and set data groupings used for producing individual plots --- see groupingVars variable comments above for more information).
plotSet <- edgetrak.minmax %>% group_by(.dots = c("cmerge","v", "frm.pos","cbasicpal","Consonant", "syll.pos")) %>%
  do(
    ultrasoundOutput = ultrasoundRaw(inputdata = .,
                                     dialectFacet=T,
                                     plotmaintitle=paste0(.$syll.pos, " /",unique(.$cmerge),"/", # Consonant type
                                                          " (=", .$Consonant,")",
                                                          ", ", unique(.$Vowel)," context",
                                                          ", C ",unique(.$frm.pos)
                                     ),
                                     plotsubtitle="By dialect and speaker"
    )
    
  )


#########
# Save PDFs of raw tracings
for (k in 1:nrow(plotSet)){
  currplotRow<-plotSet[k,]
  
  filename<-paste("Phase1_allspeakers_raw",
                  currplotRow$syll.pos,
                  currplotRow$cbasicpal,
                  currplotRow$v,
                  currplotRow$frm.pos,
                  sep="_")
  
  makeultRawPDF(currplotRow,
                filename,
                allspeakers = T)
  
  print(filename)
}

rm(k)
  