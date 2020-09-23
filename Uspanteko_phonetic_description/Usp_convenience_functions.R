######################################################
# Convert Hz to Bark
# Uses the formula in:
# H. Traunmüller (1990) "Analytical expressions for the tonotopic sensory scale" J. Acoust. Soc. Am. 88: 97-100. 
# Traunmuller, Hartmut. 1997. Auditory scales of frequency representation. [ Online: http://www.ling.su.se/staff/hartmut/bark.htm ]
# z = [26.81 / (1 + 1960 / f )] - 0.53, with f in Hz
######################################################
hzToBark<-function(hz){
  return((26.81 / (1 + 1960 / hz )) - 0.53)
}


######################################################
# Convert Hz to ERB
# Following Praat:
# 11.17 * ln ((Hz + 312) / (Hz + 14680)) + 43
# http://www.fon.hum.uva.nl/praat/manual/Formulas_5__Mathematical_functions.html
######################################################
hzToERB<-function(hz){
  return(11.17 * log((hz + 312) / (hz + 14680)) + 43)
}

######################################################
#
# Compute z-scores.
# Writing this function is essentially academic;
# the R scale() function computes z-scores when 
# default parameter values are chosen.
#
#
######################################################
z.score<-function(dfrm,cname,logfirst=F){
    
    # Create null vector for holding z-scores.
    zscore.vec<-c()
    
    # For each speaker.
    for (spk in unique(dfrm$speaker)){

        # Extract the speaker's data
        spk.data<-subset(dfrm,speaker==spk)
        
        col.dat<-as.numeric(spk.data[,cname])
        
        
#         # Get the mean value and standard deviation
#         # of the column/measure of interest
#         # for that speaker.
#         col.avg<-mean(na.rm=T,coldat)
#         col.sd<-sd(na.rm=T,coldat)
#         
#         # Convert the data column to z-scores.
#         col.zscore<-(col.dat-col.avg)/col.sd
        
        if (logfirst==F){
          # Compute z-scores for the column you chose.
          col.zscore<-scale(col.dat)
        }else{
          col.zscore<-scale(log(col.dat))
        }
        
        # Add speakers's z-scores to z-score output vector.
        zscore.vec.hold<-c(zscore.vec,col.zscore)
        zscore.vec<-zscore.vec.hold
    }
    
    # Bind z-score column to the original input data frame.
    out.frame<-cbind(dfrm,zscore.vec)
    
    # Give the z-score column a sensible name
    col.indx<-ncol(out.frame)
    colnames(out.frame)[col.indx]<-paste(cname,".zscore",sep="")
    
    return(out.frame)
}



######################################################################################

# Define a custom function for plotting number of observations in each condition.
#
# http://stackoverflow.com/questions/15660829/how-to-add-a-number-of-observations-per-group-and-use-group-mean-in-ggplot2-boxp
#
give.n <- function(x,ypos=0){
  #  return(c(y = 40, label = length(x)))
  #
  # Setting the value of y here affects the vertical positioning of the labels.
  data.frame(y = ypos, label = paste0("n=",length(x)))
}

give.n.phon <- function(x,ypos=-2.6){
  #  return(c(y = 40, label = length(x)))
  #
  # Setting the value of y here affects the vertical positioning of the labels.
  data.frame(y = ypos, label = paste0("n=",length(x)))
}



######################################################################################

# Define a custom function for plotting two charts side-by-side.
# 
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# 
multiplot<-function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}




######################################################################################
#
# Do Lobanov normalization over F1-F3
# Two different functions defined: one operating over all measurement steps,
# and one operating over single measurement steps.

# library(vowels)
# 
# You can do Lobanov transformation using the vowels::norm.lobanov()
# function, but I prefer to do it by hand so that you can control
# the output and plot using ggplot2 (the vowels package uses plotnik or something).
# 
# You would probably have to reshape your Praat output data anyway to use vowels::norm.lobanov()
# 
# Lobanov normalization is calculated after Adank et al. (2004).
# It is essentially a z-score transformation:
# 
# F.L.ti.x = (F.ti.x - mean.ti)/SD.ti
#
# Where:
# x = token x
# t = talker index
# i = formant index
# F.L. = Lobanov-corrected frequency
# F.L.ti.x = Lobanov-corrected frequency (for formant i, for token x, for speaker t)
# mean.ti = the average frequency for formant i across all
#           (monophthongal) vowel tokens for talker t
# SD.ti = the standard deviation for formant i across all
#           (monophthongal) vowel tokens for talker t
#
# Define a function for Lobanov transformation:
# This function assumes that your input data frame contains single-point values
# for each vowel token.
# 
lob.v.trans.onestep<-function(voweldata){
    
    # Supply a list of header names for the output data frame.
    lob.head.names<-c("segment","word","token.code","F1","F1.lob","F2","F2.lob",
                      "F3","F3.lob","prec.seg.wd","next.seg.wd","speaker")
    
    # Figure out how long the resulting output frame should be.
    # It should be equal to the number of distinct vowel tokens.
    outframe.len<-length(unique(voweldata$token.code))

    # Create a null data frame for holding data
    dummymatrix<-matrix(NA,ncol=length(lob.head.names),nrow=outframe.len)
    lob.frame<-data.frame(dummymatrix)

    # Fix header names
    names(lob.frame)<-lob.head.names

    # Start a counter for adding data to this data frame
    # one row at a time.
    token.counter<-1
    
    # Begin applying Lobanov normalization.
    for (the.spkr in unique(voweldata$speaker)){

        # Subset out each speaker's data.
        spk.data<-subset(voweldata,speaker==the.spkr)
        
        # Subset out raw F1-F3 values.
        F1.raw<-subset(spk.data,
                   formant=="F1"
                  )
        
        F2.raw<-subset(spk.data,
                   formant=="F2"
                  )

        F3.raw<-subset(spk.data,
                   formant=="F3"
                  )

        
        # Get mean F1-F3 across all vowel tokens.
        F1.mean<-mean(na.rm=T,F1.raw$freq)
        F2.mean<-mean(na.rm=T,F2.raw$freq)
        F3.mean<-mean(na.rm=T,F3.raw$freq)
    
        # Get standard deviations for F1-F3 across
        # all vowel tokens.
        F1.sd<-sd(na.rm=T,F1.raw$freq)
        F2.sd<-sd(na.rm=T,F2.raw$freq)
        F3.sd<-sd(na.rm=T,F3.raw$freq)
        
        # Get Lobanov normalized values for each token.
        for (v.token in unique(spk.data$token.code)){
            
            # Extract data for each token.
            token.data<-subset(spk.data,token.code==v.token)

            # Get F1 and F2 values for that token.
            token.F1<-subset(token.data,formant=="F1")$freq
            token.F2<-subset(token.data,formant=="F2")$freq
            token.F3<-subset(token.data,formant=="F3")$freq
                
            # Apply Lobanov normalization for that token.
            F1.lob<-(token.F1 - F1.mean)/F1.sd
            F2.lob<-(token.F2 - F2.mean)/F2.sd
            F3.lob<-(token.F3 - F3.mean)/F3.sd
        
            # Create a vector for storing the results.
            lob.vec<-c(as.character(unique(token.data$segment)),
                        as.character(unique(token.data$word)),
                        as.character(unique(token.data$token.code)),
                        token.F1,
                        F1.lob,
                        token.F2,
                        F2.lob,
                        token.F3,
                        F3.lob,
                        as.character(unique(token.data$prec.seg.wd)),
                        as.character(unique(token.data$next.seg.wd)),
                        the.spkr
                        )
            
            # Store the results of Lobanov normalization.
            lob.frame[token.counter,]<-lob.vec
            token.counter<-token.counter+1

        }

    }

    # Make sure that data types are correct for the output data frame.
    lob.frame$segment<-as.factor(lob.frame$segment)
    lob.frame$word<-as.factor(lob.frame$word)
    lob.frame$token.code<-as.factor(lob.frame$token.code)
    lob.frame$speaker<-as.factor(lob.frame$speaker)
    lob.frame$prec.seg.wd<-as.factor(lob.frame$prec.seg.wd)
    lob.frame$next.seg.wd<-as.factor(lob.frame$next.seg.wd)
    
    lob.frame$F1<-as.numeric(lob.frame$F1)
    lob.frame$F1.lob<-as.numeric(lob.frame$F1.lob)
    lob.frame$F2<-as.numeric(lob.frame$F2)
    lob.frame$F2.lob<-as.numeric(lob.frame$F2.lob)
    lob.frame$F3<-as.numeric(lob.frame$F3)
    lob.frame$F3.lob<-as.numeric(lob.frame$F3.lob)
    
    return(lob.frame)
    
}


#########################################

lob.v.trans.allsteps<-function(voweldata,dropcols=c("freq","formant")){
    
    # Supply a list of header names for the output data frame.
    basenames<-subset(names(voweldata),!(names(voweldata)%in%dropcols))
    lob.head.names<-c(basenames,"F1","F1.lob","F2","F2.lob","F3","F3.lob")
    
    # Figure out how long the resulting output frame should be.
    # It should be equal to the number of distinct vowel tokens multiplied
    # by the number of distinct steps per vowel.
    outframe.len<-length(unique(voweldata$token.code))*length(unique(voweldata$step))

    # Create a null data frame for holding data
    dummymatrix<-matrix(NA,ncol=length(lob.head.names),nrow=outframe.len)
    lob.frame<-data.frame(dummymatrix)

    # Fix header names
    names(lob.frame)<-lob.head.names

    # Start a counter for adding data to this data frame
    # one row at a time.
    token.counter<-1
    
    # Begin applying Lobanov normalization.
    for (the.spkr in unique(voweldata$speaker)){

        # Subset out each speaker's data.
        spk.data<-subset(voweldata,speaker==the.spkr)
        
        # Subset out raw F1-F3 values.
        F1.raw<-subset(spk.data,
                   formant=="F1"
                  )
        
        F2.raw<-subset(spk.data,
                   formant=="F2"
                  )

        F3.raw<-subset(spk.data,
                   formant=="F3"
                  )

        
        # Get mean F1-F3 across all vowel tokens/steps.
        F1.mean<-mean(na.rm=T,F1.raw$freq)
        F2.mean<-mean(na.rm=T,F2.raw$freq)
        F3.mean<-mean(na.rm=T,F3.raw$freq)
    
        # Get standard deviations for F1-F3 across
        # all vowel tokens/steps.
        F1.sd<-sd(na.rm=T,F1.raw$freq)
        F2.sd<-sd(na.rm=T,F2.raw$freq)
        F3.sd<-sd(na.rm=T,F3.raw$freq)
        
        # Get Lobanov normalized values for each token.
        for (v.token in unique(spk.data$token.code)){
            
            for (v.step in unique(spk.data$step)){
            
                # Extract data for each token and step.
                token.data<-subset(spk.data,token.code==v.token&step==v.step)
    
                # Get F1-F3 values for that token/step.
                token.F1<-subset(token.data,formant=="F1")$freq
                token.F2<-subset(token.data,formant=="F2")$freq
                token.F3<-subset(token.data,formant=="F3")$freq
                    
                # Apply Lobanov normalization for that token/step.
                F1.lob<-(token.F1 - F1.mean)/F1.sd
                F2.lob<-(token.F2 - F2.mean)/F2.sd
                F3.lob<-(token.F3 - F3.mean)/F3.sd
            
                # Create a vector for storing the results.
    #             lob.vec<-c(as.character(unique(token.data$segment)),
    #                         as.character(unique(token.data$word)),
    #                         as.character(unique(token.data$token.code)),
    #                         token.F1,
    #                         F1.lob,
    #                         token.F2,
    #                         F2.lob,
    #                         as.character(unique(token.data$prec.seg.wd)),
    #                         as.character(unique(token.data$next.seg.wd)),
    #                         the.spkr
    #                         )
                token.data.vec<-token.data[1,!(names(token.data)%in%dropcols)]
                lob.vec<-c(as.matrix(token.data.vec),token.F1,F1.lob,token.F2,F2.lob,
                                                     token.F3,F3.lob)
                
                # Store the results of Lobanov normalization.
                lob.frame[token.counter,]<-lob.vec
                token.counter<-token.counter+1
                
                # Fix header names
                # (you might want to move this outside of the for-loop, I'm not sure)
                names(lob.frame)<-lob.head.names

            }
        }

    }

    # Make sure that data types are correct for the output data frame.
    lob.frame$segment<-as.factor(lob.frame$segment)
    lob.frame$word<-as.factor(lob.frame$word)

    lob.frame$token.code<-as.factor(lob.frame$token.code)
    lob.frame$speaker<-as.factor(lob.frame$speaker)

    lob.frame$prec.seg.wd<-as.factor(lob.frame$prec.seg.wd)
    lob.frame$next.seg.wd<-as.factor(lob.frame$next.seg.wd)
    lob.frame$next.seg.wd<-as.factor(lob.frame$next.seg.wd)

    lob.frame$prec.seg<-as.factor(lob.frame$prec.seg)
    lob.frame$next.seg<-as.factor(lob.frame$next.seg)
    
    lob.frame$prec.c.wd.class<-as.factor(lob.frame$prec.c.wd.class)
    lob.frame$next.c.wd.class<-as.factor(lob.frame$next.c.wd.class)
    lob.frame$prec.c.wd.lar<-as.factor(lob.frame$prec.c.wd.lar)
    lob.frame$next.c.wd.lar<-as.factor(lob.frame$next.c.wd.lar)
    lob.frame$prec.c.wd.pl<-as.factor(lob.frame$prec.c.wd.pl)
    lob.frame$next.c.wd.pl<-as.factor(lob.frame$next.c.wd.pl)
        
    lob.frame$step<-as.factor(lob.frame$step)
    
    lob.frame$stress<-as.factor(lob.frame$stress)
    lob.frame$v.qual<-as.factor(lob.frame$v.qual)
    lob.frame$v.height<-as.factor(lob.frame$v.height)
    lob.frame$v.back<-as.factor(lob.frame$v.back)
    lob.frame$v.len<-as.factor(lob.frame$v.len)
    lob.frame$v.str.len<-as.factor(lob.frame$v.str.len)
    
    lob.frame$dur<-as.numeric(lob.frame$dur)
    lob.frame$F1<-as.numeric(lob.frame$F1)
    lob.frame$F1.lob<-as.numeric(lob.frame$F1.lob)
    lob.frame$F2<-as.numeric(lob.frame$F2)
    lob.frame$F2.lob<-as.numeric(lob.frame$F2.lob)
    lob.frame$F3<-as.numeric(lob.frame$F3)
    lob.frame$F3.lob<-as.numeric(lob.frame$F3.lob)
    lob.frame$start.time<-as.numeric(lob.frame$start.time)
    lob.frame$end.time<-as.numeric(lob.frame$end.time)
    
    return(lob.frame)
    
}
