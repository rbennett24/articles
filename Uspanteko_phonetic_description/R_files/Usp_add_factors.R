# http://stat.ethz.ch/R-manual/R-devel/library/base/html/grep.html
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html

usp.add.factors<- function(praat){

    # Get number of segments measured.
    seg.count<-nrow(praat)
    
    # Create vectors for storing new factor values.
    stress<-rep("NA",seg.count)
    v.qual<-rep("NA",seg.count)
    v.height<-rep("NA",seg.count)
    v.back<-rep("NA",seg.count)
    v.len<-rep("NA",seg.count)
    v.tone<-rep("NA",seg.count)
    v.str.len<-rep("NA",seg.count)
    v.str.tone<-rep("NA",seg.count)
    prec.c.wd.class<-rep("NA",seg.count)
    next.c.wd.class<-rep("NA",seg.count)
    prec.c.wd.lar<-rep("NA",seg.count)
    next.c.wd.lar<-rep("NA",seg.count)
    prec.c.wd.pl<-rep("NA",seg.count)
    next.c.wd.pl<-rep("NA",seg.count)
    
    # Get the label of each segment.
    for (n in 1:seg.count){
        
        seg.lab<-praat[n,"segment"]
        lastGraph<-nchar(as.character(seg.lab))
        
        # Code for stress.
        if(substr(seg.lab,lastGraph,lastGraph)==1){
            stress.code<-1
            } else if (substr(seg.lab,lastGraph,lastGraph)==0){
            stress.code<-0
            } else {
            stress.code<-"NA"
            }
        
        stress[n]<-stress.code

        
        # Code for vowel quality.
        #
        # Get the initial character of the segment label.
        seg.v<-substring(seg.lab,1,1)
                
        if(seg.v %in% c("A","E","I","O","U")){
            v.qual.code<-tolower(seg.v)
            } else {
            v.qual.code<-"NA"
            #cat("I don't know the vowel quality of ",as.character(seg.lab),". Oh no!", "\n",sep="")
            }
        
        v.qual[n]<-v.qual.code

        
        # Code for vowel height.
        #
        if(seg.v %in% c("A")){
            v.height.code<-"Low"
            } else if(seg.v %in% c("E","O")){
            v.height.code<-"Mid"
            } else if(seg.v %in% c("I","U")){
            v.height.code<-"High"
            } else {    
            v.height.code<-"NA"
            #cat("I don't know the vowel height of ",as.character(seg.lab)," on line ",n,". Oh no!", "\n",sep="")
            }

        
        v.height[n]<-v.height.code

        
        # Code for vowel backness.
        #
        if(seg.v %in% c("A")){
            v.back.code<-"Central"
            } else if(seg.v %in% c("E","I")){
            v.back.code<-"Front"
            } else if(seg.v %in% c("O","U")){
            v.back.code<-"Back"
            } else {    
            v.back.code<-"NA"
            #cat("I don't know the vowel height of ",as.character(seg.lab)," on line ",n,". Oh no!", "\n",sep="")
            }

        
        v.back[n]<-v.back.code

        
        # Code for tone.
        #
        match.tone<-grepl("[AEIOU][LS]T", seg.lab)
        
        if(match.tone==1){
          vtone.code<-1
        } else {
          vtone.code<-0
        }
        
        v.tone[n]<-vtone.code
        
        
        # Code for vowel length.
        #
        match.long<-grepl("[AEIOU]L", seg.lab)
        match.short<-grepl("[AEIOU]S", seg.lab)
        
        if(match.long==1){
            vlen.code<-"Long"
            } else if (match.short==1) {
            vlen.code<-"Short"
            } else {
            vlen.code<-"NA"
            }
        
        v.len[n]<-vlen.code
        
        # Merge vowel length and stress into a single code.
        match.str.long<-grepl("[AEIOU]L", seg.lab)
        match.unstr.short<-grepl("[AEIOU]S0", seg.lab)
        match.str.short<-grepl("[AEIOU](S|ST)1", seg.lab)
        #match.str.short.tone<-grepl("[AEIOU]ST1", seg.lab)
        
                
        if(match.str.long==1){
              v.str.len.code<-"\u2C8V\u2D0"
            } else if (match.unstr.short==1) {
              v.str.len.code<-"V"
            } else if (match.str.short==1) {
              v.str.len.code<-"\u2C8V"
            # } else if (match.str.short.tone==1) {
            #   v.str.len.code<-"Short, stressed, tonal"
            } else {
              v.str.len.code<-"NA"
            }
        
        v.str.len[n]<-v.str.len.code
        

        # Merge tone and stress into a single code.
        v.str.tone[n]<-as.numeric(v.tone[n])+as.numeric(stress[n])

        
        # Segmental environment
        #
        prec.c.wd<-praat[n,"prec.seg.wd"]
        next.c.wd<-praat[n,"next.seg.wd"]
        
        len.prec.c<-nchar(as.character(prec.c.wd))
        len.next.c<-nchar(as.character(next.c.wd))
        
        other.glots<-c("GS")
        plain.stops<-c("P","T","K","Q")
        plain.aff<-c("TS","CH")
        sonorants<-c("W","Y","R","L","N","M","NP")
        fricatives<-c("S","SH","H")
        
        # Code for consonant class of the flanking segments.
        # Preceding segment.
        if (is.na(prec.c.wd)==T){
            prec.c.cls.code<-"NA"
            } else if(substr(prec.c.wd,len.prec.c,len.prec.c)=="G"|prec.c.wd%in%other.glots){
            prec.c.cls.code<-"Glottalized stop"
            } else if (substr(prec.c.wd,1,1)%in%plain.stops|substr(prec.c.wd,1,2)%in%plain.aff){
            prec.c.cls.code<-"Voiceless stop"
            } else if (substr(prec.c.wd,1,1)%in%sonorants){
            prec.c.cls.code<-"Sonorant"
            } else if (substr(prec.c.wd,1,1)%in%sonorants & substr(prec.c.wd,len.prec.c,len.prec.c)=="D"){
            prec.c.cls.code<-"Sonorant (devoiced)"
            } else if (substr(prec.c.wd,1,1)%in%fricatives){
            prec.c.cls.code<-"Fricative"
            } else {
            prec.c.cls.code<-"Other"
            }
        
        prec.c.wd.class[n]<-prec.c.cls.code
        
        # Following segment.
        if (is.na(next.c.wd)==T){
            next.c.cls.code<-"NA"
            } else if(substr(next.c.wd,len.next.c,len.next.c)=="G"|next.c.wd%in%other.glots){
            next.c.cls.code<-"Glottalized stop"
            } else if (substr(next.c.wd,1,1)%in%plain.stops|substr(next.c.wd,1,2)%in%plain.aff){
            next.c.cls.code<-"Voiceless stop"
            } else if (substr(next.c.wd,1,1)%in%sonorants){
            next.c.cls.code<-"Sonorant"
            } else if (substr(next.c.wd,1,1)%in%sonorants & substr(next.c.wd,len.next.c,len.next.c)=="D"){
            next.c.cls.code<-"Sonorant (devoiced)"
            } else if (substr(next.c.wd,1,1)%in%fricatives){
            next.c.cls.code<-"Fricative"
            } else {
            next.c.cls.code<-"Other"
            }
        
        next.c.wd.class[n]<-next.c.cls.code

        
        # Code for laryngeal state of the flanking segments.
        # Preceding segment.
        if (is.na(prec.c.wd)==T){
            prec.c.lar.code<-"NA"
            } else if(prec.c.wd%in%paste(plain.stops,"G",sep="")|prec.c.wd%in%paste(plain.aff,"G",sep="")|prec.c.wd=="GS"){
            prec.c.lar.code<-"Voiceless glottalized stop"
            } else if (substr(prec.c.wd,1,1)%in%plain.stops|substr(prec.c.wd,1,2)%in%plain.aff|substr(prec.c.wd,1,1)%in%fricatives){
            prec.c.lar.code<-"Voiceless obstruent"
            } else if (prec.c.wd=="BG"){
            prec.c.lar.code<-"Implosive"
            } else if (substr(prec.c.wd,1,1)%in%sonorants & substr(prec.c.wd,len.prec.c,len.prec.c)=="D"){
            prec.c.lar.code<-"Sonorant (devoiced)"
            } else if (substr(prec.c.wd,1,1)%in%sonorants){
            prec.c.lar.code<-"Sonorant"
            } else {
            prec.c.lar.code<-"Other"
            }
        
        prec.c.wd.lar[n]<-prec.c.lar.code
        
        # Following segment.
        if (is.na(next.c.wd)==T){
            next.c.lar.code<-"NA"
            } else if(next.c.wd%in%paste(plain.stops,"G",sep="")|next.c.wd%in%paste(plain.aff,"G",sep="")|next.c.wd=="GS"){
            next.c.lar.code<-"Voiceless glottalized stop"
            } else if (substr(next.c.wd,1,1)%in%plain.stops|substr(next.c.wd,1,2)%in%plain.aff|substr(next.c.wd,1,1)%in%fricatives){
            next.c.lar.code<-"Voiceless obstruent"
            } else if (next.c.wd=="BG"){
            next.c.lar.code<-"Implosive"
            } else if (substr(next.c.wd,1,1)%in%sonorants & substr(next.c.wd,len.next.c,len.next.c)=="D"){
            next.c.lar.code<-"Sonorant (devoiced)"
            } else if (substr(next.c.wd,1,1)%in%sonorants){
            next.c.lar.code<-"Sonorant"
            } else {
            next.c.lar.code<-"Other"
            }
        
        next.c.wd.lar[n]<-next.c.lar.code
        
        
        # Code for place of the flanking segments.
        alveolar.a<-c("T","S","R","L","N")
        alveolar.b<-c("TS")
        postalveolar.a<-c("Y")
        postalveolar.b<-c("CH","SH","NP")
        labial.a<-c("P","W","M")
        labial.b<-c("BG")
        velar<-c("K")
        uvular<-c("Q","H")
        glottal<-c("GS")
        
        # Preceding segment.
        if (is.na(prec.c.wd)==T){
            prec.c.pl.code<-"NA"
            } else if (substr(prec.c.wd,1,1)%in%postalveolar.a|substr(prec.c.wd,1,2)%in%postalveolar.b){
            prec.c.pl.code<-"Postalveolar"
            } else if(substr(prec.c.wd,1,1)%in%alveolar.a|substr(prec.c.wd,1,2)%in%alveolar.b){
            prec.c.pl.code<-"Alveolar"
            } else if (substr(prec.c.wd,1,1)%in%labial.a|substr(prec.c.wd,1,2)%in%labial.b){
            prec.c.pl.code<-"Labial"
            } else if (substr(prec.c.wd,1,1)%in%velar){
            prec.c.pl.code<-"Velar"
            } else if (substr(prec.c.wd,1,1)%in%uvular){
            prec.c.pl.code<-"Uvular"
            } else if (substr(prec.c.wd,1,2)%in%glottal){
            prec.c.pl.code<-"Glottal"
            } else {
            prec.c.pl.code<-"Other"
            }
        
        prec.c.wd.pl[n]<-prec.c.pl.code
        
        # Following segment.
        if (is.na(next.c.wd)==T){
            next.c.pl.code<-"NA"
            } else if (substr(next.c.wd,1,1)%in%postalveolar.a|substr(next.c.wd,1,2)%in%postalveolar.b){
            next.c.pl.code<-"Postalveolar"
            } else if(substr(next.c.wd,1,1)%in%alveolar.a|substr(next.c.wd,1,2)%in%alveolar.b){
            next.c.pl.code<-"Alveolar"
            } else if (substr(next.c.wd,1,1)%in%labial.a|substr(next.c.wd,1,2)%in%labial.b){
            next.c.pl.code<-"Labial"
            } else if (substr(next.c.wd,1,1)%in%velar){
            next.c.pl.code<-"Velar"
            } else if (substr(next.c.wd,1,1)%in%uvular){
            next.c.pl.code<-"Uvular"
            } else if (substr(next.c.wd,1,2)%in%glottal){
            next.c.pl.code<-"Glottal"
            } else {
            next.c.pl.code<-"Other"
            }
        
        next.c.wd.pl[n]<-next.c.pl.code
        
        #
        # Stop adding factors.
        #
        }
    
  # Compile the output frame.
  new.frame<-cbind(praat,stress,v.tone,v.qual,v.height,v.back,v.len,v.str.len,v.str.tone,prec.c.wd.class,next.c.wd.class,prec.c.wd.lar,next.c.wd.lar,prec.c.wd.pl,next.c.wd.pl)
      
  # Reorder levels of v.qual so that they're sorted in terms of height.
  new.frame$v.qual <- factor(new.frame$v.qual, levels = c("a","e","o","i","u","NA"))
      
  # Reorder levels of v.height so that they're sorted in terms of height.
  new.frame$v.height <- factor(new.frame$v.height, levels = c("Low","Mid","High","NA"))
      
  # Reorder levels of v.height so that they're sorted in terms of backness.
  new.frame$v.back <- factor(new.frame$v.back, levels = c("Front","Central","Back","NA"))
      
  # Reorder levels of v.len so that they're sorted Short,Long
  new.frame$v.len <- factor(new.frame$v.len, levels = c("Short","Long"))
  
  # Reorder levels of merged stress/length vector (v.str.len) they're sorted Stressed/Tense -> Stressed/Lax -> Unstresed/Tense.
  new.frame$v.str.len <- factor(new.frame$v.str.len, levels = c("V","\u2C8V","\u2C8V\u2D0"))
      
  new.frame$prec.c.wd.class<-factor(new.frame$prec.c.wd.class, levels = c("Glottalized stop","Voiceless stop","Fricative","Sonorant","Sonorant (devoiced)","Other"))
  new.frame$next.c.wd.class<-factor(new.frame$next.c.wd.class, levels = c("Glottalized stop","Voiceless stop","Fricative","Sonorant","Sonorant (devoiced)","Other"))
  new.frame$prec.c.wd.lar<-factor(new.frame$prec.c.wd.lar, levels = c("Implosive","Voiceless glottalized stop","Voiceless obstruent","Sonorant","Sonorant (devoiced)","Other"))
  new.frame$next.c.wd.lar<-factor(new.frame$next.c.wd.lar, levels = c("Implosive","Voiceless glottalized stop","Voiceless obstruent","Sonorant","Sonorant (devoiced)","Other"))
  new.frame$prec.c.wd.pl<-factor(new.frame$prec.c.wd.pl, levels = c("Labial","Alveolar","Postalveolar","Velar","Uvular","Glottal","Other"))
  new.frame$next.c.wd.pl<-factor(new.frame$next.c.wd.pl, levels = c("Labial","Alveolar","Postalveolar","Velar","Uvular","Glottal","Other"))
      
      
  # Convert other columns to factors.
  # 
  new.frame$stress<-as.factor(new.frame$stress)
  new.frame$v.tone<-as.factor(new.frame$v.tone)
  new.frame$v.str.tone<-as.factor(new.frame$v.str.tone)
 
  
  # Use newly defined columns to define the position of vowels in each word.
  # You could probably do this cleaner by just coding final vowels,
  # and treating non-final as the default case (or vice-versa).
  v.pos<-rep(NA,nrow(new.frame))
  new.frame<-cbind(new.frame,v.pos)
  levels(new.frame$v.pos)<-c("Non-final","Final")
  for (k in 1:nrow(new.frame)){
    seg.row<-new.frame[k,]
    # Final long vowels
    if (seg.row$v.len=="Long"){
      new.frame$v.pos[k]<-"Final"
    }
    # Final non-tonal short vowels
    else if (seg.row$stress==1 & seg.row$v.len=="Short" & seg.row$v.tone==0){
      new.frame$v.pos[k]<-"Final"
    }
    # Penultimate tonal short vowels
    else if (seg.row$stress==1 & seg.row$v.len=="Short" & seg.row$v.tone==1){
      new.frame$v.pos[k]<-"Non-final"
    }
    # Unstressed vowels -- here it gets complicated, and will slow down processing.
    else if (seg.row$stress==0){
      # Pop off numerical vowel code at end of string, and use for matching vowels in the same
      # word token.
      token.code.word<-gsub("-[0-9]+$","",seg.row[,"token.code"])
      segs.in.wd<-subset(new.frame,gsub("-[0-9]+$","",token.code)==token.code.word)
      
      # Check to see if there are any tonal short vowels in the word...
      tonal.short<-subset(segs.in.wd,v.tone==1 & v.len=="Short")
      
      # If there AREN'T any tonal short vowels in the word, you know that all unstressed short vowels are non-final.
      if (nrow(tonal.short)==0){
        new.frame$v.pos[k]<-"Non-final"  
      }
      # But if there ARE tonal short vowels in the word, things are more complicated again...
      else{
        # Figure out of the unstressed vowel precedes or follows the tonal short vowel.
        tone.index<-unique(as.numeric(rownames(tonal.short)))
        unstr.v.index<-unique(as.numeric(rownames(seg.row)))
        if (tone.index<unstr.v.index){
          new.frame$v.pos[k]<-"Final"  
        } else {
          new.frame$v.pos[k]<-"Non-final"
        }
      }
    }
  }
  
  return(new.frame)
}