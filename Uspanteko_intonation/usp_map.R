
# For mapping
library(maps)

# For high-resolution national outlines
library(mapdata)

# For working with shapefiles
library(maptools)

# For using colors with different transparency levels.
library(scales)

# For working with maps that use different projections.
library(rgdal)

##############################################
#
# Plotting Uspanteko-relevant towns
# 
###############################################

computer <- "Tiamat"
rootdir <- paste0("C:/Users/",computer,"/Dropbox/Research/Mayan/")
datadir <- paste0(rootdir,"Kaqchikel/Kaq_corpus/Maps/")
outdir <- paste0(rootdir,"Uspanteko/Uspanteko_NSF_project/Articles/Intonation/Images/Map/")

cairo_pdf(file=paste0(outdir,"usp_full_guate.pdf"),
          width=8,height=7)


# Plot external boundaries, and fill with grey.
# Guatemala only.
map("worldHires",
    c("Guatemala"
    ),
    # xlim=c(-92.3,-88.2),
    # ylim=c(13.8,17.82),
    fill=T,col="gray95"
)


# Plot internal waterways in Guatemala.
# You got the shapefiles here:
# http://www.diva-gis.org/gdata
#
# This plots some bonkers stuff up north
# in the Peten, probably because it includes lagoons, estuaries, and maybe underground reservoirs.
#
# You just want to plot Lake Atitlan, Lake Petén Itzá, and Lake Izabal (any other really major ones?)

guat.waters<-readOGR(paste0(datadir,"GTM_water_areas_dcw.shp"),encoding="UTF8")

lakes<-subset(guat.waters,grepl("LAGO",guat.waters$NAME))

# Plot Guatemalan lakes with 50% transparency
plot(lakes,col=alpha("darkblue", 0.5),
     border=F,
     add=T)


# For rivers and whatnot
# Not a very good set of rivers
# rivers<-subset(guat.waters,grepl("RIO",guat.waters$NAME))

guat.rivers<-readOGR(paste0(datadir,"GTM_water_lines_dcw.shp"),encoding="UTF8")

big.rivers<-subset(guat.rivers,grepl("RIO",guat.rivers$NAM))

# Plot Guatemalan rivers with 50% transparency
plot(big.rivers,col=alpha("darkblue", 0.5),
     add=T)

guat.admin<-readOGR(paste0(datadir,"GTM_adm1.shp"),encoding="UTF8")
guat.muni<-readOGR(paste0(datadir,"GTM_adm2.shp"),encoding="UTF8")
# GTM_adm2.shp makes a wider set of distinctions, for some reason.

muni.names<-c("Uspantán")
dept.names<-c("Quiché")

usp<-subset(guat.muni,NAME_2 %in% muni.names)
quiche<-subset(guat.admin,NAME_1 %in% dept.names)

plot(usp,col=alpha("red", 0.4),
add=T)

# plot(quiche,col=alpha("red", 0.2),
#        add=T)

# Inset box
rect(xleft=-90.925,
     xright=-90.7,
     ybottom=15.3,
     ytop=15.475,
     lty="dashed"
     # col=alpha("darkgreen", 0.2),
     # add=T
)

# 
# text(-90.1,
#      15.25,
#      "Uspanteko\n(~2500 speakers)",
#      cex=1.1,
#      font=2
# )

# Add reference cities.

# Guatemala City
points(-90.535278,14.613333,
       col="black",pch=19,cex=1.5)

text(-90.14,
     14.45,
     "Guatemala\n City",
     cex=1,
     font=2
)


# Antigua
points(-90.733333,14.566667,
       col="black",pch=19,cex=1.5)

text(-91.05,
     14.51,
     "Antigua",
     cex=1,
     font=2
)


# Cobán
points(-90.475,15.483333,
       col="black",pch=19,cex=1.5)
text(-90.2,
     15.5,
     "Cobán",
     cex=1,
     font=2
)


# Santa Cruz del Quiché
points(-91.15,15.03,
       col="black",pch=19,cex=1.5)

text(-91.57,
     14.925,
     "Santa Cruz\n del Quiché",
     cex=1,
     font=2
)

# SM Uspantán
points(-90.869444,15.345833,
       col="black",pch=19,cex=1.5)

text(-91.35,15.345833,
     "San Miguel\nUspantán",
     cex=1,
     font=2
)



# Add scale
map.scale(ratio=F,
          x=-89.6
)


dev.off()


####################
# Plot inset
####################
cairo_pdf(file=paste0(outdir,"usp_towns.pdf"),
          width=8,height=7)

# Plot base Guatemala map with rivers, road, elevation, etc., but use limits
map("worldHires",
    c("Guatemala"
    ),
    xlim=c(-90.925,-90.7),
    ylim=c(15.3,15.475),
    fill=T,col="gray95"
)

# Plot Guatemalan lakes with 50% transparency
plot(lakes,col=alpha("darkblue", 0.5),
     border=F,
     add=T)

# Plot Guatemalan rivers with 50% transparency
plot(big.rivers,col=alpha("darkblue", 0.5),
     add=T)

# Add roads
guat.roads<-readOGR(paste0(datadir,"GTM_roads.shp"))

plot(guat.roads,col=alpha("darkred", 0.7),
     add=T)


# SM Uspantán
points(-90.869444,15.345833,
       col="black",pch=19,cex=1.25)

text(-90.90,15.35,
     "San Miguel\nUspantán",
     cex=1.5,
     font=2
)


# Las Pacayas
points(-90.77375,15.42855,
       col="black",pch=19,cex=1.25)

text(-90.77375,15.434,
     "Las Pacayas",
     cex=1.5,
     font=2
)


# Chipaj
points(-90.781361,15.384600,
       #-90.763164,15.431932
       col="black",pch=19,cex=1.25)

text(-90.781361,15.391,
     "Chipaj",
     cex=1.5,
     font=2
)


# Add scale
map.scale(ratio=F,
          x=-90.75,
          cex=1.75
)


dev.off()

####################
# Plot elevation
####################

# To add elevation maps, you'll need to figure out how to work with raster/grid files. The code below is totally bunk.
# See also RgoogleMaps stuff at 
# http://www.molecularecologist.com/2012/09/making-maps-with-r/
#
# Add elevations
# guat.elevs<-readOGR(paste0(datadir,"GTM_msk_alt.grd"))

# plot(guat.elevs,col=alpha("darkred", 0.5),
#       add=T)
