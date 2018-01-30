# This code will compute a 2 dimensional density distribution based on points (in this case fatal crash locations), and then 
# identify the density value at a seperate input point (for example, a given signalized intersection).  This can be extended
# to add various densities on top of each other (i.e. fatal crashes, non-fatal crashes, etc.) which can be weighted differently
# and added to get a composite density value.


library(RCurl)
library(dplyr)
library(ggtern)
library(MASS)
library(ggmap)
library(ggplot2)


# DATA IMPORTING AND CLEANING
  
  # We will start with the same fatal crash data set from the Basic Density Heat Mapping code.
  crashes <-read.csv( text = getURL("https://raw.githubusercontent.com/LMcDiffett/DensityHeatMap/master/Fatal_Crash_Data_Spokane_WA.csv", ssl.verifypeer = FALSE), header = T, stringsAsFactors = F)
  
  # clean the data in case there are missing or abnormal GPS coordinates
  # we can ensure that no impossible coordinates are present by limiting Lat / Long to -180 <= x <= 180
  crashes <- crashes %>% filter( !is.na(LATITUDE) , !is.na(LONGITUDE), LATITUDE >= -180, LATITUDE <= 180, LONGITUDE >= -180, LONGITUDE <= 180)
  
  # only interested in crashes over the last five years of available data (2010-2015)
  crashes <- crashes %>% filter(YEAR >= 2010)


# CALCULATING DENSITY  

  
  #Next we will need to calculate the 2 dimensional spatial density using the weighted 2 dimensional kernel function in ggtern package
    # side note:  this could be done without using the weighted kernal function, but using weighted gives the option of using input
    # data that may be aggregated.  For example, if you had intersection locations and the crash volume at each intersection instead of points
    # for each crash, you can still used that data.  Since we aren't aggregating, we will simply give everything a weight of 1.
  
  # going to load this package JUST for the weighted kde2d function, then remove it
  
  
  # let's define the boundary and bandwidth of the density mapping using maximum and minimum coordinates in data set
  boundary <- c( min(crashes$LONGITUDE) + sign(crashes[1,]$LONGITUDE) * 0.2,
                 max(crashes$LONGITUDE) + -sign(crashes[1,]$LONGITUDE) * 0.2,
                 min(crashes$LATITUDE) + sign(crashes[1,]$LATITUDE) * 0.2,
                 max(crashes$LATITUDE) + -sign(crashes[1,]$LATITUDE) * 0.2)
  
  bandwidth <-c( bandwidth.nrd(crashes$LONGITUDE), bandwidth.nrd(crashes$LATITUDE))
  
  # here, we will set the weight = 1 for all rows since we are not aggregating.  If you had a column with aggregation, that would be your weight column
  # and you would simply change the w parameter in the kde2d.weighted function to reference that column
  crashes$weight <- 1
  
  
  crashDensity <- kde2d.weighted( crashes$LONGITUDE, crashes$LATITUDE, h = bandwidth,
                                          w = crashes$weight, lims = boundary, n = 500 )
  
 
  
  # this gives us a list of 3 elements which correspond to the n^2 number of pieces in the nxn grid.  For higher resolution increase n, for lower resolution decrease n
    # we will turn this grid into a set of coordinates corresponding to each point in the grid (n^2 pairs)
  crashDensity <- with(crashDensity, data.frame(expand.grid(x = x, y = y), z = as.vector(z)))
  colnames(crashDensity) <- c("LONGITUDE","LATITUDE","DENSITYSCORE")
  
  # let's scale the density score (z in the crashDensity list)
  crashDensity$DENSITYSCORESCALED <- crashDensity$DENSITYSCORE/max(crashDensity$DENSITYSCORE) 
  
# CREATING A COMPOSITE DENSITY
  
    # You could now go through the same steps in lines 50, 55, 56, and 59 to create another data frame with a different density (i.e. red light crashes, speeding crashes)
    # and then add those various density scores to create a composite density score.  Each density could have it's own weight in the overall score in this case.
  
# GRAPHING DENSITY
  
    # Once you have your density (or composite density), you can plot it using ggplot2 package 
  
  
  basemap <- get_map(location = "Spokane,WA",maptype = "roadmap", zoom = 12, color = "bw")
  
  ggmap(basemap, extent = "device")+
    stat_contour(data = crashDensity, aes(x =LONGITUDE, y = LATITUDE, z =DENSITYSCORESCALED, fill = ..level.., alpha = ..level..), geom = "polygon")+
    scale_fill_gradient(low = "green3", high = "red", guide = F) +
    scale_alpha(range = c(0.05, 0.2), guide = F)+
    labs( title = "Fatal Crash Density",
          caption = "Spokane (2010 - 2015)")
  
    

  
# EXTRACTING THE DENSITY SCORE
  # now that we've created a data frame containing nxn points our density map, we can extract the density score for abitrary points on the map
  # for this, i'll use 5 intersections in Spokane
  
  intersections <- data.frame( ID = c(1:5),
                               INTERSECTIONS = c("N DIVISION ST & W MISSION AVE",
                                                 "N DIVISION ST & W SHARP AVE",
                                                 "N DIVISION ST & W BUCKEYE AVE",
                                                 "N MAPLE ST & W BROADWAY AVE",
                                                 "N MAPLE ST & W MAXWELL AVE"),
                               LONGITUDE = c(-117.411132,
                                             -117.411150,
                                             -117.411169,
                                             -117.435489,
                                             -117.435552),
                               LATITUDE = c(47.671666,
                                            47.669486,
                                            47.682654,
                                            47.664354,
                                            47.670841))


  
  # looking at the intersections on the density map, we see that there are two that are in the highest density area (N MAPLE ST), with one being right on the boundary 
  # between red / orange.  Having a density score to extract will give us some more insight into whether the two points are actually considered just as risky 
  # or if one is riskier than the other.
  
  ggmap(basemap, extent = "device")+
    stat_contour(data = crashDensity, aes(x =LONGITUDE, y = LATITUDE, z =DENSITYSCORESCALED, fill = ..level.., alpha = ..level..), geom = "polygon")+
    scale_fill_gradient(low = "green3", high = "red", guide = F) +
    scale_alpha(range = c(0.05, 0.2), guide = F)+
    geom_point(data = intersections, aes(x = LONGITUDE, y = LATITUDE))+
    labs( title = "Fatal Crash Density",
          caption = "Spokane (2010 - 2015)")
  
  
  # Extracting density score
    # this will work by calculating the absolute difference in longitude and latitude, applying pythagorean theorem to find the distance from each 
    # grid point to the intersection (in decimal degrees), and then find the average of the 4 nearest points around the intersection (the square 
    # conatining the intersection).  If you make the squares sufficiently small, you get the value of the actual point!
  
  density.i <- crashDensity
  
  for(i in 1:nrow(intersections))
  {
    loc.i <- intersections[i,]
    density.i$xDiff <- abs(density.i$LONGITUDE - loc.i$LONGITUDE )
    density.i$yDiff <- abs(density.i$LATITUDE - loc.i$LATITUDE )
    density.i$xyDiff <- sqrt(density.i$xDiff^2 + density.i$yDiff^2)
    
    density.i <- density.i[order(density.i$xyDiff),]
    intersections[i,"DENSITYSCORE"]  <- mean( density.i[1:4,"DENSITYSCORESCALED"])
    
  }

  remove(loc.i)
  remove(density.i)
  
  
  # You now have the relative density values for each of the intersections!
  
  #we can rank them
  intersections <- intersections %>% arrange(-DENSITYSCORE)
  intersections$LABEL <- paste("Score ",round(intersections$DENSITYSCORE,1),sep = "" )
  
  ggmap(basemap, extent = "device")+
    stat_contour(data = crashDensity, aes(x =LONGITUDE, y = LATITUDE, z =DENSITYSCORESCALED, fill = ..level.., alpha = ..level..), geom = "polygon")+
    scale_fill_gradient(low = "green3", high = "red", guide = F) +
    scale_alpha(range = c(0.05, 0.2), guide = F)+
    geom_point(data = intersections, aes(x = LONGITUDE, y = LATITUDE), size = 3 )+
    geom_text(data = intersections, aes(x = LONGITUDE, y = LATITUDE, label = LABEL), hjust = -.2, size = 2)+
    labs( title = "Fatal Crash Density",
          caption = "Spokane (2010 - 2015)")+
    guides(color = F)+
    scale_color_gradient(low = "green3", high = "red", guide = F)


