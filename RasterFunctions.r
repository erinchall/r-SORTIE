library(raster)
library(rgdal)
#December 16, 2020
#r1 <- raster("ExValuesRaster.grd")

ExEmptyRaster <- raster(nrows=25,ncols=25, xmn=0,xmx=200,ymn=0,ymx=200)
ExValuesRaster<- ExEmptyRaster
ExValuesRaster[] <- rbinom(n=ncell(ExEmptyRaster), size=1, prob=0.5) 
#writeRaster(ExValuesRaster,"ExValuesRaster_S.grd")

r1 <- raster("ExValuesRaster_S.grd")  #<-Put your rastor filename here 

#plot(r1)

values(r1)[1:10]

write("", file="locations.txt")
for(i in 1:ncell(r1)){
  
  if (values(r1)[i] > 0){
    loc <- xyFromCell(r1,i)
    p1 <- paste0("<ha_applyToCell x=\"",loc[1],"\" y=\"",loc[2],"\"/>")
    #print(p1)
    write(p1, file="locations.txt",append=TRUE)  #This prints out the x-y locations in the format needed by the harvest xml
  }
}
  