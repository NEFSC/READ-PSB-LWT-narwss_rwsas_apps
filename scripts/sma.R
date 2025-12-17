## Evaluating over SMAs ----

# UTM unPROJECTED 19 for US, 20 for CA
print(egtable)
sightings20 <- egtable %>%
  filter(LONGITUDE >= -66)
sightings19 <- egtable %>%
  filter(LONGITUDE < -66)

if (nrow(sightings19) >= nrow(sightings20)) {
  ##US
  CRS.new <-
    CRS("+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
} else if (nrow(sightings19) < nrow(sightings20)) {
  ##Canada
  CRS.new <-
    CRS("+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
}
#print("CRS.new")
#print(CRS.new)

if (nrow(sightings19) >= nrow(sightings20)) {
  ##US
  CRS.utm <-
    CRS("+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
} else if (nrow(sightings19) < nrow(sightings20)) {
  ##Canada
  CRS.utm <-
    CRS("+proj=utm +zone=20 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}
#print("CRS.utm")
#print(CRS.utm)

CRS.latlon <- CRS("=epsg:4326 +proj=longlat +datum=WGS84") #251120 changes with getting towards sf
  #CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #1/2 drop of +init - others in photo_server line 101
  #CRS("=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0") #this could be easier I think using WGS84 or EPSG 4326 but unclear 20251118 HJF
  #might need and st_transform() or something similar? st_as_sf? specify utm of crs? 
  #CRS("+proj=longlat +datum=WGS84 +no_defs") #another option that might work but unclear
  
#print("CRS.latlon")  
#print(CRS.latlon)

smapath <- "./SMA ind shp" 
allSMA <- sf::st_read(smapath, layer = 'right_whale_SMA_all_po') #1/7 20251118 HJF edits to sf from rgdal
allSMA.tr <- sf::st_transform(allSMA, CRS.new)

NEUS_shiplane <- sf::st_read(smapath, layer = 'Main traffic Lanes with new TSS')
ecanada <- sf::st_read(smapath, layer = "ecanada")
dyna_ship <- sf::st_read(smapath, layer = "NARW_RZs_2020_02_07")
GSL_shiplane <- sf::st_read(smapath, layer = "shiplane")
##france
spm <- sf::st_read(smapath, layer = "spm")
WEA <- sf::st_read(smapath, layer = "BOEM_Wind_Lease_Outlines_06_06_2024") 
EEZ <- sf::st_read(smapath, layer = "EEZ_NWAtl") #20251216 add for glider detections near border to visualize it is in Canada


##sma projected properly
#as CRSs are defined above using proj strings, batch changing spTransform to st_transform for 2-47 instances
NEUS_shiplane.tr <- sf::st_transform(NEUS_shiplane, CRS.new)
ecanada <- sf::st_transform(ecanada, CRS.new)
dyna_ship.tr <- sf::st_transform(dyna_ship, CRS.new)
GSL_shiplane.tr <- sf::st_transform(GSL_shiplane, CRS.new)
spm.tr <- sf::st_transform(spm, CRS.new)
WEA.tr <- sf::st_transform(WEA, CRS.new)
EEZ.tr <- sf::st_transform(EEZ, CRS.new)

##no SEUS
##01Jan - 29Feb CCB, MANO, BI, MASO, seshore
sma1 <- subset(allSMA.tr, ID %in% 1:8)
##01MAR - 31MAR CCB, MANO, BI, RACE, seshore
sma2 <- subset(allSMA.tr, ID %in% 1:9)
##01APR - 15APR CCB, MANO, BI, RACE, GSC, seshore
sma3.1 <- subset(allSMA.tr, ID %in% 1:10)
##16Apr - 30Apr CCB, MANO, BI, RACE, GSC
sma3.2 <- subset(allSMA.tr, ID %in% 3:10)
##01MAY - 15MAY CCB, GSC
sma4 <- subset(allSMA.tr, ID %in% c(8, 10))
##16MAY-31JULY
sma5 <- subset(allSMA.tr, ID == 10)
##01NOv-14Nov
sma6 <- subset(allSMA.tr, ID %in% 2:7)
##15Nov-31Dec
sma7 <- subset(allSMA.tr, ID %in% 1:7)

smapresent <- NULL
smaname <- NULL

print(str(MODA))
MODA <- as.character(MODA) #20251208 added  - poss from function in other script

if (between(MODA, "01-01", "02-29")) {
  smapresent <- sma1
  smaname <- "sma1"
} else if (between(MODA, "03-01", "03-31")) {
  smapresent <- sma2
  smaname <- "sma2"
} else if (between(MODA, "04-01", "04-15")) {
  smapresent <- sma3.1
  smaname <- "sma3.1"
} else if (between(MODA, "04-16", "04-30")) {
  smapresent <- sma3.2
  smaname <- "sma3.2"
} else if (between(MODA, "05-01", "05-15")) {
  smapresent <- sma4
  smaname <- "sma4"
} else if (between(MODA, "05-16", "07-31")) {
  smapresent <- sma5
  smaname <- "sma5"
} else if (between(MODA, "11-01", "11-14")) {
  smapresent <- sma6
  smaname <- "sma6"
} else if (between(MODA, "11-15", "12-31")) {
  smapresent <- sma7
  smaname <- "sma7"
} else {
  smapresent <- smapresent
  smaname <- "none"
}

if (is.null(smapresent)) {
  fakesma <- SpatialPolygons(list(fakeslowzone))
  smapresent.sp <- fakesma
} else {
  smapresent.sp <- sf::st_transform(smapresent, CRS.latlon)
}

print(class(smapresent.sp))
print(str(smapresent.sp))

smafort <- fortify(smapresent.sp)
MA = NULL
for (i in 1:nrow(smafort)) {
  MA = 'Seasonal Management Area'
}
smafort <- cbind(smafort, MA)
smafort$MA <- as.factor(smafort$MA)

# transform shapes to latlon ----
print('sma line 139')
NEUS_shiplane.sp <- sf::st_transform(NEUS_shiplane.tr, CRS.latlon)
spm.sp <- sf::st_transform(spm.tr, CRS.latlon)
dyna_ship.sp <- sf::st_transform(dyna_ship.tr, CRS.latlon)
GSL_shiplane.sp <- sf::st_transform(GSL_shiplane.tr, CRS.latlon)
WEA.sp <- sf::st_transform(WEA.tr, CRS.latlon) 
EEZ.sp <- sf::st_transform(EEZ.tr, CRS.latlon)