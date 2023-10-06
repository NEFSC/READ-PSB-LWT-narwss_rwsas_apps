
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

print(CRS.new)

if (nrow(sightings19) >= nrow(sightings20)) {
  ##US
  CRS.utm <-
    CRS("+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
} else if (nrow(sightings19) < nrow(sightings20)) {
  ##Canada
  CRS.utm <-
    CRS("+proj=utm +zone=20 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

print(CRS.utm)

CRS.latlon <-
  CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")

smapath <- "./SMA ind shp"
##shapefile update 18-Mar-2022 lmc
allSMA <- readOGR(smapath, layer = 'right_whale_SMA_all_po')
allSMA.tr <- sp::spTransform(allSMA, CRS.new)

NEUS_shiplane <-
  readOGR(smapath, layer = 'Main traffic Lanes with new TSS')
ecanada <- readOGR(smapath, layer = "ecanada")
dyna_ship <- readOGR(smapath, layer = "NARW_RZs_2020_02_07")
GSL_shiplane <- readOGR(smapath, layer = "shiplane")
##france
spm <- readOGR(smapath, layer = "spm")
#WEA <- read_sf(smapath, layer = "Wind_Lease_Outlines_2_2023") #using sf not rgdal doesnt return SPDF
WEA <- readOGR(smapath, layer = "Wind_Lease_Outlines_2_2023")
print('line 62')

##sma projected properly
NEUS_shiplane.tr <- sp::spTransform(NEUS_shiplane, CRS.new)
ecanada <- sp::spTransform(ecanada, CRS.new)
dyna_ship.tr <- sp::spTransform(dyna_ship, CRS.new)
GSL_shiplane.tr <- sp::spTransform(GSL_shiplane, CRS.new)
spm.tr <- sp::spTransform(spm, CRS.new)
WEA.tr <- sp::spTransform(WEA, CRS.new)
#WEA.tr <- st_transform(WEA, CRS.new) #using sf not sp - doesnt return SPDF
print('line 78')

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
  smapresent.sp <- sp::spTransform(smapresent, CRS.latlon)
}

smafort <- fortify(smapresent.sp)
MA = NULL
for (i in 1:nrow(smafort)) {
  MA = 'Seasonal Management Area'
}
smafort <- cbind(smafort, MA)
smafort$MA <- as.factor(smafort$MA)

# transform shapes to latlon ----
print('line139')
NEUS_shiplane.sp <- sp::spTransform(NEUS_shiplane.tr, CRS.latlon)
spm.sp <- sp::spTransform(spm.tr, CRS.latlon)
dyna_ship.sp <- sp::spTransform(dyna_ship.tr, CRS.latlon)
GSL_shiplane.sp <- sp::spTransform(GSL_shiplane.tr, CRS.latlon)
WEA.sp <- sp:: spTransform(WEA.tr, CRS.latlon)
#WEA.sp <- st_transform(WEA.tr, CRS.latlon) #using st instead of sp not returning SPDF
