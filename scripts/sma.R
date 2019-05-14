##########################
## Evaluating over SMAs ##
##########################

#UTM unPROJECTED 19 for US, should be 20 for CA
sightings20<-egtable%>%
  filter(LONGITUDE >= -66)
sightings19<-egtable%>%
  filter(LONGITUDE < -66)


if (nrow(sightings19) >= nrow(sightings20)){
  ##US
  CRS.new<-CRS("+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
} else if (nrow(sightings19) < nrow(sightings20)){
  ##Canada
  CRS.new<-CRS("+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
}

print(CRS.new)

if (nrow(sightings19) >= nrow(sightings20)){
  ##US
  CRS.utm<-CRS("+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
} else if (nrow(sightings19) < nrow(sightings20)){
  ##Canada
  CRS.utm<-CRS("+proj=utm +zone=20 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
}

print(CRS.utm)

CRS.latlon<-CRS("+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0")
######

##cape cod bay 01 Jan - 15 May
ccb<-readOGR(smapath, layer = "sma_ccb")
##race point 01 March - 30 April
race<-readOGR(smapath, layer = "sma_race")
##great south channel 01 April - 31 July
gsc<-readOGR(smapath, layer = "sma_gsc")
##block island 01 Nov - 30 April
block<-readOGR(smapath, layer = "sma_block")
##MID Atlantic ports 01 Nov - 30 April
mano<-readOGR(smapath, layer = "MA_SMA_north_po")
##mid Atlantic south carolina 01 Nov - 30 April
maso<-readOGR(smapath, layer = "MA_SMA_south_po")
##calving grounds 15 Nov - 15 April
seshore<-readOGR(smapath, layer = "SE_SMA2shore2_po")
##canada
ecanada<-readOGR(smapath, layer = "ecanada")
##france
spm<-readOGR(smapath, layer = "spm")

##sma projected properly
ccb.tr<-spTransform(ccb, CRS.new)
race.tr<-spTransform(race, CRS.new)
gsc.tr<-spTransform(gsc, CRS.new)
block.tr<-spTransform(block, CRS.new)
mano.tr<-spTransform(mano, CRS.new)
maso.tr<-spTransform(maso, CRS.new)
seshore.tr<-spTransform(seshore, CRS.new)
ecanada<-spTransform(ecanada, CRS.new)
spm<-spTransform(spm, CRS.new)

##no SEUS
##01Jan - 29Feb CCB, MANO, BI
sma1<-rgeos::union(ccb.tr, mano.tr)
sma1<-rgeos::union(sma1, block.tr)
##01MAR - 31MAR CCB, MANO, BI, RACE
sma2<-rgeos::union(sma1, race.tr)
##01APR - 30APR CCB, MANO, BI, RACE, GSC
sma3<-rgeos::union(sma2, gsc.tr)
##01MAY - 15MAY CCB, GSC
sma4<-rgeos::union(ccb.tr, gsc.tr)
##16MAY-31JULY
sma5<-gsc.tr
##01NOv-31DEC
sma6<-rgeos::union(mano.tr, block.tr)
########

smapresent<-NULL
smaname<-NULL
#####     

if (between(MODA,"01-01", "02-29")){
  smapresent<-sma1
  smaname<-"sma1"
} else if (between(MODA,"03-01","03-31")){
  smapresent<-sma2
  smaname<-"sma2"
} else if (between(MODA,"04-01","04-30")){
  smapresent<-sma3
  smaname<-"sma3"
} else if (between(MODA,"05-01","05-15")){
  smapresent<-sma4
  smaname<-"sma4"
} else if (between(MODA,"05-16","07-31")){
  smapresent<-sma5
  smaname<-"sma5"
} else if (between(MODA,"11-01","12-31")){
  smapresent<-sma6
  smaname<-"sma6"
} else {
  smapresent<-smapresent
  smaname<-"none"
}
smapresent.sp<-spTransform(smapresent,CRS.latlon)
smafort<-fortify(smapresent.sp)
MA = NULL
for (i in 1:nrow(smafort)){
  MA = 'Seasonal Management Area'
}
smafort<-cbind(smafort,MA)
smafort$MA<-as.factor(smafort$MA)

######  