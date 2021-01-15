##########################
## Evaluating over SMAs ##
##########################

#UTM unPROJECTED 19 for US, should be 20 for CA
print(egtable)
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

smapath<-"./SMA ind shp"

##Boston <-> NYC shiplane
NEUS_shiplane<-readOGR(smapath, layer = 'Main traffic Lanes with new TSS')
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
#
dyna_ship<-readOGR(smapath, layer = "NARW_RZs_2020_02_07")
GSL_shiplane<-readOGR(smapath, layer = "shiplane")
##france
spm<-readOGR(smapath, layer = "spm")
print('line 62')

##sma projected properly
NEUS_shiplane.tr<-sp::spTransform(NEUS_shiplane, CRS.new)
ccb.tr<-sp::spTransform(ccb, CRS.new)
race.tr<-sp::spTransform(race, CRS.new)
gsc.tr<-sp::spTransform(gsc, CRS.new)
block.tr<-sp::spTransform(block, CRS.new)
mano.tr<-sp::spTransform(mano, CRS.new)
maso.tr<-sp::spTransform(maso, CRS.new)
seshore.tr<-sp::spTransform(seshore, CRS.new)
ecanada<-sp::spTransform(ecanada, CRS.new)
dyna_ship.tr<-sp::spTransform(dyna_ship, CRS.new)
GSL_shiplane.tr<-sp::spTransform(GSL_shiplane, CRS.new)
spm.tr<-sp::spTransform(spm, CRS.new)
print('line 78')

##no SEUS
##01Jan - 29Feb CCB, MANO, BI, MASO, seshore
sma1<-rgeos::union(ccb.tr, mano.tr)
sma1<-rgeos::union(sma1, block.tr)
sma1<-rgeos::union(sma1, maso.tr)
sma1<-rgeos::union(sma1, seshore.tr)
##01MAR - 31MAR CCB, MANO, BI, RACE, seshore
sma2<-rgeos::union(sma1, race.tr)
##01APR - 30APR CCB, MANO, BI, RACE, GSC, seshore(but need to fix since this only goes until the 15th)
sma3<-rgeos::union(sma2, gsc.tr)
##01MAY - 15MAY CCB, GSC
sma4<-rgeos::union(ccb.tr, gsc.tr)
##16MAY-31JULY
sma5<-gsc.tr
##01NOv-31DEC seshore(but need to fix since this starts on nove 15th)
sma6<-rgeos::union(mano.tr, block.tr)
sma6<-rgeos::union(sma6, maso.tr)
sma6<-rgeos::union(sma6, seshore.tr)
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

if (is.null(smapresent)){
  fakesma<-SpatialPolygons(list(fakedma))
  smapresent.sp<-fakesma
} else {
  smapresent.sp<-sp::spTransform(smapresent,CRS.latlon)
}

smafort<-fortify(smapresent.sp)
MA = NULL
for (i in 1:nrow(smafort)){
  MA = 'Seasonal Management Area'
}
smafort<-cbind(smafort,MA)
smafort$MA<-as.factor(smafort$MA)

######  
print('line139')
NEUS_shiplane.sp<-sp::spTransform(NEUS_shiplane.tr,CRS.latlon)
spm.sp<-sp::spTransform(spm.tr,CRS.latlon)
dyna_ship.sp<-sp::spTransform(dyna_ship.tr,CRS.latlon)
GSL_shiplane.sp<-sp::spTransform(GSL_shiplane.tr,CRS.latlon)