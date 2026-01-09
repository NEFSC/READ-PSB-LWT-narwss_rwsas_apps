## Action & Slow Zone analysis

## Sighting dataframe cluster FUNCTION ----
##Original from LMC commented out until below works correctly with sf objects when called below
### clustering overlapping sightings in sp

# clustdf_fun <- function(x, y) {
# 
#   if (length(names(x)) > 1) {
#     ##Overlap of whale density core area analysis
#     polycomb <- data.frame(poly1 = NA,
#                            poly2 = NA,
#                            overlap = NA)
#     ##creates a list of 2 combinations to compare
#     #print(names(x))
#     combos <- combn(names(x), 2)
#     ##compares the list
#     for (i in seq_along(combos[1, ])) {
#       poly1 <- combos[1, i]
#       poly2 <- combos[2, i]
#       #if they don't overlap, the result of the below "if statement" is NULL
#       # uses rgeos fix or update entire function
#       if (!is.null(gIntersection(y[poly1], y[poly2], byid = TRUE))) {
#         overlap = 'yes'
#       } else {
#         overlap = 'no'
#       }
#       if (st_is_valid(y[poly1, ]) & st_is_valid(y[poly2, ])) {
#       overlap_bool <- as.logical(st_intersects(y[poly1, ], y[poly2, ], sparse = FALSE))
#       overlap <- ifelse(overlap_bool, 'yes', 'no')
#       } else {
#         overlap <- 'no'
#       }
#     df <- data.frame(poly1 = poly1,
#                     poly2 = poly2,
#                     overlap = overlap)
#     polycomb <- rbind(polycomb, df)
#     }
# 
#     polycomb$poly1 <- as.numeric(polycomb$poly1)
#     polycomb$poly2 <- as.numeric(polycomb$poly2)
#     polycluster <- polycomb %>% filter(!is.na(poly1))
#   } else if (length(names(x)) == 1) {
#     polycluster <- data.frame(poly1 = 1,
#                               poly2 = 1,
#                               overlap = 'no')
#   }
# 
#   ##clustering polygons that overlap
#   polycluster_yes <- polycluster %>%
#     filter(overlap == "yes")
# 
#   ##transitive property of overlapping core areas
#   polymat = graph_from_edgelist(as.matrix(polycluster_yes[, 1:2]), directed = FALSE)
#   #unique polygons
#   upoly = sort(unique(c(
#     polycluster_yes$poly1, polycluster_yes$poly2
#   )))
#   (cluster = components(polymat)$membership[upoly])
#   #final cluster assignment df for overlap = yes
#   (polyassign = data.frame(upoly, cluster, row.names = NULL))
# 
#   poly12 <- rbind(unlist(polycluster$poly1), unlist(polycluster$poly2))
#   poly12 <- data.frame(upoly = c(polycluster$poly1, polycluster$poly2))
# 
#   ##these sightings are NOT triggering on their own (or are trigger by one sighting of 3+ without overlapping sightings) are assigned a cluster of -1
#   not <- poly12 %>%
#     filter((!poly12$upoly %in% polyassign$upoly) |
#              (!poly12$upoly %in% polyassign$upoly)) %>%
#     distinct() %>%
#     mutate(cluster = -1)
# 
#   ##put together the trigger sightings that don't overlap with any other sightings, with those that do with assigned clusters
#   totpolyassign <- rbind(polyassign, not)
#   totpolyassign$cluster <- as.numeric(totpolyassign$cluster)
#   print(totpolyassign)
#   ##clustmin is for a totpolyassign df without any overlapping triggers
#   clustmin = 0
#   ##assigns consecutive cluster numbers to those sightings that don't overlap, but are triggering all on their own
#   for (i in 1:nrow(totpolyassign))
#     if (totpolyassign$cluster[i] == -1 &
#         max(totpolyassign$cluster) > 0) {
#       totpolyassign$cluster[i] <- max(totpolyassign$cluster) + 1
#     } else if (totpolyassign$cluster[i] == -1 &
#                max(totpolyassign$cluster) < 0) {
#       totpolyassign$cluster[i] <- clustmin + 1
#     } else {
# 
#     }
#   ##
#   print("totpolyassign")
#   print(totpolyassign)
#   totpolyassign
# }

##ChatGPT rewrite - doesn't show core areas - possible to revamp it up
# clustdf_fun_sf <- function(x, y) {
#   
#   n <- length(x)
#   
#   # --- 1. If only one polygon, assign cluster 1 ---
#   if (n == 1) {
#     return(data.frame(upoly = 1, cluster = 1))
#   }
#   
#   # Ensure polygon names are numeric row indices
#   if (!is.numeric(names(x))) names(x) <- seq_len(n)
#   
#   # --- 2. Generate all pairwise combinations ---
#   combos <- t(combn(names(x), 2)) %>% as.data.frame()
#   names(combos) <- c("poly1", "poly2")
#   combos$poly1 <- as.numeric(combos$poly1)
#   combos$poly2 <- as.numeric(combos$poly2)
#   
#   # --- 3. Compute pairwise overlaps safely ---
#   overlap_vec <- logical(nrow(combos))
#   
#   for (i in seq_len(nrow(combos))) {
#     p1 <- y[combos$poly1[i], ]
#     p2 <- y[combos$poly2[i], ]
#     
#     valid1 <- st_is_valid(p1)
#     valid2 <- st_is_valid(p2)
#     valid1 <- ifelse(is.na(valid1), FALSE, valid1)
#     valid2 <- ifelse(is.na(valid2), FALSE, valid2)
#     
#     if (valid1 & valid2) {
#       tmp <- lengths(st_intersects(p1, p2))
#       overlap_vec[i] <- ifelse(is.na(tmp) | tmp == 0, FALSE, TRUE)
#     } else {
#       overlap_vec[i] <- FALSE
#     }
#   }
#   combos$overlap <- ifelse(overlap_vec, "yes", "no")
#   
#   # --- 4. Keep only overlapping pairs ---
#   polycluster_yes <- combos %>% filter(overlap == "yes")
#   
#   # --- 5. Assign clusters via graph ---
#   if (nrow(polycluster_yes) > 0) {
#     polymat <- graph_from_edgelist(as.matrix(polycluster_yes[, 1:2]), directed = FALSE)
#     upoly <- sort(unique(c(polycluster_yes$poly1, polycluster_yes$poly2)))
#     cluster <- components(polymat)$membership[as.character(upoly)]
#     polyassign <- data.frame(upoly, cluster, row.names = NULL)
#   } else {
#     polyassign <- data.frame(upoly = numeric(0), cluster = numeric(0))
#   }
#   
#   # --- 6. Add isolated polygons (not in any overlap) ---
#   poly12 <- data.frame(upoly = seq_len(n))
#   not <- poly12 %>%
#     filter(!(upoly %in% polyassign$upoly)) %>%
#     mutate(cluster = -1)
#   
#   # --- 7. Combine overlapping and isolated polygons ---
#   totpolyassign <- rbind(polyassign, not)
#   
#   # --- 8. NA-safe: replace any remaining NAs with -1 ---
#   totpolyassign$cluster <- as.numeric(totpolyassign$cluster)
#   totpolyassign$cluster[is.na(totpolyassign$cluster)] <- -1
#   
#   # --- 9. Assign new cluster IDs for isolated polygons ---
#   isolated_idx <- which(totpolyassign$cluster == -1)
#   for (i in isolated_idx) {
#     totpolyassign$cluster[i] <- max(totpolyassign$cluster, na.rm = TRUE) + 1
#   }
#   
#   # --- 10. Sort by polygon index ---
#   totpolyassign <- totpolyassign %>% arrange(upoly)
#   
#   return(totpolyassign)
# }

#CHatGPT rewrite of function to cluster overlapping sightings using sf & vectorizing to speed up double for loop 
#doesn't work correctly for 121225 - figure out clustering of nonoverlapping sightings
clustdf_fun_sf <- function(x, y) {

  # If only one polygon
  if (length(names(x)) == 1) {
    return(data.frame(upoly = 1, cluster = 1))
  }

  # Generate all pairwise combinations of polygon names
  combos <- t(combn(names(x), 2)) %>% as.data.frame()
  names(combos) <- c("poly1", "poly2")

  # Convert to numeric
  combos$poly1 <- as.numeric(combos$poly1)
  combos$poly2 <- as.numeric(combos$poly2)

  # Vectorized intersection check using st_intersects
  intersects_matrix <- st_intersects(y[combos$poly1, ], y[combos$poly2, ], sparse = FALSE)
  combos$overlap <- ifelse(diag(intersects_matrix) | rowSums(intersects_matrix) > 0, "yes", "no")
  combos$overlap <- ifelse(is.na(combos$overlap), "no", combos$overlap)

  # Filter only overlapping polygons
  polycluster_yes <- combos %>% filter(overlap == "yes")

  # If any overlaps exist, build graph and assign clusters
  if (nrow(polycluster_yes) > 0) {
    polymat <- graph_from_edgelist(as.matrix(polycluster_yes[, 1:2]), directed = FALSE)
    upoly <- sort(unique(c(polycluster_yes$poly1, polycluster_yes$poly2)))
    cluster <- components(polymat)$membership[upoly]
    polyassign <- data.frame(upoly, cluster, row.names = NULL)
  } else {
    polyassign <- data.frame(upoly = numeric(0),
                             cluster = numeric(0))
  }

  # Find polygons that don’t overlap with any other
  poly12 <- data.frame(upoly = c(combos$poly1, combos$poly2))
  not <- poly12 %>%
    filter(!(upoly %in% polyassign$upoly)) %>%
    distinct() %>%
    mutate(cluster = -1)

  # Combine overlapping and non-overlapping polygons
  totpolyassign <- rbind(polyassign, not)
  totpolyassign$cluster <- as.numeric(totpolyassign$cluster)

  # Assign new cluster IDs for isolated polygons
  clustmin <- 0
  for (i in 1:nrow(totpolyassign)) {
    if (totpolyassign$cluster[i] == -1 & max(totpolyassign$cluster) > 0) {
      totpolyassign$cluster[i] <- max(totpolyassign$cluster) + 1
    } else if (totpolyassign$cluster[i] == -1 & max(totpolyassign$cluster) < 0) {
      totpolyassign$cluster[i] <- clustmin + 1
    }
  }
  print("totpolyassign")
  totpolyassign
}

## LEAFLET BASE ----

sasdma <-
  leaflet(data = egsas, options = leafletOptions(zoomControl = FALSE)) %>%
  addEsriBasemapLayer(esriBasemapLayers$Oceans, autoLabels = TRUE) %>%
  addPolygons(data = smapresent.sp, 
              weight = 2,
              color = "red") %>%
  addPolylines(  #adding for slow zone report maps 20241230 HJF
    data = WEA.sp,
    weight = 1,
    color = "green",
    fill = F
  ) %>%
  addPolylines(
    data = NEUS_shiplane.sp,
    weight = 1,
    color = "grey",
    fill = F
  ) %>%
  addPolylines(  #adding Hague for easy Canada visualizations 20251216 HJF
    data = EEZ.sp,
    weight = 1,
    color = "black",
    fill = F
    )

## ACTION ----

egsas$GROUP_SIZE <- as.numeric(egsas$GROUP_SIZE)
##copy for spatializing
eg <- egsas
print("eg line 226 a&sz")
print(eg)
##declare which columns are coordinates
#coordinates(eg) <-  ~ LONGITUDE + LATITUDE #can be deleted
##declare what kind of projection they are in
#proj4string(eg) <- CRS.latlon
#make an sf object 20251118 HJF
eg.sp <- st_as_sf(eg, coords = c("LONGITUDE", "LATITUDE"), remove = FALSE, crs = 4326) #251120 update to sf/ditch rgdal
##change projection
eg.tr <- sf::st_transform(eg.sp, CRS.new) #sf
#eg.tr <- spTransform(eg, CRS.new) #old sp
print(str(MODA))

## in or out of active sma? TRUE = in ----
#inoutsma <- NULL

# for (i in 1:nrow(egsas))
#   if (between(MODA, "01-01", "02-29")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma1, "SpatialPolygons"))) #sp
#     lengths(sf::st_intersects(eg.tr, sma1)) > 0
#   } else if (between(MODA, "03-01", "03-31")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma2, "SpatialPolygons")))
#     lengths(sf::st_intersects(eg.tr, sma2)) > 0
#   } else if (between(MODA, "04-01", "04-15")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma3.1, "SpatialPolygons"))) #HJF added w/ SMA code edits
#     lengths(sf::st_intersects(eg.tr, sma3.1)) > 0
#   } else if (between(MODA, "04-16", "04-30")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma3.2, "SpatialPolygons"))) #HJF added w/ SMA code edits
#     lengths(sf::st_intersects(eg.tr, sma3.2)) > 0
#   } else if (between(MODA, "05-01", "05-15")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma4, "SpatialPolygons")))
#     lengths(sf::st_intersects(eg.tr, sma4)) > 0
#   } else if (between(MODA, "05-16", "07-31")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma5, "SpatialPolygons")))
#     lengths(sf::st_intersects(eg.tr, sma5)) > 0
#   } else if (between(MODA, "11-01", "12-31")) {
#     #inoutsma <- !is.na(sp::over(eg.tr, as(sma6, "SpatialPolygons")))
#     lengths(sf::st_intersects(eg.tr, sma6)) > 0
#   } else {
#     nrow(inoutsma) == nrow(egsas)
#     inoutsma <- FALSE
#   }

##20251121 chatgpt rewrite to see if eg sights are in our out of active sma #IS THIS WORKING CORRECTLY? check all instances
# Convert MODA ("MM-DD") to a date in dummy year 2000
MODA_clean <- format(as.Date(MODA, "%m-%d"), "%m-%d")
d <- as.Date(paste0("2000-", MODA_clean))
# Helper function to replace !is.na(sp::over())
#inside <- function(points, polys) {
 # lengths(st_intersects(points, polys)) > 0
#}
inside <- function(points, polys) {
  if (nrow(points) == 0) return(logical(0))  # handle empty subsets
  lengths(st_intersects(points, polys)) > 0
}
# Initialize
#inoutsma <- NULL #20260108 comment from previous code with this line includeed - delete if below works
# Initialize result #20260108 new with 121225 errors
inoutsma <- rep(FALSE, nrow(eg.tr))
# helper to avoid empty idx issues
safe_assign <- function(idx, polys) {
  if (any(idx)) inoutsma[idx] <<- inside(eg.tr[idx, ], polys)
}

safe_assign(between(d, as.Date("2000-01-01"), as.Date("2000-02-29")), sma1)
safe_assign(between(d, as.Date("2000-03-01"), as.Date("2000-03-31")), sma2)
safe_assign(between(d, as.Date("2000-04-01"), as.Date("2000-04-15")), sma3.1)
safe_assign(between(d, as.Date("2000-04-16"), as.Date("2000-04-30")), sma3.2)
safe_assign(between(d, as.Date("2000-05-01"), as.Date("2000-05-15")), sma4)
safe_assign(between(d, as.Date("2000-05-16"), as.Date("2000-07-31")), sma5)
safe_assign(between(d, as.Date("2000-11-01"), as.Date("2000-12-31")), sma6)
# # Jan 1 – Feb 29
# idx <- between(d, as.Date("2000-01-01"), as.Date("2000-02-29"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma1)
# 
# # Mar 1 – Mar 31
# idx <- between(d, as.Date("2000-03-01"), as.Date("2000-03-31"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma2)
# 
# # Apr 1 – Apr 15
# idx <- between(d, as.Date("2000-04-01"), as.Date("2000-04-15"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma3.1)
# 
# # Apr 16 – Apr 30
# idx <- between(d, as.Date("2000-04-16"), as.Date("2000-04-30"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma3.2)
# 
# # May 1 – May 15
# idx <- between(d, as.Date("2000-05-01"), as.Date("2000-05-15"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma4)
# 
# # May 16 – Jul 31
# idx <- between(d, as.Date("2000-05-16"), as.Date("2000-07-31"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma5)
# 
# # Nov 1 – Dec 31
# idx <- between(d, as.Date("2000-11-01"), as.Date("2000-12-31"))
# inoutsma[idx] <- inside(eg.tr[idx, ], sma6)

#20260108 below commented out with 121225 errors delete if all instances work correctly
# if (between(d, as.Date("2000-01-01"), as.Date("2000-02-29"))) {
#   inoutsma <- inside(eg.tr, sma1)
# } else if (between(d, as.Date("2000-03-01"), as.Date("2000-03-31"))) {
#   inoutsma <- inside(eg.tr, sma2)
# } else if (between(d, as.Date("2000-04-01"), as.Date("2000-04-15"))) {
#   inoutsma <- inside(eg.tr, sma3.1)
# } else if (between(d, as.Date("2000-04-16"), as.Date("2000-04-30"))) {
#   inoutsma <- inside(eg.tr, sma3.2)
# } else if (between(d, as.Date("2000-05-01"), as.Date("2000-05-15"))) {
#   inoutsma <- inside(eg.tr, sma4)
# } else if (between(d, as.Date("2000-05-16"), as.Date("2000-07-31"))) {
#   inoutsma <- inside(eg.tr, sma5)
# } else if (between(d, as.Date("2000-11-01"), as.Date("2000-12-31"))) {
#   inoutsma <- inside(eg.tr, sma6)
# } else {
#   # If date does not fall in any SMA period, return all FALSE
#   inoutsma <- rep(FALSE, nrow(eg.tr))
# }
print("inoutsma")
print(inoutsma)
#Canada <- !is.na(sp::over(eg.tr, as(ecanada, "SpatialPolygons"))) #sp
Canada <- lengths(sf::st_intersects(eg.tr, ecanada)) > 0 #sf 251121 defined in sma script as sf obj
#SPM <- !is.na(sp::over(eg.tr, as(spm.tr, "SpatialPolygons"))) #sp
SPM <- lengths(sf::st_intersects(eg.tr, spm.tr)) > 0 #sf 251121 defined in sma script as sf obj
sightID <- 1:nrow(egsas)
egsas <- cbind(egsas, inoutsma, Canada, SPM, sightID)
egsas <- egsas %>% mutate(ACTION_NEW = NA)
#print(egsas)

#bDMA <- !is.na(sp::over(eg.tr, as(benigndma.tr, "SpatialPolygons")))
bDMA <- lengths(sf::st_intersects(eg.tr, benigndma.tr)) > 0 #sf 251121
#eDMA <- !is.na(sp::over(eg.tr, as(extensiondma.tr, "SpatialPolygons")))
eDMA <- lengths(sf::st_intersects(eg.tr, extensiondma.tr)) > 0   #sf 251121
#bAPZ <- !is.na(sp::over(eg.tr, as(benignapz.tr, "SpatialPolygons")))
bAPZ <- lengths(sf::st_intersects(eg.tr, benignapz.tr)) > 0  #sf 251121
#eAPZ <- !is.na(sp::over(eg.tr, as(extensionapz.tr, "SpatialPolygons")))
eAPZ <- lengths(sf::st_intersects(eg.tr, extensionapz.tr)) > 0  #sf 251121

egsas <- cbind(egsas, bDMA, eDMA, bAPZ, eAPZ)
#print("egsas")
#print(egsas)

##

for (i in 1:nrow(egsas))
  if (egsas$inoutsma[i] == TRUE) {
    egsas$ACTION_NEW[i] = 2
  } else if (egsas$Canada[i] == TRUE) {
    egsas$ACTION_NEW[i] = 6
  } else if (egsas$SPM[i] == TRUE) {
    egsas$ACTION_NEW[i] = 6
    output$error3 <-
      renderText({
        "Soc re bleu! One of these right whales was in France!"
      })
  } else if (isolate(criteria$loc) == 'Network') {
  #231003 HJF example data errors on network } else if (isolate(criteria$loc) == 'Network' | criteria$path == './example_data/') {
    if (egsas$eDMA[i] == TRUE &
        (isolate(criteria$DMAapp) == "vissig" |
         isolate(criteria$DMAapp) == "rwsurv")) {
      #visual detections in an extension eligible DMA
      egsas$ACTION_NEW[i] = 55
    } else if (egsas$eAPZ[i] == TRUE &
               isolate(criteria$DMAapp) == "acoudet") {
      #acoustic detections in an extension eligible APZ
      egsas$ACTION_NEW[i] = 55
    } else if (egsas$bDMA[i] == TRUE &
               (isolate(criteria$DMAapp) == "vissig" |
                isolate(criteria$DMAapp) == "rwsurv")) {
      #vis dets in benign DMA aka cannot extend
      egsas$ACTION_NEW[i] = 2
    } else if (egsas$bAPZ[i] == TRUE &
               isolate(criteria$DMAapp) == "acoudet") {
      #acoustic detections in benign APZ aka cannot extend
      egsas$ACTION_NEW[i] = 2
    }
  } else if (egsas$inoutsma[i] == FALSE) {
    egsas$ACTION_NEW[i] = NA
  }
print("egsas A&SZ line 356") 
print(egsas)

## slow zone evaluation ----

#spatial analysis
## 1 nautical mile is 1852 meters
m_nm <- 1 / 1852
## eg density is 4 whales/100nm^2 (50 CFR Part 224)
egden <- 0.0416

## these will get overwritten if there are DMAs to create or extend
alldmas <- NULL
dmacoord <- NULL
dmanameout <- NULL

## animals potential for DMA extension ----

if (55 %in% egsas$ACTION_NEW) {
  print("beg 55 line 375 A&SZ")
  
  if (isolate(criteria$DMAapp) == "acoudet") {
    prot.tr <- extapz.tr
  } else {
    prot.tr <- extdma.tr
  }
  
  ##assess which DMA they are in using sp - can be deleted once all is working
  # for (i in names(prot.tr)) {
  #   if (exists("actionext_indlist") == FALSE) {
  #     indDMA <- sp::over(eg.tr, as(prot.tr[[i]], "SpatialPolygons"))
  #     indDMA[indDMA == 1] <- i
  #     actionext <- cbind(egsas, indDMA)
  #     actionext_sig <- actionext %>%
  #       filter(ACTION_NEW == 55) %>%
  #       dplyr::select("DateTime",
  #                     "LATITUDE",
  #                     "LONGITUDE",
  #                     "GROUP_SIZE",
  #                     "sightID",
  #                     "indDMA")
  #     actionext_indlist <- list(actionext_sig)
  #   } else {
  #     indDMA <- sp::over(eg.tr, as(prot.tr[[i]], "SpatialPolygons"))
  #     indDMA[indDMA == 1] <- i
  #     actionext <- cbind(egsas, indDMA)
  #     actionext_sig <- actionext %>%
  #       filter(ACTION_NEW == 55) %>%
  #       dplyr::select("DateTime",
  #                     "LATITUDE",
  #                     "LONGITUDE",
  #                     "GROUP_SIZE",
  #                     "sightID",
  #                     "indDMA")
  #     actionext_indlist <-
  #       list.append(actionext_indlist, actionext_sig)
  #   }
  # }
  
  ##assess which DMA they are in using sf 251121 rewrite - lets hope this works
  #helper: return 1/NA like sp::over()
  inside_flag <- function(points, polys) {
    r <- lengths(st_intersects(points, polys))
    ifelse(r > 0, 1, NA)
  }
  
  # main loop
  for (i in names(prot.tr)) {
    
    # sf replacement for sp::over()
    indDMA <- inside_flag(eg.tr, prot.tr[[i]])
    
    # mimic your original behavior: turn “1” into the DMA name
    indDMA[indDMA == 1] <- i
    
    actionext <- cbind(egsas, indDMA)
    
    actionext_sig <- actionext %>%
      filter(ACTION_NEW == 55) %>%
      dplyr::select(DateTime,
                    LATITUDE,
                    LONGITUDE,
                    GROUP_SIZE,
                    sightID,
                    indDMA)
    
    if (!exists("actionext_indlist")) {
      actionext_indlist <- list(actionext_sig)
    } else {
      actionext_indlist <- list.append(actionext_indlist, actionext_sig)
    }
  }
  print("actionext_indlist")
  print(actionext_indlist)
  
  ##cycle through all animals for each extension Slow Zone
  fullextlist <- lapply(actionext_indlist, function(x) {
    #print("enter the ext list")
    actionext_ind <- x
    ##animals that are in a Slow Zone up for extension
    ##factor to numeric -- maybe not be necessary
    actionext_ind$indDMA <- as.character(actionext_ind$indDMA)
    actionext_ind$indDMA <- as.numeric(actionext_ind$indDMA)
    #filter out the sightings that aren't in any of these Slow Zones up for extension
    actionext_ind %>% filter(!is.na(indDMA))
  })
  print("fullextlist")
  print(fullextlist)
  
  uniqueextlist <- lapply(fullextlist, function(x) {
    x %>%
      distinct(indDMA)
  })
  print("uniqueextlist")
  print(uniqueextlist)
  uniqueext <- bind_rows(uniqueextlist)
  print("uniqueext")
  print(uniqueext)
  DMAlist <- as.list(uniqueext$indDMA)
  print("DMAlist")
  print(DMAlist)
  
  #test if the sightings in each DMA will trigger an extension (are there enough within the right distance to each other) #this OG code - no sf rewrites as of 251220
  comboext <- lapply(fullextlist, function(x) {
    #print(x)
    actionfil <- x
    ##distance between points matrix -- compares right whale sightings positions to each other
    comboext <- reshape::expand.grid.df(actionfil, actionfil)
    names(comboext)[7:12] <-
      c("DateTime2",
        "LATITUDE2",
        "LONGITUDE2",
        "GROUP_SIZE2",
        "sightID2",
        "indDMA2")
    comboext$GROUP_SIZE <- as.character(comboext$GROUP_SIZE)
    comboext$GROUP_SIZE <- as.numeric(comboext$GROUP_SIZE)
    
    ##calculates core area
    
    if (isolate(criteria$DMAapp) == "acoudet") {
      setDT(comboext)[, corer := 20]
    } else {
      setDT(comboext)[, corer := round(sqrt(GROUP_SIZE / (pi * egden)), 2)]
    }
    
    ##calculates distance between points in nautical miles
    setDT(comboext)[, dist_nm := geosphere::distVincentyEllipsoid(
      matrix(c(LONGITUDE, LATITUDE), ncol = 2),
      matrix(c(LONGITUDE2, LATITUDE2), ncol = 2),
      a = 6378137,
      f = 1 / 298.257222101
    ) * m_nm]
    comboext
  })
  print("comboext")
  #print(comboext)
  names(comboext) <- DMAlist
  print(comboext)
  
  #filters out points compared where core radius is less than the distance between them (meaning that the position combo will not have overlapping core radii) and
  #keeps the single sightings where group size would be enough to trigger a DMA (0 nm dist means it is compared to itself)
  #I don't remember why I named this dmacand -- maybe dma combo and... then some?
  # applied over the list of sightings that fall within each DMA
  
  extdf_list <- lapply(comboext, function(x) {
    #print(x)
    dmacandext <- x %>%
      dplyr::filter((x$dist_nm != 0 &
                       x$dist_nm <= x$corer) | (x$GROUP_SIZE > 2 & x$dist_nm == 0))
    #print(dmacandext)
    DMAid <- unique(x$indDMA)
    #print("DMAid")
    #print(DMAid)
    ##filters for distinct sightings that should be considered for DMA calculation
    dmaextsightID <-
      data.frame(sightID = c(dmacandext$sightID, dmacandext$sightID2)) %>%
      distinct()
    #print("dmaextsightID")
    #print(dmaextsightID)
    
    #blank df for the dmas to enter
    extdf_list <- data.frame(
      extDMAs = NA,
      TRIGGER_GROUPSIZE = NA,
      TRIGGERDATE = NA,
      TRIGGERORG = NA
    )
    
    if (nrow(dmaextsightID) > 0) {
      ## this section determines observer_organization for extended dma input into the dmainfo table in Oracle.
      ## For cases where sightings from multiple organizations are considered together, this code picks the organization that has the most sightings that contribute.
      if (isolate(criteria$triggrptrue) == TRUE &
          isolate(criteria$DMAapp) == "vissig") {
        obs_org <- left_join(dmaextsightID, egsas, by = "sightID") %>%
          group_by(GROUP_SIZE) %>%
          mutate(rank = rank(GROUP_SIZE, ties.method = "first")) %>%
          filter(rank == 1) %>%
          ungroup()
        #print(obs_org)
        
        obs_org <- obs_org %>%
          distinct(OBSERVER_ORG)
        #print(obs_org)
        
        exttot <- left_join(dmaextsightID, egsas, by = "sightID") %>%
          dplyr::select(sightID,
                        DateTime,
                        LATITUDE,
                        LONGITUDE,
                        GROUP_SIZE,
                        OBSERVER_ORG) %>%
          distinct() %>%
          arrange(sightID) %>%
          summarise(
            total = sum(GROUP_SIZE),
            TRIGGERDATE = min(DateTime),
            OBSERVER_ORG = obs_org$OBSERVER_ORG
          )
        
      } else if (isolate(criteria$triggrptrue) == TRUE &
                 isolate(criteria$DMAapp) == "acoudet") {
        exttot <- left_join(dmaextsightID, egsas, by = "sightID") %>%
          dplyr::select(sightID, DateTime, LATITUDE, LONGITUDE, GROUP_SIZE) %>%
          distinct() %>%
          arrange(sightID) %>%
          summarise(
            total = sum(GROUP_SIZE),
            TRIGGERDATE = min(DateTime),
            OBSERVER_ORG = 82
          ) #82 is Robots4Whales
        
      } else {
        exttot <- left_join(dmaextsightID, egsas, by = "sightID") %>%
          dplyr::select(sightID, DateTime, LATITUDE, LONGITUDE, GROUP_SIZE) %>%
          distinct() %>%
          arrange(sightID) %>%
          summarise(
            total = sum(GROUP_SIZE),
            TRIGGERDATE = min(DateTime),
            OBSERVER_ORG = 1
          ) #1 is NEFSC
      }
      #print("extension total")
      #print(exttot)
    }
    
    ##DMAid will pass into the next for loop
    ##the below doesn't mean anything going forward for egsas
    ##this is all part of the lapply to make the extdf_list
    print("dmaextsightID #1 line 606 A&SZ")
    print(dmaextsightID)
    
    for (i in 1:nrow(egsas))
      if (egsas$sightID[i] %in% dmaextsightID$sightID) {
        #print(i)
        #print("1")
        egsas$ACTION_NEW[i] = 55
        #print(egsas)
        df <- data.frame(
          extDMAs = DMAid,
          TRIGGER_GROUPSIZE = exttot$total,
          TRIGGERDATE = exttot$TRIGGERDATE,
          TRIGGERORG = exttot$OBSERVER_ORG
        )
        print("A&SZ line 621")
        #print(df)
        extdf_list <- rbind(extdf_list, df)
        
      } else {
        #print(i)
        #print("3")
        egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
      }
    print("extdf_list")
    extdf_list
  })
  
  for (i in 1:nrow(egsas))
    if (is.na(egsas$ACTION_NEW[i])) {
      #is.na = DMA 4 animals
      print(i)
      print("4")
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    } else if (exists("dmaextsightID") &&
               egsas$sightID[i] %in% dmaextsightID$sightID) {
      print(i)
      print("1")
      egsas$ACTION_NEW[i] = 55
      print(head(egsas))
      df <- data.frame(
        extDMAs = DMAid,
        TRIGGER_GROUPSIZE = exttot$total,
        TRIGGERDATE = exttot$TRIGGERDATE,
        TRIGGERORG = exttot$OBSERVER_ORG
      )
      print(df)
      extdf_list <- rbind(extdf_list, df)
      #print(extdf)
    } else if (egsas$ACTION_NEW[i] == 55) {
      print(i)
      print("2")
      egsas$ACTION_NEW[i] = 2 #still in protected zone, but not trigger anything
      print(egsas)
    } else {
      print(i)
      print("3")
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
  print("here")
  
  print("extension details")
  print(extdf_list)
  extdf <- bind_rows(extdf_list, .id = "column_label")
  print("extdf")
  #print(extdf)
  
  extdf <- extdf %>%
    filter(!is.na(extDMAs)) %>%
    distinct()
  
  print(dplyr::left_join(extdf, actdmadf, by = c("extDMAs" = "ID")))
  extdf$extDMAs <- as.integer(extdf$extDMAs)
  
  extdfname <-
    dplyr::left_join(extdf, actdmadf, by = c("extDMAs" = "ID")) %>%
    mutate(INITOREXT = "e") %>%
    dplyr::select(extDMAs,
                  NAME,
                  INITOREXT,
                  TRIGGER_GROUPSIZE,
                  TRIGGERDATE,
                  TRIGGERORG) %>%
    dplyr::rename("ID" = "extDMAs") %>%
    distinct()
  
  extdfbounds <-
    left_join(extdf, actdmadf, by = c("extDMAs" = "ID")) %>%
    dplyr::select(extDMAs, VERTEX, LAT, LON) %>%
    dplyr::rename("ID" = "extDMAs") %>%
    distinct()
  print("extdfbounds")
  print(extdfbounds)
  
  print("end 55")
  
  ##this section I copied from above because I am not clever enough right now
  dmaextsightID <- lapply(comboext, function(x) {
    dmacandext <- x %>%
      dplyr::filter((x$dist_nm != 0 &
                       x$dist_nm <= x$corer) | (x$GROUP_SIZE > 2 & x$dist_nm == 0))
    ##filters for distinct sightings that should be considered for DMA calculation
    data.frame(sightID = c(dmacandext$sightID, dmacandext$sightID2)) %>%
      distinct()
  })
  
  allcomboext <- bind_rows(comboext, .id = "column_label")
  alldmaextsightID <- bind_rows(dmaextsightID, .id = "column_label")
  print("allcomboext")
  print(allcomboext)
  print("alldmaextsightID")
  print(alldmaextsightID)
  
  ##
  
  dmaextsig <-
    inner_join(allcomboext, alldmaextsightID, by = "sightID")
  dmaextsights <- dmaextsig %>%
    dplyr::select(DateTime, LATITUDE, LONGITUDE, GROUP_SIZE, sightID, indDMA) %>%
    distinct() %>%
    mutate(corer = round(sqrt(GROUP_SIZE / (pi * egden)), 2)) %>%
    as.data.frame()
  
  dmaextsights$GROUP_SIZE <- as.numeric(dmaextsights$GROUP_SIZE)
  
  if (nrow(dmaextsights) > 0) {
    #core radius in meters
    dmaextsights <- dmaextsights %>%
      mutate(extcorer_m = dmaextsights$corer * 1852,
             extPolyID = 1:nrow(dmaextsights))
    
    ###copy for spatializing
    dmaextdf <- dmaextsights
    print("dmaextdf")
    print(dmaextdf)
    
    ## df to spatial object ----
    ##declare which values are coordinates
    print("A&SZ line 743") 
    #coordinates(dmaextdf) <-  ~ LONGITUDE + LATITUDE
    ##declare what projection they are in
    #proj4string(dmaextdf) <- CRS.latlon #delete - unnecessary now
    dmaextdf.sp <- st_as_sf(dmaextdf, coords = c("LONGITUDE", "LATITUDE"), remove = FALSE, crs = 4326)
    ##transform projection
    dmaextdf.tr <- sf::st_transform(dmaextdf.sp, 32619) #or set UTM zone with crs = xxxx was CRS.utm
    print("dmaextdf.tr") #coords should be in m for buffering
    print(dmaextdf.tr)
    
    ##gbuffer/st_buffer both need utm to calculate radius in meters
    dmaextbuff <- st_buffer(dmaextdf.tr, dist = dmaextdf.tr$extcorer_m) #251218 was dmaextdf$extcorer_m
      #gBuffer(dmaextdf.tr,byid = TRUE, width = dmaextdf$extcorer_m,capStyle = "ROUND") #old sp way
    print("dmaextbuff")
    print(dmaextbuff) #simple feature collection (polygons) X features and X fields
      
    ##buffer data back to latlon dataframe
    ##this will be used later when sightings are clustered by overlapping core radiis
    extclustdf <- sf::st_transform(dmaextdf.tr, 4326) 
    #extclustdf_sf <- extclustdf #this should also work for display of the buffers in leaflet with below lines unneeded in sf but needed for downstream joins
    extclustdf <- st_drop_geometry(extclustdf) #to send back to a df only should still have lat and long columns
    extclustdf <- as.data.frame(extclustdf)
    print("extclustdf")
    print(extclustdf)
    print(str(extclustdf)) #does it need to be latlon dataframe or something different?
    
    ##creates a dataframe from the density buffers put around sightings considered for DMA analysis
    #extpolycoord <- dmaextbuff %>% 
       #fortify() %>% dplyr::select("long", "lat", "id")  #THIS NEEDS TO BE REWRITEN - FORTIFY IS FOR SP OBJECTS SEE BELOW FOR NEW DMA SECTION
     #  sf::st_drop_geometry() %>%
      #dplyr::select("LONGITUDE", "LATITUDE", "extPolyID") %>%
      #mutate(id = row_number())

    #print("extpolycoord")
    #print(extpolycoord)
    #print(str(extpolycoord))
    
    ##poly coordinates out of utm spatial stuff here seems unneeeded - just take the lat long sf object above and make polys therefrom
    print("A&SZ line 782") 
    #coordinates(extpolycoord) <-  ~ long + lat #named correctly?
    #proj4string(extpolycoord) <- CRS.utm
    #extpolycoord.sp <- st_as_sf(extpolycoord, coords = c("LONGITDUE", "LATITUDE"), remove = FALSE, crs = 32619) 
    #st_crs(extpolycoord.sp) <- CRS.utm
    #extpolycoord.tr <- sf::st_transform(extpolycoord, CRS.latlon)
    #extpolycoorddf <- as.data.frame(extpolycoord)
    #extpolycoorddf$id <- as.numeric(extpolycoorddf$id)
    #extpolycoorddf$extPolyID <- as.numeric(extpolycoorddf$extPolyID)
    
    #trying easier version
    extpolycoorddf <- st_transform(dmaextbuff, 4326) #THIS COULD BE VERY WRONG but isn't a dataframer version despite name
    # Preserve ID and extPolyID for downstream joins
    extpolycoorddf$id <- as.numeric(extpolycoorddf$extPolyID) #making it have an id column despite changing the extidpoly < - split() line below
    extpolycoorddf$extPolyID <- dmaextdf.tr$extPolyID
    print("extpolycoorddf")
    print(extpolycoorddf) #check columns for id!
    
    # Transform buffers back to lat/lon for Leaflet display (renamed but same as extpolycoorddf above)
    extpolycoorddf_sp <- st_transform(dmaextbuff, 4326)
    
    # Preserve ID and extPolyID for downstream joins
    extpolycoorddf_sp$id <- dmaextdf.tr$id #.tr doesn't have id so this shouldn't work
    extpolycoorddf_sp$extPolyID <- dmaextdf.tr$extPolyID
    
    print("extpolycoorddf_sp")
    print(extpolycoorddf_sp) #THIS IS WHAT GETS SENT TO LEAFLET
    print(str(extpolycoorddf_sp)) #CHECK TO MAKE SURE THIS IS THE CORRECT OUTPUT SF Collection with VALID CRS, POLY GEOMETRY, X features and Y field
    
    ## the circular core areas are the polygons in the below section  #this repeats belows for new dma - needed same rewrites
    extidpoly <- split(extpolycoorddf, extpolycoorddf$extPolyID) #was split(extpolycoorddf, extpolycoorddf$id)
    extidpoly <- lapply(extidpoly, function(x) {
      x["id"] <- NULL
      x
    })
    print("extidpoly")
    print(extidpoly)
    
    #think this is unnecessary now     
    # extpcoord <- lapply(extidpoly, function(df) {
    #   # convert data-frame columns to numeric matrix (LONG then LAT)
    #   coords <- as.matrix(df[, c("LONGITUDE", "LATITUDE")])
    #   storage.mode(coords) <- "double"
    #   st_polygon(list(coords))
    # })
    # poly_sfc <- try(st_sfc(extpcoord, crs = 4326), silent = FALSE)
    # extpolycoorddf_sp <- st_sf(
    #   ID = names(extidpoly),
    #   geometry = poly_sfc
    # )
    
    # extpcoord <- lapply(extidpoly, Polygon)
    # extpcoord_ <-
    #   lapply(seq_along(extpcoord), function(i)
    #     Polygons(list(extpcoord[[i]]), ID = names(extidpoly)[i]))
    # extpolycoorddf_sp <-
    #   SpatialPolygons(extpcoord_, proj4string = CRS.latlon) #sp
    # #sf rewrite converting
    
    ##CALLS FUNCTION AT TOP OF SCRIPT (lines 7-95) hopefully sf re-write works correctly here as well
    ext_clustdf_fun_out <- clustdf_fun_sf(extidpoly, extpolycoorddf_sp) #y must be sf
    extclustdf$extPolyID <- as.numeric(extclustdf$extPolyID)
    
    extclustdf <- full_join(ext_clustdf_fun_out,
                extclustdf,
                by = c("upoly" = "extPolyID"))
    print("extclustdf")
    print(extclustdf)
    
    extclusty <- extclustdf %>%
      group_by(cluster) %>%
      mutate(totes = sum(GROUP_SIZE)) %>%
      filter(totes >= 3)
    extclustn <- extclustdf %>%
      group_by(cluster) %>%
      mutate(totes = sum(GROUP_SIZE)) %>%
      filter(totes < 3)
    
    print("yes")
    print("extclusty")
    print(extclusty)
    #print(extclustn)
    #print(egsas)
    
    totalnew <- extclusty %>%
      ungroup() %>%
      distinct(indDMA, cluster, totes) %>%
      group_by(indDMA) %>%
      summarise (n = sum(totes))
    
    print("totalnew")
    print(totalnew)
    
    for (i in 1:nrow(egsas))
      if (is.na(egsas$ACTION_NEW[i])) {
        #is.na = DMA 4 animals
        egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
      } else if (egsas$sightID[i] %in% alldmaextsightID$sightID &
                 (egsas$sightID[i] %in% extclusty$sightID)) {
        egsas$ACTION_NEW[i] = 5
        #print(head(egsas))
      } else if (egsas$ACTION_NEW[i] == 55) {
        egsas$ACTION_NEW[i] = 2 #still in protected zone, but not trigger anything
      } else {
        egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
      }
    print("egsas")
    print(egsas)
    
  } #276
} #end 55 in action_new


## animals potential for new DMA ----
if (NA %in% egsas$ACTION_NEW) {
  print("beg ACTION_NEW NAs")
  ##only taking ACTION_NEW = na
  actionna <- egsas %>%
    filter(is.na(egsas$ACTION_NEW)) %>%
    ##calculates whale density radius
    mutate(corer = round(sqrt(GROUP_SIZE / (pi * egden)), 2)) %>%
    dplyr::select("DateTime",
                  "LATITUDE",
                  "LONGITUDE",
                  "GROUP_SIZE",
                  "sightID",
                  "corer")
  
  ##distance between points matrix -- compares right whale sightings positions to each other
  combo <- reshape::expand.grid.df(actionna, actionna)
  names(combo)[7:12] <-
    c("DateTime2",
      "LATITUDE2",
      "LONGITUDE2",
      "GROUP_SIZE2",
      "sightID2",
      "corer2")
  combo$GROUP_SIZE <- as.character(combo$GROUP_SIZE)
  combo$GROUP_SIZE <- as.numeric(combo$GROUP_SIZE)
  #print("combo")
  #print(combo)
  #print(summary(combo))
  
  ##calculates distance between points in nautical miles and the radii distance between points for trigger
  combo <- combo %>%
    mutate(
      dist_nm = geosphere::distVincentyEllipsoid(
        matrix(c(LONGITUDE, LATITUDE), ncol = 2),
        matrix(c(LONGITUDE2, LATITUDE2), ncol = 2),
        a = 6378137,
        f = 1 / 298.257222101
      ) * m_nm,
      total_corer = corer + corer2
    )
  print("combo")
  print(combo)
  #filters out points compared where core radius is less than the distance between them (meaning that the position combo will not have overlapping core radii) and
  #keeps the single sightings where group size would be enough to trigger a DMA (0 nm dist means it is compared to itself)
  #I don't remember why I named this dmacand -- maybe dma combo and... then some?
  
  if (isolate(criteria$DMAapp) == "acoudet") {
    dmacand <- combo %>%
      mutate(dist_clust = case_when(dist_nm <= 20 ~ 1,
                                    dist_nm > 20 & dist_nm <= 40 ~ 2)) %>%
      group_by(dist_clust) %>%
      arrange(DateTime) %>%
      slice(1) %>%
      ungroup()
    
  } else {
    dmacand <- combo %>%
      dplyr::filter((dist_nm != 0 &
                       dist_nm <= total_corer) | (GROUP_SIZE > 2 & dist_nm == 0))
    
  }
  #print("dmacand")
  #print(dmacand)
  
  ##filters for distinct sightings that should be considered for DMA calculation
  dmasightID <-
    data.frame(sightID = c(dmacand$sightID, dmacand$sightID2)) %>%
    distinct()
  
  ##if not a dma animal, action == 1
  ##if a dma animal, action == 4
  ##if a dma animal that's in an ending dma and extending the dma, action == 5
  ##the below sees if the sightings are good for DMA calc (are in the dmasightID list), and assigns action codes accordingly which is part of NOAA database
  for (i in 1:nrow(egsas))
    if (egsas$sightID[i] %in% dmasightID$sightID) {
      egsas$ACTION_NEW[i] = 44
    } else if (isolate(criteria$DMAapp) == "acoudet" &
               is.na(egsas$ACTION_NEW[i])) {
      egsas$ACTION_NEW[i] = 22 #acoustic detection within existing protection zone
    } else if (is.na(egsas$ACTION_NEW[i])) {
      egsas$ACTION_NEW[i] = 1
    } else {
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
} #end na


## Create DMA ----
#only sightings with an action of 4 will be evaluated here for DMA
print("egsas line 984 A&SZ")
print(egsas)
if (44 %in% egsas$ACTION_NEW) {
  ## CREATING A DMA
  
  ##the below gets you all the sightings you need for DMA analysis from the combo matrix above
  dmasig <- inner_join(combo, dmasightID, by = "sightID")
  #print("dmasig")
  #print(dmasig)
  
  dmasights <- dmasig %>%
    dplyr::select(DateTime, LATITUDE, LONGITUDE, GROUP_SIZE, sightID) %>%
    distinct(DateTime, LATITUDE, LONGITUDE, GROUP_SIZE, sightID) %>%
    mutate(corer = round(sqrt(GROUP_SIZE / (pi * egden)), 2)) %>%
    as.data.frame()
  #print(dmasights)
  dmasights$GROUP_SIZE <- as.numeric(dmasights$GROUP_SIZE)
  
  PolyID <- rownames(dmasights)
  #print(PolyID)
  
  #core radius in meters
  corer_m <- dmasights$corer * 1852
  dmasights <- cbind(dmasights, corer_m, PolyID)
  
  ###copy for spatializing
  dmadf <- dmasights
  #print("dmadf")
  #print(dmadf)
  
  ## df to spatial object ----
  ##declare which values are coordinates
  print("A&SZ line 1015") 
  dmadf.sp <- st_as_sf(dmadf, coords = c("LONGITUDE", "LATITUDE"), remove = FALSE, crs = 4326) #251120 update to sf/ditch rgdal
  ##transform projection
  dmadf.tr <- sf::st_transform(dmadf.sp, 32619) 
  print("dmadf.tr") #coordinates should be in m for buffering
  print(dmadf.tr)
  
  ##st_buffer (formerly gbuffer) needs utm to calculate radius in meters goes from point to polygon sf collection
  dmabuff <- st_buffer(dmadf.tr, dist = dmadf.tr$corer_m)
    #gBuffer(dmadf.tr, byid = TRUE, width = dmadf$corer_m, capStyle = "ROUND") #sp
  print("dmabuff")
  print(dmabuff)
  
  ##buffer data/dmadf.tr data back to latlon dataframe
  ##this will be used later when sightings are clustered by overlapping core radiis
  clustdf <- st_transform(dmadf.tr, 4326) 
  clustdf <- st_drop_geometry(clustdf) #to send it back to a df only should still have lat and long columns
  clustdf <- as.data.frame(clustdf)
  print("clustdf")
  print(clustdf)
  print(str(clustdf)) 
  
  ##creates a dataframe from the density buffers put around sightings considered for DMA analysis
  # polycoord <- dmabuff %>% 
  #   sf::st_drop_geometry() %>%
  #   dplyr::select("LONGITUDE", "LATITUDE", "PolyID") %>%
  #   mutate(id = row_number())
    #fortify() %>% dplyr::select("long", "lat", "id") #fortify only works for sp objects needs sf rewrite
  
  #poly coordinates out of utm
  print("polycoord A&SZ line 1045") 
  #print(str(polycoord))
  #print(polycoord)
  #polycoorddf <- polycoord 
  
  #coordinates(polycoord) <-  ~ long + lat
  #proj4string(polycoord) <- CRS.utm
  #polycoord.sp <- st_as_sf(polycoord) #251121 update to sf/ditch rgdal
  #st_crs(polycoord.sp) <- 32619 # was CRS.utm
  #polycoord.tr <- sf::st_transform(polycoord.sp, 4326) #was CRS.latlon
  #polycoorddf <- as.data.frame(polycoord.tr) 
  #polycoorddf$id <- as.numeric(polycoorddf$id) #these isn't just id unless we make it above unclear where line 771 of github script grabs it
  #polycoorddf$sightID <- as.numeric(polycoorddf$sightID)
  #polycoorddf$PolyID <- as.numeric(polycoorddf$PolyID)
  
  #updated version that worked above for extensions
  polycoorddf <- st_transform(dmabuff, 4326) #despite name, this is NOT a df
  polycoorddf$id <- as.numeric(polycoorddf$PolyID) #back up - hopefully this doesn't screw up number at all but should be better downstream
  polycoorddf$PolyID <- dmadf.tr$PolyID
  
  print("polycoorddf")
  print(str(polycoorddf))
  print(polycoorddf)
  
  # Transform buffers back to lat/lon for Leaflet display (renamed but same as polycoorddf above)
  polycoorddf_sp <- st_transform(dmabuff, 4326)
  
  # Preserve ID and PolyID for downstream joins
  polycoorddf_sp$id <- dmadf.tr$id  #.tr doesn't have id so this won't work can make from PolyID, if necessary
  polycoorddf_sp$PolyID <- dmadf.tr$PolyID
  
  print("polycoorddf_sp")
  print(polycoorddf_sp) #THIS IS WHAT GETS SENT TO LEAFLET
  print(str(polycoorddf_sp)) #CHECK TO MAKE SURE THIS IS THE CORRECT OUTPUT SF Collection with VALID CRS, POLY GEOMETRY, X features and Y field
  

  ## the circular core areas are the polygons in the below section
  idpoly <- split(polycoorddf, polycoorddf$PolyID) #251219 changed from $id but should it be 'extPolyID'? Or just PolyID?
  idpoly <- lapply(idpoly, function(x) {
    x["id"] <- NULL
    x
  })
  print("idpoly")
  print(idpoly)
  print(str(idpoly))
  
  #below seems unnecessary and can be deleted after confirming all above works
  #pcoord <- lapply(idpoly, Polygon) #making sp objects needs editing below #sp

  #make them polygons with sf returning sf object output to match code below for idpoly
  # pcoord <- lapply(idpoly, function(df) {
  #   # convert data-frame columns to numeric matrix (LONG then LAT)
  #   coords <- as.matrix(df[, c("LONGITUDE", "LATITUDE")])
  #   storage.mode(coords) <- "double"
  #   st_polygon(list(coords))
  # })
  # poly_sfc <- try(st_sfc(pcoord, crs = 4326), silent = FALSE)
  # polycoorddf_sp <- st_sf(
  #   ID = names(idpoly),
  #   geometry = poly_sfc
  # )
  
  #pcoord_ <- #OLD SP WAY
   #lapply(seq_along(pcoord), function(i)
    #Polygons(list(pcoord[[i]]), ID = names(idpoly)[i]))  #is this still needed -
  #polycoorddf_sp <- SpatialPolygons(pcoord, proj4string = CRS.latlon) #251125 update to sf language
  # print("polycoorddf_sp")
  # print(polycoorddf_sp)
  # print(str(polycoorddf_sp)) #CHECK TO MAKE SURE THIS IS THE CORRECT OUTPUT SF Collection with POLY GEOMETRY AND VALID CRS!
  
  #sp -> sf rewrites for function is at top of script (lines 7-95) - takes out gIntersection uses sf::st_intersects()
  clustdf_fun_out <- clustdf_fun_sf(idpoly, polycoorddf_sp) #y must be sf
  print("clustdf_fun_out")
  print(clustdf_fun_out)
  clustdf$PolyID <- as.numeric(clustdf$PolyID)
  
  clustdf <- full_join(clustdf_fun_out, clustdf, by = c("upoly" = "PolyID"))
  clusty <- clustdf %>%
    group_by(cluster) %>%
    mutate(totes = sum(GROUP_SIZE)) %>%
    filter(totes >= 3)
  clustn <- clustdf %>%
    group_by(cluster) %>%
    mutate(totes = sum(GROUP_SIZE)) %>%
    filter(totes < 3)
  
  for (i in 1:nrow(egsas))
    if (egsas$ACTION_NEW[i] == 44 &
        (egsas$sightID[i] %in% clusty$sightID)) {
      egsas$ACTION_NEW[i] = 4
    } else if (egsas$ACTION_NEW[i] == 44 &
               (egsas$sightID[i] %in% clustn$sightID)) {
      egsas$ACTION_NEW[i] = 1
    } else {
      egsas$ACTION_NEW[i] = egsas$ACTION_NEW[i]
    }
  
  clusty <- clusty %>%
    dplyr::rename('PolyID' = 'upoly')
  print("clusty")
  print(clusty)
  
  if (4 %in% egsas$ACTION_NEW) {
    poly_clean <- polycoorddf %>%
      dplyr::select(
        id,
        LONGITUDE,
        LATITUDE,
        DateTime,
        GROUP_SIZE,
        corer,
        corer_m,
        geometry
      )
    poly_clean$id <- as.numeric(poly_clean$id)
    corepoly <- poly_clean %>%
      right_join(clusty %>% dplyr::select(PolyID, cluster),
                 by = c("id" = "PolyID"))
    
    #corepoly <- right_join(polycoorddf, clusty, by = c('id' = 'PolyID')) 
    print("corepoly")
    print(corepoly)
    # corepoly <- corepoly%>%
    #   dplyr::select(
    #     "LONGITUDE.x",
    #     "LATITUDE.x",
    #     "id",
    #     "DateTime",
    #     "GROUP_SIZE",
    #     "corer",
    #     "corer_m",
    #     #"LONGITUDE.y",
    #     #"LATITUDE.y",
    #     "cluster") %>%
    #     dplyr::rename(
    #       LONGITUDE = LONGITUDE.x,
    #       LATITUDE = LATITUDE.x
    #       #LONGITUDE = LONGITUDE.y,
    #       #LATITUDE = LATITUDE.y
     # )
    #print("corepoly")
    #print(corepoly)
    
    ## for DMA insert ----
    
    clustersigs <- clusty %>%
      dplyr::select(PolyID, cluster, DateTime, GROUP_SIZE, sightID)
    
    clustersigs$DateTime <- ymd_hms(clustersigs$DateTime)
    print("clustersigs")
    print(clustersigs)
    
    trigsize <- clustersigs %>%
      group_by(cluster) %>%
      summarise(TRIGGER_GROUPSIZE = sum(GROUP_SIZE),
                TRIGGERDATE = min(DateTime))
    print("trigsize")
    print(trigsize)
    
    ##gets to the core for the cluster
    polymaxmin <- corepoly %>%
      group_by(cluster) %>%
      summarise(
        maxlat = max(LATITUDE),
        minlat = min(LATITUDE),
        maxlon = max(LONGITUDE),
        minlon = min(LONGITUDE)
      ) %>%
      as.data.frame()
    print("polymaxmin")
    print(polymaxmin)
    print("A&SZ line 1218")
    
    if (isolate(criteria$DMAapp) == "acoudet") {
      #20 is the nm radius that we want for the acoustic buffer, but the acoustic positions are filled as group_size of 3 by default, which already gives a 4.79 buffer
      buffnm <- 20 - round(sqrt(3 / (pi * egden)), 2)
      #print(buffnm)
    } else {
      buffnm <- 15
      
    }
    ##spatialize the corners #REDO WITH SF?
    corebounds_nw <- polymaxmin
    coordinates(corebounds_nw) <-  ~ minlon + maxlat
    proj4string(corebounds_nw) <- CRS.latlon
    
    corebounds_sw <- polymaxmin
    coordinates(corebounds_sw) <-  ~ minlon + minlat
    proj4string(corebounds_sw) <- CRS.latlon
    
    corebounds_ne <- polymaxmin
    coordinates(corebounds_ne) <-  ~ maxlon + maxlat
    proj4string(corebounds_ne) <- CRS.latlon
    
    corebounds_se <- polymaxmin
    coordinates(corebounds_se) <-  ~ maxlon + minlat
    proj4string(corebounds_se) <- CRS.latlon
    
    ##the below calculates the distance that needs to be added to each corner (the hypotenuse) to add the 15nm buffer
    dmabuffnm <- buffnm / cos(45 * pi / 180)
    
    nw <- 315
    sw <- 225
    ne <- 45
    se <- 135
    
    ##coords needs to be in degrees
    nw_p = destPoint(corebounds_nw,
                     nw,
                     dmabuffnm / m_nm,
                     a = 6378137,
                     f = 1 / 298.257222101)
    sw_p = destPoint(corebounds_sw,
                     sw,
                     dmabuffnm / m_nm,
                     a = 6378137,
                     f = 1 / 298.257222101)
    ne_p = destPoint(corebounds_ne,
                     ne,
                     dmabuffnm / m_nm,
                     a = 6378137,
                     f = 1 / 298.257222101)
    se_p = destPoint(corebounds_se,
                     se,
                     dmabuffnm / m_nm,
                     a = 6378137,
                     f = 1 / 298.257222101)
    #make buffer polygons
    #cbind the 15nm buffer point with the original cardinal direction point
    #rbind them together and then group by Clustmax
    nwdf <- as.data.frame(cbind(corebounds_nw, nw_p))
    swdf <- as.data.frame(cbind(corebounds_sw, sw_p))
    nedf <- as.data.frame(cbind(corebounds_ne, ne_p))
    sedf <- as.data.frame(cbind(corebounds_se, se_p))
    
    dma15 <- rbind(nwdf, swdf, sedf, nedf, nwdf)
    print("dma15")
    print(dma15)
    
    dma15 <- dma15 %>%
      dplyr::select(cluster, lon, lat) #%>%
      #st_as_sf(coords = c("lon", "lat"), crs = 4326) #sf additions 251218
    
    IDclust <- split(dma15, dma15$cluster) #used below in name section
    
    IDclust <- lapply(IDclust, function(x) {
     x["cluster"] <- NULL
    x
    })
    
    polyclust <- lapply(IDclust, Polygon)
    
    polyclust_ <- #is this still needed?
     lapply(seq_along(polyclust), function(i)
      Polygons(list(polyclust[[i]]), ID = names(IDclust)[i]))
    
    #print("polyclust_")
    #print(polyclust_)
    
    polyclust_sp <- dma15 %>% #new sf rewrite of SpatialPolygons Dec 2025
      group_by(cluster)%>%
      reframe(
        geometry = list({
          coords <- cbind(.data$lon, .data$lat)
          #ensure poly ring is closed
          if (!all(coords[1, ] == coords[nrow(coords), ])) {
            coords <- rbind(coords, coords[1, ])
          }
          st_polygon(list(coords))
        })
      ) %>%
      st_as_sf(crs = 4326)  # cluster is preserved as a column
    
    polyclust_sp <- polyclust_sp %>%
      mutate(id = as.character(cluster))
    
    #print("polyclust_sp")
    #print(polyclust_sp)
    
    print("new dma bounds")
    #replace fortify and extract polygon vertices
    polyclust_sp_coords <- st_coordinates(polyclust_sp) %>% as.data.frame()
    
    dmabounds <- polyclust_sp_coords %>%
      #st_coordinates() %>%
      #as.data.frame() %>%
      mutate(
        ID = polyclust_sp$cluster[L1]
      ) %>%
      group_by(ID) %>%                     # per polygon
      mutate(
        VERTEX = row_number(),             #tried vertex = L1 and it just repeated 1 for each vertex
        LAT = round(Y, 2),
        LON = round(X, 2)
      ) %>%
      ungroup() %>%
      dplyr::select(ID, VERTEX, LAT, LON)
    
    #Former SP version - keep until above works and displays correctly
    #polyclust_sp <-   ##NEEDS SF REWRITE ASAP
     # SpatialPolygons(polyclust_, proj4string = CRS.latlon)
    
    #polyclust_sp_df <- ##NEEDS SF REWRITE ASAP
     # SpatialPolygonsDataFrame(polyclust_sp, data.frame(
      #  id = unique(dma15$cluster),
       # row.names = unique(dma15$cluster)
      #))
    
    # print("new dma bounds")
    # dmabounds <- polyclust_sp %>% ##NEEDS SF REWRITE ASAP
    #   fortify() %>%
    #   mutate(LAT = round(lat, 2), LON = round(long, 2)) %>%
    #   dplyr::select(id, order, LAT, LON) %>%
    #   dplyr::rename("ID" = "id", "VERTEX" = "order")
    
    kmlcoord <- dmabounds#%>%
    #filter(Vertex != 5)
    
    ## dma name ----
    
    ##port/landmark reference
    dmaname <-
      data.frame(
        port = c(
          "Bay of Fundy Canada",
          "Portland ME",
          "Portsmouth NH",
          "Boston MA",
          "Providence RI",
          "New York NY",
          "Atlantic City NJ",
          "Ocean City MD",
          "Virginia Beach VA",
          "Martha's Vineyard MA",
          "Nantucket MA",
          "Cape Cod MA",
          "Cape Cod Bay",
          "Hyannis MA",
          "Chatham MA",
          "Cape Hatteras NC", #Five SEUS PORTS ADDED 20241230 HJF
          "Charleston SC",
          "Savannah GA",
          "Jacksonville FL",
          "Cape Canaveral FL"
        ),
        lon = c(
          -66.9317,
          -70.2500,
          -70.7333,
          -71.0833,-71.4000,
          -73.9667,
          -74.4167,
          -75.0849,
          -75.9595,-70.6167,
          -70.0833,
          -69.9778,
          -70.27,
          -70.27,
          -69.973,
          -75.525,
          -79.9238,
          -81.1341,
          -81.6206,
          -80.5900
        ),
        lat = c(
          44.7533,
          43.6667,
          43.0833,
          42.3500,
          41.8333,
          40.7833,
          39.3500,
          38.3365,
          36.8469,
          41.4000,
          41.2833,
          41.8830,
          41.80,
          41.65,
          41.686,
          35.2286,
          32.7843,
          32.1216,
          30.3572,
          28.4068
        ),
        cardinal = NA
      )
    #needs sf rewrite asap   
    dmadist <- dmaname
    print("A&SZ 1423")
    coordinates(dmadist) <-  ~ lon + lat
    proj4string(dmadist) <- CRS.latlon
    
    names(polyclust_) <- names(IDclust)
    
    for (i in names(polyclust_)) {
      #x <- list(polyclust_[[i]])
      x <- polyclust_[[i]]   
      x_sp <- SpatialPolygons(list(x), proj4string = CRS.latlon) #convert to sp first from oject of class Polygons (polyclust_)
      x_sf <- st_as_sf(x_sp) #convert to sf for centroid calcs
      #sf replacement for gCentroid
      center_sf <- st_centroid(x_sf)
      center <- as(center_sf, "Spatial") #convert it back to spatial points for geosphere::distVincentyEllipsoid below
      #old sp calls - keep until above 2 lines work and displays correctly 
      # x_sp <- SpatialPolygons(x, proj4string = CRS.latlon)
      # center <- rgeos::gCentroid(x_sp) #sf:st_centroid()
      
      dmaname <- dmaname %>%
        mutate(
          ID = i,
          disttocenter_nm = (
            geosphere::distVincentyEllipsoid(
              dmadist,
              center,
              a = 6378137,
              f = 1 / 298.257222101
            ) * m_nm
          ),
          bearing = bearingRhumb(dmadist, center) #dmadist calculated as sp but okay as bearingRhumb takes that - could update eventually 
        ) %>%
        dplyr::select(ID, everything())
      
      dmaname$cardinal[dmaname$bearing >= 337.5 |
                         dmaname$bearing < 22.5] <- 'N'
      dmaname$cardinal[dmaname$bearing >=  22.5 &
                         dmaname$bearing < 67.5] <- 'NE'
      dmaname$cardinal[dmaname$bearing >=  67.5 &
                         dmaname$bearing < 112.5] <- 'E'
      dmaname$cardinal[dmaname$bearing >= 112.5 &
                         dmaname$bearing < 157.5] <- 'SE'
      dmaname$cardinal[dmaname$bearing >= 157.5 &
                         dmaname$bearing < 202.5] <- 'S'
      dmaname$cardinal[dmaname$bearing >= 202.5 &
                         dmaname$bearing < 247.5] <- 'SW'
      dmaname$cardinal[dmaname$bearing >= 247.5 &
                         dmaname$bearing < 292.5] <- 'W'
      dmaname$cardinal[dmaname$bearing >= 292.5 &
                         dmaname$bearing < 337.5] <- 'NW'
      
      dmanametop <- dmaname %>%
        top_n(-1, disttocenter_nm) %>%
        arrange(disttocenter_nm)
      
      #rbind the list of options
      if (exists("dmanamedf") == FALSE) {
        dmanamedf <- list(dmanametop)
      } else if (length(dmanamedf) > 0) {
        dmanamedf <- list.append(dmanamedf, dmanametop)#rlist::list.append
      }
      
    }
    
    ##combine list of multiple dma names
    dmanamedf <- rbindlist(dmanamedf)
    print("dmanamedf")
    print(dmanamedf)
    ## paste together the title
    dmanamedf <- dmanamedf %>%
      mutate(
        NAME = paste0(
          round(dmanamedf$disttocenter_nm, 0),
          'nm ',
          dmanamedf$cardinal,
          ' ',
          dmanamedf$port
        ),
        INITOREXT = 'i'
      ) %>%
      dplyr::select(ID, NAME, INITOREXT)
    ## rename so that CCB does not have bearing or distance
    dmanamedf$NAME[grepl('Cape Cod Bay', dmanamedf$NAME)] <-
      'Cape Cod Bay'
    
    ##join on columns with same data type
    trigsize$cluster <- as.character(trigsize$cluster)
    dmanamedf <- left_join(dmanamedf, trigsize, by = c("ID" = "cluster"))
    print("with trigsize")
    print(dmanamedf)
    
    ## this section determines observer_organization for dma input into the dmainfo table in Oracle.
    ## For cases where sightings from multiple organizations are considered together, this code picks the organization that has the most sightings that contribute.
    
    if (isolate(criteria$triggrptrue) == TRUE &
        isolate(criteria$DMAapp) == "vissig") {
      obs_org2 <- left_join(dmasightID, egsas, by = "sightID") %>%
        group_by(GROUP_SIZE) %>%
        mutate(rank = rank(GROUP_SIZE, ties.method = "first")) %>%
        filter(rank == 1) %>%
        ungroup() %>%
        distinct(OBSERVER_ORG)
      #print(obs_org2)
      
      if (nrow(obs_org2) != 1) {
        obs_org2 <- obs_org2 %>%
          filter(OBSERVER_ORG != 0)
      }
      #print(obs_org2)
      
      if (nrow(obs_org2) != 1) {
        obs_org2 <- obs_org2 %>%
          slice(1)
      }
      #print(obs_org2)
      
      dmanamedf <- dmanamedf %>%
        mutate(TRIGGERORG = obs_org2$OBSERVER_ORG)
      
    } else if (isolate(criteria$triggrptrue) == TRUE &
               isolate(criteria$DMAapp) == "acoudet") {
      dmanamedf <- dmanamedf %>%
        mutate(TRIGGERORG = 82) #82 is Robots4Whales
    } else {
      dmanamedf <- dmanamedf %>%
        mutate(TRIGGERORG = 1) #1 is NEFSC
    }
    #print(dmanamedf)
  }
} # end of 4

if (4 %in% egsas$ACTION_NEW | (5 %in% egsas$ACTION_NEW)) {
  print("4 or 5")
  if (!exists("polyclust_sp")) {
    polyclust_sp <- SpatialPolygons(list(fakeslowzone))
  }
  
  #the below is to put a bandaid on sightings trigger extensions not having black core ring to plot.
  #add it to the list
  if (!exists("polycoorddf_sp")) {
    polycoorddf_sp <- SpatialPolygons(list(fakeslowzone))
  }
  
  if (!exists("extpolycoorddf_sp")) {
    extpolycoorddf_sp <- SpatialPolygons(list(fakeslowzone))
  }
  
  ##join dmanamedf with ext dmas
  ##ext IDs should be 1 + max(ID) of new dmas
  if (exists("dmanamedf") & exists("extdfname")) {
    extdfname$ID <- as.character(extdfname$ID)
    extdfname$NAME <- as.character(extdfname$NAME)
    extdfname$TRIGGERDATE <- ymd_hms(extdfname$TRIGGERDATE)
    
    alldmas <- rbind(dmanamedf, extdfname)
    
  } else if (!exists("dmanamedf") & exists("extdfname")) {
    alldmas <- extdfname
  } else if (exists("dmanamedf") & !exists("extdfname")) {
    alldmas <- dmanamedf
  }
  
  alldmas_trig <- alldmas %>%
    filter(TRIGGER_GROUPSIZE > 2) %>%
    group_by(ID) %>%
    slice_max(TRIGGERORG) %>%
    ungroup()
  
  print("alldmas_trig")
  print(alldmas_trig)
  
  alldmas <- alldmas_trig %>%
    mutate(ID = dense_rank(ID))
  print("alldmas")
  print(alldmas)
  
  print("do bounds exist?")
  if (exists("dmabounds") & exists("extdfbounds")) {
    alldmabounds <- rbind(dmabounds, extdfbounds)
  } else if (!exists("dmabounds") & exists("extdfbounds")) {
    alldmabounds <- extdfbounds
  } else if (exists("dmabounds") & !exists("extdfbounds")) {
    alldmabounds <- dmabounds
  }
  
  print("alldmabounds1")
  print(alldmabounds)
  
  alldmabounds <- alldmabounds %>% mutate(ID = as.character(ID))
  alldmas_trig <- alldmas_trig %>% mutate(ID = as.character(ID))
  
  alldmabounds <- alldmabounds %>%
    dplyr::right_join(alldmas_trig, by = "ID") %>% #ID NEED TO BE BOTH DOUBLE OR BOTH CHAR FIX FROM BOTH ALLDMABOUNDS AND ALLDMAS
    mutate(ID = dense_rank(ID)) %>%
    dplyr::select(ID, VERTEX, LAT, LON)
  print("alldmabounds2")
  print(alldmabounds)
  
  ##for database (excludes the 5th point to close the polygon)
  dmacoord <- alldmabounds %>%
    mutate(
      "Lat (Degree Minutes)" = paste(trunc(LAT), formatC(
        round((LAT %% 1) * 60, 0), width = 2, flag = 0
      ), "N", sep = " "),
      "Lon (Degree Minutes)" = paste(
        formatC(abs(trunc(LON)), width = 3, flag = 0),
        formatC(round((abs(
          LON
        ) %% 1) * 60, 0), width = 2, flag = 0),
        "W",
        sep = " "
      )
    ) %>%
    dplyr::rename("Lat (Decimal Degrees)" = LAT,
                  "Lon (Decimal Degrees)" = LON) %>%
    filter(VERTEX != 5)
  print("dmacoord")
  print(dmacoord)
  
  if (exists("kmlcoord")) {
    print("enter kml land")
    
    kmlcoord <- kmlcoord %>%
      dplyr::select(-VERTEX)
    
    if (nrow(kmlcoord) > 0) {
      ##KML for new dmas only
      CRS.gearth <-
        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") # gearth = google earth
      print("A&SZ 1474") 
      coordinates(kmlcoord) <-  ~ LON + LAT #named correctly?
      #may need to add code here but don't do .kmls anyway 251121
      proj4string(kmlcoord) <- CRS.latlon
      kmlcoord.sp <- st_as_sf(kmlcoord, coords = c("LONGITUDE", "LATITUDE"), remove = FALSE, crs = 4326) #251120 update to sf/ditch rgdal
      #st_crs(kmlcoord.sp) <- CRS.latlon #defined in above code
      dmabounds_kml.tr <- sf::st_transform(kmlcoord.sp, CRS.gearth)
      
      ##KML making
      output$kml <- downloadHandler(
        filename = function() {
          paste0(dma_date, ".kml")
        },
        content = function(file) {
          #convert as needed
          dmabounds_sf <- st_as_sf(dmabounds_kml.tr)
          polyclust_sf <- st_as_sf(polyclust_sp_df)
          #write first layer
          st_write(dmabounds_sf,
                   dsn = file,
                   delete_dsn = TRUE,
                   driver = "KML")
          #append 2nd layer to same kml
          st_write(
            polyclust_sf, 
            dsn = file,
            layer = "KML_DMA",
            driver = "KML",
            update = TRUE)
        }
      )
    }
  }
          #kmlPolygons(
            #obj = polyclust_sp_df,
            #kmlfile = file,
            #name = "KML DMA",
            #description = "",
            #col = NULL,
            #visibility = 1,
            #lwd = 2,
            #border = "yellow",
            #kmlname = "",
            #kmldescription = ""
  #         )
  #       }
  #     )
  #   }
  # }
  
  if (criteria$DMAapp == "acoudet") {
    pzone_ = "ASZ_"
  } else {
    pzone_ = c("DMA_")
  }
  dma_date <-
    paste0(
      pzone_,
      year(egsas$DateTime[1]),
      "_",
      strftime(egsas$DateTime[1], "%m"),
      "_",
      strftime(egsas$DateTime[1], "%d")
    )
  
  ##buttons ----
  print("enable")
  enable("dmaup")
  
  dmanameout <- alldmas %>%
    dplyr::rename("GROUP_SIZE" = "TRIGGER_GROUPSIZE")
  print("dmanameout")
  print(dmanameout)
  dmanameout$TRIGGERDATE <- as.character(dmanameout$TRIGGERDATE)
  dmanameout$GROUP_SIZE <-
    sprintf("%.0f", round(as.numeric(dmanameout$GROUP_SIZE), digits = 0))
  dmanameout$TRIGGERORG <-
    sprintf("%.0f", round(as.numeric(dmanameout$TRIGGERORG), digits = 0))
  
  #don't display group size for acoustics
  if (isolate(criteria$DMAapp) == 'acoudet') {
    dmanameout <- dmanameout %>%
      dplyr::select(-GROUP_SIZE)
  }
  
  output$dmanameout <- renderTable({
    dmanameout
  })
  output$dmacoord <- renderTable({
    dmacoord
  })
  
  print("a&sz 1641")
  #print(egsas)
  
  if ("ID" %in% colnames(egsas)) {
    if (isolate(criteria$DMAapp) == "acoudet") {
      egsastab <- egsas %>%
        dplyr::select(
          ID,
          PLATFORM,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          ACTION_NEW
        ) %>%
        mutate(CATEGORY = 7)
    } else {
      egsastab <- egsas %>%
        dplyr::select(
          ID,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          MOMCALF,
          FEEDING,
          DEAD,
          SAG,
          ENTANGLED,
          CATEGORY,
          ACTION_NEW
        )
    }
    
  } else {
    if (isolate(criteria$DMAapp) == "acoudet") {
      egsastab <- egsas %>%
        dplyr::select(
          PLATFORM,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          ACTION_NEW
        ) %>%
        mutate(CATEGORY = 7)
    } else {
      egsastab <- egsas %>%
        dplyr::select(
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          MOMCALF,
          FEEDING,
          DEAD,
          SAG,
          ENTANGLED,
          CATEGORY,
          ACTION_NEW
        )
    }
  }
  
  if (isolate(criteria$loc) == 'Network') {
  #231003 HJF example data errors if (isolate(criteria$loc) == 'Network' | criteria$path == './example_data/') {
    #The shapes that get plotted are independent of the names
    #print("label testing")
    
    sasdma <- sasdma %>%
      addPolygons(data = benigndma,
                  weight = 2,
                  color = "yellow") %>%
      addPolygons(data = extensiondma,
                  weight = 2,
                  color = "orange") %>%
      addPolygons(data = benignapz,
                  weight = 2,
                  color = "yellow",
                  dashArray = "4 8"
                  ) %>%
      addPolygons(data = extensionapz,
                  weight = 2,
                  color = "orange",
                  dashArray = "4 8"
                  ) %>%
      addLegend(
                  title = "Dynamic Management: solid border = DMA, dashed border = Acoustic",
                  colors = c("yellow", "orange", "blue"),
                  labels = c(
                  "Active zone",
                  "Active zone eligible for extension",
                  "Potential zone"
                  ),
                  opacity = 0.4,
                  position = "topleft"
                  )
  }
  
  ##display core areas for visual sightings
  if (isolate(criteria$DMAapp) == 'vissig' |
      isolate(criteria$DMAapp) == 'rwsurv') {
    sasdma <- sasdma %>%
      addPolygons(data = polyclust_sp,
                  weight = 2,
                  color = "blue") %>%
      addPolygons(data = polycoorddf_sp,
                  weight = 2,
                  color = "black") %>%
      addPolygons(data = extpolycoorddf_sp,
                  weight = 2,
                  color = "black")
      #sf attempt with reprojected back to latlong after buffering sf object 
      # addPolygons(data = polyclust_sp,
      #             weight = 2,
      #             color = "blue") %>%
      # addPolygons(data = clustdf_sf,
      #             weight = 2,
      #             color = "black") %>%
      # addPolygons(data = extclustdf_sf,
      #             weight = 2,
      #             color = "black")
    
  } else {
    sasdma <- sasdma %>%
      addPolygons(data = polyclust_sp,
                  weight = 2,
                  color = "blue",
                  dashArray = "4 8"
                  )
  }
  
} else {
  ##4 in egsas$action_new
  
  if ("ID" %in% colnames(egsas)) {
    if (isolate(criteria$DMAapp) == "acoudet") {
      egsastab <- egsas %>%
        dplyr::select(
          ID,
          PLATFORM,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          ACTION_NEW
        ) %>%
        mutate(CATEGORY = 7)
    } else {
      egsastab <- egsas %>%
        dplyr::select(
          ID,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          MOMCALF,
          FEEDING,
          DEAD,
          SAG,
          ENTANGLED,
          CATEGORY,
          ACTION_NEW
        )
    }
    
  } else {
    if (isolate(criteria$DMAapp) == "acoudet") {
      egsastab <- egsas %>%
        dplyr::select(
          PLATFORM,
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          ACTION_NEW
        ) %>%
        mutate(CATEGORY = 7)
    } else {
      egsastab <- egsas %>%
        dplyr::select(
          DateTime,
          GROUP_SIZE,
          LATITUDE,
          LONGITUDE,
          ID_RELIABILITY,
          MOMCALF,
          FEEDING,
          DEAD,
          SAG,
          ENTANGLED,
          CATEGORY,
          ACTION_NEW
        )
    }
  }
  
  if (isolate(criteria$loc) == 'Network') {
  #231003 HJF example data errors on network if (isolate(criteria$loc) == 'Network' | criteria$path == './example_data/') {
    sasdma <- sasdma %>%
      addPolygons(data = benigndma,
                  weight = 2,
                  color = "yellow") %>%
      addPolygons(data = extensiondma,
                  weight = 2,
                  color = "orange") %>%
      addPolygons(data = benignapz,
                  weight = 2,
                  color = "yellow") %>%
      addPolygons(data = extensionapz,
                  weight = 2,
                  color = "orange")
  }
  
}

## LEAFLET FINAL ----

##visual
if (isolate(criteria$DMAapp) == 'vissig' |
    isolate(criteria$DMAapp) == 'rwsurv') {
  sasdma <- sasdma %>%
      addCircleMarkers(
      lng = ~ egsas$LONGITUDE,
      lat = ~ egsas$LATITUDE,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.5 ,
      color = "black",
      popup = paste0(egsas$DateTime, ", Group Size:", egsas$GROUP_SIZE)
    ) %>%
    addLegend(
      colors = c("grey", "red", "black", "green"),
      labels = c("Shipping Lanes", "SMA", "Core area of right whale sightings", "Wind Energy Areas"),
      opacity = 0.4,
      position = "topleft"
    )
  
  ##acoustic
} else if (isolate(criteria$DMAapp) == "acoudet") {
  egsas_dma <- egsas %>% filter(ACTION_NEW == 4 | ACTION_NEW == 5)
  egsas_notdma <- egsas %>% filter(ACTION_NEW != 4 &
                                     ACTION_NEW != 5)
  
  sasdma <- sasdma %>%
    addCircleMarkers(
      lng = ~ egsas_notdma$LONGITUDE,
      lat = ~ egsas_notdma$LATITUDE,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.5 ,
      color = "grey",
      popup = egsas_notdma$DateTime
    ) %>%
    addCircleMarkers(
      lng = ~ egsas_dma$LONGITUDE,
      lat = ~ egsas_dma$LATITUDE,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.5 ,
      color = "black",
      popup = egsas_dma$DateTime
    ) %>%
    addLegend(
      colors = c("grey", "red", "black", "grey", "green"),
      labels = c(
        "Shipping Lanes",
        "SMA",
        "Right whale acoustic detection - trigger",
        "Other right whale acoustic detection",
        "Wind Energy Areas"
      ),
      opacity = 0.4,
      position = "topleft"
    )
  
}

egsastab$GROUP_SIZE <- sprintf("%.0f", round(egsastab$GROUP_SIZE, digits = 0))
egsastab$CATEGORY <- as.numeric(egsastab$CATEGORY)
egsastab$CATEGORY <- sprintf("%.0f", round(egsastab$CATEGORY, digits = 0))
egsastab$ID_RELIABILITY <- as.numeric(egsastab$ID_RELIABILITY)
egsastab$ID_RELIABILITY <- sprintf("%.0f", round(egsastab$ID_RELIABILITY, digits = 0))
egsastab$ACTION_NEW <- as.numeric(egsastab$ACTION_NEW)

### egsas table for output

if (isolate(criteria$DMAapp) == 'acoudet') {
  egsastab <- egsastab %>%
    dplyr::select(-GROUP_SIZE) %>%
    mutate(
      ACTION_NEW = replace(ACTION_NEW, ACTION_NEW == 4, 24),
      ACTION_NEW = replace(ACTION_NEW, ACTION_NEW == 5, 25)
    )
}

sas_react$egsastab <- egsastab

## On network ----
  if (isolate(criteria$loc) == 'Network') {
#231003 HJF example data errors on network if (isolate(criteria$loc) == 'Network' | criteria$path == './example_data/') {
  
  ###sas on network
  egsastabout <- sas_react$egsastab %>%
    left_join(actioncodedf, by = c("ACTION_NEW" = "ID")) %>%
    dplyr::rename("ACTION_NEW_TRANSLATION" = "ACTION")
  #print(str(egsastabout))
  egsastabout$ACTION_NEW <- sprintf("%.0f", round(egsastabout$ACTION_NEW, digits = 0))
  
  ##dmas on network
  dma_react$egsas = egsas
  dma_react$sasdma = sasdma
  dma_react$alldmas = alldmas
  
  if (exists("extdfname")) {
    dma_react$extdfname = extdfname
  } else {
    dma_react$extdfname = ""
  }
  
  dma_react$dmacoord = dmacoord
  dma_react$dmanameout = dmanameout
  
  ## Outputs ----
  
  output$egsastabout <- renderTable({
    egsastabout
  },  striped = TRUE)
  output$sasdma = renderLeaflet({
    print(dma_react$sasdma)
  })
  
}#ends network path for Oracle uploads
