library(dplyr)
library(sf)
library(raster)
library(rgeos)

# load datasets
plots = read.csv("plots.csv")
spp = read.csv("species_info.csv")
FL=read.csv("FL_TREE.csv")
# keep only the species codes that exist for species of interest (Q. chapmanii is not listed in the FIA database)
SPCDs=spp$FIA.SPCD[1:9]

# keep only variables of interest for species of interest
FL_filtered=FL%>%
  filter(SPCD%in%SPCDs)%>%
  dplyr::select("PLOT", "SUBP", "SPCD")
FL_filtered = unique(FL_filtered)
plots = plots %>% 
  dplyr::select("PLOT", "LAT", "LON")
# join lat/lon coordinates to species records
FL_filt_join = left_join(FL_filtered, plots) %>% 
  dplyr::select("SPCD", "LAT", "LON")
FL_filt_join = left_join(FL_filt_join, spp[,c(1,3)], 
                         by = c("SPCD" = "FIA.SPCD")) %>% 
  dplyr::select(-SPCD)

backup = FL_filt_join
# convert species records to simple feature
FL_filt_join = st_as_sf(FL_filt_join, 
                        coords = c("LON", "LAT"), 
                        crs = 4269)

# get political boundaries for the USA
dplyr::filter(ccodes(), NAME %in% "United States")
poly = getData("GADM", country = "USA", level = 1)
contiguous = st_as_sf(poly) %>% 
  filter(NAME_1 != "Alaska" & NAME_1 != "Hawaii")

# plot to look at distributions (need to change the spatial extent because the US colonised so many islands it's ridiculous)
plot(st_geometry(contiguous), xlim = st_bbox(contiguous)[c(1,3)], ylim = st_bbox(contiguous)[c(2,4)])
plot(st_geometry(FL_filt_join)[1:1000], add = TRUE)
