
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(plyr)
setwd("D:/My Folders/R/2016/blog/20160714_districts")

load(file = "data/data.eke.rdata")
head(data.eke)
# When we derive EKE, grid cells over land produce NaNs, so we must replace them with NAs
data.eke$eke[which(is.nan(data.eke$eke))] <- NA
#' the EKE data spans from -18.875 E to 43.875 E and -38.88 N to -68.62 N.
#' Those are the extents I needed for the full study, but its too much fr the plot, so we must subset it.

# Define our map extents
latmin <- -55
latmax <- -43
lonmin <- 20
lonmax <- 43
map.extents <- extent(lonmin, lonmax, latmin, latmax)
data.eke.sub <- subset(data.eke,
                       data.eke$lat > latmin &
                           data.eke$lat <= latmax &
                           data.eke$lon >= lonmin &
                           data.eke$lon <= lonmax)


#' **Shape Files**
#'
#' The plot includes the Prince Edward Islands. Instead of leaving blank spaces, lets use some shape files
#' These shape files come from http://www.naturalearthdata.com . They are from the fine scale (1:10), Admin 0 -Countries set
#' available, again for free(!!!) from [here](http://www.naturalearthdata.com/downloads/10m-cultural-vectors/).

za.districts <- readOGR("shapes", layer="ZAF_adm2")
za.districts@data$linkid <- rownames(za.districts@data)
za.districts.data <- za.districts@data

head(za.districts.data)

attributes((countries))
c.data <- countries@data
za.districts.df <- fortify(za.districts)

plot.colour <- ggplot() +
    geom_polygon(data = za.districts.df, aes(x = long, y = lat, group = group),
                 colour="black", fill="white", alpha=1, size = 0.3) +
    coord_map("mercator")
plot.colour

unique(za.districts.data$TYPE_2)

####################################################################

za.districts <- readOGR("Districts", layer="DistrictMunicipalities2011")
za.districts@data$id <- rownames(za.districts@data)
za.districts.data <- za.districts@data
head(za.districts.data)

za.districts.df <- fortify(za.districts)
head(za.districts.df)

za.districts.df <- join(za.districts.df, za.districts.data, by="id")
head(za.districts.df)

plot.colour <- ggplot() +
    geom_polygon(data = za.districts.df, aes(x = long, y = lat, group = group),
                 colour="black", fill="gold", alpha=1, size = 0.3) +
    coord_map("mercator")
plot.colour

unique(za.districts.data$TYPE_2)


# load and clean 2011 candidate data
# 2011 Ward candidates
library(xlsx)
ward.2011 <- read.xlsx2("data/2011-ward-candidates-list.xls", 1, startRow = 3)
ptys <- unique(ward.2011$Party)
ptys

head(ward.2011)
tail(ward.2011)

library(stringr)
dist.split <- str_split_fixed(ward.2011$Municipality, " - ", 2)
ward.2011$dist.code <- dist.split[,1]
ward.2011$dist.name <- dist.split[,2]

ward.2011.long <- ddply(ward.2011, c("dist.code", "Party"), function(df) nrow(df))
names(ward.2011.long)[3] <- "ward.2011.num"
head(ward.2011.long)

ward.2011.tot <- ddply(ward.2011.long, "dist.code", function(df) sum(df$ward.2011.num))
head(ward.2011.tot)

ward.2011.long <- join(ward.2011.long, ward.2011.tot, by = "dist.code")
names(ward.2011.long)[4] <- "ward.2011.tot"
head(ward.2011.long, 20)

ward.2011.long$ward.2011.prop <- ward.2011.long$ward.2011.num / ward.2011.long$ward.2011.tot
ward.2011.long <- arrange(ward.2011.long, dist.code, -ward.2011.prop)
head(ward.2011.long)

# 2011 P Representative candidates

pr.2011 <- read.xlsx2("data/2011-pr-candidate-lists.xls", 1, startRow = 3)
head(pr.2011)
tail(pr.2011)

dist.split <- str_split_fixed(pr.2011$Municipality, " - ", 2)
pr.2011$dist.code <- dist.split[,1]
pr.2011$dist.name <- dist.split[,2]

pr.2011.long <- ddply(pr.2011, c("dist.code", "Party"), function(df) nrow(df))
names(pr.2011.long)[3] <- "pr.2011.num"
head(pr.2011.long)

pr.2011.tot <- ddply(pr.2011.long, "dist.code", function(df) sum(df$pr.2011.num))
head(pr.2011.tot)

pr.2011.long <- join(pr.2011.long, pr.2011.tot, by = "dist.code")
names(pr.2011.long)[4] <- "pr.2011.tot"
head(pr.2011.long, 20)

pr.2011.long$pr.2011.prop <- pr.2011.long$pr.2011.num / pr.2011.long$pr.2011.tot
pr.2011.long <- arrange(pr.2011.long, dist.code, -pr.2011.prop)
head(pr.2011.long)

# Join the 2011 data

candidates.2011 <- join(pr.2011.long, ward.2011.long, by = c("dist.code", "Party"))
head(candidates.2011, 1000)
candidates.2011[1:1000, c(1:3)]
candidates.2011[40:332, ]

length(unique(candidates.2011$Party)) == length(unique(pr.2011.long$Party))
length(unique(candidates.2011$Party)) == length(unique(ward.2011.long$Party))

# quick test plot
head(za.districts.df)
head(candidates.2011)
unique(candidates.2011$Party)


data.party <- subset(candidates.2011, candidates.2011$Party == "AFRICAN NATIONAL CONGRESS")
summary(data.party$ward.2011.prop)
hist(data.party$ward.2011.prop)


length(unique(za.districts.df$DISTRICT))
?merge
plot.data <- merge(za.districts.df, data.party, by.x = "DISTRICT", by.y = "dist.code", all = FALSE)
length(unique(plot.data$pr.2011.prop))

head(plot.data)
names(plot.data)
length(unique(plt.data$dist.code))

min(plot.data$pr.2011.prop, na.rm = TRUE)
max(plot.data$pr.2011.prop, na.rm = TRUE)


norm.col <- function(col){
    out <- (col - min(col))/(max(col) - min(col))
    return(out)
}



plot.data$pr.2011.norm <- norm.col(plot.data$pr.2011.prop)

hist(plot.data$pr.2011.norm)

summary(plot.data$ward.2011.prop)
hist(plot.data$ward.2011.prop)

plot.colour <- ggplot() +
    geom_polygon(data = plot.data, aes(x = long, y = lat, group = group, alpha = pr.2011.norm),
                 fill="gold", colour = "grey") +
    coord_map("mercator")
plot.colour


# load and clean 2016 candidate data
# 2016 Ward candidates
ward.pr.2016 <- read.csv("data/Electoral_Candidates_2016.csv")
head(ward.pr.2016)
tail(ward.pr.2016)

summary(ward.pr.2016$PR.List.OrderNo...Ward.No)

ward.2016 <- subset(ward.pr.2016, ward.pr.2016$PR.List.OrderNo...Ward.No > 1000)
names(ward.2016)[4] <- "ward.no"
summary(ward.2016$ward.no)

pr.2016 <- subset(ward.pr.2016, ward.pr.2016$PR.List.OrderNo...Ward.No < 1000)
names(pr.2016)[4] <- "list.order.no"
summary(pr.2016$list.order.no)

names(pr.2016)


# 2016 P Representative candidates


dist.split <- str_split_fixed(pr.2016$Municipality, " - ", 2)
pr.2016$dist.code <- dist.split[,1]
pr.2016$dist.name <- dist.split[,2]

pr.2016.long <- ddply(pr.2016, c("dist.code", "Party"), function(df) nrow(df))
names(pr.2016.long)[3] <- "pr.2016.num"
head(pr.2016.long)
unique(pr.2016.long$dist.code)

pr.2016.tot <- ddply(pr.2016.long, "dist.code", function(df) sum(df$pr.2016.num))
head(pr.2016.tot)

pr.2016.long <- join(pr.2016.long, pr.2016.tot, by = "dist.code")
names(pr.2016.long)[4] <- "pr.2016.tot"
head(pr.2016.long, 20)

pr.2016.long$pr.2016.prop <- pr.2016.long$pr.2016.num / pr.2016.long$pr.2016.tot
pr.2016.long <- arrange(pr.2016.long, dist.code, -pr.2016.prop)
head(pr.2016.long)

candidates.2016 <- pr.2016.long

# quick test plot
head(za.districts.df)
head(candidates.2016)
unique(candidates.2016$Party)


data.party.2016 <- subset(candidates.2016, candidates.2016$Party == "ECONOMIC FREEDOM FIGHTERS")
summary(data.party.2016$pr.2016.prop)
hist(data.party.2016$pr.2016.prop)


length(unique(za.districts.df$DISTRICT))
?merge
plot.data.2016 <- merge(za.districts.df, data.party.2016, by.x = "DISTRICT", by.y = "dist.code", all = FALSE)
length(unique(plot.data.2016$pr.2016.prop))
length(unique(plot.data.2016$DISTRICT))

plot.data.2016$alpha <- (1 - plot.data.2016$pr.2016.prop)

head(plot.data.2016)
names(plot.data.2016)
length(unique(plot.data.2016$dist.code))

plot.data.2016$pr.2016.prop[which(plot.data.2016$DISTRICT == "CPT")]

min(plot.data.2016$pr.2016.prop, na.rm = TRUE)
max(plot.data.2016$pr.2016.prop, na.rm = TRUE)

plot.data.2016$pr.2016.norm <- norm.col(plot.data.2016$pr.2016.prop)

plot.colour <- ggplot() +
    geom_polygon(data = plot.data.2016, aes(x = long, y = lat, group = group, alpha = pr.2016.norm),
                 fill="red", colour = "grey") +
    coord_map("mercator")
plot.colour
















za.zones <- readOGR("meso_2010_base_dd", layer="meso_2010_base_dd")
za.zones@data$id <- rownames(za.municipal@data)

za.zones.data <- za.zones@data

a <- attributes((za.municipal))
b <- getSlots(za.municipal)
summary(a)

tail(za.municipal@data)
head(za.municipal@comment)

which(max(za.municipal@data$AREA_TOT))

length(unique(za.municipal@data$MESO_ID))



za.zones.fortified <- fortify(za.zones)

head(za.zones.fortified)
head(za.zones.data)

za.zones.df <- join(za.zones.fortified, za.zones.data, by="id")
za.sub <- subset(za.zones.df, za.zones.df$AREA == max(za.zones.df$AREA))
head(za.sub)
head(za.municipal.df)



# subset South Africa and her Islands and tidy up a bit
za <- subset(countries.df, countries.df$ISO_A2=="ZA")
rm(countries, countries.df)

plot.colour <- ggplot() +
    geom_polygon(data = za.sub, aes(x = long, y = lat, group = group),
                 colour="black", fill="white", alpha=1, size = 0.3) +
    coord_map("mercator")
plot.colour



candidates <- read.csv("D:/My Folders/GIS_data/other_data/electoral_candidates_2016/Electoral_Candidates_2016.csv")

names(candidates)
length(unique(candidates$Municipality))


municipal

cod.2013 <- read.csv("D:/My Folders/GIS_data/other_data/causesofdeath2013/CausesOfDeath2013_F1.csv")
head(cod.2013)
cod.2013$DeathProv.2 <- as.factor(cod.2013$DeathProv)
summary(cod.2013$DeathProv.2)


countries@data$id <- rownames(countries@data)
# Fortify the spatial polygons in preparation for ggplot2
countries.df <- fortify(countries)
countries.df <- join(countries.df, countries@data, by="id")

# subset South Africa and her Islands and tidy up a bit
za <- subset(countries.df, countries.df$ISO_A2=="ZA")
rm(countries, countries.df)
























#' **Geotiff**
#'
#' I need the plot to have some bathymetric contour lines. I am using the ETOPO1 data for this via a geotiff.
#' There are higher resolution data sets around (e.g. ETOTPO2 and GEBCO) but ETOPO1 will suffice for this applicaiton.
#' the ETOPO1 data is available from [here](http://www.ngdc.noaa.gov/mgg/global/global.html), again free I think.
etopo1.full <- raster("etopo/ETOPO1_Ice_c_geotiff.tif")
# Crop out our ROI
etopo.crop <- crop(etopo1.full, map.extents)
# Prepare for ggplot2 and tidy up a bit
etopo.crop.df <- as.data.frame(etopo.crop, xy=TRUE)
names(etopo.crop.df) <- c("long", "lat", "z")
rm(etopo1.full)

#' **Polygon**
#'
#' Build a simple polygon to show where we defined the region of elevated EKE
eflatmin <- -53
eflatmax <- -47.33
eflonmin <- 27.33
eflonmax <- 37.66

# ef.extents <- extent(eflonmin, eflonmax, eflatmin, eflatmax)
ef.coords <- cbind(c(eflonmin, eflonmax, eflonmax, eflonmin, eflonmin),
                   c(eflatmax, eflatmax, eflatmin, eflatmin, eflatmax))
ef.poly <- Polygon(ef.coords)
# Prepare for ggplot2 and tidy up
ef.df <- fortify(ef.poly)
ef.df$group <-1
rm(ef.coords, ef.poly)

#' ### Coloured plot time
#'
#' Finally we have all the bits in place so we can build a plot. We'll start with the colourful version,
#' and then refine it to the grey scale contoured version.
#'
#' **Preliminaries**

# define some lables and their coordinates
lbl <- data.frame(x = c(40, 36, 30, 37.1, 22.5),
                  y = c(-46.5, -53.4, -50, -45, -51.5),
                  txt = c("Marion Island", "Eddy Field", " ", "SWIR", "SWIR"))
# Define the theme data as I like it for these plots...
thm <- theme_bw() +
    theme(axis.text.x = element_text(size=8, face = "plain"),
          axis.text.y = element_text(size=8, face = "plain"),
          axis.title.x = element_text(size=8, face = "plain"),
          axis.title.y = element_text(size=8, face = "plain"),
          axis.ticks.x = element_line(size=0.3),
          axis.ticks.y = element_line(size=0.3),
          legend.key.height = unit(13, units="mm"),
          legend.text = element_text(size=8, face = "plain"),
          legend.title = element_text(size=8, angle = 90, vjust = 1, face = "plain"),
          legend.title.align = 0.5,
          panel.border = element_rect(colour = "black", fill=NA, size=.3))

#' **Coloured tiles**
plot.eke.colour <- ggplot() +
    geom_tile(data=data.eke.sub, aes(x=lon,y=lat,fill=eke)) +
    geom_polygon(data = za, aes(x = long, y = lat, group = group),
                 colour="black", fill="white", alpha=1, size = 0.3) +
    geom_polygon(data=ef.df, aes(x=long, y=lat, group=group),
                 colour="black", fill="white", alpha=0, linetype="dashed", size = 0.3) +
    geom_contour(data=etopo.crop.df, aes(x=long,y=lat,z=z),
                 breaks=c(-3000), colour="black", size = 0.3) +
    labs(x = 'Longitude (ºE)', y = 'Latitude (ºN)') +
    coord_map("mercator") +
    coord_fixed(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), expand = FALSE) +
    scale_fill_gradientn(colours = rev(rainbow(7, end=4/6)),
                         space = "Lab",
                         guide = guide_colorbar(title="Eddy kinetic energy (cm²/s²)",
                                                title.position="right")) +
    geom_text(data=lbl, aes(x=x, y=y, label=txt), size=rel(3), colour="white", fontface="bold", alpha = 1) +
    geom_text(data=lbl, aes(x=x+0.01, y=y+0.01, label=txt), size=rel(3), colour="black", fontface="bold") +
    thm
plot.eke.colour
#' That looks pretty good. Unfortunately when we save the file, things don't always look the same as they do on the screen so
#' theres generally some fine tuning involved. This version, as it stands, makes rather a good print. The colour pallette is familiar to all my matlab/python
#' oceanographic colleages too.
# Saving the plot accoring to the publishers requirements
ggsave(filename = 'plot.eke.colour.png', plot = plot.eke.colour, width = 174, height = 105, units="mm", dpi = 300, type="cairo-png")

#' ### Grey scale contoured plot time
#' If we just jump in and plot the existing data using the stat_contour() approach we immediately hit a problem.
#' It's a little difficult to explain, but easy enough to understand if you play around a bit. I'll try though.
#' some of the contoured values exist as closed polygons within the plot frame. These plot as we would expect.
#' Unfortunately, others, should extend out of the plot window, along some path which would eventually have closed.
#' As a result, ggplot2 sees them as single lines, and cant fill them with shading as required. So gpglot2 closes them
#' automatically by joiing their ends. This is pretty catastophic. You can see what happens for yourself by exchanging
#' `data.eke.sub.contour` for `data.eke.sub` in the plot code below.
#'
#' To avoid this we need to add extra, make-believe data just outside the plotting frame. we set this to a low, arbitrary value which
#' allows ggplot2 to close off the polygons which we are interested in.
#' What the arbitrary value is, is not that important, as long as it is lower than everything else.
#' I found this approach on [stackoverflow here](from http://stackoverflow.com/questions/28469829/how-to-fill-in-the-contour-fully-using-stat-contour)
#' and it basically looks at the data spread, divides it into n bins and sets an arbitrary value according to the lowest value
#' minus the bin width * 1.5.
#'
#' **Calculate a low arbitrary value**
bins<-50
bin.width<-(diff(range(na.omit(data.eke.sub$eke)))/bins)
arbitary.value=min(na.omit(data.eke.sub$eke))-bin.width*1.5

# Build some data frames representing 1 degree of extra data in each direction, with the arbitrary value as EKE
min <-sapply(data.eke.sub, min, na.rm = TRUE)
max <-sapply(data.eke.sub, max, na.rm = TRUE)
seq.lat <- seq(min['lat'], max['lat'], 0.25)
seq.lon <- seq(min['lon'], max['lon'], 0.25)

west.edge <- data.frame(lon = rep(min['lon'] - 1, length(seq.lat)), lat = seq.lat, eke = arbitary.value)
east.edge <- data.frame(lon = rep(max['lon'] + 1, length(seq.lat)), lat = seq.lat, eke = arbitary.value)
south.edge <- data.frame(lon = seq.lon, lat = rep(min['lat'] - 1, length(seq.lon)), eke = arbitary.value)
north.edge <- data.frame(lon = seq.lon, lat = rep(max['lat'] + 1, length(seq.lon)), eke = arbitary.value)

# rbind the new data to the existing data and tidy up
data.eke.sub.contour <- rbind(data.eke.sub, west.edge, east.edge, north.edge, south.edge)

rm(bins, bin.width, arbitary.value, min, max, seq.lat, seq.lon, west.edge, east.edge, north.edge, south.edge)

#' **Grey scale, contoured plot**
plot.eke.contour.grey <- ggplot() +
    stat_contour(data = data.eke.sub.contour, aes(x = lon, y = lat, z = eke, fill = ..level..),
                 geom = "polygon", breaks = c(0, 0.02, 0.04, 0.06, 0.08)) +
    geom_polygon(data = za, aes(x = long, y = lat, group = group),
                 colour="black", fill="white", alpha=1, size = 0.3) +
    geom_polygon(data=ef.df, aes(x=long, y=lat, group=group),
                 colour="black", fill="white", alpha=0, linetype="dashed", size = 0.3) +
    geom_contour(data=etopo.crop.df, aes(x=long,y=lat,z=z), breaks=c(-3000), colour="black", size = 0.3) +
    coord_map("mercator") +
    coord_fixed(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), expand = FALSE) +
    labs(x = 'Longitude (ºE)', y = 'Latitude (ºN)') +
    scale_fill_gradient(low = "gray25", high = "gray95",
                        space = "Lab",
                        guide = guide_colorbar(title="Eddy kinetic energy (cm²/s²)", title.position="right")) +
    geom_text(data=lbl, aes(x=x, y=y, label=txt), size=rel(3), colour="white") +
    thm
plot.eke.contour.grey

#' There we go! The contours look great. There aren't too many of them, they dont have distracting borders, the 3000m batymetry contour
#' looks nice and soenst distract.
# Saving the plot accoring to the publishers requirements
ggsave(filename = 'plot.eke.contour.grey.png', plot = plot.eke.contour.grey, width = 174, height = 105, units="mm", dpi = 300, type="cairo-png")

#' I hope this helps someone :)