
## Functions

normaliser <- function(col){
    out <- (col - min(col))/(max(col) - min(col))
    return(out)
}

processor <- function(df){
    # get name for col names - must be at beginning - dont know why?!?
    data.name <- (deparse(substitute(df)))
    # df <- pr.cand.2011

    # split the Municipality names into codes and names
    dist.split <- str_split_fixed(df$Municipality, " - ", 2)
    df$dist.code <- dist.split[,1]
    df$dist.name <- dist.split[,2]

    if (data.name == "ward.cand.2011" | data.name == "ward.cand.2016"  ){
        df <- df[df$dist.code %in% za.ward.list, ]
    } else if (data.name == "pr.cand.2011" | data.name == "pr.cand.2016"){
        # keeps only the district values. Main centers and DC areas. Smaller areas are excluded...
        df <- df[df$dist.code %in% za.district.list, ]
    }
    # length(unique(df$dist.code))

    # split data by district and party, counting how many candidates per party per district
    df.long <- ddply(df, c("dist.code", "Party"), function(df) nrow(df))

    # temp naming - to keep track not NB
    names(df.long)[3] <- "num"

    # split new data by district, summing tot candidates from all parties (in district)
    df.tot <- ddply(df.long, "dist.code", function(df) sum(df$num))

    # join tot numbers by district to main data
    df.long <- join(df.long, df.tot, by = "dist.code")

    # temp naming - to keep track not NB
    names(df.long)[4] <- "tot"

    # calculate the proportion of each party to total, for each district
    df.long$prop <- df.long$num / df.long$tot

    # quick srting, not v NB
    df.long <- arrange(df.long, dist.code, -prop)

    # renaming based on variable name
    names(df.long)[c(3:5)] <- c(paste0(data.name, ".num"), paste0(data.name, ".tot"), paste0(data.name, ".prop"))

    return(df.long)
}

## Libraries
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(plyr)
library(stringr)
library(RColorBrewer)
setwd("D:/My Folders/R/2016/blog/20160714_election_effort")

# Shape Files
## Provinces
za.provinces <- readOGR("Province", layer="Province_New_SANeighbours")
za.provinces@data$id <- rownames(za.provinces@data)
za.provinces.df <- fortify(za.provinces)
za.provinces.df <- join(za.provinces.df, za.provinces@data, by="id")
head(za.provinces.df)
za.provinces.list <- unique(za.provinces.df$DISTRICT)

## Districts
za.districts <- readOGR("Districts", layer="DistrictMunicipalities2011")
za.districts@data$id <- rownames(za.districts@data)
za.districts.df <- fortify(za.districts)
za.districts.df <- join(za.districts.df, za.districts@data, by="id")
head(za.districts.df)
za.district.list <- unique(za.districts.df$DISTRICT)

## Wards
za.wards <- readOGR("Wards", layer="Wards2011")
za.wards@data$id <- rownames(za.wards@data)
za.wards.df <- fortify(za.wards)
za.wards.df <- join(za.wards.df, za.wards@data, by="id")
head(za.wards.df)
za.ward.list <- unique(za.wards.df$CAT_B)

## quick plot to check
plotting <- za.wards.df
plotting <- za.districts.df

plot.test <- ggplot() +
    geom_polygon(data = plotting, aes(x = long, y = lat, group = group),
                 colour="black", fill="white", alpha=1, size = 0.1) +
    coord_map("mercator")
plot.test

# Electoral candidate data
## 2011 candidate data
### Ward candidates
library(xlsx)
ward.cand.2011 <- read.xlsx2("data/2011-ward-candidates-list.xls", 1, startRow = 3)
ward.cand.2011$Party <- str_to_title(str_replace_all(ward.cand.2011$Party, "  ", " "))
ward.cand.2011$Party[ward.cand.2011$Party == "Democratic Alliance/Demokratiese Alliansie"] <- "Democratic Alliance"
# 234 'Municipalities' AKA wards

### Provincial Representative candidates
pr.cand.2011 <- read.xlsx2("data/2011-pr-candidate-lists.xls", 1, startRow = 3)
pr.cand.2011$Party <- str_to_title(str_replace_all(pr.cand.2011$Party, "  ", " "))
pr.cand.2011$Party[pr.cand.2011$Party == "Democratic Alliance/Demokratiese Alliansie"] <- "Democratic Alliance"

#### Process ward and PR data
ward.cand.2011.long <- processor(ward.cand.2011)
pr.cand.2011.long <- processor(pr.cand.2011)

## 2016 candidate data
### Ward & PR candidates
ward.pr.cand.2016 <- read.csv("data/Electoral_Candidates_2016.csv")
ward.pr.cand.2016$Party <- str_to_title(str_replace_all(ward.pr.cand.2016$Party, "  ", " "))

#### Split the data into ward and pr data
summary(ward.pr.cand.2016$PR.List.OrderNo...Ward.No)

ward.cand.2016 <- subset(ward.pr.cand.2016, ward.pr.cand.2016$PR.List.OrderNo...Ward.No > 1000)
names(ward.cand.2016)[4] <- "ward.no"

pr.cand.2016 <- subset(ward.pr.cand.2016, ward.pr.cand.2016$PR.List.OrderNo...Ward.No < 1000)
names(pr.cand.2016)[4] <- "list.order.no"

#### Process ward and PR data
ward.cand.2016.long <- processor(ward.cand.2016)
pr.cand.2016.long <- processor(pr.cand.2016)

# Check it out a bit
head(ward.cand.2011.long)
head(ward.cand.2016.long)


head(pr.cand.2011.long)
head(pr.cand.2016.long)

# Join data
ward.cand <- join(ward.cand.2011.long, ward.cand.2016.long, by = c("dist.code", "Party"), type = "full")
pr.cand <- join(pr.cand.2011.long, pr.cand.2016.long, by = c("dist.code", "Party"), type = "full")

ward.cand[is.na(ward.cand)] <- 0
pr.cand[is.na(pr.cand)] <- 0

ward.cand$rel <- ward.cand$ward.cand.2016.prop - ward.cand$ward.cand.2011.prop
pr.cand$rel <- pr.cand$pr.cand.2016.prop - pr.cand$pr.cand.2011.prop

ward.cand$abs <- ward.cand$ward.cand.2016.num - ward.cand$ward.cand.2011.num
pr.cand$abs <- pr.cand$pr.cand.2016.num - pr.cand$pr.cand.2011.num

plotter <- function(data, party, metric) {
    data.name <- (deparse(substitute(data)))
    data <- subset(data, data$Party == party)

    if (data.name == "ward.cand") {
        df <- merge(za.wards.df, data, by.x = "CAT_B", by.y = "dist.code")
        title <- paste0(party, " ", "Ward Candidates.")
        # df <- arrange(df, WARD_ID, order)
    } else if (data.name == "pr.cand") {
        df <- merge(za.districts.df, data, by.x = "DISTRICT", by.y = "dist.code")
        title <- paste0(party, " ", "PR Candidates.")
    }
    df <- arrange(df, group, order)

    if (metric == "rel") {
        title <- paste0(title, "\nChange in proportional representation from 2011 to 2016")
    } else if (metric == "abs"){
        title <- paste0(title, "\nChange in absolute representation from 2011 to 2016")
    }

    plot.colour <- ggplot() +
        geom_polygon(data = df, aes_string(x = "long", y = "lat", group = "group", fill = metric),
                     colour = "transparent") +
        # scale_fill_gradientn(colors=brewer.pal(name="RdBu", n=6)) +
        # scale_fill_distiller(palette = "RdBu",  direction=2) +
        scale_fill_gradient2(low=brewer.pal(name="RdBu", n=3)[3], high=brewer.pal(name="RdBu", n=3)[1] ) +
        geom_polygon(data = df, aes_string(x = "long", y = "lat", group = "group"),
                     colour = "antiquewhite3", fill = "transparent", size = 0.25) +
        geom_polygon(data = za.provinces.df, aes_string(x = "long", y = "lat", group = "group"),
                     colour = "burlywood4", fill = "transparent", size = 1) +
        coord_map("mercator") +
        labs(title = title, x = "Latitude", y = "Longitude")

    return(plot.colour)
}

plotter(pr.cand, "African National Congress", "rel")

plotter(pr.cand, "Democratic Alliance", "rel")

plotter(pr.cand, "Inkatha Freedom Party", "rel")

plotter(pr.cand, "African National Congress", "abs")

plotter(pr.cand, "Democratic Alliance", "abs")

plotter(pr.cand, "Inkatha Freedom Party", "abs")

plotter(ward.cand, "African National Congress", "rel")

plotter(ward.cand, "Democratic Alliance", "rel")

plotter(ward.cand, "Inkatha Freedom Party", "rel")

plotter(ward.cand, "African National Congress", "abs")

plotter(ward.cand, "Democratic Alliance", "abs")

plotter(ward.cand, "Inkatha Freedom Party", "abs")


# Party list
plotting <- ward.cand
plotting <- pr.cand

parties <- sort(unique(plotting$Party))
parties

party <- "Inkatha Freedom Party"



# plotting testing

plotting <- "ward.cand"
plotting <- "pr.cand"

parties <- sort(unique(get(plotting)$Party))
parties

party <- "Inkatha Freedom Party"

plot.data <- subset(get(plotting), get(plotting)$Party == party)
head(plot.data)



if (plotting == "ward.cand") {
    plot.data.fin <- merge(za.wards.df, plot.data, by.x = "CAT_B", by.y = "dist.code")
    plot.data.fin <- arrange(plot.data.fin, WARD_ID, order)
} else if (plotting == "pr.cand") {
    plot.data.fin <- merge(za.districts.df, plot.data, by.x = "DISTRICT", by.y = "dist.code", all = T)
}

head(plot.data.fin)

# pr
head(za.districts.df)
length(unique(za.districts.df$DISTRICT))
length(unique(plot.data$dist.code))

length(unique(plot.data.fin$DISTRICT))

sort(unique(za.districts.df$DISTRICT)) == sort(unique(plot.data.fin$DISTRICT))


a <- plot.data.fin[complete.cases(plot.data.fin),]

head(plot.data.fin)
head(za.districts.df)

sum(plot.data.fin$long != za.districts.df$long)


plot.data.fin$DISTRICT[1:27000]
plot.data.fin$order[1:27000]
plot.data.fin <- arrange(plot.data.fin, group, order)





# wards
head(za.wards.df)
za.wards.df$long == plot.data.fin$long

nrow(za.wards.df)
nrow(plot.data.fin)

summary(za.wards.df$group)

sort(unique(za.wards.df$CAT_B)) == sort(unique(plot.data.fin$dist.code))


# plotting
min(plot.data$rel)
max(plot.data$rel)

plot.colour <- ggplot() +
    geom_polygon(data = plot.data.fin, aes(x = long, y = lat, group = group),
                 colour = "grey") +
    # scale_fill_gradientn(colors=brewer.pal(name="RdBu", n=6)) +
    scale_fill_gradient2(low=brewer.pal(name="RdBu", n=3)[3], high=brewer.pal(name="RdBu", n=3)[1] ) +
    # scale_fill_distiller(palette = "RdBu",  direction=2) +
    coord_map("mercator")
plot.colour

library(RColorBrewer)

# and with a diverging scale
ggplot(d, aes(x=x, y=y, fill=z)) + geom_raster() + scale_fill_distiller(palette="Spectral")

ggplot(d, aes(x=x, y=y, fill=z)) + geom_raster() + scale_fill_distiller(palette="Spectral", direction=1)
ggplot(d, aes(x=x, y=y, fill=z)) + geom_raster() + scale_fill_gradientn(colors=brewer.pal(name="Spectral", n=6))


## quick plot to check
plotting <- za.wards.df
plotting <- za.districts.df

plot.test <- ggplot() +
    geom_polygon(data = plotting, aes(x = long, y = lat, group = group),
                 colour="black", fill="white", alpha=1, size = 0.1) +
    coord_map("mercator")
plot.test


###########################

d3datamaker <- function(data, party) {

#     data <- pr.cand
#     head(data)
#
#     data <- arrange(data, pr.cand.2016.num)

    data.name <- (deparse(substitute(data)))

    if ( str_to_lower(party) != "all") {
        data <- subset(data, data$Party == party)
    }

    if (data.name == "ward.cand") {
        df <- merge(data, za.wards.df, by.x = "CAT_B", by.y = "dist.code")
    } else if (data.name == "pr.cand") {
        stemp <- unique(za.districts.df[, c(10,15)])
        df <- merge(data, stemp, by.x = "dist.code", by.y = "DISTRICT")
    }
    df <- df[,c(1,2,6,9,10)]
    names(df) <- c("dist_code","party","pr_cand_2016_num","relative_change","absolute_change")
    return(df)
}

library(dplyr)
library(reshape2)
head(pr.cand)

d3_data_all <- d3datamaker(pr.cand, "all")
names(d3_data_all)
head(d3_data_all)


grouped <- group_by(d3_data_all, party)
head(grouped)

d3_party_list <- summarise(grouped, tot = sum(pr_cand_2016_num))
d3_party_list <- d3_party_list[d3_party_list$tot > 0, ]
d3_party_list <- arrange(d3_party_list, -tot)
head(d3_party_list)

d3_party_list <- data.frame(party = d3_party_list)

d3_data_all <- d3_data_all[, c(1, 2, 4, 5)]

write.csv(d3_data_all, file="d3_data_all.csv", row.names = FALSE, quote = FALSE)
write.csv(d3_party_list, file="d3_party_list.csv", row.names = FALSE, quote = FALSE)
