## Libraries
#library(sp)
library(rgdal) #
library(xlsx) #
library(plyr)#
library(stringr)#
library(dplyr) #
setwd("D:/My Folders/R/2016/blog/20160714_election_effort")

# Shape Files
## Districts
za.districts <- readOGR("Districts", layer="DistrictMunicipalities2011")
za.district.list <- unique(za.districts@data$DISTRICT)
rm(za.districts)

# Electoral candidate data
## 2011 candidate data
### Proportional Representative candidates
pr.cand.2011 <- read.xlsx2("data/2011-pr-candidate-lists.xls", 1, startRow = 3)

# A few minor edits to party names
pr.cand.2011$Party <- str_to_title(str_replace_all(pr.cand.2011$Party, "  ", " "))
pr.cand.2011$Party[pr.cand.2011$Party == "Democratic Alliance/Demokratiese Alliansie"] <- "Democratic Alliance"
pr.cand.2011$Party[pr.cand.2011$Party == "Independent Ratepayers Association Of Sa"] <- "Independent Ratepayers Association Of SA"
pr.cand.2011$Party[pr.cand.2011$Party == "South African Maintanance And Estate Beneficiaries Associati"] <- "South African Maintanance And Estate Beneficiaries Association"
unique(pr.cand.2011$Party)


## 2016 candidate data
### Ward & Proportional Representative candidates
ward.pr.cand.2016 <- read.csv("data/Electoral_Candidates_2016.csv")
# A few minor edits to party names
ward.pr.cand.2016$Party <- str_to_title(str_replace_all(ward.pr.cand.2016$Party, "  ", " "))
ward.pr.cand.2016$Party[ward.pr.cand.2016$Party == "Independent Ratepayers Association Of Sa"] <- "Independent Ratepayers Association Of SA"
ward.pr.cand.2016$Party[ward.pr.cand.2016$Party == "South African Maintanance And Estate Beneficiaries Associati"] <- "South African Maintanance And Estate Beneficiaries Association"
unique(ward.pr.cand.2016$Party)

# fix strange district name errors
err_index <- grep("\f", ward.pr.cand.2016$Municipality)
ward.pr.cand.2016$Municipality[err_index] <- str_replace(ward.pr.cand.2016$Municipality[err_index], "\f", "")
ward.pr.cand.2016 <- droplevels(ward.pr.cand.2016)

#### Split the data into ward and pr data
# change variable names so they match later
pr.cand.2016 <- subset(ward.pr.cand.2016, ward.pr.cand.2016$PR.List.OrderNo...Ward.No < 1000)
names(pr.cand.2016)[4] <- "list.order.no"

#### Process ward and PR data
processor <- function(df){
    # get name for col names - must be at beginning - dont know why?!?
    data.name <- (deparse(substitute(df)))
    # split the Municipality names into codes and names
    dist.split <- str_split_fixed(df$Municipality, " - ", 2)
    df$dist.code <- dist.split[,1]
    df$dist.name <- dist.split[,2]
    # keeps only the district values. Main centers and DC areas.
    # data set includes candidates for smaller areas too.
    df <- df[df$dist.code %in% za.district.list, ]
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

pr.cand.2011.long <- processor(pr.cand.2011)
pr.cand.2016.long <- processor(pr.cand.2016)

# Check it out a bit
head(pr.cand.2011.long)
head(pr.cand.2016.long)

# Join data, dropping parties that no longer exist
pr.cand <- join(pr.cand.2011.long, pr.cand.2016.long, by = c("dist.code", "Party"), type = "right")

# replace NAs with zero for parties new in 2016
pr.cand[is.na(pr.cand)] <- 0

# calculate relative and absolute change
pr.cand$rel <- pr.cand$pr.cand.2016.prop - pr.cand$pr.cand.2011.prop
pr.cand$abs <- pr.cand$pr.cand.2016.num - pr.cand$pr.cand.2011.num

###########################
# Parties per district
names(pr.cand)
head(pr.cand)
grouped <- group_by(pr.cand, dist.code, Party)
head(grouped)
grouped$counter <- 1
d3_party_district <- summarise(grouped, tot = sum(counter))
head(d3_party_district)



###########################
# generate data for d3 map
# full data list
names(pr.cand)
d3_data_all <- pr.cand[, c(1, 2, 9, 10)]
names(d3_data_all) <- c("dist_code", "party", "Relative Change", "Absolute Change")
write.csv(d3_data_all, file="data_d3/d3_data_all.csv", row.names = FALSE, quote = FALSE)

# Party data list, ordered by total number or district PR candidates
# group the data by Party
grouped <- group_by(pr.cand, Party)
head(grouped)
# Summarise by summing the total candidates per party, excluding zeros and sorting
d3_party_list <- summarise(grouped, tot = sum(pr.cand.2016.num))
d3_party_list <- d3_party_list[d3_party_list$tot > 0, ]
d3_party_list <- arrange(d3_party_list, -tot)
head(d3_party_list)
# now we only need the party list
d3_party_list <- data.frame(party = d3_party_list$Party)

write.csv(d3_party_list, file="data_d3/d3_party_list.csv", row.names = FALSE, quote = FALSE)

