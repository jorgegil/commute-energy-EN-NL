# English cars versus Dutch bikes energy


# data manipulation functions, including joins
library("dplyr")
library("memisc")
# for plotting data
library("ggplot2")
# for plotting good quality images
library("Cairo")
# for loading shape files
library("rgdal")
# restructure data
library("reshape")
# for printing tables
library("gridExtra")


##
#### Data setup: ####
##
# import OViN data from dat SPSS file
ovin_data_raw <- read.delim(paste0(getwd(),"/data/OViN2012_Databestand_revisie.dat"))
ovin_data <- ovin_data_raw[,c(2,12,15,58,71:75,77,79:82,84:87,89,92,93,99,131,132,134,135,138,139,144,151:153)]
colnames(ovin_data) <- c("person_id","home_municip","home_prov","weekday","journey","journey_id","journey_nr","return","nr_legs","purpose","o_pcode","o_pcode_int","o_municip",
                         "o_prov","d_pcode","d_pcode_int","d_municip","d_prov","distance_journey","mode_main","mode_main_class",
                         "duration_journey","leg","leg_id","leg_nr","distance_leg","mode_leg","mode_leg_class","duration_leg","factorh","factorp","factorj")
# write.csv(ovin_data,paste0(getwd(),"/data/ovin2012_data.csv"))
ovin_select <- subset(ovin_data, !is.na("home_municip") & !is.na("home_prov") & !is.na("journey_id") & ovin_data$purpose==1)
ovin_municipalities <- read.csv2(paste0(getwd(),"/data/OViN2012_Codeboek_revisie_Wogem.csv"),
                                 header = TRUE, skip=1)
ovin_municipalities <- ovin_municipalities[,c(3,4)]
colnames(ovin_municipalities) <- c('code','name')

######
# import population data for provinces
cbs_data_province <- read.csv2(paste0(getwd(),"/data/Population_provinces.csv"))
cbs_data_province <- subset(cbs_data_province, cbs_data_province$Perioden==2012)[,1:3]
cbs_data_province$admin <- NA
cbs_data_province[grep(" (PV)",cbs_data_province$Regios,fixed=TRUE),]$admin <- as.character("PV")
cbs_data_province[grep(" (LD)",cbs_data_province$Regios,fixed=TRUE),]$admin <- as.character("LD")
cbs_data_province$Regios <- gsub(" (PV)","",cbs_data_province$Regios,fixed=TRUE)
cbs_data_province$Regios <- gsub(" (LD)","",cbs_data_province$Regios,fixed=TRUE)

# import province data (natural earth)
geo_province <- readOGR(dsn="data",layer="provinces")
# include only NL and exclude overseas provinces
geo_province <- geo_province[geo_province$gadm_level==1 & geo_province$iso_a2=="NL",]
# calculate area (the column has 0)
geo_province$area_sqkm <- sapply(slot(geo_province, "polygons"),slot, "area")
# join population data
cbs_data_province <- rename(cbs_data_province, c(Regios = "woe_name",admin="type"))
cbs_data_province$woe_name <- as.factor(cbs_data_province$woe_name)
geo_province@data <- left_join(geo_province@data, cbs_data_province,by=c("woe_name"))
geo_province$id <- row.names(geo_province)
# look into data
head(geo_province@data,n=2)
# make basic plot
plot(geo_province)


######
# import municipality geo data (CBS)
geo_municipality <- readOGR(dsn="data",layer="gem_2012_v2")
geo_municipality$area_sqm <- sapply(slot(geo_municipality, "polygons"),slot, "area")
## this is terribly slow. must be done in QGIS
# plot(geo_municipality)


######
# prepare grouping vectors
modes <- c("Total","Car","Car passenger","Rail","Bus/Tram/Metro","Moped","Bicycle","Walk","Other")
Ef <- c(0,2.1,2.1,1.65,0.91,1.73,0.06,0.16,0)  # in MJ/km, source Banister
provinces <- as.vector(cbs_data_province[5:16,1])
ovin_modes <- c(1:8)
ovin_provinces <- c(1:12)
ovin_municip <- sort(unique(ovin_select$home_municip))


##
##### Figure 2: ####
##
# modal split for commuter trips (national, province, municipal)
# select data for journeys of type 1 (work)
mob_data <- ovin_select[which(ovin_select$journey==1),]

##
# national
national_modal_split <- data.frame( "Region"="Netherlands","Total"=sum(mob_data$factorj))
# get total per mode
national_modal_split[,3:10] <- tapply(mob_data$factorj, mob_data$mode_main_class, sum)
colnames(national_modal_split)[2:10] <- modes
# calculate share of overall total
national_modal_split[,3:10] <- national_modal_split[,3:10]/national_modal_split$Total
# plot the bar chart
data_long <- melt(national_modal_split,id.vars=c("Region"))
data_long$variable <- as.character(data_long$variable)
data_long <- data_long[order(data_long$variable),]
plot <- ggplot(data_long[c(1:4,7,9),]) +
        geom_bar(aes(x=variable, y=value, fill=variable), stat="identity") +
        labs(x="mode", y="% of commuter trips", title="Netherlands", fill="mode")
ggsave("images/modal_split_national.png", plot, type="cairo-png")

##
# province
province_modal_split <- data.frame("Province" = ovin_provinces)
# calculate the overal total journeys
province_modal_split$Total <- aggregate(factorj~home_prov,mob_data,FUN=sum)[,2]
# using aggregate, reshape and merge
sums <- aggregate(factorj~home_prov+mode_main_class,mob_data,FUN=sum)
province_modal_split <- merge(province_modal_split,cast(sums,home_prov~mode_main_class,value="factorj"),by.x="Province", by.y="home_prov")
province_modal_split[,3:10] <- province_modal_split[,3:10]/province_modal_split$Total
colnames(province_modal_split)[2:10] <- modes
province_modal_split$Province <- provinces
# plot a bar charts array
data_long <- melt(province_modal_split[,c(1,3:6,8,9)],id.vars=c("Province"))
data_long$variable <- as.character(data_long$variable)
data_long <- data_long[order(data_long$variable),]
plot <- ggplot(data_long) +
    geom_bar(aes(x=variable, y=value, fill=variable), stat="identity") +
    labs(x="mode", y="% of commuter trips", fill="mode") +
    theme(axis.text.x = element_blank()) +
    facet_wrap(~ Province)
ggsave("images/modal_split_province.png", plot, type="cairo-png")
# print a nicely formatted table
pdf("images/data_modal_split_province.pdf", height=4, width=9)
grid.table(province_modal_split[,c(1,3:6,8,9)])
dev.off()
# save a csv with the results
write.csv(province_modal_split[,c(1,3:6,8,9)], "results/data_modal_split_province.csv", row.names=FALSE, na="")

##
# municipal modal split
municipal_modal_split <- data.frame("Municipality"=ovin_municip)
# calculate overall total journeys
municipal_modal_split$Total <- aggregate(factorj~home_municip,mob_data,FUN=sum)[,2]
# using aggregate, reshape and merge
# calculate journeys per mode
sums <- aggregate(factorj~home_municip+mode_main_class,mob_data,FUN=sum)
# join with municipalities
municipal_modal_split <- merge(municipal_modal_split,cast(sums,home_municip~mode_main_class,value="factorj",add.missing=TRUE,fill=0),by.x="Municipality", by.y="home_municip")
# calculate mode share
municipal_modal_split[,3:10] <-  municipal_modal_split[,3:10]/municipal_modal_split$Total
colnames(municipal_modal_split)[2:10] <- modes
# add municipality code
municipal_modal_split$gm_code <- sprintf("GM%04d",municipal_modal_split$Municipality)
# save results for later
write.csv(municipal_modal_split[,c(1,3:6,8,9,11)], "results/data_modal_split_municipal.csv", row.names=FALSE, na="")


##
#### Figure 3: ####
##
# total distance and average distance for commuter trips per mode (national, province, municipal)

##
# national data
# total distance
national_total_distance <- data.frame( "Region"="Netherlands","Total"=sum(mob_data$factorj*mob_data$distance_journey/10))
national_total_distance[,3:10] <- tapply((mob_data$factorj*mob_data$distance_journey/10), mob_data$mode_main_class, sum)
national_total_distance[,3:10] <- national_total_distance[,3:10]/national_total_distance$Total
colnames(national_total_distance)[2:10] <- modes
# average distance
national_average_distance <- data.frame( "Region"="Netherlands","Total"=(sum(mob_data$factorj*mob_data$distance_journey/10)/sum(mob_data$factorj)))
national_average_distance[,3:10] <- tapply((mob_data$factorj*mob_data$distance_journey/10), mob_data$mode_main_class, sum)/tapply(mob_data$factorj, mob_data$mode_main_class, sum)
colnames(national_average_distance)[2:10] <- modes
# plot the bar chart
data_long <- melt(national_average_distance,id.vars=c("Region"))
data_long$variable <- as.character(data_long$variable)
data_long <- data_long[order(data_long$variable),]
plot <- ggplot(data_long[c(1:4,7,9),]) +
    geom_bar(aes(x=variable, y=value, fill=variable), stat="identity") +
    labs(x="mode", y="Distance of commute (km)", title="Netherlands", fill="mode")
ggsave("images/average_distance_national.png", plot, type="cairo-png")

##
# province
##
# total distance
province_total_distance <- data.frame("Province"=ovin_provinces)
province_total_distance$Total <- aggregate((factorj*distance_journey/10)~home_prov,mob_data,FUN=sum)[,2]
# calculate distance per mode, using the journey weighting factor factorj * the distance
# distance is recorded in 100m steps instead of 1km, hence 20 = 2
sums <- aggregate((factorj*distance_journey/10)~home_prov+mode_main_class,mob_data,FUN=sum)
colnames(sums)[3] <- "total"
# join the sums with the overall total
province_total_distance <- merge(province_total_distance,cast(sums,home_prov~mode_main_class,add.missing=TRUE,fill=0,value="total"),by.x="Province", by.y="home_prov")
# calculate the distance share per mode
province_total_distance[,3:10] <-  province_total_distance[,3:10]/province_total_distance$Total
colnames(province_total_distance)[2:10] <- modes
##
# average distance
province_average_distance <- data.frame("Province"=ovin_provinces)
province_average_distance$Total <- aggregate((factorj*distance_journey/10)~home_prov,mob_data,FUN=sum)[,2]/aggregate(factorj~home_prov,mob_data,FUN=sum)[,2]
sums[,3] <- sums[,3]/aggregate(factorj~home_prov+mode_main_class,mob_data,FUN=sum)[,3]
colnames(sums)[3] <- "average"
province_average_distance <- merge(province_average_distance,cast(sums,home_prov~mode_main_class,add.missing=TRUE,fill=0,value="average"),by.x="Province", by.y="home_prov")
colnames(province_average_distance)[2:10] <- modes
province_average_distance$Province <- provinces
# plot a bar charts array
data_long <- melt(province_average_distance[,c(1,3:6,8,9)],id.vars=c("Province"))
data_long$variable <- as.character(data_long$variable)
data_long <- data_long[order(data_long$variable),]
plot <- ggplot(data_long) +
    geom_bar(aes(x=variable, y=value, fill=variable), stat="identity") +
    labs(x="mode", y="Distance of commute (km)", fill="mode") +
    theme(axis.text.x = element_blank()) +
    facet_wrap(~ Province)
ggsave("images/average_distance_province.png", plot, type="cairo-png")
# print a table
pdf("images/data_average_distance_province.pdf", height=4, width=9)
grid.table(province_average_distance[,c(1,3:6,8,9)])
dev.off()
write.csv(province_average_distance[,c(1,3:6,8,9)], "results/data_average_distance_province.csv", row.names=FALSE, na="")

##
# municipal
##
# total distance
municipal_total_distance <- data.frame("Municipality"=ovin_municip)
municipal_total_distance$Total <- aggregate((factorj*distance_journey/10)~home_municip,mob_data,FUN=sum)[,2]
sums <- aggregate((factorj*distance_journey/10)~home_municip+mode_main_class,mob_data,FUN=sum)
colnames(sums)[3] <- "total"
municipal_total_distance <- merge(municipal_total_distance,cast(sums,home_municip~mode_main_class,add.missing=TRUE,fill=0,value="total"),by.x="Municipality", by.y="home_municip")
municipal_total_distance[,3:10] <-  municipal_total_distance[,3:10]/municipal_total_distance$Total
colnames(municipal_total_distance)[2:10] <- modes
##
# average distance
municipal_average_distance <- data.frame("Municipality"=ovin_municip)
municipal_average_distance$Total <- aggregate((factorj*distance_journey/10)~home_municip,mob_data,FUN=sum)[,2]/aggregate(factorj~home_municip,mob_data,FUN=sum)[,2]
sums[,3] <- sums[,3]/aggregate(factorj~home_municip+mode_main_class,mob_data,FUN=sum)[,3]
colnames(sums)[3] <- "average"
municipal_average_distance <- merge(municipal_average_distance,cast(sums,home_municip~mode_main_class,add.missing=TRUE,fill=0,value="average"),by.x="Municipality", by.y="home_municip")
colnames(municipal_average_distance)[2:10] <- modes
municipal_average_distance$gm_code <- sprintf("GM%04d",municipal_average_distance$Municipality)
# save results
write.csv(municipal_average_distance[,c(1,3:6,8,9,11)], "results/data_average_distance_municipal.csv", row.names=FALSE, na="")


##
#### Figure 1: ####
##
# calculate energy per commuter trip
# Taking as reference average mode expenditure
# from the paper, for each mode: the product of mode efficiency (Ef), average route distance (dR) and modal split (p)
# Etrp = sum(pm × Efm × dRm)

##
# national
national_Etrp <- data.frame( "Region"="Netherlands","Total"=0)
national_Etrp[,3:10] <- Ef[2:9] * national_average_distance[,3:10] * national_modal_split[,3:10]
national_Etrp[,2] <- sum(national_Etrp[,3:10])

##
# province
province_Etrp <- data.frame("Province"=ovin_provinces,"Total"=0)
# calculate energy per mode
for (j in ovin_modes){
    province_Etrp[,j+2] <- round(Ef[j+1] * province_average_distance[,j+2] * province_modal_split[,j+2],6)
}    
for(i in c(1:length(ovin_provinces))){
    province_Etrp[i,2] <- sum(province_Etrp[i,3:10])
}
colnames(province_Etrp)[2:10] <- modes
province_Etrp$woe_name <- cbs_data_province[which(cbs_data_province$type=='PV'),1]
province_Etrp$id <- left_join(province_Etrp, geo_province@data, by="woe_name")$id
# plot map
geo_province_f <- fortify(geo_province)
geo_province_f <- left_join(geo_province_f, province_Etrp, by="id")
#head(geo_province_f,n=2)
plot <- ggplot() +
    geom_polygon(data=geo_province_f, 
                 aes(x=long, y=lat, group=group, fill=Total), 
                 colour="grey", alpha=1) +
    labs(x="", y="", title="Province energy per commuter trip") + #labels
    theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
          axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
          plot.title = element_text(lineheight=.8, face="bold", vjust=1)) + # make title bold and add space
    scale_fill_gradient(name="Etrp (MJ/trip)", low="green", high="red") + # change color scale
    coord_equal(ratio=1)
ggsave(plot, file = "images/map_energy_province.png", width = 6, height = 4.5, type="cairo-png")
# print a table
pdf("images/data_energy_province.pdf", height=4, width=9)
grid.table(province_Etrp[,c(11,2,8,6,3:5,9)])
dev.off()
write.csv(province_Etrp[,c(11,2,8,6,3:5,9)], "results/data_energy_province.csv", row.names=FALSE, na="")

##
# municipal
municipal_Etrp <- data.frame("Municipality"=ovin_municip,"Total"=0)
for (j in ovin_modes){
    municipal_Etrp[,j+2] <- Ef[j+1] * municipal_average_distance[,j+2] * municipal_modal_split[,j+2]
}
for(i in c(1:length(ovin_municip))){
    municipal_Etrp[i,2] <- sum(municipal_Etrp[i,3:10])
}
colnames(municipal_Etrp)[2:10] <- modes
municipal_Etrp$name <- ovin_municipalities[match(municipal_Etrp$Municipality,ovin_municipalities$code),2]
# save results
write.csv(municipal_Etrp[,c(1,8,6,3:5,9,11)], "results/data_energy_municipal.csv", row.names=FALSE, na="")



##
#### Figure 4: ####
##
# population density against average trip energy
# province
province_density <- geo_province@data[,c(9,27,61)]
province_density$area_sqkm <- province_density$area_sqkm/1000000
province_density$density <- province_density$Totale.bevolking/province_density$area_sqkm
province_density$energy <- province_Etrp[match(province_density$name,province_Etrp$woe_name),2]
# based on journeys
# plot scatter
corr <- cor(province_density$density, province_density$energy)
# -0.49
plot <- ggplot(province_density,aes(density, energy)) +
    labs(x="Population density (ppl/sqkm)", y="Energy costs of commuting (MJ/trip)", 
         title="Population density against commuter energy use in Provinces") +
        geom_point()
ggsave(plot, file = "images/energy_density_province.png", width = 6, height = 4.5, type="cairo-png")
# print a table
pdf("images/data_density_province.pdf", height=4, width=9)
grid.table(province_density)
dev.off()
write.csv(province_density, "results/data_density_province.csv", row.names=FALSE, na="")

# municipal
municipality_density <- geo_municipality@data[,c(1,2,6,153,156)]
colnames(municipality_density) <- c('id','name','population','area_hect','area_sqkm')
municipality_density <- municipality_density[which(!is.na(municipality_density$name)),]
municipality_density$area_sqkm <- municipality_density$area_sqkm/1000000
municipality_density$density <- municipality_density$population/municipality_density$area_sqkm
municipality_density$code <- as.integer(substr(municipality_density[,1],3,6))
municipality_density$energy <- municipal_Etrp[match(municipality_density$code,municipal_Etrp$Municipality),2]
# based on journeys
# plot scatter
corr <- cor(x=municipality_density$density, y=municipality_density$energy, use="pairwise.complete.obs")
# -0.096
plot <- ggplot(municipality_density,aes(density, energy)) +
    labs(x="Population density (ppl/sqkm)", y="Energy costs of commuting (MJ/trip)", 
         title="Population density against commuter energy use in Municipalities") +
        geom_point()
ggsave(plot, file = "images/energy_density_municipal.png", width = 6, height = 4.5, type="cairo-png")
# save results
write.csv(municipality_density, "results/data_density_municipal.csv", row.names=FALSE, na="")

