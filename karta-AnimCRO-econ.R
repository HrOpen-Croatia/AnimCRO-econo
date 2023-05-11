# AnimCRO
# supporting material for economic chalenge, datathon 2023, Croatia
# Data from: on cows: number of owners per county from /HAPIH: Anual report 2021 Cattle breeding/, /link: https://www.hapih.hr/godisnja-izvjesca-cs-hapih-2021//
#            on Croatia geography: GADM, OD Cro portal /link/
#            on rural tourism in Croatia: /description/, /link/


library("raster")
library("geodata")
library("ggplot2")
library("tidyterra")

admin_cro1 <- gadm(country='HRV',level=1, path="C:/Users/salam/Dropbox (FAZ, Uni ZG)/_0_2023/05-svib2023/datathon-AnimCRO")

admin_cro1$c.surface <- c(2640, 2030, 1781,641,2813,3626,1748,1229,5353,729,4155,1823,3588,4468,4540,2984,1262,2024, 2454,3646,3060)

admin_cro1$head.keepers <- c(3644, 885,173,384,775,1759,3893,1337, 1879,469,1944,579,264,2163,
                  1082, 763,827,962,1.369,583,2959)

admin_cro1$istrian.keepers <- c(0,0,0,0,104,4,3,0,31,0,0,0,9,2,4,1,0,0,0,3,1)
admin_cro1$busa.keepers <- c(10,1,13,0,1,7,3,1,105,1,0,2,10,10,22,30,0,0,0,27,4)
admin_cro1$podol.keepers <- c(1,11,0,0,0,0,0,0,10,0,5,1,0,2,0,0,0,2,3,0,0)
admin_cro1$tourist <- c(7,6,34,1,28,9,2,14,3,1,8,10,8,13,14,20,2,1,10,6,17)

admin_cro1$istr.INT <- admin_cro1$istrian.keepers/admin_cro1$c.surface
admin_cro1$busa.INT <- admin_cro1$busa.keepers/admin_cro1$c.surface
admin_cro1$podol.INT <- admin_cro1$podol.keepers/admin_cro1$c.surface
admin_cro1$tour.INT <- admin_cro1$tourist/admin_cro1$c.surface
admin_cro1$head.INT <- admin_cro1$head.keepers/admin_cro1$c.surface

# Zero-Based Equal Interval Classification
# Similarly, stretching the colors between the minimum and maximum can also exaggerate 
# differences. Starting with a minimum of zero creates the map with the least contrast, 
# which accentuates a narrative
# Equal Interval Classification


# cattle in Croatia owners map - process intensity
breaks = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$head.INT, na.rm=T))

categories = as.numeric(cut(admin_cro1$head.INT, breaks))
categories = cut(admin_cro1$head.INT, breaks)

palette = colorRampPalette(c("peachpuff1", "black"))
ramp = palette(4)
colors = ramp[categories]

admin_cro1$categories <- categories

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories), color= "white", fill=colors) +
  coord_sf(crs = 4326)

cat.INT <- c("(0,0.557]", "(0.557,1.11]", "(1.11,1.67]", "(1.67,2.23]")


# cattle in Croatia owners map

breaks = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$head.keepers, na.rm=T))

categories = as.numeric(cut(admin_cro1$head.keepers, breaks))
categories = cut(admin_cro1$head.keepers, breaks)

palette = colorRampPalette(c("peachpuff1", "black"))
ramp = palette(4)
colors = ramp[categories]

admin_cro1$categories <- categories

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories), color= "white", fill=colors) +
  coord_sf(crs = 4326)

cat <- c("0-973", "974-1950", "1950-2920", "2920-3890")


# istrian cattle busa owners map - INTENSITY

breaks2 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$istr.INT, na.rm=T))

categories2 = as.numeric(cut(admin_cro1$istr.INT, breaks2))
categories2 = cut(admin_cro1$istr.INT, breaks2)

palette2 = colorRampPalette(c("peachpuff1", "tomato4"))
ramp2 = palette2(4)
colors2 = ramp2[categories2]

admin_cro1$categories2 <- categories2

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories2), color= "white", fill=colors2) +
  coord_sf(crs = 4326)

cat2.INT <- c("(0,0.00924]", "(0.00924,0.0185]", "(0.0185,0.0277]", "(0.0277,0.037]")



# istrian cattle busa owners map

breaks2 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$istrian.keepers, na.rm=T))

categories2 = as.numeric(cut(admin_cro1$istrian.keepers, breaks2))
categories2 = cut(admin_cro1$istrian.keepers, breaks2)

palette2 = colorRampPalette(c("peachpuff1", "tomato4"))
ramp2 = palette2(4)
colors2 = ramp2[categories2]

admin_cro1$categories2 <- categories2

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories2), color= "white", fill=colors2) +
  coord_sf(crs = 4326)

cat2 <- c("(0,26]", "(26,52]", "(52,78]", "(78,104]")


# busa local breed owners map INTENSITY

breaks3 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$busa.INT, na.rm=T))

categories3 = as.numeric(cut(admin_cro1$busa.INT, breaks3))
categories3 = cut(admin_cro1$busa.INT, breaks3)

palette3 = colorRampPalette(c("peachpuff1", "lightpink4"))
ramp3 = palette3(4)
colors3 = ramp3[categories3]

admin_cro1$categories3 <- categories3

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories3), color= "white", fill=colors3) +
  coord_sf(crs = 4326)

cat3.INT <- c("(0,0.0049]", "(0.0049,0.00981]", "(0.00981,0.0147]", "(0.0147,0.0196]")


# busa local breed owners map

breaks3 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$busa.keepers, na.rm=T))

categories3 = as.numeric(cut(admin_cro1$busa.keepers, breaks3))
categories3 = cut(admin_cro1$busa.keepers, breaks3)

palette3 = colorRampPalette(c("peachpuff1", "lightpink4"))
ramp3 = palette3(4)
colors3 = ramp3[categories3]

admin_cro1$categories3 <- categories3

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories3), color= "white", fill=colors3) +
  coord_sf(crs = 4326)

cat3 <- c("(0,26.2]" ,"(26.2,52.5]", "(52.5,78.8]" ,"(78.8,105]")


# podolac local breed owners map - INTENSITY
breaks4 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$podol.INT, na.rm=T))

categories4 = as.numeric(cut(admin_cro1$podol.INT, breaks4))
categories4 = cut(admin_cro1$podol.INT, breaks4)

palette4 = colorRampPalette(c("peachpuff1", "goldenrod4"))
ramp4 = palette4(4)
colors4 = ramp4[categories4]

admin_cro1$categories4 <- categories4

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories4), color= "white", fill=colors4) +
  coord_sf(crs = 4326)

cat4.INT <-  c("(0,0.00135]", "(0.00135,0.00271]", "(0.00271,0.00406] ","(0.00406,0.00542]")


# podolac local breed owners map
breaks4 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$podol.keepers, na.rm=T))

categories4 = as.numeric(cut(admin_cro1$podol.keepers, breaks4))
categories4 = cut(admin_cro1$podol.keepers, breaks4)

palette4 = colorRampPalette(c("peachpuff1", "goldenrod4"))
ramp4 = palette4(4)
colors4 = ramp4[categories4]

admin_cro1$categories4 <- categories4

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories4), color= "white", fill=colors4) +
  coord_sf(crs = 4326)

cat4 <-  c("(0,2.75]", "(2.75,5.5]", "(5.5,8.25]", "(8.25,11]")

# rural tourism map INT

breaks5 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$tour.INT, na.rm=T))

categories5 = as.numeric(cut(admin_cro1$tour.INT, breaks5))
categories5 = cut(admin_cro1$tour.INT, breaks5)

palette5 = colorRampPalette(c("gray80", "darkseagreen4"))
ramp5 = palette5(4)
colors5 = ramp5[categories5]

admin_cro1$categories5 <- categories5

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories5), color= "white", fill=colors5) +
  coord_sf(crs = 4326)

catTOURint <- c("(0,0.00477]", "(0.00477,0.00955]", "(0.00955,0.0143]", "(0.0143,0.0191]")

# rural tourism map

breaks5 = qunif(seq(0, 1, 0.25), min = 0, max = max(admin_cro1$tourist, na.rm=T))

categories5 = as.numeric(cut(admin_cro1$tourist, breaks5))
categories5 = cut(admin_cro1$tourist, breaks5)

palette5 = colorRampPalette(c("gray80", "darkseagreen4"))
ramp5 = palette5(4)
colors5 = ramp5[categories5]

admin_cro1$categories5 <- categories5

ggplot(admin_cro1) +
  geom_spatvector(aes(fill = categories5), color= "white", fill=colors5) +
  coord_sf(crs = 4326)

cat5 <- c("(0,8.5]", "(8.5,17]", "(17,25.5]", "(25.5,34]")



