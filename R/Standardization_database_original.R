####### standardization of trawl survey database


library(dplyr)
library(ggplot2)
library(maptools)
# install.packages('DATRAS',repos='http://www.rforge.net/',type='source')
library(DATRAS)
library(doBy)


####### Swedish historical data
####### load historical data

stand <- read.csv("New/Raw.csv", sep = ";")
stand_un <- read.csv("New/stand_un.csv", sep = ";")

##### create the index with date as well since there is a case in which the haul number is the same
stand_un$idx <- paste(stand_un$IDX_SSB, stand_un$datum, sep = ".")
stand$idx <- paste(stand$IDX_SSB, stand$datum, sep = ".")

##### remove pelagic trawl

stand <- stand[!stand$idx %in% stand_un$idx[stand_un$type == "pel"], ]

stand_un <- stand_un[!stand_un$type == "pel", ]


##### create a dataset for each species valid historic hauls


##### select hauls with wrong information
# ML: This refers to hauls with a total weight only. Add this indicator column to the new species I've added (whiting and dab)
# For now, I will just indicate if there's no lenght and count information
stand <- stand %>%
  mutate(
    DAB = ifelse(is.na(lŠngd) & is.na(antal), "no", ""),
    WHI = ifelse(is.na(lŠngd) & is.na(antal), "no", "")
  )

nocod <- stand %>%
  select(IDX_SSB, COD, FLO, HER, SPR, PLA, DAB, WHI) %>%
  filter(COD == "no")

noflo <- stand %>%
  select(IDX_SSB, COD, FLO, HER, SPR, PLA, DAB, WHI) %>%
  filter(FLO == "no")

noher <- stand %>%
  select(IDX_SSB, COD, FLO, HER, SPR, PLA, DAB, WHI) %>%
  filter(HER == "no")

nospr <- stand %>%
  select(IDX_SSB, COD, FLO, HER, SPR, PLA, DAB, WHI) %>%
  filter(SPR == "no")

nodab <- stand %>%
  select(IDX_SSB, COD, FLO, HER, SPR, PLA, DAB, WHI) %>%
  filter(DAB == "no")

nowhi <- stand %>%
  select(IDX_SSB, COD, FLO, HER, SPR, PLA, DAB, WHI) %>%
  filter(WHI == "no")


###### take just valid hauls ( for herring and sprat just the hauls with catch)

codhist <- stand %>%
  filter(art == "Torsk") %>%
  merge(x = ., stand_un, by = intersect(names(.), names(stand_un)), all = TRUE) %>% #### add 0 hauls
  mutate(COD = ifelse(is.na(COD), 0, COD)) %>%
  filter(!IDX_SSB %in% nocod$IDX_SSB) %>% ##### remove invalid hauls
  mutate(art = "Gadus morhua")
summary(codhist)

flohist <- stand %>%
  filter(art == "Skrubbskädda") %>%
  merge(x = ., stand_un, by = intersect(names(.), names(stand_un)), all = TRUE) %>%
  mutate(FLO = ifelse(is.na(FLO), 0, FLO)) %>%
  filter(!IDX_SSB %in% noflo$IDX_SSB) %>%
  mutate(art = "Platichthys flesus")
summary(flohist)

herhist <- stand %>%
  filter(art == "Sill" & !IDX_SSB %in% noher$IDX_SSB) %>%
  mutate(art = "Clupea harengus")
summary(herhist)

sprhist <- stand %>%
  filter(art == "Skarpsill" & !IDX_SSB %in% nospr$IDX_SSB) %>%
  mutate(art = "Sprattus sprattus")
summary(sprhist)

plahist <- stand %>%
  filter(art == "Rödspätta") %>%
  merge(x = ., stand_un, by = intersect(names(.), names(stand_un)), all = TRUE) %>%
  mutate(PLA = ifelse(is.na(PLA), 0, PLA)) %>%
  filter(!IDX_SSB %in% nopla$IDX_SSB) %>%
  mutate(art = "Pleuronectes platessa")
summary(plahist)

dabhist <- stand %>%
  filter(art == "Rödspätta") %>%
  merge(x = ., stand_un, by = intersect(names(.), names(stand_un)), all = TRUE) %>%
  mutate(PLA = ifelse(is.na(PLA), 0, PLA)) %>%
  filter(!IDX_SSB %in% nopla$IDX_SSB) %>%
  mutate(art = "Pleuronectes platessa")
summary(plahist)

plahist <- stand %>%
  filter(art == "Rödspätta") %>%
  merge(x = ., stand_un, by = intersect(names(.), names(stand_un)), all = TRUE) %>%
  mutate(PLA = ifelse(is.na(PLA), 0, PLA)) %>%
  filter(!IDX_SSB %in% nopla$IDX_SSB) %>%
  mutate(art = "Pleuronectes platessa")
summary(plahist)

######## change NAs in CPUEs with 0s

codhist$CPUEantal[is.na(codhist$CPUEantal)] <- 0
flohist$CPUEantal[is.na(flohist$CPUEantal)] <- 0
plahist$CPUEantal[is.na(plahist$CPUEantal)] <- 0
herhist$CPUEantal[is.na(herhist$CPUEantal)] <- 0
sprhist$CPUEantal[is.na(sprhist$CPUEantal)] <- 0


######### Fiskdata2

#### load data up to 2014
index <- read.csv("New/Fiskdata2/Research Östersjön 2.csv", sep = ";")
cod <- read.csv("New/Fiskdata2/trawlsurv_zincl_l_COD.csv", sep = ";")
flounder <- read.csv("New/Fiskdata2/trawlsurv_zincl_l_FLOUNDER.csv", sep = ";")
herring <- read.csv("New/Fiskdata2/trawlsurv_zincl_l_HERRING.csv", sep = ";")
sprat <- read.csv("New/Fiskdata2/trawlsurv_zincl_l_SPRAT.csv", sep = ";")
plaice <- read.csv("New/Fiskdata2/trawlsurv_zincl_l_PLAICE.csv", sep = ";")


#### load data for 2015-16
index1 <- read.csv("New/Fiskdata2/Research Östersjön 3.csv", sep = ";")
cod1 <- read.csv("New/Fiskdata2/trawl_surveys_zincl_(l)BITSCOD.csv", sep = ";")
flounder1 <- read.csv("New/Fiskdata2/trawl_surveys_zincl_(l)BITSFLOUNDER.csv", sep = ";")
herring1 <- read.csv("New/Fiskdata2/trawl_surveys_zincl_(l)BITSHERRING.csv", sep = ";")
sprat1 <- read.csv("New/Fiskdata2/trawl_surveys_zincl_(l)BITSSPRAT.csv", sep = ";")
plaice1 <- read.csv("New/Fiskdata2/trawl_surveys_zincl_(l)BITSPLAICE.csv", sep = ";")

#### make them equal

cod1$Subdivision <- factor(cod1$Subdivision)
flounder1$Subdivision <- factor(flounder1$Subdivision)
herring1$Subdivision <- factor(herring1$Subdivision)
sprat1$Subdivision <- factor(sprat1$Subdivision)
plaice1$Subdivision <- factor(plaice1$Subdivision)

###### merge them
index <- rbind(index[, -1], index1)
cod <- rbind(cod[, -32], cod1[, -1])
flounder <- rbind(flounder, flounder1[, -1])
herring <- rbind(herring, herring1[, -1])
sprat <- rbind(sprat, sprat1[, -1])
plaice <- rbind(plaice, plaice1[, -1])



summary(index)
summary(cod)
summary(flounder)
summary(herring)
summary(sprat)
summary(plaice)


###### change length in cm

cod$Lengthcl <- cod$Lengthcl / 10
flounder$Lengthcl <- flounder$Lengthcl / 10
herring$Lengthcl <- herring$Lengthcl / 10
sprat$Lengthcl <- sprat$Lengthcl / 10
plaice$Lengthcl <- plaice$Lengthcl / 10


####### Add species name

cod$species <- "cod"
flounder$species <- "flounder"
herring$species <- "herring"
sprat$species <- "sprat"
plaice$species <- "plaice"


#### remove SD21

cod <- cod[!cod$Subdivision == 21, ]
flounder <- flounder[!flounder$Subdivision == 21, ]
herring <- herring[!herring$Subdivision == 21, ]
sprat <- sprat[!sprat$Subdivision == 21, ]
plaice <- plaice[!plaice$Subdivision == 21, ]


###### create a new index with the date as well

index <- cbind(index, do.call("rbind", strsplit(as.character(index$STA_FROMDATETIME), " ")))
index$idx <- paste(index$index, index$"1", sep = ".")

cod$idx <- paste(cod$Index, cod$Date, sep = ".")
flounder$idx <- paste(flounder$Index, flounder$Date, sep = ".")
herring$idx <- paste(herring$Index, herring$Date, sep = ".")
sprat$idx <- paste(sprat$Index, sprat$Date, sep = ".")
plaice$idx <- paste(plaice$Index, plaice$Date, sep = ".")

index$idx <- factor(index$idx)
cod$idx <- factor(cod$idx)
flounder$idx <- factor(flounder$idx)
herring$idx <- factor(herring$idx)
sprat$idx <- factor(sprat$idx)
plaice$idx <- factor(plaice$idx)


##### merge

codT <- merge(index, cod, by = "idx", all.y = T)
flounderT <- merge(index, flounder, by = "idx", all.y = T)
herringT <- merge(index, herring, by = "idx", all.y = T)
spratT <- merge(index, sprat, by = "idx", all.y = T)
plaiceT <- merge(index, plaice, by = "idx", all.y = T)


##### select just OTB

codT <- codT[codT$REDSKENG == "OTB Otter trawl bottom", ]
flounderT <- flounderT[flounderT$REDSKENG == "OTB Otter trawl bottom", ]
herringT <- herringT[herringT$REDSKENG == "OTB Otter trawl bottom", ]
spratT <- spratT[spratT$REDSKENG == "OTB Otter trawl bottom", ]
plaiceT <- plaiceT[plaiceT$REDSKENG == "OTB Otter trawl bottom", ]

index <- index[index$REDSKENG == "OTB Otter trawl bottom", ]

##### remove invalid and calibration hauls

type <- c("I", "C")
type2 <- c("Invalid", "Calibration")


codT <- codT[!codT$Validity..haul. %in% type, ]
flounderT <- flounderT[!flounderT$Validity..haul. %in% type, ]
herringT <- herringT[!herringT$Validity..haul. %in% type, ]
spratT <- spratT[!spratT$Validity..haul. %in% type, ]
plaiceT <- plaiceT[!plaiceT$Validity..haul. %in% type, ]
index <- index[!index$STAVALCODE_DESCENG %in% type2, ]

##### remove hauls of tvl with sweep  25 and 50

wrongsweep <- index %>%
  filter(STA_SWEEPLENGTH <= 59 & GEARMOD_NAMEENG == "TV3 BOTTOM TRAWL FOR COD 930 MESH" & STAVALCODE_DESCENG == "Valid")

index <- index[!index$idx %in% wrongsweep$idx, ]
codT <- codT[!codT$idx %in% wrongsweep$idx, ]
flounderT <- flounderT[!flounderT$idx %in% wrongsweep$idx, ]
herringT <- herringT[!herringT$idx %in% wrongsweep$idx, ]
spratT <- spratT[!spratT$idx %in% wrongsweep$idx, ]
plaiceT <- plaiceT[!plaiceT$idx %in% wrongsweep$idx, ]

summary(codT)
summary(flounderT)
summary(herringT)
summary(spratT)
summary(plaiceT)

##### check that for all the species we have data for all the hauls

levels(factor(codT$idx))
levels(factor(flounderT$idx))
levels(factor(herringT$idx))
levels(factor(spratT$idx))
levels(factor(plaiceT$idx))

##### remove the hauls with no LFD from the index file

index <- index[index$idx %in% levels(factor(plaiceT$idx)), ]

###### remove hauls for which we know they have fished the species but we don't have LFD

codinvalid <- codT %>%
  filter(is.na(No..hour) & is.na(Lengthcl)) %>%
  filter(!(is.na(Tot.No..hour) & is.na(Kg.hour))) %>%
  filter(!is.na(Kg.hour))

codT <- codT[!codT$idx %in% codinvalid$idx, ]

flounderinvalid <- flounderT %>%
  filter(is.na(No..hour) & is.na(Lengthcl)) %>%
  filter(!(is.na(Tot.No..hour) & is.na(Kg.hour)))

flounderT <- flounderT[!flounderT$idx %in% flounderinvalid$idx, ]

herringinvalid <- herringT %>%
  filter(is.na(No..hour) & is.na(Lengthcl)) %>%
  filter(!(is.na(Tot.No..hour) & is.na(Kg.hour))) %>%
  filter(!is.na(Kg.hour))

herringT <- herringT[!herringT$idx %in% herringinvalid$idx, ]

spratinvalid <- spratT %>%
  filter(is.na(No..hour) & is.na(Lengthcl)) %>%
  filter(!(is.na(Tot.No..hour) & is.na(Kg.hour)))

spratT <- spratT[!spratT$idx %in% spratinvalid$idx, ]

plaiceinvalid <- plaiceT %>%
  filter(is.na(No..hour) & is.na(Lengthcl)) %>%
  filter(!(is.na(Tot.No..hour) & is.na(Kg.hour)))

plaiceT <- plaiceT[!plaiceT$idx %in% plaiceinvalid$idx, ]



######### change NA in No..hour to 0

codT$No..hour[is.na(codT$No..hour)] <- 0
flounderT$No..hour[is.na(flounderT$No..hour)] <- 0
herringT$No..hour[is.na(herringT$No..hour)] <- 0
spratT$No..hour[is.na(spratT$No..hour)] <- 0
plaiceT$No..hour[is.na(plaiceT$No..hour)] <- 0

###### create a dataset with only the hauls with 0 catches

cod0 <- codT[is.na(codT$Lengthcl), ]
flounder0 <- flounderT[is.na(flounderT$Lengthcl), ]
herring0 <- herringT[is.na(herringT$Lengthcl), ]
sprat0 <- spratT[is.na(spratT$Lengthcl), ]
plaice0 <- plaiceT[is.na(plaiceT$Lengthcl), ]





###### merge historical swedish data with fiskdata2

historical_idx <- stand_un[, c("Fartyg", "drag", "datum", "år", "mån", "årstid", "tråltid", "område", "distans", "speed", "idx", "LATDD", "LONGDD", "Depth", "Size.final..m.", "Size.final..foot.", "Horizontal.opening..m.", "Fishing.line..foot.", "Swep.both.side..after.formula...meter.", "Swep.both.side..after.formula...foot.", "Swep.both.side..foot.", "Swept.area", "RSA", "RS")]

names(historical_idx) <- c("vessel", "haul", "date", "year", "month", "quarter", "trawl_time", "SD", "distans", "speed", "IDX", "LATDD", "LONGDD", "Depth", "Size.final..m.", "Size.final..foot.", "Horizontal.opening..m.", "Fishing.line..foot.", "Swep.both.side..after.formula...meter.", "Swep.both.side..after.formula...foot.", "Swep.both.side..foot.", "Swept.area", "RSA", "RS")


fiskdata2_idx <- index[, c("VESS_NAME", "1", "FROMYEAR", "STA_DURATION", "STA_SUBDIV_SUBDIV", "STA_DIST", "STA_SPEED", "idx", "STAVALCODE_DESCENG", "LATDD", "LONGDD", "STA_BOTAVG", "Size.final..m.", "Size.final..foot.", "Horizontal.opening..m.", "Fishing.line..foot.", "Swep.both.side..after.formula...meter.", "Swep.both.side..after.formula...foot.", "sweep.both.sides..foot.", "Swept.area", "RSA", "RS")]

names(fiskdata2_idx) <- c("vessel", "date", "year", "trawl_time", "SD", "distans", "speed", "IDX", "validity", "LATDD", "LONGDD", "Depth", "Size.final..m.", "Size.final..foot.", "Horizontal.opening..m.", "Fishing.line..foot.", "Swep.both.side..after.formula...meter.", "Swep.both.side..after.formula...foot.", "Swep.both.side..foot.", "Swept.area", "RSA", "RS")


survey_idx <- merge(historical_idx, fiskdata2_idx, all = T)

summary(survey_idx)



####### fix SD and ICES rect


##### RECT
### read the shapefile
xx <- readShapeSpatial("ICES_rect_shapefile/ices_squares_simple", IDvar = "ICESNAME")


##### match ICES rect
id <- over(SpatialPoints(survey_idx[, c("LONGDD", "LATDD")]), xx)

survey_idx$Rect <- id$ICESNAME
survey_idx$Rect <- factor(survey_idx$Rect)

##### SD
### read the shapefile
yy <- readShapeSpatial("ICES_areas/ices_areas")

sd <- c("22", "23", "24", "25", "26", "27", "28-1", "28-2", "29", "30", "31", "32", "IIIa")
yy <- yy[yy@data$ICES_area %in% sd, ]
idindex <- over(SpatialPoints(survey_idx[, c("LONGDD", "LATDD")]), yy)
survey_idx$SD <- idindex$ICES_area

survey_idx$SD <- factor(survey_idx$SD)
summary(survey_idx$SD)


##### cod
names(codT)
codTStuart <- codT[, c("VESS_NAME", "1", "FROMYEAR", "STA_DURATION", "STA_SUBDIV_SUBDIV", "STA_ICESRECT_RUTA", "STA_SPEED", "idx", "STAVALCODE_DESCENG", "LATDD", "LONGDD", "STA_BOTAVG", "Lengthcl", "No..hour", "RSA", "RS")]

names(codTStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "Rect", "speed", "IDX", "validity", "LATDD", "LONGDD", "Depth", "Lengthcl", "CPUEun", "RSA", "RS")
summary(codTStuart)

names(codhist)

codhistStuart <- codhist[, c("Fartyg", "datum", "år", "tråltid", "område", "speed", "idx", "LATDD", "LONGDD", "Depth", "art", "längd", "CPUEantal", "RSA", "RS")]

names(codhistStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "speed", "IDX", "LATDD", "LONGDD", "Depth", "species", "Lengthcl", "CPUEun", "RSA", "RS")
summary(codhistStuart)


Totcod <- merge(codhistStuart, codTStuart, all = T)
Totcod$species <- "Gadus morhua"
summary(Totcod)


##### flounder
names(flounderT)
flounderTStuart <- flounderT[, c("VESS_NAME", "1", "FROMYEAR", "STA_DURATION", "STA_SUBDIV_SUBDIV", "STA_ICESRECT_RUTA", "STA_SPEED", "idx", "STAVALCODE_DESCENG", "LATDD", "LONGDD", "STA_BOTAVG", "Lengthcl", "No..hour", "RSA", "RS")]

names(flounderTStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "Rect", "speed", "IDX", "validity", "LATDD", "LONGDD", "Depth", "Lengthcl", "CPUEun", "RSA", "RS")
summary(flounderTStuart)

names(flohist)

flohistStuart <- flohist[, c("Fartyg", "datum", "år", "tråltid", "område", "speed", "idx", "LATDD", "LONGDD", "Depth", "art", "längd", "CPUEantal", "RSA", "RS")]

names(flohistStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "speed", "IDX", "LATDD", "LONGDD", "Depth", "species", "Lengthcl", "CPUEun", "RSA", "RS")
summary(flohistStuart)


Totflo <- merge(flohistStuart, flounderTStuart, all = T)
Totflo$species <- "Platichthys flesus"
summary(Totflo)


##### herring
names(herringT)
herringTStuart <- herringT[, c("VESS_NAME", "1", "FROMYEAR", "STA_DURATION", "STA_SUBDIV_SUBDIV", "STA_ICESRECT_RUTA", "STA_SPEED", "idx", "STAVALCODE_DESCENG", "LATDD", "LONGDD", "STA_BOTAVG", "Lengthcl", "No..hour", "RSA", "RS")]

names(herringTStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "Rect", "speed", "IDX", "validity", "LATDD", "LONGDD", "Depth", "Lengthcl", "CPUEun", "RSA", "RS")
summary(herringTStuart)

names(herhist)

herhistStuart <- herhist[, c("Fartyg", "datum", "år", "tråltid", "område", "speed", "idx", "LATDD", "LONGDD", "Depth", "art", "längd", "CPUEantal", "RSA", "RS")]

names(herhistStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "speed", "IDX", "LATDD", "LONGDD", "Depth", "species", "Lengthcl", "CPUEun", "RSA", "RS")
summary(herhistStuart)


Tother <- merge(herhistStuart, herringTStuart, all = T)
Tother$species <- "Clupea harengus"
summary(Tother)


##### sprat
names(spratT)
spratTStuart <- spratT[, c("VESS_NAME", "1", "FROMYEAR", "STA_DURATION", "STA_SUBDIV_SUBDIV", "STA_ICESRECT_RUTA", "STA_SPEED", "idx", "STAVALCODE_DESCENG", "LATDD", "LONGDD", "STA_BOTAVG", "Lengthcl", "No..hour", "RSA", "RS")]

names(spratTStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "Rect", "speed", "IDX", "validity", "LATDD", "LONGDD", "Depth", "Lengthcl", "CPUEun", "RSA", "RS")
summary(spratTStuart)

names(sprhist)

sprhistStuart <- sprhist[, c("Fartyg", "datum", "år", "tråltid", "område", "speed", "idx", "LATDD", "LONGDD", "Depth", "art", "längd", "CPUEantal", "RSA", "RS")]

names(sprhistStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "speed", "IDX", "LATDD", "LONGDD", "Depth", "species", "Lengthcl", "CPUEun", "RSA", "RS")
summary(sprhistStuart)


Totspr <- merge(sprhistStuart, spratTStuart, all = T)
Totspr$species <- "Sprattus sprattus"
summary(Totspr)

##### plaice
names(plaiceT)
plaiceTStuart <- plaiceT[, c("VESS_NAME", "1", "FROMYEAR", "STA_DURATION", "STA_SUBDIV_SUBDIV", "STA_ICESRECT_RUTA", "STA_SPEED", "idx", "STAVALCODE_DESCENG", "LATDD", "LONGDD", "STA_BOTAVG", "Lengthcl", "No..hour", "RSA", "RS")]

names(plaiceTStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "Rect", "speed", "IDX", "validity", "LATDD", "LONGDD", "Depth", "Lengthcl", "CPUEun", "RSA", "RS")
summary(plaiceTStuart)

names(plahist)

plahistStuart <- plahist[, c("Fartyg", "datum", "år", "tråltid", "område", "speed", "idx", "LATDD", "LONGDD", "Depth", "art", "längd", "CPUEantal", "RSA", "RS")]

names(plahistStuart) <- c("vessel", "date", "year", "trawl_time", "SD", "speed", "IDX", "LATDD", "LONGDD", "Depth", "species", "Lengthcl", "CPUEun", "RSA", "RS")
summary(plahistStuart)


Totpla <- merge(plahistStuart, plaiceTStuart, all = T)
Totpla$species <- "Pleuronectes platessa"
summary(Totpla)


###### merge all species

DataStuart <- rbind(Totcod, Totflo, Tother, Totspr, Totpla)

DataStuart$species <- factor(DataStuart$species)

##### add standardized CPUE
DataStuart$CPUEst <- DataStuart$CPUEun * DataStuart$RSA * DataStuart$RS

######## change NAs in CPUEs with 0s

DataStuart$CPUEst[is.na(DataStuart$CPUEst)] <- 0

### fix SD and Rect

DataStuart$SD <- survey_idx[match(DataStuart$IDX, survey_idx$IDX), "SD"]
DataStuart$Rect <- survey_idx[match(DataStuart$IDX, survey_idx$IDX), "Rect"]

#### there are 3 hauls that are in "IIIa" so I will remove them

DataStuart <- DataStuart[DataStuart$SD != "IIIa", ]

summary(DataStuart)


######### standardization DATRAS

### Read the zip file



datras <- readExchange("New/Datras/Exchange Data_2017-03-28 11_54_40.zip")


####### HH

HH <- datras[["HH"]]

names(HH)

levels(HH$haul.id)

### put year as numeric

HH$Year <- as.numeric(as.character(HH$Year))

###### select just valid, additional and no oxygen hauls from every country except Sweden

HHdata <- HH %>%
  filter(Country != "SWE" & HaulVal %in% c("A", "N", "V"))


####### fix SD and ICES rect

##### RECT
### read the shapefile
xx <- readShapeSpatial("ICES_rect_shapefile/ices_squares_simple", IDvar = "ICESNAME")


##### match ICES rect
id <- over(SpatialPoints(HHdata[, c("lon", "lat")]), xx)

HHdata$Rect <- id$ICESNAME
HHdata$Rect <- factor(HHdata$Rect)

summary(HHdata$Rect)
summary(factor(HHdata$StatRec))


##### SD
### read the shapefile
yy <- readShapeSpatial("ICES_areas/ices_areas")


idindex <- over(SpatialPoints(HHdata[, c("lon", "lat")]), yy)
HHdata$SD <- idindex$ICES_area

##### there are 4 hauls on land. I will eliminate the 2 from Denmark because I don't know where to put them. I will put the Polish one in SD 26

HHdata$SD[is.na(HHdata$SD) & HHdata$Country == "POL"] <- "26"
HHdata <- HHdata[!is.na(HHdata$SD), ]

HHdata$SD <- factor(HHdata$SD)

summary(HHdata$SD)

##### there are 7 hauls with no depth registered. I will just put an aprox depth for one haul in SD 24, for one in 26 and 4 in SD 22.

HHdata[is.na(HHdata$Depth), ]
HHdata$Depth[is.na(HHdata$Depth) & HHdata$SD == "26"] <- 115
HHdata$Depth[is.na(HHdata$Depth) & HHdata$SD == "24"] <- 31
HHdata$Depth[is.na(HHdata$Depth) & HHdata$SD == "22" & HHdata$haul.id == "2000:4:DEN:HAF:TVS:018138:1"] <- 23
HHdata$Depth[is.na(HHdata$Depth) & HHdata$SD == "22" & HHdata$haul.id == "2002:4:DEN:HAF:TVS:8882:25"] <- 37
HHdata$Depth[is.na(HHdata$Depth) & HHdata$SD == "22" & HHdata$haul.id == "2008:4:DEN:HAF:TVS:1878:27"] <- 15
HHdata$Depth[is.na(HHdata$Depth) & HHdata$SD == "22" & HHdata$haul.id == "2009:4:DEN:HAF:TVS:2365:28"] <- 20



summary(HHdata)




####### HL

HL <- datras[["HL"]]

names(HL)

### put year as numeric

HL$Year <- as.numeric(as.character(HL$Year))

###### select just the hauls in HHdata

HLdata <- HL[HL$haul.id %in% HHdata$haul.id, ]

# write.csv(HLdata,"New/Datras/HLdata.csv")

#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode

HLdata$SD <- HHdata[match(HLdata$haul.id, HHdata$haul.id), "SD"]
HLdata$Rect <- HHdata[match(HLdata$haul.id, HHdata$haul.id), "Rect"]
HLdata$HaulVal <- HHdata[match(HLdata$haul.id, HHdata$haul.id), "HaulVal"]
HLdata$StdSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id), "StdSpecRecCode"]
HLdata$BycSpecRecCode <- HHdata[match(HLdata$haul.id, HHdata$haul.id), "BycSpecRecCode"]


summary(HLdata$HaulVal)
summary(HHdata$HaulVal)

####### select different species

hlcod <- HLdata %>%
  filter(Species == "Gadus morhua")

hlflo <- HLdata %>%
  filter(Species == "Platichthys flesus")

hlher <- HLdata %>%
  filter(Species == "Clupea harengus")

hlspr <- HLdata %>%
  filter(Species == "Sprattus sprattus")

hlpla <- HLdata %>%
  filter(Species == "Pleuronectes platessa")


##### add 0 catches
#### common columns for merging
comcol <- intersect(names(hlcod), names(HHdata))
comcol <- comcol[-c(2, 14, 16)]

##### for cod add 0s and then remove lines with SpecVal = 0
hlcod0 <- merge(hlcod, HHdata[, comcol], by = comcol, all = TRUE)
summary(hlcod0)

hlcod0$SpecVal[is.na(hlcod0$SpecVal)] <- "zeroCatch"
hlcod0$SpecVal <- factor(hlcod0$SpecVal)

hlcod0 <- hlcod0 %>%
  filter(SpecVal != "0")

hlcod0$Species <- "Gadus morhua"

##### for flounder add 0s, remove them if StdSpecRecCode !=1 and then remove lines with SpecVal = 0
hlflo0 <- merge(hlflo, HHdata[, comcol], by = comcol, all = TRUE)
summary(hlflo0)

hlflo0 <- hlflo0[!(is.na(hlflo0$Species) & hlflo0$StdSpecRecCode != 1), ]

hlflo0$SpecVal[is.na(hlflo0$SpecVal)] <- "zeroCatch"
hlflo0$SpecVal <- factor(hlflo0$SpecVal)

hlflo0 <- hlflo0 %>%
  filter(SpecVal != "0")

hlflo0$Species <- "Platichthys flesus"

##### for plaice add 0s, remove them if StdSpecRecCode !=1 and then remove lines with SpecVal = 0
hlpla0 <- merge(hlpla, HHdata[, comcol], by = comcol, all = TRUE)
summary(hlpla0)

hlpla0 <- hlpla0[!(is.na(hlpla0$Species) & hlpla0$StdSpecRecCode != 1), ]

hlpla0$SpecVal[is.na(hlpla0$SpecVal)] <- "zeroCatch"
hlpla0$SpecVal <- factor(hlpla0$SpecVal)

hlpla0 <- hlpla0 %>%
  filter(SpecVal != "0")

hlpla0$Species <- "Pleuronectes platessa"

##### for herring add 0s, remove them if BycSpecRecCode !=1 and then remove lines with SpecVal = 0
hlher0 <- merge(hlher, HHdata[, comcol], by = comcol, all = TRUE)
summary(hlher0)

hlher0 <- hlher0[!(is.na(hlher0$Species) & hlher0$BycSpecRecCode != 1), ]

hlher0$SpecVal[is.na(hlher0$SpecVal)] <- "zeroCatch"
hlher0$SpecVal <- factor(hlher0$SpecVal)

hlher0 <- hlher0 %>%
  filter(SpecVal != "0")

hlher0$Species <- "Clupea harengus"

##### for sprat add 0s, remove them if BycSpecRecCode !=1 and then remove lines with SpecVal = 0
hlspr0 <- merge(hlspr, HHdata[, comcol], by = comcol, all = TRUE)
summary(hlspr0)

hlspr0 <- hlspr0[!(is.na(hlspr0$Species) & hlspr0$BycSpecRecCode != 1), ]

hlspr0$SpecVal[is.na(hlspr0$SpecVal)] <- "zeroCatch"
hlspr0$SpecVal <- factor(hlspr0$SpecVal)

hlspr0 <- hlspr0 %>%
  filter(SpecVal != "0")

hlspr0$Species <- "Sprattus sprattus"

#### check number of hauls for each species
n_distinct(HHdata$haul.id)
n_distinct(hlcod0$haul.id)
n_distinct(hlflo0$haul.id)
n_distinct(hlpla0$haul.id)
n_distinct(hlher0$haul.id)
n_distinct(hlspr0$haul.id)



####### create CPUE unstandardized for SpecVal=1. If DataType = C then CPUEun=HLNoAtLngt, if DataType=R then CPUEun=HLNoAtLngt/(HaulDur/60), if DataType = S then CPUEun=(HLNoAtLngt*SubFactor)/(HaulDur/60). If SpecVal="zeroCatch" then CPUEun=0, if SpecVal=4 we need to decide.
# Then I will sum for the same haul the CPUE of the same length classes if they were sampled with different subfactors or with different sexes.

#### cod
hlcod0 <- hlcod0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

hlcod4 <- hlcod0 %>%
  filter(SpecVal == "4")

#### sum the CPUE for the fishes of the same length class in a haul
colsumsex <- names(hlcod0)[c(1:22, 30, 31, 35:37, 39)]


hlcodL <- hlcod0[, colsumsex] %>%
  filter(!SpecVal == "4")

temp <- summaryBy(CPUEun ~ LngtClas + haul.id, data = hlcodL, FUN = sum, keep.names = TRUE)

hlcodL <- merge(hlcodL[, -28], temp, by = c("LngtClas", "haul.id"), all.y = TRUE)

hlcodL <- unique(hlcodL)

#### flounder
hlflo0 <- hlflo0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

hlflo4 <- hlflo0 %>%
  filter(SpecVal == "4")

#### sum the CPUE for the fishes of the same length class in a haul

hlfloL <- hlflo0[, colsumsex] %>%
  filter(!SpecVal == "4")

temp <- summaryBy(CPUEun ~ LngtClas + haul.id, data = hlfloL, FUN = sum, keep.names = TRUE)

hlfloL <- merge(hlfloL[, -28], temp, by = c("LngtClas", "haul.id"), all.y = TRUE)

hlfloL <- unique(hlfloL)

#### herring
hlher0 <- hlher0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

hlher4 <- hlher0 %>%
  filter(SpecVal == "4")

#### sum the CPUE for the fishes of the same length class in a haul

hlherL <- hlher0[, colsumsex] %>%
  filter(!SpecVal == "4")

temp <- summaryBy(CPUEun ~ LngtClas + haul.id, data = hlherL, FUN = sum, keep.names = TRUE)

hlherL <- merge(hlherL[, -28], temp, by = c("LngtClas", "haul.id"), all.y = TRUE)

hlherL <- unique(hlherL)

#### sprat
hlspr0 <- hlspr0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

hlspr4 <- hlspr0 %>%
  filter(SpecVal == "4")

#### sum the CPUE for the fishes of the same length class in a haul

hlsprL <- hlspr0[, colsumsex] %>%
  filter(!SpecVal == "4")

temp <- summaryBy(CPUEun ~ LngtClas + haul.id, data = hlsprL, FUN = sum, keep.names = TRUE)

hlsprL <- merge(hlsprL[, -28], temp, by = c("LngtClas", "haul.id"), all.y = TRUE)

hlsprL <- unique(hlsprL)

#### plaice
hlpla0 <- hlpla0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

hlpla4 <- hlpla0 %>%
  filter(SpecVal == "4")

#### sum the CPUE for the fishes of the same length class in a haul

hlplaL <- hlpla0[, colsumsex] %>%
  filter(!SpecVal == "4")

temp <- summaryBy(CPUEun ~ LngtClas + haul.id, data = hlplaL, FUN = sum, keep.names = TRUE)

hlplaL <- merge(hlplaL[, -28], temp, by = c("LngtClas", "haul.id"), all.y = TRUE)

hlplaL <- unique(hlplaL)

####### remove SD 3a and sprat >20 cm

HHdata <- HHdata[HHdata$SD != "IIIa", ]
hlcodL <- hlcodL[hlcodL$SD != "IIIa", ]
hlfloL <- hlfloL[hlfloL$SD != "IIIa", ]
hlherL <- hlherL[hlherL$SD != "IIIa", ]
hlplaL <- hlplaL[hlplaL$SD != "IIIa", ]

hlsprL <- hlsprL %>%
  filter(SD != "IIIa" & LngtCm <= 20)

######### CA

CA <- datras[["CA"]]

names(CA)

### put year as numeric

CA$Year <- as.numeric(as.character(CA$Year))

###### select just the hauls in HHdata

CAdata <- CA[CA$haul.id %in% HHdata$haul.id, ]



#### match haul SD, rect, haul validity

CAdata$SD <- HHdata[match(CAdata$haul.id, HHdata$haul.id), "SD"]
CAdata$Rect <- HHdata[match(CAdata$haul.id, HHdata$haul.id), "Rect"]
CAdata$HaulVal <- HHdata[match(CAdata$haul.id, HHdata$haul.id), "HaulVal"]

####### select different species remove missing individual weights or weight = 0

cacod <- CAdata %>%
  filter(Species == "Gadus morhua" & IndWgt != 0 & !(is.na(IndWgt)))

caflo <- CAdata %>%
  filter(Species == "Platichthys flesus" & IndWgt != 0 & !(is.na(IndWgt)))

caher <- CAdata %>%
  filter(Species == "Clupea harengus" & IndWgt != 0 & !(is.na(IndWgt)))

caspr <- CAdata %>%
  filter(Species == "Sprattus sprattus" & IndWgt != 0 & !(is.na(IndWgt)))

capla <- CAdata %>%
  filter(Species == "Pleuronectes platessa" & IndWgt != 0 & !(is.na(IndWgt)))

####### change the flounder of 347 cm in 34.7 and remove sprat >=20

caflo$LngtCm[caflo$LngtCm == 347] <- 34.7

caspr <- caspr %>%
  filter(LngtCm <= 20)

###### check for outliers

ggplot(data = cacod, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("cod")
ggplot(data = caflo, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("flounder")
ggplot(data = caher, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("herring")
ggplot(data = caspr, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("sprat")
ggplot(data = capla, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("plaice")

ggplot(data = cacod, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("cod")
ggplot(data = caflo, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("flounder")
ggplot(data = caher, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("herring")
ggplot(data = caspr, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("sprat")
ggplot(data = capla, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("plaice")

###### clean outliers

cacod <- cacod %>%
  filter(!(haul.id == "1996:1:DEN:HAF:TVS:5666:44" & LngtCm == 88)) %>%
  filter(!(haul.id == "1999:1:DEN:DAN2:GRT:108:50" & LngtCm == 32)) %>%
  filter(!(haul.id == "2010:4:POL:BAL:TVL:25003:12" & LngtCm == 52 & IndWgt >= 10000)) %>%
  filter(!(haul.id == "1994:1:POL:BAL:P20:Jan:33" & LngtCm == 91)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:4" & LngtCm == 75)) %>%
  filter(!(haul.id == "2013:1:GFR:SOL2:TVS:24104:49" & LngtCm == 81))

caflo <- caflo %>%
  filter(!(haul.id == "2008:4:DEN:DAN2:TVL:65:31")) %>%
  filter(!(haul.id == "2004:4:RUS:ATL:TVL:NA:71")) %>%
  filter(!(haul.id == "2007:4:EST:CEV:TVS:2:10" & LngtCm == 17 & IndWgt >= 200)) %>%
  filter(!(haul.id == "2006:4:EST:CEV:TVS:1:2" & LngtCm == 19 & IndWgt >= 800)) %>%
  filter(!(haul.id == "2002:1:GFR:SOL:TVS:288:125" & LngtCm == 24 & IndWgt == 530)) %>%
  filter(!(haul.id == "1995:1:GFR:SOL:H20:37:27" & LngtCm == 26 & IndWgt == 550)) %>%
  filter(!(haul.id == "1996:4:GFR:SOL:H20:27:59" & LngtCm == 36 & IndWgt == 270)) %>%
  filter(!(haul.id == "1996:4:GFR:SOL:H20:49:48" & LngtCm == 38 & IndWgt == 450)) %>%
  filter(!(haul.id == "1996:1:GFR:SOL:H20:51:43" & LngtCm == 38 & IndWgt == 520)) %>%
  filter(!(haul.id == "1996:4:GFR:SOL:H20:48:47" & LngtCm == 38 & IndWgt == 565)) %>%
  filter(!(haul.id == "1996:1:GFR:SOL:H20:46:49" & LngtCm == 38 & IndWgt == 590)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:29:15" & LngtCm == 26 & IndWgt == 85)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:26:13" & LngtCm == 30 & IndWgt == 156)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:26:13" & LngtCm == 30 & IndWgt == 166)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:3:2" & LngtCm == "26.5" & IndWgt == "826")) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:6:3" & LngtCm == "26.5" & IndWgt == "448")) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:6:3" & LngtCm == 34 & IndWgt == 184)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:32:16" & LngtCm == "31.5" & IndWgt == "216")) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:29:15" & LngtCm == "31.5" & IndWgt == "930")) %>%
  filter(!(haul.id == "2005:4:GFR:SOL2:TVS:24076:50" & LngtCm == 40 & IndWgt == 462)) %>%
  filter(!(haul.id == "2012:4:GFR:SOL2:TVS:24325:17" & LngtCm == 25 & IndWgt == 523)) %>%
  filter(!(haul.id == "1995:1:GFR:SOL:H20:71:67" & LngtCm == 28 & IndWgt == 560)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:83:40" & LngtCm == "34.5" & IndWgt == "986")) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:54:26" & LngtCm == 34 & IndWgt == 980)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:67:32" & LngtCm == "35.5" & IndWgt == "930")) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:77:37" & LngtCm == 37 & IndWgt == 902)) %>%
  filter(!(haul.id == "2001:1:DEN:DAN2:TVL:61:29" & LngtCm == "36.5" & IndWgt == "884")) %>%
  filter(!(haul.id == "2003:4:POL:BAL:TVL:NA:26" & LngtCm == 40 & IndWgt == 429)) %>%
  filter(!(haul.id == "2005:4:POL:BAL:TVL:NA:22" & LngtCm == 27 & IndWgt == 830)) %>%
  filter(!(haul.id == "2005:4:POL:BAL:TVL:NA:11" & LngtCm == 25 & IndWgt == 587)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:257:34" & LngtCm == 24 & IndWgt == 588)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:166:13" & LngtCm == 34 & IndWgt == 150)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:206:18" & LngtCm == 35 & IndWgt == 162)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:162:11" & LngtCm == 35 & IndWgt == 202)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:162:11" & LngtCm == 39 & IndWgt == 314)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:167:14" & LngtCm == 37 & IndWgt == 346)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:162:11" & LngtCm == 37 & IndWgt == 418)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:166:13" & LngtCm == 37 & IndWgt == 498)) %>%
  filter(!(haul.id == "2007:4:DEN:DAN2:TVL:208:19" & LngtCm == 38 & IndWgt == 522)) %>%
  filter(!(haul.id == "2009:4:DEN:DAN2:TVL:76:47" & LngtCm == 16 & IndWgt == 186)) %>%
  filter(!(haul.id == "2010:4:DEN:DAN2:TVL:55:44" & LngtCm == 20 & IndWgt == 290)) %>%
  filter(!(haul.id == "2010:4:DEN:DAN2:TVL:110:4" & LngtCm == 20 & IndWgt == 280)) %>%
  filter(!(haul.id == "2010:4:DEN:DAN2:TVL:110:4" & LngtCm == 20 & IndWgt == 238)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:180:18" & LngtCm == 20 & IndWgt == 258)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:182:19" & LngtCm == 23 & IndWgt == 470)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:200:29" & LngtCm == 22 & IndWgt == 436)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:210:34" & LngtCm == 21 & IndWgt == 358)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:206:32" & LngtCm == 23 & IndWgt == 348)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:218:38" & LngtCm == 22 & IndWgt == 304)) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:132:8" & LngtCm == "23.7" & IndWgt == "412")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:113:4" & LngtCm == "20.3" & IndWgt == "374")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:68:44" & LngtCm == "24.8" & IndWgt == "244")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:86:46" & LngtCm == "22.6" & IndWgt == "236")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:139:10" & LngtCm == "32.8" & IndWgt == "116")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:145:14" & LngtCm == "33.2" & IndWgt == "150")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:134:9" & LngtCm == "35.9" & IndWgt == "270")) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:113:4" & LngtCm == "40.8" & IndWgt == "284")) %>%
  filter(!(haul.id == "2002:1:RUS:VSH:TVL:NA:1" & LngtCm == 41 & IndWgt == 530)) %>%
  filter(!(haul.id == "2002:1:RUS:VSH:TVL:NA:38" & LngtCm == 42 & IndWgt == 640)) %>%
  filter(!(haul.id == "2002:1:POL:BAL:TVL:7:7" & LngtCm == 44 & IndWgt == 650)) %>%
  filter(!(haul.id == "2002:1:POL:BAL:TVL:7:7" & LngtCm == 44 & IndWgt == 680)) %>%
  filter(!(haul.id == "2002:1:RUS:VSH:TVL:NA:41" & LngtCm == 41 & IndWgt == 720)) %>%
  filter(!(haul.id == "2003:1:RUS:ATL:TVL:NA:50" & LngtCm == 20 & IndWgt == 300)) %>%
  filter(!(haul.id == "2003:4:RUS:ATLD:TVL:NA:58" & LngtCm == 33 & IndWgt == 140)) %>%
  filter(!(haul.id == "2004:4:LTU:DAR:TVS:NA:1" & LngtCm == 11 & IndWgt == 183)) %>%
  filter(!(haul.id == "2004:4:POL:BAL:TVL:NA:7" & LngtCm == 29 & IndWgt == 860)) %>%
  filter(!(haul.id == "2004:1:RUS:ATL:TVL:NA:6" & LngtCm == 30 & IndWgt == 600)) %>%
  filter(!(haul.id == "2004:1:RUS:ATL:TVL:NA:22" & LngtCm == 25 & IndWgt == 480)) %>%
  filter(!(haul.id == "2005:1:LTU:DAR:TVS:NA:3" & LngtCm == 23 & IndWgt == 534)) %>%
  filter(!(haul.id == "2005:1:RUS:ATL:TVL:NA:1" & LngtCm == 23 & IndWgt == 430)) %>%
  filter(!(haul.id == "2005:1:RUS:ATL:TVL:NA:1" & LngtCm == 24 & IndWgt == 420)) %>%
  filter(!(haul.id == "2005:1:RUS:ATL:TVL:NA:52" & LngtCm == 26 & IndWgt == 430)) %>%
  filter(!(haul.id == "2005:4:RUS:ATLD:TVL:36:42" & LngtCm == 35 & IndWgt == 284)) %>%
  filter(!(haul.id == "2005:4:RUS:ATLD:TVL:36:35" & LngtCm == 38 & IndWgt == 299)) %>%
  filter(!(haul.id == "2005:4:RUS:ATLD:TVL:36:34" & LngtCm == 38 & IndWgt == 380)) %>%
  filter(!(haul.id == "2006:1:RUS:ATLD:TVL:NA:18" & LngtCm == 39 & IndWgt == 390)) %>%
  filter(!(haul.id == "2007:1:LTU:DAR:TVS:Mar:3" & LngtCm == 24 & IndWgt == 499)) %>%
  filter(!(haul.id == "2007:4:LTU:DAR:TVS:Nov:5" & LngtCm == 20 & IndWgt == 307)) %>%
  filter(!(haul.id == "2007:4:LTU:DAR:TVS:Nov:8" & LngtCm == 23 & IndWgt == 305)) %>%
  filter(!(haul.id == "2007:1:RUS:ATL:TVL:52:3" & LngtCm == 26 & IndWgt == 440)) %>%
  filter(!(haul.id == "2007:1:LTU:DAR:TVS:Mar:4" & LngtCm == 36 & IndWgt == 221)) %>%
  filter(!(haul.id == "2008:1:RUS:ATLD:TVL:49:3" & LngtCm == 30 & IndWgt == 90)) %>%
  filter(!(haul.id == "2008:4:DEN:DAN2:TVL:46:26" & LngtCm == 27 & IndWgt == 454)) %>%
  filter(!(haul.id == "2008:1:LTU:DAR:TVS:March:1" & LngtCm == 36 & IndWgt == 237)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:25" & LngtCm == 29 & IndWgt == 560)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:6" & LngtCm == 24 & IndWgt == 510)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:6" & LngtCm == 26 & IndWgt == 490)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:6" & LngtCm == 33 & IndWgt == 200)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:10" & LngtCm == 36 & IndWgt == 300)) %>%
  filter(!(haul.id == "2009:4:LTU:DAR:TVS:26027:1" & LngtCm == 39 & IndWgt == 322)) %>%
  filter(!(haul.id == "2010:1:RUS:ATL:TVL:60:21" & LngtCm == 32 & IndWgt == 111)) %>%
  filter(!(haul.id == "2010:1:RUS:ATL:TVL:60:29" & LngtCm == 34 & IndWgt == 153)) %>%
  filter(!(haul.id == "2010:1:RUS:ATL:TVL:60:23" & LngtCm == 33 & IndWgt == 156)) %>%
  filter(!(haul.id == "2010:1:RUS:ATL:TVL:60:18" & LngtCm == 36 & IndWgt == 221)) %>%
  filter(!(haul.id == "2011:1:RUS:ATLD:TVL:56:40" & LngtCm == 30 & IndWgt == 632)) %>%
  filter(!(haul.id == "2012:4:POL:BAL:TVL:26177:17" & LngtCm == 26 & IndWgt == 23)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:190:23" & LngtCm == 22 & IndWgt == 290)) %>%
  filter(!(haul.id == "2013:1:RUS:ATLD:TVL:60:33" & LngtCm == 23 & IndWgt == 382)) %>%
  filter(!(haul.id == "2013:1:LAT:BALL:TVL:1:1" & LngtCm == 34 & IndWgt == 114)) %>%
  filter(!(haul.id == "2013:1:RUS:ATLD:TVL:60:33" & LngtCm == 32 & IndWgt == 144)) %>%
  filter(!(haul.id == "2013:1:LTU:DAR:TVS:26030:3" & LngtCm == 41 & IndWgt == 384)) %>%
  filter(!(haul.id == "1996:1:LAT:MONL:DT:000001:74" & LngtCm == 36 & IndWgt == 1350)) %>%
  filter(!(haul.id == "1997:4:LAT:CLV:LBT:000002:15" & LngtCm == 35 & IndWgt == 208)) %>%
  filter(!(haul.id == "1997:2:LAT:CLV:LBT:000001:2" & LngtCm == 33 & IndWgt == 240)) %>%
  filter(!(haul.id == "1999:1:LAT:CLV:LBT:000001:21" & LngtCm == 20 & IndWgt == 220)) %>%
  filter(!(haul.id == "1999:1:LAT:CLV:LBT:000001:4" & LngtCm == 24 & IndWgt == 626)) %>%
  filter(!(haul.id == "1999:2:LAT:CLV:LBT:000001:35" & LngtCm == 23 & IndWgt == 395)) %>%
  filter(!(haul.id == "1999:2:LAT:CLV:LBT:000001:27" & LngtCm == 21 & IndWgt == 280)) %>%
  filter(!(haul.id == "2000:1:LAT:CLV:LBT:000001:16" & LngtCm == 20 & IndWgt == 210)) %>%
  filter(!(haul.id == "2000:4:LAT:CLV:LBT:000002:15" & LngtCm == 28 & IndWgt == 603)) %>%
  filter(!(haul.id == "2001:1:LAT:CLV:TVS:1:3" & LngtCm == 40 & IndWgt == 618)) %>%
  filter(!(haul.id == "2005:4:EST:CEV:TVS:1:10" & LngtCm == 26 & IndWgt == 435)) %>%
  filter(!(haul.id == "2014:1:LAT:BALL:TVL:1:10" & LngtCm == 14 & IndWgt == 108)) %>%
  filter(!(haul.id == "2000:4:EST:KOOT:TVS:000018:18" & LngtCm == 21 & IndWgt == 18)) %>%
  filter(!(haul.id == "2000:4:EST:KOOT:TVS:000018:18" & LngtCm == 20 & IndWgt == 32)) %>%
  filter(!(haul.id == "2000:4:EST:KOOT:TVS:000018:18" & LngtCm == 36 & IndWgt == 973)) %>%
  filter(!(haul.id == "2005:4:EST:CEV:TVS:1:2" & LngtCm == 26 & IndWgt == 94)) %>%
  filter(!(haul.id == "2005:4:EST:CEV:TVS:1:6" & LngtCm == 29 & IndWgt == 110)) %>%
  filter(!(haul.id == "2006:4:EST:CEV:TVS:1:1" & LngtCm == 20 & IndWgt == 300)) %>%
  filter(!(haul.id == "2006:4:EST:CEV:TVS:1:3" & LngtCm == 18 & IndWgt == 167)) %>%
  filter(!(haul.id == "2006:4:EST:CEV:TVS:1:3" & LngtCm == 19 & IndWgt == 161)) %>%
  filter(!(haul.id == "2007:4:EST:CEV:TVS:1:6" & LngtCm == 21 & IndWgt == 226)) %>%
  filter(!(haul.id == "2007:4:EST:CEV:TVS:1:6" & LngtCm == 19 & IndWgt == 185)) %>%
  filter(!(haul.id == "2007:4:EST:CEV:TVS:1:5" & LngtCm == 20 & IndWgt == 175)) %>%
  filter(!(haul.id == "2007:4:EST:CEV:TVS:1:5" & LngtCm == 19 & IndWgt == 165)) %>%
  filter(!(haul.id == "2007:4:EST:CEV:TVS:1:3" & LngtCm == 22 & IndWgt == 37)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:4" & LngtCm == 11 & IndWgt == 117)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:4" & LngtCm == 11 & IndWgt == 135)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:1" & LngtCm == 20 & IndWgt == 31)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:1" & LngtCm == 20 & IndWgt == 37)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:1" & LngtCm == 19 & IndWgt == 32)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:1" & LngtCm == 19 & IndWgt == 36)) %>%
  filter(!(haul.id == "2009:4:EST:CEV:TVS:2:1" & LngtCm == 29 & IndWgt == 137)) %>%
  filter(!(haul.id == "2010:4:EST:CEV:TVS:1:8" & LngtCm == 29 & IndWgt == 133)) %>%
  filter(!(haul.id == "2014:4:EST:CEV:TVS:1:4" & LngtCm == 11 & IndWgt == 52)) %>%
  filter(!(haul.id == "2014:4:EST:CEV:TVS:1:4" & LngtCm == 20 & IndWgt == 193)) %>%
  filter(!(haul.id == "2014:4:EST:CEV:TVS:1:4" & LngtCm == "18" & IndWgt == "179.5")) %>%
  filter(!(haul.id == "2014:4:EST:CEV:TVS:1:4" & LngtCm == 23 & IndWgt == 56.5)) %>%
  filter(!(haul.id == "1994:2:LAT:MONL:DT:000001:14" & LngtCm == 25 & IndWgt == 420)) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:6" & LngtCm == 24 & IndWgt == 460)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:222:41" & LngtCm == 25 & IndWgt == 476)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:222:41" & LngtCm == 25 & IndWgt == 470)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:222:41" & LngtCm == 25 & IndWgt == 424)) %>%
  filter(!(haul.id == "2013:1:DEN:DAN2:TVL:113:4" & LngtCm == "25.2" & IndWgt == "546")) %>%
  filter(!(haul.id == "2009:1:RUS:ATL:TVL:57:34" & LngtCm == 24 & IndWgt == 460)) %>%
  filter(!(haul.id == "2016:1:DEN:DAN2:TVL:53:10" & LngtCm == 20 & IndWgt == 490)) %>%
  filter(!(haul.id == "2015:1:GFR:SOL2:TVS:24067:19" & LngtCm == 32 & IndWgt == 137)) %>%
  filter(!(haul.id == "2017:1:GFR:SOL2:TVS:24321:26" & LngtCm == 23 & IndWgt == 413)) %>%
  filter(!(haul.id == "2008:4:DEN:DAN2:TVL:63:30"))



caflo %>%
  filter(LngtCm <= 25 & IndWgt >= 400 & LngtCm >= 20)



capla <- capla %>%
  filter(!(haul.id == "2009:1:DEN:DAN2:TVL:92:48" & LngtCm == 33)) %>%
  filter(!(haul.id == "2014:4:GFR:SOL2:TVS:24039:28" & LngtCm == 25)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:172:14" & LngtCm == 1)) %>%
  filter(!(haul.id == "2012:1:DEN:DAN2:TVL:172:14" & LngtCm == 2)) %>%
  filter(!(haul.id == "2009:1:DEN:DAN2:TVL:146:10" & LngtCm == 49)) %>%
  filter(!(haul.id == "2009:1:DEN:DAN2:TVL:140:7" & LngtCm == 40)) %>%
  filter(!(haul.id == "2009:1:DEN:DAN2:TVL:140:7" & LngtCm == 42))

###### create mean condition per year and SD per different length classes and plot it

cacod1020 <- cacod %>%
  filter(LngtCm >= 10 & LngtCm <= 19.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = cacod1020[cacod1020$SD != "29", ], aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("cod 10-20")

cacod3040 <- cacod %>%
  filter(LngtCm >= 30 & LngtCm <= 39.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = cacod3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("cod 30-40")

cacod5060 <- cacod %>%
  filter(LngtCm >= 50 & LngtCm <= 59.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = cacod5060[cacod5060$SD != "29", ], aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("cod 50-60")

caflo1020 <- caflo %>%
  filter(LngtCm >= 10 & LngtCm <= 19.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = caflo1020, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("flounder 10-20")

caflo2030 <- caflo %>%
  filter(LngtCm >= 20 & LngtCm <= 29.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = caflo2030, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("flounder 20-30")

caflo3040 <- caflo %>%
  filter(LngtCm >= 30 & LngtCm <= 39.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = caflo3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("flounder 30-40")

capla2030 <- capla %>%
  filter(LngtCm >= 20 & LngtCm <= 29.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = capla2030, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("plaice 20-30")

capla3040 <- capla %>%
  filter(LngtCm >= 30 & LngtCm <= 39.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = capla3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  stat_smooth() +
  ggtitle("plaice 30-40")


######## load fiskdata2 individual data for cod and flounder
cacodfd2 <- read.csv("New/Fiskdata2/trawlsurv_s_Torsk.csv", sep = ";")
caflofd2 <- read.csv("New/Fiskdata2/trawlsurv_s_Skrubba.csv", sep = ";")

cacodfd21 <- read.csv("New/Fiskdata2/trawl_surveys_(s)BITSCOD.csv", sep = ";")
caflofd21 <- read.csv("New/Fiskdata2/trawl_surveys_(s)BITSFLOUNDER.csv", sep = ";")


###### merge them
cacodfd2 <- rbind(cacodfd2[, -17], cacodfd21[, c(1:17, 19, 22:24)])
caflofd2 <- rbind(caflofd2[, -17], caflofd21[, c(1:17, 19, 22:24)])



###### change length in cm

cacodfd2$Lengthcl <- cacodfd2$Lengthcl / 10
caflofd2$Lengthcl <- caflofd2$Lengthcl / 10

###### create a new index to match them

cacodfd2$idx <- paste(cacodfd2$Date, cacodfd2$Haul, sep = ".")
caflofd2$idx <- paste(caflofd2$Date, caflofd2$Haul, sep = ".")
index <- cbind(index, do.call("rbind", strsplit(as.character(index$index), "[.]")))
index$idx2 <- paste(index$"1", index$"3", sep = ".")

#### match lat and long

cacodfd2$LATDD <- index[match(cacodfd2$idx, index$idx2), "LATDD"]
cacodfd2$LONGDD <- index[match(cacodfd2$idx, index$idx2), "LONGDD"]
caflofd2$LATDD <- index[match(caflofd2$idx, index$idx2), "LATDD"]
caflofd2$LONGDD <- index[match(caflofd2$idx, index$idx2), "LONGDD"]

##### remove invalid and calibration hauls

cacodfd2 <- cacodfd2[!cacodfd2$Validity %in% type, ]
caflofd2 <- caflofd2[!caflofd2$Validity %in% type, ]

##### remove length =0 and weight=0 and NAs in lat and long

cacodfd2 <- cacodfd2 %>%
  filter(Lengthcl != 0) %>%
  filter(!(is.na(Weight))) %>%
  filter(!(is.na(LATDD)))

summary(cacodfd2)

caflofd2 <- caflofd2 %>%
  filter(Lengthcl != 0) %>%
  filter(!(is.na(Weight))) %>%
  filter(!(is.na(LATDD)))

summary(caflofd2)


##### match ICES rect
id <- over(SpatialPoints(cacodfd2[, c("LONGDD", "LATDD")]), xx)

cacodfd2$Rect <- id$ICESNAME
cacodfd2$Rect <- factor(cacodfd2$Rect)

id <- over(SpatialPoints(caflofd2[, c("LONGDD", "LATDD")]), xx)

caflofd2$Rect <- id$ICESNAME
caflofd2$Rect <- factor(caflofd2$Rect)


##### SD

sd <- c("22", "23", "24", "25", "26", "27", "28-1", "28-2", "29", "30", "31", "32", "IIIa")
yy <- yy[yy@data$ICES_area %in% sd, ]


idindex <- over(SpatialPoints(cacodfd2[, c("LONGDD", "LATDD")]), yy)
cacodfd2$SD <- idindex$ICES_area
cacodfd2$SD <- factor(cacodfd2$SD)


idindex <- over(SpatialPoints(caflofd2[, c("LONGDD", "LATDD")]), yy)
caflofd2$SD <- idindex$ICES_area
caflofd2$SD <- factor(caflofd2$SD)

##### remove SD="IIIa" for cod

cacodfd2 <- cacodfd2[cacodfd2$SD != "IIIa", ]

###### check for outliers

ggplot(data = cacodfd2, aes(x = Lengthcl, y = Weight)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("cod")
ggplot(data = caflofd2, aes(x = Lengthcl, y = Weight)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("flounder")

ggplot(data = cacodfd2, aes(x = Lengthcl, y = Weight)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("cod")
ggplot(data = caflofd2, aes(x = Lengthcl, y = Weight)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("flounder")


###### clean outliers

caflofd2 <- caflofd2 %>%
  filter(!(Year == 2002 & Lengthcl == 10)) %>%
  filter(!(Year == 2010 & Lengthcl == 22 & Weight == 396)) %>%
  filter(!(Year == 2016 & Lengthcl == 19 & Weight == 720)) %>%
  filter(!(Year == 2016 & Lengthcl == 37 & Weight == 206)) %>%
  filter(!(Year == 2008 & Lengthcl == 39 & Weight == 258))


###### merge L/W of cod and flounder

lwcod <- cacod %>%
  filter(Year <= 2016) %>%
  select(Year, SD, LngtCm, IndWgt)

lwflo <- caflo %>%
  filter(Year <= 2016) %>%
  select(Year, SD, LngtCm, IndWgt)

lwcod2 <- cacodfd2 %>%
  select(Year, SD, Lengthcl, Weight)

names(lwcod2) <- names(lwcod)

lwflo2 <- caflofd2 %>%
  select(Year, SD, Lengthcl, Weight)

names(lwflo2) <- names(lwflo)

lwcodT <- rbind(lwcod, lwcod2)
lwfloT <- rbind(lwflo, lwflo2)

###### check for outliers

ggplot(data = lwcodT, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("cod")
ggplot(data = lwfloT, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("flounder")

ggplot(data = lwcodT, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("cod")
ggplot(data = lwfloT, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("flounder")


#######  L/W from DATRAS plaice
lwplaT <- capla %>%
  filter(Year <= 2016) %>%
  select(Year, SD, LngtCm, IndWgt)




######## Historical Latvian data

####### HH_LAT

HH_LAT <- read.csv("New/Datras/Historic Latvian/HH_LAT.csv", sep = ";")

names(HH_LAT)

#### create haul.id

HH_LAT$haul.id <- paste(HH_LAT$Year, HH_LAT$Quarter, HH_LAT$Country, HH_LAT$Ship, HH_LAT$Gear, HH_LAT$StNo, HH_LAT$HaulNo, sep = ":")

HH_LAT$haul.id <- factor(HH_LAT$haul.id)

levels(HH_LAT$haul.id)


###### select just valid, additional and no oxygen hauls

HH_LAT <- HH_LAT %>%
  filter(HaulVal %in% c("A", "N", "V"))

##### select lat and long

HH_LAT$lat <- HH_LAT$ShootLat
HH_LAT$lon <- HH_LAT$ShootLong

##### fix one hauls that seems to south

HH_LAT$lat[HH_LAT$haul.id == "1978:1:LAT:4576:LBT:3:5"] <- 54.78
HH_LAT$ShootLat[HH_LAT$haul.id == "1978:1:LAT:4576:LBT:3:5"] <- 54.78
HH_LAT$HaulLat[HH_LAT$haul.id == "1978:1:LAT:4576:LBT:3:5"] <- 54.81


####### fix SD and ICES rect


##### RECT
### read the shapefile
xx <- readShapeSpatial("ICES_rect_shapefile/ices_squares_simple", IDvar = "ICESNAME")


##### match ICES rect
id <- over(SpatialPoints(HH_LAT[, c("lon", "lat")]), xx)

HH_LAT$Rect <- id$ICESNAME
HH_LAT$Rect <- factor(HH_LAT$Rect)

summary(HH_LAT$Rect)
summary(factor(HH_LAT$StatRec))


##### SD
### read the shapefile
yy <- readShapeSpatial("ICES_areas/ices_areas")


idindex <- over(SpatialPoints(HH_LAT[, c("lon", "lat")]), yy)
HH_LAT$SD <- idindex$ICES_area

###### remove pelagic gear LPT

HH_LAT <- HH_LAT[!HH_LAT$Gear == "LPT", ]

summary(HH_LAT)

##### there are 2 hauls with no depth registered. I will just put an aprox depth

HH_LAT[HH_LAT$Depth == "-9", ]
HH_LAT$Depth[HH_LAT$haul.id == "1981:4:LAT:ZBA:LBT:11:4"] <- 10
HH_LAT$Depth[HH_LAT$haul.id == "1982:1:LAT:ZBA:LBT:3:27"] <- 18



####### HL_LAT

HL_LAT <- read.csv("New/Datras/Historic Latvian/HL_LAT.csv", sep = ";")

names(HL_LAT)

#### create haul.id

HL_LAT$haul.id <- paste(HL_LAT$Year, HL_LAT$Quarter, HL_LAT$Country, HL_LAT$Ship, HL_LAT$Gear, HL_LAT$StNo, HL_LAT$HaulNo, sep = ":")

HL_LAT$haul.id <- factor(HL_LAT$haul.id)

levels(HL_LAT$haul.id)


###### select just the hauls in HH_LAT

HL_LAT <- HL_LAT[HL_LAT$haul.id %in% HH_LAT$haul.id, ]


#### match haul SD, rect, haul validity, StdSpecRecCode and BycSpecRecCode and haul duration

HL_LAT$SD <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "SD"]
HL_LAT$Rect <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "Rect"]
HL_LAT$HaulVal <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "HaulVal"]
HL_LAT$StdSpecRecCode <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "StdSpecRecCode"]
HL_LAT$BycSpecRecCode <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "BycSpecRecCode"]
HL_LAT$HaulDur <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "HaulDur"]
HL_LAT$DataType <- HH_LAT[match(HL_LAT$haul.id, HH_LAT$haul.id), "DataType"]


summary(HL_LAT$HaulVal)
summary(HH_LAT$HaulVal)

####### select different species

HL_LATcod <- HL_LAT %>%
  filter(Species == "Gadus morhua")

HL_LATflo <- HL_LAT %>%
  filter(Species == "Platichthys flesus")


##### add 0 catches
#### common columns for merging
comcol <- intersect(names(HL_LATcod), names(HH_LAT))
comcol <- comcol[-c(1)]

##### for cod add 0s and then remove lines with SpecVal = 0
HL_LATcod0 <- merge(HL_LATcod, HH_LAT[, comcol], by = comcol, all = TRUE)
summary(HL_LATcod0)

HL_LATcod0$SpecVal[is.na(HL_LATcod0$SpecVal)] <- "zeroCatch"
HL_LATcod0$SpecVal <- factor(HL_LATcod0$SpecVal)

HL_LATcod0 <- HL_LATcod0 %>%
  filter(SpecVal != "0")

HL_LATcod0$Species <- "Gadus morhua"

##### for flounder add 0s, remove them if StdSpecRecCode !=1 and then remove lines with SpecVal = 0
HL_LATflo0 <- merge(HL_LATflo, HH_LAT[, comcol], by = comcol, all = TRUE)
summary(HL_LATflo0)

HL_LATflo0 <- HL_LATflo0[!(is.na(HL_LATflo0$Species) & HL_LATflo0$StdSpecRecCode != 1), ]

HL_LATflo0$SpecVal[is.na(HL_LATflo0$SpecVal)] <- "zeroCatch"
HL_LATflo0$SpecVal <- factor(HL_LATflo0$SpecVal)

HL_LATflo0 <- HL_LATflo0 %>%
  filter(SpecVal != "0")

HL_LATflo0$Species <- "Platichthys flesus"


#### check number of hauls for each species
n_distinct(HH_LAT$haul.id)
n_distinct(HL_LATcod0$haul.id)
n_distinct(HL_LATflo0$haul.id)



####### create CPUE unstandardized for SpecVal=1. If DataType = C then CPUEun=HLNoAtLngt, if DataType=R then CPUEun=HLNoAtLngt/(HaulDur/60), if DataType = S then CPUEun=(HLNoAtLngt*SubFactor)/(HaulDur/60). If SpecVal="zeroCatch" then CPUEun=0, if SpecVal=4 we need to decide.


#### cod
HL_LATcod0 <- HL_LATcod0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

HL_LATcod4 <- HL_LATcod0 %>%
  filter(SpecVal == "4")

##### create the HL with LFD

HL_LATcodL <- HL_LATcod0 %>%
  filter(!SpecVal == "4")

#### flounder
HL_LATflo0 <- HL_LATflo0 %>%
  mutate(CPUEun = ifelse(SpecVal == "1" & DataType == "C", HLNoAtLngt, ifelse(SpecVal == "1" & DataType == "R", HLNoAtLngt / (HaulDur / 60), ifelse(SpecVal == "1" & DataType == "S", (HLNoAtLngt * SubFactor) / (HaulDur / 60), ifelse(SpecVal == "zeroCatch", 0, NA)))))

##### create the data.frame with records without LFD

HL_LATflo4 <- HL_LATflo0 %>%
  filter(SpecVal == "4")

##### create the HL with LFD

HL_LATfloL <- HL_LATflo0 %>%
  filter(!SpecVal == "4")

######### CA_LAT

CA_LAT <- read.csv("New/Datras/Historic Latvian/CA_LAT.csv", sep = ";")

names(CA_LAT)

#### create haul.id

CA_LAT$haul.id <- paste(CA_LAT$Year, CA_LAT$Quarter, CA_LAT$Country, CA_LAT$Ship, CA_LAT$Gear, CA_LAT$StNo, CA_LAT$HaulNo, sep = ":")

CA_LAT$haul.id <- factor(CA_LAT$haul.id)

levels(CA_LAT$haul.id)


###### select just the hauls in HH_LAT

CA_LAT <- CA_LAT[CA_LAT$haul.id %in% HH_LAT$haul.id, ]



#### match haul SD, rect, haul validity

CA_LAT$SD <- HH_LAT[match(CA_LAT$haul.id, HH_LAT$haul.id), "SD"]
CA_LAT$Rect <- HH_LAT[match(CA_LAT$haul.id, HH_LAT$haul.id), "Rect"]
CA_LAT$HaulVal <- HH_LAT[match(CA_LAT$haul.id, HH_LAT$haul.id), "HaulVal"]

####### select different species remove missing individual weights or weight = 0

CA_LATcod <- CA_LAT %>%
  filter(Species == "Gadus morhua" & IndWgt != 0 & !(is.na(IndWgt)))

CA_LATflo <- CA_LAT %>%
  filter(Species == "Platichthys flesus" & IndWgt != 0 & !(is.na(IndWgt)))


###### check for outliers

ggplot(data = CA_LATcod, aes(x = LngtClas, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("cod")
ggplot(data = CA_LATflo, aes(x = LngtClas, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("flounder")


ggplot(data = CA_LATcod, aes(x = LngtClas, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("cod")
ggplot(data = CA_LATflo, aes(x = LngtClas, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("flounder")


###### clean outliers

CA_LATcod <- CA_LATcod %>%
  filter(!(haul.id == "1990:1:LAT:ZBA:LBT:3:12" & LngtClas == 107)) %>%
  filter(!(haul.id == "1978:2:LAT:4576:LBT:4:1" & LngtClas == 83)) %>%
  filter(!(haul.id == "1978:2:LAT:4576:LBT:4:3" & LngtClas == 85)) %>%
  filter(!(haul.id == "1983:2:LAT:ZBA:LBT:4:23" & IndWgt == 7410)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:1:24" & LngtClas == 97)) %>%
  filter(!(haul.id == "1985:1:LAT:ZBA:LBT:1:20" & LngtClas == 52 & IndWgt == 3805)) %>%
  filter(!(haul.id == "1989:1:LAT:ZBA:LBT:3:13" & LngtClas == 47 & IndWgt == 11355)) %>%
  filter(!(haul.id == "1989:1:LAT:ZBA:LBT:1:22" & LngtClas == 49 & IndWgt == 4030)) %>%
  filter(!(haul.id == "1989:1:LAT:ZBA:LBT:3:2" & LngtClas == 66 & IndWgt == 980)) %>%
  filter(!(haul.id == "1989:4:LAT:ZBA:LBT:12:6" & LngtClas == 88 & IndWgt == 2950)) %>%
  filter(!(haul.id == "1982:4:LAT:ZBA:LBT:11:34" & LngtClas == 83 & IndWgt == 400)) %>%
  filter(!(haul.id == "1980:2:LAT:ZBA:LBT:4:25" & LngtClas == 60 & IndWgt == 300)) %>%
  filter(!(haul.id == "1980:2:LAT:ZBA:LBT:4:22" & LngtClas == 54 & IndWgt == 570)) %>%
  filter(!(haul.id == "1982:1:LAT:ZBA:LBT:3:19" & LngtClas == 63 & IndWgt == 4680)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:1:14" & LngtClas == 99 & IndWgt == 560)) %>%
  filter(!(haul.id == "1984:2:LAT:ZBA:LBT:6:16" & LngtClas == 44 & IndWgt == 7300)) %>%
  filter(!(haul.id == "1984:2:LAT:ZBA:LBT:6:16" & LngtClas == 41 & IndWgt == 6100)) %>%
  filter(!(haul.id == "1987:2:LAT:ZBA:LBT:4:26" & LngtClas == 32 & IndWgt == 2700)) %>%
  filter(!(haul.id == "1983:2:LAT:ZBA:LBT:4:11" & LngtClas == 70 & IndWgt == 200)) %>%
  filter(!(haul.id == "1983:2:LAT:ZBA:LBT:4:11" & LngtClas == 4 & IndWgt == 550))





CA_LATflo <- CA_LATflo %>%
  filter(!(haul.id == "1990:1:LAT:ZBA:LBT:3:27" & LngtClas == 28 & IndWgt == 23.6)) %>%
  filter(!(haul.id == "1990:1:LAT:ZBA:LBT:3:14" & LngtClas == 21 & IndWgt == 950)) %>%
  filter(!(haul.id == "1983:1:LAT:ZBA:LBT:3:14" & LngtClas == 23 & IndWgt == 360)) %>%
  filter(!(haul.id == "1986:1:LAT:BPE:LBT:3:23" & LngtClas == 26 & IndWgt == 760)) %>%
  filter(!(haul.id == "1989:1:LAT:ZBA:LBT:3:13" & LngtClas == 27 & IndWgt == 960)) %>%
  filter(!(haul.id == "1989:2:LAT:ZBA:LBT:4:23" & LngtClas == 29 & IndWgt == 32.8)) %>%
  filter(!(haul.id == "1990:1:LAT:ZBA:LBT:3:31" & LngtClas == 28 & IndWgt == 894)) %>%
  filter(!(haul.id == "1981:4:LAT:ZBA:LBT:11:41" & LngtClas == 30 & IndWgt == 715)) %>%
  filter(!(haul.id == "1982:1:LAT:ZBA:LBT:3:31" & LngtClas == 19 & IndWgt == 340)) %>%
  filter(!(haul.id == "1983:2:LAT:ZBA:LBT:4:13" & LngtClas == 29 & IndWgt == 28.6)) %>%
  filter(!(haul.id == "1983:2:LAT:ZBA:LBT:4:13" & LngtClas == 35 & IndWgt == 160)) %>%
  filter(!(haul.id == "1986:2:LAT:BPE:LBT:5:9" & LngtClas == 35 & IndWgt == 46.7)) %>%
  filter(!(haul.id == "1986:2:LAT:BPE:LBT:5:5" & LngtClas == 27 & IndWgt == 533)) %>%
  filter(!(haul.id == "1985:2:LAT:ZBA:LBT:4:27" & LngtClas == 31 & IndWgt == 27.4)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 24 & IndWgt == 620)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 24 & IndWgt == 360)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:45" & LngtClas == 22 & IndWgt == 320)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 26 & IndWgt == 780)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 28 & IndWgt == 770)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 26 & IndWgt == 670)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 26 & IndWgt == 600)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 29 & IndWgt == 540)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 29 & IndWgt == 530)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:45" & LngtClas == 32 & IndWgt == 134)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 35 & IndWgt == 134)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 34 & IndWgt == 200)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 35 & IndWgt == 950)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 35 & IndWgt == 770)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 36 & IndWgt == 250)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 36 & IndWgt == 300)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 38 & IndWgt == 340)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 37 & IndWgt == 370)) %>%
  filter(!(haul.id == "1984:2:LAT:27:LBT:4:23" & LngtClas == 37 & IndWgt == 380)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:45" & LngtClas == 36 & IndWgt == 410)) %>%
  filter(!(haul.id == "1984:2:LAT:27:LBT:4:35" & LngtClas == 36 & IndWgt == 450)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 41 & IndWgt == 200)) %>%
  filter(!(haul.id == "1984:1:LAT:ZBA:LBT:3:47" & LngtClas == 42 & IndWgt == 260)) %>%
  filter(!(haul.id == "1984:2:LAT:27:LBT:4:22" & LngtClas == 42 & IndWgt == 620))







###### create mean condition per year and SD per different length classes and plot it

CA_LATcod1020 <- CA_LATcod %>%
  filter(LngtClas >= 10 & LngtClas <= 19.99) %>%
  mutate(cond = IndWgt / (LngtClas^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = CA_LATcod1020, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("cod 10-20")

CA_LATcod3040 <- CA_LATcod %>%
  filter(LngtClas >= 30 & LngtClas <= 39.99) %>%
  mutate(cond = IndWgt / (LngtClas^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = CA_LATcod3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("cod 30-40")

CA_LATcod5060 <- CA_LATcod %>%
  filter(LngtClas >= 50 & LngtClas <= 59.99) %>%
  mutate(cond = IndWgt / (LngtClas^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = CA_LATcod5060, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("cod 50-60")

CA_LATflo1020 <- CA_LATflo %>%
  filter(LngtClas >= 10 & LngtClas <= 19.99) %>%
  mutate(cond = IndWgt / (LngtClas^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = CA_LATflo1020, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("flounder 10-20")

CA_LATflo2030 <- CA_LATflo %>%
  filter(LngtClas >= 20 & LngtClas <= 29.99) %>%
  mutate(cond = IndWgt / (LngtClas^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = CA_LATflo2030, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("flounder 20-30")

CA_LATflo3040 <- CA_LATflo %>%
  filter(LngtClas >= 30 & LngtClas <= 39.99) %>%
  mutate(cond = IndWgt / (LngtClas^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = CA_LATflo3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("flounder 30-40")


###### merge all the CA

lwcod3 <- CA_LATcod %>%
  filter(Year <= 2016) %>%
  select(Year, SD, LngtClas, IndWgt)

lwflo3 <- CA_LATflo %>%
  filter(Year <= 2016) %>%
  select(Year, SD, LngtClas, IndWgt)

names(lwcod3) <- names(lwcod2)
names(lwflo3) <- names(lwflo2)


lwcodTotal <- rbind(lwcod, lwcod2, lwcod3)
lwfloTotal <- rbind(lwflo, lwflo2, lwflo3)

###### check for outliers

ggplot(data = lwcodTotal, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("cod")
ggplot(data = lwfloTotal, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_wrap(~Year) +
  ggtitle("flounder")

ggplot(data = lwcodTotal, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("cod")
ggplot(data = lwfloTotal, aes(x = LngtCm, y = IndWgt)) +
  geom_point(color = "red", size = 3) +
  facet_grid(SD ~ Year) +
  ggtitle("flounder")

###### create mean condition per year and SD per different length classes and plot it

lwcodTotal1020 <- lwcodTotal %>%
  filter(LngtCm >= 10 & LngtCm <= 19.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = lwcodTotal1020, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("cod 10-20")

lwcodTotal3040 <- lwcodTotal %>%
  filter(LngtCm >= 30 & LngtCm <= 39.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = lwcodTotal3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("cod 30-40")

lwcodTotal5060 <- lwcodTotal %>%
  filter(LngtCm >= 50 & LngtCm <= 59.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = lwcodTotal5060, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("cod 50-60")

lwfloTotal1020 <- lwfloTotal %>%
  filter(LngtCm >= 10 & LngtCm <= 19.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = lwfloTotal1020, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("flounder 10-20")

lwfloTotal2030 <- lwfloTotal %>%
  filter(LngtCm >= 20 & LngtCm <= 29.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = lwfloTotal2030, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("flounder 20-30")

lwfloTotal3040 <- lwfloTotal %>%
  filter(LngtCm >= 30 & LngtCm <= 39.99) %>%
  mutate(cond = IndWgt / (LngtCm^3)) %>%
  group_by(SD, Year) %>%
  summarise(meancond = mean(cond))

ggplot(data = lwfloTotal3040, aes(x = Year, y = meancond, color = SD, fill = SD)) +
  geom_line(size = 1) +
  ggtitle("flounder 30-40")


##### Transform L and W in Log scale
lwcodTotal$logL <- log(lwcodTotal$LngtCm)
lwcodTotal$logW <- log(lwcodTotal$IndWgt)

lwfloTotal$logL <- log(lwfloTotal$LngtCm)
lwfloTotal$logW <- log(lwfloTotal$IndWgt)

lwplaT$logL <- log(lwplaT$LngtCm)
lwplaT$logW <- log(lwplaT$IndWgt)


##### create the index of year SD

idxcod <- levels(factor(lwcodTotal$Year))
idxcod2223 <- levels(factor(lwcodTotal$Year[lwcodTotal$SD %in% c("22", "23")]))
idxflo <- levels(factor(lwfloTotal$Year))
idxpla <- levels(factor(lwplaT$Year))

idxdcod <- as.data.frame(idxcod)
idxdcod2223 <- as.data.frame(idxcod2223)
idxdflo <- as.data.frame(idxflo)
idxdpla <- as.data.frame(idxpla)


###### COD SD 24 on unique

##### run the length weight relationships

mod24on <- lm(logW ~ logL, data = lwcodTotal[!lwcodTotal$SD %in% c("22", "23"), ])
lwcodTotal$auni[!lwcodTotal$SD %in% c("22", "23")] <- mod24on$coefficients[1]
lwcodTotal$buni[!lwcodTotal$SD %in% c("22", "23")] <- mod24on$coefficients[2]

summary(mod24on)

###### COD SD 24 on yearly

##### run the length weight relationships
ab <- list()
for (i in 1:length(idxcod)) {
  ab[[i]] <- lm(logW ~ logL, data = lwcodTotal[lwcodTotal$Year == idxcod[i] & !lwcodTotal$SD %in% c("22", "23"), ])
  idxdcod$a[[i]] <- ab[[i]]$coefficients[1]
  idxdcod$b[[i]] <- ab[[i]]$coefficients[2]
}

#### merge the a and b with the dataset
lwcodT24on <- merge(lwcodTotal[!lwcodTotal$SD %in% c("22", "23"), ], idxdcod, by.x = "Year", by.y = "idxcod", all.x = TRUE)

###### COD SD 22-23 unique

##### run the length weight relationships

mod2223 <- lm(logW ~ logL, data = lwcodTotal[lwcodTotal$SD %in% c("22", "23"), ])
lwcodTotal$auni[lwcodTotal$SD %in% c("22", "23")] <- mod2223$coefficients[1]
lwcodTotal$buni[lwcodTotal$SD %in% c("22", "23")] <- mod2223$coefficients[2]

summary(mod2223)

###### COD SD 22-23 yearly

##### run the length weight relationships
ab <- list()
for (i in 1:length(idxcod2223)) {
  ab[[i]] <- lm(logW ~ logL, data = lwcodTotal[lwcodTotal$Year == idxcod2223[i] & lwcodTotal$SD %in% c("22", "23"), ])
  idxdcod2223$a[[i]] <- ab[[i]]$coefficients[1]
  idxdcod2223$b[[i]] <- ab[[i]]$coefficients[2]
}

#### merge the a and b with the dataset
lwcodT2223 <- merge(lwcodTotal[lwcodTotal$SD %in% c("22", "23"), ], idxdcod2223, by.x = "Year", by.y = "idxcod2223", all.x = TRUE)

#### merge the two cod dataset

lwcodTotal <- rbind(lwcodT24on, lwcodT2223)

##### Cod 22-23 mean of the first 3 years

oldcod2223 <- lwcodTotal %>%
  filter(SD %in% c("22", "23"), Year <= 1994)

mod2223old <- lm(logW ~ logL, data = oldcod2223)
summary(mod2223old)

acod2223old <- exp(mod2223old$coefficients[1])
bcod2223old <- mod2223old$coefficients[2]


##### Cod 24 on mean of the first 3 years

oldcod24on <- lwcodTotal %>%
  filter(!SD %in% c("22", "23"), Year <= 1979)

mod24onold <- lm(logW ~ logL, data = oldcod24on)
summary(mod24onold)

acod24onold <- exp(mod24onold$coefficients[1])
bcod24onold <- mod24onold$coefficients[2]

##### Cod 24 on 1977 mean of 1976 and 1978

cod24on1977 <- lwcodTotal %>%
  filter(!SD %in% c("22", "23"), Year %in% c(1976, 1978))

mod24on1977 <- lm(logW ~ logL, data = cod24on1977)
summary(mod24on1977)

acod24on1977 <- exp(mod24on1977$coefficients[1])
bcod24on1977 <- mod24on1977$coefficients[2]



###### FLOUNDER unique

##### run the length weight relationships

modflo <- lm(logW ~ logL, data = lwfloTotal)
lwfloTotal$auni <- modflo$coefficients[1]
lwfloTotal$buni <- modflo$coefficients[2]

###### FLOUNDER yearly

##### run the length weight relationships
ab <- list()
for (i in 1:length(idxflo)) {
  ab[[i]] <- lm(logW ~ logL, data = lwfloTotal[lwfloTotal$Year == idxflo[i], ])
  idxdflo$a[[i]] <- ab[[i]]$coefficients[1]
  idxdflo$b[[i]] <- ab[[i]]$coefficients[2]
}

#### merge the a and b with the dataset
lwfloTotal <- merge(lwfloTotal, idxdflo, by.x = "Year", by.y = "idxflo", all.x = TRUE)

##### Flounder mean of the first 3 years

oldflo <- lwfloTotal %>%
  filter(Year <= 1980)

modfloold <- lm(logW ~ logL, data = oldflo)
summary(modfloold)

afloold <- exp(modfloold$coefficients[1])
bfloold <- modfloold$coefficients[2]

##### Flounder 1977-78 mean of 1976 and 1979

flo7778 <- lwfloTotal %>%
  filter(Year %in% c(1976, 1979))

modflo7778 <- lm(logW ~ logL, data = flo7778)
summary(modflo7778)

aflo7778 <- exp(modflo7778$coefficients[1])
bflo7778 <- modflo7778$coefficients[2]


##### Flounder 1987 mean of 1986 and 1988

flo87 <- lwfloTotal %>%
  filter(Year %in% c(1986, 1988))

modflo87 <- lm(logW ~ logL, data = flo87)
summary(modflo87)

aflo87 <- exp(modflo87$coefficients[1])
bflo87 <- modflo87$coefficients[2]

##### Flounder 1991 mean of 1990 and 1992

flo91 <- lwfloTotal %>%
  filter(Year %in% c(1990, 1992))

modflo91 <- lm(logW ~ logL, data = flo91)
summary(modflo91)

aflo91 <- exp(modflo91$coefficients[1])
bflo91 <- modflo91$coefficients[2]

###### PLAICE unique

##### run the length weight relationships
modpla <- lm(logW ~ logL, data = lwplaT)
lwplaT$auni <- modpla$coefficients[1]
lwplaT$buni <- modpla$coefficients[2]

###### PLAICE yearly

##### run the length weight relationships
ab <- list()
for (i in 1:length(idxpla)) {
  ab[[i]] <- lm(logW ~ logL, data = lwplaT[lwplaT$Year == idxpla[i], ])
  idxdpla$a[[i]] <- ab[[i]]$coefficients[1]
  idxdpla$b[[i]] <- ab[[i]]$coefficients[2]
}

#### merge the a and b with the dataset
lwplaT <- merge(lwplaT, idxdpla, by.x = "Year", by.y = "idxpla", all.x = TRUE)

##### Plaice mean of the first 3 years

oldpla <- lwplaT %>%
  filter(Year <= 1996)

modplaold <- lm(logW ~ logL, data = oldpla)
summary(modplaold)

aplaold <- exp(modplaold$coefficients[1])
bplaold <- modplaold$coefficients[2]

############################### transform all the a coefficients back to linear scale

lwcodTotal$auniL <- exp(lwcodTotal$auni)
lwcodTotal$aL <- exp(lwcodTotal$a)
lwfloTotal$auniL <- exp(lwfloTotal$auni)
lwfloTotal$aL <- exp(lwfloTotal$a)
lwplaT$auniL <- exp(lwplaT$auni)
lwplaT$aL <- exp(lwplaT$a)


########### create a dataframe with the a and b

lwcodTotal$SDgroup[lwcodTotal$SD %in% c("22", "23")] <- "2223"
lwcodTotal$SDgroup[!lwcodTotal$SD %in% c("22", "23")] <- "24on"


lwCOD <- unique(lwcodTotal[, c("Year", "SDgroup", "auniL", "buni", "aL", "b")])

lwFLO <- unique(lwfloTotal[, c("Year", "auniL", "buni", "aL", "b")])

lwPLA <- unique(lwplaT[, c("Year", "auniL", "buni", "aL", "b")])


##### Calculate estimated weight and convert CPUE in Biomass

##### cod hlcodL

hlcodL <- hlcodL %>%
  filter(Year <= 2016)

hlcodL$SDgroup[hlcodL$SD %in% c("22", "23")] <- "2223"
hlcodL$SDgroup[!hlcodL$SD %in% c("22", "23")] <- "24on"

hlcodL <- merge(hlcodL, lwCOD, by = c("Year", "SDgroup"), all.x = TRUE)
summary(hlcodL)

summary(hlcodL[hlcodL$SDgroup == "2223" & is.na(hlcodL$aL), ])

hlcodL$aL[hlcodL$SDgroup == "2223" & is.na(hlcodL$aL)] <- acod2223old
hlcodL$auniL[hlcodL$SDgroup == "2223" & is.na(hlcodL$auniL)] <- acod2223old
hlcodL$b[hlcodL$SDgroup == "2223" & is.na(hlcodL$b)] <- bcod2223old
hlcodL$buni[hlcodL$SDgroup == "2223" & is.na(hlcodL$buni)] <- bcod2223old



hlcodL$estWuni <- hlcodL$auniL * ((hlcodL$LngtCm)^hlcodL$buni)
hlcodL$estWyear <- hlcodL$aL * ((hlcodL$LngtCm)^hlcodL$b)

hlcodL$CPUEunBIOuni <- hlcodL$CPUEun * hlcodL$estWuni
hlcodL$CPUEunBIOyear <- hlcodL$CPUEun * hlcodL$estWyear

hlcodL$CPUEunBIOuni[hlcodL$SpecVal == "zeroCatch"] <- 0
hlcodL$CPUEunBIOyear[hlcodL$SpecVal == "zeroCatch"] <- 0


##### flounder hlfloL

hlfloL <- hlfloL %>%
  filter(Year <= 2016)

hlfloL <- merge(hlfloL, lwFLO, by = "Year", all.x = TRUE)
summary(hlfloL)

summary(hlfloL[is.na(hlfloL$aL), ])

hlfloL$aL[is.na(hlfloL$aL)] <- aflo91
hlfloL$auniL[is.na(hlfloL$auniL)] <- aflo91
hlfloL$b[is.na(hlfloL$b)] <- bflo91
hlfloL$buni[is.na(hlfloL$buni)] <- bflo91


hlfloL$estWuni <- hlfloL$auniL * ((hlfloL$LngtCm)^hlfloL$buni)
hlfloL$estWyear <- hlfloL$aL * ((hlfloL$LngtCm)^hlfloL$b)

hlfloL$CPUEunBIOuni <- hlfloL$CPUEun * hlfloL$estWuni
hlfloL$CPUEunBIOyear <- hlfloL$CPUEun * hlfloL$estWyear

hlfloL$CPUEunBIOuni[hlfloL$SpecVal == "zeroCatch"] <- 0
hlfloL$CPUEunBIOyear[hlfloL$SpecVal == "zeroCatch"] <- 0


##### plaice hlplaL

hlplaL <- hlplaL %>%
  filter(Year <= 2016)

hlplaL <- merge(hlplaL, lwPLA, by = "Year", all.x = TRUE)
summary(hlplaL)

summary(hlplaL[is.na(hlplaL$aL), ])

hlplaL$aL[is.na(hlplaL$aL)] <- aplaold
hlplaL$auniL[is.na(hlplaL$auniL)] <- aplaold
hlplaL$b[is.na(hlplaL$b)] <- bplaold
hlplaL$buni[is.na(hlplaL$buni)] <- bplaold

hlplaL$estWuni <- hlplaL$auniL * ((hlplaL$LngtCm)^hlplaL$buni)
hlplaL$estWyear <- hlplaL$aL * ((hlplaL$LngtCm)^hlplaL$b)

hlplaL$CPUEunBIOuni <- hlplaL$CPUEun * hlplaL$estWuni
hlplaL$CPUEunBIOyear <- hlplaL$CPUEun * hlplaL$estWyear

hlplaL$CPUEunBIOuni[hlplaL$SpecVal == "zeroCatch"] <- 0
hlplaL$CPUEunBIOyear[hlplaL$SpecVal == "zeroCatch"] <- 0

###### cod DataStuart (surveys script has to be run before)

codswe <- DataStuart[DataStuart$species == "Gadus morhua", ]


codswe$SDgroup[codswe$SD %in% c("22", "23")] <- "2223"
codswe$SDgroup[!codswe$SD %in% c("22", "23")] <- "24on"


codswe <- merge(codswe, lwCOD, by.x = c("year", "SDgroup"), by.y = c("Year", "SDgroup"), all.x = TRUE)
summary(codswe)

summary(codswe[codswe$SDgroup == "2223" & is.na(codswe$aL), ])

codswe$aL[codswe$SDgroup == "2223" & is.na(codswe$aL)] <- acod2223old
codswe$auniL[codswe$SDgroup == "2223" & is.na(codswe$auniL)] <- acod2223old
codswe$b[codswe$SDgroup == "2223" & is.na(codswe$b)] <- bcod2223old
codswe$buni[codswe$SDgroup == "2223" & is.na(codswe$buni)] <- bcod2223old

summary(codswe[codswe$SDgroup == "24on" & is.na(codswe$aL), ])

codswe$aL[codswe$SDgroup == "24on" & is.na(codswe$aL)] <- acod24onold
codswe$auniL[codswe$SDgroup == "24on" & is.na(codswe$auniL)] <- acod24onold
codswe$b[codswe$SDgroup == "24on" & is.na(codswe$b)] <- bcod24onold
codswe$buni[codswe$SDgroup == "24on" & is.na(codswe$buni)] <- bcod24onold


codswe$estWuni <- codswe$auniL * ((codswe$Lengthcl)^codswe$buni)
codswe$estWyear <- codswe$aL * ((codswe$Lengthcl)^codswe$b)

codswe$CPUEunBIOuni <- codswe$CPUEun * codswe$estWuni
codswe$CPUEunBIOyear <- codswe$CPUEun * codswe$estWyear

codswe$CPUEstBIOuni <- codswe$CPUEst * codswe$estWuni
codswe$CPUEstBIOyear <- codswe$CPUEst * codswe$estWyear

######## change NAs in CPUEs with 0s

codswe$CPUEunBIOuni[is.na(codswe$CPUEunBIOuni)] <- 0
codswe$CPUEunBIOyear[is.na(codswe$CPUEunBIOyear)] <- 0
codswe$CPUEstBIOuni[is.na(codswe$CPUEstBIOuni)] <- 0
codswe$CPUEstBIOyear[is.na(codswe$CPUEstBIOyear)] <- 0


###### flounder DataStuart

floswe <- DataStuart[DataStuart$species == "Platichthys flesus", ]

floswe <- merge(floswe, lwFLO, by.x = "year", by.y = "Year", all.x = TRUE)
summary(floswe)

summary(floswe[is.na(floswe$aL), ])

floswe$aL[is.na(floswe$aL) & floswe$year == "1991"] <- aflo91
floswe$auniL[is.na(floswe$auniL) & floswe$year == "1991"] <- aflo91
floswe$b[is.na(floswe$b) & floswe$year == "1991"] <- bflo91
floswe$buni[is.na(floswe$buni) & floswe$year == "1991"] <- bflo91

floswe$aL[is.na(floswe$aL)] <- afloold
floswe$auniL[is.na(floswe$auniL)] <- afloold
floswe$b[is.na(floswe$b)] <- bfloold
floswe$buni[is.na(floswe$buni)] <- bfloold

floswe$estWuni <- floswe$auniL * ((floswe$Lengthcl)^floswe$buni)
floswe$estWyear <- floswe$aL * ((floswe$Lengthcl)^floswe$b)

floswe$CPUEunBIOuni <- floswe$CPUEun * floswe$estWuni
floswe$CPUEunBIOyear <- floswe$CPUEun * floswe$estWyear

floswe$CPUEstBIOuni <- floswe$CPUEst * floswe$estWuni
floswe$CPUEstBIOyear <- floswe$CPUEst * floswe$estWyear


######## change NAs in CPUEs with 0s

floswe$CPUEunBIOuni[is.na(floswe$CPUEunBIOuni)] <- 0
floswe$CPUEunBIOyear[is.na(floswe$CPUEunBIOyear)] <- 0
floswe$CPUEstBIOuni[is.na(floswe$CPUEstBIOuni)] <- 0
floswe$CPUEstBIOyear[is.na(floswe$CPUEstBIOyear)] <- 0

###### plaice DataStuart

plaswe <- DataStuart[DataStuart$species == "Pleuronectes platessa", ]

plaswe <- merge(plaswe, lwPLA, by.x = "year", by.y = "Year", all.x = TRUE)
summary(plaswe)

summary(plaswe[is.na(plaswe$aL), ])

plaswe$aL[is.na(plaswe$aL)] <- aplaold
plaswe$auniL[is.na(plaswe$auniL)] <- aplaold
plaswe$b[is.na(plaswe$b)] <- bplaold
plaswe$buni[is.na(plaswe$buni)] <- bplaold


plaswe$estWuni <- plaswe$auniL * ((plaswe$Lengthcl)^plaswe$buni)
plaswe$estWyear <- plaswe$aL * ((plaswe$Lengthcl)^plaswe$b)

plaswe$CPUEunBIOuni <- plaswe$CPUEun * plaswe$estWuni
plaswe$CPUEunBIOyear <- plaswe$CPUEun * plaswe$estWyear

plaswe$CPUEstBIOuni <- plaswe$CPUEst * plaswe$estWuni
plaswe$CPUEstBIOyear <- plaswe$CPUEst * plaswe$estWyear

######## change NAs in CPUEs with 0s

plaswe$CPUEunBIOuni[is.na(plaswe$CPUEunBIOuni)] <- 0
plaswe$CPUEunBIOyear[is.na(plaswe$CPUEunBIOyear)] <- 0
plaswe$CPUEstBIOuni[is.na(plaswe$CPUEstBIOuni)] <- 0
plaswe$CPUEstBIOyear[is.na(plaswe$CPUEstBIOyear)] <- 0


##### cod HL_LATcodL

HL_LATcodL <- HL_LATcodL %>%
  filter(Year <= 2016)

HL_LATcodL$SDgroup[HL_LATcodL$SD %in% c("22", "23")] <- "2223"
HL_LATcodL$SDgroup[!HL_LATcodL$SD %in% c("22", "23")] <- "24on"

HL_LATcodL <- merge(HL_LATcodL, lwCOD, by = c("Year", "SDgroup"), all.x = TRUE)
summary(HL_LATcodL)

summary(HL_LATcodL[HL_LATcodL$SDgroup == "2223" & is.na(HL_LATcodL$aL), ])

HL_LATcodL$aL[HL_LATcodL$SDgroup == "2223" & is.na(HL_LATcodL$aL)] <- acod2223old
HL_LATcodL$auniL[HL_LATcodL$SDgroup == "2223" & is.na(HL_LATcodL$auniL)] <- acod2223old
HL_LATcodL$b[HL_LATcodL$SDgroup == "2223" & is.na(HL_LATcodL$b)] <- bcod2223old
HL_LATcodL$buni[HL_LATcodL$SDgroup == "2223" & is.na(HL_LATcodL$buni)] <- bcod2223old

summary(HL_LATcodL[HL_LATcodL$SDgroup == "24on" & is.na(HL_LATcodL$aL), ])

HL_LATcodL$aL[HL_LATcodL$SDgroup == "24on" & is.na(HL_LATcodL$aL)] <- acod2223old
HL_LATcodL$auniL[HL_LATcodL$SDgroup == "24on" & is.na(HL_LATcodL$auniL)] <- acod2223old
HL_LATcodL$b[HL_LATcodL$SDgroup == "24on" & is.na(HL_LATcodL$b)] <- bcod2223old
HL_LATcodL$buni[HL_LATcodL$SDgroup == "24on" & is.na(HL_LATcodL$buni)] <- bcod2223old




HL_LATcodL$estWuni <- HL_LATcodL$auniL * ((HL_LATcodL$LngtClas)^HL_LATcodL$buni)
HL_LATcodL$estWyear <- HL_LATcodL$aL * ((HL_LATcodL$LngtClas)^HL_LATcodL$b)

HL_LATcodL$CPUEunBIOuni <- HL_LATcodL$CPUEun * HL_LATcodL$estWuni
HL_LATcodL$CPUEunBIOyear <- HL_LATcodL$CPUEun * HL_LATcodL$estWyear

HL_LATcodL$CPUEunBIOuni[HL_LATcodL$SpecVal == "zeroCatch"] <- 0
HL_LATcodL$CPUEunBIOyear[HL_LATcodL$SpecVal == "zeroCatch"] <- 0


##### flounder HL_LATfloL

HL_LATfloL <- HL_LATfloL %>%
  filter(Year <= 2016)

HL_LATfloL <- merge(HL_LATfloL, lwFLO, by = "Year", all.x = TRUE)
summary(HL_LATfloL)

summary(HL_LATfloL[is.na(HL_LATfloL$aL), ])

HL_LATfloL$aL[is.na(HL_LATfloL$aL) & HL_LATfloL$Year == "1978"] <- aflo7778
HL_LATfloL$auniL[is.na(HL_LATfloL$auniL) & HL_LATfloL$Year == "1978"] <- aflo7778
HL_LATfloL$b[is.na(HL_LATfloL$b) & HL_LATfloL$Year == "1978"] <- bflo7778
HL_LATfloL$buni[is.na(HL_LATfloL$buni) & HL_LATfloL$Year == "1978"] <- bflo7778

HL_LATfloL$aL[is.na(HL_LATfloL$aL) & HL_LATfloL$Year == "1987"] <- aflo87
HL_LATfloL$auniL[is.na(HL_LATfloL$auniL) & HL_LATfloL$Year == "1987"] <- aflo87
HL_LATfloL$b[is.na(HL_LATfloL$b) & HL_LATfloL$Year == "1987"] <- bflo87
HL_LATfloL$buni[is.na(HL_LATfloL$buni) & HL_LATfloL$Year == "1987"] <- bflo87

HL_LATfloL$estWuni <- HL_LATfloL$auniL * ((HL_LATfloL$LngtClas)^HL_LATfloL$buni)
HL_LATfloL$estWyear <- HL_LATfloL$aL * ((HL_LATfloL$LngtClas)^HL_LATfloL$b)

HL_LATfloL$CPUEunBIOuni <- HL_LATfloL$CPUEun * HL_LATfloL$estWuni
HL_LATfloL$CPUEunBIOyear <- HL_LATfloL$CPUEun * HL_LATfloL$estWyear

HL_LATfloL$CPUEunBIOuni[HL_LATfloL$SpecVal == "zeroCatch"] <- 0
HL_LATfloL$CPUEunBIOyear[HL_LATfloL$SpecVal == "zeroCatch"] <- 0



##### create the date in HHdata and HH_LAT

HHdata$date <- paste(HHdata$Year, HHdata$Month, HHdata$Day, sep = "-")
HH_LAT$date <- paste(HH_LAT$Year, HH_LAT$month, HH_LAT$Day, sep = "-")


##### read the sweep file with gear and speed info

newsweep <- read.csv("New/Datras/sweep_9116.csv", sep = ";")

sweep_LAT <- read.csv("New/Datras/Historic Latvian/sweepLatvia.csv", sep = ";")


######## merge Datras with Sweden and Latvia

##### cod

names(hlcodL)

COD <- hlcodL[, c("Year", "SDgroup", "Ship", "HaulDur", "SD", "haul.id", "LngtCm", "CPUEun", "Species", "Rect", "HaulVal", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear")]

#### match missing information

COD$date <- factor(HHdata[match(COD$haul.id, HHdata$haul.id), "date"])
COD$speed <- newsweep[match(COD$haul.id, newsweep$haul.id), "GroundSpeed"]
COD$LATDD <- HHdata[match(COD$haul.id, HHdata$haul.id), "lat"]
COD$LONGDD <- HHdata[match(COD$haul.id, HHdata$haul.id), "lon"]
COD$Depth <- HHdata[match(COD$haul.id, HHdata$haul.id), "Depth"]
COD$RSA <- newsweep[match(COD$haul.id, newsweep$haul.id), "RSA"]
COD$RS <- newsweep[match(COD$haul.id, newsweep$haul.id), "RS"]
COD$CPUEst <- COD$CPUEun * COD$RSA * COD$RS
COD$CPUEstBIOuni <- COD$CPUEunBIOuni * COD$RSA * COD$RS
COD$CPUEstBIOyear <- COD$CPUEunBIOyear * COD$RSA * COD$RS

summary(COD)

names(COD) <- c("year", "SDgroup", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst", "CPUEstBIOuni", "CPUEstBIOyear")



names(HL_LATcodL)

COD_LAT <- HL_LATcodL[, c("Year", "SDgroup", "Ship", "HaulDur", "SD", "haul.id", "LngtClass", "CPUEun", "Species", "Rect", "HaulVal", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear")]

#### match missing information

COD_LAT$date <- factor(HH_LAT[match(COD_LAT$haul.id, HH_LAT$haul.id), "date"])
COD_LAT$speed <- sweep_LAT[match(COD_LAT$haul.id, sweep_LAT$haul.id), "GroundSpeed"]
COD_LAT$LATDD <- HH_LAT[match(COD_LAT$haul.id, HH_LAT$haul.id), "lat"]
COD_LAT$LONGDD <- HH_LAT[match(COD_LAT$haul.id, HH_LAT$haul.id), "lon"]
COD_LAT$Depth <- HH_LAT[match(COD_LAT$haul.id, HH_LAT$haul.id), "Depth"]
COD_LAT$RSA <- sweep_LAT[match(COD_LAT$haul.id, sweep_LAT$haul.id), "RSA"]
COD_LAT$RS <- sweep_LAT[match(COD_LAT$haul.id, sweep_LAT$haul.id), "RS"]
COD_LAT$CPUEst <- COD_LAT$CPUEun * COD_LAT$RSA * COD_LAT$RS
COD_LAT$CPUEstBIOuni <- COD_LAT$CPUEunBIOuni * COD_LAT$RSA * COD_LAT$RS
COD_LAT$CPUEstBIOyear <- COD_LAT$CPUEunBIOyear * COD_LAT$RSA * COD_LAT$RS

summary(COD_LAT)

names(COD_LAT) <- c("year", "SDgroup", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst", "CPUEstBIOuni", "CPUEstBIOyear")


COD2 <- merge(COD, codswe, all = T)
COD2 <- merge(COD2, COD_LAT, all = T)

COD2$CPUEst[COD2$CPUEun == 0] <- 0
COD2$CPUEstBIOuni[COD2$CPUEun == 0] <- 0
COD2$CPUEstBIOyear[COD2$CPUEun == 0] <- 0

### remove rows with no standardized CPUEs

COD2 <- COD2[!is.na(COD2$CPUEst), ]

##### flounder

names(hlfloL)

FLO <- hlfloL[, c("Year", "Ship", "HaulDur", "SD", "haul.id", "LngtCm", "CPUEun", "Species", "Rect", "HaulVal", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear")]

#### match missing information

FLO$date <- factor(HHdata[match(FLO$haul.id, HHdata$haul.id), "date"])
FLO$speed <- newsweep[match(FLO$haul.id, newsweep$haul.id), "GroundSpeed"]
FLO$LATDD <- HHdata[match(FLO$haul.id, HHdata$haul.id), "lat"]
FLO$LONGDD <- HHdata[match(FLO$haul.id, HHdata$haul.id), "lon"]
FLO$Depth <- HHdata[match(FLO$haul.id, HHdata$haul.id), "Depth"]
FLO$RSA <- newsweep[match(FLO$haul.id, newsweep$haul.id), "RSA"]
FLO$RS <- newsweep[match(FLO$haul.id, newsweep$haul.id), "RS"]
FLO$CPUEst <- FLO$CPUEun * FLO$RSA * FLO$RS
FLO$CPUEstBIOuni <- FLO$CPUEunBIOuni * FLO$RSA * FLO$RS
FLO$CPUEstBIOyear <- FLO$CPUEunBIOyear * FLO$RSA * FLO$RS

summary(FLO)

names(FLO) <- c("year", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst", "CPUEstBIOuni", "CPUEstBIOyear")


names(HL_LATfloL)

FLO_LAT <- HL_LATfloL[, c("Year", "Ship", "HaulDur", "SD", "haul.id", "LngtClass", "CPUEun", "Species", "Rect", "HaulVal", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear")]


#### match missing information

FLO_LAT$date <- factor(HH_LAT[match(FLO_LAT$haul.id, HH_LAT$haul.id), "date"])
FLO_LAT$speed <- sweep_LAT[match(FLO_LAT$haul.id, sweep_LAT$haul.id), "GroundSpeed"]
FLO_LAT$LATDD <- HH_LAT[match(FLO_LAT$haul.id, HH_LAT$haul.id), "lat"]
FLO_LAT$LONGDD <- HH_LAT[match(FLO_LAT$haul.id, HH_LAT$haul.id), "lon"]
FLO_LAT$Depth <- HH_LAT[match(FLO_LAT$haul.id, HH_LAT$haul.id), "Depth"]
FLO_LAT$RSA <- sweep_LAT[match(FLO_LAT$haul.id, sweep_LAT$haul.id), "RSA"]
FLO_LAT$RS <- sweep_LAT[match(FLO_LAT$haul.id, sweep_LAT$haul.id), "RS"]
FLO_LAT$CPUEst <- FLO_LAT$CPUEun * FLO_LAT$RSA * FLO_LAT$RS
FLO_LAT$CPUEstBIOuni <- FLO_LAT$CPUEunBIOuni * FLO_LAT$RSA * FLO_LAT$RS
FLO_LAT$CPUEstBIOyear <- FLO_LAT$CPUEunBIOyear * FLO_LAT$RSA * FLO_LAT$RS

summary(FLO_LAT)

names(FLO_LAT) <- c("year", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst", "CPUEstBIOuni", "CPUEstBIOyear")


FLO2 <- merge(FLO, floswe, all = T)
FLO2 <- merge(FLO2, FLO_LAT, all = T)


FLO2$CPUEst[FLO2$CPUEun == 0] <- 0
FLO2$CPUEstBIOuni[FLO2$CPUEun == 0] <- 0
FLO2$CPUEstBIOyear[FLO2$CPUEun == 0] <- 0

### remove rows with no standardized CPUEs

FLO2 <- FLO2[!is.na(FLO2$CPUEst), ]

##### plaice

names(hlplaL)

PLA <- hlplaL[, c("Year", "Ship", "HaulDur", "SD", "haul.id", "LngtCm", "CPUEun", "Species", "Rect", "HaulVal", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear")]

#### match missing information

PLA$date <- factor(HHdata[match(PLA$haul.id, HHdata$haul.id), "date"])
PLA$speed <- newsweep[match(PLA$haul.id, newsweep$haul.id), "GroundSpeed"]
PLA$LATDD <- HHdata[match(PLA$haul.id, HHdata$haul.id), "lat"]
PLA$LONGDD <- HHdata[match(PLA$haul.id, HHdata$haul.id), "lon"]
PLA$Depth <- HHdata[match(PLA$haul.id, HHdata$haul.id), "Depth"]
PLA$RSA <- newsweep[match(PLA$haul.id, newsweep$haul.id), "RSA"]
PLA$RS <- newsweep[match(PLA$haul.id, newsweep$haul.id), "RS"]
PLA$CPUEst <- PLA$CPUEun * PLA$RSA * PLA$RS
PLA$CPUEstBIOuni <- PLA$CPUEunBIOuni * PLA$RSA * PLA$RS
PLA$CPUEstBIOyear <- PLA$CPUEunBIOyear * PLA$RSA * PLA$RS

summary(PLA)

names(PLA) <- c("year", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "auniL", "buni", "aL", "b", "estWuni", "estWyear", "CPUEunBIOuni", "CPUEunBIOyear", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst", "CPUEstBIOuni", "CPUEstBIOyear")

PLA <- merge(PLA, plaswe, all = T)

PLA$CPUEst[PLA$CPUEun == 0] <- 0
PLA$CPUEstBIOuni[PLA$CPUEun == 0] <- 0
PLA$CPUEstBIOyear[PLA$CPUEun == 0] <- 0

### remove rows with no standardized CPUEs

PLA <- PLA[!is.na(PLA$CPUEst), ]

##### herring

names(hlherL)

hlherL <- hlherL %>%
  filter(Year <= 2016)

HER <- hlherL[, c("Year", "Ship", "HaulDur", "SD", "haul.id", "LngtCm", "CPUEun", "Species", "Rect", "HaulVal")]

#### match missing information

HER$date <- factor(HHdata[match(HER$haul.id, HHdata$haul.id), "date"])
HER$speed <- newsweep[match(HER$haul.id, newsweep$haul.id), "GroundSpeed"]
HER$LATDD <- HHdata[match(HER$haul.id, HHdata$haul.id), "lat"]
HER$LONGDD <- HHdata[match(HER$haul.id, HHdata$haul.id), "lon"]
HER$Depth <- HHdata[match(HER$haul.id, HHdata$haul.id), "Depth"]
HER$RSA <- newsweep[match(HER$haul.id, newsweep$haul.id), "RSA"]
HER$RS <- newsweep[match(HER$haul.id, newsweep$haul.id), "RS"]
HER$CPUEst <- HER$CPUEun * HER$RSA * HER$RS


summary(HER)

names(HER) <- c("year", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst")


herswe <- DataStuart[DataStuart$species == "Clupea harengus", ]

HER <- merge(HER, herswe, all = T)

HER$CPUEst[HER$CPUEun == 0] <- 0

#### there are a few records in the Swedish data that have length but no numbers. I will remove them

HER <- HER %>%
  filter(!(IDX == "SEPI.1990.66.1990-03-08" & CPUEst == 0)) %>%
  filter(!(IDX == "SEPI.1990.93.1990-03-19" & CPUEst == 0))

### remove rows with no standardized CPUEs

HER <- HER[!is.na(HER$CPUEst), ]

##### sprat

names(hlsprL)

hlsprL <- hlsprL %>%
  filter(Year <= 2016)

SPR <- hlsprL[, c("Year", "Ship", "HaulDur", "SD", "haul.id", "LngtCm", "CPUEun", "Species", "Rect", "HaulVal")]

#### match missing information

SPR$date <- factor(HHdata[match(SPR$haul.id, HHdata$haul.id), "date"])
SPR$speed <- newsweep[match(SPR$haul.id, newsweep$haul.id), "GroundSpeed"]
SPR$LATDD <- HHdata[match(SPR$haul.id, HHdata$haul.id), "lat"]
SPR$LONGDD <- HHdata[match(SPR$haul.id, HHdata$haul.id), "lon"]
SPR$Depth <- HHdata[match(SPR$haul.id, HHdata$haul.id), "Depth"]
SPR$RSA <- newsweep[match(SPR$haul.id, newsweep$haul.id), "RSA"]
SPR$RS <- newsweep[match(SPR$haul.id, newsweep$haul.id), "RS"]
SPR$CPUEst <- SPR$CPUEun * SPR$RSA * SPR$RS

summary(SPR)

names(SPR) <- c("year", "vessel", "trawl_time", "SD", "IDX", "Lengthcl", "CPUEun", "species", "Rect", "validity", "date", "speed", "LATDD", "LONGDD", "Depth", "RSA", "RS", "CPUEst")

sprswe <- DataStuart[DataStuart$species == "Sprattus sprattus", ]

SPR <- merge(SPR, sprswe, all = T)

SPR$CPUEst[SPR$CPUEun == 0] <- 0

### remove rows with no standardized CPUEs

SPR <- SPR[!is.na(SPR$CPUEst), ]

###### remove hauls done with gears PEL and LPT

pelagic <- c("1993:2:LAT:BPE:LPT:000001:30", "1993:2:LAT:BPE:LPT:000001:31", "1993:2:LAT:BPE:LPT:000001:32", "1993:2:LAT:BPE:LPT:000001:33", "1993:2:LAT:BPE:LPT:000001:34", "1993:2:LAT:BPE:LPT:000001:35", "2008:4:POL:BAL:PEL:26093:26", "2008:4:POL:BAL:PEL:26130:25")

COD2 <- COD2[!COD2$IDX %in% pelagic, ]
FLO2 <- FLO2[!FLO2$IDX %in% pelagic, ]
PLA <- PLA[!PLA$IDX %in% pelagic, ]
HER <- HER[!HER$IDX %in% pelagic, ]
SPR <- SPR[!SPR$IDX %in% pelagic, ]

##### create a dataframe with haul position

codhauls <- COD2 %>%
  select(IDX, LATDD, LONGDD, year, vessel, trawl_time, SD, Rect, validity, date, speed, Depth, RSA, RS) %>%
  distinct(IDX, .keep_all = TRUE)

flohauls <- FLO2 %>%
  select(IDX, LATDD, LONGDD, year, vessel, trawl_time, SD, Rect, validity, date, speed, Depth, RSA, RS) %>%
  distinct(IDX, .keep_all = TRUE)

plahauls <- PLA %>%
  select(IDX, LATDD, LONGDD, year, vessel, trawl_time, SD, Rect, validity, date, speed, Depth, RSA, RS) %>%
  distinct(IDX, .keep_all = TRUE)

herhauls <- HER %>%
  select(IDX, LATDD, LONGDD, year, vessel, trawl_time, SD, Rect, validity, date, speed, Depth, RSA, RS) %>%
  distinct(IDX, .keep_all = TRUE)

sprhauls <- SPR %>%
  select(IDX, LATDD, LONGDD, year, vessel, trawl_time, SD, Rect, validity, date, speed, Depth, RSA, RS) %>%
  distinct(IDX, .keep_all = TRUE)

# write.csv(hauls,"hauls.csv")


####### new data Stuart


DataStuart3 <- rbind(COD2[, names(HER)], FLO2[, names(HER)], HER, SPR, PLA[, names(HER)])

summary(DataStuart3)

DataStuart3$species <- factor(DataStuart3$species)



# write.csv(DataStuart3,"DataStuartFullLat16.csv")



######## flounder with no LFD
#### match missing info

HL_LATflo4$LATDD <- HH_LAT[match(HL_LATflo4$haul.id, HH_LAT$haul.id), "lat"]
HL_LATflo4$LONGDD <- HH_LAT[match(HL_LATflo4$haul.id, HH_LAT$haul.id), "lon"]
HL_LATflo4$year <- HL_LATflo4$Year
HL_LATflo4$date <- HH_LAT[match(HL_LATflo4$haul.id, HH_LAT$haul.id), "date"]
HL_LATflo4$Depth <- HH_LAT[match(HL_LATflo4$haul.id, HH_LAT$haul.id), "Depth"]
HL_LATflo4$RSA <- sweep_LAT[match(HL_LATflo4$haul.id, sweep_LAT$haul.id), "RSA"]
HL_LATflo4$RS <- sweep_LAT[match(HL_LATflo4$haul.id, sweep_LAT$haul.id), "RS"]





hlflo4$LATDD <- HHdata[match(hlflo4$haul.id, HHdata$haul.id), "lat"]
hlflo4$LONGDD <- HHdata[match(hlflo4$haul.id, HHdata$haul.id), "lon"]

hlflo4$date <- factor(HHdata[match(hlflo4$haul.id, HHdata$haul.id), "date"])
hlflo4$Depth <- HHdata[match(hlflo4$haul.id, HHdata$haul.id), "Depth"]
hlflo4$RSA <- newsweep[match(hlflo4$haul.id, newsweep$haul.id), "RSA"]
hlflo4$RS <- newsweep[match(hlflo4$haul.id, newsweep$haul.id), "RS"]


##### merge them
names(HL_LATflo4)
names(hlflo4)

flonoLFD <- merge(HL_LATflo4, hlflo4, by = intersect(names(HL_LATflo4), names(hlflo4)), all = TRUE)


flonoLFD <- flonoLFD %>%
  filter(!(is.na(RSA)) & SD %in% c("24", "25", "26", "27", "28-2") & !(is.na(CatCatchWgt)) & !CatCatchWgt <= 100 & !CatCatchWgt >= 500000)


flonoLFD$CPUEst <- flonoLFD$CatCatchWgt * flonoLFD$RSA * flonoLFD$RS

summary(flonoLFD)





###### FLO >20

FLO20on <- FLO2 %>%
  filter(Lengthcl >= 20) %>%
  group_by(IDX) %>%
  summarise(totalCPUEst = sum(CPUEstBIOyear))

FLO20on <- merge(FLO20on, flohauls, by = "IDX", all = T)

FLO20on$totalCPUEst[is.na(FLO20on$totalCPUEst)] <- 0

summary(FLO20on)

FLO20on$col <- cut(FLO20on$totalCPUEst, breaks = c(-Inf, 0, Inf), labels = c("0", ">0"))




##### FLO <20

FLOless20 <- FLO2 %>%
  filter(Lengthcl <= 19.9) %>%
  group_by(IDX) %>%
  summarise(totalCPUEst2 = sum(CPUEstBIOyear))

FLOless20 <- merge(FLOless20, flohauls, by = "IDX", all = T)

FLOless20$totalCPUEst2[is.na(FLOless20$totalCPUEst2)] <- 0

summary(FLOless20)

FLOless20$col <- cut(FLOless20$totalCPUEst2, breaks = c(-Inf, 0, Inf), labels = c("0", ">0"))



##### FLO Total

FLOtotal <- FLO2 %>%
  group_by(IDX) %>%
  summarise(totalCPUEst3 = sum(CPUEstBIOyear))

FLOtotal <- merge(FLOtotal, flohauls, by = "IDX", all = T)

FLOtotal$totalCPUEst3[is.na(FLOtotal$totalCPUEst3)] <- 0

summary(FLOtotal)

FLOtotal$col <- cut(FLOtotal$totalCPUEst3, breaks = c(-Inf, 0, Inf), labels = c("0", ">0"))



FLOtotal <- merge(FLOtotal, FLOless20, all.x = TRUE)
FLOtotal <- merge(FLOtotal, FLO20on, all.x = TRUE)



FLOtotal$rapp <- (FLOtotal$totalCPUEst2 / FLOtotal$totalCPUEst3) * 100


summary(FLOtotal[FLOtotal$year %in% c(1978:1990) & FLOtotal$SD %in% c("26", "28-2"), ])
summary(FLOtotal[FLOtotal$year %in% c(1978:1990) & FLOtotal$SD %in% c("26"), ])
summary(FLOtotal[FLOtotal$year %in% c(1978:1990) & FLOtotal$SD %in% c("28-2"), ])


#
#
#
# rapporto1 <- FLOtotal[FLOtotal$year %in% c(1978:2014) & FLOtotal$SD %in% c("24","25","27", "26","28-2"),]  %>%
#   group_by(year) %>%
#   summarise(mean_rapp = mean(rapp, na.rm=TRUE))
#
#
#
# ggsave(last_plot(),file=paste("FLOUNDER ratio of small.png"), width=12, height=8, dpi=300)
#
# rapportoSD1 <- FLOtotal[FLOtotal$year %in% c(1978:2014) & FLOtotal$SD %in% c("24","25","27", "26","28-2"),] %>%
#   group_by(year,SD) %>%
#   summarise(mean_rapp = mean(rapp, na.rm=TRUE))
#
#
# ggplot(rapportoSD1, aes(year,mean_rapp))+geom_line()+facet_grid(SD~.)
#
# ggplot(rapportoSD1, aes(year,mean_rapp))+geom_line()+geom_line(data=rapportoSD, aes(year,mean_rapp),color="red")+facet_grid(SD~.)
#
#
#

###### FLO total
##### change the names of totalCPUEst3 to totalCPUEst and vice versa so that the variable with number 3 will be the biomass of small flounder and the one without number will be the total biomass

names(FLOtotal)
names(FLOtotal) <- c("IDX", "LATDD", "LONGDD", "year", "vessel", "trawl_time", "SD", "Rect", "validity", "date", "speed", "Depth", "RSA", "RS", "col", "totalCPUEst", "totalCPUEst2", "totalCPUEst3", "rapp")



names(flonoLFD)

summary(flonoLFD)
flonoLFD$year <- flonoLFD$Year
flonoLFD$IDX <- flonoLFD$haul.id
flonoLFD$totalCPUEst <- flonoLFD$CPUEst
flonoLFD$vessel <- flonoLFD$Ship
flonoLFD$trawl_time <- flonoLFD$HaulDur
flonoLFD$validity <- flonoLFD$HaulVal

FLOcomplete <- merge(flonoLFD, FLOtotal, by = intersect(names(flonoLFD), names(FLOtotal)), all = TRUE)

FLOcomplete <- FLOcomplete[, names(FLOtotal)]

FLOcomplete$col <- cut(FLOcomplete$totalCPUEst, breaks = c(-Inf, 0, Inf), labels = c("0", ">0"))

summary(FLOcomplete)

FLOcomplete <- cbind(FLOcomplete, do.call("rbind", strsplit(as.character(FLOcomplete$date), "-")))
FLOcomplete$"2" <- factor(FLOcomplete$"2")

FLOcomplete$"2" <- as.integer(as.character(FLOcomplete$"2"))

FLOcomplete$"2"[FLOcomplete$"2" %in% c(1:3)] <- 1
FLOcomplete$"2"[FLOcomplete$"2" %in% c(4:6)] <- 2
FLOcomplete$"2"[FLOcomplete$"2" %in% c(7:9)] <- 3
FLOcomplete$"2"[FLOcomplete$"2" %in% c(10:12)] <- 4

FLOcomplete$quarter <- factor(FLOcomplete$"2")

FLOcomplete <- FLOcomplete[, c(1:16, 23)]


###### match the hidrological data




sal82 <- read.csv("Hidrological data_new/deep layer/INSPIRE1982_monthly_oce_SAL.csv")



mydist <- function(row, df2) {
  dists <- (row[["LONGDD"]] - df2$lon)^2 + (row[["LATDD"]] - df2$lat)^2
  return(cbind(df2[which.min(dists), ], distance = min(dists)))
}

z <- cbind(codhauls, do.call(rbind, lapply(1:nrow(codhauls), function(x) mydist(row = codhauls[x, ], df2 = sal82))))

z <- z[, c(1:4, 10, 15:17)]

z <- z[z$year >= 1979, ]


z <- cbind(z, do.call("rbind", strsplit(as.character(z$date), "-")))
z$"2" <- factor(z$"2")

z$"2" <- as.integer(as.character(z$"2"))

z$month <- z$"2"

z <- z[, c(1:8, 12)]

z$OXY <- NA
z$SAL <- NA
z$TEM <- NA
z$lat2 <- NA
z$lon2 <- NA

for (i in c("OXY", "TEM", "SAL")) {
  for (l in 1979:2016) {
    for (m in 1:12) {
      temp <- read.csv(paste("Hidrological data_new/deep layer/INSPIRE", l, "_monthly_oce_", i, ".csv", sep = ""))

      z[z$year == l & z$month == m, i] <- temp[match(z$X[z$year == l & z$month == m], temp$X), paste("month_", m, sep = "")]
      z[z$year == l & z$month == m, "lat2"] <- temp[match(z$X[z$year == l & z$month == m], temp$X), "lat"]
      z[z$year == l & z$month == m, "lon2"] <- temp[match(z$X[z$year == l & z$month == m], temp$X), "lon"]
    }
  }
}


# write.csv(z, "env_var_HH.csv")

summary(z$lon == z$lon2)

codhauls_env <- codhauls[codhauls$year >= 1979, ]
codhauls_env$OXY <- z[match(codhauls_env$IDX, z$IDX), "OXY"]
codhauls_env$TEM <- z[match(codhauls_env$IDX, z$IDX), "TEM"]
codhauls_env$SAL <- z[match(codhauls_env$IDX, z$IDX), "SAL"]





summary(FLOcomplete)
names(FLOcomplete)

z2 <- FLOcomplete[, c(1:4, 10)]
summary(z2)



z2 <- cbind(z2, do.call("rbind", strsplit(as.character(z2$date), "-")))
z2$"2" <- factor(z2$"2")

z2$"2" <- as.integer(as.character(z2$"2"))

z2$month <- z2$"2"
summary(z2)
names(z2)
z2 <- z2[, c(1:5, 9)]


z2 <- z2[z2$year >= 1979, ]

z2$OXY <- z[match(z2$IDX, z$IDX), "OXY"]
z2$TEM <- z[match(z2$IDX, z$IDX), "TEM"]
z2$SAL <- z[match(z2$IDX, z$IDX), "SAL"]

summary(z2)

z3 <- z2[is.na(z2$SAL), ]

z3 <- cbind(z3, do.call(rbind, lapply(1:nrow(z3), function(x) mydist(row = z3[x, ], df2 = sal82))))

names(z3)

z3 <- z3[, c(1:12)]



z3$lat2 <- NA
z3$lon2 <- NA

for (i in c("OXY", "TEM", "SAL")) {
  for (l in 1979:2016) {
    for (m in 1:12) {
      temp <- read.csv(paste("Hidrological data_new/deep layer/INSPIRE", l, "_monthly_oce_", i, ".csv", sep = ""))

      z3[z3$year == l & z3$month == m, i] <- temp[match(z3$X[z3$year == l & z3$month == m], temp$X), paste("month_", m, sep = "")]
      z3[z3$year == l & z3$month == m, "lat2"] <- temp[match(z3$X[z3$year == l & z3$month == m], temp$X), "lat"]
      z3[z3$year == l & z3$month == m, "lon2"] <- temp[match(z3$X[z3$year == l & z3$month == m], temp$X), "lon"]
    }
  }
}


summary(z3$lon == z3$lon2)

summary(codhauls_env)

env_idx <- codhauls_env[, c(1, 15:17)]
env_idx <- rbind(env_idx, z3[, c(1, 7:9)])

write.csv(env_idx, "env_idx_new.csv")
