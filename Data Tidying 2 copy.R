receiver <- read.csv("/Users/freyakeyser/Desktop/Metadata/Receiver Data.csv")
MP2011 <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/2013 Summer/2011 Minas Passage/Spreadsheets/2011 Striped Bass - All Detects - Drift Corrected - 2013-12-10.csv" , header=TRUE)
MB2011 <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/Masters/2011 deployment/VUE_Export_2011SB_MinasBasin_2014-05-23.csv")
MBMP2012 <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/Masters/2012-2013 deployment/Overall spreadsheets/VUE_Export_2012SB_AllReceivers_2014-05-08.csv", header=TRUE)
gasp2011 <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/Masters/2011 deployment/VUE_Export_2011_GaspereauRiver_for Freya.csv")
tagging <- read.csv("/Users/freyakeyser/Desktop/Metadata/Tagging Data.csv")

citation()

require(plyr)
require(reshape2)
require(lubridate)
require(ggplot2)

# Make column names consistent
names(MP2011) <- c("Date.and.Time..UTC.", "Month", "Detection.Number", "Transmitter", "Fish.Code", "Fish.Number", "Sensor.Value", "Detection.Depth", "Height.above.bottom", "Station.Name", "Depth", "Receiver", "Latitude", "Longitude", "Tagging.location", "Fork.Length", "Fork.Length.Bin", "Array", "Date_Time.AST.ADT", "Hour.AST", "Sunrise.Date_Time.UTC", "Sunrise.Date_Time.AST", "Sunset.Date_Time.UTC", "Sunset.Date_Time.AST", "Day", "Night", "Twilight", "Day_Night_Twilight")
names(MB2011)

# Subset for only necessary detection information columns
MP2011 <- subset(MP2011, select=c("Date.and.Time..UTC.", "Receiver", "Transmitter", "Sensor.Value"))    
MB2011 <- subset(MB2011, select=c("Date.and.Time..UTC.", "Receiver", "Transmitter", "Sensor.Value"))
MBMP2012 <- subset(MBMP2012, select=c("Date.and.Time..UTC.", "Receiver", "Transmitter", "Sensor.Value"))
gasp2011 <- subset(gasp2011, select=c("Date.and.Time..UTC.", "Receiver", "Transmitter", "Sensor.Value"))

# Make data format consistent between spreadsheets
MB2011$Receiver <- as.factor(MB2011$Receiver)
levels(MB2011$Receiver)
cols <- colsplit(MB2011$Receiver, "-", c("alpha", "numeric"))
cols$complete <- ifelse(is.na(cols$numeric), cols$alpha, cols$numeric)
MB2011$Receiver <- as.character(cols$complete)

MB2011$Transmitter <- as.factor(MB2011$Transmitter)
levels(MB2011$Transmitter)
cols <- colsplit(MB2011$Transmitter, "-", c("alpha", "middle", "numeric"))
MB2011$Transmitter <- as.character(cols$numeric)

gasp2011$Receiver <- as.factor(gasp2011$Receiver)
levels(gasp2011$Receiver)
cols <- colsplit(gasp2011$Receiver, "-", c("alpha", "numeric"))
cols$complete <- ifelse(is.na(cols$numeric), cols$alpha, cols$numeric)
gasp2011$Receiver <- as.character(cols$complete)

gasp2011$Transmitter <- as.factor(gasp2011$Transmitter)
levels(gasp2011$Transmitter)
cols <- colsplit(gasp2011$Transmitter, "-", c("alpha", "middle", "numeric"))
gasp2011$Transmitter <- as.character(cols$numeric)

MP2011$Date.and.Time..UTC. <- mdy_hms(MP2011$Date.and.Time..UTC.)
MB2011$Date.and.Time..UTC. <- mdy_hms(MB2011$Date.and.Time..UTC.)
MBMP2012$Date.and.Time..UTC. <- mdy_hms(MBMP2012$Date.and.Time..UTC.)
gasp2011$Date.and.Time..UTC. <- mdy_hms(gasp2011$Date.and.Time..UTC.)

MBMP2012$Receiver <- as.factor(MBMP2012$Receiver)
levels(MBMP2012$Receiver)
cols <- colsplit(MBMP2012$Receiver, "-", c("alpha", "numeric"))
cols$complete <- ifelse(is.na(cols$numeric), cols$alpha, cols$numeric)
unique(cols$complete)
MBMP2012$Receiver <- as.character(cols$complete)

MBMP2012$Transmitter <- as.factor(MBMP2012$Transmitter)
levels(MBMP2012$Transmitter)
cols <- colsplit(MBMP2012$Transmitter, "-", c("alpha", "middle", "numeric"))
MBMP2012$Transmitter <- as.character(cols$numeric)

# Join two dataframes together
merge1 <- join(MBMP2012, MB2011, type="full")
merge1$Transmitter <- as.character(merge1$Transmitter)
merge1$Sensor.Value <- as.numeric(merge1$Sensor.Value)

MP2011$Receiver <- as.character(MP2011$Receiver)
MP2011$Transmitter <- as.character(MP2011$Transmitter)
MP2011$Sensor.Value <- as.numeric(MP2011$Sensor.Value)

# Join another dataframe
merge2 <- join(merge1, MP2011, type="full")

gasp2011$Receiver <- as.character(gasp2011$Receiver)
gasp2011$Transmitter <- as.character(gasp2011$Transmitter)
gasp2011$Sensor.Value <- as.numeric(gasp2011$Sensor.Value)

# Join last dataframe
merge3 <- join(merge2, gasp2011, type="full")

# tagdata contains ALL DETECTIONS
tagdata <- merge3

tagdata$Date.and.Time..UTC. <- ymd_hms(tagdata$Date.and.Time..UTC.)

# Prep receiver dataframe
names(receiver) <- c("Array", "Station.Name", "CDN", "Deploy.Date.and.Time", "Recover.Date.and.Time", "Latitude", "Longitude", "Receiver")
receiver$Deploy.Date.and.Time <- mdy_hm(receiver$Deploy.Date.and.Time)
receiver$Recover.Date.and.Time <- mdy_hm(receiver$Recover.Date.and.Time)
levels(receiver$Array) <- c("MPD", "MPS", "NSP", "NSP")
MPSreceiver <- subset(receiver, Array=="MPS")
otherreceiver <- subset(receiver, Array=="MPD"|Array=="NSP")

MPSreceiver$Station.Name <- as.character(MPSreceiver$Station.Name)
MPSreceiver$Station.Name <- as.factor(MPSreceiver$Station.Name)
levels(MPSreceiver$Station.Name) <- c("MPS-01", "MPS-02", "MPS-03", "MPS-04", "MPS-05",
                                      "MPS-06", "MPS-07", "MPS-08", "MPS-09", "MPS-10",
                                      "MPS-11", "MPS-12")
receiver <- merge(MPSreceiver, otherreceiver, all=TRUE)
receiver$Station.Name <- as.character(receiver$Station.Name)
receiver$Station.Name <- as.factor(receiver$Station.Name)
receiver$Deploy.Date.and.Time <- ymd_hms(receiver$Deploy.Date.and.Time)
receiver$Recover.Date.and.Time <- ymd_hms(receiver$Recover.Date.and.Time)

write.csv(receiver, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Receiver metadata.csv")

# Merge receiver data and tag data. There will be duplicated rows whenever the same receiver was redeployed
receivertag <- merge(tagdata, receiver, by="Receiver", all=TRUE)
receivertag$Deploy.Date.and.Time <- ymd_hms(receivertag$Deploy.Date.and.Time)
receivertag$Recover.Date.and.Time <- ymd_hms(receivertag$Recover.Date.and.Time)
receivertag$Date.and.Time..UTC. <- ymd_hms(receivertag$Date.and.Time..UTC.)

# Subset using date constraints to remove those duplicated rows
receivertag1 <- subset(receivertag, Date.and.Time..UTC.>=Deploy.Date.and.Time & Date.and.Time..UTC.<=Recover.Date.and.Time)

receivertagother <- subset(receivertag, Receiver==113695 | Receiver==106591)

receivertagfinal <- merge(receivertag1, receivertagother, all=TRUE)

# Adjust names of tagging dataframe for consistency
names(tagging) <- c("Floy.tag", "Tag.model", "Transmitter", "Fish.code", "Est.tag.life", "Capture.location", "Length", "Release.date.time", "Sensor.equation")

levels(tagging$Fish.code) <- c("G-1-21", "G-1-22", "G-1-23", "G-1-24", "G-1-25", "G-1-26", "G-1-27",
                                "G-1-28", "G-1-29", "G-1-30", "G-1-31", "G-1-32", "G-1-33", "G-1-34",
                                "G-1-35", "G-1-36", "G-1-37", "G-1-38", "G-1-39", "G-1-40", "G-2-10",
                                "G-2-11", "G-2-12", "G-2-13", "G-2-14", "G-2-15", "G-2-16", "G-2-17",
                                "G-2-18", "G-2-19", "G-2-20", "G-2-21", "G-2-22", "G-2-23", "G-2-24",
                                "G-2-25", "G-2-26", "G-2-27", "G-2-28", "G-2-29", "G-2-30", "G-2-31",
                                "G-2-32", "G-2-33", "G-2-34", "G-2-35", "G-2-36", "G-2-37", "G-2-38",
                                "G-2-08",  "G-2-09",  "K-2-39", "K-2-40", "K-2-41", "K-2-42", "K-2-43",
                                "K-2-44", "K-2-45", "S-1-01",  "S-1-10", "S-1-11", "S-1-12", "S-1-13", 
                                "S-1-14", "S-1-15", "S-1-16", "S-1-17", "S-1-18", "S-1-19", "S-1-02", 
                                "S-1-20", "S-1-03",  "S-1-04",  "S-1-05",  "S-1-06",  "S-1-07",  "S-1-08",
                                "S-1-09",  "S-2-01",  "S-2-02",  "S-2-03",  "S-2-04",  "S-2-05",  "S-2-06",
                                "S-2-07")

tagging$Fish.code <- factor(tagging$Fish.code, levels=
                               c("S-1-01",  "S-1-02", "S-1-03", "S-1-04", "S-1-05", 
                                 "S-1-06", "S-1-07", "S-1-08", "S-1-09","S-1-10", 
                                 "S-1-11", "S-1-12", "S-1-13", "S-1-14", "S-1-15", 
                                 "S-1-16", "S-1-17", "S-1-18", "S-1-19", "S-1-20",
                                 "G-1-21", "G-1-22", "G-1-23", "G-1-24", "G-1-25", 
                                 "G-1-26", "G-1-27", "G-1-28", "G-1-29", "G-1-30",
                                 "G-1-31", "G-1-32", "G-1-33", "G-1-34","G-1-35", 
                                 "G-1-36", "G-1-37", "G-1-38", "G-1-39", "G-1-40", 
                                 "S-2-01", "S-2-02", "S-2-03", "S-2-04", "S-2-05",
                                 "S-2-06", "S-2-07", "G-2-08",  "G-2-09", "G-2-10",
                                 "G-2-11", "G-2-12", "G-2-13", "G-2-14", "G-2-15", 
                                 "G-2-16", "G-2-17", "G-2-18", "G-2-19", "G-2-20", 
                                 "G-2-21", "G-2-22", "G-2-23", "G-2-24", "G-2-25", 
                                 "G-2-26", "G-2-27", "G-2-28", "G-2-29", "G-2-30", 
                                 "G-2-31", "G-2-32", "G-2-33", "G-2-34", "G-2-35", 
                                 "G-2-36", "G-2-37", "G-2-38", "K-2-39", "K-2-40",
                                 "K-2-41", "K-2-42", "K-2-43", "K-2-44", "K-2-45"))

cols <- colsplit(tagging$Release.date.time, "T", c("date", "time"))
tagging$Release.date <- as.character(cols$date)
tagging$Release.time <- as.character(cols$time)
tagging$Release.date.time <- paste0(tagging$Release.date, " ", tagging$Release.time)

tagging$year <- year(tagging$Release.date.time)

levels(tagging$Capture.location) <- c("Grand Pré", "Grand Pré", "Stewiacke", "Stewiacke", "Kingsport", "Stewiacke")

write.csv(tagging, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Tagging metadata.csv")

# Merge tagging data with final detection data
receivertag2 <- merge(receivertagfinal, tagging, by="Transmitter")

receivertag2$Transmitter <- as.numeric(receivertag2$Transmitter)

# Calibrate sensor values
v13 <- subset(receivertag2, Transmitter<7407)
v13$Detection.depth <- 0.4397*(v13$Sensor.Value)-1.7587

v16 <- subset(receivertag2, Transmitter>14376)
v16$Detection.depth <- 0.6065*(v16$Sensor.Value)-2.4258

receivertag2 <- merge(v13, v16, all=TRUE)

receivertag3 <- cbind(receivertag2[3],receivertag2[1:2], receivertag2[6], receivertag2[5], 
                      receivertag2[10:11], receivertag2[4], receivertag2[23], receivertag2[20], receivertag2[21], 
                      receivertag2[7:9], receivertag2[12:19], receivertag2[22])

receivertag3$Array <- as.character(receivertag3$Array)

unique(receivertag3$Array)

receivertag3$Array <- gsub(" ","", receivertag3$Array , fixed=TRUE)

alldata <- receivertag3

alldata$year <- year(alldata$Date.and.Time..UTC.)
alldata$month <- month(alldata$Date.and.Time..UTC.)

### Conforming all MP station names

alldata$Station.Name <- as.character(alldata$Station.Name)
alldata$Station.Name <- as.factor(alldata$Station.Name)
levels(alldata$Station.Name) <- c("AUL-01", "AUL-02", "AUL-03", "AUL-04", "AUL-05", "AUL-06", 
                                  "AUL-07", "AUL-08", "AUL-09", "AUL-10", "AUL-11","AUL-12", "AUL-13", "AUL-14",
                                  "AUL-T1", "AUL-T2", "AUL-T3", "Blomidon", "Blue Beach", 
                                  "Boot Island East", "Bramber", "Cheverie",
                                  "Delhaven", "Economy 1", "Economy 2", "Five Islands", 
                                  "Gaspereau East", "Gaspereau West", "GR-01", "GR-02", "GR-03", "GR-04",
                                  "GR-05", "Guzzle", "Kennetcook", "Kennetcook Upper", "Kingsport",
                                  "Linkletter Weir", "Maitland", "Melanson Bridge",  "MPS-01",
                                  "MPS-02", "MPS-03", "MPS-04", "MPS-05", "MPS-06", "MPS-07", "MPS-08",
                                  "MPS-09", "MPS-10", "MPS-11", "MPS-12", "St. Croix", "STEW-01",
                                  "STEW-02", "Wallbrook", "Walton 1", "Walton 2", "Walton 3")        

winterdata <- subset(alldata, year == 2012 & month == 12 | year == 2013 & month < 4)

### WRITE ALL DATA TO SEPARATE CSV

write.csv(alldata, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Detections_MP_MB_2011-2013.csv")

### WRITE WINTER DATA TO SEPARATE CSV

write.csv(winterdata, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Winter 2012-2013_Oct31.csv")

winterfish <- subset(winterdata, select=c("Fish.code", "Length"))
unique(winterfish)

### SUBSET BASIN DATA 

basindata <- subset(alldata, Station.Name=="Avon East" | Station.Name== "Blomidon" | Station.Name=="Blue Beach" 
                    | Station.Name=="Boot Island East" | Station.Name=="Bramber" | Station.Name=="Cheverie" | Station.Name=="Cornwallis River"
                    | Station.Name== "Delhaven" | Station.Name=="Economy 1" | Station.Name=="Economy 2" | Station.Name=="Evangeline Beach 2" | Station.Name=="Five Islands" 
                    | Station.Name=="Gaspereau East" | Station.Name=="Gaspereau West" | Station.Name=="Guzzle" | Station.Name=="Kennetcook" | Station.Name=="Kennetcook Upper" | Station.Name=="Kingsport"
                    | Station.Name=="Linkletter Weir" | Station.Name=="Maitland" | Station.Name=="Melanson Bridge" | Station.Name=="Spencer Creek" | Station.Name=="St. Croix"
                    | Station.Name=="St. Croix Upper" | Station.Name=="STEW1" | Station.Name=="STEW2" | Station.Name=="STEW3" | Station.Name=="Wallbrook" | Station.Name=="Walton 1"
                    | Station.Name=="Walton 2" | Station.Name=="Walton 3" )

write.csv(basindata, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Minas Basin detections.csv")

write.csv(AUL, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/AUL detections.csv")

### Correcting tagging location names

levels(alldata$Capture.location) <- c("Grand Pré", "Grand Pré", "Stewiacke", "Stewiacke", "Kingsport", "Stewiacke")

### Subset MP years

MP_all <- subset(alldata, Station.Name=="AUL-T1" | Station.Name=="AUL-T2" | Station.Name=="AUL-T3" | Station.Name=="AUL-01" | 
                    Station.Name=="AUL-02" | Station.Name=="AUL-03" | Station.Name=="AUL-04" | Station.Name=="AUL-05" | 
                    Station.Name=="AUL-06" | Station.Name=="AUL-07" | Station.Name=="AUL-08" | Station.Name=="AUL-09" | 
                    Station.Name=="AUL-10" | Station.Name=="AUL-11" | Station.Name=="AUL-12" | Station.Name=="AUL-13" | 
                    Station.Name=="AUL-14" | Station.Name=="MPS-01" | Station.Name=="MPS-02" | Station.Name=="MPS-03" | 
                    Station.Name=="MPS-04" | Station.Name=="MPS-05" | Station.Name=="MPS-06" | Station.Name=="MPS-07" | 
                    Station.Name=="MPS-08" | Station.Name=="MPS-09" | Station.Name=="MPS-10" | Station.Name=="MPS-11" | 
                    Station.Name=="MPS-12")

MP_2011 <- subset(MP_all, year==2011)
MP_2012 <- subset(MP_all, year==2012 | year==2013)
MP_2012 <- subset(MP_2012, !Station.Name %in% c("AUL-T1"))

MP_2012$season <- ifelse(MP_2012$Date.and.Time..UTC.<"2012-12-01 00:00:00 UTC", "Spring/Summer/Fall", "Winter")

### WRITE MINAS PASSAGE DATA TO SEPARATE CSV

write.csv(MP_all, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Detections_MP_2011-2013.csv")

### For modelled data from Brian

MP_all_Brian <- subset(MP_all, select=c("Date.and.Time..UTC.", "Transmitter", "Receiver", "Station.Name", "Latitude", "Longitude", "Fish.code"))

write.csv(MP_all_Brian, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Detections_MP_2011-2013_minimal.csv")

### SUBSET FORCE DATA

forcedata <- subset(alldata, Station.Name=="AUL-T1" | Station.Name=="AUL-T2" | Station.Name=="AUL-T3" | Station.Name=="AUL-01" | 
                      Station.Name=="AUL-02" | Station.Name=="AUL-03" | Station.Name=="AUL-04" | Station.Name=="AUL-05" | 
                      Station.Name=="AUL-06" | Station.Name=="AUL-07" | Station.Name=="AUL-08" | Station.Name=="AUL-09" | 
                      Station.Name=="AUL-10" | Station.Name=="AUL-11" | Station.Name=="AUL-12" | Station.Name=="AUL-13" | 
                      Station.Name=="AUL-14")
forcedata_2 <- subset(forcedata, year=="2012" | year== "2013")
forcedata_3 <- subset(alldata, Station.Name=="AUL-T1" | Station.Name=="AUL-T2" | Station.Name=="AUL-T3")

forcedata_all <- merge(forcedata_2, forcedata_3, all=TRUE)

length(unique(forcedata_all$Fish.code))

### Subset MPS and AUL data

MPS <- subset(alldata, Array=="MPS")

AUL <- subset(alldata, Array=="NSP")

### Brian's CS data
tagging <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Tagging metadata.csv")

detections_withCS <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Passage/Current Model from Brian (Jan 8)/Detections_MP_2011-2013_minimal_H_el_u_v.csv")

detections_withCS$speed <- sqrt((detections_withCS$u.m.s)^2 + (detections_withCS$v.m.s)^2)

detects_final_MP <- read.csv("/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/Passage/Filtered MP detections.csv")
unique(detects_final_MP$Station.Name)

detects_final_ALL_withCS <- join(detects_final_MP, detections_withCS, type="left")
levels(detects_final_ALL_withCS$Station.Name)
detects_final_ALL_withCS$year <- year(detects_final_ALL_withCS$Date.and.Time..UTC.)

detects_final_ALL_withCS_recode <- subset(detects_final_ALL_withCS, year=="2011" & Array=="NSP")
detects_final_ALL_withCS_recode$Station.Name.New <- detects_final_ALL_withCS_recode$Station.Name
detects_final_ALL_withCS_recode$Station.Name.New <- as.character(detects_final_ALL_withCS_recode$Station.Name.New)
detects_final_ALL_withCS_recode$Station.Name.New <- as.factor(detects_final_ALL_withCS_recode$Station.Name.New)
levels(detects_final_ALL_withCS_recode$Station.Name.New) <- c("A1-01", "A1-02", "A1-03", "A1-04", "A1-05", "A1-06", "A1-07", "A1-08", "A1-09", "A1-10", "A1-11", "A1-12", "A1-13", "A1-14", "A1-T1", "A1-T2", "A1-T3")

detects_final_ALL_withCS_recode2 <- subset(detects_final_ALL_withCS, (year=="2012" | year=="2013") & Array=="NSP")
detects_final_ALL_withCS_recode2$Station.Name.New <- detects_final_ALL_withCS_recode2$Station.Name
detects_final_ALL_withCS_recode2$Station.Name.New <- as.character(detects_final_ALL_withCS_recode2$Station.Name.New)
detects_final_ALL_withCS_recode2$Station.Name.New <- as.factor(detects_final_ALL_withCS_recode2$Station.Name.New)
levels(detects_final_ALL_withCS_recode2$Station.Name.New) <- c("A2-01", "A2-02", "A2-03", "A2-04", "A2-05", "A2-06", "A2-07", "A2-08", "A2-09", "A2-10", "A2-11", "A2-12", "A2-T1")

detects_final_MPS_withCS <- subset(detects_final_ALL_withCS, Array=="MPS")
detects_final_MPS_withCS$Station.Name.New <-  detects_final_MPS_withCS$Station.Name

detects_final_ALL_withCS_2 <- join(detects_final_MPS_withCS, detects_final_ALL_withCS_recode, type="full")
detects_final_ALL_withCS_2 <- join(detects_final_ALL_withCS_2, detects_final_ALL_withCS_recode2, type="full")

detects_final_ALL_withCS_2 <- subset(detects_final_ALL_withCS_2, select=c("Fish.code", "Array", "Detection.depth", "Station.Name.New", "Date.and.Time..UTC.", "season", "sunriseUTC", "sunsetUTC", "daynight", "year", "Transmitter", "Receiver", "Latitude", "Longitude", "H.m.", "elev.m.", "u.m.s.", "v.m.s.", "speed"))

### Adding new variables, fixing old ones

### Tide direction
detects_final_ALL_withCS_2$Tide.direction <- ifelse(detects_final_ALL_withCS_2$u.m.s. < 0, "Ebb", "Flood")

### Water column depth
detects_final_ALL_withCS_2$watercolumn <- detects_final_ALL_withCS_2$H.m. + detects_final_ALL_withCS_2$elev.m.

### Month
detects_final_ALL_withCS_2$month <- month(detects_final_ALL_withCS_2$Date.and.Time..UTC.)

### Adding fish data
detects_final_ALL_withCS_2_tag <- join(detects_final_ALL_withCS_2, tagging, type="left", by="Fish.code")

### Subsetting to get useful columns only
depthmodeldf <- subset(detects_final_ALL_withCS_2_tag, select=c("Date.and.Time..UTC.", "Fish.code", "Station.Name.New", "Latitude", "Longitude", "Detection.depth", "Capture.location", "Length", "year", "month", "speed", "Tide.direction", "watercolumn", "daynight"))
  ####### USED IN DEPTH MODEL ^^^
write.csv(depthmodeldf, "/Users/freyakeyser/Documents/Acadia/ACER/Masters/Overall spreadsheets:figures/depthmodeldf.csv")

### MPS receivers 2011 metadata for Stephanie Smedbol

MPSreceiver$year <- year(MPSreceiver$Deploy.Date.and.Time)
MPSreceiver2011 <- subset(MPSreceiver, year=="2011", select=c("Station.Name", "Deploy.Date.and.Time", "Recover.Date.and.Time", "Latitude", "Longitude", "Receiver"))

receiverdepth <- subset(detections_withCS, select=c("Receiver", "Station.Name", "Date.and.Time..UTC.", "H.m."))
receiverdepth$year <- year(receiverdepth$Date.and.Time..UTC.)
receiverdepth <- subset(receiverdepth, year=="2011", select=c("Receiver", "Station.Name", "year", "H.m."))
MPSreceiverdepth <- unique(receiverdepth)
MPSreceiver2011watercolumn <- join(MPSreceiver2011, MPSreceiverdepth, type="left")
names(MPSreceiver2011watercolumn) <- c("Station Name", "Deploy Date and Time", "Recover Date and Time", "Latitude", "Longitude", "Receiver", "Year", "Water Column Depth")

write.csv(MPSreceiver2011watercolumn, "/Users/freyakeyser/Desktop/MPS Receivers 2011.csv")

### Publication/thesis figure formatting
figure_theme <- theme_bw(base_size=15) +
  theme(legend.background=element_rect(fill="white", colour="black", size=0.4), 
        strip.background = element_rect(colour="black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border=element_rect(colour="black"))

### t-test on tagging sizes
GPfish <- subset(tagging, Capture.location=="Grand Pré")
Stewfish <- subset(tagging, Capture.location=="Stewiacke")
Kingsfish <- subset(tagging, Capture.location=="Kingsport")

t.test(GPfish$Length, Stewfish$Length)
# Welch Two Sample t-test
# 
# data:  GPfish$Length and Stewfish$Length
# t = -17.3059, df = 51.195, p-value < 2.2e-16 
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2752544 -0.2180353
# sample estimates:
#   mean of x mean of y 
# 0.4557255 0.7023704 

t.test(GPfish$Length, Kingsfish$Length)
# Welch Two Sample t-test
# 
# data:  GPfish$Length and Kingsfish$Length
# t = -13.0285, df = 10.223, p-value = 1.072e-07
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2548249 -0.1805813
# sample estimates:
#   mean of x mean of y 
# 0.4557255 0.6734286 

t.test(Stewfish$Length, Kingsfish$Length)
# Welch Two Sample t-test
# 
# data:  Stewfish$Length and Kingsfish$Length
# t = 1.5487, df = 14.774, p-value = 0.1426
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01094281  0.06882640
# sample estimates:
#   mean of x mean of y 
# 0.7023704 0.6734286 
