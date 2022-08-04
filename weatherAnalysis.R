library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)

if(!file.exists("weatherData.bz2")){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileUrl,destfile = "./weatherData.bz2",method="curl")}

## Read data into dataframe
  
weather <- read.csv("weatherData.bz2")

## Convert BGN_DATE to a date format

weather$startDate <- as.Date(weather$BGN_DATE,"%m/%d/%Y")

head(weather)

weather %>%
  mutate(year = format(startDate, "%Y")) %>%
  group_by(year, EVTYPE) %>%
  summarise(count=n()) -> weatherTypes

weatherTypes %>%
  group_by(year) %>%
  summarise(count=n()) -> yearCounts

yearCounts[40:50,]

## Filter out dates prior to 1993 when all weather types started being recorded, prior only 3 types recorded

wd <- subset(weather, startDate >= "1993-01-01")

## Since we're looking at human and property damage, filter out all events that included no injuries, fatalities,
## crop damage, or property damage

wd <- subset(wd, FATALITIES > 0 | INJURIES > 0 | CROPDMG > 0 | PROPDMG > 0)

## Clean event types
## started with 516 unique event types

## Set all types to uppercase
## Leaves 438 unique event types
wd$EVTYPE <- toupper(wd$EVTYPE)

## strip white space
## Leaves 430 unique event types
wd$EVTYPE <- trimws(wd$EVTYPE)

## strip extra internal spaces
## Leaves 426 unique event types
wd$EVTYPE <- str_squish(wd$EVTYPE)

## First pass - condense all the main weather events - anything with tornado, tropical storm,
## hurricane, thunderstorm, thunderstorm wind, hail
wd$EVTYPE <- gsub("WINDS","WIND",wd$EVTYPE)
wd$EVTYPE <- gsub(".*TORN(AD|DA)O.*","TORNADO",wd$EVTYPE)
wd$EVTYPE <- gsub("LANDSPOUT","TORNADO",wd$EVTYPE)
wd$EVTYPE <- gsub("TSTMW?","THUNDERSTORM", wd$EVTYPE)
wd$EVTYPE <- gsub("THU.*WIN.*","THUNDERSTORM WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("(SEVERE)? ?THUNDERSTORM(S|W)?","THUNDERSTORM",wd$EVTYPE)
wd$EVTYPE <- gsub(".*HAIL.*","HAIL",wd$EVTYPE)
wd$EVTYPE <- gsub("^TROPICAL.*","TROPICAL STORM",wd$EVTYPE)
wd$EVTYPE <- gsub("HURRICANE.*","HURRICANE",wd$EVTYPE)
wd$EVTYPE <- gsub("TYPHOON.*","HURRICANE",wd$EVTYPE)
wd$EVTYPE <- gsub(".*LIG.*ING.*","THUNDERSTORM",wd$EVTYPE)

## Second pass - condense other weather events
wd$EVTYPE <- gsub("^AVALAN.*","AVALANCHE",wd$EVTYPE)
wd$EVTYPE <- gsub(".*BLIZZARD.*","BLIZZARD",wd$EVTYPE)
wd$EVTYPE <- gsub("EROSION/CSTL*","COASTAL FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub("^(COASTAL|BEACH).*","COASTAL FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub("^COLD.*","COLD/WIND CHILL",wd$EVTYPE)
wd$EVTYPE <- gsub("(EXTENDED|RECORD) COLD.*","COLD/WIND CHILL",wd$EVTYPE)
wd$EVTYPE <- gsub("UNSEA.*COLD","COLD/WIND CHILL",wd$EVTYPE)
wd$EVTYPE <- gsub("(COOL AND WET|LOW TEMPERATURE)","COLD/WIND CHILL",wd$EVTYPE)
wd$EVTYPE <- gsub("BLOWING DUST.*","DUST STORM",wd$EVTYPE)
wd$EVTYPE <- gsub("DUST STORM.*","DUST STORM",wd$EVTYPE)
wd$EVTYPE <- gsub("WHIRLWIND.*","DUST DEVIL",wd$EVTYPE)
wd$EVTYPE <- gsub("DUST DEVIL.*","DUST DEVIL",wd$EVTYPE)
wd$EVTYPE <- gsub("DROUGHT.*","DROUGHT",wd$EVTYPE)
wd$EVTYPE <- gsub("EXTREME.*","EXTREME COLD/WIND CHILL",wd$EVTYPE)
wd$EVTYPE <- gsub("^HYPOTH.*","EXTREME COLD/WIND CHILL",wd$EVTYPE)
wd$EVTYPE <- gsub("(URBAN|DAM|RURAL|SMALL STREAM).*","FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub("^FLOOD.*","FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub("(RIVER|BREAKUP|LAKE|LAKESHORE|MAJOR|MINOR|NON-SEVERE|THUNDERSTORM).* FLOOD.*","FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub(".*FLASH.*","FLASH FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub(".*ICE (JAM|FLOES).*","FLASH FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub("RAPIDLY RISING WATER","FLASH FLOOD",wd$EVTYPE)
wd$EVTYPE <- gsub(".*FREEZE$","FROST/FREEZE",wd$EVTYPE)
wd$EVTYPE <- gsub(".*FROST.*","FROST/FREEZE",wd$EVTYPE)
wd$EVTYPE <- gsub("^FOG.*","FOG",wd$EVTYPE)
wd$EVTYPE <- gsub(".*WARM.*","HEAT",wd$EVTYPE)
wd$EVTYPE <- gsub("HEAT WAVE.*","HEAT",wd$EVTYPE)
wd$EVTYPE <- gsub("RECORD.*HEAT.*","HEAT",wd$EVTYPE)
wd$EVTYPE <- gsub("^HYPERTH.*","HEAT",wd$EVTYPE)
wd$EVTYPE <- gsub("(UNSEASONAL|EXCESSIVE|TORRENTIAL|RECORD|H(EA)?VY) (RAIN|WET|PRECIPITATION|SHOWER).*","HEAVY RAIN",wd$EVTYPE)
wd$EVTYPE <- gsub("HEAVY SNOW.*","HEAVY SNOW",wd$EVTYPE)
wd$EVTYPE <- gsub("(EXCESSIVE|RECORD|LATE SEASON) SNOW.*","HEAVY SNOW",wd$EVTYPE)
wd$EVTYPE <- gsub("SNOW SQUALL.*","HEAVY SNOW",wd$EVTYPE)
wd$EVTYPE <- gsub("THUNDERSNOW.*","HEAVY SNOW",wd$EVTYPE)
wd$EVTYPE <- gsub("HEAVY LAKE SNOW","HEAVY SNOW",wd$EVTYPE)
wd$EVTYPE <- gsub("HEAVY SURF.*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("HIGH (SEAS|TIDES).*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("HEAVY SEAS.*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("(HIGH|HEAVY) SWELLS.*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("^ROUGH.*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("HAZARDOUS SURF.*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub(".*HIGH SURF.*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("HIGH (WATER|WAVES).*","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("TIDAL FLOODING","HIGH SURF",wd$EVTYPE)
wd$EVTYPE <- gsub("(GUSTY|GRADIENT) WIND.*","HIGH WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("HIGH WIND.*","HIGH WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("IC.*ROAD.*","ICE STORM",wd$EVTYPE)
wd$EVTYPE <- gsub("^IC(E|Y).*","ICE STORM",wd$EVTYPE)
wd$EVTYPE <- gsub(".*SLIDE.*","DEBRIS FLOW",wd$EVTYPE)
wd$EVTYPE <- gsub("(APACHE COUNTY|\\?)","OTHER",wd$EVTYPE)
wd$EVTYPE <- gsub("^RAIN.*","RAIN",wd$EVTYPE)
wd$EVTYPE <- gsub("RIP CURRENT.*","RIP CURRENT",wd$EVTYPE)
wd$EVTYPE <- gsub("^SLEET.*","SLEET",wd$EVTYPE)
wd$EVTYPE <- gsub(".*(SURGE|ROGUE).*","STORM SURGE/TIDE",wd$EVTYPE)
wd$EVTYPE <- gsub(".*(BURST|GUSTNADO).*","THUNDERSTORM WIND",wd$EVTYPE)
wd$EVTYPE <- gsub(".*STORM.*WIND","THUNDERSTORM WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("NON-SEVERE WIND DAMAGE.*","STRONG WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("NON( |-)THUNDERSTORM WIND.*","STRONG WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("^WIND .*","STRONG WIND",wd$EVTYPE)
wd$EVTYPE <- gsub("WATERSPOUT.*","WATERSPOUT",wd$EVTYPE)
wd$EVTYPE <- gsub(".*FIRE.*","WILDFIRE",wd$EVTYPE)
wd$EVTYPE <- gsub("RAIN/SNOW.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("MIXED PRECIP.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("FALLING SNOW/ICE.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("FREEZING DRIZZLE.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub(".*WINTER WEATHER.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("GLAZE.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub(".*FREEZING RAIN.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("LIGHT SNOW.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("BLOWING SNOW.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("^SNOW.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("(WINTRY|HEAVY) MIX.*","WINTER WEATHER",wd$EVTYPE)
wd$EVTYPE <- gsub("WINTER STORM.*","WINTER STORM",wd$EVTYPE)
wd$EVTYPE <- gsub("^RAIN$","HEAVY RAIN",wd$EVTYPE)
wd$EVTYPE <- gsub("^WIND$","HIGH WIND",wd$EVTYPE)


unique(sort(wd$EVTYPE))

wd_inj <- subset(wd,FATALITIES > 0 | INJURIES > 0)

unique(sort(wd_inj$EVTYPE))

wd_inj %>%
  group_by(EVTYPE) %>%
  summarize(count=n()) -> events


wd_inj %>%
  group_by(EVTYPE) %>%
  summarize(injuries=sum(INJURIES),deaths=sum(FATALITIES)) -> injuries

injuries %>%
  arrange(desc(injuries)) %>%
  slice(1:10) -> inj_top

injuries %>%
  arrange(desc(deaths)) %>%
  slice(1:10)  -> death_top

top_health <- data.frame(inj_top$EVTYPE,death_top$EVTYPE)

colnames(top_health) <- c("Injuries","Fatalities")


wd_inj %>%
  group_by(EVTYPE) %>%
  summarize(injuries=sum(INJURIES),deaths=sum(FATALITIES),totalPeople=sum(INJURIES,FATALITIES)) %>%
  arrange(desc(totalPeople)) %>%
  slice(1:10) -> people

people_long <- pivot_longer(people,-c(EVTYPE,totalPeople),names_to="damageType",values_to="value")


people_long %>%
  group_by(EVTYPE,damageType) %>%
  summarize(sum=sum(value)) %>%
  ggplot(aes(x=EVTYPE,y=sum,fill=damageType)) + geom_bar(stat="identity") + 
  labs(title="Top 10 Weather Events by Harm to Population Health", x="Weather Type",y="Number of People Injured or Killed", fill="Damage Type") + 
  theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1)) +
  scale_fill_manual(labels=c('Deaths','Injuries'),values=c("light blue","#0072B2"))




wd_dmg <- subset(wd,CROPDMG > 0 | PROPDMG > 0)



wd_dmg %>%
  group_by(EVTYPE) %>%
  summarize(count=n()) -> events_dmg

unique(sort(wd_dmg$CROPDMGEXP))

wd_dmg$CROPDMGEXP <- toupper(wd_dmg$CROPDMGEXP)

subset(wd_dmg,CROPDMGEXP == '0')



wd_dmg %>%
  mutate(cropDmgAmt = case_when(
    CROPDMGEXP == 'K' ~ CROPDMG * 1000,
    CROPDMGEXP == 'M' ~ CROPDMG * 1000000,
    CROPDMGEXP == 'B' ~ CROPDMG * 1000000000,
    CROPDMGEXP == '0' ~ CROPDMG,
    CROPDMG > 0 ~ CROPDMG,
    TRUE ~ 0)) -> wd_dmg

unique(sort(wd_dmg$PROPDMGEXP))

wd_dmg$PROPDMGEXP <- toupper(wd_dmg$PROPDMGEXP)

billions <- subset(wd_dmg,PROPDMGEXP == 'B')

subset(wd_dmg,PROPDMGEXP == 'B' & STATE == 'CA' & EVTYPE == 'FLOOD')

wd_dmg %>%
  mutate(propDmgAmt = case_when(
    PROPDMGEXP == 'H' ~ PROPDMG * 100,
    PROPDMGEXP == 'K' ~ PROPDMG * 1000,
    PROPDMGEXP == 'M' ~ PROPDMG * 1000000,
    PROPDMGEXP == 'B' ~ PROPDMG * 1000000000,
    PROPDMGEXP == '0' ~ PROPDMG,
    PROPDMGEXP == '+' ~ PROPDMG,
    PROPDMGEXP == '-' ~ PROPDMG,
    PROPDMGEXP == '2' ~ PROPDMG * 100,
    PROPDMGEXP == '3' ~ PROPDMG * 1000,
    PROPDMGEXP == '4' ~ PROPDMG * 10000,
    PROPDMGEXP == '5' ~ PROPDMG * 100000,
    PROPDMGEXP == '6' ~ PROPDMG * 1000000,
    PROPDMGEXP == '7' ~ PROPDMG * 10000000,
    PROPDMG > 0 ~ PROPDMG,
    TRUE ~ 0)) -> wd_dmg


wd_dmg %>%
  group_by(EVTYPE) %>%
  summarize(cropDamage=sum(cropDmgAmt),propDamage=sum(propDmgAmt),totalDamage=sum(cropDmgAmt,propDmgAmt)) %>%
  arrange(desc(totalDamage)) %>%
  slice(1:10) -> damage

damage_long <- pivot_longer(damage,-c(EVTYPE,totalDamage),names_to="damageType",values_to="value")


damage_long %>%
  group_by(EVTYPE,damageType) %>%
  summarize(sum=sum(value)) %>%
  ggplot(aes(x=EVTYPE,y=sum,fill=damageType)) + geom_bar(stat="identity") + 
  labs(title="Top 10 Weather Events by $ Value of Damage Caused", x="Weather Type",y="$ Amount of Damage Caused", fill="Damage Type") + 
  theme(axis.text.x = element_text(angle=60,vjust=1,hjust=1)) +
  scale_fill_manual(labels=c('Crop Damage','Property Damage'),values=c("light blue","#0072B2"))


wd_dmg %>%
  group_by(EVTYPE) %>%
  summarize(crops=sum(cropDmgAmt),property=sum(propDmgAmt)) -> dmg_amt

dmg_amt %>%
  arrange(desc(crops)) %>%
  slice(1:10) -> crops_top

dmg_amt %>%
  arrange(desc(property)) %>%
  slice(1:10)  -> prop_top

top_damage <- crops_top[,1]

names(top_damage) <- c("Crops")

top_damage$Property <- prop_top[,1]
  

 



