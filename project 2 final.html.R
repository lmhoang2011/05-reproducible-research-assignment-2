> rawData <- read.csv(D:/stormdata.csv)
Error: unexpected '/' in "rawData <- read.csv(D:/"
> rawData <- read.csv("D:/stormdata.csv")
> 
> stormdata$EVTYPE <- as.factor(stormdata$EVTYPE)
Error in is.factor(x) : object 'stormdata' not found
> rawdata$EVTYPE <- as.factor(rawdata$EVTYPE)
Error in is.factor(x) : object 'rawdata' not found
> rawData$EVTYPE <- as.factor(rawData$EVTYPE)
> rawData$BGN_DATE <- as.Date(rawData$BGN_DATE, "%m/%d/%Y")
> rawData$END_DATE <- as.Date(rawData$END_DATE, "%m/%d/%Y")
> fatalData <- subset(rawData, subset=rawData$FATALITIES>0)
> injuryData <- subset(rawData, subset=rawData$INJURIES>0)
> harmData <- rbind(fatalData, injuryData)
> propData <- subset(rawData, subset=rawData$PROPDMG>0)
> cropData <- subset(rawData, subset=rawData$CROPDMG>0)
> econData <- rbind(propData, cropData)
> totalFatal <- aggregate(fatalData$FATALITIES, by=list(fatalData$EVTYPE), FUN=sum)
> colnames(totalFatal) <- c("Event", "Fatalities")
> totalFatal <- totalFatal[order(totalFatal$Fatalities, decreasing=TRUE),]
> head(totalFatal)
             Event Fatalities
141        TORNADO       5633
26  EXCESSIVE HEAT       1903
35     FLASH FLOOD        978
57            HEAT        937
97       LIGHTNING        816
145      TSTM WIND        504
> totalInjury <- aggregate(injuryData$INJURIES, by=list(injuryData$EVTYPE), FUN=sum)
> colnames(totalInjury) <- c("Event", "Injuries")
> totalInjury <- totalInjury[order(totalInjury$Injuries, decreasing=TRUE),]
> head(totalInjury)
             Event Injuries
129        TORNADO    91346
135      TSTM WIND     6957
30           FLOOD     6789
20  EXCESSIVE HEAT     6525
85       LIGHTNING     5230
47            HEAT     2100
> par(mfrow=c(2,1))
> with(fatalData[fatalData$EVTYPE=="TORNADO",], plot(BGN_DATE, FATALITIES, main="Fatalities caused by tornado", xlab= "DATE"))
> with(injuryData[injuryData$EVTYPE=="TORNADO",], plot(BGN_DATE, INJURIES, main="Injuries caused by tornado", xlab= "DATE"))
Warning messages:
1: unable to open printer 
2: opening device failed 
> ## Across the United States, which types of events have the greatest economic consequences?
> totalProp <- aggregate(propData$PROPDMG, by=list(propData$EVTYPE), FUN=sum)
> colnames(totalProp) <- c("Event", "Property_Damage")
> totalProp <- totalProp[order(totalProp$Property_Damage, decreasing=TRUE),]
> head(totalProp)
                Event Property_Damage
334           TORNADO       3212258.2
51        FLASH FLOOD       1420124.6
348         TSTM WIND       1335965.6
64              FLOOD        899938.5
296 THUNDERSTORM WIND        876844.2
106              HAIL        688693.4
> totalCrop <- aggregate(cropData$CROPDMG, by=list(cropData$EVTYPE), FUN=sum)
> colnames(totalCrop) <- c("Event", "Crop_Damage")
> totalCrop <- totalCrop[order(totalCrop$Crop_Damage, decreasing=TRUE),]
> head(totalCrop)
                Event Crop_Damage
42               HAIL   579596.28
23        FLASH FLOOD   179200.46
27              FLOOD   168037.88
115         TSTM WIND   109202.60
107           TORNADO   100018.52
94  THUNDERSTORM WIND    66791.45
> 
