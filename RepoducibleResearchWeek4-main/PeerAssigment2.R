#Loading required libraries for cleaning and managing the data
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(gridExtra)
library(chron)

#Downloading the data. First we check if we have the file already in our working directory. If not we download it.

if(!file.exists("StormData")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileURL, "StormData")
  data <- read.csv('C:/Users/priskeer/Desktop/PilotTest24.02.2021/ProjectExtras/Privat/School/Data Science/Course 5/Week4/StormData', header=T)
  storm <- data
  storm <- data.frame(storm)
}

if(file.exists("StormData")){
  data <- read.csv('C:/Users/priskeer/Desktop/PilotTest24.02.2021/ProjectExtras/Privat/School/Data Science/Course 5/Week4/StormData', header=T)
  storm <- data
  storm <- data.frame(storm)
}

#We want to analyse what Event type is responsable for the most injuries, fatalities and property damage, so the first step is to process the database.
storm <- storm[-c(9:22, 26:37)]

#Getting an idea of the data. With the summary we realize that we have 2304 NA´s
head(storm)
summary(storm)
str(storm)

#Processing the Storm Data
storm$BGN_DATE <- as.character(storm$BGN_DATE)
storm$BGN_DATE <- as_date(mdy_hms(storm$BGN_DATE), tz=NULL)
storm$COUNTY <- as.factor(storm$COUNTY)
storm$STATE <- as.factor(storm$STATE)

#We want to know which State has the most Event. We want the top 8 States.
EventState <- aggregate(EVTYPE ~ STATE, storm, FUN = length)
EventState %>%
  arrange(desc(EVTYPE)) %>%
  slice(1:8) %>%
  ggplot(aes(x = reorder(STATE, -EVTYPE), y = EVTYPE, fill = STATE)) +
  geom_col(size = 2) +
  labs(title = "Total Event Types per State", subtitle = "From 1952 to 2011") +
  ylab("Count of Events") +
  xlab("State") +
  theme_classic() +
  scale_fill_brewer(palette = "Set3")

#We want to know what type of Event is the most ocurring in USA.
EventT <- sort(table(storm$EVTYPE), decreasing = TRUE)
EventT <- EventT[1:8]
EventT <- data.frame(EventT)
colnames(EventT) <- c("EVTYPE", "Frequency")
EventT %>%
  ggplot(aes(x = EVTYPE, y = Frequency, fill = EVTYPE)) +
  geom_col(size = 2) +
  labs(title = "Total Event Types per State", subtitle = "From 1952 to 2011") +
  scale_y_continuous(name="Count of Events", labels = scales::comma) +
  xlab("Event Type") +
  theme_classic(base_size = 7) +
  scale_fill_brewer(palette = "Set3")

#We want to know which event causes the most injuries and Fatalities and Property Damage.
#First we summarise and create a new Data frame with the new data.
EventDMG <- data.frame(storm[8:11])
EventDMG <- EventDMG %>% 
              group_by(EVTYPE) %>%
              summarise(
                FATALITIES = sum(FATALITIES),
                INJURIES = sum(INJURIES),
                PROPDMG = sum(PROPDMG)
    
  )

#Top 8 from Event Type ordered by Fatalities
EventF <- EventDMG[1:2]
EventF <- EventF[order(EventF$FATALITIES, decreasing = TRUE),]
EventF <- EventF[1:8,]

#Top 8 from Event Type ordered by Injuries
EventI <- select(EventDMG, 1, 3)
EventI <- EventI[order(EventI$INJURIES, decreasing = TRUE),]
EventI <- EventI[1:8,]

#Top 8 from Event Type ordered by Property Damage
EventP <- select(EventDMG, 1, 4)
EventP <- EventP[order(EventP$PROPDMG, decreasing = TRUE),]
EventP <- EventP[1:8,]

#Now we want to plot this information with the top 8 from every Variable
require(gridExtra)

plot1 <- EventF %>%
  ggplot(aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES, fill = EVTYPE)) +
  geom_col(size = 2) +
  labs(title = "Top 8 Fatalities per Event Type", subtitle = "From 1952 to 2011") +
  scale_y_continuous(name="Count of Fatalities", labels = scales::comma) +
  xlab("Event Type") +
  theme_classic(base_size = 7) +
  scale_fill_brewer(palette = "Set1") +
  guides(fill="none")

plot2 <- EventI %>%
  ggplot(aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES, fill = EVTYPE)) +
  geom_col(size = 2) +
  labs(title = "Top 8 Injuries per Event Type", subtitle = "From 1952 to 2011") +
  scale_y_continuous(name="Count of Injuries", labels = scales::comma) +
  xlab("Event Type") +
  theme_classic(base_size = 7) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill="none")

plot3 <- EventP %>%
  ggplot(aes(x = reorder(EVTYPE, -PROPDMG), y = PROPDMG, fill = EVTYPE)) +
  geom_col(size = 2) +
  labs(title = "Top 8 Propertie Damage per Event Type", subtitle = "From 1952 to 2011") +
  scale_y_continuous(name="Sum of Propertie Damage", labels = scales::comma) +
  xlab("Event Type") +
  theme_classic(base_size = 7) +
  scale_fill_brewer(palette = "Set3") +
  guides(fill="none")

grid.arrange(plot1, plot2, plot3, nrow=3)

head(EventF)
head(EventI)
head(EventP)
