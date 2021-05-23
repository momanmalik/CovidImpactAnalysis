---
  title: "LDSS Project Code"
output: html_document
---
```{r setup, include=FALSE}

library(reshape)
library(dplyr)
library(magrittr)
library(scales)
library(gridExtra)
library(data.table)
library(usmap) #import the package
library(ggplot2) #use ggplot2 to add layer for visualization
library(plyr)
library(data.table)
library(TSstudio)
library(tinytex)

#Sources
#https://www.cidrap.umn.edu/news-perspective/2020/05/us-job-losses-due-covid-19-highest-great-depression
#https://www.theverge.com/2020/11/5/21551683/uber-q3-2020-earnings-revenue-loss-delivery
```

```{r}

apple_mobility <- read.csv("apple mobility trends.csv") 

Unemployement <- read.csv("unemployment rate.csv")

us_covid <- read.csv("us_counties_covid19_daily.csv")


```

```{r}
#Clean data
#apple_mobility2 <- select(apple_mobility,-c(geo_type,sub.region,country,alternative_name)) 


Unemployement_before_covid <- slice(Unemployement,1:868)
Unemployement_during_covid <- slice(Unemployement,868:875)
head(us_covid)

apple_mobility2 <- select(apple_mobility,-c(sub.region,alternative_name)) 

#cases from jan 21st 2020 --> march 10th 2020
setDT(us_covid)
Total_case = us_covid[ , .(cases = mean(cases)), by = .(state)] %>%
  arrange(desc(cases))

Total_case %>%   
  mutate(cases=round(Total_case$cases,digits = 0)) %>%
  head(10)

```

```{r}
ggplot(Total_case,aes(x=state,y=cases))+ geom_bar(stat="identity", position=position_dodge(width=100)) + scale_x_discrete(guide = guide_axis(n.dodge=1))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("States") + ylab("Covid Cases") + ggtitle("Number of Cases by State")
```

```{r}
ggplot(Unemployement_before_covid,aes(x=UNRATE),binwidth=20) + geom_histogram()
```

```{r}

ggplot(Unemployement_during_covid,aes(x=DATE,y=UNRATE)) + geom_point()

```

```{r}
H3N2_Pandemic <- slice(Unemployement_before_covid,241:248)
bp<- ggplot(H3N2_Pandemic, aes(x=DATE, y=UNRATE, fill=DATE))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)
pie
ggplot(H3N2_Pandemic,aes(x=UNRATE,y=DATE),binwidth=20) + geom_bin2d() 

```

```{r}
walking <- filter(apple_mobility2, region == "United States", transportation_type == "walking") 
walking <- select(walking, -c(country, geo_type, region, transportation_type))
walking <- t(walking)
walking <- as.data.frame(walking)
head(walking,10)
```

```{r}
walking      <- as.data.table(t(walking))
walking$date <- rownames(walking)
walking      <- melt(walking, id.vars=c("date"))
colnames(walking)[3] <- "percent"
walking$date <- as.numeric(gsub('X', '', walking$date, ignore.case=TRUE))
walking %<>% 
  mutate(walking, variable=as.Date(variable, format="X%m.%d.%y"))
head(walking,10)
```

```{r}
ggplot(walking, aes(x=variable, y=percent)) + geom_path() + ylim(0, 150) + 
  ggtitle("Change in volume of routing requests (walking) in the United States") + xlab("Date") + ylab("Percent Change")
```

```{r}
driving <- filter(apple_mobility2, region == "United States", transportation_type == "driving") 
driving <- select(driving, -c(country, geo_type, region, transportation_type))
driving <- t(driving)
driving <- as.data.frame(driving)
driving <- as.data.table(t(driving))
driving$date <- rownames(driving)
driving      <- melt(driving, id.vars=c("date"))
colnames(driving)[3] <- "percent"
driving$date <- as.numeric(gsub('X', '', driving$date, ignore.case=TRUE))
driving %<>% 
  mutate(driving, variable=as.Date(variable, format="X%m.%d.%y"))
driving
ggplot(driving, aes(x=variable, y=percent)) + geom_path() + ylim(0, 150) + 
  ggtitle("Change in volume of routing requests (driving) in the United States") + xlab("Date") + ylab("Percent Change")
```

```{r}
transit <- filter(apple_mobility2, region == "United States", transportation_type == "transit") 
transit <- select(transit, -c(country, geo_type, region, transportation_type))
transit <- t(transit)
transit <- as.data.frame(transit)
transit      <- as.data.table(t(transit))
transit$date <- rownames(transit)
transit      <- melt(transit, id.vars=c("date"))
colnames(transit)[3] <- "percent"
transit$date <- as.numeric(gsub('X', '', transit$date, ignore.case=TRUE))
transit %<>% 
  mutate(transit, variable=as.Date(variable, format="X%m.%d.%y"))
ggplot(transit, aes(x=variable, y=percent)) + geom_path() + ylim(0, 150) +
  ggtitle("Change in volume of routing requests (transit) in the United States") + xlab("Date") + ylab("Percent Change")
```

```{r}
transit <- filter(apple_mobility2, region == "United States", transportation_type == "transit") 
transit <- select(transit, -c(country, geo_type, region, transportation_type))
transit <- t(transit)
transit <- as.data.frame(transit)
transit      <- as.data.table(t(transit))
transit$date <- rownames(transit)
transit      <- melt(transit, id.vars=c("date"))
colnames(transit)[3] <- "percent"
transit$date <- as.numeric(gsub('X', '', transit$date, ignore.case=TRUE))
transit %<>% 
  mutate(transit, variable=as.Date(variable, format="X%m.%d.%y"))
head(transit,10)

plot <- ggplot() + 
  geom_line(data=transit, aes(x=variable, y=percent, color='Transit')) + 
  geom_line(data=walking, aes(x=variable, y=percent, color='Walking')) +
  geom_line(data=driving, aes(x=variable, y=percent, color='Driving')) + ylim(0, 150) 
plot <- plot + ggtitle("Apple Mobility Trends in the United States") 
plot <- plot + xlab("Date") + ylab("% Change") + scale_y_continuous(breaks=seq(0,200,10)) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y", guide = guide_axis(n.dodge = 2)) 
plot <- plot + geom_hline(yintercept=100)
plot
```

```{r}
makeDF <- function(target, df, transport) {
  
  index <- df$region %in% target
  bestWalkLine <- df[index, ]
  bestWalkLine <- filter(bestWalkLine, transportation_type == transport) 
  bestWalkLine <- select(bestWalkLine, -c(country, geo_type, transportation_type))
  bestWalkLine
  bestWalkLine <- t(bestWalkLine)
  bestWalkLine <- as.data.frame(bestWalkLine)
  colnames(bestWalkLine) <- bestWalkLine[1,]
  bestWalkLine <- bestWalkLine[-1,]
  bestWalkLine
  bestWalkLine$date <- rownames(bestWalkLine)
  bestWalkLine %<>%   
    mutate(bestWalkLine, date=as.Date(date, format="X%m.%d.%y"))
  
  bestWalkLine[,1:(ncol(bestWalkLine)-1)] <- sapply(bestWalkLine[,1:(ncol(bestWalkLine)-1)],as.numeric)
  names(bestWalkLine)<-str_replace_all(names(bestWalkLine), c(" " = "_" , "," = "" ))
  bestWalkLine
  
  return(bestWalkLine)
}
```

```{r}
walkingMean <- filter(apple_mobility, geo_type == "country/region", transportation_type == "walking") 
walkingMean <- select(walkingMean,-c(geo_type,transportation_type,alternative_name, sub.region, country)) 

```
```{r}
walkingMean$mean <- format(round(rowMeans(walkingMean[,-1], na.rm=TRUE) - 100, 2), nsmall = 2)
reqd <- as.vector(c("region","mean")) 
walkingMean <- walkingMean[,reqd]    
head(walkingMean,10)
```

```{r}
walkingMean$mean <- as.numeric(as.character(walkingMean$mean))
```

```{r}
p <-ggplot(walkingMean, aes(x=reorder(region,-mean), mean))+geom_bar(stat="identity", position=position_dodge(width=100)) + scale_x_discrete(guide = guide_axis(n.dodge=1))
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Countries") + ylab("Mean of % Change in Volume") + ggtitle("Mean of percent change in volume of directions requests (walking) from January 2020 to 2021")
p
```

```{r}
transitMean <- filter(apple_mobility, geo_type == "country/region", transportation_type == "transit") 
transitMean <- select(transitMean,-c(geo_type,transportation_type,alternative_name, sub.region, country)) 


transitMean$mean <- format(round(rowMeans(transitMean[,-1], na.rm=TRUE) - 100, 2), nsmall = 2)
reqd <- as.vector(c("region","mean")) 
transitMean <- transitMean[,reqd]    

transitMean$mean <- as.numeric(as.character(transitMean$mean))
head(transitMean,10)

p <-ggplot(transitMean, aes(x=reorder(region,-mean), mean))+geom_bar(stat="identity", position=position_dodge(width=100)) + scale_x_discrete(guide = guide_axis(n.dodge=1))
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Countries") + ylab("Mean of % Change in Volume") + ggtitle("Mean of percent change in volume of directions requests (transit) from January 2020 to 2021")
p
```

```{r}
drivingMean <- filter(apple_mobility, geo_type == "country/region", transportation_type == "driving") 
drivingMean <- select(drivingMean,-c(geo_type,transportation_type,alternative_name, sub.region, country)) 


drivingMean$mean <- format(round(rowMeans(drivingMean[,-1], na.rm=TRUE) - 100, 2), nsmall = 2)
reqd <- as.vector(c("region","mean")) 
drivingMean <- drivingMean[,reqd]    

drivingMean$mean <- as.numeric(as.character(drivingMean$mean))
head(drivingMean,10)

p <-ggplot(drivingMean, aes(x=reorder(region,-mean), mean))+geom_bar(stat="identity", position=position_dodge(width=100)) + scale_x_discrete(guide = guide_axis(n.dodge=1))
p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Countries") + ylab("Mean of % Change in Volume") + ggtitle("Mean of percent change in volume of directions requests (driving) from January 2020 to 2021")
p
```

```{r}
total <- merge(walkingMean, transitMean, by="region", all=TRUE)
total <- merge(total, drivingMean, by="region", all=TRUE)
colnames(total)[2:4] <- c("walking", "transit", "driving")

```

```{r}

df.long<-melt(setDT(total))
df.long$region <- factor(df.long$region, levels=unique(df.long$region))
head(df.long,10)
```

```{r}
barPlot <- ggplot(df.long,aes(x=reorder(region, value, sum, na.rm=TRUE), y=value,fill=variable)) + geom_col(stat="identity") + 
  theme(axis.text.y = element_text(size=5, angle = 0))+
  xlab("Countries") + ylab("Mean of % Changed") + coord_flip() + ggtitle("Average percentage change in volume of directions requests for all 3 transportation types (walking, driving, transit) from January 2020 to 2021")
barPlot
```
