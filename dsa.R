library(leaflet)
library(dplyr)
library("PostcodesioR")
library(geosphere)
library(tidyverse)

Obtainlat<- function(pc) {
  a<-postcode_lookup(pc)
  c(a$latitude, a$longitude) 
}
df2<- read.csv("./dva.csv",na.strings=c('','NA'))

names <- c("Name", "Conducted.male", "Passes.male", "Pass.rate.male", 
           "Conducted.female", "Passes.female", "Pass.rate.female", 
           "Conducted.total", "Passes.total", "Pass.rate.total" 
           )
names(df2)<- names
df2Name <- df2$Name
df2 <- mutate_all(df2, function(x) as.numeric(as.character(x)))
df2$Name <- df2Name 
#df2<- df2 %>% mutate(Pass.rate.male= df2$Passes.male/df2$Conducted.male)%>%
#  mutate(Pass.rate.female= df2$Passes.female/df2$Conducted.female)

months<- df2$Name[2:13]
centres <- unique(df2$Name)

centres <- centres[! centres %in% months]

df2.bycentre <- na.omit(subset(df2, Name %in% centres))
df2<- na.omit(df2)


df2.passrates.by.gender<- subset(df2.bycentre, select=c(4,7))
t.test(df2.passrates.by.gender$Pass.rate.male, df2.passrates.by.gender$Pass.rate.female)


df<- read.csv("./practical.csv") 
df<- subset(df, select= c(Name, Lat,Long)) 

df3<- right_join(df, df2.bycentre, by = "Name", copy = FALSE, suffix = c(".x", ".y"))

pc<- "E3 5NT"
gender <- "Male"
loc<- Obtainlat(pc)

df3 <- df3 %>% rowwise() %>% 
        mutate(Dist = (distHaversine(loc, c(Lat, Long)))) %>%
        arrange(Dist)
df3$Dist <- df3$Dist/1000

df4 <- head(df3, 10)
mapdf <- subset(df4, select=c(1,2,3))
df4 <- subset(df4, select=c(1, 6, 9, 12, 13))
if (gender=="Male"){
res <- df4[which.max(df4$Pass.rate.male),c(1,2,5)]
} 
if (gender=="Female"){
  res <-  df4[which.max(df4$Pass.rate.female),c(1,3,5)]
}
print(res)

e<- which(grepl(res[1], df2$Name))
f<- e+1
g<- f+11
df5<- df2[f:g,]
if (gender=="Male"){
  date <- df5[which.max(df5$Pass.rate.male),c(1,4)]
} 
if (gender=="Female"){
  date <-  df5[which.max(df5$Pass.rate.female),c(1,4)]
}


res_date<- date[1]
res_date<-str_split(res_date, " ", n = 3, simplify = TRUE)
res_date<- res_date[2]


col <- 4
if (gender=="Female") {
  col <- 7
}
dates = seq(as.Date("01-04-2019",  format = "%d-%m-%Y"), length.out = 12, by = "month")
plot(df5[,col], type="l", ylab="Passrate", xlab=dates)
df6 <- data.frame(dates, y=df5[,col]) 
ggplot(df6, aes(dates, y)) + geom_line(color="red", size=2) + scale_x_date(date_labels = "%b-%Y") + 
    labs(x = "Month", y="Pass rate") + theme_dark()

print(res)
print(res_date)
mapdf %>%         
  leaflet() %>%         
  addTiles() %>%         
  addCircles(color="blue")  