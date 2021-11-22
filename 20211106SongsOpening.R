library(tidyverse)
library(ARTofR)
library(dplyr)
xxx_title1("Questions")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                                  QUESTIONS                               ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

xxx_box1("See")

#...............................................................................
#                                                                              .
# What does the average album look like: how many sad, what balance
# Do albums with similar songs do better
# Do albums have multiple hit songs or only 1/2 songs make it ?
# Do artists experiment after a hit or do more of the same? Are they scare or overconfident?
# Do negative songs do better?
# Do winter songs do better?
# When does a artist shine to fame: first or next or last?
# Is the same song a hit in multiple places? What are unique geographic hits?
# 
#                                                                              .
#...............................................................................

xxx_divider1("First Steps")
#..........................First Steps...........................
# Collate all songs in one file
# Collate all artist data in one file
# Collate all album data in one file
# Get all album and artist data in front of each song
# Divide them based on need.


countrywisecollated <- read_csv("Final database.csv") 
countrywisecollated$ReleaseDate<-as.Date(as.character(countrywisecollated$Release_date))
#countrywisecollated$ReleaseDate<-format(as.Date(as.character(countrywisecollated$Release_date),  "%Y-%m-%d")) This was giving some problem.
countrywisecollated$YearRelease<-format(countrywisecollated$ReleaseDate, format= "%Y")

xxx_title3("When hit")
##~~~~~~~~~~~~~~~~~~
##  ~ When hit  ----
##~~~~~~~~~~~~~~~~~~
## filter global For each artist, club by year, note first year; note year of biggest hit

artistyearfile<-data.frame()

globalcollated<-filter(countrywisecollated,Country=="Global")
globalcollated2<-select(globalcollated, 1:10,YearRelease)
for (i in 1:length(unique(globalcollated2$Artist))){
 # for (y in 1:length(unique(globalcollated$YearRelease))){
    
    df1<-filter(globalcollated2,Artist==unique(globalcollated2$Artist)[i])
    df2<-df1%>%group_by(YearRelease)%>%summarise(top=max(Popularity))
    df2<- df2[order(-df2$top),] #Arrange in Descending order
    artistyearfile[i,1]<-  unique(globalcollated$Artist)[i]
    artistyearfile[i,2]<-  min(df1$YearRelease) #First song
    artistyearfile[i,3]<-  df2$YearRelease[1]#Top Song Year
  #}
    print(i)
}

colnames(artistyearfile)<-c("Artist","Since","TopHit")
artistyearfile<-mutate(artistyearfile, Diff= TopHit-Since)

##You can see the difference and show 
# Who got it soonest.
# How many got it soonest
# How many in 1 year
# Who got it late
# How mamy after 5 years
# How many total? 
# How many unqiue artists
# How many years of data


##WHen do you know you have failed?

#### Compiling files


tracks <- read_csv("tracks.csv", col_types = cols(release_date = col_character()))
###TODO: Release Date: year + Y-m-d
tracks2 <- read_csv("spotify_tracks.csv") ##This has lyrics
colnames(tracks2)[5]<-"id_artists" ##TO make it similar to TRacks file.

tracks$release_date<-as.Date(as.character(tracks$release_date)) #this is now as per date
tracks$YearRelease<-format(tracks$release_date,format="%Y") #Get year also

artistyearfile2<-data.frame()

#globalcollated<-filter(countrywisecollated,Country=="Global")
globalcollated2<-select(tracks, 1:8,YearRelease)
for (i in 32507:length(unique(globalcollated2$id_artists))){ ### RUN THIS
  df1<-filter(globalcollated2,id_artists==unique(globalcollated2$id_artists)[i])
  df1<- df1[order(-df1$popularity),] #Arrange in Descending order
  artistyearfile2[i,1]<-  unique(globalcollated2$id_artists)[i]
  artistyearfile2[i,2]<-  min(df1$YearRelease,na.rm = TRUE) #First song #Easy, direct
  artistyearfile2[i,3]<- min(df1$release_date,na.rm=TRUE)
  artistyearfile2[i,4]<-  df1$YearRelease[1]#Top Song Year
  artistyearfile2[i,5]<- df1$release_date[1] #NA hai toh avoid kar do .
  artistyearfile2[i,6]<- max(df1$YearRelease,na.rm = TRUE) #First song #Easy, direct
  artistyearfile2[i,7]<- max(df1$release_date,na.rm=TRUE)
  #}
  print(i)
} ###There is some problem with the dates in this place
colnames(artistyearfile2)<-c("Artist","Year1","Date1","GoldenYear","GoldenDate","LastYear","LastDate")
write.csv(artistyearfile2,"artistyearfile2.csv")

str(artistyearfile2)
artistyearfile2$Date1<-as.Date(artistyearfile2$Date1)

# Sequence ----------------------------------------------------------------


## In order to do our learning hypothesis, we might also need the month wise learning thing, basically to see whether they have learnt. Year wise or song wise. SOng wise is easy. Similar to the above, we can do Number and then have a new dataset.

globalcollated3<-select(tracks, 1:8,YearRelease)
globalcollated3<-globalcollated3[complete.cases(globalcollated3),] #This will remove redundancies
df5<-df4 #We have derived this from m=1 thing

for (i in 1:length(unique(globalcollated3$id_artists))){
  df1<-filter(globalcollated3,id_artists==unique(globalcollated3$id_artists)[i])
  df1<- df1[order(df1$release_date),] #Arrange in Descending order
  df2<-cbind.data.frame(unique(df1$release_date),1:length(unique(df1$release_date)))# Cbind Data Frame helps
  colnames(df2)<-c("release_date","seq")
  df1<-merge(df1,df2,by="release_date",all.x = TRUE) #This will allow us to combine the 1/2/3 with the main frame
  
  for (m in 1:length(unique(df1$release_date))){
    df4<-filter(df1,release_date==unique(df1$release_date)[m])
    df4<-filter(df4, popularity==max(df4$popularity)) #Get the song with max popularity for that date
    df5<-rbind(df5,df4) #Append it to a dataset
  }
  print(i)
  print(m)
}
write.csv(df5,"HighestPerDatePerArt.csv")

###Filter out artists that have only one song, or only one release date. We can get artists that have a 2. That means it has at least one. 
df5<-read_csv("HighestPerDatePerArt.csv")
df6<-filter(df5,seq==2) ##To ensure there is at least one more song and not just the first one
df6<-select(df6,id_artists,name) # Name will tell us which ones are there in this 
df6<-merge(df6,df5,by="id_artists",all.y=TRUE) #This is to get traits of the song
df6<-filter(df6,!is.na(df6$name.x)) #This file now has all artists that have more than 1 song in the database.
# To study role of sequence on popularity
df6<-merge(df6,tracks,by="id", all.x=TRUE,all.y=FALSE)
reg1<-lm(popularity.x~seq+duration_ms.x+danceability+energy+key+loudness+mode+speechiness+acousticness+
     instrumentalness+instrumentalness +valence+tempo+time_signature+explicit.x+YearRelease.x,data=df6)
summary(reg1) ###yes, seq has a positive coefficient










