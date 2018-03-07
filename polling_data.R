###Scrape Public Opinion Data from Wahlrecht.de
###Creation of dataset including values for the six major parties (CDU/CSU, SPD, Grüne, FDP, Linke, AfD + Sonstige) of all polling institutes

library(rvest)

#Identify matching pattern to scrape all poll results from wahlrecht.de since 2009
institute <- c("allensbach", "allensbach/2013", "emnid", "emnid/2013", "forsa", "forsa/2013", "politbarometer", "politbarometer/politbarometer-2013", "gms", "dimap", "dimap/2013", "insa")


url_list <- paste0("http://www.wahlrecht.de/umfragen/",institute,".htm")

#Scrape tables from homepage
public_opinion_df <- list()
for(i in seq_along(url_list)){
  url_parsed <- read_html(url_list[i])
  tables <- html_table(url_parsed,fill=TRUE)
  public_opinion <- tables[[2]]
  public_opinion_df[[i]] <- public_opinion
}

#Clean data:

#Remove second column
for(i in 1:12){
  public_opinion_df[[i]] <- public_opinion_df[[i]][-2]
}

#Remove column with name ".1"
drop <- c(".1")

for(i in 1:12){
  public_opinion_df[[i]] <- public_opinion_df[[i]][,!(names(public_opinion_df[[i]]) %in% drop)]
}

#Remove header rows
for(i in 1:12){
  public_opinion_df[[i]] <- public_opinion_df[[i]][-c(1:3),]
}

#Remove remaining incorrect rows
public_opinion_df[[4]] <- public_opinion_df[[4]][-c(1),]
public_opinion_df[[9]] <- public_opinion_df[[9]][-c(1),]
public_opinion_df[[12]] <- public_opinion_df[[12]][-c(1),]

#Add names of Institutes
public_opinion_df[[1]]["institute"] <- "Allensbach" 
public_opinion_df[[2]]["institute"] <- "Allensbach"
public_opinion_df[[3]]["institute"] <- "Emnid"
public_opinion_df[[4]]["institute"] <- "Emnid"
public_opinion_df[[5]]["institute"] <- "Forsa"
public_opinion_df[[6]]["institute"] <- "Forsa"
public_opinion_df[[7]]["institute"] <- "Politbarometer"
public_opinion_df[[8]]["institute"] <- "Politbarometer"
public_opinion_df[[9]]["institute"] <- "GMS"
public_opinion_df[[10]]["institute"] <- "Dimap"
public_opinion_df[[11]]["institute"] <- "Dimap"
public_opinion_df[[12]]["institute"] <- "INSA"






#Since the column are not aligned properly, I decided to restructure the complete data frame by hand - although not particularily beautiful, it works by now...
Datum <- c(public_opinion_df[[1]][,1],public_opinion_df[[2]][,1],public_opinion_df[[3]][,1],public_opinion_df[[4]][,1],public_opinion_df[[5]][,1],public_opinion_df[[6]][,1],public_opinion_df[[7]][,1],public_opinion_df[[8]][,1],public_opinion_df[[9]][,1],public_opinion_df[[10]][,1],public_opinion_df[[11]][,1],public_opinion_df[[12]][,1])

cdu_csu <- c(public_opinion_df[[1]][,2],public_opinion_df[[2]][,2],public_opinion_df[[3]][,2],public_opinion_df[[4]][,2],public_opinion_df[[5]][,2],public_opinion_df[[6]][,2],public_opinion_df[[7]][,2],public_opinion_df[[8]][,2],public_opinion_df[[9]][,2],public_opinion_df[[10]][,2],public_opinion_df[[11]][,2],public_opinion_df[[12]][,2])

spd <- c(public_opinion_df[[1]][,3],public_opinion_df[[2]][,3],public_opinion_df[[3]][,3],public_opinion_df[[4]][,3],public_opinion_df[[5]][,3],public_opinion_df[[6]][,3],public_opinion_df[[7]][,3],public_opinion_df[[8]][,3],public_opinion_df[[9]][,3],public_opinion_df[[10]][,3],public_opinion_df[[11]][,3],public_opinion_df[[12]][,3])

gru <- c(public_opinion_df[[1]][,4],public_opinion_df[[2]][,4],public_opinion_df[[3]][,4],public_opinion_df[[4]][,4],public_opinion_df[[5]][,4],public_opinion_df[[6]][,4],public_opinion_df[[7]][,4],public_opinion_df[[8]][,4],public_opinion_df[[9]][,4],public_opinion_df[[10]][,4],public_opinion_df[[11]][,4],public_opinion_df[[12]][,4])

fdp <- c(public_opinion_df[[1]][,5],public_opinion_df[[2]][,5],public_opinion_df[[3]][,5],public_opinion_df[[4]][,5],public_opinion_df[[5]][,5],public_opinion_df[[6]][,5],public_opinion_df[[7]][,5],public_opinion_df[[8]][,5],public_opinion_df[[9]][,5],public_opinion_df[[10]][,5],public_opinion_df[[11]][,5],public_opinion_df[[12]][,5])

lin <- c(public_opinion_df[[1]][,6],public_opinion_df[[2]][,6],public_opinion_df[[3]][,6],public_opinion_df[[4]][,6],public_opinion_df[[5]][,6],public_opinion_df[[6]][,6],public_opinion_df[[7]][,6],public_opinion_df[[8]][,6],public_opinion_df[[9]][,6],public_opinion_df[[10]][,6],public_opinion_df[[11]][,6],public_opinion_df[[12]][,6])

afd <- c(public_opinion_df[[1]][,7],public_opinion_df[[2]][,8],public_opinion_df[[3]][,7],public_opinion_df[[4]][,8],public_opinion_df[[5]][,7],public_opinion_df[[6]][,8],public_opinion_df[[7]][,7],public_opinion_df[[8]][,8],public_opinion_df[[9]][,9],public_opinion_df[[10]][,7],public_opinion_df[[11]][,8],public_opinion_df[[12]][,9])

befragte <- c(public_opinion_df[[1]][,9],public_opinion_df[[2]][,10],public_opinion_df[[3]][,9],public_opinion_df[[4]][,11],public_opinion_df[[5]][,10],public_opinion_df[[6]][,10],public_opinion_df[[7]][,10],public_opinion_df[[8]][,10],public_opinion_df[[9]][,11],public_opinion_df[[10]][,9],public_opinion_df[[11]][,10],public_opinion_df[[12]][,12])

zeitraum <- c(public_opinion_df[[1]][,10],public_opinion_df[[2]][,11],public_opinion_df[[3]][,10],public_opinion_df[[4]][,12],public_opinion_df[[5]][,11],public_opinion_df[[6]][,11],public_opinion_df[[7]][,11],public_opinion_df[[8]][,11],public_opinion_df[[9]][,12],public_opinion_df[[10]][,10],public_opinion_df[[11]][,11],public_opinion_df[[12]][,13])

institute <- c(public_opinion_df[[1]][,11],public_opinion_df[[2]][,12],public_opinion_df[[3]][,11],public_opinion_df[[4]][,13],public_opinion_df[[5]][,12],public_opinion_df[[6]][,12],public_opinion_df[[7]][,12],public_opinion_df[[8]][,12],public_opinion_df[[9]][,13],public_opinion_df[[10]][,11],public_opinion_df[[11]][,12],public_opinion_df[[12]][,14])

df <- data.frame(Datum,cdu_csu,spd,gru,fdp,lin,afd,befragte,zeitraum, institute)

rm(list=setdiff(ls(), "df"))

#Data Cleaning

#Drop rows with results from Federal Election

df_po <- df[!(df$befragte=="Bundestagswahl"),]

#Remove % from columns
for(i in 2:7){
  df_po[,i] <- gsub("\\ %", "", df_po[,i])
  df_po[,i] <- gsub(",", ".", df_po[,i])
  df_po[,i] <- as.numeric(df_po[,i])
}
#NAs aren't a big deal, because they are being created for AfD values pre-2013. Replace them with 0 afterwards
df_po$afd[is.na(df_po$afd)] <- 0

#Create a "sonstige" Value
df_po$sonstige <- 100-rowSums(df_po[2:7])


#Remove missing values in "Befragte" variable
df_po <- df_po[!is.na(df_po$sonstige),]

df_po$befragte <- gsub("[.]", "", df_po$befragte)
df_po$befragte[grepl("[?]", df_po$befragte, ignore.case=FALSE)] <- NA

df_po$befragte <- gsub("[^[:digit:]., ]", "",df_po$befragte )

df_po$befragte <- as.numeric(df_po$befragte)

#Change Zeitraum variable zu string:
df_po$zeitraum <- as.character(df_po$zeitraum)

#Change Datum variable zu datetime:
df_po$Datum <- gsub("[.]", "-", df_po$Datum)
df_po$Datum <- as.Date(df_po$Datum, "%d-%m-%Y")

#Check type of every column
str(df_po)

