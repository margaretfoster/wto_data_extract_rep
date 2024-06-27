rm(list=ls())

library(stm)
library(xtable)
library(quanteda)

## Script to exract CTD Metadata

dataPath1<- "~/Dropbox/WTO-Data/rdatas/"

load(paste0(dataPath1,
            "processedTextforSTM.Rdata"))

ls()

class(out)
dim(out$meta)
colnames(out$meta)
head(out$meta$docid)

## Extract the pre-generated meeting-level metadata:
## docid, date, year, meeting no, numdate
## Then country-speakers: 
## iso3x, country, region, income level

meeting.meta <- c("docid", "date", 
                   "year", "meetingno", 
                  "numdate")

states.meta <- c("firstent", "iso3c",
                 "country", "region",
                 "income_level_iso3c")


meeting.meta.df <- unique(out$meta[,meeting.meta])

states.meta.df <- unique(out$meta[,states.meta])


dim(meeting.meta.df) ## 114x5
dim(states.meta.df)

dataPath2 <- "~/Dropbox/WTO/Speaker-Paragraph-Unit/"
write.csv(meeting.meta.df,
          row.names=FALSE,
          file=paste0(dataPath2,
                      "meetingmetadf.csv"))


write.csv(states.meta.df,
          row.names=FALSE,
          file=paste0(dataPath2,
                      "speakersmetadf.csv"))
