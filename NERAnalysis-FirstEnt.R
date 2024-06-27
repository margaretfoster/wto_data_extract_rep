rm(list=ls())

##install.packages("spacyr", dependencies=TRUE)

library(pdftools)
library(tidyr)
library(dplyr)
library(tidyverse)
library("xlsx")

## Data Process:
library("spacyr")

spacy_install() ## Installs and launches a python environment for SpacyR
## MJF NOTE: This is long, but helps avoid dependencies hell 

spacy_initialize(model = "en_core_web_sm")

##%%%%%%%%%%%%%
## Begin Data Processing
##%%%%%%%%%%%%%

dataPath = "./data/ENGLISH/" ## Fresh DL 7/25/22

minutes <- list.files(dataPath,
                      pattern=".pdf")
minutes

readandformat <- function(file){
  
  ##0-3: Read data, convert to vector, move into tibble, remove empty rows
  ## 4: for each row, look for a pattern of 1-3 numbers followed by a period OR
  ## one uppercase letter followed by a period. If found, use for "paranum"
  ## 5: Carry that begining digit over until number changes
  ## 6-7: collapse the  lines by paranum to recreate paragraph structure
  
  t <- pdf_text(file) %>% str_split("\n") %>% #(0)
    as_vector() %>% # (1)
    as_tibble_col(column_name = "row") %>% #(2) 
    filter(row != "") %>% #(3) 
    mutate(paranum = ifelse(
      grepl("^(\\d{1,3}|[A-Z]){1}\\.", row) == TRUE,
      str_extract(row, "^(\\d+|[A-Z])"),
      NA)) %>%  #(4)
    fill(paranum) %>% #(5) 
    group_by(paranum) %>% #(6) 
    summarise(paratext = #(7)
                paste(row, collapse = " "))
  return(t)
}

## Implement for the CTD Data:

ctdMinutes <- data.frame()

for(i in minutes){
  print(i)
  txt <- readandformat(paste0(dataPath, i))
  txt$doc <- i
  ctdMinutes <- rbind(ctdMinutes, txt)
}

dim(ctdMinutes) ##all minutes,  12,442
head(ctdMinutes)
table(ctdMinutes$paranum)

## Subset out paragraph numbers in A:Z, which are assumed to be agenda points, not debate:
ctdM2 <- ctdMinutes[!(ctdMinutes$paranum %in% LETTERS),] ## 11496

### NER Tagging:

titles <- c('Committee',  ## titles needed in this corpus & which SpacyR doesn't pick up by default
                'Secretariat', 
                'Chairman', 
                'Chairperson',
                'Members',
                'Member', 'Ambassador', 
                'Director-General',
                'Head', 
                'Deputy', 
                'Director',
                'Legal Affairs Division',
                "Uruguay",
                "Vanuatu",
            "ITTC",
            "Solomon Islands") ## Uruguay and Vanatau not recognized for some reason
  
not.ents <- c("the", ## These are identified as "entities" but are not speakers:
              "The", 
              "the_Enabling_Clause",
              "the_GCC_Customs_Union",
              "Turning",
              "TM",
              "TA",
              "GCC",
              "GATT",
              "Gatt_Article_xxiv",
              "the_Final_Report",
              "the_Doha_Round",
              "Scheme",
              "S&D",
              "the_Minutes_of_the")

ctdMparsing <- function(.data, titles){
  ## initilize objects: 
    entscol <- c()
    firstents <- c()
  for(r in 1:dim(.data)[1]){   
    print(r)
    ## do parsing:
     ptxt <- spacy_parse(as.character(.data[r, "paratext"]),
                              lemma = FALSE, 
                              entity = TRUE, 
                              nounphrase = TRUE)
    
      ptxt[which(ptxt$token %in% titles), ## flag the titles
                  "entity"] <- "PERSON_B"
    
     ## Extract:
    ents <-  entity_extract(ptxt) %>% 
      filter(!entity %in% not.ents) %>% ## remove over-identified non-entities
      select(entity) %>%
      as_vector() %>%
      unique() %>%
      paste(collapse = ", ")
    
    ents <- str_to_title(ents) ## Make casing consistent
    
  ##  print(ents)
     firstent <- unlist(strsplit(ents, ","))[1]
   
  ## print(firstent)
     
  # append entities to the newcol vector
  entscol <- append(entscol, ents)
  firstents <- append(firstents, firstent)
  
  }
  outlist <- list(ents=entscol, firsts=firstents)
    return(outlist)
}

## Process:  
ctdMP <- ctdMparsing(ctdM2, titles) ## Uses spacyR's tagger & also the custom list of titles

## Add:
ctdM <- cbind(ctdM2, ctdMP$firsts, ctdMP$ents) ## bind in entities and first entities

colnames(ctdM) <- c("paranum", "paratext", 
                     "doc", "firstent",
                     "ents") ## rename columns

## Some cleanup:
## Remove .pdf extensions from docID:
ctdM$doc <- sub('\\.pdf$', '',ctdM$doc) 
colnames(ctdM)

## Flag some supplemental docs:
sups <- c("WTCOMTDM110C1",
          "WTCOMTDM28A1",
          "WTCOMTDM3C1",
          "WTCOMTDM6C1")

ctdM <- ctdM %>% ## remove the supplmental docs
  filter(!doc %in% sups)



##%%%%%%%%%
## Speaker-turns
## Tag paragraphs that we think start a speaker-turn
## Implemented via: A speaker title in the first 50 characters
## (originally tried 75, but that was too long a window)
## List of titled derived by reading data rows 3,000 - 4,000

speaker.flags<- c("representative", "Chairman",
                  "Chairperson", 
                  "Secretariat",
                  "Director", 
                  "Committee",
                  "Deputy",
                  "Director-General",
                  "Ambassador", 
                  "Mrs",
                  "Mr",
                  "Dr")

s.pattern <- paste(speaker.flags, collapse="|")

ctdM$speaker.change <- ifelse(grepl(pattern=s.pattern, ## look for speaker "flag" in first 50 characters
      substr(ctdM$"paratext",1,50)), 1, 0)

table(ctdM$speaker.change) ## 0: 2654, yes: 8870

## Also flag metadata:

ctdM$isagenda <- ifelse(grepl(pattern="COMTD*", ## look for tags indicating header metadata
                                      x= substr(ctdM$"paratext",1,75)), ## has "COMTD in first 75 characters
                        1, 0)


table(ctdM$isagenda) ## 97 yes (low number becuase cut out all letter-numbered paragraphs)


## Tag some recurring meeting metadata:

ctdM$minutes.meta <- 0
ctdM[which(grepl(pattern="The meeting was adjourned", ctdM$paratext) == TRUE), "minutes.meta"] <- 1 
ctdM[which(grepl(pattern="It was so agreed", ctdM$paratext) == TRUE), "minutes.meta"] <- 1
ctdM[which(grepl(pattern="The agenda was adopted", ctdM$paratext) == TRUE), "minutes.meta"] <- 1
ctdM[which(grepl(pattern="The meeting was suspended", ctdM$paratext) == TRUE), "minutes.meta"] <- 1
ctdM[which(grepl(pattern="No Member took the floor", ctdM$paratext) == TRUE), "minutes.meta"] <- 1
table(ctdM$minutes.meta) ## 552
     
## Notifications:
## Tagging paragraphs that start with "notifications..."
## Note: revisit this, maybe they're tagged with letter starts

## "On behalf of"
ctdM$behalf <- 0
ctdM[which(grepl(pattern="on behalf of", ctdM$paratext) == TRUE), "behalf"] <-1 

table(ctdM$behalf) ## 376 yes

##%%%%%%%%%%%%%%%%%%%%%
## METADATA
##%%%%%%%%%%%%%%%%%%%%

dataPath2 <- "~/Dropbox/WTO/Speaker-Paragraph-Unit/"

meetings.meta <- read.csv(paste0(dataPath2,
                                 "meetingmetadf.csv"))

## remove numdate, add it in after new rows
meetings.meta$numdate <- NULL

docid <-c("WTCOMTDM114", "WTCOMTDM115", "WTCOMTDM116", "WTCOMTDM117")
date <- c(2021-03-29, 2021-06-28, 2021-11-10, 2022-03-25)
year <- c(2021, 2021, 2021, 2022)
meetingno <- c(114, 115, 116, 117)

more.meetings <- cbind(docid, date, year, meetingno)

meetings.meta <- rbind(meetings.meta, more.meetings)

meetings.meta$date <- as.Date(meetings.meta$date)
meetings.meta$numdate <- as.numeric(meetings.meta$date)

## Format consistency:
ctdM$doc <- toupper(ctdM$doc) ## Make sure case the same

## Check that there are not any gaps:
setdiff(ctdM$doc, 
        meetings.meta$docid)

setdiff(meetings.meta$docid,
        ctdM$doc)

table(meetings.meta$docid)
table(ctdM$doc)

ctdM <- left_join(ctdM, meetings.meta,
                  by=c("doc"= "docid"))

table(ctdM$year)


##%%%%%%%%%%%%%%%
#### Speaker Identification
##%%%%%%%%%%%%%%

## Want to get frequencies of only those firstents in a speaker-turn:
## in the subset of columns that have a speaker-chagne
head(ctdM)

tabl <- as.data.frame(table(ctdM[which(ctdM$speaker.change==1),]$firstent))
tabl <- tabl[rev(order(tabl$Freq)),]
head(tabl) ## 257 distinct first ents

dim(tabl) ##257 unique entities


## Tackle those that needed hand-tuning:

## Ivory coast:
ivoryc <- c("Côte", "Cote d'Ivoire", 
            "Cote_d'ivoire", 
            "Côte_d'ivoire")

ctdM[which(ctdM$firstent %in% ivoryc), "firstent"] <-"Cote_ivoire"

## Merge EU variants:

eus <- c("The_european_communities",
         "The_european_union",
         "The_european_commission", 
         "EC",
         "Ec",
         "The_european_communities_(_Netherlands",
         "European_communities",
         "The_european_community") 

ctdM[which(ctdM$firstent %in% eus), "firstent"] <- "The_european_union"

## Merge US variants:
uses <- c("Us",
          "U.s.", 
          "The_unites_states",
          "The_united_states", 
          "United_states")

ctdM[which(ctdM$firstent %in% uses), "firstent"] <- "The_united_states"


## Chair variants:
## Id variants:
unique(ctdM[grep(pattern="[Cc]hair",
          x=ctdM$firstent), "firstent"]) ## amost: but also wcp_chairs

ch <- c("Chairman", "Chairperson")
ctdM[which(ctdM$firstent %in% ch), "firstent"] <- "Chairperson"

## Ambiguous entries:
## Half are JAG half are LDCs; can probably take the second entry in the ents
ctdM[grep(pattern="Chairperson_of_the" ,
                 x=ctdM$firstent), ]
       
###Bolivia
bolv <- c("The_plurinational_state", "Bolivia")
ctdM[which(ctdM$firstent %in% bolv), "firstent"] <- "Bolivia"

### "Chinese Taipei" [Taiwan]
taiwan <- c("Chinese", "Taipei", "Taiwan")
ctdM[which(ctdM$firstent %in% taiwan), "firstent"] <- "Taiwan"

## Saudi Arabia:
sa <- c("The_kingdom_of_saudi_arabia", "The_saudi_arabia", "Saudi_arabia")
ctdM[which(ctdM$firstent %in% sa), "firstent"] <- "The_kingdom_of_saudi_arabia"

## Investigate: 
## "Chairperson_of_the_sub_-", mostly chairperson of  subcommmitte on LDCs
ctdM[which(ctdM$firstent=="Chairperson_of_the_sub_-"),]

ctdM[which(ctdM$firstent=="Members" & 
             ctdM$speaker.change==1), ] ## basically all in-text

## "The_sub_", mostly The Sub-Committee; sometimes Chairperson of Subcommittee.
ctdM[which(ctdM$firstent=="The_sub_-"),]

## Low This is a person, Development and Economic Research Division

## Bahrain
ctdM[grep(pattern="ahr",
          x=ctdM$firstent), "firstent"] <- "The_kingdom_of_bahrain"

ctdM[grep(pattern="central_african_republic",
          x=ctdM$firstent), "firstent"] <- "The_central_african_republic"

ctdM[grep(pattern="[Ss]olomon",
          x=ctdM$firstent), "firstent"] <- "Solomon_islands"

ctdM[grep(pattern="[Mm]oldova",
          x=ctdM$firstent), "firstent"] <- "The_republic_of_moldova"

ctdM[grep(pattern="[Vv]ezuela",
          x=ctdM$firstent), "firstent"] <- "Venezuela"

ctdM[grep(pattern="[Cc]ost_rica",
          x=ctdM$firstent), "firstent"] <- "Costa_rica"

ctdM[grep(pattern="[Aa]ntigua",
          x=ctdM$firstent), "firstent"] <- "Antigua_and_barbuda"

ctdM[grep(pattern="ussia", ## Catch variants of "Russia"
          x=ctdM$firstent), "firstent"] <- "Russian_federation"


## St_lucia
ctdM[grep(pattern="ucia", ## Catch variants of "St Lucia"
          x=ctdM$firstent), "firstent"]<- "Saint_lucia"

## Korea:
ctdM[grep(pattern="korea", ## Catch variants of South Korea
          x=ctdM$firstent), "firstent"] <- "The_republic_of_korea"

## People with inconsistent spelling:
ctdM[grep(pattern="élisle",
          x=ctdM$firstent), "firstent"] <- "Bélisle"

ctdM[grep(pattern="sakwe", # Chiedu_osakwe
          x=ctdM$firstent), "firstent"] <- "Osakwe"

ctdM[grep(pattern="ercier", # Claude Mercier
          x=ctdM$firstent), "firstent"] <- "Mercier"

ctdM[which(ctdM$firstent=="Low"),]
ctdM[which(ctdM$firstent=="Low"),"firstent"] <- "Development and Economic Research Division"

table(ctdM[which(ctdM$firstent=="Africa"), "speaker.change"]) ## Identification is accurate

## People whose names were split:

ctdM[grep(pattern="millan",
          x=ctdM$firstent), "firstent"] <-  "Mcmillan"

## Cosgrove-Sacks
ctdM[grep(pattern="Cosgrove",
          x=ctdM$firstent), "firstent"] <- "Cosgrove-Sacks"


## Which should be capitalized:
orgs <- c("Itc", "Wto", "Jag", "Icco", "Ersd", 
          "Unido", "Itu", "Idlo", "Caricom", "Unctad",
          "Fao", "Ctd", "Unece", "Undp", "Ico", 
          "Ittc", "Nepad", "Hlm", "laia")

ctdM[which(ctdM$firstent %in% orgs),]$firstent <- toupper(ctdM[which(ctdM$firstent %in% orgs),]$firstent)


people <- c("Tulloch", "Dagata", "Lanvin", "El_kabbaj", "Aboutahir", 
            "Cosgrove-Sacks","Burki", "Bélisle", "Stoler", "Paul_rolian",
            "Mercier", "Gurunlian", "Chiedu_osakwe", "Ben_fadhl", "Abbot",
            "Cristina_thayer", "Osakwe", "Teltscher", "Patrick_low", "Ismail",
            "Mariel_picado", "Picado")


ctdM$people <- 0
ctdM[which(ctdM$firstent %in% people), "people"] <- 1
table(ctdM$people) ##81

  ## Update:
tabl <- as.data.frame(table(ctdM[which(ctdM$speaker.change==1),]$firstent))
tabl <- tabl[rev(order(tabl$Freq)),]
head(tabl) ## 223 ents

library(countrycode)

tabl$codes <- countrycode(tabl$Var1,
                          origin = "country.name", 
                          destination = "iso3c",
                          nomatch = "NONST")

## Hand-code needed:
tabl[which(tabl$Var1=="The_european_union"), "codes"] <- "EUN"
tabl[which(tabl$Var1=="Republic_of_korea"), "codes"] <- "KOR"
tabl[which(tabl$Var1=="The_kingdom_of_saudi_arabia"), "codes"] <- "SAU"
tabl[which(tabl$Var1=="Saint_lucia"), "codes"] <- "LCA"
tabl[which(tabl$Var1=="The_central_african_republic"), "codes"] <- "CAF"
tabl[which(tabl$Var1=="The_republic_of_korea"), "codes"] <- "KOR"

### Add region and income level information:
library(wbstats)

metacols <- c("country", "region",
              "iso3c","income_level_iso3c" )

meta.inc <- wb_cachelist$countries[,metacols]

head(meta.inc)

key.df <- merge(tabl,
                meta.inc,
                by.x="codes",
                by.y="iso3c",
                all.x=TRUE)

key.df[which(key.df$codes=="EUN"), "region"] <- "Europe & Central Asia"
key.df[which(key.df$codes=="EUN"), "income_level_iso3c"] <- "HIC"

key.df[which(key.df$codes=="NONST"), "region"] <- "NONST"
key.df[which(key.df$codes=="NONST"), "income_level_iso3c"] <- "NONST"
key.df[which(key.df$codes=="NONST"), "country"] <- "NONST"
## Merge back into data:

ctdM <- ctdM %>%
  left_join(key.df,
            by=c("firstent"= "Var1"))


## Arrange to fill:
ctdM$paranum <- as.numeric(ctdM$paranum)
ctdM <- ctdM %>%
  arrange(year, meetingno, paranum)


## note in the data writeup that the
## error rate will be higher in the early Meetings
## because the format isn't yet standardized

## Now make presumed speaker field:
## ID lines with a speaker change, fill the first end
ctdM$pres.speaker <- ifelse(ctdM$speaker.change==1, 
                            ctdM$firstent, "")


behalfOf <- ctdM[which(ctdM$behalf==1),]

summary(as.numeric(ctdM$meetingno)) ## 1-117

dim(ctdM) ##11737 x 16 

ctdM$pid <- 1:nrow(ctdM)

##%%%%%%%%%%%%%
##Save data, this is a checkpoint
##%%%%%%%%%%%%%%
##write.csv2(ctdM,
##    file="~/Dropbox/WTO-Data/wtoCTDSpeakerParagraphMto117.csv")

##%%%%%%%%%%%%%%%%
## Oct 20, 2022
##%%%%%%%%%%%%%%%%%

##PID

dim(ctdM) ## 11737 x 21
nrow(ctdM)

## Simple paragraph-id code:
ctdM$pid <- 1:nrow(ctdM)

## Sample to test
sample.size <- ceiling(nrow(ctdM)*.25)

set.seed(102022)
sample.set <- sample(x= ctdM$pid, replace=FALSE, size= sample.size)

hist(sample.set) ## pretty uniformly distributed

ctdM$sample1 <- ifelse(ctdM$pid %in% sample.set, 1, 0)

## sub-samples:
ss.1 <- ceiling(sample.size*.25)

set.seed(102022)
ss.set1 <-  sample(x= sample.set, replace=FALSE, size= ss.1)

## subset 2
sample.set2 <- setdiff(sample.set, ss.set1) ## now take those out of the sample set

set.seed(102022)
ss.set2 <-  sample(x= sample.set2, replace=FALSE, size= ss.1)

## subset 3
sample.set2 <- setdiff(sample.set, c(ss.set1, ss.set2)) ## take out subset 1 &2
set.seed(102022)
ss.set3 <-  sample(x= sample.set2, replace=FALSE, size= ss.1)

## subset4
ss.set4 <- setdiff(sample.set, c(ss.set1, ss.set2, ss.set3)) ## take sets 1,2,3 out
## sample set 4 is what is left

## code into a column:
ctdM$sample.subset <- 0

ctdM[ctdM$pid %in% ss.set1, c("sample.subset")] <- 1
ctdM[ctdM$pid %in% ss.set2, c("sample.subset")] <- 2
ctdM[ctdM$pid %in% ss.set3, c("sample.subset")] <- 3
ctdM[ctdM$pid %in% ss.set4, c("sample.subset")] <- 4

hist(ctdM[which(ctdM$sample.subset==1),]$pid)
hist(ctdM[which(ctdM$sample.subset==2),]$pid)
hist(ctdM[which(ctdM$sample.subset==3),]$pid)
hist(ctdM[which(ctdM$sample.subset==4),]$pid)



write.xlsx2(ctdM, 
           file="./data/wtoCTDSpeakerParagraphMto117.xlsx")
