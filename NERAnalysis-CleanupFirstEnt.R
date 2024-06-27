rm(list=ls())

## This script:
## Load pre-extracted and tagged paragraphs
## update the NER identification

library(pdftools)
library(tidyr)
library(dplyr)
library(tidyverse)

## Data Process:
library("spacyr")

spacy_install() ## Installs and launches a python environment for SpacyR
## MJF NOTE: This is long, but helps avoid dependencies hell 

spacy_initialize(model = "en_core_web_sm")

##%%%%%%%%%%%%%
## Begin Data Processing
##%%%%%%%%%%%%%

dataPath = "~/Dropbox/WTO-Data/" ## 

ctm <- readxl::read_xlsx(paste0(dataPath,
                                "ValidationNoteswtoCTDSpeakerParagraphMto117.xlsx"))


### NER Tagging:
## first remove hyphens in the texts, because spacy will break them up
## and we have a lot in the entities

ctm$paratext <- gsub(pattern="-",
     replacement="",
     ctm$paratext)


titles <- c('Committee',  ## titles needed in this corpus & which SpacyR doesn't pick up by default
            'Secretariat', 
            'Chairman', 
            'Chairperson',
            ##'Members',
              'Member', 
            "Sub-Committee",
            'Ambassador', 
                'Director-General',
                'Head', 
            "Deputy Directory General",
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
              "the_Minutes_of_the",
              "Session",
              "Notifications",
              "Workshop",
              "Agenda",
              "Program",
              "Restricted")

fragments <- c("Annex", 
               "Restricted",
               )

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
    ents.r2 <-  entity_extract(ptxt) %>% 
      filter(!entity %in% not.ents) %>% ## remove over-identified non-entities
      filter(!grepl("^Wt", entity)) %>% ## remove internal references to documents
      filter(!grepl("Annex*",  entity)) %>% ## variations on "annex" , "annexes"
      filter(!grepl("Sw F", entity)) %>% ## Refs to swiss franc expenses
      select(entity) %>%
      as_vector() %>%
      unique() %>%
      paste(collapse = ", ")
    
    ents.r2 <- str_to_title(ents.r2) ## Make casing consistent
  
    ## remove 
  ##  print(ents)
    ## firstent <- unlist(strsplit(ents, ","))[1]
   
  ## print(firstent)
     
  # append entities to the newcol vector
  entscol <- append(entscol, ents.r2)
  
  }
  outlist <- list(ents.r2=entscol)
    return(outlist)
}

## Process:  
ctdMP <- ctdMparsing(ctm, titles) ## Uses spacyR's tagger & also the custom list of titles

## remove references to "Wt_/_" ids:
ents.r2 <- gsub(pattern="(Wt)[^.?, ]*",
                      replacement="",
                      ctdMP$ents.r2) 

## replace underscores with spaces, for country matches

ents.r2 <- gsub(pattern="_",
                replacement=" ",
                ents.r2) 

ents.r2 <- str_to_title(ents.r2)

## Add:
ctdM <- cbind(ctm, ents.r2) ## bind in entities and first entities


ctdM$firstent<- gsub(pattern="_",
        replacement=" ",
       ctdM$firstent) 

ctdM$firstent <- str_to_title(ctdM$firstent)

colnames(ctdM)

ctdM[which(ctdM$pres.speaker=="Point_2(C"),]

ctdM[grep(pattern="ivoire", x= ctdM$firstent), c("firstent", "pres.speaker")]

speakers <- as.data.frame(table(ctdM$pres.speaker)) ## most common speakers matches
speakers <- speakers[order(speakers$Freq, decreasing=TRUE),]

compare.fe <- as.data.frame(table(ctdM$firstent)) ## most common speakers matches
compare.fe <- compare.fe[order(compare.fe$Freq, decreasing=TRUE),]

compare.fe2 <- compare.fe %>% 
  filter(!grepl("^Wt",Var1 )) 

dim(compare.fe) ## 704 x 2
dim(compare.fe2) ## 645 x 2

## 
## WB stats

library(wbstats)

metacols <- c("country", "region",
              "iso3c","income_level_iso3c" )

meta.inc <- wb_cachelist$countries[,metacols]

head(meta.inc)

### Copy over the hand-cleaning from the firstents file:


