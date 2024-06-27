rm(list=ls())

###########################################
## This script:
## 0 Load pre-extracted and tagged paragraphs
## 1 Run the NER identification
## 2 Do some formatting cleanup to prepare for messy deduplification
## 3 Use text similarity to cluster identified entities to [somewhat] facilitate hand-dedup
## 4 Write out a [large] spreadsheet of identified entities for hand dedup
## (dedup by hand b/c lots of duplicated, ambig, or fragmentary entities & little metadata )
## 5 Read back in the spreadsheet mapping NER ents and manually clarified ents
## 6 Iterate steps 3 & 4 again to pick up stragglers
## 7 Merge mapped entities into underlying data
## 8 Convert underlying data into a series of speaker-entities they referenced list
## Metadata including: year, meeting, speaker country code [for presentation], pid for linking to rest of data
## 9 save the speaker-entity lists into a separate spreadsheet for network analysis

###########################
## Load libraries:

library(tidyr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(readxl)

## Data Process:
library("spacyr")
spacy_install() ## Installs and launches a python environment for SpacyR
## NOTE: This ads a bit of time, but helps avoid dependencies hell

## Select 2 to initiatlize a conda environment:
spacy_initialize(model = "en_core_web_sm") ## initialize model used for NER;

##%%%%%%%%%%%%%
## 1  Data Processing & NER Identification
##%%%%%%%%%%%%%

dataPath = "./data/" ## 

ctm <- readxl::read_xlsx(paste0(dataPath, ## converted CTD paragraphs
                                "ValidationNoteswtoCTDSpeakerParagraphMto117.xlsx"))

####################################
### NER Tagging:
## Remove hyphens in the texts
## otherwise spacy will break entities & find lots of fragments
####################################

## Reading in supplemental entities specific to this context:
## WTO entities:
## (Slightly edited the list to make all plurals singular, which are the expected reference 
## format in the texts)
wto_org <- read_excel(paste0(dataPath,"WTO-Data/WTOOrgChartExcel4523.xlsx"),
                      col_names = FALSE) ## 47

colnames(wto_org) <- c("wto_ent")
wto_org$wto_ent <- as.character(wto_org$wto_ent)

## Read in trade and development global regimes list:
## (Slightly redacted to remove generic entries 
## [eg: "international NGO"; "social benefit organization"]
## that are unlikely to be described in those terms


global_regimes <- read_excel(paste0(dataPath,
                                    "~/Dropbox/WTO-Data/GlobalRegimesInstitutions_GlobalDev.xlsx"),
                      col_names = FALSE) ## 149
colnames(global_regimes) <- c("global_ent")
global_regimes$global_ent <- as.character(global_regimes$global_ent)

#### Consistent formatting:

ctm$paratext <- gsub(pattern="-",
     replacement="",
     ctm$paratext)

global_regimes$global_ent <- gsub(pattern="-",
                     replacement="",
                     global_regimes$global_ent)

wto_org$wto_ent <- gsub(pattern="-",
                        replacement="",
                        wto_org$wto_ent)

## underscores instead of spaces b/c that's what spacy will do:
global_regimes$global_ent <- gsub(pattern=" ",
                                  replacement="_",
                                  global_regimes$global_ent)

wto_org$wto_ent <- gsub(pattern=" ",
                        replacement="_",
                        wto_org$wto_ent)

## ID some titles that spacy might miss, but are important in this context
titles <- c('Committee',  ## titles needed in this corpus & which SpacyR doesn't pick up by default
            'Secretariat', 
            'Chairman', 
            'Chairperson',
            ##'Members',
              'Member', 
            'Heads_of_State',
            "SubCommittee",
            'Ambassador', 
            'DirectorGeneral',
              ##  'Head', 
            "Deputy_Directory_General",
              'Director',
                'Legal_Affairs_Division',
                "Uruguay",
                "Vanuatu",
            "ITTC",
            "Solomon_Islands") ## Uruguay and Vanatau not recognized for some reason
  
## Entities that are never meeting minute speakers:
not_speakers <- c("the", ## These are identified as "entities" but are not speakers:
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
              "Program")

## function to run spacyR over all of the CTD paragraphs:
## using entity consolidate:
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
                              nounphrase = TRUE) ##%>%
##       entity_consolidate() 
    
      ptxt[which(ptxt$token %in% c(titles, 
                                   wto_org$wto_ent, 
                                   global_regimes$global_ent)), ## flag the titles
                  "entity"] <- "PERSON_B"
    
     ## Extract:
    ents_r2 <-  entity_extract(ptxt) %>% 
      filter(!entity %in% not_speakers) %>% ## remove over-identified non-entities
      filter(!grepl("^Wt", entity)) %>% ## remove internal references to documents
      filter(!grepl("Annex*",  entity)) %>% ## variations on "annex" , "annexes"
      filter(!grepl("Restri*",  entity)) %>% ## variations on "annex" , "annexes"
      filter(!grepl("Sw F", entity)) %>% ## Refs to swiss franc expenses
      select(entity) %>%
      as_vector() %>%
      unique() %>%
      paste(collapse = ", ")
    
    ents_r2 <- str_to_title(ents_r2) ## Make casing consistent
  
    ## remove 
  ##  print(ents)
    ## firstent <- unlist(strsplit(ents, ","))[1]
   
  ## print(firstent)
     
  # append entities to the newcol vector
  entscol <- append(entscol, ents_r2)
  
  }
    
  outlist <- list(ents_r2=entscol)
    return(outlist)
}

## Call function:
ctdMP <- ctdMparsing(ctm, titles) ## Uses spacyR's tagger & also the custom list of titles

## remove references to "Wt_/_" ids:
ents_r2 <- gsub(pattern="(Wt)[^.?, ]*",
                replacement="",
                ctdMP$ents_r2) 

## replace underscores with spaces, for country matches

ents_r2 <- gsub(pattern="_",
                replacement=" ",
                ents_r2) 

ents_r2 <- str_to_title(ents_r2)

## Add:
ctdM <- cbind(ctm, ents_r2) ## bind in entities and first entities


ctdM$firstent<- gsub(pattern="_",
                     replacement=" ",
                     ctdM$firstent) 

ctdM$firstent <- str_to_title(ctdM$firstent)

colnames(ctdM)

head(ctdM)

## make backup (comment out if in replication/not development mode):
##ctdMP.backup <- ctdMP
##ctdMP <- ctdMP.backup ## (reload from checkpoint, comment out when not needed)

####

### Clean up and some formatting:
### Debugging first entities:

allents_df <-tibble(ctdMP$ents_r2, text =ctdMP$ents_r2 ) %>%
  unnest_tokens(ents, text, token = 'regex', pattern=",") ## make df of identified ents to work with

allents_df$ents <- str_squish(allents_df$ents) ## leading, trailing,and more than one internal WS

## Remove some common junk entries:
allents_df$ents <- gsub(pattern="Wt_\\/.*$", ## "Wt_/" to end of entry
                      replacement="",
                      allents_df$ents,
                      ignore.case=TRUE) 

## Replace underscores with spaces, for country matches
allents_df$ents <- gsub(pattern="_", 
                      replacement=" ",
                      allents_df$ents) 

## Remove "the" prefix:
allents_df$ents <- gsub(pattern="the ",
                      replacement=" ",
                      allents_df$ents,
                      ignore.case=TRUE) 

## Remove posessives:
## (actually everything after apostrophes until end of line)
allents_df$ents <- gsub(pattern="'.*$",
                      replacement=" ",
                      allents_df$ents,
                      ignore.case=TRUE) 

## Everything from open parens to end of line:
allents_df$ents <- gsub(pattern="\\(.*$",
                      replacement=" ",
                      allents_df$ents,
                      ignore.case=TRUE) 


## Everything from close parens to end of line:
allents_df$ents <- gsub(pattern="\\).*$",
                      replacement=" ",
                      allents_df$ents,
                      ignore.case=TRUE) 

allents_df$ents <- str_to_title(allents_df$ents)

allents_df$ents <- str_squish(allents_df$ents) 


## Review a summary frequency table:

ents_freq <- allents_df %>%
  count(ents, sort = TRUE) 

summary(ents_freq$n) ## min 1, median 1, max 5225k, n=5728

ents_freq$id <- 1:nrow(ents_freq) ## ID to link if needed down the line

## Write out if needed (comment out if not)
##write.csv(ents_freq,
##          file="WTO-NER-AllEnts.csv")
 
## %%%%%%%%%%%%%%%%%%%%%%%%%%
## 3 Cluster entities with similar text to [somewhat] facilitate 
## the hand-matching step
## Note there are several approaches here
## the one that worked best was bigrams + kmeans
##%%%%%%%%%%%%%%%%%%%%%%%%%%%

## All-to-all string distance 

library("tidystringdist")
library(stringr)

##First take out all three-letter acroymns:
## (Reason: shortness creates lots of false clusters)

ents_freq$v1.length <- str_length(ents_freq$ents)
   
match <- ents_freq[which(ents_freq$v1.length>3),]$ents %>% 
  tidy_comb_all(text)  %>% ## combines all possible couples
  tidy_stringdist() 

## output:
## osa = optimal string alignment, distance between two strings via "cost" of operations
## needed to transform
## lv =  Levenshtein distance 

colnames(match) ## v1, v2, osa, lv, dl, hamming, lcs, qgram, cosin, jaccard, jw, soundex

##summary(match$osa) ## min 1, max 121
 
##potentials <- filter(osa < 10) %>%
##  arrange(osa) ## 1,224,557

summary(match$lv) ## Lower is better b/c a measure of edit distance

potentials_lv <- match %>% filter(lv < 5) %>%
  arrange(lv) ## ~40k

summary(potentials_lv$lv)
hist(potentials_lv$lv)

table(potentials_lv$lv)

## 627 1, 31k 4
## Note: need to take out the 1 lv that have numbers
## or are "session" ;"chapter" ,"item", "Agenda"
## b/c those are close in edit distance but reference to different things

### 3b: ngram shingling
## Take the list of entities
## convert into bi or trigrams
## tf-idf on the n-grams
## kmeans on the tf-idf

ents_bigrams <- ents_freq %>%
  unnest_tokens(shingle, ents, token="character_shingles", n=3) %>%
  filter(!is.na(shingle)) %>%
  group_by(id) ## Note "N" is the  

length(unique(ents_bigrams$id)) ## 5,654

dtm <- ents_bigrams %>%
  count(id, shingle) %>%
  cast_dtm(id, shingle, n)

dim(dtm) # 5654 x 5329, so rows are ids

dist<- dist(dtm, method = "euclidian") ## distance between rows


fit_hclust <- hclust(d = dist, method = "complete") ## hierarchical clustering ## 5,329k objects

fit_hclust ## 5645 objects

head(fit_hclust)

## Kmeans:

class(dtm)

### 1,000 clusters:
## Chosen b/c prior is that there are more unique entities than duplicates
## and 1,000 is about as high as kmeans will converge on this data

k_means_data <- as.matrix(dtm) ## from t(dtm)
dim(k_means_data) ## 5654 x 5329

set.seed(121922)

kfit_1000 <- kmeans(k_means_data, 1000)

clusters <- as.data.frame(cbind(cluster=kfit_1000$cluster, 
                                id=rownames(kfit_1000$cluster))) ## 5454 x 1

clusters$id <- rownames(clusters)


clusters <- merge(clusters, ents_freq[,c("id", "ents")],
                  by="id")

table(clusters$cluster) ## check out a few clusters:


## Check out some clusters to assess meaningfulness:
clusters[which(clusters$cluster==1000),] ## Guinea related names

clusters[which(clusters$cluster==532),] ## land related names [icelad, switzerland...]

clusters[which(clusters$cluster==425),] ## "committee on" ents

clusters[which(clusters$cluster==977),] ## relate to "meeting"

clusters[which(clusters$cluster==807),] ## reviews
clusters[which(clusters$cluster==885),] ## divisions 

clusters[which(clusters$cluster==996),] 

clusters[which(clusters$cluster==969),] ## one-off

clusters[which(clusters$cluster==771),] ## divisions 


#######
## Experimental fit of clusters test:  take cosine similarity of the rows?
## Commented out b/c useful to test for fit, but didn't end up expediting the merge process 

## library(text2vec)

## cluster.1000.ids <- as.numeric(unique(clusters[which(clusters$cluster==1000), "id"]))

## c.1000.raw <- k_means_data[which(rownames(k_means_data)%in% cluster.1000.ids),] ## 5 x 5329k
## c.1000.raw <- c.1000.raw[,which(colSums(c.1000.raw)>=1)] ## get rid of columns that are all 0
## dim(c.1000.raw) ##5x28 

## c.1000.sims <- sim2(c.1000.raw, method="cosine", norm="l2")
## c.1000.sims[lower.tri(c.1000.sims)] <- 0
## c.1000.sims

### sims function:
## requires: 
## clusters, k_means_data
##cluster.sims <- function(clusternum){

## cluster.ids <- as.numeric(unique(clusters[which(clusters$cluster==clusternum), "id"]))
## cluster.1000.ids

## c.1000.raw <- k_means_data[which(rownames(k_means_data)%in% cluster.1000.ids),] ## 5 x 5329k

## c.1000.raw <- c.1000.raw[,which(colSums(c.1000.raw)>=1)] ## get rid of columns that are all 0

## dim(c.1000.raw) ##5x28 

## c.1000.sims <- sim2(c.1000.raw, method="cosine", norm="l2")
##c.1000.sims[lower.tri(c.1000.sims)] <- 0

##c.1000.sims

###############################################
## 4: Merge clusters and duplicated entities

## 4a: Write clusters and then merge the spreadsheet via --brute force--- by hand

## Uncomment if redoing NER, commented out 2/23 to avoid
## overwriting the clusters already connected
##write_csv(clusters,
##          file="clustersToMerge.csv")

## 4b: Read in hand-clustered entities
## and do some light formatting
## read in spreadsheet mapping NER-ided Ents to Ents to use:

library(readxl)

ents.merged <- read_excel("./data/clustersToMerge_merged.xlsx") ## 5166k

ents.merged$`Which to use` <- tolower(ents.merged$`Which to use`)
ents.merged$`Which to use` <- ifelse(is.na(ents.merged$`Which to use`), 
                                     ents.merged$ents, 
                                     ents.merged$`Which to use`)

ents.merged$`Which to use` <- tolower(ents.merged$`Which to use`)


## Connect to the underlying data:

key <-  ctdM[,c("pid", "ents")]

## match processing:
key$ents <- str_squish(key$ents) ## leading, trailing,and more than one internal WS

## Some cleanup:
key$ents <- gsub(pattern="Wt_\\/.*$", ## "Wt_/" to end of entry
                        replacement="",
                        key$ents,
                        ignore.case=TRUE) 

## replace underscores with spaces, for country matches
key$ents <- gsub(pattern="_",
                        replacement=" ",
                        key$ents) 

## remove "the" prefix:

key$ents <- gsub(pattern="the ",
                        replacement=" ",
                        key$ents,
                        ignore.case=TRUE) 

## Remove posessives:
## (actually everything after apostrophes until end of line)
key$ents <- gsub(pattern="'.*$",
                        replacement=" ",
                        key$ents,
                        ignore.case=TRUE) 

## everything from open parens to end of line:
key$ents <- gsub(pattern="\\(.*$",
                        replacement=" ",
                        key$ents,
                        ignore.case=TRUE) 


## everything from close parens to end of line:
key$ents <- gsub(pattern="\\).*$",
                        replacement=" ",
                        key$ents,
                        ignore.case=TRUE) 

key$ents <- str_to_title(key$ents)

key$ents <- str_squish(key$ents) 

## Merge the data together:

tmp2 <-  key %>%
  separate_rows(ents, sep=",") 

tmp2$ents <- str_squish(tmp2$ents) ## remove leading spaces for everything that followed a comma

## merge in with the ents.merged

tmp3 <- tmp2 %>%
  left_join(ents.merged[,c("ents", "Which to use")], 
            by=c("ents"= "ents"))

## Now take only those with NA in the which to use
## see how common:
## (Note, will be the 2-3 letter entries)

nas <- tmp3[is.na(tmp3$`Which to use`),]

nas.freq <- as.data.frame(table(nas$ents))

nas.freq <- nas.freq[order(nas.freq$Freq,  ## 394
                           decreasing=TRUE),]

## write out to manually clean up the second round:
## Most of these were too short for the clustering
## but are important (eg: EU & variants)

##write.csv(nas.freq, 
##          file="entityResolvePt2.csv")

## read-in the merged nas.freq:
mergedr2 <- read_excel("/.data/entityResolvePt2_merged.xlsx") ## 394

mergedr2$`Which to use` <- ifelse(is.na(mergedr2$`Which to use`), 
                                  mergedr2$Ent, 
                                  mergedr2$`Which to use`)

mergedr2$`Which to use` <- tolower(mergedr2$`Which to use`)


for.adjmat <- tmp3 %>%
  left_join(mergedr2[,c("Ent", "Which to use")], 
            by=c("ents"= "Ent"))

## patch the second set of "which to use" into the merged set:
for.adjmat$`Which to use.x` <- ifelse(is.na(for.adjmat$`Which to use.x`), ## is NA 
                                     for.adjmat$`Which to use.y`, ## if yes: pull from the `Which to use.y'
                                     for.adjmat$`Which to use.x`) ##if no, keep original 


nas <- for.adjmat[is.na(for.adjmat$`Which to use.x`),]
table(nas$ents) ## numbers and empties

### Clean up and turn into something that can be made into an adjacency network

for.adjmat$`Which to use.y` <- NULL## remove residual column

for.adjmat <- for.adjmat %>% 
  left_join(ctdM[,c("pid", "pres.speaker", "meetingno", "codes", "year")],
            by=c("pid"= "pid")) 

colnames(for.adjmat) <- c("pid","NERents", "mergedref", "presumed.speaker", 
                    "meetingno", "speakerccode", "year") ## "mergedref" is the manually deduped list

## clean up a duplicated European community/communities
## clean up a space in some LDCS
unique(for.adjmat[grepl("european", for.adjmat$mergedref),"mergedref"])
unique(for.adjmat[grepl("ldc", for.adjmat$mergedref),"mergedref"])
unique(for.adjmat[grepl("korea", for.adjmat$mergedref),"mergedref"])

for.adjmat[which(for.adjmat$mergedref=="european communities"), "mergedref"] <- "european community"
for.adjmat[which(for.adjmat$mergedref=="ldc s"), "mergedref"] <- "ldcs"
for.adjmat[which(for.adjmat$mergedref=="plurinational state of bolivia"),
           "mergedref"] <-  "bolivia"
for.adjmat[which(for.adjmat$mergedref=="south korea" |
                   for.adjmat$mergedref=="korea"|
                   for.adjmat$mergedref=="republic of korea"), "mergedref"] <- "republic of korea"
## Republics of: (Maps doesn't like)
for.adjmat[which(for.adjmat$mergedref=="united republic of tanzania"),
"mergedref"] <-  "tanzania"
for.adjmat[which(for.adjmat$mergedref=="democratic republic of the congo"),
           "mergedref"] <-  "congo-kinshasa" ## not pretty
## now remove all "republic of " to help streamline the country IDs:


## remove intermediaries:
rm(tmp2, tmp3) ## intermediary merges

## save the adjmat data:
##write.csv(for.adjmat,
##          file="wtoInTextRefs.csv")

### 4/3/23: TO HERE

## Want to take the "for adjmat" data object and run through for
## geographic locations (countries & capitals)
## and the WTO entities

ent.freq2 <- as.data.frame(table(for.adjmat$mergedref)) ##2787
ent.freq2 <- ent.freq2[rev(order(ent.freq2$Freq)),]

## Tag which are countries, WTO entities, etc
library(maps)

data(world.cities)

location.db <- world.cities[,c("name", "country.etc")]

location.db[(tolower(location.db$name)=='india'),] ##there's an "India, Gabon"

location.db[grepl("United",location.db$country.etc),] ### united states in as US;
## United Kingdom in as "UK"; going to need to hand ID "Ivory Coast"


for.adjmat$is.country <- for.adjmat$mergedref %in% tolower(location.db$country.etc)
for.adjmat$is.city <- for.adjmat$mergedref %in% tolower(location.db$name)

## observe that there are quite a few cities that are also country names
## baking in an assumption that in this context,
##nouns on both lists are more likely to be countries than cities:
for.adjmat[which(for.adjmat$is.country ==TRUE &
                   for.adjmat$is.city == TRUE), "is.city"] <- FALSE

## The EU corner-case:
for.adjmat[which(for.adjmat$mergedref=="european community"), "is.country"] <- TRUE 
for.adjmat[which(for.adjmat$mergedref=="european union"), "is.country"] <- TRUE 
for.adjmat[which(for.adjmat$mergedref=="united states"), "is.country"] <- TRUE 
for.adjmat[which(for.adjmat$mergedref=="united kingdom"), "is.country"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="cote d'ivoire"), "is.country"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="cote d'ivoire"), "is.country"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="republic of korea"), "is.country"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="kyrgyz republic"), "is.country"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="hong kong"), "is.city"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="trinidad"|
                   for.adjmat$mergedref=="tobago"), "is.country"] <- TRUE
for.adjmat[which(for.adjmat$mergedref=="trinidad"|
                   for.adjmat$mergedref=="tobago"), "is.city"] <- FALSE


table(for.adjmat$is.country) ## 12717 true
table(for.adjmat$is.city) ## 1125 true (once get rid of "country" city names)

to.check <- unique(for.adjmat[,c("mergedref", "is.country", "is.city")]) ## for a hand-check

## cleanup:
rm(world.cities)
rm(to.check)
rm(parsed)
rm(nas.freq)

## Tag other entities of interest:
## Generic terms that are common in the WTO-org chart:
## Or indicate previous rules
wto_ents1 <- c("general council", 
               "committee", "ctg", "council", 
               "conference", "session", "working group")

external.ingos <- c("united nations", "imf",
                    "bank","g7", "jag") ## can't just use "un" b/c fragment
## Agreements:
agreements_ents <- c("rta", "asean", "african",
                     "gatt", "doha", "regional",
                     "ministerial", "decision")

for.adjmat$is.wto.body <- grepl(paste(wto_ents1, collapse= "|"),
               for.adjmat$mergedref)

table(for.adjmat$is.wto.body)## true: 4485

for.adjmat$is.agreement.ref <- grepl(paste(agreements_ents, collapse= "|"),
                                for.adjmat$mergedref)

table(for.adjmat$is.agreement.ref) ## TRUE: 2673
###################################
### Some summaries of the raw data
##################################

freq.tab <-as.data.frame(table(for.adjmat$mergedref))
freq.tab <- freq.tab[order(freq.tab$Freq, decreasing=TRUE),]
head(freq.tab)

hist(freq.tab$Freq)

####################
## Write out the adjacency matrix
#####################
## using txt becuase the java-based write excel
## options run out of memory

## reorder columns to be clearly a list of ties:
colnames(for.adjmat)
for.adjmat <- for.adjmat[,c(4, 3, 1, 5, 2,6:11 )]

## double check that we have all of the pids in the underlying
## "ctm" data:
setdiff(ctm$pid, for.adjmat$pid) ##in x but not y ## 0


library(data.table)

write.table(for.adjmat,
           file="./data/ctdEdgeList.txt",
           col.names = TRUE, 
           row.names = FALSE,
           sep = ";")

