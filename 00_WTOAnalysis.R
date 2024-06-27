## This script to streamline the process through the WTO data

###############################
## Part 0: Data Links
###############################
## Data is in ./data/ENGLISH/ for pdfs
## ./data/ for processed .xlsx
###############################
## Part 1: Data Preparation
###############################

source("NERAnalysis-FirstEnt.R") ## processes the meeting minute pdfs into dataframe
## Takes: path to directory with downloaded WTO CTD meeting minutes (1-117)
## returns: ./data/wtoCTDSpeakerParagraphMto117.xlsx--  11,737 rows

## Out of script: convert  wtoCTDSpeakerParagraphMto117.xlsx to
## ValidationNoteswtoCTDSpeakerParagraphMto117.xlsx
## by hand-checking the NER identification 

source("NERAnalysis-CleanupEnts.R") ## additional processing of in-paragraph references; 
22## NOTE: REQUIRES EXTENSIVE HAND VALIDATION of NER ENTITIES.
## Code to produce validation sets is commented out
## Also produces speaker-reference edgelist
## Takes: 
## (1) ValidationNoteswtoCTDSpeakerParagraphMto117.xlsx -- underlying data; 11,737 rows
## (2)  WTOOrgChartExcel4523.xlsx -- Slightly modified WTO org chart 
## (3) GlobalRegimesInstitutions_GlobalDev.xlsx -- Slightly modified list of global trade regimes
## (4) clustersToMerge_merged.xlsx -- hand-validated clusters from the kmeans clustering
## (5) entityResolvePt2_merged.xlsx -- second pass on the hand-validated entities

## Produces: 
## (1) WTO-NER-AllEnts.csv -- list of all IDed entities, for validation
## (2) clustersToMerge.csv Goes through the produced in-paragraph entities, coverts into ngrams
## and then uses Kmeans to produce 1k clusters of similar names. 
## (3) entityResolvePt2.csv -- second pass for the entity validations
## (4) ctdEdgeList.txt -- ;-separated txt speaker-reference edgelist with some metadata tags
## ctdEdgeList.txt uploaded to Google Drive as a spreadsheet. ~56,364 rows
## (5+) Some supplemental CSVs of notable but fragmentary keywords for qualitative summaries
## (eg: African; director) 

##############
## PT 2: Text Analysis
##############

## Text Reuse Modeling
