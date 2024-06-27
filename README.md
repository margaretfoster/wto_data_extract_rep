## Welcome!

This repository contains code to extract and clean speaker-paragraphs from meetings 1-117 of the World Trade Organization's [Committee on Trade and Development](https://www.wto.org/english/tratop_e/devel_e/d3ctte_e.htm).

To use it, start with 00_WTOAnalysis.R. 

This script calls two sub-processing scripts. 
The first (NERAnalysis-FirstEnt.) uses Spacy (via the SpacyR wrapper) to injest pdfs of the Committee minutes and extract the named entities.
Interested users can download the minute pdfs from the WTO's [documents](https://www.wto.org/english/res_e/res_e.htm) page.

The second (NERAnalysis-CleanupEnts.R) integrates the results of several extensive rounds of hand-validation, cleaning, and standardization.

In total, these produce a dataset of ~12k observations of speaker-paragraphs spanning semiannual meetings from 1995-2022, cleaned, validated, and with some country-level metadata.

The NER is complex, as speakers are typically identified as Delegates of states or as representatives of \[International/MNC \] organizations. 
The speaker assignments have been extensively hand-validated, although errors may remain.

### Corner Cases

Note that some corner cases remain, such as Directors \[of entities\].
Finally, certain individuals are sometimes referred to by title sometimes by their proper name. 
The cleaning has attempted to standardize these references. 

### Edge list

NERAnalysis-CleanupEnts.R attempts to identify salient internal references to produce an edgelist for network analysis. 

Note that the edge list is largely built from early-in-the-turn references, based on the observation that country names that occur early in a speaker-turn typically reference previous delegations. 
Conversely, country references that occur later in the speaker-turn often reference locations, events, or presentations.
