# Domestic Space

This repository contains files pertaining to the lab domestic space project, principally relating to annotation done prior to July 2023, and to the development of regression models using those annotations. Several older approaches and methods are included as well. 

Note that the actual annoation data is included here, which should be considered prior to taking the repo public.

This is a static upload; I developed and ran this code locally, and it's very possible that something is missing here. 

 If you run any of this code yourself, you'll generate `.rds` files in a variety of places where that represents a considerable time saving for re-running the code; there are options in individual scripts to overwrite this behaviour if you need to regenerate these files. Note also that these files can be quite large (10+ gB) so you may want to either disable this behaviour or delete the files when done with the project.

### `domestic_space.Rproj`

This is an R Studio project file I used for this project. You probably don't need to use it, but it will ensure that the paths used in all the code here resolve correctly. 

Note that I don't store the parsed (or unparsed, which isn't used here) corpus in this directory; my assumption is that those files are located at `../FullChadwyckBritishParse`, that is, in a directory at the same level as this `domestic_space` repo.

## Model and annotation directories

### `annotations`

This directory contains all the annotation data, broken down into the original 1000-tags, the non-domestic tags (both in a single sheet and individually) and the unfiltered passages (again, collectively and in one sheet)

### `annotations_evaluation`

Contains scripts (a bit messy) for generating plots of each individual annotator's tag breakdown, annotator agreement, and similar

### `domestic_regression_model`

Contains model code and resulting plots

### `nondomestic_passage_extraction`

Code used to extract passages and then filter out those that included our domestic terms

### `unfiltered_passage_extraction`

Code for extracting purely random passages for determining baseline rates

## Miscellaneous directories

### `operationalizing_model`

This contains code to build an intial hand-parameterized model based off the use of key domestic terms and a gradual decay over time. This model was used in fall 2022 to help think through the assumptions underlying the project and the modelling we intended to do. This code is a bit rough.

### `window_correlates`

Another early exploration, this code looks at relative word frequencies within and outside of windows surrounding key domestic terms. This code is also a little rough; this approach wasn't too successful.

### `presentation_vizualization`

These are just visualization for lab presentations; for the most part this isn't a programmatically managed or used directory. Included here is the graphviz file for the tagging flowchart (this was a pain to create; I recommend not making flowcharts)

