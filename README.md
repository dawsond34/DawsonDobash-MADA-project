# Overview

Here is my MADA project. The information and background for this project is within the introduction section of the manuscript document. I have included the data sets I will possibly use within the raw data folder. I also added some code to the processing script on reading in the data ans showing what the data looks like. 

# Pre-requisites

Please load the R_project called MADA_project.Rproj before running any code. Also check within the data folder there are 9 data sets within the raw data folder. 

To run the code, you first need to run the processing script first in the processing_code folder within the code folder. This will create the cleaned up processed data set from all of the raw data sets. This new processed data set will be put into the processed_data folder. Once we ran the processed script and created the new processed data, we can run the analysis script. 

# Structure

* All data goes into the subfolders inside the `data` folder.
* All code goes into the `code` folder or subfolders.
* All results (figures, tables, computed values) go into `results` folder or subfolders.
* All products (manuscripts, supplement, presentation slides, web apps, etc.) go into `products` subfolders.
* See the various `readme.md` files in those folders for some more information.

# Content 

### Code
**processingscript.R** cleaning data and creating a processed data set
**analysisscript.R** Exploratory analysis and other analyses
**main_analysis.R** Main analysis of project

### Data
**raw_data folder** Includes all the raw data sets
**processeddata.rds** the processed data created from the processing script


### Results

This folder contains all of the figures and tables created from the analysis script

### Products

This folder contains the manuscript and the supplemental materials

**SUPPLEMENTAL MATERIALS NOT FINISHED AND A WORK IN PROGRESS**