# Introduction
This repository contains code for the COVID-19 Response Dashboard developed by Mathematica with The Rockefeller Foundation .  

This dashboard tracks Covid-19 case counts, testing and vaccination response, and other contextual indicators in regions where The Rockefeller Foundation and its partners operate. Indicators are pulled or calculated from publicly available sources. For the United States, vaccine equity is a major focus of the dashboard. U.S. vaccination rates by race/ethnicity are calculated or imputed based on underlying data from the Centers for Disease Control and Prevention, the Kaiser Family Foundation, and the U.S. Census Bureau. Detailed source information is provided in the `data/df_dashboard_data_dictionary.xlsx` and `www/2022-02-11 data sources description.pdf` files.  

For more information, please contact Laura Blue at LBlue@mathematica-mpr.com and Anitha Sivasankaran at ASivasankaran@mathematica-mpr.com  

# Installation

## Software
To run the code in this repository you will need the following software installed:
* [Docker](https://docs.docker.com/get-docker/)

## Installation instructions
1. Run `git config core.hooksPath hooks/` to activate the scripts in the hooks directory. These hooks prevent the following:
* Forgetting to update renv.lock when package changes are made
* Forgetting to update the data file when the code is changed or updated.

2. *.csv files are tracked using Git Large File Storage. To install and use:
  
  * Download here: https://git-lfs.github.com/
  * Change `git config lfs.activitytimeout 3000`
  * To add other files to lfs, run 'git lfs track "*.fileExtension"'.
  * Make sure .gitattributes is tracked: `git add .gitattributes`

3. Docker

This project uses Docker, so you can run (or develop) the app in RStudio or deploy the app locally with the Dockerfile. Since this project using R 3.5, this is especially helpful with the upgrade to R 4.0 (otherwise, make sure to set Tools --> Global Options --> R Version to R 3.5)

To build the Docker image, git bash into the main project folder and run 
`docker build -t nameforimage .` 
where nameforimage is what you want to name this image 

Then to run a container from this image, run 
`docker run -d -e PASSWORD=myspecialpassword -p 8787:8787 -v local/path/to/folder:/home/rstudio nameforimage`
where local/path/to/folder is the path to the main project folder on your computer and nameforimage is the name of the image you created 

You can then go to `localhost:8787` to develop locally. Enter rstudio as your username and `myspecialpassword` (or whatever you set to be your password). 

If you want to view the app in shiny server, change the port mapping from 8787:8787 to 3838:3838 and then go to localhost:3838.

# Contents
* Dockerfile  
  + Dockerfile that contains the commands for a user to assemble a Docker image for local development and general deployment.
* data/
  + for storing intermediate and final datasets and dictionaries used by the Dashboard  
* src/
  + helper scripts for pulling and processing the data for the dashboard  
* create_df_dashboard.R  
  + main script for pulling and processing the data for the dashboard  
* ui.R  
  + Shiny App UI  
* server.R  
  + Shiny App Server
* www/
  + 2022-02-11 data sources description.pdf: PDF describing the data sources used in the Dashboard


