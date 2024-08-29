# HMS Smoke PM2.5 Analysis App V2

This application provides tools for analyzing HMS Smoke and PM2.5 data, as well as daily air quality, meteorological data, and back trajectory analysis.

## Features

- HMS Smoke PM2.5 Analysis
- Daily Air Quality, Meteorological, HMS Smoke, and Back Trajectory Analysis
- Interactive plots and data tables
- Data filtering and download capabilities

## Tutorial

For a detailed walkthrough of the app's features and usage, please watch our tutorial video:

[![HMS Smoke PM2.5 Analysis App Tutorial](https://img.youtube.com/vi/86-No7dH3OE/0.jpg)](https://www.youtube.com/watch?v=86-No7dH3OE)

## Screenshots

Here are some screenshots of the app in action:

![HMS Plot](https://github.com/user-attachments/assets/84c6428a-4ec0-4281-ac51-b51e10ddd92c)

<img width="1600" alt="Met_Data" src="https://github.com/user-attachments/assets/bdf05a09-bef4-4e0e-a07f-ab01ef323b3f">

<img width="981" alt="Trajectory" src="https://github.com/user-attachments/assets/fc70a543-3708-44d5-8672-6e66ed5cf32c">


## Installation and Setup

### 1. Clone the Repository

Open your R console or RStudio and run the following commands:

```R
# Cloning the repository and navigating into it:
repo_url <- "https://github.com/Cuevman81/HMS_Smoke_PM25_Analysis_App_V2.git"
system(paste("git clone", repo_url))

# Setting the working directory to the cloned repository:
setwd("HMS_Smoke_PM25_Analysis_App_V2")
```

### 2. Install Required Packages

Run the following command to install all necessary packages:

```R
install.packages(c("shiny", "shinythemes", "shinydashboard", "shinyWidgets", "shinycssloaders",
                   "dplyr", "readr", "sf", "maps", "ggplot2", "lubridate", "DT", 
                   "lwgeom", "httr", "png", "magick", "gridExtra", "grid", "mapdata", 
                   "splitr", "ggrepel", "purrr", "memoise", "foreach", "doParallel", "furrr", "future"))
```

### 3. Run the App

To run the app, use the following command:

```R
shiny::runApp()
```

## Usage

1. Open the app in R Studio after cloning.
2. Navigate between the "HMS Smoke PM2.5 Analysis" and "Daily AQ, Met, HMS Smoke, and Back Trajectory Analysis" tabs.
3. Use the input controls to select dates, locations, and other parameters.
4. Generate plots and data tables using the action buttons.
5. Download data and plots as needed.

## Support

For questions, comments, or to report bugs, please email RCuevas@mdeq.ms.gov.
