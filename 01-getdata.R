#------------------------UK Gender Pay Gap-------------------------------------#
#-Author: Yannis Galanakis; galanakis.gian@gmail.com-----Created: Sep 12, 2022-#
#-R Version: 4.2.1---------------------------------------Revised: Sep 15, 2022-#
#-Outputs: Get data------------------------------------------------------------#

# Set Project Options ----
options(
  digits = 4, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"]
)

# Load libraries ----
pacman::p_load(
  "tidyverse", "naniar", "haven", "survey", "zoo", "janitor", "glue",
  "data.table", "lubridate", "ggalt", "cowplot", "animation", "ggtext",
  "patchwork", "sp", "scales", "raster", "rgeos", "mapproj", "stringr",
  "rgdal", "maptools", "emojifont", "nord", "paletteer", "plotly", "vroom",
  "showtext"
)

# GPG data ----
paygap_raw <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv")

# Postcode/Local Authority data ----
pc <- read_csv("https://opendata.camden.gov.uk/api/views/tr8t-gqz7/rows.csv")

# import SIC codes converter
SIC <- read_csv("data/input/convertSIC.csv")
