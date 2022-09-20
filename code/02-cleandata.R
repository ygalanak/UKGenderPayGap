#------------------------UK Gender Pay Gap-------------------------------------#
#-Author: Yannis Galanakis; galanakis.gian@gmail.com-----Created: Sep 12, 2022-#
#-R Version: 4.2.1---------------------------------------Revised: Sep 15, 2022-#
#-Outputs: Clean data------------------------------------------------------------#

# Set Project Options ----
options(
  digits = 4, # Significant figures output
  scipen = 999, # Disable scientific notation
  repos = getOption("repos")["CRAN"]
)

# Load libraries ----
packages <- c(
  "tidyverse", "naniar", "haven", "survey", "zoo", "janitor", "glue",
  "data.table", "lubridate", "ggalt", "cowplot", "animation", "ggtext",
  "patchwork", "sp", "scales", "raster", "rgeos", "mapproj", "stringr",
  "rgdal", "maptools", "emojifont", "nord", "paletteer", "plotly", "vroom",
  "showtext"
)
pkg_notinstall <- packages[!(packages %in% installed.packages()[, "Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type = "source")
library(gpclib)
gpclibPermit() # Gives maptool permisssion to use gpclib

# Postcodes/Local Authorities ----
## save the outcode postcode and its corresponding county and LA codes
pc |>
  # clean names of the df
  clean_names() |>
  # in pc3, keep only the outcode of each postcode
  separate(postcode_3, c("pc3", "second")) |>
  # keep each postcode only once
  distinct(pc3, region_name, county_code, county_name, local_authority_code, local_authority_name) |>
  # create a new column/variable with region's name
  mutate(region_name = str_remove(region_name, "\\(pseudo\\) ")) |>
  # save this data frame as an .rds (it's the most compressed form)
  write_rds("./data/input/pc_region_localauthority_county.rds")

# bring the saved .rds
regions <- read_rds(here::here(".", "data/input", "pc_region_localauthority_county.rds"))

# remove pc
remove(pc)

# GPG data ----
## GPG by employer and year
paygap <- paygap_raw |>
  # by employer and year
  group_by(employer_id,
    # create a variable called year (comes from the date of submission)
    year = year(date_submitted)
  ) |>
  # if multiple submission by the same employer, keep only the latest submission in each year
  filter(date_submitted == max(date_submitted)) |>
  ungroup()

## merge paygap df with regions df
data_tbl <- paygap |>
  # in paygap df, create a pc3 variable that keeps only the outcode of postcode
  mutate(pc3 = str_extract(post_code, "^[:alnum:]+")) |>
  inner_join(regions, by = "pc3") |>
  mutate(year = year(due_date)) |>
  filter(year < 2023)

## Regional GPG ----
## paygap by region
paygap_regional <- data_tbl |>
  # by region and LA names and year
  group_by(region_name, local_authority_name, year) |>
  # use the median difference of the hourly wages in percent
  summarise(median = median(diff_median_hourly_percent)) |>
  group_by(region_name, local_authority_name) |>
  ungroup()

## fill in with NA if any LA doesn't have data for a particular year
paygap_regional <- paygap_regional |>
  complete(year, local_authority_name, fill = list(median = NA))
write.csv(paygap_regional, "data/output/paygap_regional.csv", row.names = F)


### by region in London ----
paygap_London <- data_tbl |>
  # filter in london
  filter(region_name == "London") |>
  # by region and LA names and year
  group_by(region_name, local_authority_name, year) |>
  # use the median difference of the hourly wages in percent
  summarise(median = median(diff_median_hourly_percent)) |>
  group_by(region_name, local_authority_name) |>
  ungroup()

## fill in with NA if any LA doesn't have data for a particular year
paygap_London <- paygap_London |>
  complete(year, local_authority_name, fill = list(median = NA))
write.csv(paygap_London, "data/output/paygap_London.csv", row.names = F)

### by country ----
## define countries - In England exclude London
data_tbl$country <- ifelse(data_tbl$region_name == "Scotland", "Scotland", NA)
data_tbl$country <- ifelse(data_tbl$region_name == "Northern Ireland",
  "Northern Ireland", data_tbl$country
)
data_tbl$country <- ifelse(data_tbl$region_name == "Wales",
  "Wales", data_tbl$country
)
data_tbl$country <- ifelse(data_tbl$region_name == "London",
  "London", data_tbl$country
)
data_tbl$country <- ifelse(is.na(data_tbl$country) & !is.na(data_tbl$region_name),
  "England (excl. London)", data_tbl$country
)

paygap_bycountry <- data_tbl |>
  group_by(year, country) |>
  summarise(
    median = median(diff_median_hourly_percent),
    .groups = "drop"
  )
write.csv(paygap_bycountry, "data/output/paygap_country.csv", row.names = F)


## National GPG ----
paygap_national <- data_tbl |>
  group_by(year) |>
  summarise(
    median = median(diff_median_hourly_percent),
    .groups = "drop"
  )
write.csv(paygap_national, "data/output/paygap_national.csv", row.names = F)


# SIC analysis ----
## How many employers in each SIC code do we have? ----
PGraw <- paygap_raw |>
  select(sic_codes) |>
  separate_rows(sic_codes, sep = ":") |>
  count(sic_codes, sort = TRUE)

SIC_4digit_average <- data_tbl |>
  # some employers have multiple codes, separate lines for each code
  separate_rows(sic_codes, sep = ":") |>
  # exclude if sic code is NA
  filter(!is.na(sic_codes)) |>
  # create a SIC.4 for any SIC code not equal to 1 (armed forces)
  mutate(SIC.4 = ifelse(sic_codes != 1, as.integer(as.integer(sic_codes) / 10), as.integer(sic_codes))) |>
  # by region and LA names and year
  group_by(SIC.4, year) |>
  # use the median difference of the hourly wages in percent
  summarise(median = median(diff_median_hourly_percent)) |>
  group_by(SIC.4) |>
  ungroup() %>%
  # merge with the class's names and conversion to sectors
  left_join(SIC, by = c("SIC.4" = "Class"))

write.csv(SIC_4digit_average, "data/output/paygap_ClassSector.csv", row.names = F)


SIC_2digit_average <- data_tbl |>
  # some employers have multiple codes, separate lines for each code
  separate_rows(sic_codes, sep = ":") |>
  # exclude if sic code is NA and armed forces
  filter(!is.na(sic_codes) | sic_codes == 1) |>
  # create a SIC.4 for any SIC code not equal to 1 (armed forces)
  mutate(SIC.2 = ifelse(sic_codes != 1, as.integer(as.integer(sic_codes) / 1000), as.integer(sic_codes))) |>
  # by region and LA names and year
  group_by(SIC.2, year) |>
  # use the median difference of the hourly wages in percent
  summarise(median = median(diff_median_hourly_percent)) |>
  group_by(SIC.2) |>
  ungroup() %>%
  # merge with the class's names and conversion to sectors
  left_join(SIC[c("Section", "Section.name", "Division.name", "SIC2dg1", "SectionAbb")], by = c("SIC.2" = "SIC2dg1")) |>
  distinct(SIC.2, year, median, .keep_all = T)

# aggregate for 1-digit sector ----
SIC_1digit_average <- SIC_2digit_average |>
  group_by(SectionAbb, year) |>
  summarise(median = median(median)) |>
  group_by(SectionAbb) |>
  ungroup()

levels <- SIC_1digit_average %>%
  filter(year == 2022) %>%
  arrange(median) %>%
  pull(SectionAbb)

SIC_1digit_average22 <- SIC_1digit_average %>%
  mutate(
    SIC.1.name = factor(SectionAbb, levels),
    SIC_rnk = as.numeric(SIC.1.name)
  )

SIC_1digit_average <- SIC_1digit_average |>
  left_join(SIC[c("SectionAbb", "Section.name")], by = ("SectionAbb")) |>
  distinct(SectionAbb, year, .keep_all = T)
write.csv(SIC_1digit_average, "data/output/paygap_1digitSIC.csv", row.names = F)

# paygap by employers size ----
paygap_size <- data_tbl |>
  group_by(employer_size, year) |>
  # use the median difference of the hourly wages in percent
  summarise(median = median(diff_median_hourly_percent)) |>
  group_by(employer_size) |>
  ungroup()
write.csv(paygap_size, "data/output/paygap_employersize.csv", row.names = F)

# add name ranking for employers size
paygap_size$ranking <- ifelse(paygap_size$employer_size == "Not Provided", 0, NA)
paygap_size$ranking <- ifelse(paygap_size$employer_size == "Less than 250", 1, paygap_size$ranking)
paygap_size$ranking <- ifelse(paygap_size$employer_size == "250 to 499", 2, paygap_size$ranking)
paygap_size$ranking <- ifelse(paygap_size$employer_size == "500 to 999", 3, paygap_size$ranking)
paygap_size$ranking <- ifelse(paygap_size$employer_size == "1000 to 4999", 4, paygap_size$ranking)
paygap_size$ranking <- ifelse(paygap_size$employer_size == "5000 to 19,999", 5, paygap_size$ranking)
paygap_size$ranking <- ifelse(paygap_size$employer_size == "20,000 or more", 6, paygap_size$ranking)

# Top/Bottom 5 ----
## top5= awarding more women ----
top5 <- data_tbl |>
  filter(year == 2022) |>
  arrange(diff_median_hourly_percent) |>
  distinct(employer_name, .keep_all = T) |>
  head(10) |>
  distinct(sic_codes, .keep_all = F) |>
  mutate(Class = as.integer(as.integer(sic_codes)/10)) |>
  left_join(SIC[c("Class", "Class.name")], by = "Class") |>
  na.omit() |>
  select(!c("sic_codes")) |>
  rename("4-digit SIC" = "Class",
         "Description" = "Class.name")
write.csv(top5, "data/output/top10.csv", row.names = F)

## bottom 5 = awarding more men ----
bottom5 <- data_tbl |>
  filter(year == 2022) |>
  arrange(desc(diff_median_hourly_percent)) |>
  distinct(employer_name, .keep_all = T) |>
  head(10) |>
  distinct(sic_codes, .keep_all = F) |>
  mutate(Class = as.integer(as.integer(sic_codes)/10)) |>
  left_join(SIC[c("Class", "Class.name")], by = "Class") |>
  na.omit() |>
  select(!c("sic_codes")) |>
  rename("4-digit SIC" = "Class",
         "Description" = "Class.name")
write.csv(bottom5, "data/output/bottom10.csv", row.names = F)
