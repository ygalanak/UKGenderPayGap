#------------------------UK Gender Pay Gap-------------------------------------#
#-Author: Yannis Galanakis; galanakis.gian@gmail.com-----Created: Sep 12, 2022-#
#-R Version: 4.2.1---------------------------------------Revised: Sep 15, 2022-#
#-Outputs: Plots---------------------------------------------------------------#

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
  "showtext", "sf"
)
pkg_notinstall <- packages[!(packages %in% installed.packages()[, "Package"])]

lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
if (!require(gpclib)) install.packages("gpclib", type = "source")
library(gpclib)
gpclibPermit() # Gives maptool permisssion to use gpclib

# for twitter icon
font_add("fa-reg", "fonts/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "fonts/Font Awesome 6 Free-Solid-900.otf")


# read map data ----
map0 <- st_read("data/input/mapC")
# merge map data and regional GPG
mapLA <- merge(map0, paygap_regional, by.x = "LAD21NM", by.y = "local_authority_name")

# Map by year ----
map_byyear <- ggplot(data = mapLA, aes(text = paste0("The GPG in ", LAD21NM, " is <br>", median, "%"))) +
  geom_sf(aes(fill = median)) +
  # one map by each year
  facet_wrap(mapLA$year,
    nrow = 1,
    # set scales to free to select in a particular year a particular region
    # if you want to keep the ggplot just deactivate this line
    #   scales = "free"
  ) +
  # positive and negative values different color gradient: blue men earn more; pink women earn more; white equality
  scale_fill_gradient2() +
  theme_yannis() +
  labs(
    title = "UK Gender Pay Gap",
    #  subtitle = "2018-2022",
    caption = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: YannisGalanakis | @YannisGalanakis",
    fill = "", # removing legend title
    x = "", y = ""
  ) +
  theme(
    # remove x and y axis
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    # place legend under the graphs
    legend.position = "bottom"
  )

plotly_json(
ggplotly(map_byyear, tooltip = "text"))

# by country ----
library(ggrepel)
# add paygap_national to country so that we plot them in one graph
countryplot <- paygap_national |>
  # create a column called country in paygap_national
  mutate(country = "UK") |>
  full_join(paygap_bycountry) |>
ggplot(aes(x = year, y = median, colour = country)) +
  geom_line(size=1) +
  theme_yannis() +
  labs(
    title = "UK Gender Pay Gap",
    #  subtitle = "2018-2022",
    caption = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: Yannis Galanakis | @YannisGalanakis",
    fill = "", # removing legend title
    x = "", y = "Median GPG (in %)"
  ) +
  theme(
    # remove legend under the graphs
    legend.position = "none"
  ) +
  # add non-overlapping labels of each line
  geom_text_repel(
    # restrict data to max of year to add the label only at the end of the latest year entry
    data = . %>% filter(year == max(year)),
    aes(color = country, label = country),
    family = "Lato",
    size = 5,
    direction = "y",
    xlim = c(2023),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  ## coordinate system + scales
  scale_x_continuous(
    limits = c(2018, 2023),
    breaks = seq(2018, 2022, by = 1)
  ) +
  scale_y_continuous(
    labels = number_format(suffix = "%")
  )

ggsave(countryplot, filename = "countryplot.jpeg",
       width = 7, height = 4, units = "in", device = "jpeg", dpi = 300,
       background = "#fffff")


# London map in 2019 vs. 2022 ----
# merge map data and regional GPG
mapLondon <- merge(map0, paygap_London, by.x = "LAD21NM", by.y = "local_authority_name")

map_London <- mapLondon |>
  filter(year == 2019 | year == 2022) |>
  ggplot(aes(text = paste0("The GPG in ", LAD21NM, " is <br>", median, "%"))) +
  geom_sf(aes(fill = median)) +
  # add the name of each Local Authority
  # geom_sf_text(aes(label = LAD21NM), colour = "white", size = 2,
  #             fun.geometry = sf::st_centroid) +
  # one map by each year
  facet_wrap(mapLondon[mapLondon$year == 2019 | mapLondon$year == 2022, ]$year,
    nrow = 1,
    # set scales to free to select in a particular year a particular region
    # if you want to keep the ggplot just deactivate this line
    #   scales = "free"
  ) +
  # positive and negative values different color gradient: blue men earn more; pink women earn more; white equality
  scale_fill_gradient2() +
  theme_yannis() +
  labs(
    title = "UK Gender Pay Gap",
    #subtitle = "London: a gentlemen's city",
    caption = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: YannisGalanakis | @YannisGalanakis",
    fill = "", # removing legend title
    x = "", y = ""
  ) +
  theme(
    # remove x and y axis
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    # place legend under the graphs
    legend.position = "bottom"
  )

plotly_json(
ggplotly(map_London, tooltip = "text") |>
  layout(#margin = list(l = 50, r = 50, b = 10, t = 10),
         annotations = list(x = 1, y = -0.3, text = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: YannisGalanakis | @YannisGalanakis",
                            xref='paper', yref='paper', showarrow = F, 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font = list(size = 10)))
)

# SIC of employer ranking ----
SICplot <- ggplot(SIC_1digit_average22, aes(x = median, y = SIC_rnk)) +
  geom_vline(aes(xintercept = 0), colour = "#f57d91", linetype = 2, size = 1.2) +
  geom_label(aes(x = 0, y = 24, label = "Equal Pay"),
    colour = "#f57d91",
    family = "baskerville",
    fontface = "italic",
    fill = "#eff2f7"
  ) +
  geom_label(aes(x = 20, y = 24, label = "Men are paid more"),
    colour = "#f57d91",
    family = "baskerville",
    fontface = "italic",
    fill = "#eff2f7"
  ) +
  geom_line(aes(group = SIC_rnk, colour = factor(year)),
    size = .1,
    show.legend = F
  ) +
  geom_point(aes(colour = factor(year)),
    size = 3,
    show.legend = T
  ) +
  geom_label(
    data = SIC_1digit_average22 %>% group_by(SIC.1.name) %>%
      filter(median == min(median)) %>%
      filter(median < minimum),
    aes(
      x = median - 0.5, y = SIC_rnk,
      label = SIC.1.name
    ), hjust = "right",
    family = "baskerville", colour = "#012169", size = 5,
    fill = "transparent", label.size = NA
  ) +
  geom_label(
    data = SIC_1digit_average22 %>% group_by(SIC.1.name) %>%
      filter(median == max(median)) %>%
      filter(median > maximum + 1),
    aes(
      x = median + 0.5, y = SIC_rnk,
      label = SIC.1.name
    ), hjust = "left",
    family = "baskerville", colour = "#012169", size = 5,
    fill = "transparent", label.size = NA
  ) +
  scale_x_continuous(
    limits = c(-5, 40),
    labels = number_format(suffix = "%", style_positive = "plus"),
    expand = expansion(add = c(0, 5))
  ) +
  #  scale_colour_manual(values = factor(year)) +
  labs(
    title = "UK Gender Pay Gap",
    subtitle = "by employers' sector of activity",
    caption = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: YannisGalanakis | @YannisGalanakis",
    x = "Median difference", y = NULL, col = NULL
  ) +
  theme_yannis() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

# by employers' size -----
employerplot <- ggplot(paygap_size[paygap_size$employer_size != "Not Provided", ], aes(x = median, y = ranking)) +
  geom_vline(aes(xintercept = 0), colour = "#f57d91", linetype = 2, size = 1.2) +
  geom_label(aes(x = 0, y = 8, label = "Equal Pay"),
    colour = "#f57d91",
    family = "baskerville",
    fontface = "italic",
    fill = "#eff2f7"
  ) +
  geom_label(aes(x = 7.5, y = 8, label = "Men are paid more"),
    colour = "#f57d91",
    family = "baskerville",
    fontface = "italic",
    fill = "#eff2f7"
  ) +
  geom_line(aes(group = ranking, colour = factor(year)),
    size = .1,
    show.legend = F
  ) +
  geom_point(aes(colour = factor(year)),
    size = 3,
    show.legend = T
  ) +
  geom_label(
    data = paygap_size[paygap_size$employer_size != "Not Provided", ] %>% group_by(employer_size) %>%
      filter(median == min(median)) %>%
      filter(median < minimum),
    aes(
      x = median - 0.5, y = ranking,
      label = employer_size
    ), hjust = "right",
    family = "baskerville", colour = "#012169", size = 5,
    fill = "transparent", label.size = NA
  ) +
  geom_label(
    data = paygap_size[paygap_size$employer_size != "Not Provided", ] %>% group_by(employer_size) %>%
      filter(median == max(median)) %>%
      filter(median > maximum + 1),
    aes(
      x = median + 0.5, y = ranking,
      label = employer_size
    ), hjust = "left",
    family = "baskerville", colour = "#012169", size = 5,
    fill = "transparent", label.size = NA
  ) +
  scale_x_continuous(
    limits = c(-2, 15),
    labels = number_format(suffix = "%", style_positive = "plus"),
    expand = expansion(add = c(0, 5))
  ) +
  labs(
    title = "UK Gender Pay Gap",
    subtitle = "by employer size",
    caption = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: YannisGalanakis | @YannisGalanakis",
    x = "Median difference", y = "Number of employees", col = NULL
  ) +
  theme_yannis() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

# Distribution of median difference in hourly pay ----
distribution <- paygap_raw |>
  ggplot(aes(diff_median_hourly_percent / 100)) +
  geom_density(size = 1) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  theme_yannis() +
  labs(title = "UK Gender Pay Gap",
       subtitle = "Distribution of median difference in hourly pay",
       x = "Median difference in hourly pay",
       y = "Density",
       caption = "Data: https://gender-pay-gap.service.gov.uk/ \n Illustration: Yannis Galanakis | @YannisGalanakis",
  ) 
