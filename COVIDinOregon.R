library(tidyverse)  
library(zoo)
library(httr) 
library(jsonlite)
library(lubridate)
library(paletteer)
library(extrafont)
library(sf)
library(cowplot)
library(prismatic)
library(here)
library(magick)
library(ggtext)
library(readr)
library(janitor)
library(tidycensus)
library(ggrepel)
# remotes::install_github("Nowosad/rcartocolor")
library(rcartocolor)
library(RColorBrewer)
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
# devtools::install_github("thomasp85/transformr")
library(transformr)
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

#custom theme
theme_owen <- function () { 
  theme_minimal(base_size=12, base_family="Gill Sans MT") %+replace% 
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    )
}


# census_api_key("aa7dad57625fd3a7f42a8066e1f3a2ea3ff31ed7", install = T)
oregonpop <- get_acs(geography = "county",
                     state = "OR",
                     variables = "B01003_001",
                     year = 2019) %>%
  rename(population = estimate) # Get Oregon County populations
# v19 <- load_variables(2019, "acs5", cache = TRUE) # Latest oregon population data codes
# 
# View(v19)

#Get current COVID data from the covid tracking project
url <- "https://api.covidtracking.com/v1/states/or/daily.json"
covidtracking_df <- jsonlite::fromJSON(url) %>% as_tibble() %>%
  mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, nchar(date)), sep = '-'))) # No count level data but useful in general

oregon_sf <- get_urbn_map("counties", sf = TRUE) %>%
  dplyr::filter(state_name == "Oregon") %>%
  select(county_fips, geometry)
  # Get oregon county level map data

county_level_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  clean_names() %>%
  dplyr::filter(country_region == "US") %>%
  pivot_longer(matches("x"),
               names_to="dates",
               values_to = "case_count")%>%
  group_by(province_state) %>%
  mutate(date_count = row_number()) %>%
  ungroup() %>%
  mutate(date_str=str_replace_all(dates,"x","")) %>%
  mutate(date_str=str_replace_all(date_str,"_","/")) %>%
  mutate(date_fmt = as.Date(date_str,format="%m/%d/%y"))

oregon_county_cases <- county_level_cases %>%
  dplyr::filter(province_state == "Oregon")

#Clean up the date time
df <- oregon_county_cases %>%
  mutate(date = mdy(date_str),
         fips = as.character(fips)) %>%
  select(fips, case_count, date, admin2) %>%
  rename(abbr = admin2)

df <- left_join(df, oregonpop, by = c("fips" = "GEOID")) %>%
  select(fips, case_count, date, population, abbr)

df <- df %>% 
  mutate(cases_per_thousand = (case_count / population) * 1000) %>%
  arrange(desc(date))

#Find the center of each hex (county) so that we can add text 
centers <- 
  st_centroid(oregon_sf) %>% 
  st_coordinates() %>% 
  as_tibble() %>% 
  set_names(str_to_lower) %>%
  bind_cols(oregon_sf$county_fips) %>%
  rename(county_fips = ...3)

#Combine the centeroid data with the orginial data frame
df <- left_join(centers, df, by = c("county_fips" = "fips"))# %>% st_drop_geometry()

df <- oregon_sf %>% left_join(df, by = c("county_fips"))

#Create an outline of Oregon
oregon_hex_outline <- oregon_sf %>%
  st_union() %>%
  st_buffer(dist = 30000)


today_df <- df %>%
  dplyr::filter(date == (Sys.Date() - 1)) %>%
  mutate(textColor = ifelse(cases_per_thousand >= quantile(cases_per_thousand, .75, na.rm = TRUE), 
                            "white", 
                            "black"))

today_df <- today_df %>%
  left_join(select(covidtracking_df, date, positive), by = c("date"))

#Create plot
p <- today_df %>%
  ggplot() +
  geom_sf(data = oregon_hex_outline, color = "#008080", fill = "transparent", size = 1, 
          inherit.aes = F) +
  geom_sf(data = today_df, aes(fill = cases_per_thousand), size = 1, color = 'white') +
  # coord_sf(crs = 3785, datum = NA) + # Changes oregon rotation but useless right now
  geom_text(data = today_df, aes(x, y, label = abbr, color = textColor), 
            family = "Gill Sans MT", size = 2.25, fontface = 'bold') +
  geom_text(data = today_df, aes(x, y, label = round(cases_per_thousand, 2), color = textColor), 
            family = "Gill Sans MT", size = 2, fontface = 'bold', vjust = 2.5) + 
  rcartocolor::scale_fill_carto_c(
    name = "Confirmed Cases Per Thousand Residents",
    palette = "DarkMint",
    breaks = scales::breaks_pretty(n = 8)) +
  theme_owen() +
  scale_color_identity() +
  theme(text=element_text(size=14,  family="Gill Sans MT"), 
        plot.title = element_text(hjust = 0.5, face = "bold",  vjust = 0, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = 0), 
        plot.caption = element_text(face = "italic", size = 8, hjust = .5, vjust = 8), 
        legend.spacing.x = unit(0, 'cm'), 
        legend.title=element_text(size=11), 
        legend.text = element_text(size = rel(0.6)), 
        legend.margin=margin(10,0,-1,0),
        legend.position = 'bottom',
        plot.margin = margin(0, -.5, 0, -.5, "cm"), 
        legend.box.margin=margin(-30,0,15,0))  +
  guides(fill=guide_legend(
    keywidth=.5,
    keyheight=.15,
    default.unit="inch", 
    label.position = 'bottom', 
    title.position = 'top',
    title.hjust = .5,
    title.vjust = 0,
    label.vjust = 3,
    nrow = 1)) +
  labs(title = "Confirmed Cases Due To COVID-19 Per Thousand Residents", 
       caption  = paste0("Data updated ", format(Sys.time(), "%b %d %X")), 
       subtitle = paste0("Total Confirmed Cases Due To COVID-19 In Oregon: ", scales::comma_format()(today_df$positive[1])))
p
# display_carto_all() # Color palette


cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))


#Add custom footer image
ggsave(here("Images/OregonCovid.png"), width = 6, height = 6, dpi = 300)
# footy <- image_read(here("Images/Footer.png"))
# graf <- image_read(here("Images/OregonCovid.png"))
# image_composite(graf, footy, offset = "+0+1745") %>% image_write(here("Images/OregonCovid.png"))











