library(tidyverse)  
library(zoo)
library(httr) 
library(jsonlite)
library(lubridate)
library(paletteer)
library(prismatic)
library(here)
library(magick)
library(ggtext)
library(readr)
library(janitor)
library(tidycensus)
library(ggrepel)
library(sf)
library(urbnmapr)
census_api_key("aa7dad57625fd3a7f42a8066e1f3a2ea3ff31ed7", install = T)

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


case_tracking_df <- df %>%
  dplyr::group_by(abbr) %>%
  dplyr::mutate(new_cases = case_count - lead(case_count),
                ave_new_cases = rollmean(new_cases, 7, na.pad = T, align = "right"),
                new_cases_adjust = cases_per_thousand - lead(cases_per_thousand),
                ave_new_cases_adjust = rollmean(cases_per_thousand, 7, na.pad = T, align = "right")) %>%
  dplyr::filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
  st_drop_geometry()

cum_text_df_all <- case_tracking_df %>%
  group_by(abbr) %>%
  dplyr::slice(which.max(date)) %>% 
  ungroup() %>%
  arrange(desc(ave_new_cases)) %>%
  dplyr::mutate(cum = cumsum(ave_new_cases)) %>%
  dplyr::mutate(xdist = 8 - 0.1*str_length(abbr)*row_number()^0.75,
                ydist = -85 + row_number()^1.5) # X distance position based on string length


sevenday_new_cases_all <- ggplot(case_tracking_df, aes(x = date)) +
  geom_area(aes(y = ave_new_cases, fill = fct_reorder(abbr, ave_new_cases)),
            color = "black") + #f7d6d1
  scale_fill_manual(values = color_pal) +
  labs(y = "New Cases", x = "Date", title = "7 Day Average of Covid-19 Cases in Oregon by County") +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%b") +
  hrbrthemes::theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5, color = "#df5a48"),
        axis.title = element_text(color = "#df5a48", face = "bold"),
        axis.line.x = element_blank(),
        axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.ticks.x = element_line(color = "#df5a48"),
        axis.text.y = element_text(hjust = 0.75),
        axis.ticks.length.x = unit(0.5, "cm"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both"), color = "#C4161B", size = 1.5),
        legend.position = "none")

sevenday_new_cases_all <- ggplot(case_tracking_df, aes(x = date)) +
  geom_area(aes(y = ave_new_cases, fill = fct_reorder(abbr, ave_new_cases)),
            color = "black") + #f7d6d1
  scale_fill_manual(values = color_pal) +
  labs(y = "New Cases", x = "Date", title = "7 Day Average of Covid-19 Cases in Oregon by County") +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%b") +
  hrbrthemes::theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5, color = "#df5a48"),
        axis.title = element_text(color = "#df5a48", face = "bold"),
        axis.line.x = element_blank(),
        axis.title.x = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.title.y = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.ticks.x = element_line(color = "#df5a48"),
        axis.text.y = element_text(hjust = 0.75),
        axis.ticks.length.x = unit(0.5, "cm"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both"), color = "#C4161B", size = 1.5),
        legend.position = "none")

num_colors <- length(unique(case_tracking_df$abbr))
color_pal <- colorRampPalette(brewer.pal(9, "Reds"))(num_colors)

sevenday_new_cases_all +
  geom_text(data = cum_text_df_all, 
            aes(color = rev(factor(cum)), # Need to reverse color factor here for position stack
                x = (as.Date(date) + xdist), 
                y = cum, 
                size = ave_new_cases,
                label = paste0(abbr, "\nCounty\n(", round(ave_new_cases, 2), ")")),
            fontface = "bold") +
  scale_size(range = c(0, 2.5), guide = F) +
  scale_color_manual(values = color_pal[(37-nrow(cum_text_df_all)):36]) +
  annotate(geom="point", x=as.Date("2020-11-17"), y=1280, size=10, shape=21, fill="#f7d6d1", color = "transparent") +
  geom_text(aes(label = "November 14"), color = "#df5a48", 
            x = as.Date("2020-11-09"), y = 1335, size = 3, fontface = "bold") +
  geom_text(aes(label = "Statewide Freeze"), color = "black", 
            x = as.Date("2020-11-09"), y = 1300, size = 3) +
  annotate(geom="point", x=as.Date("2020-12-02"), y=1550, size=10, shape=21, fill="#f7d6d1", color = "transparent") +
  geom_text(aes(label = "December 2"), color = "#df5a48", 
            x = as.Date("2020-12-09"), y = 1615, size = 3, fontface = "bold") +
  geom_text(aes(label = "End of Freeze"), color = "black", 
            x = as.Date("2020-12-09"), y = 1585, size = 3) +
  annotate(geom="point", x=as.Date("2020-07-01"), y=260, size=10, shape=21, fill="#f7d6d1", color = "transparent") +
  geom_text(aes(label = "July 1"), color = "#df5a48", 
            x = as.Date("2020-06-22"), y = 315, size = 3, fontface = "bold") +
  geom_text(aes(label = "Mask Mandate"), color = "black", 
            x = as.Date("2020-06-22"), y = 280, size = 3) +
  annotate(geom="point", x=as.Date("2020-03-23"), y=50, size=10, shape=21, fill="#f7d6d1", color = "transparent") +
  geom_text(aes(label = "March 23"), color = "#df5a48", 
            x = as.Date("2020-03-15"), y = 105, size = 3, fontface = "bold") +
  geom_text(aes(label = "Stay-at-home Order"), color = "black", 
            x = as.Date("2020-03-15"), y = 85, size = 3) +
  annotate(geom="point", x=as.Date("2020-07-10"), y=330, size=10, shape=21, fill="#f7d6d1", color = "transparent") +
  geom_text(aes(label = "July 6"), color = "#df5a48", 
            x = as.Date("2020-07-20"), y = 385, size = 3, fontface = "bold") +
  geom_text(aes(label = "Stay-at-home Lifted"), color = "black", 
            x = as.Date("2020-07-20"), y = 365, size = 3)

ggsave(here::here("Images/OregonCovidAverageAll.png"), width = 16, height = 10, dpi = 300)

