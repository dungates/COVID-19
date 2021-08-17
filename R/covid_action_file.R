#' @title COVID Oregon Graph Maker
#'
#' @export

oregon_county_graph <- function() {
  tidycensus::census_api_key("aa7dad57625fd3a7f42a8066e1f3a2ea3ff31ed7", install = T)
  
  oregonpop <- tidycensus::get_acs(
    geography = "county",
    state = "OR",
    variables = "B01003_001",
    year = 2019
  ) %>%
    dplyr::rename(population = estimate) # Get Oregon County populations
  
  # Get current COVID data from the covid tracking project
  url <- "https://api.covidtracking.com/v1/states/or/daily.json"
  covidtracking_df <- jsonlite::fromJSON(url) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(date = as.Date(paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, nchar(date)), sep = "-"))) # No count level data but useful in general
  
  oregon_sf <- urbnmapr::get_urbn_map("counties", sf = TRUE) %>%
    dplyr::filter(state_name == "Oregon") %>%
    dplyr::select(county_fips, geometry)
  
  # Get Oregon county level map data
  
  county_level_cases <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
    janitor::clean_names() %>%
    dplyr::filter(country_region == "US") %>%
    tidyr::pivot_longer(tidyselect::matches("x"),
                        names_to = "dates",
                        values_to = "case_count"
    ) %>%
    dplyr::group_by(province_state) %>%
    dplyr::mutate(date_count = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date_str = stringr::str_replace_all(dates, "x", "")) %>%
    dplyr::mutate(date_str = stringr::str_replace_all(date_str, "_", "/")) %>%
    dplyr::mutate(date_fmt = as.Date(date_str, format = "%m/%d/%y"))
  
  oregon_county_cases <- county_level_cases %>%
    dplyr::filter(province_state == "Oregon")
  
  # Clean up the date time
  df <- oregon_county_cases %>%
    dplyr::mutate(
      date = lubridate::mdy(date_str),
      fips = as.character(fips)
    ) %>%
    dplyr::select(fips, case_count, date, admin2) %>%
    dplyr::rename(abbr = admin2)
  
  df <- dplyr::left_join(df, oregonpop, by = c("fips" = "GEOID")) %>%
    dplyr::select(fips, case_count, date, population, abbr)
  
  df <- df %>%
    dplyr::mutate(cases_per_thousand = (case_count / population) * 1000) %>%
    dplyr::arrange(desc(date))
  
  # Find the center of each hex (county) so that we can add text
  centers <-
    sf::st_centroid(oregon_sf) %>%
    sf::st_coordinates() %>%
    tibble::as_tibble() %>%
    purrr::set_names(str_to_lower) %>%
    dplyr::bind_cols(oregon_sf$county_fips) %>%
    dplyr::rename(county_fips = ...3)
  
  # Combine the centeroid data with the orginial data frame
  df <- dplyr::left_join(centers, df, by = c("county_fips" = "fips")) # %>% st_drop_geometry()
  
  df <- oregon_sf %>% dplyr::left_join(df, by = c("county_fips"))
  
  
  case_tracking_df <- df %>%
    dplyr::group_by(abbr) %>%
    dplyr::mutate(
      new_cases = case_count - dplyr::lead(case_count),
      ave_new_cases = zoo::rollmean(new_cases, 7, na.pad = T, align = "right"),
      new_cases_adjust = cases_per_thousand - dplyr::lead(cases_per_thousand),
      ave_new_cases_adjust = zoo::rollmean(cases_per_thousand, 7, na.pad = T, align = "right")
    ) %>%
    dplyr::filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
    sf::st_drop_geometry()
  
  cum_text_df_all <- case_tracking_df %>%
    dplyr::group_by(abbr) %>%
    dplyr::slice(which.max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(ave_new_cases)) %>%
    dplyr::mutate(cum = cumsum(ave_new_cases)) %>%
    dplyr::mutate(
      xdist = 8 - 0.1 * str_length(abbr) * dplyr::row_number()^0.75,
      ydist = -85 + row_number()^1.5
    ) # X distance position based on string length
  
  
  sevenday_new_cases_all <- ggplot2::ggplot(case_tracking_df, ggplot2::aes(x = date)) +
    ggplot2::geom_area(ggplot2::aes(y = ave_new_cases, fill = forcats::fct_reorder(abbr, ave_new_cases)),
                       color = "black"
    ) + # f7d6d1
    ggplot2::scale_fill_manual(values = color_pal) +
    ggplot2::labs(y = "New Cases", x = "Date", title = "7 Day Average of Covid-19 Cases in Oregon by County") +
    ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%b") +
    DGThemes::theme_premium() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, color = "#df5a48"),
      axis.title = ggplot2::element_text(color = "#df5a48", face = "bold"),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.ticks.x = ggplot2::element_line(color = "#df5a48"),
      axis.text.y = ggplot2::element_text(hjust = 0.75),
      axis.ticks.length.x = unit(0.5, "cm"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(arrow = grid::arrow(
        length = unit(0.3, "cm"),
        ends = "both"
      ), color = "#C4161B", size = 1.5),
      legend.position = "none"
    )
  
  sevenday_new_cases_all <- ggplot2::ggplot(case_tracking_df, ggplot2::aes(x = date)) +
    ggplot2::geom_area(ggplot2::aes(y = ave_new_cases, fill = forcats::fct_reorder(abbr, ave_new_cases)),
                       color = "black"
    ) + # f7d6d1
    ggplot2::scale_fill_manual(values = color_pal) +
    ggplot2::labs(y = "New Cases", x = "Date", title = "7 Day Average of Covid-19 Cases in Oregon by County") +
    ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%b") +
    DGThemes::theme_premium() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, color = "#df5a48"),
      axis.title = ggplot2::element_text(color = "#df5a48", face = "bold"),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.ticks.x = ggplot2::element_line(color = "#df5a48"),
      axis.text.y = ggplot2::element_text(hjust = 0.75),
      axis.ticks.length.x = unit(0.5, "cm"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_line(arrow = grid::arrow(
        length = unit(0.3, "cm"),
        ends = "both"
      ), color = "#C4161B", size = 1.5),
      legend.position = "none"
    )
  
  num_colors <- length(unique(case_tracking_df$abbr))
  color_pal <- colorRampPalette(brewer.pal(9, "Reds"))(num_colors)
  
  sevenday_new_cases_all +
    ggplot2::geom_text(
      data = cum_text_df_all,
      ggplot2::aes(
        color = rev(factor(cum)), # Need to reverse color factor here for position stack
        x = (as.Date(date) + xdist),
        y = cum,
        size = ave_new_cases,
        label = paste0(abbr, "\nCounty\n(", round(ave_new_cases, 2), ")")
      ),
      fontface = "bold"
    ) +
    ggplot2::scale_size(range = c(0, 2.5), guide = F) +
    ggplot2::scale_color_manual(values = color_pal[(37 - nrow(cum_text_df_all)):36]) +
    annotate(geom = "point", x = as.Date("2020-11-17"), y = 1280, size = 10, shape = 21, fill = "#f7d6d1", color = "transparent") +
    ggplot2::geom_text(ggplot2::aes(label = "November 14"),
                       color = "#df5a48",
                       x = as.Date("2020-11-09"), y = 1335, size = 3, fontface = "bold"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = "Statewide Freeze"),
                       color = "black",
                       x = as.Date("2020-11-09"), y = 1300, size = 3
    ) +
    ggplot2::annotate(geom = "point", x = as.Date("2020-12-02"), y = 1550, size = 10, shape = 21, fill = "#f7d6d1", color = "transparent") +
    ggplot2::geom_text(ggplot2::aes(label = "December 2"),
                       color = "#df5a48",
                       x = as.Date("2020-12-09"), y = 1615, size = 3, fontface = "bold"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = "End of Freeze"),
                       color = "black",
                       x = as.Date("2020-12-09"), y = 1585, size = 3
    ) +
    annotate(geom = "point", x = as.Date("2020-07-01"), y = 260, size = 10, shape = 21, fill = "#f7d6d1", color = "transparent") +
    ggplot2::geom_text(ggplot2::aes(label = "July 1"),
                       color = "#df5a48",
                       x = as.Date("2020-06-22"), y = 315, size = 3, fontface = "bold"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = "Mask Mandate"),
                       color = "black",
                       x = as.Date("2020-06-22"), y = 280, size = 3
    ) +
    annotate(geom = "point", x = as.Date("2020-03-23"), y = 50, size = 10, shape = 21, fill = "#f7d6d1", color = "transparent") +
    ggplot2::geom_text(ggplot2::aes(label = "March 23"),
                       color = "#df5a48",
                       x = as.Date("2020-03-15"), y = 105, size = 3, fontface = "bold"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = "Stay-at-home Order"),
                       color = "black",
                       x = as.Date("2020-03-15"), y = 85, size = 3
    ) +
    annotate(geom = "point", x = as.Date("2020-07-10"), y = 330, size = 10, shape = 21, fill = "#f7d6d1", color = "transparent") +
    ggplot2::geom_text(ggplot2::aes(label = "July 6"),
                       color = "#df5a48",
                       x = as.Date("2020-07-20"), y = 385, size = 3, fontface = "bold"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = "Stay-at-home Lifted"),
                       color = "black",
                       x = as.Date("2020-07-20"), y = 365, size = 3
    )
  
  ggplot2::ggsave(here::here("Images/OregonCovidAverageAll.png"), width = 16, height = 10, dpi = 300)
  
}
