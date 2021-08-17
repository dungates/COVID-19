library(here)
source(here("COVIDinOregon.R"))

## Line Graph stuff
case_tracking_df <- df %>%
  dplyr::group_by(abbr) %>%
  dplyr::mutate(new_cases = case_count - lead(case_count),
                ave_new_cases = rollmean(new_cases, 7, na.pad = T, align = "right"),
                new_cases_adjust = cases_per_thousand - lead(cases_per_thousand),
                ave_new_cases_adjust = rollmean(cases_per_thousand, 7, na.pad = T, align = "right")) %>%
  dplyr::filter(!is.na(new_cases), !is.na(ave_new_cases)) %>%
  st_drop_geometry()

# By County
sevenday_county <- ggplot(case_tracking_df, aes(x = date)) +
  geom_col(aes(y = new_cases), state = "identity", fill = "skyblue") +
  geom_line(aes(y = ave_new_cases), color = "red") +
  facet_wrap( ~ fct_reorder(abbr, new_cases, sum, .desc = T)) + # Sum instead of max to get overall trend
  labs(y = "New Cases", x = "Date", title = "7 Day Rolling Average of New Cases in Oregon by County") +
  ylim(0, NA) +
  scale_x_date(breaks = scales::breaks_pretty(n = 5), date_labels = "%b") +
  hrbrthemes::theme_ipsum() +
  theme(panel.spacing = unit(0, "lines"),
        plot.title = element_text(hjust = 0.5, color = "#df5a48"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(color = "#df5a48"),
        axis.title.x = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "none"
  )

ggsave(here("Images/RollingAverageOregon.png"), height = 12, width = 9, dpi = 300)

# ggplotly(sevenday_county)

# By County Log Scale
sevenday_county_log <- ggplot(case_tracking_df, aes(x = date)) +
  geom_col(aes(y = log10(new_cases)), state = "identity", fill = "skyblue") +
  geom_line(aes(y = log10(ave_new_cases)), color = "red") +
  facet_wrap( ~ fct_reorder(abbr, new_cases, sum, .desc = T)) + # Sum instead of max to get overall trend
  labs(y = "New Cases", x = "Date", title = "7 Day Rolling Average of New Cases in Oregon by County",
       subtitle = "Log Scale") +
  scale_y_log10() +
  scale_x_date(breaks = scales::breaks_pretty(n = 5), date_labels = "%b") +
  hrbrthemes::theme_ipsum() +
  theme(panel.spacing = unit(0, "lines"),
        plot.title = element_text(hjust = 0.5))

# ggplotly(sevenday_county_log) %>%
#   layout(title = list(text = paste0('7 Day Rolling Average of New Cases in Oregon by County',
#                                     '<br>',
#                                     '<sup>',
#                                     'Log Scale',
#                                     '</sup>')))

# By County Adjusted for Population
# First make new color palette
num_colors <- length(unique(case_tracking_df$abbr))
color_pal <- colorRampPalette(brewer.pal(9, "Reds"))(num_colors)


# With Union Observations
sevenday_county_popadjust <- ggplot(case_tracking_df, aes(x = date)) +
  geom_area(aes(y = new_cases_adjust, 
                fill = fct_reorder(abbr, new_cases_adjust, .fun = sum))) + #f7d6d1
  geom_text(aes(label = ifelse(abbr == "Union", 
                               "Large Number of Positive\nTests from Single Church", 
                               ""), x = as.Date("2020-06-16"), y = 4),
            color = "black", size = 2) +
  scale_fill_manual(values = color_pal) +
  facet_wrap( ~ fct_reorder(abbr, new_cases_adjust, .fun = first, .desc = T)) +
  labs(y = "New Cases per 1,000 People", x = "Date", 
       title = "New Cases in Oregon by County Adjusted for Population",
       caption = "Cases begin spiking everywhere in early July",
       subtitle = paste0("As of ", 
                         format(covidtracking_df$date, "%b %d"), 
                         ", Total Cases Increased by ", 
                         scales::comma_format()(covidtracking_df$positive[1]-covidtracking_df$positive[2]))) +
  ylim(0, NA) +
  scale_x_date(breaks = scales::breaks_pretty(n = 8), date_labels = "%b") +
  hrbrthemes::theme_ipsum() +
  theme(panel.spacing = unit(0, "lines"),
        plot.title = element_text(hjust = 0.5, color = "#df5a48"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(color = "#df5a48"),
        axis.title.x = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "none"
        )
## Without union
sevenday_county_popadjust_nounion <- case_tracking_df %>%
  dplyr::filter(abbr != "Union" | !(date == "2020-06-16" | date == "2020-06-15")) %>%
  # dplyr::mutate(new_cases_adjust_recent = paste0(abbr, ": ", new_cases_adjust, " Today")) %>% # Idk trying to change label 
  ggplot(aes(x = date)) +
  geom_area(aes(y = new_cases_adjust, 
                fill = fct_reorder(abbr, new_cases_adjust, .fun = sum))) + #f7d6d1
  scale_fill_manual(values = color_pal) +
  facet_wrap( ~ fct_reorder(abbr, new_cases_adjust, .fun = first, .desc = T)) +
  labs(y = "New Cases per 1,000 People", x = "Date", 
       title = "New Cases in Oregon by County Adjusted for Population",
       caption = "Cases begin spiking everywhere in early July",
       subtitle = paste0("As of ", 
                         format(covidtracking_df$date, "%b %d"), 
                         ", Total Cases Increased by ", 
                         scales::comma_format()(covidtracking_df$positive[1]-covidtracking_df$positive[2]))) +
  ylim(0, NA) +
  scale_x_date(breaks = scales::breaks_pretty(n = 8), date_labels = "%b") +
  hrbrthemes::theme_ipsum() +
  theme(panel.spacing = unit(0, "lines"),
        plot.title = element_text(hjust = 0.5, color = "#df5a48"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(color = "#df5a48"),
        axis.title.x = element_text(face = "bold", hjust = 0.5),
        axis.title.y = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank(),
        legend.position = "none"
  )

sevenday_county_popadjust_nounion
ggsave(here("Images/OregonNewCountyCases.png"), height = 9, width = 12)

## Deaths per 1000 adjusted by county


# ggplotly(sevenday_county_popadjust) %>%
#   layout(title = list(text = paste0('7 Day Rolling Average of New Cases in Oregon by County')))

# Oregon as a whole
sevenday_new_cases <- ggplot(case_tracking_df, aes(x = date)) +
  geom_area(aes(y = ave_new_cases, fill = fct_reorder(abbr, ave_new_cases)),
            color = "black") + #f7d6d1
  scale_fill_manual(values = color_pal) +
  labs(y = "New Cases", x = "Date", title = "7 Day Average of Covid-19 Cases in Oregon by County",
       caption = "Labels represent counties that currently average more than 100 new cases a day") +
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

# Text for Plot
cum_text_df <- case_tracking_df %>%
  group_by(abbr) %>%
  dplyr::slice(which.max(date)) %>% 
  ungroup() %>%
  arrange(desc(ave_new_cases)) %>%
  dplyr::mutate(cum = cumsum(ave_new_cases)) %>%
  dplyr::filter(ave_new_cases > 100)

sevenday_new_cases +
  geom_text(data = cum_text_df, 
            aes(color = rev(factor(cum)), # Need to reverse color factor here for position stack
                x = as.Date(date) + 10, 
                y = ave_new_cases, 
                label = paste0(abbr, "\nCounty\n(", round(ave_new_cases, 2), ")")),
            size = 2.5,
            direction = "y",
            position = position_stack(vjust = 0.5),
            fontface = "bold") +
  scale_color_manual(values = color_pal[(36-nrow(cum_text_df)):36]) +
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

ggsave(here("Images/OregonCovidAverage.png"), width = 16, height = 10, dpi = 300)

## Second Version of Same Plot with All Counties

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

cum_text_df_all <- case_tracking_df %>%
  group_by(abbr) %>%
  dplyr::slice(which.max(date)) %>% 
  ungroup() %>%
  arrange(desc(ave_new_cases)) %>%
  dplyr::mutate(cum = cumsum(ave_new_cases)) %>%
  dplyr::mutate(xdist = 8 - 0.1*str_length(abbr)*row_number()^0.75,
                ydist = -85 + row_number()^1.5) # X distance position based on string length

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

ggsave(here("Images/OregonCovidAverageAll.png"), width = 16, height = 10, dpi = 300)






## Percent of Oregon Cases Over Time


# num_colors <- length(unique(case_tracking_df$abbr))
# color_pal <- colorRampPalette(brewer.pal(9, "Reds"))(num_colors)
# current_cases <- toString(case_tracking_df$case_count[1])
# bar_percent_covid <- ggplot(case_tracking_df, 
#                             aes(x = 1, y = ave_new_cases/sum(ave_new_cases, na.rm = T), fill = fct_reorder(abbr, ave_new_cases)))
# 
# bar_percent_covid + geom_bar(stat = "identity", color = "gray80") +
#   scale_x_discrete() +
#   scale_y_continuous(labels = scales::percent) +
#   scale_fill_manual(values = color_pal) +
#   guides(fill = guide_legend(reverse = TRUE,
#                              title.position = "top",
#                              label.position = "bottom",
#                              keywidth = 3,
#                              nrow = 1)) +
#   labs(x = NULL, y = NULL,
#        fill = "Percentage of Oregon's Cases",
#        title = "Oregon Covid Cases as a Percentage by County")
#   theme(legend.position = "top",
#         axis.text.y = element_text(face = "bold", hjust = 1, size = 12),
#         axis.ticks.length = unit(0, "cm"),
#         panel.grid.major.y = element_blank()) +
#   coord_flip()
