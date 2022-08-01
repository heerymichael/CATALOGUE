library(tidyverse)
library(janitor)
library(gganimate)
library(espnscrapeR)
library(nflreadr)
library(lubridate)
library(ggimage)
library(ggtext)
library(shadowtext)
library(av)
library(magick)
library(googlesheets4)
library(googledrive)
library(ggborderline)
source('Final ETR dataviz themes.R')



## ==== Link to image function ====

link_to_img <- function(x, width = 30) {
  glue::glue("<img src='{x}' width='{width}'/>")
}


## ==== Importing team colours ====

team_colours <- get_nfl_teams() %>% 
  filter(team_abb == "TB")

tampa_bay_logo <- get_nfl_teams() %>% 
  filter(team_abb == "TB") %>% 
  select(logo) %>% 
  mutate(logo = link_to_img(logo))



## ==== Importing relevant data ====

data <- read_sheet("https://docs.google.com/spreadsheets/d/1ImICZHG7nq-eohn1ETlzzNa7_fy9n_VGxpNtrwctmJE/edit#gid=0",
                   sheet = "Last 60 Days") %>% 
  clean_names()



## ==== Simple initial overview plot ====

ggplot(data,
       aes(x = as_date(date),
                  y = adp,
                  group = player)) +
  geom_line() +
  scale_y_reverse()

## ==== Tailoring data to control time ====  

bucs_oship_trends <- data  %>% 
  clean_names() %>% 
  # Controlling the showtime for date Gronk retired
  mutate(show_time = case_when(
    date == as_date("2022-06-21") ~ 20,
    date == as_date("2022-07-26") ~ 20,
    TRUE ~ 1)) %>% 
  uncount(show_time) %>%
  group_by(player) %>%
  mutate(reveal_time = row_number()) %>%
  ungroup() 

## ==== Adding player headshots 
  
player_headshots <- load_rosters() %>% 
    select(full_name,
           headshot_url) %>% 
    rename(player = full_name)
  
player_headshots_data <- bucs_oship_trends %>% 
    filter(date == as_date("2022-06-03")) %>% 
    left_join(player_headshots,
              by = "player") %>% 
    mutate(image_date = as_date("2022-06-03"))

## ==== Adding the labelling/annotation variable and player line colour ====

bucs_oship_trends <- bucs_oship_trends %>% 
  mutate(animation_annotation = case_when(
    date == as_date("2022-06-21") ~ "      Jun 21: Gronkowski retires",
    # date == as_date("2022-07-22") ~ "Jul 22: Kyle Rudolph signs",
    date == as_date("2022-07-26") ~ "      Jul 26: Julio Jones signs & Godwin cleared for practice",
    TRUE ~ ""
  )) %>% 
  mutate(player_color = case_when(
    player == "Mike Evans" ~ "#808080",
    player == "Chris Godwin" ~ "#37af4a",
    player == "Russell Gage" ~ "#7d42be",
    player == "Rob Gronkowski" ~ "#c53160",
    player == "Cameron Brate" ~ "#808080",
    player == "Julio Jones" ~ "#2dc6d2"
  )) %>% 
  # removing brady
  filter(player != "Tom Brady") %>% 
  # Fixing the date lines for key events in final chart by setting a colour variable
  mutate(line_color_1 = case_when(
    date > as_date("2022-06-20") ~ "#666666",
    TRUE ~ "NA"
  )) %>%
  mutate(line_color_2 = case_when(
    date > as_date("2022-07-25") ~ "#666666",
    TRUE ~ "NA"
  )) %>%
  mutate(anotation_label_color = case_when(
    date == as_date("2022-08-01") ~ "#666666",
         TRUE ~ "NA"
  )) %>% 
  mutate(annotation_shadow_color = case_when(
    date == as_date("2022-08-01") ~ "#ffffff",
    TRUE ~ "NA"
  )) 




## ==== Basic static plot with Evans included====

# All players

all_players_static <- ggplot() +
  theme_etr() +
  geom_rect(aes(xmin = as_date("2022-05-09"),
                xmax = as_date("2022-05-31"),
                ymin = -30,
                ymax = 300),
            color = "#ffffff",
            fill = "#ffffff") +
  geom_rect(aes(xmin = as_date("2022-06-01"),
                xmax = as_date("2022-08-02"),
                ymin = -30,
                ymax = -1),
            color = "#ffffff",
            fill = "#ffffff") +
  geom_segment(data = bucs_oship_trends,
               aes(x = as_date("2022-06-21"),
                   xend = as_date("2022-06-21"),
                   y = -30,
                   yend = 300,
                   color = line_color_1),
               linetype = "dotted",
               size = 0.8) +
  geom_shadowtext(data = bucs_oship_trends,
                  aes(x = as_date("2022-06-21"),
                      y =-25,
                      label="Gronkowski\nretires",
                      color = anotation_label_color,
                      bg.color = annotation_shadow_color),
                  bg.r = 0.4,
                  vjust = 1,
                  hjust = 0.5,
                  size = 4,
                  fontface = "bold") +
  geom_segment(data = bucs_oship_trends,
               aes(x = as_date("2022-07-26"),
                   xend = as_date("2022-07-26"),
                   y = -30,
                   yend = 300,
                   color = line_color_2),
               linetype = "dotted",
               size = 0.8) +
  geom_shadowtext(data = bucs_oship_trends,
                  aes(x = as_date("2022-07-26"),
                      y =-25,
                      label="Jones signs\nGodwin fit",
                      color = anotation_label_color,
                      bg.color = annotation_shadow_color),
                  bg.r = 0.4,
                  vjust = 1,
                  hjust = 0.5,
                  size = 4,
                  fontface = "bold") +
  geom_line(
    data = bucs_oship_trends,
    aes(x = as_date(date),
        y = adp,
        group = player,
        color = player_color),
    size = 1.4,
    show.legend = FALSE) +
  geom_shadowtext(data = bucs_oship_trends,
                  aes(x = as_date("2022-05-10"),
                      y = -20,
                      label= animation_annotation),
                  color = "#333333",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 0,
                  size = 5) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 25.86,
                      label="Mike Evans"),
                  color = "#808080",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Mike Evans"),
             aes(x = as_date("2022-05-18"),
                 y = 25.86,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 62.25,
                      label="Chris Godwin"),
                  color = "#37af41",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Chris Godwin"),
             aes(x = as_date("2022-05-16"),
                 y = 62.25,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 102,
                      label="Rob Gronkowski"),
                  color = "#c53160",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Rob Gronkowski"),
             aes(x = as_date("2022-05-13"),
                 y = 102,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 121,
                      label="Russell Gage"),
                  color = "#7d42be",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Russell Gage"),
             aes(x = as_date("2022-05-17"),
                 y = 121,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 220,
                      label="Cameron Brate"),
                  color = "#808080",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Cameron Brate"),
             aes(x = as_date("2022-05-15"),
                 y = 220,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 240,
                      label="Julio Jones"),
                  color = "#2dc6d2",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Julio Jones"),
             aes(x = as_date("2022-05-18"),
                 y = 240,
                 image = headshot_url),
             size = 0.08) +
  scale_x_date(position = "bottom",
               limits = c(as_date("2022-05-09"),
                          as_date("2022-08-02")),
               breaks = c(as_date("2022-06-01"),
                          as_date("2022-06-15"),
                          as_date("2022-07-01"),
                          as_date("2022-07-15"),
                          as_date("2022-08-01")),
               labels = c("Jun 01",
                           "Jun 15",
                           "Jul 01",
                          "Jul 15",
                          "Aug 01"),
               expand = c(0,0)) +
  scale_y_reverse(position = "right",
                  expand = c(0,0),
                  limits = c(331,-30),
                  breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205, 217, 229),
                  labels = c(1,
                             "",
                             "",
                             "",
                             "5",
                             "",
                             "",
                             "",
                             "",
                             "10",
                             "",
                             "",
                             "",
                             "",
                             "15",
                             "",
                             "",
                             "",
                             "",
                             "20")) +
  coord_cartesian(ylim = c(260,-30)) +
  theme(plot.title = element_markdown()) +
  scale_color_identity() +
  labs(title = "<img src='https://a.espncdn.com/i/teamlogos/nfl/500/tb.png' width='25'/> Tampa Bay ADP: June and July",
       x = "",
       y = "ROUND",
       caption = "**CHART**: @HEERYMICHAEL   |   **DATA**: FFPC  |  **ESTABLISH THE RUN**")


all_players_static
  ## ==== Animating plot ====

all_players_aniamted_basic <- all_players_static +
  transition_reveal(-reveal_time)
  
all_players_aniamted_basic
  
animate(all_players_aniamted_basic,
        height = 7,
        width = 7,
        units = "in",
        res = 300,
        duration = 8,
        end_pause = 1,
        renderer = av_renderer())

anim_save("all_players_bucs_june_and_july_adp.mp4")


## ==== With geom_borderline ====

borderline_plot <- ggplot() +
  theme_etr() +
  geom_rect(aes(xmin = as_date("2022-05-09"),
                xmax = as_date("2022-05-31"),
                ymin = -30,
                ymax = 300),
            color = "#ffffff",
            fill = "#ffffff") +
  geom_rect(aes(xmin = as_date("2022-06-01"),
                xmax = as_date("2022-08-02"),
                ymin = -30,
                ymax = -1),
            color = "#ffffff",
            fill = "#ffffff") +
  geom_segment(data = bucs_oship_trends,
               aes(x = as_date("2022-06-21"),
                   xend = as_date("2022-06-21"),
                   y = -30,
                   yend = 300,
                   color = line_color_1),
               linetype = "dotted",
               size = 0.8) +
  geom_shadowtext(data = bucs_oship_trends,
                  aes(x = as_date("2022-06-21"),
                      y =-25,
                      label="Gronkowski\nretires",
                      color = anotation_label_color,
                      bg.color = annotation_shadow_color),
                  bg.r = 0.4,
                  vjust = 1,
                  hjust = 0.5,
                  size = 4,
                  fontface = "bold") +
  geom_segment(data = bucs_oship_trends,
               aes(x = as_date("2022-07-26"),
                   xend = as_date("2022-07-26"),
                   y = -30,
                   yend = 300,
                   color = line_color_2),
               linetype = "dotted",
               size = 0.8) +
  geom_shadowtext(data = bucs_oship_trends,
                  aes(x = as_date("2022-07-26"),
                      y =-25,
                      label="Jones signs\nGodwin fit?",
                      color = anotation_label_color,
                      bg.color = annotation_shadow_color),
                  bg.r = 0.4,
                  vjust = 1,
                  hjust = 0.5,
                  size = 4,
                  fontface = "bold") +
  geom_borderline(
    data = bucs_oship_trends,
    aes(x = as_date(date),
        y = adp,
        group = player,
        color = player_color),
    size = 1.4,
    show.legend = FALSE) +
  geom_shadowtext(data = bucs_oship_trends,
                  aes(x = as_date("2022-05-10"),
                      y = -20,
                      label= animation_annotation),
                  color = "#333333",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 0,
                  size = 5,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 25.86,
                      label="Mike Evans"),
                  color = "#808080",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Mike Evans"),
             aes(x = as_date("2022-05-18"),
                 y = 25.86,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 62.25,
                      label="Chris Godwin"),
                  color = "#37af41",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Chris Godwin"),
             aes(x = as_date("2022-05-16"),
                 y = 62.25,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 102,
                      label="Rob Gronkowski"),
                  color = "#c53160",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Rob Gronkowski"),
             aes(x = as_date("2022-05-13"),
                 y = 102,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 121,
                      label="Russell Gage"),
                  color = "#7d42be",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Russell Gage"),
             aes(x = as_date("2022-05-17"),
                 y = 121,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 220,
                      label="Cameron Brate"),
                  color = "#808080",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Cameron Brate"),
             aes(x = as_date("2022-05-15"),
                 y = 220,
                 image = headshot_url),
             size = 0.08) +
  geom_shadowtext(aes(x = as_date("2022-06-03"),
                      y = 240,
                      label="Julio Jones"),
                  color = "#2dc6d2",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0.5,
                  hjust = 1,
                  size = 4,
                  fontface = "bold") +
  geom_image(data = player_headshots_data %>% 
               filter(player == "Julio Jones"),
             aes(x = as_date("2022-05-18"),
                 y = 240,
                 image = headshot_url),
             size = 0.08) +
  scale_x_date(position = "bottom",
               limits = c(as_date("2022-05-09"),
                          as_date("2022-08-02")),
               breaks = c(as_date("2022-06-01"),
                          as_date("2022-06-15"),
                          as_date("2022-07-01"),
                          as_date("2022-07-15"),
                          as_date("2022-08-01")),
               labels = c("Jun 01",
                          "Jun 15",
                          "Jul 01",
                          "Jul 15",
                          "Aug 01"),
               expand = c(0,0)) +
  scale_y_reverse(position = "right",
                  expand = c(0,0),
                  limits = c(331,-30),
                  breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205, 217, 229),
                  labels = c(1,
                             "",
                             "",
                             "",
                             "5",
                             "",
                             "",
                             "",
                             "",
                             "10",
                             "",
                             "",
                             "",
                             "",
                             "15",
                             "",
                             "",
                             "",
                             "",
                             "20")) +
  coord_cartesian(ylim = c(260,-30)) +
  theme(plot.title = element_markdown()) +
  scale_color_identity() +
  labs(title = "<img src='https://a.espncdn.com/i/teamlogos/nfl/500/tb.png' width='25'/> Tampa Bay ADP volatility",
       x = "",
       y = "ROUND",
       caption = "**CHART**: @HEERYMICHAEL   |   **DATA**: FFPC  |  **ESTABLISH THE RUN**")

## ==== Animating plot ====

borderline_animated <- borderline_plot +
  transition_reveal(-reveal_time)

all_players_aniamted_basic

animate(borderline_animated,
        height = 7,
        width = 7,
        units = "in",
        res = 300,
        duration = 8,
        end_pause = 1,
        renderer = av_renderer())

anim_save("bucs animated borderline plot B.mp4")
