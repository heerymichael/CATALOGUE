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
library(cowplot)

## ==== Link to image function ====

link_to_img <- function(x, width = 30) {
  glue::glue("<img src='{x}' width='{width}'/>")
}


## ==== Importing team colours ====

team_colours <- get_nfl_teams() %>% 
  filter(team_abb == "TB")

## ==== Importing relevant data ====

bucs_oship_trends <- read_csv("~/Desktop/Establish the Run data viz/Bucs ADPs - Last 60 Days.csv") %>% 
  clean_names() %>% 
  filter(player != "Cade Otton") %>% 
  # Controlling the showtime for date Gronk retired
  mutate(show_time = case_when(
    date == "2022-06-21" ~ 20,
    TRUE ~ 1)) %>% 
  uncount(show_time) %>%
  group_by(player) %>%
  mutate(reveal_time = row_number()) %>%
  ungroup() %>% 
  mutate(alpha_state = case_when(
    date == "2022-06-21" ~ 0.95,
    TRUE ~ 0)) %>% 
  mutate(color_identifier = case_when(
    player == "Rob Gronkowski" ~ "RG",
    player == "Russell Gage" | player == "Cameron Brate" ~ "Risers",
    TRUE ~ "Other"
  )) %>% 
  mutate(alpha_rating = case_when(
    player == "Cameron Brate" ~ 1,
    player == "Russell Gage" ~ 1,
    player == "Rob Gronkowski" ~ 1,
    TRUE ~ 0.2
  ))


## Adding player headshots to data ====

player_headshots <- load_rosters() %>% 
  select(full_name,
         headshot_url) %>% 
  rename(player = full_name)

player_headshots_data <- bucs_oship_trends %>% 
  filter(date == as_date("2022-05-02")) %>% 
  left_join(player_headshots,
            by = "player") %>% 
  mutate(image_date = case_when(
    player == "Cameron Brate" ~ as_date("2022-05-30"),
    TRUE ~ as_date("2022-05-02"))) %>% 
  mutate(adp = case_when(
    player == "Cameron Brate" ~ 180,
    TRUE ~ adp
  )) 


## ==== Basic static plot ====

# All players

all_players_static <- ggplot() +
  geom_vline(xintercept = as_date("2022-06-21"),
             color = "#666666",
             linetype = "dotted",
             size = 0.8) +
  geom_shadowtext(aes(x = as_date("2022-06-21"),
                      y = 40,
                      label = "RETIREMENT DATE"),
                  vjust = 1,
                  color = "#666666",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  fontface = "bold") +
  geom_line(
    data = bucs_oship_trends,
    aes(x = date,
        y = adp,
        group = player,
        color = color_identifier),
    size = 1.4,
    show.legend = FALSE) +
  geom_image(data = player_headshots_data,
             aes(x = image_date,
                 y = adp,
                 image = headshot_url),
             size = 0.12) +
  geom_shadowtext(aes(x = as_date("2022-05-30"),
                      y = 200,
                      label="Cameron Brate"),
                  color = "#a80d08",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Rob Gronkowski"),
                  aes(x = date + 3,
                      y = adp,
                      label = player),
                  color = "#343026",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Russell Gage"),
                  aes(x = date + 3,
                      y = adp,
                      label = player),
                  color = "#a80d08",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Chris Godwin"),
                  aes(x = date + 3,
                      y = adp,
                      label = player,
                      color = color_identifier),
                  color = "#bbbbbb",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Tom Brady"),
                  aes(x = date + 3,
                      y = adp,
                      label = player,
                      color = color_identifier),
                  color = "#bbbbbb",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Mike Evans"),
                  aes(x = date + 3,
                      y = adp,
                      label = player,
                      color = color_identifier),
                  color = "#bbbbbb",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_curve(
    aes(x = as_date("2022-06-03"),
        xend = as_date("2022-06-11"),
        y = 188,
        yend = 205),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.3,
    color = "#999999") +
  scale_x_date(position = "top") +
  scale_size_identity() +
  scale_y_reverse(
    position = "right",
    breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205),
    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),) +
  theme_etr_white() +
  theme(plot.caption = element_markdown()) +
  coord_cartesian(ylim = c(205,0)) +
  scale_color_manual(values = rev(c("#a80d08",
                                    "#343026",
                                    "#bbbbbb"))) +
  labs(title = "Gronkowski retirement",
       subtitle = "Russell Gage and Cameron Brate ADP were already climbing\nbefore Gronk's retirement and have continued since",
       x = "",
       y = "ROUND",
       caption = "**TABLE**: @HEERYMICHAEL   |   **DATA**: UNDERDOG  |  **ESTABLISH THE RUN**")


# Only 3 players
three_players_static <- ggplot() +
  geom_vline(xintercept = as_date("2022-06-21"),
             color = "#666666",
             linetype = "dotted",
             size = 0.8) +
  geom_shadowtext(aes(x = as_date("2022-06-21"),
                      y = 61,
                      label = "RETIREMENT DATE\n21st JUNE"),
                  fontface = "bold",
                  vjust = 1,
                  size = 4,
                  color = "#666666",
                  bg.r = 0.4,
                  bg.color = "#ffffff") +
  geom_line(
    data = bucs_oship_trends %>% 
      filter(player != "Chris Godwin") %>% 
      filter(player != "Tom Brady") %>% 
      filter(player != "Mike Evans"),
    aes(x = date,
        y = adp,
        group = player,
        color = color_identifier),
    size = 1.4,
    show.legend = FALSE) +
  geom_image(data = player_headshots_data %>% 
               filter(player != "Chris Godwin") %>% 
               filter(player != "Tom Brady") %>% 
               filter(player != "Mike Evans"),
             aes(x = image_date,
                 y = adp,
                 image = headshot_url),
             size = 0.11,
             by = "height") +
  geom_shadowtext(aes(x = as_date("2022-05-02"),
                      y = 92,
                      label="Rob\nGronkowski"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 0,
                  bg.r = 0.6,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-05-02"),
                      y = 137,
                      label="Russell Gage"),
                  color = "#a80d08",
                  bg.color = "#ffffff",
                  vjust = 1,
                  bg.r = 0.6,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-05-30"),
                      y = 197,
                      label="Cameron Brate"),
                  color = "#a80d08",
                  bg.color = "#ffffff",
                  vjust = 0,
                  bg.r = 0.6) +
  geom_curve(
    aes(x = as_date("2022-06-08"),
        xend = as_date("2022-06-11"),
        y = 195,
        yend = 205),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.3,
    color = "#999999") +
  scale_x_date(position = "top",
               expand = c(0.15,0)) +
  scale_size_identity() +
  scale_y_reverse(
    position = "right",
    breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205),
    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),) +
  theme_etr_white() +
  coord_cartesian(ylim = c(205,58)) +
  scale_color_manual(values = rev(c("#a80d08",
                                    "#343026"))) +
  labs(title = "Gronkowski retirement",
       subtitle = "Russell Gage and Cameron Brate ADP were already climbing\nbefore Gronk's retirement and have continued since",
       x = "",
       y = "ROUND")

# Three players with ETR colours
three_players_etr_palette <- ggplot() +
  geom_vline(xintercept = as_date("2022-06-21"),
             color = "#666666",
             linetype = "dotted",
             size = 0.8) +
  geom_shadowtext(aes(x = as_date("2022-06-21"),
                      y = 61,
                      label = "RETIREMENT DATE\n21st JUNE"),
                  fontface = "bold",
                  vjust = 1,
                  size = 4,
                  color = "#666666",
                  bg.r = 0.4,
                  bg.color = "#ffffff") +
  geom_line(
    data = bucs_oship_trends %>% 
      filter(player != "Chris Godwin") %>% 
      filter(player != "Tom Brady") %>% 
      filter(player != "Mike Evans"),
    aes(x = date,
        y = adp,
        group = player,
        color = color_identifier),
    size = 1.4,
    show.legend = FALSE) +
  geom_image(data = player_headshots_data %>% 
               filter(player != "Chris Godwin") %>% 
               filter(player != "Tom Brady") %>% 
               filter(player != "Mike Evans"),
             aes(x = image_date,
                 y = adp,
                 image = headshot_url),
             size = 0.11,
             by = "height") +
  geom_shadowtext(aes(x = as_date("2022-05-02"),
                      y = 92,
                      label="Rob\nGronkowski"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 0,
                  bg.r = 0.6,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-05-02"),
                      y = 137,
                      label="Russell Gage"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 1,
                  bg.r = 0.6,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-05-30"),
                      y = 197,
                      label="Cameron Brate"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 0,
                  bg.r = 0.6) +
  geom_curve(
    aes(x = as_date("2022-06-08"),
        xend = as_date("2022-06-11"),
        y = 195,
        yend = 205),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.3,
    color = "#999999") +
  scale_x_date(position = "top",
               expand = c(0.15,0)) +
  scale_size_identity() +
  scale_y_reverse(
    position = "right",
    breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205),
    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),) +
  theme_etr_white() +
  coord_cartesian(ylim = c(205,58)) +
  scale_color_manual(values = rev(c("#37af41",
                                    "#7d42be"))) +
  labs(title = "Gronkowski retirement",
       subtitle = "Russell Gage and Cameron Brate ADP were already climbing\nbefore Gronk's retirement and have continued since",
       x = "",
       y = "ROUND")


## ==== Animated plot ====
# All players
all_player_animation_data <- ggplot() +
  geom_vline(xintercept = as_date("2022-06-21"),
             color = "#666666",
             linetype = "dotted",
             size = 0.8) +
  geom_shadowtext(aes(x = as_date("2022-06-21"),
                      y = 40,
                      label = "RETIREMENT DATE"),
                  vjust = 1,
                  color = "#666666",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  fontface = "bold") +
  geom_line(
    data = bucs_oship_trends,
    aes(x = date,
        y = adp,
        group = player,
        color = color_identifier),
    size = 1.4,
    show.legend = FALSE) +
  geom_image(data = player_headshots_data,
             aes(x = image_date,
                 y = adp,
                 image = headshot_url),
             size = 0.12) +
  geom_shadowtext(aes(x = as_date("2022-05-30"),
                      y = 200,
                      label="Cameron Brate"),
                  color = "#a80d08",
                  bg.color = "#ffffff",
                  bg.r = 0.4,
                  vjust = 0,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Rob Gronkowski"),
                  aes(x = date + 3,
                      y = adp,
                      label = player),
                  color = "#343026",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Russell Gage"),
                  aes(x = date + 3,
                      y = adp,
                      label = player),
                  color = "#a80d08",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Chris Godwin"),
                  aes(x = date + 3,
                      y = adp,
                      label = player,
                      color = color_identifier),
                  color = "#bbbbbb",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Tom Brady"),
                  aes(x = date + 3,
                      y = adp,
                      label = player,
                      color = color_identifier),
                  color = "#bbbbbb",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(data = player_headshots_data %>% 
                    filter(player == "Mike Evans"),
                  aes(x = date + 3,
                      y = adp,
                      label = player,
                      color = color_identifier),
                  color = "#bbbbbb",
                  bg.r = 0.4,
                  bg.color = "#ffffff",
                  hjust = 0,
                  show.legend = FALSE,
                  size = 4,
                  fontface = "bold") +
  geom_curve(
    aes(x = as_date("2022-06-03"),
        xend = as_date("2022-06-11"),
        y = 188,
        yend = 205),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.3,
    color = "#999999") +
  scale_x_date(position = "top",
               expand = c(0.1,0)) +
  scale_size_identity() +
  scale_y_reverse(
    position = "right",
    breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205),
    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),) +
  theme_etr_white() +
  theme(plot.caption = element_markdown()) +
  coord_cartesian(ylim = c(205,0)) +
  scale_color_manual(values = rev(c("#a80d08",
                                    "#343026",
                                    "#bbbbbb"))) +
  labs(title = "Gronkowski retirement",
       subtitle = "Russell Gage and Cameron Brate ADP were already rising\nbefore Gronk's retirement and have continued since",
       x = "",
       y = "ROUND",
       caption = "CHART: @HEERYMICHAEL  |  **ESTABLISH THE RUN**") +
  transition_reveal(-reveal_time)


# animate in a two step process:
animate(all_player_animation_data,
        height = 7,
        width = 7,
        units = "in",
        res = 300,
        duration = 10,
        end_pause = 40,
        renderer = av_renderer())

anim_save("all_player_animation.mp4")


# Three player animation

three_player_animation <- ggplot() +
  geom_vline(xintercept = as_date("2022-06-21"),
             color = "#666666",
             linetype = "dotted",
             size = 0.8) +
  geom_shadowtext(aes(x = as_date("2022-06-21"),
                      y = 61,
                      label = "RETIREMENT DATE\n21st JUNE"),
                  fontface = "bold",
                  vjust = 1,
                  size = 4,
                  color = "#666666",
                  bg.r = 0.4,
                  bg.color = "#ffffff") +
  geom_line(
    data = bucs_oship_trends %>% 
      filter(player != "Chris Godwin") %>% 
      filter(player != "Tom Brady") %>% 
      filter(player != "Mike Evans"),
    aes(x = date,
        y = adp,
        group = player,
        color = color_identifier),
    size = 1.4,
    show.legend = FALSE) +
  geom_image(data = player_headshots_data %>% 
               filter(player != "Chris Godwin") %>% 
               filter(player != "Tom Brady") %>% 
               filter(player != "Mike Evans"),
             aes(x = image_date,
                 y = adp,
                 image = headshot_url),
             size = 0.11,
             by = "height") +
  geom_shadowtext(aes(x = as_date("2022-05-02"),
                      y = 92,
                      label="Rob\nGronkowski"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 0,
                  bg.r = 0.6,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-05-02"),
                      y = 137,
                      label="Russell Gage"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 1,
                  bg.r = 0.6,
                  size = 4,
                  fontface = "bold") +
  geom_shadowtext(aes(x = as_date("2022-05-30"),
                      y = 197,
                      label="Cameron Brate"),
                  color = "#343026",
                  bg.color = "#ffffff",
                  vjust = 0,
                  bg.r = 0.6,
                  size = 4) +
  geom_curve(
    aes(x = as_date("2022-06-08"),
        xend = as_date("2022-06-11"),
        y = 195,
        yend = 205),
    arrow = arrow(length = unit(0.03, "npc")),
    curvature = -0.3,
    color = "#999999") +
  scale_x_date(position = "top",
               expand = c(0.15,0)) +
  scale_size_identity() +
  scale_y_reverse(
    position = "right",
    breaks = c(1,13,25,37,49,61,73,85,97,109,121,133,145,157,169,181,193,205),
    labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),) +
  theme_etr_white() +
  coord_cartesian(ylim = c(205,58)) +
  scale_color_manual(values = rev(c("#a80d08",
                                    "#343026"))) +
  labs(title = "Gronkowski retirement",
       subtitle = "Russell Gage and Cameron Brate ADP were already climbing\nbefore Gronk's retirement and have continued since",
       x = "",
       y = "ROUND") +
  transition_reveal(-reveal_time)

# animate in a two step process:
animate(three_player_animation,
        height = 7,
        width = 7,
        units = "in",
        res = 300,
        duration = 10,
        end_pause = 40,
        renderer = av_renderer())

anim_save("three_player_animation.mp4")


## ==== Saving static images ====

ggsave(all_players_static,filename = "all_players_static.png")
ggsave(three_players_etr_palette, filename = "three_players_etr_palette.png")
ggsave(three_players_static, filename = "three_players_static.png")       


