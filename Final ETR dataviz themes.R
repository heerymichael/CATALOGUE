

## ==== {ggplot} theme ====

theme_etr <- function() {
  theme(
    
    # setting up no background colour
    rect = element_rect(color = "#ffffff",
                        fill = "#ffffff"),
    plot.background = element_rect(color ="#ffffff",
                                   fill = "#ffffff"),
    panel.background = element_rect(color ="#ffffff",
                                    fill = "#ffffff"),
    strip.background = element_rect(color ="#ffffff",
                                    fill = "#ffffff"),
    
    
    # Setting default text font and colour
    text = element_text(family = "Chivo",
                        colour = "#333333"),
    
    # Plot margins
    
    plot.margin = unit(c(t = 0.75,
                         l = 0.75,
                         b = 0.5,
                         r = 0.75),
                       "cm"),
    
    # Controlling plot title, subtitle and caption formatting
    plot.title.position = "plot",
    plot.title = element_text(size = 30,
                              face = "bold"),
    plot.subtitle = element_text(size = 20,
                                 margin = margin(b = 0.5,
                                                 unit = "cm")),
    plot.caption = element_markdown(size = 16,
                                    color = "#999999",
                                    margin = margin(t = 0.5,
                                                    unit = "cm")),
    strip.text = element_text(size = 14),
    
    # gridlines
    panel.grid.major = element_line(color = "#dddddd",
                                    size = 0.2),
    panel.grid.minor = element_blank(),
    
    # Axis text
    axis.text.x = element_text(size = 14, color = "#999999"),
    axis.text.y = element_text(size = 14, color = "#999999"),
    axis.title = element_text(size = 14, color = "#999999"),
    
    # removing tickmarks
    axis.ticks = element_blank()
    
  )
}

## ==== {gt} theme ====

gt_theme_etr <- function(data, ...){
  data %>% 
    opt_all_caps(locations = c("column_labels"))  %>%
    opt_table_font(font = "Chivo") %>% 
    tab_options(
    table_body.border.top.width = px(2),
    table_body.border.top.color = "#808080",
    column_labels.background.color = "white",
    table.border.top.width = px(2),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(1),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(0.8),
    column_labels.border.bottom.color = "#808080",
    table_body.border.bottom.color = "#808080",
    table_body.border.bottom.width = px(2),
    data_row.padding = px(3),
    source_notes.font.size = 14,
    table.font.size = 16,
    heading.align = "left",
    heading.title.font.size = 24,
    heading.title.font.weight = "bolder",
    heading.subtitle.font.size = 16,
    heading.subtitle.font.weight = "lighter",

  ...
  ) 
}
    
    