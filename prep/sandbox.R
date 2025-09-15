# Add offset columns to your dataframe
df <- df |>
  mutate(
    # Scale confidence to point radius (approximately)
    start_radius = (start_conf - min(c(start_conf, end_conf))) / 
      (max(c(start_conf, end_conf)) - min(c(start_conf, end_conf))) * 
      (6 - 2) + 2,
    end_radius = (end_conf - min(c(start_conf, end_conf))) / 
      (max(c(start_conf, end_conf)) - min(c(start_conf, end_conf))) * 
      (6 - 2) + 2,
    # Adjust for coordinate flip and point edges
    segment_start = start + start_radius * 0.02,  # Right edge of start point
    segment_end = end - end_radius * 0.02,
    case = str_wrap(case, 30)# Left edge of end point
  )

# Then use the adjusted coordinates

subtitle <- "<span style = 'font-size:14pt; color:#E17F48;'>Start Point RRA Level </span>compared with
<span style = 'font-size:14pt; color:#4682B4;'>End Point RRA Level </span>"

labels = str_wrap(c("Minimal and/or Restricted Anticorruption Engagement/Action",
           "Limited Anticorruption Engagement/Action",
           "Emerging Anticorruption Engagement/Action",
           "Preliminary Government Response",
           "Government Responsiveness",
           "Accountable Responsiveness"), 20)

df |> 
  ggplot(aes(x = fct_reorder(case, change))) +
  geom_point(aes(y = start, size = start_conf), color = "#E17F48", alpha = 1) +
  geom_point(aes(y = end, size = end_conf), color = "#4682B4", alpha = 1) +
  geom_segment(aes(y = segment_start, yend = segment_end, xend = fct_reorder(case, change)), 
               size = .7, arrow = arrow(length = unit(0.3, "cm"))
  ) +
  scale_size_continuous(range = c(2, 6), name = "Confidence",
                        breaks = c(1, 2, 3)
                        , labels = c("low", "medium", "high")) +
  scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5), 
                     labels = labels)+
  labs(title = "Change in RRA Level Over Time",
       subtitle = subtitle,
       y = "RRA Level", x = "") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 16),
        plot.subtitle = element_markdown(size = 14),
        legend.position = "right")

  