
# Pacotes -----------------------------------------------------------------

library(tidyverse)


# Carregando dados da semana 26 -------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2022-07-05')

rent <- tuesdata$rent

# Tratamento --------------------------------------------------------------

rent <- rent %>% 
  select(year, city, price)

unique(rent$city)


rent %>%
  group_by(city, year) %>%
  summarise(media = median(price)) %>%
  mutate(pct_growth = media / lag(media) - 1) %>%
  drop_na() %>%
  mutate(sum_pct_growth = cumsum(pct_growth)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = sum_pct_growth, color = city)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(max(sum_pct_growth) > 4,
                           label_params = list(size = 4, vjust = 0.5, hjust = 1, label.size = NA, alpha = 0.8)) +
  scale_y_continuous(
    breaks = c(-1:8),
    position = "right",
    labels = c(
      "-100%",
      "0%",
      "100%",
      "200%",
      "300%",
      "400%",
      "500%",
      "600%",
      "700%",
      "800%"
    )
  ) +
  scale_x_continuous(n.breaks = 10) +
  theme_minimal() +
  scale_color_manual(values = c("#1f306e", "#553772", "#8f3b76", "#c7417b", "#f5487f"))+
  labs(title = "Median rent price valorization by the years in San Francisco",
       subtitle = "Top 5 cities",
       caption = "Source: data.sfgov.org")+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold",
                                      size = 15, hjust = 0.5),
        plot.subtitle = element_text(size = 15, vjust = 0, hjust = 0.5, 
                                         face = "italic", colour = "#14213d"),
        plot.caption = element_text(size = 8, hjust = 0.9),
        plot.background = element_rect(fill = "#f1e8e6"),
        text = element_text(colour = "#f55951"))




