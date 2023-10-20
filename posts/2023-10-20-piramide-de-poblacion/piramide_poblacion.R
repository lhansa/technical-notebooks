# library(tidyverse)
library(gganimate)
df <- read_csv2("~/../Downloads/56934.csv")


Sys.getlocale("LC_TIME") # "English_United Kingdom.utf8"
Sys.setlocale("LC_TIME", "Spanish")

df_2_plot <- df |> 
  janitor::clean_names() |> 
  filter(edad_simple != "Todas las edades", 
         sexo != "Total", 
         str_detect(edad_simple, "y más", negate = TRUE), 
         !is.na(edad_simple)) |> 
  mutate(
    total = parse_number(total, locale = locale(grouping_mark = ".")), 
    total = if_else(sexo == "Mujeres", -total, total),
    periodo = lubridate::dmy(periodo), 
    edad_simple = str_extract(edad_simple, "\\d+"), 
    edad_simple = factor(edad_simple, levels = 0:100), 
    sexo = fct_relevel(sexo, "Mujeres", "Hombres")
  )

df_2_plot <- df_2_plot |> 
  arrange(periodo, edad_simple) |> 
  filter(
    !is.na(edad_simple), 
    format(periodo, "%m") == "01"
  ) |> 
  mutate(year = format(periodo, "%Y"), 
         year = as.integer(year))

df_2_plot |> 
  filter(is.na(total)) |> 
  print(n = 400)

ggplot(df_2_plot) + 
  geom_col(aes(y = edad_simple, x = total, fill = sexo)) + 
  facet_wrap(~year) +
  scale_x_continuous(
    limits = c(-400000, 400000), 
    breaks = c(-200000, 0, 200000),
    labels = \(x) scales::scientific(abs(x))) + 
  scale_y_discrete(breaks = as.character(seq(0, 100, by = 25))) +
  labs(title = "Población de mujeres y hombres por edad", 
       y = "Edad", x = "", 
       caption = "Fuente: INE") + 
  theme_light() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8))


P <- ggplot(df_2_plot) + 
  geom_col(aes(y = edad_simple, x = total, fill = sexo)) + 
  transition_time(year) + 
  scale_x_continuous(
    limits = c(-400000, 400000), 
    breaks = c(-200000, 0, 200000),
    labels = \(x) scales::scientific(abs(x))) + 
  scale_y_discrete(breaks = as.character(seq(0, 100, by = 25))) +
  labs(title = "Población de mujeres y hombres por edad: {frame_time}", 
       y = "Edad", x = "", 
       caption = "Fuente: INE") + 
  theme_light() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text = element_text(size = 10), 
        axis.text = element_text(size = 10), 
        axis.title = element_text(size = 12))


P_animated <- animate(P, renderer = gifski_renderer(), fps = 5)
anim_save("piramide-poblacion.gif", P_animated)
