df_pernoctaciones |> 
  filter(str_detect(comunidad, "Madrid"), mes < as.Date("2023-01-01")) |> 
  mutate(year = format(mes, format = "%Y")) |> 
  group_by(year) |> 
  summarise(pernoctaciones = sum(pernoctaciones, na.rm = TRUE)) |> 
  ggplot(aes(x = year, y = pernoctaciones)) +
  geom_col() + 
  labs(x = "", y = "Pernoctaciones", 
       title = "Pernoctaciones, Comunidad de Madrid", 
       caption = "Fuente: INE. Elaboración: longitudsinanchura.com") + 
  theme_light()
  

df |> 
  select(year, matrimonios) |> 
  ggplot(aes(x = as.numeric(year), y = matrimonios)) + 
  geom_col(fill = "#800080") +
  labs(x = "", y = "Matrimonios", 
       title = "Evolución de matrimonios en España", 
       caption = "Fuente: INE. Elaboración: longitudsinanchura.com") + 
  scale_y_continuous(labels = scales::scientific)
