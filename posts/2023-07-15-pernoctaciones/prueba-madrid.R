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
  
