epi_viral <- aislamiento %>% 
  filter(year(FECHA) == 2024) %>%  # filtro de año para 2024
  mutate(viral = case_when(value == "Enterovirus" ~ "Enterovirus",
                           value == "enterovirus" ~ "Enterovirus",
                           value == "enterovirus humano" ~ "Enterovirus",
                           value == "Rhinovirus"~ "Rhinovirus",
                           value == "Rinovirus"~ "Rhinovirus",
                           value == "Human Rhinovirus/Enterovirus"~"Rhinovirus",
                           value == "Rinovirus/Enterovirus Humano"~"Rhinovirus",
                           value == "metaneumovirus" ~ "metaneumovirus",
                           value == "Metaneumovirus" ~ "metaneumovirus",
                           value == "Metapneumovirus" ~ "metaneumovirus",
                           value == "metapneumovirus"~ "metaneumovirus",
                           value == "Human metapneumovirus"~ "metaneumovirus",
                           value == "Human Metapneumovirus"~ "metaneumovirus",
                           value == "Influenza A/H1-2009"~ "Influenza AH1N1",
                           value == "Influenza A H1-2009"~"Influenza AH1N1",
                           value == "Influenza A H1 – 2009"~"Influenza AH1N1",
                           value == "Adenovirus" ~ "Adenovirus",
                           value == "Influenza A" ~ "Influenza A",
                           value == "Influenza A H3" ~ "Influenza A",
                           value == "Respiratory Syncytial Virus"~ "VSR",
                           value == "Virus Sincitial Respiratorio"~ "VSR",
                           value == "Virus Syncitial Respiratorio"~ "VSR",
                           value == "Respiratory syncytial virus"~ "VSR",
                           value == "SARS-CoV-2"~"SARS-CoV-2",
                           value == "Coronavirus SARS Cov 2"~"SARS-CoV-2",
                           value == "Influenza B"~"Influenza B",
                           value == "Parainfluenza Virus" | value == "Parainfluenza Virus 1" | 
                             value == "Parainfluenza Virus 2" | value == "Parainfluenza Virus 3" | 
                             value == "Parainfluenza Virus 4"| value == "Parainfluenza Virus" ~ "Parainfluenza Virus (1,2,3)")) %>% 
  filter(is.na(viral)==FALSE) %>% 
  count(viral, gr_semana) %>% 
  filter(!is.na(gr_semana)) %>% 
  pivot_wider(id_cols = viral, names_from = gr_semana, values_from = n, values_fill = 0)
  