#### Análise dos dados ####


# importar pacotes -------------------------------------------------------------

pacman::p_load(tidyverse, rio, lubridate, wesanderson)

# Imports relevantes -----------------------------------------------------------

# Tabela inicial
president <- import("final_president.csv", setclass = "tibble")

# Paleta padronizada
palette <- wes_palette("Rushmore1", n = 5)



# Trabalho de dados ------------------------------------------------------------

#Transformar a base de dados em longa e não wide
long_pres <- president %>%
  select(pub_year, fhc, lula, dilma, temer, bolsonaro) %>%
  pivot_longer(-1) %>%
  group_by(pub_year, name) %>%
  summarise(total = sum(value, na.rm = T))


#Primeira visualização sem filtro

long_pres %>%
  filter(pub_year != 2022) %>%
  ggplot(aes(pub_year, total)) +
  geom_point(aes(color = name)) +
  geom_line(aes(color = name, group = name)) +
  scale_color_manual(values = palette) +
  geom_vline(xintercept = 1995, color = palette[3]) +
  geom_vline(xintercept = 2003, color = palette[4]) +
  geom_vline(xintercept = 2011, color = palette[2]) +
  geom_vline(xintercept = 2016, color = palette[5]) +
  geom_vline(xintercept = 2019, color = palette[1])

long_pres %>%
  filter(pub_year != 2022) %>%
  mutate(in_charge = case_when(pub_year < 2003 ~"fhc",
                               pub_year < 2011 & pub_year > 2002 ~"lula",
                               pub_year < 2016 & pub_year > 2010 ~"dilma",
                               pub_year < 2019 & pub_year > 2015 ~"temer",
                               pub_year >= 2019 & pub_year > 2018 ~"bolsonaro")) %>%
  filter(name == in_charge) %>%
  ggplot(aes(pub_year, total)) +
  geom_point(aes(color = name)) +
  geom_line(aes(color = name, group = name)) +
  scale_color_manual(values = palette)


big_ra <- president %>%
  mutate(research_area = str_split(research_area, "; ")) %>%
  unnest(research_area) %>%
  count(research_area) %>%
  arrange(-n) %>%
  slice_max(n, n = 10)


president %>%
  filter(pub_year != 2022) %>%
  filter(research_area %in% big_ra$research_area) %>%
  mutate(research_area = str_split(research_area, "; ")) %>%
  unnest(research_area) %>%
  count(pub_year, research_area) %>%
  arrange(-n) %>%
  ggplot(aes(pub_year, n)) +
  geom_line(aes(group = research_area, color = research_area), show.legend = F) +
  geom_point(aes(color = research_area), show.legend = F) +
  facet_wrap(~research_area)
  
