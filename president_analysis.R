#### Análise dos dados ####


# importar pacotes -------------------------------------------------------------

pacman::p_load(tidyverse, rio, lubridate, wesanderson, ggrepel, janitor)

# Imports relevantes -----------------------------------------------------------

# Tabela inicial
president <- import("final_president.csv", setclass = "tibble")

# Paleta padronizada
palette <- wes_palette("Rushmore1", n = 5)
palette[6] <- "#9055A2"
palette[2] <- "#084887"


# Trabalho de dados ------------------------------------------------------------

#Transformar a base de dados em longa e não wide
long_pres <- president %>%
  select(pub_year, fhc, lula, dilma, temer, bolsonaro, collor) %>%
  pivot_longer(-1) %>%
  group_by(pub_year, name) %>%
  summarise(total = sum(value, na.rm = T))


#Primeira visualização sem filtro

long_pres %>%
  filter(pub_year != 2022 & pub_year >= 1995) %>%
  group_by(name) %>%
  mutate(title = ifelse(pub_year == max(pub_year), name, NA)) %>%
  ungroup() %>%
  ggplot(aes(pub_year, total)) +
  geom_point(aes(color = name)) +
  geom_line(aes(color = name, group = name)) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  geom_vline(xintercept = 1995, color = palette[4]) +
  geom_vline(xintercept = 2003, color = palette[5]) +
  geom_vline(xintercept = 2011, color = palette[3]) +
  geom_vline(xintercept = 2016, color = palette[6]) +
  geom_vline(xintercept = 2019, color = palette[1]) +
  geom_label_repel(aes(label = title, fill = name), color = "white")

long_pres %>%
  filter(pub_year != 2022 & pub_year > 1995) %>%
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

president %>%
  select(author, title, total_citation, fhc, lula, dilma, temer, bolsonaro) %>%
  arrange(-total_citation) %>%
  filter(total_citation < 1000)

president %>%
  select(language, pub_year, fhc, lula, dilma, temer, bolsonaro) %>%
  pivot_longer(3:7) %>%
  filter(value == T & pub_year > 1994 & pub_year < 2022) %>%
  count(language, pub_year, name) %>%
  group_by(language) %>%
  mutate(total = sum(n)) %>%
  filter(total > 15) %>%
  ggplot(aes(pub_year, n)) +
  geom_line(aes(color = language, group = language)) +
  geom_point(aes(color = language)) +
  facet_wrap(name~., scales = "free_y") +
  scale_color_manual(values = palette) +
  theme_bw() +
  labs(title = "Idioma dos textos por presidente",
       x = "Ano",
       y = "Número de artigos",
       color = "Idioma")
  
pres_names <- c("jair bolsonaro", "dilma rousseff", "lula", "bolsonaro")

main_key <- president %>%
  mutate(keywords = str_split(tolower(keywords), "; ")) %>%
  unnest(keywords) %>%
  count(keywords) %>%
  arrange(-n) %>%
  filter(!is.na(keywords) & !keywords %in% pres_names) %>%
  slice_max(n, n = 30)

president %>%
  mutate(keywords = str_split(tolower(keywords), "; ")) %>%
  unnest(keywords) %>%
  filter(!is.na(keywords) & keywords %in% main_key$keywords & pub_year > 1994 & pub_year < 2022) %>%
  count(pub_year, keywords) %>%
  ggplot(aes(pub_year, n)) +
  geom_line(aes(color = keywords, group = keywords)) +
  geom_point(aes(color = keywords))


