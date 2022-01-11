#### Análise dos dados ####


# importar pacotes

pacman::p_load(tidyverse, rio, lubridate)

# Visões básicas

president <- import("final_president.csv", setclass = "tibble")

long_pres <- president %>%
  select(pub_year, fhc, lula, dilma, temer, bolsonaro) %>%
  pivot_longer(-1) %>%
  group_by(pub_year, name) %>%
  summarise(total = sum(value, na.rm = T))

long_pres %>%
  filter(pub_year != 2022) %>%
  ggplot(aes(pub_year, total)) +
  geom_point(aes(color = name)) +
  geom_line(aes(color = name, group = name)) +
  geom_vline(xintercept = 1995, color = "dark green") +
  geom_vline(xintercept = 2003, color = "blue") +
  geom_vline(xintercept = 2011, color = "yellow") +
  geom_vline(xintercept = 2016, color = "pink") +
  geom_vline(xintercept = 2019, color = "red")

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
  geom_line(aes(color = name, group = name))

