#### Usar pacotes ####


# Importar valores
pacman::p_load(tidyverse, rio, lubridate)


sci <- list.files("Pesquisa O Globo", pattern = "scie", full.names = T) %>%
  map(import, setclass = "tibble") %>%
  bind_rows() %>%
  mutate(origin = "Scielo") %>%
  select(autor = PT, titulo = BE,
         titulo_2 = TI, titulo_3 = X1,
         titulo_4 = Y1, revista = Z1,
         idioma = SO, tipo = LA, keywords = DT,
         keywords2 = DE, keyword_br = X5, keywords3 = Y5,
         abstract = Z5, abstract_es = AB, abstract_br = X4,
         abstract_fr = Y4, autor_afiliacao = Z4, autor_id = EM,
         coautor_id = RI, bibliografia = OI, vinculo = U2, cidade = PU, 
         disciplina = DI, area = C2)

wos <- list.files("Pesquisa O Globo", pattern = "WoS", full.names = T) %>%
  map(import, setclass = "tibble") %>%
  bind_rows() %>%
  mutate(origin = "Web of Science") %>%
  select(autor = PT, titulo = BE,
         titulo_2 = TI, titulo_3 = X1,
         titulo_4 = Y1, revista = Z1,
         idioma = SO, tipo = LA, keywords = DT,
         keywords2 = DE, keyword_br = X5, keywords3 = Y5,
         abstract = Z5, abstract_es = AB, abstract_br = X4,
         abstract_fr = Y4, autor_afiliacao = Z4, autor_id = EM,
         coautor_id = RI, bibliografia = OI, vinculo = U2, origem_download = EC,
         cidade = PU, disciplina = DI, area = C2)


all_files <- sci %>%
  bind_rows(wos)

