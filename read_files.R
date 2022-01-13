#### Usar pacotes ####


# Baixar pacotes
pacman::p_load(tidyverse, rio, lubridate)


import_sci <- function(base, correct){
  importado <- import(base, setclass = "tibble") %>%
    select(author = AU, title = TI, title_2 = X1,
           title_3 = Y1, title_4 = Z1,
           publication_name = SO, language = LA, type = DT,
           keywords = DE, keyword_2 = X5, keywords_br = Y5, keywords_4 = Z5,
           abstract = AB, abstract_es = X4, abstract_br = Y4,
           abstract_fr = Z4, author_afiliation = C1, email = EM,
           researcher_id = RI, orcid = OI, cited_ref = CR, n_ref = NR, citation = TC, total_citation = Z9,
           publisher = PU, publisher_city = PI, issn = SN, pub_year = PY, volume = VL, issue = IS,
           begining_page = BP, end_page = EP, doi = DI, subject = EC, origin = C2, research_area = SC) %>%
    mutate_at(vars(begining_page, end_page), as.numeric) %>%
    mutate(pages = end_page - begining_page + 1) %>%
    select(-end_page, -begining_page) %>%
    mutate_at(vars(n_ref, citation, total_citation), as.numeric) %>%
    mutate(pref = correct)
  return(importado)
}

import_wos <- function(base, correct){
  importado <- import(base, setclass = "tibble") %>%
           mutate(origin = "Web of Science") %>%
           select(author = AF, title = TI, publication_name = SO, language = LA,
                  type = DT, conference = CT, year_conf = CY,
                  city_conf = CL, institution = SP,
                  keywords = DE, keyword_plus = ID,
                  abstract = AB, author_afiliation = C1, email = EM,
                  researcher_id = RI, orcid = OI, funding = FU,
                  cited_ref = CR, n_ref = NR, citation = TC, total_citation = Z9,
                  publisher = PU, publisher_city = PI, issn = SN, pub_year = PY, volume = VL, issue = IS,
                  doi = DI, pages = PG, subject = WC, research_area = SC) %>%
           mutate_at(vars(n_ref, citation, volume), as.numeric) %>%
           mutate(pref = correct)
  return(importado)
}

# Importar tabelas

sci <- import_sci("Pesquisa O Globo/scielo.xlsx", F)
sci_2 <- import_sci("bases/termos_scielo.xlsx", T)
wos <- import_wos("Pesquisa O Globo/wos.xlsx", F)
wos_2 <- import_wos("bases/termos_wos.xlsx", T)

join_bases <- function(base, base_2){
  base %>%
    bind_rows(base_2) %>%
    distinct() %>%
    group_by(title) %>%
    mutate(n_ref = max(n_ref),
           citation = max(citation),
           total_citation = max(total_citation)) %>%
    ungroup() %>%
    unique() %>%
    group_by(title) %>%
    mutate(total = n()) %>%
    filter(total == 1 | pref == T)
}

sci_final <- join_bases(sci, sci_2)
wos_final <- join_bases(wos, wos_2)

# Join all

all_files <- sci_final %>%
  bind_rows(wos_final)
#Escrever excel final

#Encontrar o presidente entre as colunas
find_pres <- function(base, column, word){
  base %>%
    mutate({{ column }} := str_detect(tolower(title), word)) %>%
    mutate({{column}} := ifelse({{column}} == F|is.na({{column}}), str_detect(tolower(keywords), word), {{column}})) %>%
    mutate({{column}} := ifelse({{column}} ==F|is.na({{column}}), str_detect(tolower(abstract), word), {{column}})) %>%
    mutate({{column}} := ifelse({{column}} ==F|is.na({{column}}), str_detect(tolower(abstract_br), word), {{column}}))
}

#Buscar para cada um
president <- all_files %>%
  find_pres(fhc, "fernando|fhc|plano real") %>%
  find_pres(lula, "lula|lulismo") %>%
  find_pres(dilma, "dilma") %>%
  find_pres(temer, "temer") %>%
  find_pres(bolsonaro, "bolsonaro|bolsonarismo") %>%
  find_pres(collor, "collor")

write_excel_csv(president, "final_president.csv")


