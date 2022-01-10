#### Usar pacotes ####


# Baixar pacotes
pacman::p_load(tidyverse, rio, lubridate)


# Importar tabelas

sci <- import("Pesquisa O Globo/scielo.xlsx", setclass = "tibble") %>%
  select(author = AU, title = TI, title_2 = X1,
         title_3 = Y1, title_4 = Z1,
         publication_name = SO, idioma = LA, tipo = DT,
         keywords = DE, keyword_2 = X5, keywords_br = Y5, keywords_4 = Z5,
         abstract = AB, abstract_es = X4, abstract_br = Y4,
         abstract_fr = Z4, author_afiliation = C1, email = EM,
         researcher_id = RI, orcid = OI, cited_ref = CR, n_ref = NR, citation = TC, total_citation = Z9,
         publisher = PU, publisher_city = PI, issn = SN, pub_year = PY, volume = VL, issue = IS,
         begining_page = BP, end_page = EP, doi = DI, subject = EC, origin = C2, research_area = SC) %>%
  mutate_at(vars(begining_page, end_page), as.numeric) %>%
  mutate(pages = end_page - begining_page + 1) %>%
  select(-end_page, -begining_page)


wos <- import("Pesquisa O Globo/wos.xlsx", setclass = "tibble") %>%
  mutate(origin = "Web of Science") %>%
  select(autor = AF, titulo = TI, publication_name = SO, idioma = LA,
         tipo = DT, conferencia = CT, ano_conf = CY,
         cidade_conf = CL, instituicao = SP,
         keywords = DE, keyword_plus = ID,
         abstract = AB, author_afiliation = C1, email = EM,
         researcher_id = RI, orcid = OI, funding = FU,
         cited_ref = CR, n_ref = NR, citation = TC, total_citation = Z9,
         publisher = PU, publisher_city = PI, issn = SN, pub_year = PY, volume = VL, issue = IS,
         doi = DI, pages = PG, subject = WC, research_area = SC)


all_files <- sci %>%
  bind_rows(wos)

find_pres <- function(base, coluna, presidente){
  base %>%
    mutate(!!coluna = str_detect(tolower(titulo), presidente))
}
all_files %>%
  find_pres(fhc, "fernando")
