#### Network ####

# Importar pacotes ------------------------------------------------------------
pacman::p_load(tidyverse, rio, lubridate)


# Rede de copresença ----------------------------------------------------------

#Import file
president <- import("final_president.csv", setclass = "tibble") %>%
  select(title, fhc, lula, dilma, temer, bolsonaro)


# Criar função para encontrar copresença
n_co <- function(base, pres1, pres2){
  base %>%
    count({{pres1}}, {{pres2}}) %>%
    filter(.[1] == T & .[2] == T) %>%
    .[3]
}


# Usar dois loops para fazer todos os presidentes se conectarem com todos
final <- vector("list", 6)
for(i in 2:4){
  total_connect <- vector("list", 6)
for(j in (i+1):6){
  total_connect[[j]] <- president %>%
    n_co(.[i], .[j]) %>%
    mutate(name = str_c(colnames(president)[i], "_", colnames(president)[j]))
}
  final[[i]] <- total_connect %>%
    bind_rows()
  }

# Unificar os presidentes e criar uma tabela em formato do gephi
network <- final %>%
  bind_rows() %>%
  separate(name, c("source", "target"), sep = "_") %>%
  rename(weight = n) %>%
  arrange(-weight)

# Escrever no excel
write_excel_csv(network, "network/article_network.csv")  

president %>%
  count(lula, temer)
  count(fhc, dilma, lula, temer) %>%
  filter(dilma == T & temer == T)
