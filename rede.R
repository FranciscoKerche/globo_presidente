#### Network ####

# Importar pacotes ------------------------------------------------------------
pacman::p_load(tidyverse, rio, lubridate)


# Criar código para transformar em rede de copresença

president <- import("final_president.csv", setclass = "tibble") %>%
  select(title, fhc, lula, dilma, temer, bolsonaro)

n_co <- function(base, pres1, pres2){
  base %>%
    count({{pres1}}, {{pres2}}) %>%
    filter(.[1] == T & .[2] == T) %>%
    .[3]
}

final <- vector("list", 6)
for(i in 2:5){
  total_connect <- vector("list", 6)
for(j in (i+1):6){
  testing <- president[j]
  total_connect[[j]] <- president %>%
    n_co(.[2], .[j]) %>%
    mutate(name = str_c(colnames(president)[i], "_", colnames(president)[j]))
}
  final[[i]] <- total_connect %>%
    bind_rows()
  }

network <- final %>%
  bind_rows() %>%
  separate(name, c("source", "target"), sep = "_") %>%
  rename(weight = n)

write_excel_csv(network, "network/article_network.csv")  
