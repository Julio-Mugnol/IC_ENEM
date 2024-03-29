library(readxl)
library(tidyverse)

# 2018 ---------------
sin2018 <- read_excel("data/sinopses_estatisticas_educacao_basica/Sinopse_Estatistica_da_Educa��o_Basica_2018.xlsx", 
                      sheet = "Ensino M�dio 1.25",
                      col_names = FALSE,
                      skip = 11)

sin2018 <- sin2018 %>% 
  select(regiao = ...1,
         uf = ...2,
         ter_fed = ...17,
         ter_est = ...18,
         ter_mun = ...19,
         ter_pri = ...20,
         qua_fed = ...22,
         qua_est = ...23,
         qua_mun = ...24,
         qua_pri = ...25) %>% 
  filter(is.na(uf) & !is.na(ter_fed)) %>% 
  select(-uf) %>% 
  pivot_longer(cols = c(ter_fed, ter_est, ter_mun, ter_pri,
                        qua_fed, qua_est, qua_mun, qua_pri),
               names_to = c("serie", "dep_adm"),
               names_sep = c("_"),
               values_to = "n") %>% 
  mutate(ano = "2018")

# 2019 ---------------
sin2019 <- read_excel("data/sinopses_estatisticas_educacao_basica/Sinopse_Estatistica_da_Educa��o_Basica_2019.xlsx", 
                      sheet = "Ensino M�dio 1.25",
                      col_names = FALSE,
                      skip = 11)

sin2019 <- sin2019 %>% 
  select(regiao = ...1,
         uf = ...2,
         ter_fed = ...17,
         ter_est = ...18,
         ter_mun = ...19,
         ter_pri = ...20,
         qua_fed = ...22,
         qua_est = ...23,
         qua_mun = ...24,
         qua_pri = ...25) %>% 
  filter(is.na(uf) & !is.na(ter_fed)) %>% 
  select(-uf) %>% 
  pivot_longer(cols = c(ter_fed, ter_est, ter_mun, ter_pri,
                        qua_fed, qua_est, qua_mun, qua_pri),
               names_to = c("serie", "dep_adm"),
               names_sep = c("_"),
               values_to = "n") %>% 
  mutate(ano = "2019")

# 2020 ---------------
sin2020 <- read_excel("data/sinopses_estatisticas_educacao_basica/Sinopse_Estatistica_da_Educa��o_Basica_2020.xlsx", 
                      sheet = "Ensino M�dio 1.25",
                      col_names = FALSE,
                      skip = 11)

sin2020 <- sin2020 %>% 
  select(regiao = ...1,
         uf = ...2,
         ter_fed = ...17,
         ter_est = ...18,
         ter_mun = ...19,
         ter_pri = ...20,
         qua_fed = ...22,
         qua_est = ...23,
         qua_mun = ...24,
         qua_pri = ...25) %>% 
  filter(is.na(uf) & !is.na(ter_fed)) %>% 
  select(-uf) %>% 
  pivot_longer(cols = c(ter_fed, ter_est, ter_mun, ter_pri,
                        qua_fed, qua_est, qua_mun, qua_pri),
               names_to = c("serie", "dep_adm"),
               names_sep = c("_"),
               values_to = "n") %>% 
  mutate(ano = "2020")

# 2021 ---------------
sin2021 <- read_excel("data/sinopses_estatisticas_educacao_basica/Sinopse_Estatistica_da_Educa��o_Basica_2021.xlsx", 
                      sheet = "Ensino M�dio 1.25",
                      col_names = FALSE,
                      skip = 11)

sin2021 <- sin2021 %>% 
  select(regiao = ...1,
         uf = ...2,
         ter_fed = ...17,
         ter_est = ...18,
         ter_mun = ...19,
         ter_pri = ...20,
         qua_fed = ...22,
         qua_est = ...23,
         qua_mun = ...24,
         qua_pri = ...25) %>% 
  filter(is.na(uf) & !is.na(ter_fed)) %>% 
  select(-uf) %>% 
  pivot_longer(cols = c(ter_fed, ter_est, ter_mun, ter_pri,
                        qua_fed, qua_est, qua_mun, qua_pri),
               names_to = c("serie", "dep_adm"),
               names_sep = c("_"),
               values_to = "n") %>% 
  mutate(ano = "2021")

# 2022 ---------------
sin2022 <- read_excel("data/sinopses_estatisticas_educacao_basica/Sinopse_Estatistica_da_Educa��o_Basica_2022.xlsx", 
                      sheet = "Ensino M�dio 1.25",
                      col_names = FALSE,
                      skip = 11)

sin2022 <- sin2022 %>% 
  select(regiao = ...1,
         uf = ...2,
         ter_fed = ...17,
         ter_est = ...18,
         ter_mun = ...19,
         ter_pri = ...20,
         qua_fed = ...22,
         qua_est = ...23,
         qua_mun = ...24,
         qua_pri = ...25) %>% 
  filter(is.na(uf) & !is.na(ter_fed)) %>% 
  select(-uf) %>% 
  pivot_longer(cols = c(ter_fed, ter_est, ter_mun, ter_pri,
                        qua_fed, qua_est, qua_mun, qua_pri),
               names_to = c("serie", "dep_adm"),
               names_sep = c("_"),
               values_to = "n") %>% 
  mutate(ano = "2022")

# Sinopse ---------------
sinopse <- sin2018 %>% 
  bind_rows(sin2019, sin2020, sin2021, sin2022) %>% 
  mutate(regiao = as_factor(regiao),
         regiao = fct_collapse(regiao,
                               "N" = "Norte",
                               "NE" = "Nordeste",
                               "SE" = "Sudeste",
                               "S" = "Sul",
                               "CO" = "Centro-Oeste"))

save(sinopse, file = "sample/sinopse.RData")

sinopse %>% 
  group_by(ano) %>% 
  summarise(n = sum(n))


