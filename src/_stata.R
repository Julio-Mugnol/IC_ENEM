library(tidyverse)

load("sample/sample.RData")
sample <- sample %>% 
  filter(raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd" & presente) %>% 
  droplevels(.) %>% 
  mutate(raca = ifelse(raca == "pre", 1, 0),
         escola = ifelse(escola == "publ", 1, 0),
         internet = ifelse(internet == "N", 1, 0),
         sexo = ifelse(sexo == "F", 1, 0),
         educ_mae = case_when(educ_mae == "fund_inc" ~ 1,
                              educ_mae == "fund" ~ 2,
                              educ_mae == "med" ~ 3,
                              educ_mae == "sup" ~ 4),
         regiao = case_when(regiao == "N" ~ 1,
                            regiao == "NE" ~ 2,
                            regiao == "CO" ~ 3,
                            regiao == "SE" ~ 4,
                            regiao == "S" ~ 5),
         ano_20 = ifelse(ano == 2020, 1, 0),
         ano_21 = ifelse(ano == 2021, 1, 0),
         ano_22 = ifelse(ano == 2022, 1, 0)) %>% 
  select(nota, ano, ano_20, ano_21, ano_22, raca, sexo, educ_mae,
         regiao, escola, internet, muni_prova) %>% 
  drop_na(.)

sample %>% 
  group_by(internet) %>% 
  count()

write.csv(sample, file = "sample/sample_stata.csv")
