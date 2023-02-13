library(tidyverse)

sample <- data.table::fread(input='sample/sample1.csv',
                            integer64='character',
                            skip=0,  #Ler do inicio
                            nrow=-1, #Ler todos os registros
                            na.strings = "",
                            encoding = 'Latin-1',
                            showProgress = TRUE)

sample <- sample %>% 
  as_tibble() %>% 
  mutate(across(c(TP_COR_RACA, Q002, Q025, NU_ANO), as_factor)) %>% 
  mutate(TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "Não declarado" = "0",
                                    "Branca/Amarela" = c("1", "4"),
                                    "Preta/Parda/Indígena" = c("2", "3", "5"),
                                    "Não dispõe da informação" = "6"),
         Q002 = fct_collapse(Q002,
                             "Fundamental incompleto" = c("A", "B", "C"),
                             "Fundamental completo" = "D",
                             "Médio completo" = "E",
                             "Superior completo" = c("F", "G"),
                             "Não sei" = "H"),
         Q025 = fct_collapse(Q025,
                             "Sim" = "B",
                             "Não" = "A"))

sample_reg <- sample %>% 
  mutate(REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         across(REGIAO, as_factor)) %>% 
  mutate(REGIAO = fct_collapse(REGIAO,
                               "Norte" = "1",
                               "Nordeste" = "2",
                               "Sudeste" = "3",
                               "Sul" = "4",
                               "Centro-Oeste" = "5"),
         COVID = as_factor(ifelse(
           fct_match(NU_ANO, c("2020", "2021")), "Sim", "Não")),
         NU_NOTA_OBJETIVA = (NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_MT) / 4) %>%
  mutate(RACA = TP_COR_RACA, EDUC_MAE = Q002, INTERNET = Q025) %>% 
  select(NU_INSCRICAO, RACA, EDUC_MAE, INTERNET, REGIAO, COVID,
         NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO, NU_NOTA_OBJETIVA)

sample_reg %>% 
  summarise(internet = fct_count(INTERNET))
sample_reg %>% 
  summarise(regiao = fct_count(REGIAO))
sample_reg %>% 
  summarise(escolaridade = fct_count(EDUC_MAE)) 
sample_reg %>% 
  summarise(raca = fct_count(RACA))
sample_reg %>% 
  summarise(covid = fct_count(COVID))

model <- lm(NU_NOTA_OBJETIVA ~ RACA + EDUC_MAE + REGIAO + INTERNET + COVID + 
              COVID * (RACA + EDUC_MAE + REGIAO + INTERNET), 
            data = sample_reg); summary(model)

saveRDS(model, file = "model1.rda")


