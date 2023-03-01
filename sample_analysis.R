library(tidyverse)
library(MASS)
library(olsrr)

sample <- data.table::fread(input='sample/sample_c.csv',
                            integer64='character',
                            skip=0,  #Ler do inicio
                            nrow=-1, #Ler todos os registros
                            na.strings = "",
                            encoding = 'Latin-1',
                            showProgress = TRUE)


sample <- sample %>% 
  as_tibble() %>% 
  mutate(across(c(TP_COR_RACA, Q002, Q025, NU_ANO), as_factor)) %>% 
  mutate(TP_COR_RACA = fct_relevel(TP_COR_RACA, c(as.character(1:5))),
         Q002 = fct_relevel(Q002, c(LETTERS[1:7])),
         Q025 = fct_relevel(Q025, c(LETTERS[1:2]))) %>% 
  mutate(TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "Branca/Amarela" = c("1", "4"),
                                    "Preta/Parda/Indígena" = c("2", "3", "5")),
         Q002 = fct_collapse(Q002,
                             "Fundamental incompleto" = c("A", "B", "C"),
                             "Fundamental completo" = "D",
                             "Médio completo" = "E",
                             "Superior completo" = c("F", "G")),
         Q025 = fct_collapse(Q025,
                             "Sim" = "B",
                             "Não" = "A"))

sample_reg <- sample %>% 
  mutate(REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         across(REGIAO, as_factor),
         REGIAO = fct_relevel(REGIAO, c(as.character(1:5)))) %>% 
  mutate(REGIAO = fct_collapse(REGIAO,
                               "Norte" = "1",
                               "Nordeste" = "2",
                               "Sudeste" = "3",
                               "Sul" = "4",
                               "Centro-Oeste" = "5"),
         COVID = as_factor(NU_ANO %in% c("2020", "2021")),
         NU_ANO = fct_relevel(as.factor(NU_ANO), as.character(c(2019, 2018, 2020, 2021))),
         NU_NOTA_OBJETIVA = (NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_MT) / 4) %>%
  mutate(RACA = TP_COR_RACA, EDUC_MAE = Q002, INTERNET = Q025) %>% 
  dplyr::select(NU_INSCRICAO, RACA, EDUC_MAE, INTERNET, REGIAO, NU_ANO, COVID,
                NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO, NU_NOTA_OBJETIVA)

fit <- lm(NU_NOTA_OBJETIVA ~ RACA + EDUC_MAE + REGIAO + INTERNET + NU_ANO +
            COVID : (RACA + EDUC_MAE + REGIAO + INTERNET), 
            data = sample_reg)
summary(fit)

robust <- rlm(NU_NOTA_OBJETIVA ~ RACA + EDUC_MAE + REGIAO + INTERNET + NU_ANO + 
                COVID * (RACA + EDUC_MAE + REGIAO + INTERNET), 
              data = sample_reg)
summary(robust)

saveRDS(model, file = "model/model1.rda")
saveRDS(robust, file = "model/robust1.rda")

model <- readRDS(file = "model/model1.rda")
summary(model)

robust <- readRDS(file = "model/robust1.rda")
summary(robust)

ols_test_breusch_pagan(model)
