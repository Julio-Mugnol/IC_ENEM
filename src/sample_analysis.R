library(tidyverse)
library(MASS)
library(olsrr)
library(broom)

sample <- data.table::fread(input='sample/sample_c2.csv',
                            integer64='character',
                            skip=0,  #Ler do inicio
                            nrow=-1, #Ler todos os registros
                            na.strings = "",
                            encoding = 'Latin-1',
                            showProgress = TRUE)


# sample <- sample %>% 
#   as_tibble() %>% 
#   mutate(across(c(TP_COR_RACA, TP_ESCOLA, Q002, Q025, NU_ANO), as_factor)) %>% 
#   mutate(TP_COR_RACA = fct_relevel(TP_COR_RACA, c(as.character(1:5))),
#          TP_ESCOLA = fct_relevel(TP_ESCOLA, c(as.character(2:4))),
#          Q002 = fct_relevel(Q002, c(LETTERS[1:7])),
#          Q025 = fct_relevel(Q025, c(LETTERS[1:2]))) %>% 
#   mutate(TP_COR_RACA = fct_collapse(TP_COR_RACA,
#                                     "Branca/Amarela" = c("1", "4"),
#                                     "Preta/Parda/Indígena" = c("2", "3", "5")),
#          TP_ESCOLA = fct_collapse(TP_ESCOLA,
#                                   "Pública" = "2",
#                                   "Privada" = c("3", "4")),
#          Q002 = fct_collapse(Q002,
#                              "Fundamental incompleto" = c("A", "B", "C"),
#                              "Fundamental completo" = "D",
#                              "Médio completo" = "E",
#                              "Superior completo" = c("F", "G")),
#          Q025 = fct_collapse(Q025,
#                              "Sim" = "B",
#                              "Não" = "A"))

sample <- sample %>% 
  as_tibble() %>% 
  mutate(across(c(TP_COR_RACA, TP_ESCOLA, Q002, Q025), as_factor)) %>% 
  mutate(TP_COR_RACA = fct_relevel(TP_COR_RACA, c(as.character(1:5))),
         TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "bra" = c("1", "4"),
                                    "pre" = c("2", "3", "5")),
         TP_ESCOLA = fct_relevel(TP_ESCOLA, c(as.character(2:4))),
         TP_ESCOLA = fct_collapse(TP_ESCOLA,
                                  "publ" = "2",
                                  "priv" = c("3", "4")),
         Q002 = fct_relevel(Q002, c(LETTERS[1:7])),
         Q002 = fct_collapse(Q002,
                             "fund_inc" = c("A", "B", "C"),
                             "fund" = "D",
                             "med" = "E",
                             "sup" = c("F", "G")),
         Q025 = fct_relevel(Q025, c(LETTERS[1:2])),
         Q025 = fct_collapse(Q025,
                             "S" = "B",
                             "N" = "A"))

sample_reg <- sample %>% 
  mutate(REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         across(REGIAO, as_factor),
         REGIAO = fct_relevel(REGIAO, c(as.character(1:5))),
         REGIAO = fct_collapse(REGIAO,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         ANO_18 = NU_ANO == 2018,
         ANO_19 = NU_ANO == 2019,
         ANO_20 = NU_ANO == 2020,
         ANO_21 = NU_ANO == 2021,
         NU_NOTA = (NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5,
         across(c(ANO_18, ANO_19, ANO_20, ANO_21), as_factor)) %>%
  mutate(RACA = TP_COR_RACA, EDUC_MAE = Q002, INTERNET = Q025, ESCOLA = TP_ESCOLA) %>% 
  dplyr::select(RACA, EDUC_MAE, INTERNET, REGIAO, ESCOLA, ANO_18, ANO_19, ANO_20, ANO_21,
                NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO, NU_NOTA)

fit <- lm(NU_NOTA ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
            COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
            data = sample_reg)


fit <- lm(NU_NOTA ~ ANO_19 + ANO_20 + ANO_21 +
            ANO_20 * (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET) +
            ANO_21 * (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
          data = sample_reg)
summary(fit)
summ(fit, scale = TRUE, digits = 3, robust = TRUE)

predict(fit, data.frame(
  RACA = "Preta/Parda/Indígena",
  EDUC_MAE = "Fundamental incompleto",
  REGIAO = "Norte",
  ESCOLA = "Pública",
  INTERNET = "Não",
  ANO_2019 = "TRUE",
  COVID_2020 = "FALSE",
  COVID_2021 = "FALSE"
), interval = c("conf"), se.fit = TRUE)

predict(fit, data.frame(
  RACA = "Branca/Amarela",
  EDUC_MAE = "Superior completo",
  REGIAO = "Sudeste",
  ESCOLA = "Privada",
  INTERNET = "Sim",
  ANO_2019 = "TRUE",
  COVID_2020 = "FALSE",
  COVID_2021 = "FALSE"
))

fit.ch <- lm(NU_NOTA_CH ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
               COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
             data = sample_reg)
summary(fit.ch)

fit.cn <- lm(NU_NOTA_CN ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
               COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
             data = sample_reg)
summary(fit.cn)

fit.lc <- lm(NU_NOTA_LC ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
               COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
             data = sample_reg)
summary(fit.lc)

fit.mt <- lm(NU_NOTA_MT ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
               COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
             data = sample_reg)
summary(fit.mt)


saveRDS(model, file = "model/model1.rda")

model <- readRDS(file = "model/model1.rda")
summary(model)

ols_test_breusch_pagan(model)

