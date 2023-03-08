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


sample <- sample %>% 
  as_tibble() %>% 
  mutate(across(c(TP_COR_RACA, TP_ESCOLA, Q002, Q025, NU_ANO), as_factor)) %>% 
  mutate(TP_COR_RACA = fct_relevel(TP_COR_RACA, c(as.character(1:5))),
         TP_ESCOLA = fct_relevel(TP_ESCOLA, c(as.character(2:4))),
         Q002 = fct_relevel(Q002, c(LETTERS[1:7])),
         Q025 = fct_relevel(Q025, c(LETTERS[1:2]))) %>% 
  mutate(TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "Branca/Amarela" = c("1", "4"),
                                    "Preta/Parda/Indígena" = c("2", "3", "5")),
         TP_ESCOLA = fct_collapse(TP_ESCOLA,
                                  "Pública" = "2",
                                  "Privada" = c("3", "4")),
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
         NU_ANO = fct_relevel(as.factor(NU_ANO), as.character(2018:2021)),
         NU_NOTA = (NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_MT + NU_NOTA_REDACAO) / 5) %>%
  mutate(RACA = TP_COR_RACA, EDUC_MAE = Q002, INTERNET = Q025, ESCOLA = TP_ESCOLA) %>% 
  dplyr::select(NU_INSCRICAO, RACA, EDUC_MAE, INTERNET, REGIAO, ESCOLA, NU_ANO, COVID,
                NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO, NU_NOTA)

fit <- lm(NU_NOTA ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
            COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
            data = sample_reg)
summary(fit)

fit_ch <- lm(NU_NOTA_CH ~ RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET + NU_ANO +
               COVID : (RACA + EDUC_MAE + REGIAO + ESCOLA + INTERNET), 
             data = sample_reg); summary(fit_ch)

saveRDS(model, file = "model/model1.rda")
saveRDS(robust, file = "model/robust1.rda")

model <- readRDS(file = "model/model1.rda")
summary(model)

robust <- readRDS(file = "model/robust1.rda")
summary(robust)

ols_test_breusch_pagan(model)


sample_reg %>% group_by(NU_ANO, ESCOLA) %>% summarise(mean = mean(NU_NOTA_OBJETIVA))

