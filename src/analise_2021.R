library(data.table)
library(tidyverse)

setwd("C:/Dev/_INSPER/_IC")
memory.limit(24576)

ENEM_2021 <- data.table::fread(input='microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

ENEM_2021 <- ENEM_2021 %>% #Remover variáveis não utilizadas
  select(-CO_PROVA_CH, -CO_PROVA_CN, -CO_PROVA_LC, -CO_PROVA_MT) %>% #Código das provas aplicadas
  select(-TX_RESPOSTAS_CH, -TX_RESPOSTAS_CN, -TX_RESPOSTAS_LC, -TX_RESPOSTAS_MT) %>% #Respostas do aluno em cada prova
  select(-TX_GABARITO_CH, -TX_GABARITO_CN, -TX_GABARITO_LC, -TX_GABARITO_MT) #Gabarito das provas

ENEM_2021 <- ENEM_2021 %>% 
  mutate(NU_NOTA = (NU_NOTA_CN + NU_NOTA_CH + NU_NOTA_LC + NU_NOTA_MT) / 4)

base_reg <- ENEM_2021 %>% 
  select(TP_COR_RACA, NU_NOTA) %>% 
  filter(TP_COR_RACA %in% c(1,2,3,4,5)) %>% 
  filter(!is.na(NU_NOTA)) %>% 
  mutate(TP_COR_RACA = case_when(
    TP_COR_RACA == 1 ~ "Branca",
    TP_COR_RACA == 2 ~ "Preta",
    TP_COR_RACA == 3 ~ "Parda",
    TP_COR_RACA == 4 ~ "Amarela",
    TP_COR_RACA == 5 ~ "Indígena"
  )) %>% 
  sample_n(10000)

reg_raca <- lm(NU_NOTA ~ TP_COR_RACA, data = base_reg)
