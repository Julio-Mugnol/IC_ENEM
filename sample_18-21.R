library(data.table)
library(tidyverse)

setwd("C:/Dev/_INSPER/_IC")
memory.limit(24576)

n <- 1e+05 # Sample size

# 2018 ---------------
# Loading 2018 microdata
ENEM_2018 <- data.table::fread(input='microdados_enem_2018/DADOS/MICRODADOS_ENEM_2018.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2018 <- ENEM_2018 %>% 
  filter(Q002 != "H" & TP_COR_RACA != 0
         & !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & !is.na(NU_NOTA_LC) & !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_REDACAO)
         & TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT) %>% 
  select(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
         NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO) %>% 
  slice_sample(n = n)

rm(ENEM_2018)

# 2019 ---------------
# Loading 2019 microdata
ENEM_2019 <- data.table::fread(input='microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2019 <- ENEM_2019 %>% 
  filter(Q002 != "H" & TP_COR_RACA != 0
         & !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & !is.na(NU_NOTA_LC) & !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_REDACAO)
         & TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT) %>% 
  select(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
         NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO) %>% 
  slice_sample(n = n)

rm(ENEM_2019)

# 2020 ---------------
# Loading 2020 microdata
ENEM_2020 <- data.table::fread(input='microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2020 <- ENEM_2020 %>% 
  filter(Q002 != "H" & TP_COR_RACA != 0
         & !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & !is.na(NU_NOTA_LC) & !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_REDACAO)
         & TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT) %>% 
  select(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
         NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO) %>% 
  slice_sample(n = n)

rm(ENEM_2020)

# 2021 ---------------
# Loading 2021 microdata
ENEM_2021 <- data.table::fread(input='microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2021 <- ENEM_2021 %>%
  filter(Q002 != "H" & TP_COR_RACA != 0 & TP_COR_RACA != 6
         & !is.na(NU_NOTA_CH) & !is.na(NU_NOTA_CN) & !is.na(NU_NOTA_LC) & !is.na(NU_NOTA_MT) & !is.na(NU_NOTA_REDACAO)
         & TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT) %>% 
  select(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
         NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO) %>% 
  slice_sample(n = n)

rm(ENEM_2021)


# Full ---------------

sample <- rbindlist(list(sample_2018, sample_2019, sample_2020, sample_2021), fill = TRUE)
data.table::fwrite(sample, file="sample/sample1.csv")

sample <- data.table::fread(input='sample/sample1.csv',
                            integer64='character',
                            skip=0,  #Ler do inicio
                            nrow=-1, #Ler todos os registros
                            na.strings = "",
                            encoding = 'Latin-1',
                            showProgress = TRUE)
