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

# Remover vari?veis n?o utilizadas
ENEM_2018 <- ENEM_2018 %>% 
  select(-CO_PROVA_CH, -CO_PROVA_CN, -CO_PROVA_LC, -CO_PROVA_MT) %>% #C?digo das provas aplicadas
  select(-TX_RESPOSTAS_CH, -TX_RESPOSTAS_CN, -TX_RESPOSTAS_LC, -TX_RESPOSTAS_MT) %>% #Respostas do aluno em cada prova
  select(-TX_GABARITO_CH, -TX_GABARITO_CN, -TX_GABARITO_LC, -TX_GABARITO_MT) #Gabarito das provas

# Sampling
sample_2018 <- ENEM_2018 %>% sample_n(n)
rm(ENEM_2018)

# 2019 ---------------
# Loading 2019 microdata
ENEM_2019 <- data.table::fread(input='microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# Sampling
sample_2019 <- ENEM_2019 %>% sample_n(n)
rm(ENEM_2019)

# 2020 ---------------
# Loading 2020 microdata
ENEM_2020 <- data.table::fread(input='microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# Sampling
sample_2020 <- ENEM_2020 %>% sample_n(n)
rm(ENEM_2020)

# 2021 ---------------
# Loading 2021 microdata
ENEM_2021 <- data.table::fread(input='microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# Sampling
sample_2021 <- ENEM_2021 %>% sample_n(n)
rm(ENEM_2021)


# --------------------
# Full sample

sample <- rbindlist(list(sample_2018, sample_2019, sample_2020, sample_2021), fill = TRUE)
data.table::fwrite(sample, file="sample/sample1.csv")

sample <- data.table::fread(input='sample/sample1.csv',
                            integer64='character',
                            skip=0,  #Ler do inicio
                            nrow=-1, #Ler todos os registros
                            na.strings = "", 
                            showProgress = TRUE)

sample <- sample %>% 
  as_tibble() %>% 
  mutate(across(c(TP_COR_RACA, TP_ESCOLA), as_factor))

sample <- sample %>%
  mutate(TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "Não declarado" = "0",
                                    "Branca/Amarela" = c("1", "4"),
                                    "Preta/Parda/Indígena" = c("2", "3", "5"),
                                    "Não dispõe da informação" = "6")) %>% 
  mutate(TP_ESCOLA = fct_collapse(TP_ESCOLA,
                                  "Não respondeu" = "1",
                                  "Pública" = "2",
                                  "Privada" = c("3", "4")))
  
sample <- sample %>% 
  mutate(CO_REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1)) %>% 
  mutate(across(CO_REGIAO, as_factor)) %>% 
  mutate(CO_REGIAO = fct_collapse(CO_REGIAO,
                                  "Norte" = "1",
                                  "Nordeste" = "2",
                                  "Sudeste" = "3",
                                  "Sul" = "4",
                                  "Centro-Oeste" = "5"))
sample <- sample %>% 
  mutate(across(Q025, as_factor)) %>% 
  mutate(Q025 = fct_collapse(Q025,
                             "Sim" = "B",
                             "Não" = "A"))

sample <- sample %>% 
  mutate(across(Q002, as_factor)) %>%
  mutate(Q002 = fct_collapse(Q002,
                             "Fundamental incompleto" = c("A", "B", "C"),
                             "Fundamental completo" = "D",
                             "Médio completo" = "E",
                             "Superior completo" = c("F", "G"),
                             "Não sei" = "H"))
sample <- sample %>% 
  mutate(across(NU_ANO, as_factor))

sample <- sample %>% 
  mutate(COVID = as_factor(ifelse(
    fct_match(NU_ANO, c("2020", "2021")), "Sim", "Não")))

sample <- sample %>% 
  mutate(NU_NOTA_OBJETIVA = (NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_MT) / 4)

sample %>% 
  summarise(internet = fct_count(Q025))
sample %>% 
  summarise(regiao = fct_count(CO_REGIAO))
sample_reg %>% 
  summarise(escolaridade = fct_count(Q002)) 
sample %>% 
  summarise(raca = fct_count(TP_COR_RACA))
sample %>% 
  summarise(covid = fct_count(COVID))

sample_reg <- sample %>% 
  filter(!is.na(Q025) & !is.na(Q002) & TP_COR_RACA != "Não declarado"
         & !is.na(NU_NOTA_OBJETIVA) & !is.na(NU_NOTA_REDACAO) & Q002 != "Não sei") %>% 
  select(TP_COR_RACA, Q002, CO_REGIAO, Q025, COVID, NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_OBJETIVA, NU_NOTA_REDACAO)

model <- lm(NU_NOTA_OBJETIVA ~ TP_COR_RACA + Q002 + CO_REGIAO + Q025 + COVID + 
              COVID * (TP_COR_RACA + Q002 + CO_REGIAO + Q025), 
            data = sample_reg); summary(model)
