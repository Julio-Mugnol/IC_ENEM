library(tidyverse)

memory.limit(24576)

# 2018 ---------------
# Loading 2018 microdata
ENEM_2018 <- data.table::fread(input='data/microdados_enem_2018/DADOS/MICRODADOS_ENEM_2018.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2018 <- ENEM_2018[TP_ST_CONCLUSAO == 2,
                         .(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
                           TP_ESCOLA, TP_DEPENDENCIA_ADM_ESC, CO_MUNICIPIO_ESC, TP_SEXO, Q024,
                           NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,
                           TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT)]
rm(ENEM_2018)

# 2019 ---------------
# Loading 2019 microdata
ENEM_2019 <- data.table::fread(input='data/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2019 <- ENEM_2019[TP_ST_CONCLUSAO == 2,
                         .(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO, 
                           TP_ESCOLA, TP_DEPENDENCIA_ADM_ESC, CO_MUNICIPIO_ESC, TP_SEXO, Q024,
                           NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,
                           TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT)]
rm(ENEM_2019)

# 2020 ---------------
# Loading 2020 microdata
ENEM_2020 <- data.table::fread(input='data/microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2020 <- ENEM_2020[TP_ST_CONCLUSAO == 2,
                         .(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
                           TP_ESCOLA, TP_DEPENDENCIA_ADM_ESC, CO_MUNICIPIO_ESC, TP_SEXO, Q024,
                           NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,
                           TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT)]
rm(ENEM_2020)

# 2021 ---------------
# Loading 2021 microdata
ENEM_2021 <- data.table::fread(input='data/microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2021 <- ENEM_2021[TP_ST_CONCLUSAO == 2,
                         .(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
                           TP_ESCOLA, TP_DEPENDENCIA_ADM_ESC, CO_MUNICIPIO_ESC, TP_SEXO, Q024,
                           NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,
                           TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT)]
rm(ENEM_2021)

# 2022 ---------------
# Loading 2022 microdata
ENEM_2022 <- data.table::fread(input='data/microdados_enem_2022/DADOS/MICRODADOS_ENEM_2022.csv',
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Sampling
sample_2022 <- ENEM_2022[TP_ST_CONCLUSAO == 2,
                         .(NU_INSCRICAO, TP_COR_RACA, Q002, CO_MUNICIPIO_PROVA, Q025, NU_ANO,
                           TP_ESCOLA, TP_DEPENDENCIA_ADM_ESC, CO_MUNICIPIO_ESC, TP_SEXO, Q024,
                           NU_NOTA_CH, NU_NOTA_CN, NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO,
                           TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT)]
rm(ENEM_2022)


# Full ---------------

sample_u <- sample_2018 %>%
  bind_rows(sample_2019, sample_2020, sample_2021, sample_2022)

save(sample_u, file = "sample/sample_u.RData")

load("sample/sample_u.RData")

sample_u %>%  
  mutate(across(c(TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT), ~replace_na(., 0))) %>% 
  group_by(TP_PRESENCA_CH, TP_PRESENCA_CN, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  count()

sample <- sample_u %>% 
  as_tibble() %>% 
  rename(ano = NU_ANO, raca = TP_COR_RACA, escola = TP_ESCOLA, dep_adm = TP_DEPENDENCIA_ADM_ESC,
         educ_mae = Q002, internet = Q025, computador = Q024, sexo = TP_SEXO, 
         muni_prova = CO_MUNICIPIO_PROVA, muni_escola = CO_MUNICIPIO_ESC,
         nota_ch = NU_NOTA_CH, nota_cn = NU_NOTA_CN, nota_lc = NU_NOTA_LC, nota_mt = NU_NOTA_MT, 
         nota_re = NU_NOTA_REDACAO, presenca_ch = TP_PRESENCA_CH, presenca_cn = TP_PRESENCA_CN, 
         presenca_lc = TP_PRESENCA_LC, presenca_mt = TP_PRESENCA_MT) %>% 
  mutate(regiao = substr(as.character(muni_prova), 1, 1),
         educ_mae = replace_na(educ_mae, "H"),
         dep_adm = replace_na(dep_adm, 0),
         internet = replace_na(internet, "nd"),
         computador = replace_na(computador, "nd"),
         across(c(regiao, escola, dep_adm, raca, sexo, muni_prova, muni_escola), as_factor),
         across(c(presenca_ch, presenca_cn, presenca_lc, presenca_mt), ~replace_na(., 0)),
         regiao = fct_collapse(fct_relevel(regiao, c(as.character(1:5))),
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         escola = fct_collapse(escola,
                               "publ" = "2",
                               "priv" = c("3", "4"),
                               "nd" = "1"),
         dep_adm = fct_collapse(dep_adm,
                                "fed" = "1",
                                "est" = "2",
                                "mun" = "3",
                                "pri" = "4",
                                "nd" = "0"),
         raca = fct_collapse(raca,
                             "bra" = c("1", "4"),
                             "pre" = c("2", "3", "5"),
                             "nd" = "0"),
         nota = (nota_ch + nota_cn + nota_lc + nota_mt + nota_re) / 5,
         # participante = !is.na(nota),
         participante = presenca_ch != 0 | presenca_cn != 0 | presenca_lc != 0 | presenca_mt != 0,
         presente = presenca_ch == 1 & presenca_cn == 1 & presenca_lc == 1 & presenca_mt == 1,
         educ_mae = fct_collapse(fct_relevel(educ_mae, c(LETTERS[1:7])),
                                 "fund_inc" = c("A", "B", "C"),
                                 "fund" = "D",
                                 "med" = "E",
                                 "sup" = c("F", "G"),
                                 "nd" = "H"),
         internet = fct_collapse(fct_relevel(internet, c(LETTERS[1:2])),
                                 "S" = "B",
                                 "N" = "A"),
         computador = fct_collapse(computador,
                                   "S" = "A",
                                   "N" = c("B", "C", "D", "E"),
                                   "nd" = "nd")) %>% 
  dplyr::select(ano, raca, escola, dep_adm, educ_mae, internet, regiao, muni_prova, muni_escola,
                participante, presente, sexo, computador, nota, nota_ch, nota_cn, nota_lc, nota_mt, nota_re)

save(sample, file = "sample/sample.RData")
