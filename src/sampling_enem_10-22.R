library(tidyverse)

# 2010 ---------------
# Loading 2010 microdata
ENEM_2010 <- data.table::fread(input="data/microdados_enem_2010/DADOS/MICRODADOS_ENEM_2010.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2010 <- ENEM_2010 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2010)

# 2011 ---------------
# Loading 2011 microdata
ENEM_2011 <- data.table::fread(input="data/microdados_enem_2011/DADOS/MICRODADOS_ENEM_2011.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2011 <- ENEM_2011 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2011)

# 2012 ---------------
# Loading 2012 microdata
ENEM_2012 <- data.table::fread(input="data/microdados_enem_2012/DADOS/MICRODADOS_ENEM_2012.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2012 <- ENEM_2012 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2012)

# 2013 ---------------
# Loading 2013 microdata
ENEM_2013 <- data.table::fread(input="data/microdados_enem_2013/DADOS/MICRODADOS_ENEM_2013.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2013 <- ENEM_2013 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2013)

# 2014 ---------------
# Loading 2014 microdata
ENEM_2014 <- data.table::fread(input="data/microdados_enem_2014/DADOS/MICRODADOS_ENEM_2014.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2014 <- ENEM_2014 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2014)

# 2015 ---------------
# Loading 2015 microdata
ENEM_2015 <- data.table::fread(input="data/microdados_enem_2015/DADOS/MICRODADOS_ENEM_2015.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2015 <- ENEM_2015 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2015)

# 2016 ---------------
# Loading 2016 microdata
ENEM_2016 <- data.table::fread(input="data/microdados_enem_2016/DADOS/MICRODADOS_ENEM_2016.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2016 <- ENEM_2016 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2016)

# 2017 ---------------
# Loading 2017 microdata
ENEM_2017 <- data.table::fread(input="data/microdados_enem_2017/DADOS/MICRODADOS_ENEM_2017.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2017 <- ENEM_2017 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2017)

# 2018 ---------------
# Loading 2018 microdata
ENEM_2018 <- data.table::fread(input="data/microdados_enem_2018/DADOS/MICRODADOS_ENEM_2018.csv",
                               integer64='character',
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "",
                               encoding = 'Latin-1',
                               showProgress = TRUE)

# Selecting required variables and building the catalog
cat_2018 <- ENEM_2018 %>% 
  select(NU_INSCRICAO, ano = NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(raca = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         raca = fct_collapse(raca,
                             "bra_ama" = c("1", "4"),
                             "pre_par_ind" = c("2", "3", "5"),
                             "nd" = c("0", "6")),
         regiao = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         regiao = fct_collapse(regiao,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         participante = TP_PRESENCA_CN != 0 | TP_PRESENCA_CH != 0 | TP_PRESENCA_LC != 0 | TP_PRESENCA_MT != 0,
         presente = TP_PRESENCA_CN == 1 & TP_PRESENCA_CH == 1 & TP_PRESENCA_LC == 1 & TP_PRESENCA_MT == 1) %>%  
  group_by(ano, participante, presente, regiao, raca) %>%
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2018)

# 2019 ---------------
# Loading 2019 microdata
ENEM_2019 <- data.table::fread(input="data/microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# Selecting required variables
ENEM_2019 <- ENEM_2019 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(TP_COR_RACA = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "bra_ama" = c("1", "4"),
                                    "pre_par_ind" = c("2", "3", "5"),
                                    "nd" = c("0", "6")),
         REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         REGIAO = fct_collapse(REGIAO,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         STATUS = TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)

# Catalog 2019
cat_2019 <- ENEM_2019 %>%  
  group_by(NU_ANO, STATUS, REGIAO, TP_COR_RACA) %>% 
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2019)


# 2020 ---------------
# Loading 2020 microdata
ENEM_2020 <- data.table::fread(input="data/microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# Selecting required variables
ENEM_2020 <- ENEM_2020 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(TP_COR_RACA = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "bra_ama" = c("1", "4"),
                                    "pre_par_ind" = c("2", "3", "5"),
                                    "nd" = c("0", "6")),
         REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         REGIAO = fct_collapse(REGIAO,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         STATUS = TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)

# Catalog 2020
cat_2020 <- ENEM_2020 %>%  
  group_by(NU_ANO, STATUS, REGIAO, TP_COR_RACA) %>% 
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2020)


# 2021 ---------------
# Loading 2021 microdata
ENEM_2021 <- data.table::fread(input="data/microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)

# Selecting required variables
ENEM_2021 <- ENEM_2021 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, CO_MUNICIPIO_PROVA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT) %>% 
  mutate(TP_COR_RACA = fct_relevel(as_factor(TP_COR_RACA), c(as.character(1:5))),
         TP_COR_RACA = fct_collapse(TP_COR_RACA,
                                    "bra_ama" = c("1", "4"),
                                    "pre_par_ind" = c("2", "3", "5"),
                                    "nd" = c("0", "6")),
         REGIAO = substr(as.character(CO_MUNICIPIO_PROVA), 1, 1),
         REGIAO = fct_collapse(REGIAO,
                               "N" = "1",
                               "NE" = "2",
                               "SE" = "3",
                               "S" = "4",
                               "CO" = "5"),
         STATUS = TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)

# Catalog 2021
cat_2021 <- ENEM_2021 %>%  
  group_by(NU_ANO, STATUS, REGIAO, TP_COR_RACA) %>% 
  replace(is.na(.), FALSE) %>% 
  summarise(N = n())

rm(ENEM_2021)

# Catalog ------------

catalog <- cat_2010 %>% 
  bind_rows(cat_2011, cat_2012, cat_2013, cat_2014, cat_2015, cat_2016,
            cat_2017, cat_2018, cat_2019, cat_2020, cat_2021)

save(catalog, file = "sample/catalog.RData")
