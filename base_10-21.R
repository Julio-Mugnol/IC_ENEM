library(data.table)
library(tidyverse)

setwd("C:/Dev/_INSPER/_IC")
memory.limit(24576)

catalog <- data.table("year" = character(), "group" = character(),
                       "n_enr" = numeric(), "n_att" = numeric())

# 2010 ---------------
# Loading 2010 microdata
ENEM_2010 <- data.table::fread(input="microdados_enem_2010/DADOS/MICRODADOS_ENEM_2010.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2010 <- ENEM_2010 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2010 %>% 
  nrow()
enr_bra_ama <- ENEM_2010 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2010 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()

# Attendant data
attendants <- ENEM_2010 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()

# Catalog 2010
cat_2010 <- data.table("year" = rep("2010", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2010)


# 2011 ---------------
# Loading 2011 microdata
ENEM_2011 <- data.table::fread(input="microdados_enem_2011/DADOS/MICRODADOS_ENEM_2011.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2011 <- ENEM_2011 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2011 %>% 
  nrow()
enr_bra_ama <- ENEM_2011 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2011 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2011 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2011 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2011 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2011
cat_2011 <- data.table("year" = rep("2011", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2011)


# 2012 ---------------
# Loading 2012 microdata
ENEM_2012 <- data.table::fread(input="microdados_enem_2012/DADOS/MICRODADOS_ENEM_2012.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2012 <- ENEM_2012 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2012 %>% 
  nrow()
enr_bra_ama <- ENEM_2012 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2012 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
enr_publ <- ENEM_2012 %>% # Escola publica
  filter(TP_ESCOLA == 1) %>% 
  nrow()
enr_priv <- ENEM_2012 %>% # Escola privada
  filter(TP_ESCOLA == 2) %>% 
  nrow()

# Attendant data
attendants <- ENEM_2012 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
att_publ <- attendants %>% # Escola publica
  filter(TP_ESCOLA == 1) %>% 
  nrow()
att_priv <- attendants %>% # Escola privada
  filter(TP_ESCOLA == 2) %>% 
  nrow()

# Catalog 2012
cat_2012 <- data.table("year" = rep("2012", times=5),
                       "group" = c("todos", "bra_ama", "pre_par_ind", "publ", "priv"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind, enr_publ, enr_priv),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind, att_publ, att_priv))

rm(ENEM_2012)


# 2013 ---------------
# Loading 2013 microdata
ENEM_2013 <- data.table::fread(input="microdados_enem_2013/DADOS/MICRODADOS_ENEM_2013.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2013 <- ENEM_2013 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2013 %>% 
  nrow()
enr_bra_ama <- ENEM_2013 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2013 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2013 %>% # Escola publica
#   filter(TP_ESCOLA == 1) %>% 
#   nrow()
# enr_priv <- ENEM_2013 %>% # Escola privada
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2013 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 1) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()

# Catalog 2013
cat_2013 <- data.table("year" = rep("2013", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2013)


# 2014 ---------------
# Loading 2014 microdata
ENEM_2014 <- data.table::fread(input="microdados_enem_2014/DADOS/MICRODADOS_ENEM_2014.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2014 <- ENEM_2014 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2014 %>% 
  nrow()
enr_bra_ama <- ENEM_2014 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2014 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2014 %>% # Escola publica
#   filter(TP_ESCOLA == 1) %>% 
#   nrow()
# enr_priv <- ENEM_2014 %>% # Escola privada
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2014 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 1) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()

# Catalog 2014
cat_2014 <- data.table("year" = rep("2014", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2014)


# 2015 ---------------
# Loading 2015 microdata
ENEM_2015 <- data.table::fread(input="microdados_enem_2015/DADOS/MICRODADOS_ENEM_2015.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2015 <- ENEM_2015 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2015 %>% 
  nrow()
enr_bra_ama <- ENEM_2015 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2015 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2015 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2015 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2015 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2015
cat_2015 <- data.table("year" = rep("2015", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2015)


# 2016 ---------------
# Loading 2016 microdata
ENEM_2016 <- data.table::fread(input="microdados_enem_2016/DADOS/MICRODADOS_ENEM_2016.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2016 <- ENEM_2016 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2016 %>% 
  nrow()
enr_bra_ama <- ENEM_2016 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2016 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2016 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2016 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2016 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2016
cat_2016 <- data.table("year" = rep("2016", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2016)


# 2017 ---------------
# Loading 2017 microdata
ENEM_2017 <- data.table::fread(input="microdados_enem_2017/DADOS/MICRODADOS_ENEM_2017.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2017 <- ENEM_2017 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2017 %>% 
  nrow()
enr_bra_ama <- ENEM_2017 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2017 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2017 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2017 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2017 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2017
cat_2017 <- data.table("year" = rep("2017", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2017)


# 2018 ---------------
# Loading 2018 microdata
ENEM_2018 <- data.table::fread(input="microdados_enem_2018/DADOS/MICRODADOS_ENEM_2018.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2018 <- ENEM_2018 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2018 %>% 
  nrow()
enr_bra_ama <- ENEM_2018 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2018 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2018 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2018 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2018 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2018
cat_2018 <- data.table("year" = rep("2018", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2018)


# 2019 ---------------
# Loading 2019 microdata
ENEM_2019 <- data.table::fread(input="microdados_enem_2019/DADOS/MICRODADOS_ENEM_2019.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2019 <- ENEM_2019 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2019 %>% 
  nrow()
enr_bra_ama <- ENEM_2019 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2019 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2019 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2019 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2019 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2019
cat_2019 <- data.table("year" = rep("2019", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2019)


# 2020 ---------------
# Loading 2020 microdata
ENEM_2020 <- data.table::fread(input="microdados_enem_2020/DADOS/MICRODADOS_ENEM_2020.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2020 <- ENEM_2020 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2020 %>% 
  nrow()
enr_bra_ama <- ENEM_2020 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2020 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2020 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2020 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2020 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2020
cat_2020 <- data.table("year" = rep("2020", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2020)


# 2021 ---------------
# Loading 2021 microdata
ENEM_2021 <- data.table::fread(input="microdados_enem_2021/DADOS/MICRODADOS_ENEM_2021.csv",
                               integer64="character",
                               skip=0,  #Ler do inicio
                               nrow=-1, #Ler todos os registros
                               na.strings = "", 
                               showProgress = TRUE)
# Selecting required variables
ENEM_2021 <- ENEM_2021 %>% 
  select(NU_INSCRICAO, NU_ANO, TP_COR_RACA, TP_ESCOLA,
         TP_PRESENCA_CN, TP_PRESENCA_CH, TP_PRESENCA_LC, TP_PRESENCA_MT)

# Enrollment data
enrolled <- ENEM_2021 %>% 
  nrow()
enr_bra_ama <- ENEM_2021 %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
enr_pre_par_ind <- ENEM_2021 %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# enr_publ <- ENEM_2021 %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# enr_priv <- ENEM_2021 %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Attendant data
attendants <- ENEM_2021 %>% 
  filter(TP_PRESENCA_CN & TP_PRESENCA_CH & TP_PRESENCA_LC & TP_PRESENCA_MT)
attendant <- attendants %>% 
  nrow()
att_bra_ama <- attendants %>% # Brancos e amarelos
  filter(TP_COR_RACA == 1 | TP_COR_RACA == 4) %>% 
  nrow()
att_pre_par_ind <- attendants %>% # Pretos, pardos e indigenas
  filter(TP_COR_RACA == 2 | TP_COR_RACA == 3 | TP_COR_RACA == 5) %>% 
  nrow()
# att_publ <- attendants %>% # Escola publica
#   filter(TP_ESCOLA == 2) %>% 
#   nrow()
# att_priv <- attendants %>% # Escola privada
#   filter(TP_ESCOLA == 3) %>% 
#   nrow()

# Catalog 2021
cat_2021 <- data.table("year" = rep("2021", times=3),
                       "group" = c("todos", "bra_ama", "pre_par_ind"),
                       "n_enr" = c(enrolled, enr_bra_ama, enr_pre_par_ind),
                       "n_att" = c(attendant, att_bra_ama, att_pre_par_ind))

rm(ENEM_2021)

# Catalog ------------
# Final catalog
catalog <- rbindlist(list(catalog, cat_2010, cat_2011, cat_2012, cat_2013))
catalog <- rbindlist(list(catalog, cat_2014, cat_2015, cat_2016, cat_2017))
catalog <- rbindlist(list(catalog, cat_2018, cat_2019, cat_2020, cat_2021))

fwrite(catalog, "catalog.csv")

catalog <- data.table::fread(input="catalog.csv")
