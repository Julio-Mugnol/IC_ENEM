library(tidyverse)
library(fixest)
library(estimatr)
library(lmtest)
library(sandwich)

load("sample/sample.RData")
sample <- sample %>% 
  filter(presente & raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd") %>% 
  select(-muni_escola) %>% 
  mutate(ano_18 = ano == 2018,
         ano_19 = ano == 2019,
         ano_20 = ano == 2020,
         ano_21 = ano == 2021,
         ano_22 = ano == 2022,
         estado = substr(as.character(muni_prova), 1, 2),
         across(c(ano, ano_18, ano_19, ano_20, ano_21, ano_22, estado), as_factor)) %>% 
  drop_na(.) %>% 
  droplevels(.)

fit <- lm(nota ~ raca + sexo + educ_mae + regiao + escola + internet +
            ano * (raca + sexo + educ_mae + regiao + escola + internet), 
          data = sample)
summary(fit)
saveRDS(fit, file = "models/fit_nota.rda")

fit_fe <- feols(nota ~ raca + sexo + educ_mae + escola + internet +
                  ano * (raca + sexo + educ_mae + escola + internet) | muni_prova,
                data = sample)
summary(fit_fe)
saveRDS(fit_fe, file = "models/fit_nota_fe.rda")


load("sample/sample.RData")
sample <- sample %>% 
  filter(raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd") %>% 
  mutate(ano_18 = ano == 2018,
         ano_19 = ano == 2019,
         ano_20 = ano == 2020,
         ano_21 = ano == 2021,
         ano_22 = ano == 2022,
         across(c(ano, ano_18, ano_19, ano_20, ano_21, ano_22), as_factor)) %>% 
  droplevels(.)

# fit_p_logit <- glm(as.integer(presente) ~ raca + sexo + educ_mae + regiao + escola + internet +
#                      ano * (raca + sexo + educ_mae + regiao + escola + internet), 
#                    family = binomial(link='logit'),
#                    data = sample)
# summary(fit_p_logit)

fit_p_mpl <- lm(as.integer(presente) ~ raca + sexo + educ_mae + regiao + escola + internet +
                  ano * (raca + sexo + educ_mae + regiao + escola + internet),
                data = sample)
summary(fit_p_mpl)
lpm_vv <- vcovHC(fit_p_mpl, type="HC1")
coeftest(fit_p_mpl, vcov = lpm_vv)

fit_p_mpl_fe <- feols(as.integer(presente) ~ raca + sexo + educ_mae + escola + internet +
                     ano * (raca + sexo + educ_mae + escola + internet) | muni_prova,
                   data = sample)
summary(fit_p_mpl_fe)












