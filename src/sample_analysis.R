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

fit_fe <- feols(nota ~ raca + sexo + educ_mae + escola + internet +
                  ano * (raca + sexo + educ_mae + escola + internet) | muni_prova,
                data = sample)
summary(fit_fe)



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










# roc <- rocit(score = fit_p2$fitted.values, class = fit_p2$y)
# plot(roc)

confint(fit, c("racapre", "racapre:ano_20TRUE", "racapre:ano_21TRUE", "racapre:ano_22TRUE")) %>% 
  as_tibble() %>% 
  rename("min" = "2.5 %", "max" = "97.5 %") %>% 
  left_join()

tibble(
  "racapre" = coefficients(fit)["racapre"]
)


modelplot(fit, coef_map = cm) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  ylim(NA, .5)

modelplot(fit, 
          coef_map = c("racapre:ano_20TRUE" = 2020,
                       "racapre:ano_21TRUE" = 2021,
                       "racapre:ano_22TRUE" = 2022),
          draw = FALSE) %>% 
  ggplot(aes(x = term)) +
  geom_point(aes(y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_hline(yintercept = 0, linetype = "dashed")

modelplot(fit, coef_map = cm, draw = FALSE) %>% 
  mutate(estimate = estimate + fit$coefficients["racapre"])

modelplot(fit)

