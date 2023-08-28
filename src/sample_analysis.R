library(tidyverse)
library(MASS)
library(olsrr)
library(broom)
library(modelsummary)
library(stargazer)
library(ROCit)
library(plm)
library(estimatr)
library(fixest)
library(margins)

load("sample/sample.RData")
sample <- sample %>% 
  filter(participante & raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd") %>% 
  select(-muni_escola) %>% 
  mutate(ano_18 = ano == 2018,
         ano_19 = ano == 2019,
         ano_20 = ano == 2020,
         ano_21 = ano == 2021,
         ano_22 = ano == 2022,
         estado = substr(as.character(muni_prova), 1, 2),
         across(c(ano_18, ano_19, ano_20, ano_21, ano_22, estado), as_factor)) %>% 
  replace(is.na(.), 0) %>% 
  droplevels(.)

fit <- lm(nota ~ raca + sexo + educ_mae + regiao + escola + internet + 
            ano_19 + ano_20 + ano_21 + ano_22 +
            ano_20 * (raca + sexo + educ_mae + regiao + escola + internet) +
            ano_21 * (raca + sexo + educ_mae + regiao + escola + internet) +
            ano_22 * (raca + sexo + educ_mae + regiao + escola + internet), 
          data = sample)
summary(fit)

fit_fe <- lm(nota ~ raca + sexo + educ_mae + escola + internet + 
               ano_19 + ano_20 + ano_21 + ano_22 + muni_prova +
               ano_20 * (raca + sexo + educ_mae + escola + internet) +
               ano_21 * (raca + sexo + educ_mae + escola + internet) +
               ano_22 * (raca + sexo + educ_mae + escola + internet),
             data = sample)
summary(fit_fe)
fit_fe <- feols(nota ~ raca + sexo + educ_mae + escola + internet + ano_19 +
                  ano_20 * (raca + sexo + educ_mae + escola + internet) +
                  ano_21 * (raca + sexo + educ_mae + escola + internet) +
                  ano_22 * (raca + sexo + educ_mae + escola + internet) | muni_prova,
                data = sample)
summary(fit_fe)

sample %>% 
  sample_n(500000) %>% 
  lm(nota ~ raca + sexo + educ_mae + escola + internet + 
       ano_19 + ano_20 + ano_21 + ano_22 + muni_prova +
       ano_20 * (raca + sexo + educ_mae + escola + internet) +
       ano_21 * (raca + sexo + educ_mae + escola + internet) +
       ano_22 * (raca + sexo + educ_mae + escola + internet),
     data = .)

sample %>% 
  select(nota, ano, raca) %>% 
  pivot_wider(names_from = ano, values_from = raca) %>% 
  View()

# fit2 <- lm(nota ~ raca + sexo + educ_mae + regiao + escola + internet + 
#              ano_19 + ano_20 + ano_21 + ano_22 +
#              ano_19 * (raca + sexo + educ_mae + regiao + escola + internet) +
#              ano_20 * (raca + sexo + educ_mae + regiao + escola + internet) +
#              ano_21 * (raca + sexo + educ_mae + regiao + escola + internet) +
#              ano_22 * (raca + sexo + educ_mae + regiao + escola + internet), 
#            data = sample)

summary(fit)
modelsummary(list("Nota Média" = fit),
             stars = c("*" = .1, "**" = .05, "***" = .01),
             gof_map = c("nobs", "r.squared"))


# fit.ch <- lm(nota_ch ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
#                ano_20 * (raca + educ_mae + regiao + escola + internet) +
#                ano_21 * (raca + educ_mae + regiao + escola + internet), 
#              data = sample)
# summary(fit.ch)
# 
# fit.cn <- lm(nota_cn ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
#                ano_20 * (raca + educ_mae + regiao + escola + internet) +
#                ano_21 * (raca + educ_mae + regiao + escola + internet), 
#              data = sample)
# summary(fit.cn)
# 
# fit.lc <- lm(nota_lc ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
#                ano_20 * (raca + educ_mae + regiao + escola + internet) +
#                ano_21 * (raca + educ_mae + regiao + escola + internet), 
#              data = sample)
# summary(fit.lc)
# 
# fit.mt <- lm(nota_mt ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
#                ano_20 * (raca + educ_mae + regiao + escola + internet) +
#                ano_21 * (raca + educ_mae + regiao + escola + internet), 
#              data = sample)
# summary(fit.mt)
# 
# fit.re <- lm(nota_re ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
#                ano_20 * (raca + educ_mae + regiao + escola + internet) +
#                ano_21 * (raca + educ_mae + regiao + escola + internet), 
#              data = sample)
# summary(fit.re)

# modelsummary(list(
#     "Média Geral"          = fit,
#     "Ciências Humanas"     = fit.ch,
#     "Ciências da Natureza" = fit.cn,
#     "Linguagens e Códigos" = fit.lc,
#     "Matemática"           = fit.mt,
#     "Redação"              = fit.re
#   ), statistic = c("({std.error})", 
#                  "p = {p.value}"))
# modelsummary(list(
#   "Média Geral"          = fit,
#   "Ciências Humanas"     = fit.ch,
#   "Ciências da Natureza" = fit.cn,
#   "Linguagens e Códigos" = fit.lc,
#   "Matemática"           = fit.mt,
#   "Redação"              = fit.re
# ), stars = c("*" = .1, "**" = .05, "***" = .01))

sample %>% 
  ggplot(aes(x = factor(ano), y = nota)) +
    geom_boxplot()

sample %>% 
  filter(nota == 0) %>% 
  View()



fit_re.n <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 + ano_22 +
                 ano_20 * (raca + educ_mae + escola + internet) +
                 ano_21 * (raca + educ_mae + escola + internet) +
                 ano_22 * (raca + educ_mae + escola + internet), 
               data = sample %>% filter(regiao == "N"))
summary(fit_re.n)

fit_re.ne <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 + ano_22 +
                  ano_20 * (raca + educ_mae + escola + internet) +
                  ano_21 * (raca + educ_mae + escola + internet) +
                  ano_22 * (raca + educ_mae + escola + internet), 
                data = sample %>% filter(regiao == "NE"))
summary(fit_re.ne)

fit_re.co <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 + ano_22 +
                  ano_20 * (raca + educ_mae + escola + internet) +
                  ano_21 * (raca + educ_mae + escola + internet) +
                  ano_22 * (raca + educ_mae + escola + internet), 
                data = sample %>% filter(regiao == "CO"))
summary(fit_re.co)

fit_re.se <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 + ano_22 +
                  ano_20 * (raca + educ_mae + escola + internet) +
                  ano_21 * (raca + educ_mae + escola + internet) +
                  ano_22 * (raca + educ_mae + escola + internet), 
                data = sample %>% filter(regiao == "SE"))
summary(fit_re.se)

fit_re.s <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 + ano_22 +
                 ano_20 * (raca + educ_mae + escola + internet) +
                 ano_21 * (raca + educ_mae + escola + internet) +
                 ano_22 * (raca + educ_mae + escola + internet), 
               data = sample %>% filter(regiao == "S"))
summary(fit_re.s)

modelsummary(list(
  "Norte"        = fit_re.n, 
  "Nordeste"     = fit_re.ne, 
  "Centro-Oeste" = fit_re.co, 
  "Sudeste"      = fit_re.se, 
  "Sul"          = fit_re.s
  ), stars = c("*" = .1, "**" = .05, "***" = .01))


load("sample/sample.RData")
sample <- sample %>% 
  filter(raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd") %>% 
  mutate(ano_18 = ano == 2018,
         ano_19 = ano == 2019,
         ano_20 = ano == 2020,
         ano_21 = ano == 2021,
         ano_22 = ano == 2022,
         across(c(ano_18, ano_19, ano_20, ano_21, ano_22), as_factor)) %>% 
  droplevels(.)

# fit_p <- glm(as.integer(participante) ~ raca + sexo + educ_mae + regiao + escola + internet +
#                ano_19 + ano_20 + ano_21 + ano_22 +
#                ano_20 * (raca + sexo + educ_mae + regiao + escola + internet) +
#                ano_21 * (raca + sexo + educ_mae + regiao + escola + internet) +
#                ano_22 * (raca + sexo + educ_mae + regiao + escola + internet), 
#              family = binomial(link='logit'),
#              data = sample)
fit_p <- glm(as.integer(presente) ~ raca + sexo + educ_mae + regiao + escola + internet +
               ano_19 + ano_20 + ano_21 + ano_22 +
               ano_20 * (raca + sexo + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + sexo + educ_mae + regiao + escola + internet) +
               ano_22 * (raca + sexo + educ_mae + regiao + escola + internet), 
             family = binomial(link='logit'),
             data = sample)

summary(fit_p)
modelsummary(fit_p)
summary(margins(fit_p))

fit_p2 <- lm(as.integer(presente) ~ raca + sexo + educ_mae + regiao + escola + internet +
               ano_19 + ano_20 + ano_21 + ano_22 +
               ano_20 * (raca + sexo + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + sexo + educ_mae + regiao + escola + internet) +
               ano_22 * (raca + sexo + educ_mae + regiao + escola + internet),
             data = sample)

modelsummary(list("logit" = fit_p, "ols" = fit_p2),
             stars = c("*" = .1, "**" = .05, "***" = .01),
             gof_map = c("nobs", "r.squared"))

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

