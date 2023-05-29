library(tidyverse)
library(MASS)
library(olsrr)
library(broom)
library(modelsummary)
library(ROCit)

load("sample/sample.RData")

sample <- sample %>% 
  filter(participante & raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd") %>% 
  mutate(ano_18 = ano == 2018,
         ano_19 = ano == 2019,
         ano_20 = ano == 2020,
         ano_21 = ano == 2021,
         across(c(ano_18, ano_19, ano_20, ano_21), as_factor))

fit <- lm(nota ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
            ano_20 * (raca + educ_mae + regiao + escola + internet) +
            ano_21 * (raca + educ_mae + regiao + escola + internet), 
          data = sample)
summary(fit)
summ(fit, scale = TRUE, digits = 3, robust = TRUE)
modelsummary(fit, stars = TRUE)

predict()

fit.ch <- lm(nota_ch ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
               ano_20 * (raca + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + educ_mae + regiao + escola + internet), 
             data = sample)
summary(fit.ch)

fit.cn <- lm(nota_cn ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
               ano_20 * (raca + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + educ_mae + regiao + escola + internet), 
             data = sample)
summary(fit.cn)

fit.lc <- lm(nota_lc ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
               ano_20 * (raca + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + educ_mae + regiao + escola + internet), 
             data = sample)
summary(fit.lc)

fit.mt <- lm(nota_mt ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
               ano_20 * (raca + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + educ_mae + regiao + escola + internet), 
             data = sample)
summary(fit.mt)

fit.re <- lm(nota_re ~ raca + educ_mae + regiao + escola + internet + ano_19 + ano_20 + ano_21 +
               ano_20 * (raca + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + educ_mae + regiao + escola + internet), 
             data = sample)
summary(fit.re)

modelsummary(list(
    "Média Geral"          = fit,
    "Ciências Humanas"     = fit.ch,
    "Ciências da Natureza" = fit.cn,
    "Linguagens e Códigos" = fit.lc,
    "Matemática"           = fit.mt,
    "Redação"              = fit.re
  ), statistic = c("({std.error})", 
                 "p = {p.value}"))
modelsummary(list(
  "Média Geral"          = fit,
  "Ciências Humanas"     = fit.ch,
  "Ciências da Natureza" = fit.cn,
  "Linguagens e Códigos" = fit.lc,
  "Matemática"           = fit.mt,
  "Redação"              = fit.re
), stars = c("*" = .1, "**" = .05, "***" = .01))

sample %>% 
  ggplot(aes(x = factor(ano), y = nota)) +
    geom_boxplot()

sample %>% 
  filter(nota == 0) %>% 
  View()



fit_re.n <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 +
                 ano_20 * (raca + educ_mae + escola + internet) +
                 ano_21 * (raca + educ_mae + escola + internet), 
               data = sample %>% filter(regiao == "N"))
summary(fit_re.n)

fit_re.ne <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 +
                  ano_20 * (raca + educ_mae + escola + internet) +
                  ano_21 * (raca + educ_mae + escola + internet), 
                data = sample %>% filter(regiao == "NE"))
summary(fit_re.ne)

fit_re.co <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 +
                  ano_20 * (raca + educ_mae + escola + internet) +
                  ano_21 * (raca + educ_mae + escola + internet), 
                data = sample %>% filter(regiao == "CO"))
summary(fit_re.co)

fit_re.se <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 +
                  ano_20 * (raca + educ_mae + escola + internet) +
                  ano_21 * (raca + educ_mae + escola + internet), 
                data = sample %>% filter(regiao == "SE"))
summary(fit_re.se)

fit_re.s <- lm(nota ~ raca + educ_mae + escola + internet + ano_19 + ano_20 + ano_21 +
                 ano_20 * (raca + educ_mae + escola + internet) +
                 ano_21 * (raca + educ_mae + escola + internet), 
               data = sample %>% filter(regiao == "S"))
summary(fit_re.s)

modelsummary(list(fit_re.n, fit_re.ne, fit_re.co, fit_re.se, fit_re.s), stars = TRUE)


load("sample/sample.RData")
sample <- sample %>% 
  filter(raca != "nd" & escola != "nd" & educ_mae != "nd" & internet != "nd") %>% 
  mutate(ano_18 = ano == 2018,
         ano_19 = ano == 2019,
         ano_20 = ano == 2020,
         ano_21 = ano == 2021,
         across(c(ano_18, ano_19, ano_20, ano_21), as_factor))

fit_p <- glm(as.integer(participante) ~ raca + educ_mae + regiao + escola + internet + 
               ano_19 + ano_20 + ano_21 +
               ano_20 * (raca + educ_mae + regiao + escola + internet) +
               ano_21 * (raca + educ_mae + regiao + escola + internet), 
             family = binomial(link='logit'),
             data = sample)
summary(fit_p)

roc <- rocit(score = fit_p$fitted.values, class = fit_p$y)
plot(roc)



