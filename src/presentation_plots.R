library(tidyverse)
library(ggthemes)
library(modelsummary)
library(RColorBrewer)
library(scales)

##### SET THEME #####
theme_set(theme_bw() +
            theme(text = element_text(family = "sans"),
                  plot.title = element_text(size = 18),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16),
                  plot.caption = element_text(size = 16, hjust=0),
                  plot.background = element_rect(fill = "transparent", color = NA),
                  legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  legend.background = element_rect(fill = "transparent"),
                  legend.box.background = element_rect(fill = "transparent"),
                  legend.key = element_blank(),
                  legend.position = "bottom",
                  panel.grid.major.x = element_line(color = "transparent"),
                  panel.background = element_rect(fill = "transparent")))


##### PLOTS NOTAS #####

### RACA ###
p <- modelplot(fit_fe, 
               coef_map = c("racapre" = "pre_2018",
                       "racapre:ano2019" = "pre_2019",
                       "racapre:ano2020" = "pre_2020",
                       "racapre:ano2021" = "pre_2021",
                       "racapre:ano2022" = "pre_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% 
              filter(ano == 2018) %>% 
              select(term, sum = estimate), 
            by = c("term")) %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>%
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5) +
  geom_line(aes(y = estimate, group = 1), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Brancos/Amarelos")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_manual(labels = c("Negros/Indígenas"), values = c("black")) +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ . / 12.378609,
    labels = unit_format(unit = "%", scale = 1e2),
    breaks = seq(0, -.6, -.2))) +
  labs(x = "Ano", y = "Estimativa",
       color = "",
       title = "Raça x Ano")
ggsave('plots/reg/nota_raca.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

### EDUCACAO MAE ###
p <- modelplot(fit_fe, 
               coef_map = c("educ_maefund" = "fund_2018",
                            "educ_maefund:ano2019" = "fund_2019",
                            "educ_maefund:ano2020" = "fund_2020",
                            "educ_maefund:ano2021" = "fund_2021",
                            "educ_maefund:ano2022" = "fund_2022",
                            "educ_maemed" = "med_2018",
                            "educ_maemed:ano2019" = "med_2019",
                            "educ_maemed:ano2020" = "med_2020",
                            "educ_maemed:ano2021" = "med_2021",
                            "educ_maemed:ano2022" = "med_2022",
                            "educ_maesup" = "sup_2018",
                            "educ_maesup:ano2019" = "sup_2019",
                            "educ_maesup:ano2020" = "sup_2020",
                            "educ_maesup:ano2021" = "sup_2021",
                            "educ_maesup:ano2022" = "sup_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p <- p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum)))
p %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5, position = position_dodge(width = .2)) +
  geom_line(aes(y = estimate), position = position_dodge(width = .2), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, 
                position = position_dodge(width = .2), size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Fund. Incompleto")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_brewer(palette = "Paired",
                     labels = c("Fund.", "Médio", "Superior")) +
  labs(x = "Ano", y = "Estimativa",
       color = "",
       title = "Escolaridade da Mãe x Ano")
ggsave('plots/reg/nota_educ_mae.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

# Variacao porcentual
p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, (. + abs(sum)) / abs(sum) - 1, . / sum - 1))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5, position = position_dodge(width = .2)) +
  geom_line(aes(y = estimate), position = position_dodge(width = .2), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, 
                position = position_dodge(width = .2), size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Fund. Incompleto")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_brewer(palette = "Paired",
                     labels = c("Fund.", "Médio", "Superior")) +
  scale_y_continuous(labels = unit_format(unit = "%", scale = 1e2)) +
  labs(x = "Ano", y = "Estimativa (%)",
       color = "",
       title = "Escolaridade da Mãe x Ano")
ggsave('plots/reg/nota_educ_mae_pct.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

### ESCOLA ###
p <- modelplot(fit_fe, 
               coef_map = c("escolapriv" = "priv_2018",
                            "escolapriv:ano2019" = "priv_2019",
                            "escolapriv:ano2020" = "priv_2020",
                            "escolapriv:ano2021" = "priv_2021",
                            "escolapriv:ano2022" = "priv_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")
  
p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5) +
  geom_line(aes(y = estimate), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Pública")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_manual(labels = c("Privada"), values = c("black")) +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ . / 71.546145,
    labels = unit_format(unit = "%", scale = 1e2),
    breaks = seq(0, -.2, -.1))) +
  labs(x = "Ano", y = "Estimativa",
       title = "Tipo de Escola x Ano",
       color = "")
ggsave('plots/reg/nota_escola.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

# INTERNET
p <- modelplot(fit_fe, 
               coef_map = c("internetS" = "S_2018",
                            "internetS:ano2019" = "S_2019",
                            "internetS:ano2020" = "S_2020",
                            "internetS:ano2021" = "S_2021",
                            "internetS:ano2022" = "S_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5) +
  geom_line(aes(y = estimate), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Não")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_manual(labels = c("Sim"), values = c("black")) +
  scale_y_continuous(sec.axis = sec_axis(
    trans = ~ . / 16.884254,
    labels = unit_format(unit = "%", scale = 1e2),
    breaks = seq(0, .4, .2))) +
  labs(x = "Ano", y = "Estimativa",
       title = "Internet em Casa x Ano",
       color = "")
ggsave('plots/reg/nota_internet.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

# REGIAO
p <- modelplot(fit, 
               coef_map = c("regiaoNE" = "NE_2018",
                            "regiaoNE:ano2019" = "NE_2019",
                            "regiaoNE:ano2020" = "NE_2020",
                            "regiaoNE:ano2021" = "NE_2021",
                            "regiaoNE:ano2022" = "NE_2022",
                            "regiaoSE" = "SE_2018",
                            "regiaoSE:ano2019" = "SE_2019",
                            "regiaoSE:ano2020" = "SE_2020",
                            "regiaoSE:ano2021" = "SE_2021",
                            "regiaoSE:ano2022" = "SE_2022",
                            "regiaoS" = "S_2018",
                            "regiaoS:ano2019" = "S_2019",
                            "regiaoS:ano2020" = "S_2020",
                            "regiaoS:ano2021" = "S_2021",
                            "regiaoS:ano2022" = "S_2022",
                            "regiaoCO" = "CO_2018",
                            "regiaoCO:ano2019" = "CO_2019",
                            "regiaoCO:ano2020" = "CO_2020",
                            "regiaoCO:ano2021" = "CO_2021",
                            "regiaoCO:ano2022" = "CO_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p <- p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum)))

p %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5, position = position_dodge(width = .3)) +
  geom_line(aes(y = estimate), position = position_dodge(width = .3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, 
                position = position_dodge(width = .3)) +
  geom_hline(aes(yintercept = 0, linetype = "Norte")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_brewer(palette = "Paired",
                     labels = c("Centro-Oeste", "Nordeste", "Sul", "Sudeste")) +
  labs(x = "Ano", y = "Estimativa",
       color = "",
       title = "Região x Ano")
ggsave('plots/reg/nota_regiao.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

# Variacao porcentual
p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum = estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, (. + abs(sum)) / abs(sum) - 1, . / sum - 1))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5, position = position_dodge(width = .3)) +
  geom_line(aes(y = estimate), position = position_dodge(width = .3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, 
                position = position_dodge(width = .3)) +
  geom_hline(aes(yintercept = 0, linetype = "Norte")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_brewer(palette = "Paired",
                     labels = c("Centro-Oeste", "Nordeste", "Sul", "Sudeste")) +
  scale_y_continuous(labels = unit_format(unit = "%", scale = 1e2)) +
  labs(x = "Ano", y = "Estimativa (%)",
       color = "",
       title = "Região x Ano")
ggsave('plots/reg/nota_regiao_pct.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

##### PLOTS PRESENCA #####

### RACA ###
p <- modelplot(fit_p_mpl_fe, 
               coef_map = c("racapre" = "pre_2018",
                            "racapre:ano2019" = "pre_2019",
                            "racapre:ano2020" = "pre_2020",
                            "racapre:ano2021" = "pre_2021",
                            "racapre:ano2022" = "pre_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% 
              filter(ano == 2018) %>% 
              select(term, sum = estimate), 
            by = c("term")) %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>%
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5) +
  geom_line(aes(y = estimate, group = 1), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Brancos/Amarelos")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_manual(labels = c("Negros/Indígenas"), values = c("black")) +
  scale_y_continuous(labels = unit_format(unit = "p.p.", scale = 1e2, accuracy = 1)) +
  labs(x = "Ano", y = "Estimativa",
       color = "",
       title = "Raça x Ano")
ggsave('plots/reg/presenca_raca.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

### EDUC MAE ###
p <- modelplot(fit_p_mpl_fe, 
               coef_map = c("educ_maefund" = "fund_2018",
                            "educ_maefund:ano2019" = "fund_2019",
                            "educ_maefund:ano2020" = "fund_2020",
                            "educ_maefund:ano2021" = "fund_2021",
                            "educ_maefund:ano2022" = "fund_2022",
                            "educ_maemed" = "med_2018",
                            "educ_maemed:ano2019" = "med_2019",
                            "educ_maemed:ano2020" = "med_2020",
                            "educ_maemed:ano2021" = "med_2021",
                            "educ_maemed:ano2022" = "med_2022",
                            "educ_maesup" = "sup_2018",
                            "educ_maesup:ano2019" = "sup_2019",
                            "educ_maesup:ano2020" = "sup_2020",
                            "educ_maesup:ano2021" = "sup_2021",
                            "educ_maesup:ano2022" = "sup_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5, position = position_dodge(width = .2)) +
  geom_line(aes(y = estimate), position = position_dodge(width = .2), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8,
                position = position_dodge(width = .2)) +
  geom_hline(aes(yintercept = 0, linetype = "Fund. Incompleto")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_brewer(palette = "Paired",
                     labels = c("Fund.", "Médio", "Superior")) +
  scale_y_continuous(labels = unit_format(unit = "pp", scale = 1e2, accuracy = 1)) +
  labs(x = "Ano", y = "Estimativa",
       color = "",
       title = "Escolaridade da Mãe x Ano")
ggsave('plots/reg/presenca_educ_mae.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

### ESCOLA ###
p <- modelplot(fit_p_mpl_fe, 
               coef_map = c("escolapriv" = "priv_2018",
                            "escolapriv:ano2019" = "priv_2019",
                            "escolapriv:ano2020" = "priv_2020",
                            "escolapriv:ano2021" = "priv_2021",
                            "escolapriv:ano2022" = "priv_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5) +
  geom_line(aes(y = estimate), size = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Pública")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_manual(labels = c("Privada"), values = c("black")) +
  scale_y_continuous(labels = unit_format(unit = "pp", scale = 1e2, accuracy = 1)) +
  labs(x = "Ano", y = "Estimativa",
       title = "Tipo de Escola x Ano",
       color = "")
ggsave('plots/reg/presenca_escola.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

### INTERNET ###
p <- modelplot(fit_p_mpl_fe, 
               coef_map = c("internetS" = "S_2018",
                            "internetS:ano2019" = "S_2019",
                            "internetS:ano2020" = "S_2020",
                            "internetS:ano2021" = "S_2021",
                            "internetS:ano2022" = "S_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5) +
  geom_line(aes(y = estimate), size = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8) +
  geom_hline(aes(yintercept = 0, linetype = "Não")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_manual(labels = c("Sim"), values = c("black")) +
  scale_y_continuous(labels = unit_format(unit = "pp", scale = 1e2, accuracy = 1)) +
  labs(x = "Ano", y = "Estimativa",
       title = "Internet em Casa x Ano",
       color = "")
ggsave('plots/reg/presenca_internet.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

### REGIAO ###
p <- modelplot(fit_p_mpl, 
               coef_map = c("regiaoNE" = "NE_2018",
                            "regiaoNE:ano2019" = "NE_2019",
                            "regiaoNE:ano2020" = "NE_2020",
                            "regiaoNE:ano2021" = "NE_2021",
                            "regiaoNE:ano2022" = "NE_2022",
                            "regiaoSE" = "SE_2018",
                            "regiaoSE:ano2019" = "SE_2019",
                            "regiaoSE:ano2020" = "SE_2020",
                            "regiaoSE:ano2021" = "SE_2021",
                            "regiaoSE:ano2022" = "SE_2022",
                            "regiaoS" = "S_2018",
                            "regiaoS:ano2019" = "S_2019",
                            "regiaoS:ano2020" = "S_2020",
                            "regiaoS:ano2021" = "S_2021",
                            "regiaoS:ano2022" = "S_2022",
                            "regiaoCO" = "CO_2018",
                            "regiaoCO:ano2019" = "CO_2019",
                            "regiaoCO:ano2020" = "CO_2020",
                            "regiaoCO:ano2021" = "CO_2021",
                            "regiaoCO:ano2022" = "CO_2022"),
               draw = FALSE) %>% 
  separate(term, c("term", "ano"), "_")

p %>% 
  left_join(p %>% filter(ano == 2018) %>% select(term, sum =estimate), by = c("term")) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(ano != 2018, ., . - sum))) %>% 
  ggplot(aes(x = ano, group = term, color = term)) +
  geom_point(aes(y = estimate), size = 2.5, position = position_dodge(width = .3)) +
  geom_line(aes(y = estimate), position = position_dodge(width = .3), linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, size = .8,
                position = position_dodge(width = .3)) +
  geom_hline(aes(yintercept = 0, linetype = "Norte")) +
  scale_linetype_manual(name = "", values = c("dashed")) +
  scale_color_brewer(palette = "Paired",
                     labels = c("Centro-Oeste", "Nordeste", "Sul", "Sudeste")) +
  scale_y_continuous(labels = unit_format(unit = "pp", scale = 1e2, accuracy = 1)) +
  labs(x = "Ano", y = "Estimativa",
       color = "",
       title = "Região x Ano")
ggsave('plots/reg/presenca_regiao.png', dpi = 600, height = 15, width = 22, unit = 'cm', bg = 'transparent')

##### PLOTS DESCRITIVA #####

load("sample/catalog.RData")
load("sample/sinopse.RData")
load("sample/sample.RData")

palette_raca = c("#9EBCDA", "#810F7C")
palette_escola = c("#66C2A4", "#2CA25F")

sinopse <- sinopse %>% 
  mutate(across(c(serie, dep_adm), as_factor),
         ano = as.integer(ano),
         escola = fct_collapse(dep_adm,
                               "priv" = "pri",
                               "publ" = c("fed", "est" ,"mun"))) %>% 
  group_by(ano, regiao, escola, dep_adm) %>% 
  summarise(n = sum(n))

### Inscritos e Participantes no ENEM (2010 a 2022) ###
catalog %>% 
  filter(participante) %>% 
  group_by(ano) %>% 
  summarise(total = sum(N)) %>% 
  mutate(status = "Participantes") %>% 
  bind_rows(catalog %>% 
              group_by(ano) %>% 
              summarise(total = sum(N)) %>% 
              mutate(status = "Inscritos")) %>%
  ggplot(aes(ano, total, linetype = status)) +
  geom_line() +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(x = "Ano", y = "Número total (milhões)",
       title = "Inscritos e Participantes no ENEM (2010 a 2022)",
       linetype = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/1_enr_att.png', dpi = 600, height = 15, width = 22,
       unit = 'cm', bg = 'transparent')


df_escola <- sinopse %>% 
  filter(ano != 2018) %>% 
  group_by(ano, escola) %>% 
  summarise(concluintes = sum(n)) %>% 
  left_join(sample %>% 
              group_by(ano, escola) %>% 
              summarise(inscritos = n())) %>% 
  left_join(sample %>% 
              group_by(ano, escola) %>% 
              filter(participante) %>%
              summarise(participantes = n())) %>% 
  mutate(par_ins = participantes / inscritos,
         ins_con = inscritos / concluintes,
         par_con = participantes / concluintes)

### Taxa de Participantes por Inscritos (2019 a 2022) ###
df_escola %>% 
  ggplot(aes(ano, par_ins, fill = escola)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palette_escola,
                    labels = c("Pública", "Privada")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Participantes por Inscritos - Tipo de Escola (2019 a 2022)",
       fill = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/2_enr_att_school.png', dpi = 600, height = 15, width = 22,
       unit = 'cm', bg = 'transparent')

### Taxa de Inscritos por Concluintes (2019 a 2022) ###
df_escola %>% 
  ggplot(aes(ano, ins_con, fill = escola)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palette_escola,
                    labels = c("Pública", "Privada")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Inscritos por Concluintes - Tipo de Escola (2019 a 2022)",
       fill = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/2_enr_grad_school.png', dpi = 600, height = 15, width = 22,
       unit = 'cm', bg = 'transparent')


df_regiao <- sinopse %>% 
  group_by(ano, regiao) %>% 
  summarise(concluintes = sum(n)) %>% 
  left_join(sample %>% 
              group_by(ano, regiao) %>% 
              summarise(inscritos = n())) %>% 
  left_join(sample %>% 
              group_by(ano, regiao) %>% 
              filter(participante) %>%
              summarise(participantes = n(),
                        nota = mean(nota, na.rm = TRUE))) %>% 
  mutate(par_ins = participantes / inscritos,
         ins_con = inscritos / concluintes,
         par_con = participantes / concluintes)

### Taxa de Participantes por Inscritos (2018 a 2022) ###
df_regiao %>% 
  ggplot(aes(factor(ano), par_ins, fill = regiao)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired",
                    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Participantes por Inscritos - Região (2018 a 2022)",
       fill = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/2_enr_att_region.png', dpi = 600, height = 15, width = 22, 
       unit = 'cm', bg = 'transparent')

### Taxa de Participantes por Inscritos (2018 a 2022) ###
sample %>% 
  filter(participante, raca != "nd") %>% 
  group_by(ano, raca) %>% 
  summarise(participantes = n()) %>% 
  left_join(sample %>% 
              filter(raca != "nd") %>% 
              group_by(ano, raca) %>% 
              summarise(inscritos = n())) %>% 
  mutate(par_ins = participantes / inscritos) %>% 
  ggplot(aes(factor(ano), par_ins, fill = raca)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palette_raca,
                    labels = c("Branco/Amarelo", "Preto/Pardo/Indígena")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Participantes por Inscritos - Raça (2018 a 2022)",
       fill = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/2_enr_att_race.png', dpi = 600, height = 15, width = 22, 
       unit = 'cm', bg = 'transparent')

### Nota média por região (2018 a 2022) ###
df_regiao %>% 
  ggplot(aes(factor(ano), nota, group = regiao, color = regiao)) +
  geom_line(linewidth = .7) +
  geom_point() +
  scale_color_brewer(palette = "Paired",
                     labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) +
  labs(x = "Ano", y = "Nota média",
       title = "Nota média por região (2018 a 2022)",
       color = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/3_score_region.png', dpi = 600, height = 15, width = 22, 
       unit = 'cm', bg = 'transparent')

### Nota média por raça (2018 a 2022) ###
sample %>% 
  filter(participante, raca != "nd") %>% 
  group_by(ano, raca) %>% 
  summarise(nota = mean(nota, na.rm = TRUE)) %>% 
  ggplot(aes(factor(ano), nota, group = raca, color = raca)) +
  geom_line(size = .7) +
  geom_point() +
  ylim(c(490, 570)) +
  scale_color_manual(values = palette_raca,
                     labels = c("Branco/Amarelo", "Preto/Pardo/Indígena")) +
  labs(x = "Ano", y = "Nota média",
       title = "Nota média por raça (2018 a 2022)",
       color = "",
       caption = "Fonte: INEP")
ggsave('plots/presentation/3_score_race.png', dpi = 600, height = 15, width = 22, 
       unit = 'cm', bg = 'transparent')
