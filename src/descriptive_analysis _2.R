library(tidyverse)
library(ggthemes)
library(scales)


load("sample/catalog.RData")
load("sample/sinopse.RData")
load("sample/sample.RData")

catalog <- catalog %>% 
  rename(ano = NU_ANO, status = STATUS, regiao = REGIAO, raca = TP_COR_RACA, n = N)

sinopse <- sinopse %>% 
  mutate(across(c(serie, dep_adm), as_factor),
         ano = as.integer(ano),
         escola = fct_collapse(dep_adm,
                               "priv" = "pri",
                               "publ" = c("fed", "est" ,"mun"))) %>% 
  group_by(ano, regiao, escola, dep_adm) %>% 
  summarise(n = sum(n))


catalog %>% 
  filter(STATUS) %>% 
  group_by(NU_ANO) %>% 
  summarise(TOTAL = sum(N)) %>% 
  mutate(STATUS = "Participantes") %>% 
  bind_rows(catalog %>% 
              group_by(NU_ANO) %>% 
              summarise(TOTAL = sum(N)) %>% 
              mutate(STATUS = "Inscritos")) %>%
  ggplot(aes(NU_ANO, TOTAL, linetype = STATUS)) +
    geom_line() +
    scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    labs(x = "Ano", y = "Número total (milhões)",
         title = "Gráfico 1 - Inscritos e Participantes no ENEM (2010 a 2021)",
         linetype = "",
         caption = "Fonte: INEP") +
    theme_tufte() +
    theme(text = element_text(family = "sans"),
          plot.title = element_text(size = 12),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 10),
          plot.caption = element_text(size = 10, hjust=0))
ggsave('plots/1_enr_att_2.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

sinopse %>% 
  mutate(ano = as.integer(ano)) %>% 
  group_by(ano) %>% 
  summarise(concluintes = sum(n)) %>% 
  left_join(sample %>% 
              group_by(ano) %>% 
              summarise(inscritos = n())) %>% 
  left_join(sample %>% 
              group_by(ano) %>% 
              filter(participante) %>%
              summarise(participantes = n())) %>% 
  pivot_longer(c(concluintes, inscritos, participantes), names_to = "status", values_to = "n") %>% 
  ggplot(aes(ano, n, color = status)) +
    geom_line()






