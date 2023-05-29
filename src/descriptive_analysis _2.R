library(tidyverse)
library(ggthemes)
library(scales)


load("sample/catalog.RData")
load("sample/sinopse.RData")
load("sample/sample.RData")


theme_set(theme_tufte() +
            theme(text = element_text(family = "sans"),
                  plot.title = element_text(size = 12),
                  axis.text = element_text(size = 8),
                  axis.title = element_text(size = 10),
                  plot.caption = element_text(size = 10, hjust=0)))


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

df_escola %>% 
  ggplot(aes(ano, par_ins, fill = escola)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(labels = c("Pública", "Privada")) +
  labs(x = "Ano", y = "Taxa",
       title = "Gráfico 2 - Taxa de Participação por Inscritos (2019 a 2021)",
       fill = "Tipo de escola",
       caption = "Fonte: INEP") +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0))
# ggsave('plots/1_enr_att_school_2.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

df_escola %>% 
  ggplot(aes(ano, ins_con, fill = escola)) +
  geom_col(position = "dodge") +
  scale_fill_discrete(labels = c("Pública", "Privada")) +
  labs(x = "Ano", y = "Taxa",
       title = "Gráfico 3 - Taxa de Inscrição por Concluintes (2019 a 2021)",
       fill = "Tipo de escola",
       caption = "Fonte: INEP") +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0))


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

df_regiao %>% 
  ggplot(aes(factor(ano), par_ins, fill = regiao)) +
  geom_col(position = "dodge") +
  labs(x = "Ano", y = "Taxa",
       title = "Gráfico 4 - Taxa de Participação por Inscritos (2018 a 2021)",
       fill = "Região",
       caption = "Fonte: INEP") +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0))

df_regiao %>% 
  ggplot(aes(factor(ano), nota, group = regiao, color = regiao)) +
  geom_line(size = .7) +
  labs(x = "Ano", y = "Nota média",
       title = "Gráfico 5 - Nota média por região (2018 a 2021)",
       color = "Região",
       caption = "Fonte: INEP") +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0))

sample %>% 
  filter(participante, raca != "nd") %>% 
  group_by(ano, raca) %>% 
  summarise(nota = mean(nota, na.rm = TRUE)) %>% 
  ggplot(aes(factor(ano), nota, group = raca, color = raca)) +
  geom_line(size = .7) +
  ylim(c(500, 560)) +
  scale_color_manual(values = c("black", "gray"),
                     labels = c("Branco/Amarelo", "Preto/Pardo/Indígena")) +
  labs(x = "Ano", y = "Nota média",
       title = "Gráfico 6 - Nota média por raça (2018 a 2021)",
       color = "Raça",
       caption = "Fonte: INEP")

  
  
  
