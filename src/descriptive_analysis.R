library(tidyverse)
library(ggthemes)
library(scales)
library(RColorBrewer)


load("sample/catalog.RData")
load("sample/sinopse.RData")
load("sample/sample.RData")


theme_set(theme_tufte() +
            theme(text = element_text(family = "serif"),
                  plot.title = element_text(size = 12),
                  axis.text = element_text(size = 8),
                  axis.title = element_text(size = 10),
                  plot.caption = element_text(size = 10, hjust=0)))

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

# Gráfico 1 - Inscritos e Participantes no ENEM (2010 a 2022)
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
ggsave('plots/1_enr_att.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

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

# Gráfico 2 - Taxa de Participantes por Inscritos (2019 a 2022)
df_escola %>% 
  ggplot(aes(ano, par_ins, fill = escola)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palette_escola,
                    labels = c("Pública", "Privada")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Participantes por Inscritos (2019 a 2022)",
       fill = "Tipo de escola",
       caption = "Fonte: INEP")
ggsave('plots/2_enr_att_school.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

# Gráfico 3 - Taxa de Inscritos por Concluintes (2019 a 2022)
df_escola %>% 
  ggplot(aes(ano, ins_con, fill = escola)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = palette_escola,
                    labels = c("Pública", "Privada")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Inscritos por Concluintes (2019 a 2022)",
       fill = "Tipo de escola",
       caption = "Fonte: INEP")
ggsave('plots/2_enr_grad_school.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')


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

# Gráfico 4 - Taxa de Participantes por Inscritos (2018 a 2022)
df_regiao %>% 
  ggplot(aes(factor(ano), par_ins, fill = regiao)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Paired",
                    labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) +
  labs(x = "Ano", y = "Taxa",
       title = "Taxa de Participantes por Inscritos (2018 a 2022)",
       fill = "Região",
       caption = "Fonte: INEP")
ggsave('plots/2_enr_att_region.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

# Gráfico 5 - Taxa de Participantes por Inscritos (2018 a 2022)
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
       title = "Taxa de Participantes por Inscritos (2018 a 2022)",
       fill = "Raça",
       caption = "Fonte: INEP")
ggsave('plots/2_enr_att_race.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

# Gráfico 6 - Nota média por região (2018 a 2022)
df_regiao %>% 
  ggplot(aes(factor(ano), nota, group = regiao, color = regiao)) +
  geom_line(size = .7) +
  geom_point() +
  scale_color_brewer(palette = "Paired",
                     labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) +
  labs(x = "Ano", y = "Nota média",
       title = "Nota média por região (2018 a 2022)",
       color = "Região",
       caption = "Fonte: INEP")
ggsave('plots/3_score_region.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')

# Gráfico 7 - Nota média por raça (2018 a 2022)
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
       color = "Raça",
       caption = "Fonte: INEP")
ggsave('plots/3_score_race.png', dpi = 600, height = 10, width = 16, unit = 'cm', bg = 'white')




  
  
  
