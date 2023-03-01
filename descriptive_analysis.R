library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

catalog <- data.table::fread(input="catalog.csv")
catalog <- catalog %>% 
  filter(group %in% c("todos", "pre_par_ind", "bra_ama")) %>% 
  mutate(across(c(group, year), as_factor)) %>% 
  mutate(group = fct_relevel(group, c("todos", "pre_par_ind", "bra_ama")))


ggplot(catalog %>% filter(group == "todos")) +
  geom_line(aes(year, n_enr, group = 1, linetype = "Inscritos")) +
  geom_line(aes(year, n_att, group = 1, linetype = "Participantes")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Número total (milhões)",
       title = "Inscritos e Participantes (2010 a 2021)",
       linetype = "") +
  scale_linetype_manual(values = c("Inscritos" = "solid", "Participantes" = "dashed"))

ggplot(catalog %>% filter(group != "todos")) +
  geom_line(aes(year, n_enr, group = group, color = group)) +
  scale_y_continuous(limits = c(1e06, 5.3e06), labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Inscritos (milhões)",
       title = "Número de Inscritos por Raça (2010 a 2021)",
       color = "") +
  scale_color_manual(labels = c("Pretos/Pardos/Indígenas", "Brancos/Amarelos"),
                     values = c("darkblue", "darkgray"))

ggplot(catalog %>% filter(group != "todos")) +
  geom_line(aes(year, n_att, group = group, color = group)) +
  scale_y_continuous(limits = c(1e06, 5.3e06), labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Participantes (milhões)",
       title = "Número de Participantes por Raça (2010 a 2021)",
       color = "") +
  scale_color_manual(labels = c("Pretos/Pardos/Indígenas", "Brancos/Amarelos"),
                     values = c("darkblue", "darkgray"))