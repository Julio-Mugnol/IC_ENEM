library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

catalog <- data.table::fread(input="catalog.csv")
catalog <- catalog %>% 
  filter(group %in% c("todos", "pre_par_ind", "bra_ama")) %>% 
  mutate(across(c(group, year), as_factor))

ggplot(catalog %>% filter(group == "todos")) +
  geom_line(aes(year, n_enr, group = 1)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Inscritos (milhões)",
       title = "Número de Inscritos 2010-2021")

ggplot(catalog %>% filter(group != "todos")) +
  geom_line(aes(year, n_enr, group = group, colour = group)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Inscritos (milhões)",
       title = "Número de Inscritos 2010-2021")

