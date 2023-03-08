library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggpubr)
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
       title = "Gráfico 1 - Inscritos e Participantes no ENEM (2010 a 2021)",
       linetype = "",
       caption = "Fonte: Elaborado pelo autor") +
  scale_linetype_manual(values = c("Inscritos" = "solid", "Participantes" = "dashed")) +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0))
ggsave('plots/enr_att.png', dpi = 600, height = 8, width = 15, unit = 'cm', bg = 'white')









enr_rc <- ggplot(catalog %>% filter(group != "todos") %>% mutate(year = as.factor(as.numeric(as.character(year)) - 2000))) +
  geom_line(aes(year, n_enr, group = group, color = group)) +
  scale_y_continuous(limits = c(1e06, 5.3e06), labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Inscritos (milhões)",
       title = "Gráfico 2 - Inscritos e Participantes por Raça (2010 a 2021)",
       color = "",
       caption = "Fonte: Elaborado pelo autor") +
  scale_color_manual(labels = c("Pretos/Pardos/Indígenas", "Brancos/Amarelos"),
                     values = c("darkblue", "darkgray")) +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0),
        legend.position = "none")
att_rc <- ggplot(catalog %>% filter(group != "todos") %>% mutate(year = as.factor(as.numeric(as.character(year)) - 2000))) +
  geom_line(aes(year, n_att, group = group, color = group)) +
  scale_y_continuous(limits = c(1e06, 5.3e06), labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "Ano", y = "Participantes (milhões)",
       title = "",
       color = "") +
  scale_color_manual(labels = c("Pretos/Pardos/Indígenas", "Brancos/Amarelos"),
                     values = c("darkblue", "darkgray")) +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        legend.position = "none")
ggarrange(enr_rc, att_rc, ncol = 2, nrow = 1, common.legend = TRUE, legend="bottom")
ggsave('plots/enr_att_rc.png', dpi = 600, height = 8, width = 18, unit = 'cm', bg = 'white')



annotate_figure(rc,
                top = text_grob("Inscrição e participação (2010 a 2021)", family  = "sans", size = 22))

catalog <- catalog %>% mutate(rate = n_att / n_enr)


mean_w <- catalog %>% 
  filter(group == "bra_ama" & as.numeric(as.character(year)) <= 2019) %>% 
  summarise(media = mean(rate)) %>% 
  pull(media)
mean_b <- catalog %>% 
  filter(group == "pre_par_ind" & as.numeric(as.character(year)) <= 2019) %>% 
  summarise(media = mean(rate)) %>% 
  pull(media)

ggplot(catalog %>% 
         filter(group != "todos") %>% 
         mutate(rate_p = lag(lag(rate)),
                delta = rate - rate_p) %>% 
         mutate(year = as.factor(as.numeric(as.character(year)) - 2000))) +
  geom_line(aes(year, rate, group = group, color = group)) +
  geom_hline(yintercept = mean_w, color = "darkgray", linetype="dashed") +
  geom_hline(yintercept = mean_b, color = "darkblue", linetype="dashed") +
  labs(x = "Ano", y = "Número total (milhões)",
       title = "Gráfico 3 - Taxa de Participação por Raça (2010 a 2021)",
       color = "",
       caption = "Fonte: Elaborado pelo autor") +
  scale_color_manual(labels = c("Pretos/Pardos/Indígenas", "Brancos/Amarelos"),
                     values = c("darkblue", "darkgray")) +
  theme_tufte() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 12),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust=0))
ggsave('plots/rt_rc.png', dpi = 600, height = 8, width = 15, unit = 'cm', bg = 'white')


