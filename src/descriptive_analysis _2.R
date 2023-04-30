library(tidyverse)
library(ggthemes)
library(scales)


load("sample/catalog.RData")

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





