

# Biblioteca --------------------------------------------------------------

library(tidyverse)
library(readxl)
library(nexo.utils)


# Data --------------------------------------------------------------------

tabela <- readxl.all('tabela.xlsx')


tabela[[5]] %>%
  filter(ano==2020) %>% 
  gather(var, val, -ano, -categoria) %>%
  filter(categoria != 'Total') %>% 
  mutate(var2 = paste(categoria, var)) -> x1

x1 %>% 
  mutate(val2 = cut(val, breaks=c(0,1000,1500,2000,3000,4000,5000, Inf),
                   labels=c('Até 1000', '1000 a 1500', '1500 a 2000',
                            '2000 a 3000', '3000 a 4000',
                            '4000 a 5000', 'Mais de 5000'))) %>% 
  ggplot(aes(x=categoria, y=var, fill=val2)) +
  geom_tile() +
  geom_text(fill=NA, aes(label=round(val))) +
  scale_fill_brewer(palette="Greens")

plot.export('grafico 1', 1, 814, display=F)


tabela[[4]] %>%
  filter(ano==2020) %>% 
  gather(var, val, -ano, -categoria) %>%
  filter(categoria != 'Total') %>% 
  mutate(var2 = paste(categoria, var)) -> x2

x2 %>% 
  mutate(val2 = cut(val, breaks=c(0.05, 0.075, 0.10, 0.15, 0.20, 0.25, Inf),
                    labels=c('5-7,5%', '7,5-10%', '10-15%', '15-20%', '20-25%', 'Mais de 25%'))) %>% 
  ggplot(aes(x=categoria, y=var, fill=val2)) +
  geom_tile() +
  geom_text(fill=NA, aes(label=paste0(round(val*100,1), "%"))) +
  scale_fill_brewer(palette="Reds")

plot.export('grafico 2', 2, 814, display=F)

x1 %>% 
  select(var2, val) %>% 
  left_join(x2 %>% select(var2, val), by=c('var2')) %>% 
  mutate(var2 = str_replace_all(var2, "Sem instrução", "Sem formação")) %>% 
  set_names('var', 'rend', 'desoc') %>% 
  mutate(var = gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',var,perl = TRUE)) %>% 
  mutate(var2 = str_sub(var,1,2),
         var3 = str_sub(var, 3,4)) %>% 
  ggplot(aes(x=rend, y=desoc, col=var2)) +
  geom_point() +
  scale_y_continuous(labels=scales::percent, limits=c(0, .35)) +
  geom_text(aes(label=var)) +
  coord_flip() +
  scale_x_continuous(limits=c(0, 7000))  +
  facet_wrap(~var3)
 
plot.export('grafico 3', 3, 814, display=F)

x1 %>% 
  select(var2, val) %>% 
  left_join(x2 %>% select(var2, val), by=c('var2')) %>% 
  mutate(var2 = str_replace_all(var2, "Sem instrução", "Sem formação")) %>% 
  set_names('var', 'rend', 'desoc') %>% 
  mutate(var = gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',var,perl = TRUE)) %>% 
  mutate(var2 = str_sub(var,1,2),
         var3 = str_sub(var, 3,4)) %>% 
  ggplot(aes(x=reorder(var2, -rend)))
  
  ggplot(aes(x=rend, y=desoc, col=var3)) +
  geom_point() +
  scale_y_continuous(labels=scales::percent, limits=c(0, .35)) +
  geom_text(aes(label=var)) +
  coord_flip() +
  scale_x_continuous(limits=c(0, 7000)) 

#--- time series







