
# Library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(nexo.utils)

# Data --------------------------------------------------------------------

#-- dados
dados <- read_rds("Dados1.rds")

censo <- haven::read_sav("censo.sav") %>% 
  tbl_df()

# Plots -------------------------------------------------------------------

##--
dados %>% 
  group_by(ano) %>% 
  summarise(n = sum(crianc, na.rm=T)) %>% 
  ggplot(aes(x=ano, y=n)) +
  geom_col() +
  labs(x="Ano",
       y="Número de municípios")

nexo.utils::plot.export("Expansão dos conselhos municipais da Criança e do Adolescente",
                        1, '0005')

##-- 3
dados %>% 
  group_by(ano) %>% 
  summarise(n = sum(saude_, na.rm=T)) %>% 
  ggplot(aes(x=ano, y=n)) +
  geom_col() +
  labs(x="Ano",
       y="Número de municípios")

nexo.utils::plot.export("Expansão dos conselhos municipais de Saúde",
                        2, '0005')

#-- 3
censo %>% 
  filter(q3>=1988) %>% 
  group_by(q3) %>% 
  summarise(n = n()) %>% 
  tbl_df() %>% 
  mutate(n=cumsum(n)) %>%
  ggplot(aes(x=q3, y=n)) +
  geom_col() +
  labs(x="Ano",
       y="Número de municípios")

nexo.utils::plot.export("Expansão dos conselhos municipais de Ass. Social",
                        3, '0005')


dados %>% 
  group_by(ano) %>% 
  summarise(n = sum(educac, na.rm=T)) %>% 
  ggplot(aes(x=ano, y=n)) +
  geom_col() +
  labs(x="Ano",
       y="Número de municípios") 

nexo.utils::plot.export("Expansão dos conselhos municipais de Educação",
                        4, '0005')


# ---- --------------------------------------------------------------------


##--
dados %>% 
  group_by(ano) %>% 
  summarise(n = sum(crianc, na.rm=T)) %>% 
  tbl_df() %>% 
  arrange(ano) %>% 
  mutate(diff=n-lag(n)) %>% 
  ggplot(aes(x=ano, y=diff)) +
  geom_col() +
  labs(x="Ano",
       y="Ano de instalação")

nexo.utils::plot.export("Expansão dos conselhos municipais da Criança e do Adolescente",
                        '1b', '0005')

##-- 3
dados %>% 
  group_by(ano) %>% 
  summarise(n = sum(saude_, na.rm=T)) %>% 
  tbl_df() %>% 
  arrange(ano) %>% 
  mutate(diff=n-lag(n)) %>% 
  ggplot(aes(x=ano, y=diff)) +
  geom_col() +
  labs(x="Ano",
       y="Ano de instalação")

nexo.utils::plot.export("Expansão dos conselhos municipais de Saúde",
                        '2b', '0005')

#-- 3
censo %>% 
  filter(q3>=1988) %>% 
  group_by(q3) %>% 
  summarise(n = n()) %>% 
  tbl_df() %>% 
  ggplot(aes(x=q3, y=n)) +
  geom_col() +
  labs(x="Ano",
       y="Ano de instalação")

nexo.utils::plot.export("Expansão dos conselhos municipais de Saúde",
                        '3b', '0005')


dados %>% 
  group_by(ano) %>% 
  summarise(n = sum(educac, na.rm=T)) %>% 
  tbl_df() %>% 
  arrange(ano) %>% 
  mutate(diff=n-lag(n)) %>% 
  ggplot(aes(x=ano, y=diff)) +
  geom_col() +
  labs(x="Ano",
       y="Ano de instalação")



nexo.utils::plot.export("Expansão dos conselhos municipais de Educação",
                        '4b', '0005')











