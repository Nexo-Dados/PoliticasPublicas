
# Library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(nexo.utils)
library(janitor)

# Read data ---------------------------------------------------------------

dados <- read_excel("dados_brutos.xlsx", sheet="dados_brutos") %>% 
  clean_names()

glimpse(dados)

# Graficos ----------------------------------------------------------------

dados %>% 
  mutate(covid = lei_orcamentaria_anual_loa_saude -
           lei_orcamentaria_anual_loa_saude_s_covid) %>% 
  rename(orc_saude = lei_orcamentaria_anual_loa_saude_s_covid) %>% 
  pivot_longer(cols=c(orc_saude, covid)) %>% 
  group_by(ano) %>% 
  mutate(total = sum(value, na.rm=T)) %>% 
  ggplot(aes(x=ano, y=value/1e9, fill=name)) +
  geom_col() +
  geom_text(aes(y=total/1e9, label=round(total/1e9,1))) 

nexo.utils::plot2("grafico1.pdf")

#-- grf 2
dados %>% 
  ggplot(aes(x=ano, y=investimentos_em_saude/1e9)) +
  geom_area()+
  geom_text(aes(label=round(investimentos_em_saude/1e9,1)))

nexo.utils::plot2("grafico2.pdf")

#-- grf 3
dados %>%
  pivot_longer(cols=c(despesas_medicas:outros)) %>% 
  mutate(name =  fct_relevel(name, "outros", after=0)) %>% 
  group_by(ano) %>% 
  mutate(total = sum(value, na.rm=T)) %>% 
  ggplot(aes(x=ano, y=value/1e9, fill=name)) +
  geom_col() +
  geom_text(aes(y=total/1e9, label=round(total/1e9,1))) 

nexo.utils::plot2("grafico3.pdf")


  




