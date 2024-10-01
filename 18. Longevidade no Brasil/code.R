# Library -----------------------------------------------------------------

library(readxl)
library(tidyverse)
library(nexo.utils)
library(sf)

# Data --------------------------------------------------------------------

pop <- read_excel("API_SP.DYN.LE00.IN_DS2_en_excel_v2_31757.xls", skip=3)

base <- read_excel("base_de_dados.xlsx")
base2 <- read_excel("base_de_dados.xlsx", sheet=2)
base3 <- read_excel("base_de_dados.xlsx", sheet=3)

# Graficos ----------------------------------------------------------------

#-- grafico 1
pop %>% 
  filter(`Country Name`=="Brazil") %>% 
  pivot_longer(cols=c(`1960`:`2023`)) %>% 
  mutate(year = as.numeric(name)) %>% 
  ggplot(aes(x=year, y=value)) +
  geom_area()


plot2("grafico 1")

#--- grafico 2
base %>% 
  filter(ANO==2021 & AGREGACAO %in% c("UF", "BRASIL")) %>% 
  mutate(NOME = replace_na(NOME, "BRASIL")) %>% 
  right_join(mapState, by=c("CODIGO"="ibge2")) %>% 
  mutate(ESPVIDA2 = cut(ESPVIDA, breaks=c(-Inf,70,72,74,76,Inf))) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill=ESPVIDA2)) +
  geom_sf_text(aes(label=scales::number(ESPVIDA,accuracy=.1,
                                        decimal.mark=",")))

plot2("mapa")

#--- grafico 4
base2 %>%
  filter(ANO==2021 & AGREGACAO %in% c("UF", "BRASIL")) %>% 
  mutate(NOME = replace_na(NOME, "BRASIL")) %>% 
  select(NOME, SEXO, ESPVIDA) %>%
  pivot_wider(names_from=SEXO, values_from=ESPVIDA) %>% 
  rename(MASCULINO = HOMEM, FEMININO = MULHER) %>%
  left_join(infoState, by=c("NOME"="state")) %>% 
  mutate(uf = replace_na(uf, "BRASIL")) %>% 
  ggplot(aes(x=reorder(uf, FEMININO))) +
  geom_segment(aes(xend=reorder(uf, FEMININO),
                   y=MASCULINO,
                   yend=FEMININO), col="black") +
  geom_point(aes(y=FEMININO), col="purple") +
  geom_point(aes(y=MASCULINO), col="orange") +
  coord_flip()

plot2("grafico 2")

base3 %>%
  filter(ANO==2021 & AGREGACAO %in% c("UF", "BRASIL")) %>% 
  mutate(NOME = replace_na(NOME, "BRASIL")) %>% 
  select(NOME, COR, ESPVIDA) %>%
  pivot_wider(names_from=COR, values_from=ESPVIDA) %>% 
  left_join(infoState, by=c("NOME"="state")) %>% 
  mutate(uf = replace_na(uf, "BRASIL")) %>% 
  ggplot(aes(x=reorder(uf, BRANCO))) +
  geom_segment(aes(xend=reorder(uf, BRANCO),
                   y=BRANCO,
                   yend=NEGRO), col="black") +
  geom_point(aes(y=BRANCO), col="red") +
  geom_point(aes(y=NEGRO), col="blue") +
  coord_flip()

plot2("grafico 3")



  
  


