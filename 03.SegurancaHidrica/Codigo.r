## 03. Segurança Hidrica
## Autor: Gabriel Maia, Nexo Jornal
## Contato: dados@nexojornal.com.br

## pacote com as funcoes do nexo
library(nexo.utils)
## caso nao tenha instalado
## devtools::install_github('Nexo-Dados/nexo.utils')




#  Extrair os arquivos "ISH_Brasil_2015_light.zip" e "ISH_Brasil_2035_light.zip" para pastas com os mesmos nomes 
#  esses shapefiles foram obtitos em 'https://metadados.ana.gov.br/geonetwork/srv/pt/main.home' e simplificados usando a ferramente 'https://mapshaper.org/' 


# Bibliotecas e importação de dados ---------------------------------------


library(tidyverse)
library(sf)


read_rds("dados/attributes.rds") %>% 
  mutate(ISH=fct_relevel(as_factor(ish_bra_gr), c("Mínimo", "Baixo", "Médio", "Alto", "Máximo")))->attributes_2035 #apenas a tabela de atributos de 2035, sem a geometria

read_sf("dados/ISH_Brasil_2015_light/ISH_2015.shp")->dados_2015 #shapefile e atributos de 2015

dados_2015 %>% cbind(attributes_2035)->dados2 



# Visualizações -----------------------------------------------------------



dados_2015 %>% 
  st_make_valid() %>% 
  mutate(brasil=fct_relevel(as_factor(brasil), c("Mínimo", "Baixo", "Médio", "Alto", "Máximo"))) %>% 
  group_by(brasil) %>%
  summarise() %>% #junta poligonos com o mesmo valor
  ggplot()+geom_sf(aes(fill=brasil), col=NA)+
  geom_sf(data=nexo.utils::mapState,fill=NA)+
  scale_fill_manual(values = c("#456994", "#5C8CC5","#85A9D4","#AEC6E2","#D7E3F1"))

nexo.utils::plot.export("Indice de degurança hidrica no territorio brasileiro 2015",1, 9999)




dados_2015 %>% cbind(attributes_2035) %>% 
  st_make_valid() %>% 
  group_by(ISH) %>% 
  summarise() %>% #junta poligonos com o mesmo valor
  ggplot()+geom_sf(aes(fill=ISH), col=NA)+
  geom_sf(data=nexo.utils::mapState,fill=NA)+
  scale_fill_manual(values = c("#456994", "#5C8CC5","#85A9D4","#AEC6E2","#D7E3F1"))

nexo.utils::plot.export("Indice de degurança hidrica no territorio brasileiro 2035",2, 9999)









dados_2015 %>% cbind(attributes_2035) %>% 
  mutate(brasil=fct_relevel(as_factor(brasil), c("Mínimo", "Baixo", "Médio", "Alto", "Máximo"))) %>% 
  mutate(delta=case_when(as.numeric(brasil)>as.numeric(ISH)~"1.Diminuição",
                         as.numeric(brasil)==as.numeric(ISH)~"2.Estável",         #comparações entre o ISH 2015 e 2035
                         as.numeric(brasil)<as.numeric(ISH)~"3.Aumento")) %>% 
  st_make_valid() %>% 
  group_by(delta) %>% 
  summarise() %>% #junta poligonos com o mesmo valor
  ggplot()+geom_sf(aes(fill=delta), col=NA)+
  geom_sf(data=nexo.utils::mapState,fill=NA)+
  scale_fill_manual(values = c("#F4CC36","#AEC6E2","#6BA99D"))

nexo.utils::plot.export("Indice de degurança hidrica no territorio brasileiro diff 2015-2035",3, 9999)





attributes_2035 %>%
  mutate( id=row_number()) %>% 
  gather(Var, Val, ire_cs_amb:ire_cs_res) %>% 
  arrange(id) %>% #altera o formato do dataset de wide para long em relação às dimensões
  mutate(Val=ifelse(Val==0,99, Val )) %>%  #altera o valor de NA de 0 para 99, para podermos usar min() para determinar a dimensão mais critica
  group_by(id) %>% 
  mutate(atr_min=Var[which.min(Val)]) %>% 
  select(id, atr_min) %>% 
  unique() %>% 
  left_join(dados_2015 %>% mutate( id=row_number()), "id") %>%  #adição dos polígonos
  st_as_sf() %>% st_make_valid() %>%  
  select(atr_min, geometry) %>% 
  group_by(atr_min) %>% 
  summarise() %>% #junta poligonos com o mesmo valor
  ggplot()+geom_sf(aes(fill=atr_min), col=NA)+
  scale_fill_manual(values = c("#58D9B0","#FF7B80","#F4CC36","#6BA99D"))+
  facet_wrap(~atr_min)+
  geom_sf(data=nexo.utils::mapState,fill=NA)

nexo.utils::plot.export("Dimensão mais crítica do ISH",4, 9999)




attributes_2035 %>%
  mutate( id=row_number()) %>% 
  gather(Var, Val, ire_cs_amb:ire_cs_res) %>% arrange(id) %>% 
  mutate(Val=ifelse(Val==0,99, Val )) %>% 
  group_by(id) %>% 
  mutate(atr_min=Var[which.min(Val)]) %>% 
  select(id, atr_min) %>% 
  unique() %>% 
  left_join(dados_2015 %>% mutate( id=row_number())) %>% 
  st_as_sf() %>% ungroup()   ->dim_crits

st_area(dim_crits$geometry)%>% 
  cbind(dim_crits$atr_min) %>% as_tibble() %>% 
  set_names("area", "atr_min") %>% 
  mutate(area2=as.numeric(area)) %>% 
  group_by(atr_min) %>% 
  summarise(area2=sum(area2)/10^6) %>% 
  ggplot(aes(y=area2, x=1, fill= atr_min))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#58D9B0","#FF7B80","#F4CC36","#6BA99D"))

nexo.utils::plot.export("Areas das mais crítica do ISH",5, 9999)
