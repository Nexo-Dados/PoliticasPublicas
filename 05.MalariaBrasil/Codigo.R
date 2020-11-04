## 05. Malária
## Autor: Gabriel Maia, Nexo Jornal
## Contato: dados@nexojornal.com.br

## pacote com as funcoes do nexo
library(nexo.utils)
## caso nao tenha instalado
## devtools::install_github('Nexo-Dados/nexo.utils')


# Bibliotecas e importação de dados ---------------------------------------


library(tidyverse)
library(sf)


mensal<-data.table::fread("dados/05.MalariaBrasil/internacoes.csv", encoding = "Latin-1") %>% 
  gather(AnoMes, Valor, `2007/Jul`:`2020/Ago`) %>%  #converte para formato tidy
  mutate(tempor=AnoMes) %>% 
  separate(tempor, c("Ano", "Mes"), "/") %>% #separa colunas
  separate(col = Município, c("ibge6", "municipio"), sep = " ", extra = "merge") %>% #separa ibge6 e nome
  mutate(ibge6=as.numeric(ibge6)) %>% 
  mutate(Valor=replace_na(as.numeric(Valor), 0)) %>% 
  mutate(Ano=as.numeric(Ano))


mort_fx_etarias<-data.table::fread("dados/05.MalariaBrasil/mortalidade_fxetaria.csv", encoding = "Latin-1", skip = 4) %>% 
  mutate(Taxa_mortalidade=as.numeric(str_replace(Taxa_mortalidade, ",", "."))) %>% 
  mutate(fx_order=row_number()) %>% 
  mutate(`Faixa Etária 1`=fct_reorder(as_factor(`Faixa Etária 1`), fx_order))

fx_etarias<-data.table::fread("0008. Malaria/internacoes_fxetaria.csv", encoding = "Latin-1") %>% 
  mutate(fx_order=row_number()) %>% 
  gather(Ano, Valor, `2007`:`2020`) %>% #converte para formato tidy 
  filter(Ano %in% c(2008:2019)) %>% #corta anos incompletos
  mutate(`Faixa Etária 1`=fct_reorder(as_factor(`Faixa Etária 1`), fx_order)) %>% 
  filter(`Faixa Etária 1`!="Total")

# Tratamento --------------------------------------------------------------

mensal %>% filter(Ano %in% c(2008:2019)) %>% #corta anos incompletos
  group_by(ibge6,municipio,  Ano) %>% 
  summarise(Valor=sum(Valor)) %>%     #calcula internações anuais
  left_join(nexo.utils::popMunic, c("ibge6"="ibge6", "Ano"="year")) %>%  #junta com dataset de populações ano a ano
  mutate(Valor=100000*replace_na(Valor, 0)/pop) %>%   # calcula as internações per 100k anuais
  group_by(ibge6,municipio, uf,ibge2,ibge7) %>%  
  summarise(Valor=mean(Valor))->anual_percapita   #media anual no periodo




# Viz ---------------------------------------------------------------------

setwd("05.MalariaBrasil")

nexo.utils::mapMunic %>%   #shapefile municipios 
  left_join(nexo.utils::infoMunic, "ibge7") %>%    #junta com info dos municipios
  left_join(nexo.utils::popMunic %>% filter(year==2019)) %>%   
  left_join(anual_percapita %>% select(ibge6, Valor) %>% unique(), "ibge6") %>% 
  mutate(Total=replace_na(Valor, 0)) %>% 
  mutate(Total2=cut(Total,c(-Inf, 0, 5, 10, 50, 100, Inf) ,include.lowest = T)) %>% #corta valor em categorias
  ggplot(aes(fill=Total2))+geom_sf(col=NA)+scale_fill_viridis_d()
  
nexo.utils::plot.export("Internações por Malária anuais a cada 100k hab (2008 a 2019)",1, 9999)


nexo.utils::infoMunic %>% 
  left_join(anual_percapita %>% select(ibge6, Valor) %>% unique(), "ibge6") %>% 
  mutate(AmazLegal=ifelse(regionName=="Norte"|uf.x%in%c("MT","MA"),T,F)) %>% 
  group_by(AmazLegal) %>% summarise(sum(Valor, na.rm = T))


mensal %>%  filter(Ano %in% c(2008:2019)) %>% 
  group_by(AnoMes, Ano, Mes) %>% 
  summarise(Valor=sum(Valor)) %>% #calcula internações mensais de cada ano
  group_by(Mes) %>% 
  summarise(Valor=mean(Valor)) %>% #calcula media mensais para o periodo todo
  mutate(Mes=factor(Mes,levels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun","Jul","Ago", "Set","Out", "Nov", "Dez" ))) %>% 
  ggplot(aes(x=Mes, y=Valor))+geom_bar(stat="identity")
nexo.utils::plot.export("Internações por Malária medias por mes (2008 a 2019)",2, 9999)



mensal %>%  filter(Ano %in% c(2008:2019)) %>% 
  group_by(AnoMes, Ano, Mes) %>% 
  summarise(Valor=sum(Valor)) %>% #calcula internações mensais de cada ano
  group_by(Ano) %>% 
  summarise(Valor=sum(Valor)) %>% #calcula internações a cada ano
  ggplot(aes(x=Ano, y=Valor))+geom_bar(stat="identity")
nexo.utils::plot.export("Internações por Malária anuais (2008 a 2019)",3, 9999)




fx_etarias %>% 
  group_by(`Faixa Etária 1`) %>% 
  summarise(Valor=mean(Valor)) %>% 
  ggplot(aes(x=`Faixa Etária 1`, y=Valor))+geom_bar(stat="identity")

nexo.utils::plot.export("Internações por Malária por faixa etária (2008 a 2019)",4, 9999)



fx_etarias %>% 
  ggplot(aes(x=Ano, fill=`Faixa Etária 1`, y=Valor))+geom_bar(stat="identity", pos="fill") #ficou desinteressante


  

mort_fx_etarias %>% 
  ggplot(aes(x=`Faixa Etária 1`, y=Taxa_mortalidade))+geom_bar(stat="identity") 

nexo.utils::plot.export("Mortalidade da Malária por faixa etária (2008 a 2019)",5, 9999)



  
