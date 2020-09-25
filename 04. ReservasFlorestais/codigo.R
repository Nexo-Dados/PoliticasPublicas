setwd("~/Nexo/NexoPP/0007. Reservas Florestais")

library(tidyverse)
library(sf)

#unzip("es_sp_cfmodel2018_nexo.zip", exdir = "shapefile") #descomprime os arquivos grandes e cria a pasta "shapefile" 


df <- foreign::read.dbf("shapefile/es_sp_cfmodel2018_nexo.dbf") #importa apenas os atributos

read_sf("shapefile/es_sp_cfmodel2018_nexo.shp")->mapa #importa shapefile e atributos 


df %>% 
  mutate(status=case_when((def_app==0 & def_rl==0) ~ "1.Regularizada",
                          (def_app!=0 & def_rl==0) ~ "2.Irregular devido a APP",
                          (def_app==0 & def_rl!=0) ~ "3.Irregular devido a RL",
                          (def_app!=0 & def_rl!=0) ~ "4.Irregular em ambos",
                          T~"ERRO")) %>% 
  group_by(status) %>% 
  summarise(n=n())%>% 
  mutate(status=paste0(status, ", n=",n)) %>% 
  ggplot(aes(y=n,x=1, fill=status))+
  geom_bar(stat="identity")+coord_flip()

nexo.utils::plot.export("Numero de propiedades no estado de SP por status de acordo com o Código Florestal", 1, "04",subtitle = paste0("total = ", nrow(df)))
  


#Viz alternativa do gráfico anterior
df %>% 
  mutate(status=case_when((def_app==0 & def_rl==0) ~ "1.Regularizada",
                          (def_app!=0 & def_rl==0) ~ "2.Irregular devido a APP",
                          (def_app==0 & def_rl!=0) ~ "3.Irregular devido a RL",
                          (def_app!=0 & def_rl!=0) ~ "4.Irregular em ambos",
                          T~"ERRO")) %>% 
  group_by(status) %>% 
  summarise(n=n())%>% 
  mutate(status=paste0(status, ", n=",n)) %>% 
  ggplot(aes(area = n, 
           label = paste0(prettyNum(round(n/10^3,1),decimal.mark = ","), " mil"),fill=status)) +
  treemapify::geom_treemap(start="topleft") +
  treemapify::geom_treemap_text(start="topleft",grow = T) 

nexo.utils::plot.export("Numero de propiedades no estado de SP por status de acordo com o Código Florestal", 1.1, "04",subtitle = paste0("total = ", nrow(df)))



df %>% 
  mutate(status=case_when((def_app==0 & def_rl==0) ~ "1.Regularizada",
                          (def_app!=0 & def_rl==0) ~ "2.Irregular devido a APP",
                          (def_app==0 & def_rl!=0) ~ "3.Irregular devido a RL",
                          (def_app!=0 & def_rl!=0) ~ "4.Irregular em ambos",
                          T~"ERRO")) %>% 
  group_by(status) %>% 
  summarise(n=sum(areaproc))%>% 
  mutate(status=paste0(status, ", ",round(n/1000000,1), " milhões de ha")) %>% 
  ggplot(aes(y=n,x=1, fill=status))+
  geom_bar(stat="identity")+coord_flip()

nexo.utils::plot.export("Área total das propiedades no estado de SP por status de acordo com o Código Florestal", 2, "04",
                      subtitle = paste0("total = ", round((sum(df$areaproc)/10^6),1),  " milhões de ha"))



df %>% 
  filter(areaproc>1) %>% 
  mutate(status=case_when((def_app==0 & def_rl==0) ~ "1.Regularizada",
                          T~"2.Com Déficit")) %>% 
  filter(status=="2.Com Déficit") %>% 
  group_by(status, tamanho_mf) %>% 
  summarise(n=sum(def_app)+sum(def_rl), count=n())%>% 
  mutate(status2=paste0(status, ", ",round(n/1000000,1), " milhões de ha")) %>% 
ggplot(aes(y = n, x=status, 
           label = paste0(prettyNum(round(n/10^3,1),decimal.mark = ","), " mil hectares\nem ",
                          prettyNum(
                            count,  big.mark= "."), " propriedades"),fill=tamanho_mf)) +
  geom_bar(stat="identity")+
  geom_text(stat="identity", position="stack",aes(y=n*0.6))+
  scale_fill_manual(values =rev(c("#5A4D85","#7866B2","#BCB3D9")))

nexo.utils::plot.export("Deficit Florestal das propiedades no estado de SP por tamanho do lote", 3, "04")



df %>% 
  filter(areaproc>1) %>% 
  ggplot(aes(x=areaproc))+
  geom_histogram()+
  facet_wrap(~tamanho_mf,ncol = 1, scales = "free_y")+
  scale_x_log10(breaks=10**c(1:5))
  

#Os mapas da matéria foram produzidos utilizando o software gratuito de geoprocessamento "QGIS"
#O projeto e os dados utilizados se encontram nesta mesma pasta e podem ser explorados com a instalação do programa


