## 02. Doenças comunicáveis
## Autor: Gabriel Maia, Nexo Jornal
## Contato: dados@nexojornal.com.br

## pacote com as funcoes do nexo
library(nexo.utils)
## caso nao tenha instalado
## devtools::install_github('Nexo-Dados/nexo.utils')

# Bibliotecas e importação de dados ---------------------------------------


library(patchwork)
library(tidyverse)
library(gganimate)


#--
   #Junta doenças neonatais, maternais e nutricionais às comunicáveis (Metodologia do ISPS)
gbd<-read.csv('dados/brazil_gbd_1990_2017_gender_age.csv',encoding='UTF-8')%>%
  select(-c('measure_id','measure_name','cause_id','metric_id','metric_name'))%>%
  mutate(cause_name=ifelse(cause_name=='Communicable, maternal, neonatal, and nutritional diseases','Communicable diseases*',as.character(cause_name)))



 #Cria coluna com Total e porcentage de mortes para cada location, sex, year, e age
gbd_perc<-gbd%>%
  filter(cause_name!='All causes')%>%
  left_join(gbd%>%filter(cause_name=='All causes'),
            by=c('location_id','location_name','sex_name','sex_id','year','age_name','age_id'),
            suffix=c('','_total'))%>%
  mutate(perc_total=val/val_total)

#importa mapa dos estados brasileiros
sf::read_sf("dados/simple_state/UFEBRASIL.shp") %>% mutate(NM_ESTADO=tolower(NM_ESTADO),
                                                                              NM_ESTADO=ifelse(NM_ESTADO=="espirito santo", "espírito santo",NM_ESTADO))->map
#importa população do brasil ano a ano (banco mundial)
pop<- read_csv("dados/pop_e_gdp.csv") %>% filter(`Country Code`=="BRA") %>% select(Year, Pop)


# Vizualizações -----------------------------------------------------------


#faz o grafico de variação do percentual das causas de morte ao longo do tempo no brasil
gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name!='All causes'&sex_name=='Both'&age_name=='All Ages') %>% 
  mutate(age_name=as_factor(age_name)) %>% 
  ggplot(aes(x=year, y=perc_total, fill=cause_name))+
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#FF7B80","#484848","#BCB3D9"))

nexo.utils::plot.export("variação do percentual das causas de morte ao longo do tempo",
                       1, 721)



#faz o grafico de diferença do percentual de mortes por doen. comun. entre 1990 e 2017 por fx etaria no brasil
gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name=='Communicable diseases*'&sex_name=='Both'&age_name!='All Ages'&year%in%c(1990,2017)) %>% 
  mutate(age_name=as_factor(age_name)) %>% select(year,age_name, perc_total) %>% 
  spread(year, perc_total) %>% 
  ggplot(aes(x=age_name, y=`2017`-`1990`, fill=ifelse(`2017`-`1990`>=0, F,T)))+
  geom_bar(stat="identity")

nexo.utils::plot.export("diferença do percentual de mortes por doen. comun. entre 1990 e 2017 por fx etaria",
                       2, 721)

#faz o grafico de variação das causas de morte ao longo do tempo per capita no brasil
gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name!='All causes'&sex_name=='Both'&age_name=='All Ages') %>% 
  mutate(age_name=as_factor(age_name)) %>% 
  left_join(pop, c("year"="Year")) %>% 
  mutate(percapita=100000*val/Pop) %>% 
  ggplot(aes(x=year, y=percapita, col=cause_name))+
  geom_line()+
  scale_color_manual(values = c("#FF7B80","#484848","#BCB3D9"))

nexo.utils::plot.export("variação das causas de morte ao longo do tempo por 100k hab",
                       3.1, 721)


#faz o grafico de variação percentual relativa a 1990 das causas de morte ao longo do tempo no brasil
gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name!='All causes'&sex_name=='Both'&age_name=='All Ages') %>% 
  select(cause_name, year, val, perc_total) %>% 
  group_by(cause_name) %>% 
  arrange(year) %>%
  mutate(base=first(val), delta=100*(val/base))%>% 
  ggplot(aes(x=year, y=delta, col=cause_name))+
  geom_line()+
  scale_color_manual(values = c("#FF7B80","#484848","#BCB3D9"))

nexo.utils::plot.export("variação percentual relativa a 1990 das causas de morte ao longo do tempo",
                       3.2, 721)


#faz o grafico variação do valor total das causas de morte ao longo do tempo no brasil
gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name!='All causes'&sex_name=='Both'&age_name=='All Ages') %>% 
  mutate(age_name=as_factor(age_name)) %>% 
  ggplot(aes(x=year, y=val, col=cause_name))+
  geom_line()+
  scale_color_manual(values = c("#FF7B80","#484848","#BCB3D9"))

nexo.utils::plot.export("variação do valor total das causas de morte ao longo do tempo",
                       3, 721)




#setup para os mapas
scale<-scale_fill_manual(values=rev(c("#5A4D85","#7866B2","#9A8CC5","#BCB3D9","#DDD9EC")), drop=F)
cuts<-c(0,10,15,25,35,50)

gbd_perc %>% 
  mutate(location_name=tolower(location_name),location_name=ifelse(location_name=="piaui", "piauí",location_name)) %>% 
  filter(location_name%in%c(map$NM_ESTADO)&cause_name=='Communicable diseases*'&sex_name=='Both'&age_name=='All Ages'&year==1990) %>% 
  right_join(map, c("location_name"="NM_ESTADO")) %>% sf::st_as_sf() %>% 
  mutate(perc_total=cut(perc_total, cuts/100)) %>% 
  ggplot(aes(fill=perc_total))+
  theme_void()+
  theme(legend.position = "none",panel.background = element_blank())+
  geom_sf()+scale->a

gbd_perc %>% 
  mutate(location_name=tolower(location_name),location_name=ifelse(location_name=="piaui", "piauí",location_name)) %>% 
  filter(location_name%in%c(map$NM_ESTADO)&cause_name=='Communicable diseases*'&sex_name=='Both'&age_name=='All Ages'&year==2017) %>% 
  right_join(map, c("location_name"="NM_ESTADO")) %>% sf::st_as_sf() %>% 
  mutate(perc_total=cut(perc_total, cuts/100)) %>% 
  ggplot(aes(fill=perc_total))+
  theme_void()+
  theme(panel.background = element_blank())+
  geom_sf()+scale->b

a+b #plota os mapas usando patchwork

nexo.utils::plot.export("percentual de mortes por doen. comun. 2017", theme = theme_void(),
                       4, 721)


# gifs, acabaram não sendo utilizados --------------------------------------------------------------------


gbd_perc %>% 
  mutate(location_name=tolower(location_name),location_name=ifelse(location_name=="piaui", "piauí",location_name)) %>% 
  filter(location_name%in%c(map$NM_ESTADO)&cause_name=='Communicable diseases*'&sex_name=='Both'&age_name=='All Ages') %>% 
  right_join(map, c("location_name"="NM_ESTADO")) %>% sf::st_as_sf() %>% 
  ggplot(aes(fill=perc_total))+
  geom_sf()+
  transition_time(year)+
  ease_aes('linear')->anim

animate(anim, fps=19, nframes=50,type = "cairo-png",width = 700, height= 700,res=200,
        antialias = "subpixel",end_pause=25, renderer = gifski_renderer())
anim_save("teste_mapa.gif")


gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name!='All causes'&sex_name=='Both'&age_name!='All Ages') %>% 
  mutate(age_name=as_factor(age_name)) %>% 
  ggplot(aes(x=age_name, y=perc_total, fill=cause_name))+
  geom_bar(stat="identity")+
  labs(title = 'Data: {frame_time}')+
    transition_time(year)+
  ease_aes('linear')+
  theme(legend.position = "none",panel.background = element_blank(),plot.background = element_rect(fill = "#EEF3F9"),
        panel.grid = element_blank() )+
  scale_fill_manual(values = c("#FF7B80","#484848","#BCB3D9"))->anim

animate(anim, fps=19, nframes=100,type = "cairo-png",width = 700, height= 700,res=200,
        antialias = "subpixel",end_pause=25, renderer = gifski_renderer())
anim_save("teste_faixa_etarias.gif")





gbd_perc %>% 
  filter(location_name=="Brazil"&cause_name!='All causes'&sex_name=='Both'&age_name=='All Ages') %>% 
  mutate(age_name=as_factor(age_name)) %>% 
  ggplot(aes(x=year, y=perc_total, fill=cause_name))+
  geom_bar(stat="identity")+
  labs(title = 'Data: {frame_time}')+
  transition_time(year)+
  ease_aes('linear')+
  theme(legend.position = "none")+shadow_mark()+
  enter_fade()+
  theme(legend.position = "none",panel.background = element_blank(),plot.background = element_rect(fill = "#EEF3F9"),
        panel.grid = element_blank(), panel.grid.major.y = element_line(color="#DDD9EC"))+
  scale_fill_manual(values = c("#FF7B80","#484848","#BCB3D9"))->anim

animate(anim, fps=19, nframes=200,type = "cairo-png",width = 700, height= 700,res=200,
        antialias = "subpixel",end_pause=25, renderer = gifski_renderer())
anim_save("teste_1_animado.gif")





#:)

