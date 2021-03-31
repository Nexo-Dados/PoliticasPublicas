library(tidyverse)
setwd("~/Nexo/Graficos/849. Polinização")


dados<- readxl::read_excel("Tabela_para_figura.xlsx")


dados %>% mutate(animais=strsplit(`grupos funcionais_pp`, ", ")) %>% 
  mutate(order=lengths(animais)) %>% 
  unnest(c(animais)) %>% 
  mutate(animais=str_replace_all(animais, "[^[:alnum:]]","")) %>% 
  mutate(classe=case_when(depend=="pouco"~"1. 0 a 10%",
                                   depend=="modesto"~"2. 10 a 40%",
                                   depend=="alto"~"3. 40 a 90%",
                                   depend=="essencial"~"4. mais de 90%")) %>% 
  ggplot(aes(x=animais, y=reorder(cultura,order), fill=classe))+
    geom_tile()+
  scale_fill_viridis_d()

nexo.utils::plot.export('Animais resposnsáveis por polinizar cada cultivo', 1, 849)




dados %>% mutate(animais=strsplit(`grupos funcionais_pp`, ", ")) %>% 
  mutate(order=lengths(animais)) %>% 
  unnest(c(animais)) %>% 
  mutate(animais=str_replace_all(animais, "[^[:alnum:]]","")) %>% 
  mutate(classe=case_when(depend=="pouco"~"1. 0 a 10%",
                          depend=="modesto"~"2. 10 a 40%",
                          depend=="alto"~"3. 40 a 90%",
                          depend=="essencial"~"4. mais de 90%")) %>%
  arrange(cultura) %>% 
  mutate(cultura=as_factor(cultura), cultura=fct_rev(cultura)) %>% 
  ggplot(aes(x=animais, y=cultura, fill=classe))+
  geom_tile()+
  scale_fill_viridis_d()+facet_wrap(~classe, scales="free_y")
nexo.utils::plot.export('Animais resposnsáveis por polinizar cada cultivo', 1.1, 849)

faixas<-c(0,10,40,90,100)

dados %>% mutate(start=case_when(depend=="pouco"~.0,
                                 depend=="modesto"~.10,
                                 depend=="alto"~.40,
                                 depend=="essencial"~.90)) %>% 
  mutate(end=case_when(depend=="pouco"~.10,
                       depend=="modesto"~.40,
                       depend=="alto"~.90,
                       depend=="essencial"~1)) %>% 
  mutate(val_produção=val_produção/1000000000) %>% 
  mutate(start=start*val_produção,end=end*val_produção) %>% 
  mutate(rank=rank(-val_produção)) %>% 
  filter(rank<=10) %>% 
  ggplot(aes(x=reorder(cultura,val_produção)))+
  geom_bar(stat="identity", aes(y=val_produção),alpha=0.4,fill="red")+
  geom_bar(stat="identity", aes(y=end),alpha=0.4, fill="blue")+
  geom_bar(stat="identity", aes(y=start))+
  coord_flip()

nexo.utils::plot.export('Valor adcionado pelos polinizadores em cada cultura', 2.1, 849)

dados %>% mutate(start=case_when(depend=="pouco"~.0,
                                 depend=="modesto"~.10,
                                 depend=="alto"~.40,
                                 depend=="essencial"~.90)) %>% 
  mutate(end=case_when(depend=="pouco"~.10,
                       depend=="modesto"~.40,
                       depend=="alto"~.90,
                       depend=="essencial"~1)) %>% 
  mutate(val_produção=val_produção/1000000000) %>% 
  mutate(start=start*val_produção,end=end*val_produção) %>% 
  mutate(rank=rank(-val_produção)) %>% 
  filter(rank<=26,rank>6) %>% 
  ggplot(aes(x=reorder(cultura,val_produção)))+
  geom_bar(stat="identity", aes(y=val_produção),alpha=0.4,fill="red")+
  geom_bar(stat="identity", aes(y=end),alpha=0.4, fill="blue")+
  geom_bar(stat="identity", aes(y=start))+
  coord_flip()
nexo.utils::plot.export('Valor adcionado pelos polinizadores em cada cultura', 2.2, 849)


dados %>%   
  mutate(animal=ifelse(str_detect(`grupos funcionais_pp`,pattern = ", "),"diversos", `grupos funcionais_pp`)) %>% 
  mutate(animal=str_replace_all(animal, "[^[:alnum:]]","")) %>% 
  ggplot(aes(area=porcent_valorpolinização,group=cultura, fill=animal))+
  treemapify::geom_treemap()+
  treemapify::geom_treemap_text(aes(label=paste0(cultura, "\n", animal,"\n",prettyNum(round(porcent_valorpolinização,1),decimal.mark = ","),"%")))
nexo.utils::plot.export('Proporção do valor adcionado pelos polinizadores em cada cultura', 3, 849)

