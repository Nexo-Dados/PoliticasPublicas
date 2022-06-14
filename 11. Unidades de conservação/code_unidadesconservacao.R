setwd("1027. PP Dados Unidades de conservacao")

library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(ggrepel)

options(scipen=9999)

shpUC <- sf:: st_read("UNC_21_BR_CEM_V2.shp")

UCsemgeometria <- shpUC |> st_set_geometry(NULL) |> 
  mutate(BIOMA = factor(BIOMA, levels=c("AMAZONIA", "CERRADO", "MATA ATLANTICA", "CAATINGA", "PAMPA", "PANTANAL")))

shpSIMPLES <- sf::st_read("uc_simplificado.shp") |> 
  mutate(INSTANC = case_when(INSTANC == "NUNICIPAL" ~ "MUNICIPAL", TRUE ~ INSTANC))

UCsemgeometria <- arrange(UCsemgeometria, ANO)
UCsemgeometria[1,14] <-  1974

#Ver qtde total
UCsemgeometria |> count()

#Ver porcentagem instâncias
UCsemgeometria |> 
  mutate(INSTANC = case_when(INSTANC == "NUNICIPAL" ~ "MUNICIPAL",
                             TRUE ~ INSTANC)) |> 
  group_by(INSTANC) |> 
  summarise(porinst=sum(AREA_KM2)) |> 
  ungroup() |> 
  mutate(integral = sum(porinst)) |> 
  mutate(porc = round(porinst/integral, 3))

#Ver qte antes e depois do SNUC
UCsemgeometria |> 
  group_by(ANO) |> 
  summarise(soma = sum(AREA_KM2)) |> 
  arrange(ANO) |> 
  mutate(somatotal = cumsum(soma)) |> 
  filter(ANO == 2000 | ANO == 2019) |> 
  ungroup() |> 
  mutate(porcentagem = somatotal/(sum(somatotal)))

# Mapa --------------------------------------------------------------------


nexo.utils::mapState |> 
  ggplot()+
  geom_sf()+
  geom_sf(data=(shpSIMPLES |> filter(!is.na(RESTR))), col=0, aes(fill=RESTR))

nexo.utils::plot.export(title = "Unidades de conservação no Brasil", id=1, project=1027, subtitle="Por nível de restrição, em 2019")



# Bioma por ano -----------------------------------------------------------


UCsemgeometria |> 
  group_by(BIOMA, ANO) |> 
  summarise(SOMA = sum(AREA_KM2)) |> 
  ungroup() |> 
  complete(BIOMA, ANO) |> 
  mutate(SOMA = case_when(is.na(SOMA) ~ 0,
                           TRUE ~ SOMA)) |> 
  pivot_wider(names_from=BIOMA, values_from=SOMA) |>
  janitor::clean_names() |> 
  mutate(caatinga = cumsum(caatinga),
         amazonia = cumsum(amazonia),
         cerrado = cumsum(cerrado),
         mata_atlantica = cumsum(mata_atlantica),
         pampa = cumsum(pampa),
         pantanal = cumsum(pantanal)) |>
  pivot_longer(2:7, names_to="bioma", values_to="total") |> 
  mutate(bioma = factor(bioma, levels=c("amazonia", "cerrado", "mata_atlantica", "caatinga", "pampa", "pantanal"))) |> 
  ggplot(aes(x=ano, y=total, fill=bioma)) + 
  geom_area()+
  labs(y="Total, em km²")+
  annotate("curve", x = 1937, xend = 1940, y = 0, yend = 250000,
           arrow = arrow(length = unit(5, "pt")),curvature = -0.3,
           color = "grey40") +
  annotate("text", x = 1942, y = 250000, label = "Parque Nacional do\nItatiaia criado em 1937",
           lineheight = 0.8, color = "grey40", hjust=0) +
  annotate("curve", x = 2006, xend = 1985, y = 1328789.776, yend = 1200000,
           arrow = arrow(length = unit(5, "pt")),curvature = 0.2,
           color = "grey40")+
  annotate("text", x = 1985, y = 1100000, label = "Três das dez maiores\nUCs do Brasil foram\ncriadas em 2006",
           lineheight = 0.8, color = "grey40", hjust=1)+
  geom_vline(xintercept = 2000, alpha=0.5)+
  annotate("text", x = 1999, y = 1100000, label = "Criação do SNUC",
           lineheight = 0.8, color = "grey40", hjust=1, angle=90)
  

nexo.utils::plot.export(title = "Área total das unidades de conservação", id=2, project=1027, subtitle="Por bioma, de 1937 a 2019")

# Porcentagem protegida por bioma -----------------------------------------

mapbiomas <- read_excel("mapbiomas.xlsx") |> 
  select(2,47) |> 
  rename(TOTAL = `2020`) |> 
  na.omit() |> 
  group_by(biome) |> 
  summarise(TOTALBIOMA = sum(TOTAL)/100) |> 
  rename(BIOMA = biome) |> 
  mutate(BIOMA = factor(BIOMA, levels=c("AMAZÔNIA", "CERRADO", "MATA ATLÂNTICA", "CAATINGA", "PAMPA", "PANTANAL")))

mapbiomas$BIOMA <- stringi::stri_trans_general(mapbiomas$BIOMA, "Latin-ASCII")

UCsemgeometria |> 
  group_by(BIOMA) |> 
  summarise(SOMA = sum(AREA_KM2)) |> 
  merge(mapbiomas) |>
  mutate(Porc_protegida = SOMA/TOTALBIOMA) |> 
  select(1,4) |> 
  mutate(Total=1-Porc_protegida) |> 
  mutate(Protegida = Porc_protegida) |> 
  pivot_longer(2:3, values_to="Porcentagem", names_to="Status") |> 
  mutate(Status = factor(Status, levels=c("Total", "Porc_protegida"))) |> 
  ggplot(aes(x=Porcentagem, y=reorder(BIOMA, Protegida), fill=Status)) + 
  geom_col()+
  labs(y="Bioma")+
  geom_text(aes(x=0.5, label=paste0(round(Protegida*100,1), "%")))
  
nexo.utils::plot.export(title = "Área de unidades de conservação", id=3, project=1027, subtitle="Em cada bioma, em 2019")



# Colunas por instância ---------------------------------------------------

UCsemgeometria |>   
  mutate(INSTANC = case_when(INSTANC =="NUNICIPAL" ~ "MUNICIPAL",
                             TRUE ~ INSTANC)) |> 
  group_by(INSTANC, ANO) |> 
  summarise(SOMA = sum(AREA_KM2)) |> 
  ggplot(aes(x=ANO, y=SOMA, fill=INSTANC)) +
  geom_col()

UCsemgeometria |>   
  mutate(INSTANC = case_when(INSTANC =="NUNICIPAL" ~ "MUNICIPAL",
                             TRUE ~ INSTANC)) |> 
  group_by(INSTANC) |> 
  summarise(SOMA = sum(AREA_KM2)) |> 
  ggplot(aes(x=SOMA, y=reorder(INSTANC, SOMA), fill=INSTANC))+
  geom_col()+
  geom_text(aes(x=400000, y=INSTANC, label=paste0(round(SOMA/1000,1), " mil km²")))+
  labs(x="Área em km²", y= "Instância")

nexo.utils::plot.export(title = "Área de unidades de conservação", id=4, project=1027, subtitle="Por instância, em 2019")


# Ranking de maiores UCs --------------------------------------------------

UCsemgeometria |> 
  top_n(n=5, wt = AREA_KM2)

nexo.utils::mapState |> 
  filter(uf == "PA" | uf == "AP") |> 
  ggplot()+
  geom_sf()+
  geom_sf(data= (shpUC |> top_n(n=5, wt = AREA_KM2)),
          col=0, aes(fill=NOM_UC))

nexo.utils::plot.export(title = "Maiores unidades de conservação do Brasil", id=5, project=1027, subtitle="Em 2019")


# Contraste por bioma -----------------------------------------------------

porbioma_mapbiomas <- mapbiomas |> 
  mutate(porcent = round((TOTALBIOMA/sum(TOTALBIOMA)*100),1)) |> 
  mutate(grupo = "Área total")

porbiomas_uc <- UCsemgeometria |> 
  group_by(BIOMA) |> 
  summarise(TOTALBIOMA = sum(AREA_KM2)) |> 
  mutate(porcent = round((TOTALBIOMA/sum(TOTALBIOMA)*100),1)) |> 
  mutate(grupo = "Unidades de conservação")


rbind(porbioma_mapbiomas, porbiomas_uc) |> 
  mutate(BIOMA = factor(BIOMA, levels=c("AMAZONIA", "CERRADO", "MATA ATLANTICA", "CAATINGA", "PAMPA", "PANTANAL"))) |> 
  ggplot(aes(x=grupo, y=porcent, fill=BIOMA, width=0.3)) +
  geom_col()+
  geom_text(aes(x = grupo, y = porcent, label = paste0(porcent, "%"), group = BIOMA),
            position = position_stack())

nexo.utils::plot.export(title = "Área dos biomas brasileiros", id=6, project=1027, subtitle="Total e dentro de unidades de conservação")
