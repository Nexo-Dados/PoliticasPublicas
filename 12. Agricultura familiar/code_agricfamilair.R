setwd("1136. PP Dados - Agricultura familiar BPBES")

library(tidyverse)
library(sf)
library(viridis)

dados <- readxl::read_excel("dados.xlsx")



# Perfil das "duas agriculturas" ------------------------------------------

dados |> 
  group_by(`Grupo social`) |> 
  summarise(Mil_reais = sum(`Valor_mil reais`)) |> ungroup() |> 
  mutate(Porc = Mil_reais*100/sum(Mil_reais))


dados |> 
  group_by(`Grupo agrícola`, `Grupo social`) |> 
  summarise(Mil_reais = sum(`Valor_mil reais`)) |> ungroup() |> group_by(`Grupo social`) |> 
  mutate(Porc = Mil_reais*100/sum(Mil_reais),
         Label = cumsum(Porc) - Porc/2,
         `Grupo agrícola` = fct_rev(`Grupo agrícola`)) |> 
  ggplot(aes(y=`Grupo social`))+
  geom_col(aes(x=Porc, fill=`Grupo agrícola`), width=0.2)+
  geom_text(aes(x=Label, label=round(Porc, 1)))


  
  
# Por grupo agrícola ------------------------------------------------------

dados |> 
  group_by(`Grupo agrícola`, `Grupo social`) |> 
  summarise(Mil_reais = sum(`Valor_mil reais`)) |> ungroup() |> 
  pivot_wider(names_from=`Grupo social`, values_from=Mil_reais) |> 
  mutate(Ordem = `Agricultura familiar`*100/(`Agricultura familiar` + `Agricultura não familiar`)) |> 
  pivot_longer(2:3, names_to="Grupo social", values_to="Mil_reais") |> 
  group_by(`Grupo agrícola`) |> 
  mutate(Porc = Mil_reais*100/sum(Mil_reais),
         Label = cumsum(Porc) - Porc/2,
         `Grupo social` = fct_rev(`Grupo social`)) |> ungroup() |> 
  ggplot(aes(y=reorder(`Grupo agrícola`,Ordem)))+
  geom_col(aes(x=Porc, fill=`Grupo social`))+
  geom_text(aes(x=Label, label=round(Porc, 1)))+
  labs(x="Percentual do faturamento", y="Grupo agrícola")




# Gráfico por estado: familiar versus nao familiar ------------------------

familiar <- dados |> 
  group_by(UF, `Grupo social`) |> 
  summarise(Mil_reais = sum(`Valor_mil reais`)) |> ungroup() |> 
  group_by(UF) |> mutate(Porc = Mil_reais*100/sum(Mil_reais)) |> ungroup() |> 
  select(-Mil_reais) |> 
  pivot_wider(names_from=`Grupo social`, values_from=Porc) |> 
  mutate(Ordem = `Agricultura familiar`) |> 
  left_join((nexo.utils::infoState |> select(uf, state)), by=c("UF" = "state"))

nexo.utils::mapState |> 
  left_join(familiar, by=c("uf")) |> 
  mutate(Divisao = cut(`Agricultura familiar`, breaks=c(-Inf, 10, 25, 50, 70,+Inf),
                       labels=c("Até 10%", "De 10 a 25%", "De 25 a 50%", "De 50 a 70%", "Mais de 70%"))) |> 
  ggplot()+
  geom_sf(aes(fill=Divisao))+
  scale_fill_manual(values=rev(viridis(5)))




# Gráfico por estado da agricultura familiar ------------------------------

options(scipen=9999)

dados |> 
  filter(`Grupo social` == "Agricultura familiar") |> 
  group_by(UF) |> mutate(Ordem = sum(`Valor_mil reais`)) |> ungroup() |> 
  ggplot(aes(y=reorder(UF, Ordem)))+
  geom_col(aes(x=`Valor_mil reais`/10^6, fill=`Grupo agrícola`))+
  labs(x="Bilhões de reais", y="UF")



# Por estado --------------------------------------------------------------

nexo.utils::mapState |> left_join(
(dados |> 
  left_join(nexo.utils::infoState, by=c("UF" = "state"))), by=c("uf")) |> 
  filter(`Grupo social` == "Agricultura familiar") |> 
  select(uf, `Grupo agrícola`, `Valor_mil reais`, geometry)
