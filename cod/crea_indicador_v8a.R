
# cambio: incluimos VGE.
# cambios v7: cambiamos el indicador mensual de la VGE ponderado seg?n la CNTR por el que
#              la AEAT (13-08-2020).
# cambios v7b: solo cambiamos las salidas. El m?todo de calculo es el mismo.
# cambios v7c: desestacionalizamos las series diarias antes de desagregar
# cambios v7d: desagregamos la afiliaci?n y reorganizamos el c?digo para poner tareas en funciones.
# cambios v7e: incorporamos los ertes del RD 30/20.
# cambios v8a: cambiamos la afiliaci?n neta a no cvec - erte y desestacionalizamos despu?s

library("lubridate")
library("openxlsx")
library("ggplot2")
library("zoo")
library("dplyr")
library("tidyr")
library("readr")
library("gridExtra")
#install.packages("tempdisagg")
library("tempdisagg")
library("tseries")
library("forecast")
library("wrapr")

#setwd(getwd())
setwd("T:/UAM/COMUN/PRODUCCION/indicador_diario/cod")

source("T:/UAM/COMUN/SOFTWARE/R/UTILIDADES/LeerGW.R")
source("funciones_indicador_v2.R")
source("desest.R")

#################################################
#################  opciones #####################
#################################################

op <- list()

op$mes_base_1 <- as.Date("2020-03-01")
op$mes_base_2 <- as.Date("2020-03-31")

#################################################
######### LECTURA INDICADORES DIARIOS ###########
#################################################

# series por CCAA

# para el testeo del lanzamiento cargo esto en un dataframe, porque tarda bastante en ejecutarse 
afil_serie_diaria <- lectura_afiliacion()
#load("afil_serie_diaria_testing.Rda")


aux <- read.xlsx("../data/ertes.xlsx", detectDates = TRUE)
aux %>% select(fecha, ERTE) %>%
   pivot_longer(cols  = c("ERTE"), names_to = "indicador", values_to = "valor") -> ertes_serie_diaria

tarjetas_serie_diaria <- lectura_tarjetas()

# series España

tarjetas_total_diario <- lectura_tarjetas_total() # este lo deberíamos eliminar

to[
   vge_serie_diaria <- vge_serie_diaria,
   pesos_vge <- pesos_vge
   ] <- lectura_vge()

 # vge_serie_diaria %>% conv_indice(op$mes_base_1,  op$mes_base_2, "id_seccion") %>% 
 #    inner_join(pesos_vge, by = "id_seccion") %>% group_by(fecha) %>%
 #    summarize(valor = weighted.mean(valor, cntr, na.rm = TRUE)) %>% ungroup() %>%
 #    calend() %>% mutate(indicador = "VGE") -> vge_serie_diaria

 vge_serie_diaria %>% conv_indice(op$mes_base_1,  op$mes_base_2, "id_seccion") %>% 
    inner_join(pesos_vge, by = "id_seccion") %>% group_by(fecha) %>%
    summarize(valor = sum(valor, na.rm = TRUE)) %>% ungroup() %>%
    calend() %>% mutate(indicador = "VGE") -> vge_serie_diaria
 
elec_serie_diaria <- lectura_elec() 

# crear conjunto indicadores Espa?a (brutos) agregando los que est?n por CCAA.

afil_serie_diaria %>% bind_rows(ertes_serie_diaria) %>% #bind_rows(ertes_serie_diaria_rd3020) %>%
   bind_rows(tarjetas_serie_diaria) %>%
   bind_rows(vge_serie_diaria) %>% group_by(fecha, indicador) %>%
   summarize(valor = sum(valor, na.rm = TRUE))  %>% bind_rows(elec_serie_diaria) -> indic_diarios_esp

indic_diarios_esp %>% group_by(indicador) %>% summarize(ult = max(fecha)) -> ultfechas

# los desestacionaliza

selec <- setdiff(unique(indic_diarios_esp$indicador), c("AFI", "ERTE")) # desestacionalizamos los dem?s


indic_diarios_esp_desest <- desest_indi(indic_diarios_esp, id = selec) 


#################################################
######## LECTURA INDICADORES MENSUALES ##########
#################################################

to[
   indic_mensuales_esp_desest <- df_indicadores,
   matriz.indicadores <- matriz.indicadores
   ] <- lectura_mensuales(
      cods = c("190000m.d", "272701g.d", "249020cvec", "229000cc.d", "25m000"),
   noms = c("AFIL", "ICM","IASS", "IPI", "VGE"),
   defs = c(NA, NA, "RE400071", NA, NA))

# a?adir previsi?n SS

dif_afil <- as.numeric(readline("Predicción de SS. Diferencia en miles: "))

if (!is.na(dif_afil)){

   ra <- matriz.indicadores[nrow(matriz.indicadores),]
   ra$PERIODO <- ra$PERIODO +1/12
   ra$AFIL <- matriz.indicadores$AFIL[nrow(matriz.indicadores)] + dif_afil
   ra[,-c(1,2)] <- NA
   matriz.indicadores <- bind_rows(matriz.indicadores, ra)
   
   ra %>% pivot_longer(cols = c("AFIL", "ICM", "IASS", "IPI", "VGE"), names_to = "indicador",
                       values_to = "valor") -> ra_long
   
   indic_mensuales_esp_desest <-  bind_rows(indic_mensuales_esp_desest, ra_long)
   
}


######### desagrega ###################

relacion <- cbind(c("TCO", "TOS", "ELE", "VGE", "AFI"),
                  c("ICM", "IASS", "IPI", "VGE", "AFIL"))

indic_diarios_esp_desag <- data.frame()

for (ind in 1:nrow(relacion)){
   
   desag(indic_diarios_esp_desest, indic_mensuales_esp_desest, relacion[ind,1], relacion[ind,2]) %>%
      mutate(indicador = relacion[ind,2]) %>% rename(fecha = time) -> aux
   
   indic_diarios_esp_desag <- rbind(indic_diarios_esp_desag, aux)
   
}

############ Ajuste de la afiliaci?n con los ERTE ###################

indic_diarios_esp_desest %>% filter(indicador == "ERTE") -> ertes

ertes %>%  mutate(indicador = "AFIL") %>% rename(correccion = valor) -> ertes_aux   

indic_diarios_esp_desag %>% filter(indicador == "AFIL") %>%
   mutate(indicador = "AFILB") -> afil_bruta   


max_fecha_erte <- max(ertes_aux$fecha) # evitamos usar valores de afiliaci?n m?s all? de los ertes

indic_diarios_esp_desag %>% left_join(ertes_aux, by = c("fecha", "indicador")) %>%
   mutate(valor = ifelse(indicador == "AFIL" & !is.na(correccion), valor - correccion/1e3, valor)) %>% 
   filter(!(indicador == "AFIL" & fecha >max_fecha_erte)) ->
   indic_diarios_esp_desag

indic_auxiliares <- bind_rows(afil_bruta, ertes) # ertes y afiliaci?n bruta para ajustar luego
   
############ Convertimos en ?ndices ###################

indic_diarios_esp_desag_indice <-
   conv_indice(indic_diarios_esp_desag, op$mes_base_1, op$mes_base_2)

#################################################
############ CÁLCULO PONDERACIONES ##############
#################################################

pib <- LeerGW("940000d",NOMBRES = "pib", TIPO = "T")

to[
   coef <- coef,
   covcoef <- covcoef,
   intercept <- intercept,
   sigma_eps <- sigma
   ] <- ponde(pib, matriz.indicadores)
   

#################################################
################# AGREGACION ####################
#################################################

intercept_d <- intercept*4/365.24

indic_diarios_esp_desag_indice %>% inner_join(coef, by = "indicador") %>%
   mutate(prod = valor * peso)  %>% group_by(fecha) %>%
   summarize(ind_act = sum(prod), cuenta = n()) %>%
   mutate(ind_act = intercept_d * as.numeric(fecha - as.Date("2020-03-31")) +ind_act)  %>%
   dplyr::filter(cuenta == 5) %>% filter(fecha >= as.Date("2020-01-01")) %>%
   select(-"cuenta") -> datos_act

############ ajuste nivel ###############

datos_act %>% filter(fecha >= as.Date("2020-01-01") & fecha <= as.Date("2020-03-31")) %>%
   `$`("ind_act") %>% mean() -> mediaQ12020

offset <- pib$PIB[pib$PERIODO == yearqtr(2020)] - mediaQ12020

datos_act$ind_act <- datos_act$ind_act + offset

#### predicci?n ####

datos_act_for <- pred_indi(datos_act, 14, 7)

##############################################
############################################## 
###########        SALIDAS         ###########
##############################################
##############################################

#### gr?fico indicador con pred ####

datos_act %>% 
   ggplot(aes(x= fecha, y = ind_act)) + geom_line(color = "#F8766D", size = 1)+
   scale_x_date(date_breaks = "months", date_labels = "%b") +
   xlab("") + ylab("indicador diario") +
   ggtitle(paste0("actualizado ", format(Sys.time(), "%d/%m/%Y"))) +
      theme_bw(base_size=16) +
   theme(plot.title = element_text(size= 8)) +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         legend.position = "bottom", legend.title = element_blank(),
         legend.key = element_rect(fill = "transparent",colour = NA),
         legend.background = element_rect(fill = "transparent",colour = NA),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA))  -> gr_diario

ggsave("../out/ind_diario.png",
       plot = gr_diario, width = 18, height = 12,bg = "transparent",
       units = "cm", dpi = 500)

#### gr?fico indicador con pred ####

datos_act_for %>% 
   mutate(tipo = ifelse(tipo == "obs", "observado", "predicción")) %>% 
   ggplot(aes(x= fecha, y = ind_act)) + geom_line(aes(color = tipo), size = 1)+
   scale_x_date(date_breaks = "months", date_labels = "%m-%d") +
   xlab("") + ylab("indicador diario") +
   ggtitle(paste0("actualizado ", format(Sys.time(), "%d/%m/%Y"))) +
   theme_bw(base_size=16)  +
   theme(plot.title = element_text(size= 8))+
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         legend.position = "bottom", legend.title = element_blank(),
         legend.key = element_rect(fill = "transparent",colour = NA),
         legend.background = element_rect(fill = "transparent",colour = NA),
         panel.background = element_rect(fill = "transparent",colour = NA),
         plot.background = element_rect(fill = "transparent",colour = NA))  -> gr_diario

ggsave("../out/ind_diario_pred.png",
       plot = gr_diario, width = 18, height = 12,bg = "transparent",
       units = "cm", dpi = 500)

#### series diarias mensualizadas ####

mensualizar(indic_diarios_esp_desag_indice) %>% ungroup() %>% 
   pivot_wider(id_cols = c("year", "month"),names_from = "indicador", values_from = "valor") %>%
    mutate(fecha = yearmon(year + (month-1)/12)) %>% select(-c("year", "month")) %>%
   `[`(c("fecha", "AFIL", "ICM", "IPI", "VGE", "IASS")) ->
   indic_diarios_esp_desag_indice_mes

mensualizar(indic_auxiliares) %>% ungroup() %>% 
   pivot_wider(id_cols = c("year", "month"),names_from = "indicador", values_from = "valor") %>%
   mutate(fecha = yearmon(year + (month-1)/12)) %>% select(-c("year", "month")) %>%
   `[`(c("fecha", "AFILB", "ERTE")) ->
   indic_auxiliares_mes

mensualizar(indic_diarios_esp) %>% ungroup() %>% 
   pivot_wider(id_cols = c("year", "month"),names_from = "indicador", values_from = "valor") %>%
   mutate(fecha = yearmon(year + (month-1)/12)) %>% select(-c("year", "month")) %>%
   `[`( c("fecha", "AFI", "TCO", "ELE", "VGE", "TOS")) -> indic_diarios_esp_mes

mensualizar(indic_diarios_esp_desest) %>% ungroup() %>% 
   pivot_wider(id_cols = c("year", "month"),names_from = "indicador", values_from = "valor") %>%
   mutate(fecha = yearmon(year + (month-1)/12)) %>% select(-c("year", "month")) %>%
   `[`( c("fecha", "AFI", "TCO", "ELE", "VGE", "TOS")) -> indic_diarios_esp_desest_mes


#### Exportar series a Excel ####

exportar()

# archivar
if (! dir.exists("../out/archive/")){dir.create("../out/archive/")}
dir_out_today <- paste0("../out/archive/", format(Sys.time(), "%Y_%m_%d"))

if (! dir.exists(dir_out_today)){dir.create(dir_out_today)}

file.copy(list("../out/salida_series.xlsx","../out/ind_diario.png",
               "../out/ind_diario_pred.png"), dir_out_today)

# no sé muy bien qué hace esto 
#source("previsiones_v5.R")