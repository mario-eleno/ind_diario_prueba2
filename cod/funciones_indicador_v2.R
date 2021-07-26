
###############################################################################
###############################################################################
########################## LECTURA AFILIACIÓN #################################
###############################################################################
###############################################################################
###############################################################################

lectura_afiliacion<-function(){
  
  #nomfich1 <- "T:/0 Sharepoint/0_Repositorio_cronologico/20201021/data_clean/tidy_afiliados_rgeneral.csv"
  
  nomfich1 <- "T:/0 Sharepoint/dataframe/tidy_afiliados_rgeneral.csv"
  
  reg_gen<-read.table(nomfich1,header=TRUE,sep=",")
  
  reg_gen$FECHA<-as.Date(reg_gen$FECHA)
  
  #nomfich2 <- "T:/0 Sharepoint/0_Repositorio_cronologico/20201021/data_clean/tidy_afiliados_reta.csv"
  
  nomfich2 <- "T:/0 Sharepoint/dataframe/tidy_afiliados_reta.csv"
  
  
  reta <- read.table(nomfich2,header=TRUE,sep=",")
  
  reta$FECHA<-as.Date(reta$FECHA)
  
  datos <- bind_rows(reg_gen, reta)
  
  ########################### 
  # series diarias por CCAA #
  ########################### 
  
  datos %>% 
    group_by(FECHA,ISO_2_CCAA,CODAUTO) %>%
    summarize(tot = sum(NUM..TRABAJADORES)) %>%
    rename(fecha = FECHA, valor = tot) %>%
    mutate(indicador = "AFI") -> afil_serie_diaria
  
  
  # Rellenar no laborables
  
  # de momento, quitamos todo lo anterior a 2020.
  
  afil_serie_diaria %>% dplyr::filter(fecha >= as.Date("2020-01-01")) ->
    afil_serie_diaria
  
  comunidades <- unique(afil_serie_diaria$ISO_2_CCAA)
  
  afil_serie_diaria_inter <- data.frame()
  
  for (i in comunidades){
    
    afil_serie_diaria %>% dplyr::filter(ISO_2_CCAA == i) -> afil_i
    
    interp <- approx(afil_i$fecha, afil_i$valor,
                     xout = seq(from = min(afil_i$fecha), to = max(afil_i$fecha), by =1))
    
    afil_inter <- data.frame(fecha = as.Date(interp$x), valor_inter = interp$y)
    afil_inter$CODAUTO <- as.numeric(datos %>% select(CODAUTO,ISO_2_CCAA) %>% filter(ISO_2_CCAA==i) %>% select(CODAUTO) %>% unique())
    afil_inter$ISO_2_CCAA <- i
    afil_inter$indicador <- "AFI"
    
    afil_serie_diaria_inter <- bind_rows(afil_serie_diaria_inter,afil_inter )
    
  }
  
  afil_serie_diaria_inter %>% mutate(valor = valor_inter) %>%
    select(-"valor_inter") -> afil_serie_diaria
  
  rm(afil_serie_diaria_inter)
  
  afil_serie_diaria %>% rename(ca=CODAUTO,ca_literal=ISO_2_CCAA) -> afil_serie_diaria
  
  return(afil_serie_diaria)
  
}

###############################################################################
###############################################################################
############################ LECTURA ERTES ####################################
###############################################################################
###############################################################################
###############################################################################


lectura_ertes<-function(){
  
  
  ertes <- read.table("T:/0 Sharepoint/dataframe/tidy_erte_ccaa.csv", header = TRUE, sep =",")
  
  ertes %>% mutate(fecha = as.Date(fecha), personas = fm + nfm) %>% select(-c("fm", "nfm")) -> ertes
  
  
  ###### para las fechas <= 2020-05-11 distribuye proporcionalmente ######
  
  # calcula proporciÃ³n el 2020-05-12 
  
  ertes %>% dplyr::filter(cod_ine_prov >0) %>% dplyr::filter(fecha == min(fecha)) %>%
    mutate(prop = personas/sum(personas)) %>% select(c("cod_ine_prov", "prop")) -> prop
  
  
  # separa el dato nacional para distribuirlo
  
  ertes %>% dplyr::filter(cod_ine_prov == 0) %>% mutate(total = personas) %>%
    select(c("fecha", "total")) -> total
  
  # creamos combinaciones, le pegamos los datos que hay y donde no los hay ponemos el producto de la proporciÃ³n que
  # calculamos antes por el total nacional
  
  ertes %>% expand(fecha, cod_ine_prov) %>% left_join(ertes, by = c("fecha", "cod_ine_prov")) %>%
    left_join(total, by = "fecha") %>% left_join(prop, by = "cod_ine_prov") %>%
    mutate(personas = ifelse(cod_ine_prov >0 & is.na(personas), total * prop, personas)) -> ertes
  
  # agrupamos por ccaa
  
  nomfich<-"../data_aux/cod_ccaa_prov.xlsx"
  
  cod<-read.xlsx(nomfich, sheet = 1)
  
  cod %>% rename(cod_ine_prov=CODIGO_PROV) -> cod
  
  ertes %>% left_join(cod, by="cod_ine_prov") %>% filter(cod_ine_prov !=0) %>% 
    group_by (fecha,CODIGO_CCAA,COD_ISO_CCAA) %>% summarise(valor=round(sum(personas),0)) -> 
    ertes_diarios
  
  
  # Rellenar no laborables
  
  # de momento, quitamos todo lo anterior a 2020.
  
  ertes_diarios %>% dplyr::filter(fecha >= as.Date("2020-01-01")) ->
    ertes_diarios
  
  comunidades <- unique(ertes_diarios$COD_ISO_CCAA)
  
  ertes_serie_diaria_inter <- data.frame()
  
  for (i in comunidades){
    ertes_diarios %>% dplyr::filter(COD_ISO_CCAA == i) -> ertes_i
    
    interp <- approx(ertes_i$fecha, ertes_i$valor,
                     xout = seq(from = min(ertes_i$fecha), to = max(ertes_i$fecha), by =1))
    
    ertes_inter <- data.frame(fecha = as.Date(interp$x), valor_inter = interp$y)
    ertes_inter$CODIGO_CCAA <- unique(ertes_diarios$CODIGO_CCAA[ertes_diarios$COD_ISO_CCAA==i])
    ertes_inter$COD_ISO_CCAA <- i
    ertes_serie_diaria_inter <- bind_rows(ertes_serie_diaria_inter,ertes_inter )
    
  }
  
  ertes_serie_diaria_inter %>% mutate(valor = valor_inter) %>%
    select(-"valor_inter") -> ertes_serie_diaria
  
  rm(ertes_serie_diaria_inter)
  
  ertes_serie_diaria %>% rename(ca=CODIGO_CCAA,ca_literal=COD_ISO_CCAA) %>%
    mutate(indicador="ERTE") -> ertes_serie_diaria
  
}


lectura_ertes_rd3020<-function(){
 
  ertes_rd3020 <- read.table("T:/0 Sharepoint/dataframe/tidy_ertes_serie_diaria_rd302020.csv", header = TRUE, sep =",")
  ertes_rd3020 %>% select(fecha, tna_total_rd302020) %>% rename(valor = tna_total_rd302020) %>% 
    mutate(indicador = "ERT2", fecha = as.Date(fecha)) -> ertes_rd3020 
  
}

###############################################################################
###############################################################################
########################### LECTURA TARJETAS ##################################
###############################################################################
###############################################################################
###############################################################################


############################## CCAA #####################################

lectura_tarjetas<-function(){
  
  nomfich <- "T:/0 Sharepoint/dataframe/tidy_bbva.csv"
  
  tarjetas <- read.table(nomfich, header=TRUE, sep = ",")
  
  tarjetas$FECHA <-as.Date(tarjetas$FECHA)
  
  read_delim("../data_aux/equivalencias_bbva_cnae.csv", delim = ";",
             col_types = "cccc") %>%
    select(c("category", "sec_cnae")) -> equiv
  
  equiv <- transform(equiv, category = as.numeric(category)) 
  
  tarjetas %>% left_join(equiv, by = "category") %>% 
    select(FECHA,cod_ine_ccaa,Cod_ISO_2,category,Categoria,sec_cnae,amount)  %>% 
    mutate(indicador = case_when(sec_cnae == "G" ~  "TCO",
                                 sec_cnae %in% c("H", "I") ~ "TOS")) %>%
    group_by(FECHA, cod_ine_ccaa, Cod_ISO_2, indicador) %>%
    summarize(valor = sum(amount))  %>%
    dplyr::filter(!is.na(indicador)) %>%
    rename(fecha=FECHA,ca=cod_ine_ccaa,ca_literal=Cod_ISO_2) ->tarjetas_serie_diaria
 
  # esta versión es con online
  
  # nomfich <- "T:/0 Sharepoint/dataframe/tidy_total_BBVA_incl_online.csv"
  # 
  # tarjetas_io <- read.table(nomfich, header=TRUE, sep = ",")
  # 
  # tarjetas_io %>% rename(fecha = date) %>% mutate(fecha = as.Date(fecha)) %>% 
  #   left_join(equiv, by = "category") %>%
  #   rename(Categoria = category) %>% 
  #   select(fecha,Categoria,sec_cnae,amount)  %>% 
  #   mutate(indicador = case_when(sec_cnae == "G" ~  "TCO",
  #                                sec_cnae %in% c("H", "I") ~ "TOS")) %>%
  #   group_by(fecha, indicador) %>%
  #   summarize(valor = sum(amount))  %>%
  #   dplyr::filter(!is.na(indicador)) ->tarjetas_serie_diaria_io
    
}

############################## NACIONAL #####################################

lectura_tarjetas_total <-function(){
  
  # total nacional pagos tarjetas
  
  nomfich2 <- "T:/0 Sharepoint/dataframe/tidy_total_BBVA.csv"
  
  # day,month,year,provincia,category,count,amount
  
  tarjetas_total <- read.table(nomfich2, header=TRUE, sep = ",")
  
  tarjetas_total$date <-as.Date(tarjetas_total$date)
  
  read_delim("../data_aux/equivalencias_bbva_cnae.csv", delim = ";",
             col_types = "cccc") %>%
    select(c("category", "sec_cnae")) -> equiv
  
  equiv <- transform(equiv, category = as.numeric(category)) 
  
  
  
  tarjetas_total %>% left_join(equiv, by = "category") %>% 
    select(date,category,sec_cnae,amount) %>% 
    mutate(indicador = case_when(sec_cnae == "G" ~  "TCO",
                                 sec_cnae %in% c("H", "I") ~ "TOS")) %>%
    group_by(date, indicador) %>%
    summarize(valor = sum(amount))  %>%
    dplyr::filter(!is.na(indicador)) %>%
    rename(fecha=date) ->tarjetas_total_diario
  
}

# para cuando solo tenemos el fichero tidy_BBVA_CCAA.csv

lectura_tarjetas_total2 <-function(){
  
  # total nacional pagos tarjetas
  
  nomfich2 <- "T:/0 Sharepoint/dataframe/tidy_BBVA_CCAA.csv"
  
  # day,month,year,provincia,category,count,amount
  
  tarjetas_CCAA <- read.table(nomfich2, header=TRUE, sep = ",")
  
  tarjetas_CCAA %>% filter(tpv == "P") %>% group_by(date, category) %>% 
    summarize(amount = sum(amount)) -> tarjetas_total
  
  tarjetas_total$date <-as.Date(tarjetas_total$date)
  
  read_delim("../data_aux/equivalencias_bbva_cnae.csv", delim = ";",
             col_types = "cccc") %>%
    select(c("category", "sec_cnae")) -> equiv
  
  equiv <- transform(equiv, category = as.numeric(category)) 
  
  tarjetas_total %>% left_join(equiv, by = "category") %>% 
    select(date,category,sec_cnae,amount) %>% 
    mutate(indicador = case_when(sec_cnae == "G" ~  "TCO",
                                 sec_cnae %in% c("H", "I") ~ "TOS")) %>%
    group_by(date, indicador) %>%
    summarize(valor = sum(amount))  %>%
    dplyr::filter(!is.na(indicador)) %>%
    rename(fecha=date) ->tarjetas_total_diario
  
}


###############################################################################
###############################################################################
############################# LECTURA VGE #####################################
###############################################################################
###############################################################################
###############################################################################

lectura_vge <- function(){
  
  ################ CALCULA PESOS ################ 
  
  cods_cntr <- c("995100", "995200", "995300", "995410", "995420", "995440",
                 "995450", "995470")
  
  nombres <- c("A", "BCDE", "F", "GHI", "J", "L", "MN", "RT")
  
  cntr_secciones <- LeerGW(cods_cntr, NOMBRES = nombres, TIPO = "A")
  
  cntr_secciones %>% 
    pivot_longer(cols =  -"PERIODO", names_to = "id_seccion", values_to = "cntr") -> cntr_secciones
  
  cntr_secciones %>% group_by(id_seccion) %>% dplyr::filter(PERIODO == max(PERIODO)) %>%
    ungroup() %>% mutate(cntr = cntr/sum(cntr)) %>% select(-"PERIODO") -> pesos_vge
  
  ################ ################ 
  
  read.table("T:/0 Sharepoint/dataframe/tidy_ventas_diarias.csv",
             header = TRUE, sep =",") %>% mutate(fecha = as.Date(fecha)) -> vge
  
  
  vge$id_seccion <- recode(vge$id_seccion, C = "BCDE", G = "GHI", H = "GHI", I = "GHI")
  
  vge %>% group_by(id_seccion, fecha) %>% summarize(valor = sum(valor)) %>%
    mutate(indicador = "VGE") -> vge_serie_diaria
  
  return(list(vge_serie_diaria = vge_serie_diaria, pesos_vge = pesos_vge))
  
}

###############################################################################
###############################################################################
############################# LECTURA ELEC ####################################
###############################################################################
###############################################################################
###############################################################################

lectura_elec <- function(){
  
  vele <- LeerGW("vele", NOMBRES = "DEM_ELEC", TIPO = "D")
  
  vele %>%
    mutate(PERIODO = as.Date(as.character(PERIODO), format = "%Y%m%d")) %>%
    rename(fecha = PERIODO, valor = DEM_ELEC) %>% mutate(indicador = "ELE") -> aux
  
  return(aux)
  
}

###############################################################################
########################## LECTURA MENSUALES ##################################
###############################################################################

lectura_mensuales <- function(cods, noms, defs){
  
  df_ind <- LeerGW(cods, NOMBRES = noms, TIPO = "M")
  
  df_def <- LeerGW(defs[!is.na(defs)], NOMBRES = defs[!is.na(defs)], TIPO = "M")
  
  df_ind %>% left_join(df_def, by = "PERIODO") -> df_ind
  
  df_ind[,noms[!is.na(defs)]] <- 100*df_ind[,noms[!is.na(defs)]]/df_ind[,defs[!is.na(defs)]]
  
  matriz.indicadores <- df_ind[, c("PERIODO", noms)]
  
  noms <- setdiff(colnames(matriz.indicadores), "PERIODO")
  
  matriz.indicadores %>% pivot_longer(cols = all_of(noms), values_to = "valor", names_to ="indicador") ->
    df_indicadores
  
  return(list(df_indicadores = df_indicadores, matriz.indicadores = matriz.indicadores))
  
}

# corrección calendario VGE

calend <- function(vge_nacional_indice_general){
  
  vge_nacional_indice_general %>% mutate(ym = format(fecha, "%Y%m")) -> vge_mes
  
  vge_mes %>% group_by(ym) %>% summarize(media = mean(valor)) -> vge_medias_mes
  
  vge_mes %>% left_join(vge_medias_mes, by = "ym") %>% mutate(cociente = valor/media) %>%
    mutate(ult = ifelse(month(fecha + 1) != month(fecha),1,0 ))  %>%
     mutate(ult = ifelse(fecha == as.Date("2020-10-30"), 1, ult)) %>%
    # mutate(ult = ifelse(fecha == as.Date("2020-10-31"), 0, ult)) %>%
    group_by(ult) %>% summarize(mg = mean(cociente)) -> perfil
  
  vge_nacional_indice_general %>% mutate(ult = ifelse(month(fecha + 1) != month(fecha),1,0 )) %>%
     mutate(ult = ifelse(fecha == as.Date("2020-10-30"), 1, ult)) %>%
    # mutate(ult = ifelse(fecha == as.Date("2020-10-31"), 0, ult)) %>%
    left_join(perfil, by = "ult") %>% mutate(valor = valor / mg) %>%
    select(fecha, valor) -> 
    vge_nacional_indice_general_corr
  
  return(vge_nacional_indice_general_corr)

}

# desagregación temporal

desag <- function(dfd, dfm, indd, indm, metodo = "fernandez"){
  
  # diaria
  
  dfd %>% filter(indicador == indd) -> serie_diaria
  
  #mensual
  
  dfm %>% filter(indicador == indm) -> serie_mensual
  
  serie_diaria %>% select(fecha,valor) %>% rename(time=fecha) -> diaria.time

  serie_mensual %>% mutate(fecha=as.Date(PERIODO)) %>% select(fecha,valor) %>% 
    filter (fecha >= as.Date("2018-01-01")) %>% rename(time=fecha) -> mensual.time
  
  mod <- td(mensual.time ~ diaria.time, conversion="mean",to="daily", method = metodo)
  diario.desag <- mod[["values"]]
  
  diario.desag %>% rename(valor = value) -> diario.desag
  
  #########
  
  serie_mensual %>% filter(PERIODO >= as.yearmon(min(serie_diaria$fecha)))  -> serie_mensual
  serie_diaria %>%
    mutate(valor = mod$coefficients["(Intercept)"] + mod$coefficients["diaria.time"] * valor) %>% 
    mutate(tipo = "original") -> serie_diaria_lineal
  
  diario.desag %>% mutate(fecha = time, tipo = "desag") -> serie_diaria_desag
  
  bind_rows(serie_diaria_desag, serie_diaria_lineal) %>% group_by(tipo) %>% 
    mutate(valor = rollmean(valor, k = 7, fill =NA, align = "center")) -> series_diarias 
  
  serie_mensual %>% mutate(tipo = "obs") %>% filter(!is.na(valor)) -> serie_mensual
  
  ultmes <- max(serie_mensual$PERIODO)
  
  serie_diaria_desag %>% mutate(PERIODO = as.yearmon(time)) %>% group_by(PERIODO) %>% 
    summarize(valor = mean(valor)) %>% filter(PERIODO > ultmes) %>% 
    mutate(tipo = "pred", indicador = relacion[ind,2]) -> serie_mensual_pred
  
  serie_mensual %>% bind_rows(serie_mensual_pred) %>%
    mutate(fecha = as.Date(PERIODO)+15) -> series_mensuales
  
  ggplot()+ geom_point(aes(x = fecha, y = valor, shape = tipo), data = series_mensuales) +
    geom_line(data = series_diarias, aes(x = fecha, y = valor, color = tipo)) +
    ggtitle(relacion[ind,2]) -> graf
  
  ggsave(paste0("../out/", relacion[ind,2], ".png"),
         plot = graf, width = 18, height = 12,bg = "transparent",
         units = "cm", dpi = 500)
  
  #########
  
  return(diario.desag)
  
  
}


# convierte en índice

conv_indice <- function(df, fecha1, fecha2, by_var = "indicador"){
  
  
  df %>%
    dplyr::filter(fecha >= fecha1 &
                    fecha <= fecha2) %>%
    group_by(.dots = by_var) %>%
    summarize(media_base = mean(valor)) -> medias
  
  df %>%
    left_join(medias, by =by_var) %>%
    mutate(valor = 100*valor/media_base) %>%
    select(-"media_base") -> df
  
  return(df)
  
}

###############################################################################
########################## PONDERACIONES ##################################
###############################################################################

ponde <- function(pib, matriz.indicadores){
  
  nivel <- matriz.indicadores[matriz.indicadores$PERIODO == yearmon(2020+2/12),-1]
  matriz.indicadores[,-1] <- 100*matriz.indicadores[,-1]/slice(nivel, rep(1,nrow(matriz.indicadores)))
  
  # transformamos en objeto zoo para trimestralizar los
  # indicadores y unir al PIB
  
  ind <- zoo(matriz.indicadores[,-1], order.by = matriz.indicadores$PERIODO)
  
  indq <- aggregate(ind, as.yearqtr, mean)        #[-(1:64),]
  pib <- zoo(pib[,-1], order.by = pib$PERIODO)
  
  z <- merge(pib, indq)
  z1 <- z[1:112,]
  
  ################ m?todo 2a: reg + correlaci?n ####################
  
  cor.ind <- cor(cbind(diff(pib, 4), diff(indq,4)), use="complete.obs")
  
  w.isa.diario <- cor.ind[1,3:6]/sum(cor.ind[1,3:6])
  isa.indq <- indq[,c("ICM","IASS","IPI", "VGE")]
  
  isa.diario.q <- data.matrix(apply(isa.indq * (rep(1,nrow(isa.indq)) %*% t(w.isa.diario)), 1, sum))
  
  indq2<-cbind(indq,isa.diario.q)
  colnames(indq2)[6]<-"isa.diario.q"
  
  z <- merge(pib, indq2)
  z1 <- z[1:112,]
  
  mod2c <- lm(diff(PIB,1) ~  diff(AFIL,1) + diff(isa.diario.q,1), data = z1)
  resumen <- summary(mod2c)
  sig <- resumen$sigma
  covcoef <- resumen$cov.unscaled

  intercept <- mod2c$coefficients["(Intercept)"]
  w1 <- mod2c$coefficients["diff(AFIL, 1)"]
  w2 <- mod2c$coefficients["diff(isa.diario.q, 1)"]
  
  w <- c(w1, w2 * w.isa.diario[c("ICM", "IASS", "IPI", "VGE")])
  coef <- data.frame(indicador = c("AFIL", "ICM", "IASS", "IPI", "VGE"), peso = w)
  
  return(list(coef = coef, intercept = intercept, sigma = sig, covcoef = covcoef))
  
}


###############################################################################
######################### PREDICCIÓN INDICADOR ################################
###############################################################################

pred_indi <- function(datos_act, b, h){
  
  ind.d7 <- diff(datos_act$ind_act,7)
  tr <- mean(tail(ind.d7, b))
  
  datos_act_for <- datos_act
  datos_act_for$tipo <- "obs"
  
  for (i in 1:h){
    
    n_fecha <- max(datos_act_for$fecha) + 1
    n_val <- datos_act_for$ind_act[datos_act_for$fecha == max(datos_act_for$fecha) - 6] + tr
    
    datos_act_for <- bind_rows(datos_act_for, data.frame(fecha = n_fecha, ind_act = n_val, tipo = "pred"))
    
  }
  
  return(datos_act_for)
  
}

######  mensualizar #####

mensualizar <- function(indicadores){
  
  indicadores %>% mutate(year = year(fecha), month = month(fecha)) %>%
    group_by(year, month, indicador) %>% summarize(valor = mean(valor)) -> df_out
 
  return(df_out)
   
}


exportar <- function(){
  
  #### exportar series ####
  
  wb <- createWorkbook()
  
  # indicadores mensuales originales
  
  addWorksheet(wb, "mensuales_originales")
  writeData(wb, sheet = "mensuales_originales", matriz.indicadores, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  # indicadores diarios originales mensualizados
  
  addWorksheet(wb, "dia_orig_mensualiz")
  writeData(wb, sheet = "dia_orig_mensualiz",
            indic_diarios_esp_mes, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  # indicadores diarios originales mensualizados
  
  addWorksheet(wb, "dia_orig_desest_mensualiz")
  writeData(wb, sheet = "dia_orig_desest_mensualiz",
            indic_diarios_esp_desest_mes, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  # indicadores diarios desagregados mensualizados
  
  addWorksheet(wb, "dia_desag_mensualiz")
  writeData(wb, sheet = "dia_desag_mensualiz", indic_diarios_esp_desag_indice_mes,
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  
  # los tres agrupados para comparación
  
  addWorksheet(wb, "compara")
  
  matriz.indicadores %>% rename(fecha = PERIODO) %>%
    rename_with(~ paste0(.x, "_m_orig"), .cols = -"fecha") -> aux
  
  indic_diarios_esp_desag_indice_mes %>%
    rename_with(~ paste0(.x, "_d_desag_orig"), .cols = - "fecha") -> aux2
  
  indic_diarios_esp_desest_mes %>%
    rename_with(~ paste0(.x, "_d_orig"), .cols =  - "fecha") -> aux3
  
  indic_diarios_esp_mes %>%
    rename_with(~ paste0(.x, "_d_orig"), .cols =  - "fecha") -> aux4
  
  aux %>% left_join(aux2, by = "fecha") %>% left_join(aux3, by = "fecha") %>%
    left_join(aux4, by = "fecha") -> datos
  
  writeData(wb, sheet = "compara",
            datos, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  # indicadores diarios originales
  
  addWorksheet(wb, "diarios_originales")
  indic_diarios_esp %>% pivot_wider(names_from = indicador, values_from = valor) %>%
    arrange(fecha) -> datos
  writeData(wb, sheet = "diarios_originales", datos, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  # indicadores diarios desagregados
  
  addWorksheet(wb, "diarios_desagregados")
  indic_diarios_esp_desag_indice %>% 
    pivot_wider(id = fecha, names_from = indicador, values_from = valor) %>%
    arrange(fecha) -> datos
  writeData(wb, sheet = "diarios_desagregados", datos, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  # indicador diario
  
  addWorksheet(wb, "diario")
  writeData(wb, sheet = "diario", datos_act_for, startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE)
  
  
  saveWorkbook(wb, "../out/salida_series.xlsx", overwrite = TRUE)
  
}