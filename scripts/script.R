# 
# Proyecto: FIS
# Objetivo: Análisis de impacto del Fondo para Investigación en Salud
# Autor: Maria Camila Arias Álvarez - Econometría Consultores S.A.S.
# Fecha de creación: 9/05/2024
# última modificación: 22/07/2024
# 

# Initial Setup ----------------------------------------------------------¿-

rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(tidyverse, # tidy-data
       ggplot2,
       dplyr,
       tidyr)

if ( Sys.getenv("USERNAME")=="paul.rodriguez") {
  setwd("G:/.shortcut-targets-by-id/16saFOuJe6cZfw5VQ57BO3x5RmDTizAYF/MinCTeI - Evaluación FIS/04 Archivos de trabajo/Paul Rodriguez/FIS - Análisis")
} else {
  setwd("C:/Users/Maria.Arias/OneDrive - ECONOMETRIA CONSULTORES SA/Github Desktop/FIS/Procesamiento3")
}



bd <- readxl::read_xlsx("data/base.xlsx", sheet="Entre 2011 y 2019")

# Data managing ------------------------------------------------------------
## Defining treatment status
  # Si el investigador nunca recibió el FIS es un control puro
  nrow(bd) #entre 2011 y 2019 tenemos 229 investigadores
  bd <- tibble::rowid_to_column(bd, "ID") # Creamos ID
  table(bd$`202. ¿Alguna vez le fue otorgado el FIS?`) # tenemos 81 controles puros y 148 tratados
  
  # ¿En qué año fueron tratados?
  table(bd$`202. ¿Alguna vez le fue otorgado el FIS?`,bd$`203. ¿En qué año le fue otorgado?`)
  
  # Creo un status para cada investigador para cada año del periodo que me interesa analizar
  for (year in 2011:2019) {
    status_var <- paste0("status", year)
    bd <- bd %>% mutate(!!status_var := ifelse(`202. ¿Alguna vez le fue otorgado el FIS?`=="2. No",0,NA),
                        !!status_var := ifelse(is.na(!!sym(status_var)) & `203. ¿En qué año le fue otorgado?` <= year,1,0))
  }

  ## Estimating number of years of studies according to the last achieved degree
  bd <- bd %>%   mutate(años_estudio = case_when(
    `106. ¿Cuál es el último nivel de estudios alcanzado por usted?` == "1. Universitario" ~ 16,
    `106. ¿Cuál es el último nivel de estudios alcanzado por usted?` == "2. Especialización" ~ 17,
    `106. ¿Cuál es el último nivel de estudios alcanzado por usted?` == "3. Maestría" ~ 18,
    `106. ¿Cuál es el último nivel de estudios alcanzado por usted?` == "4. Doctorado" ~ 23,
    `106. ¿Cuál es el último nivel de estudios alcanzado por usted?` == "5. Pos doctorado" ~ 24,
    TRUE ~ NA_real_  
  ))
  
  ## Coding variable whether they received feedback of the evaluation committee, they appeal to the verdict and they executed the project
  bd <- bd %>% mutate(feedback = if_else(`212. ¿Recibió retroalimentación de parte de la convocatoria sobre los resultados de su evaluación?**` == "1. Sí",1,0),
                      apelo = if_else(`213. ¿Apeló la calificación?**` == "1. Sí",1,0),
                      ejecuto = if_else(`218. ¿Usted ejecutó el proyecto?**`=="1. Sí",1,0))
  
  # Transformar los datos a un formato largo
  bd_long <- bd %>%
    pivot_longer(cols = starts_with("status"),
                 names_to = "year",
                 names_prefix = "status",
                 values_to = "status")
  
  # Convertir el año de carácter a numérico
  bd_long$year <- as.numeric(bd_long$year)
  
  # Contar el número de 1 y 0 por año
  status_counts <- bd_long %>%
    group_by(year, status) %>%
    summarise(count = n(), .groups = 'drop')
  


  
# Balance table for control variables ------------------------------------------
source("scripts/tabla_balance.r")

# Extra data managing ----------------------------------------------------------
  bd_long_plus <- bd_long %>% select(year,status, everything()) %>%
    select(-starttime,-endtime,-hoy,-logo,-"NIM...6",-"0...8",
           -"Consentimiento informado",-"0...10",-"Nombres y apellidos",
           -"_id",-"_uuid", -"_submission_time", -"_validation_status",
           -"_notes", -"_status", -"_submitted_by", -"__version__", -"_tags",
           -"_index")
  
  #Es control puro, los usos de los recursos y si recibió recursos de fuentes diferentes.
  
  bd_long_plus <- bd_long_plus %>% mutate(impact_ano_mas_1 = impact_ano_mas_1-1) # So we don't have a "gap" for the CS exercise
  
  
  #Es control puro, los usos de los recursos y si recibió recursos de fuentes diferentes.
  bd_long_plus <- bd_long_plus %>% mutate(control_puro = if_else(`202. ¿Alguna vez le fue otorgado el FIS?`=="2. No",1,0),
                                          G      = if_else(is.na(`203. ¿En qué año le fue otorgado?`),0,`203. ¿En qué año le fue otorgado?`),
                                          p201 = `201. ¿En qué año se postuló por primera vez al FIS?`,
                                          p205_a = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/a. Personal científico`==1,1,0),
                                          p205_b = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/b. Viajes (seminarios y congresos)`==1,1,0),
                                          p205_c = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/c. Vinculación jóvenes investigadores`==1,1,0),
                                          p205_d = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/d. Equipos`==1,1,0),
                                          p205_e = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/e. Eventos académicos`==1,1,0),
                                          p205_f = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/f. Servicios técnicos`==1,1,0),
                                          p205_g = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/g. Publicaciones y patentes`==1,1,0),
                                          p205_h = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/h. Salidas de campo`==1,1,0),
                                          p205_i = if_else(`205. ¿Cuáles fueron los usos de estos recursos?**/i. Compra materiales para la investigación`==1,1,0),
                                          p214 = if_else(`214. ¿Recibió recursos de fuentes diferentes al FIS para financiar este proyecto o programa?**`=="2. No",0,1))
  
  
  bd_long_plus <- bd_long %>% select(starts_with("215. ¿En qué año(s) recibio estos recursos?**"), NIM...4, year) %>%
    pivot_longer(cols = starts_with("215. ¿En qué año(s) recibio estos recursos?**/"),
                 names_to = "resource_year",
                 values_to = "received") %>%
    mutate(year_resources = as.numeric(str_extract(resource_year, "\\d{4}"))) %>%
    filter(!is.na(year_resources)) %>%
    select(-resource_year,-"215. ¿En qué año(s) recibio estos recursos?**") %>%
    filter(received == 1) %>%
    mutate(p215 = if_else(year == year_resources,1,0)) %>%
    select(-received) %>%
    right_join(bd_long_plus, by = c("NIM...4", "year"))
  
  bd_long_plus <- bd_long_plus %>% select(NIM...4,status,year,p215, everything())
  
  bd_long_plus <- bd_long_plus %>% mutate(p217 = case_when(`217b. Tipo de moneda:**`=="2. Dólares" ~ `217a. Aproximadamente, ¿de qué monto fue dicho financiamiento? (Si se recibió financiamiento en varios momentos y/o varias fuentes, hacer la suma y colocar el monto aproximado)**`*1856,
                                                           `217b. Tipo de moneda:**`=="3. Euros" ~ `217a. Aproximadamente, ¿de qué monto fue dicho financiamiento? (Si se recibió financiamiento en varios momentos y/o varias fuentes, hacer la suma y colocar el monto aproximado)**`*2527,
                                                           TRUE ~ `217a. Aproximadamente, ¿de qué monto fue dicho financiamiento? (Si se recibió financiamiento en varios momentos y/o varias fuentes, hacer la suma y colocar el monto aproximado)**`),
                                          p218 = case_when(`218. ¿Usted ejecutó el proyecto?**`=="1. Sí" ~ 1,
                                                           `218. ¿Usted ejecutó el proyecto?**`=="2. No, me retiré de la institución antes del inicio del proyecto"  ~ 0,
                                                           `218. ¿Usted ejecutó el proyecto?**`=="3. No, otro ¿por qué?"  ~ 0,
                                                            TRUE ~ NA_real_),
                                          p220 = case_when(year==impact_ano_menos_1 ~ `220. Menos 3 al Menos 1...121`,
                                                           year==impact_ano_mas_1 ~ `220. Más 3 al Más 1...123`,
                                                           TRUE ~ NA_real_),
                                          p301a = case_when(year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/a. Artículos`==1~1,
                                                            year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/a. Artículos`==0~0,
                                                           year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/a. Artículos`==1 ~1,
                                                           year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/a. Artículos`==0 ~0,
                                                           TRUE ~ NA_real_  ),
                                          p301b = case_when(year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/b. Patentes`==1~1,
                                                            year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/b. Patentes`==0~0,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/b. Patentes`==1 ~1,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/b. Patentes`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p301c = case_when(year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/c. Prototipos`==1~1,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/c. Prototipos`==1 ~1,
                                                            year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/c. Prototipos`==0~0,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/c. Prototipos`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p301d = case_when(year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/d. Más financiación`==1~1,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/d. Más financiación`==1 ~1,
                                                            year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/d. Más financiación`==0~0,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/d. Más financiación`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p301e = case_when(year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/e. Consolidar alianzas estratégicas`==1~1,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/e. Consolidar alianzas estratégicas`==1 ~1,
                                                            year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/e. Consolidar alianzas estratégicas`==0~0,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/e. Consolidar alianzas estratégicas`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p301f = case_when(year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/f. Ninguna de las anteriores`==1~1,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/f. Ninguna de las anteriores`==1 ~1,
                                                            year==impact_ano_menos_1 & `301. Menos 3 al Menos 1/f. Ninguna de las anteriores`==0~0,
                                                            year==impact_ano_mas_1 & `301. Mas 3 al Mas 1/f. Ninguna de las anteriores`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p302 = case_when(year==impact_ano_menos_1 & `302. Menos 3 al Menos 1...156`=="1. Sí" ~ 1,
                                                           year==impact_ano_menos_1 & `302. Menos 3 al Menos 1...156`=="2. No" ~ 0,
                                                           year==impact_ano_mas_1 & `302. Mas 3 al Mas 1...158`=="1. Sí" ~ 1,
                                                           year==impact_ano_mas_1 & `302. Mas 3 al Mas 1...158`=="2. No" ~ 0,
                                                           TRUE ~ NA_real_  ),
                                          p303 = case_when(year==impact_ano_menos_1 & `303. Menos 3 al Menos 1...161`=="1. Sí" ~ 1,
                                                           year==impact_ano_menos_1 & `303. Menos 3 al Menos 1...161`=="2. No" ~ 0,
                                                           year==impact_ano_mas_1 & `303. Mas 3 al Mas 1...163`=="1. Sí" ~ 1,
                                                           year==impact_ano_mas_1 & `303. Mas 3 al Mas 1...163`=="2. No" ~ 0,
                                                           TRUE ~ NA_real_  ),
                                          p303a = case_when(year==impact_ano_menos_1 & `303a. Menos 3 al Menos 1...166`=="1. Sí" ~ 1,
                                                           year==impact_ano_menos_1 & `303a. Menos 3 al Menos 1...166`=="2. No" ~ 0,
                                                           year==impact_ano_mas_1 & `303a. Mas 3 al Mas 1...168`=="1. Sí" ~ 1,
                                                           year==impact_ano_mas_1 & `303a. Mas 3 al Mas 1...168`=="2. No" ~ 0,
                                                           TRUE ~ NA_real_  ),
                                          p304 = case_when(year==impact_ano_menos_1 & `304. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                           year==impact_ano_menos_1 & `304. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                           year==impact_ano_mas_1 & `304. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                           year==impact_ano_mas_1 & `304. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                           TRUE ~ NA_real_  ),
                                          p305a = case_when(year==impact_ano_menos_1 & `305a. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305a. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305a. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305a. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305b = case_when(year==impact_ano_menos_1 & `305b. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305b. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305b. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305b. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305c = case_when(year==impact_ano_menos_1 & `305c. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305c. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305c. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305c. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305d = case_when(year==impact_ano_menos_1 & `305d. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305d. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305d. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305d. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305e = case_when(year==impact_ano_menos_1 & `305e. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305e. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305e. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305e. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305f = case_when(year==impact_ano_menos_1 & `305f. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305f. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305f. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305f. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305g = case_when(year==impact_ano_menos_1 & `305g. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305g. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305g. Mas 3 al Mas 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305g. Mas 3 al Mas 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p305h = case_when(year==impact_ano_menos_1 & `305h. Menos 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `305h. Menos 3 al Menos 1`=="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `305h. Mas 3 al Menos 1`=="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `305h. Mas 3 al Menos 1`=="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p306 = case_when(year==impact_ano_menos_1 & `306. Menos 3 al Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `306. Menos 3 al Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `306. Mas 3 al Mas 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `306. Mas 3 al Mas 1` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p307 = case_when(year==impact_ano_menos_1 & `308. Menos 3 al Menos 1...220` =="1. Sí" ~ 1,
                                                           year==impact_ano_menos_1 & `308. Menos 3 al Menos 1...220` =="2. No" ~ 0,
                                                           year==impact_ano_mas_1 & `308. Mas 3 al Mas 1...222` =="1. Sí" ~ 1,
                                                           year==impact_ano_mas_1 & `308. Mas 3 al Mas 1...222` =="2. No" ~ 0,
                                                           TRUE ~ NA_real_  ),
                                          p402 = case_when(year==impact_ano_menos_1 & `402. Menos 1` =="1. Laboral a término fijo" ~ 1,
                                                           year==impact_ano_menos_1 & `402. Menos 1` =="2. Laboral a término indefinido" ~ 2,
                                                           year==impact_ano_menos_1 & `402. Menos 1` =="3. Prestación de servicios" ~ 3,
                                                           year==impact_ano_menos_1 & `402. Menos 1` =="4. Independiente" ~ 4,
                                                           year==impact_ano_mas_1 & `402. Mas 3` =="1. Laboral a término fijo" ~ 1,
                                                           year==impact_ano_mas_1 & `402. Mas 3` =="2. Laboral a término indefinido" ~ 2,
                                                           year==impact_ano_mas_1 & `402. Mas 3` =="3. Prestación de servicios" ~ 3,
                                                           year==impact_ano_mas_1 & `402. Mas 3` =="4. Independiente" ~ 4,
                                                           TRUE ~ NA_real_  ),
                                          p403 = case_when(year==impact_ano_menos_1 ~ `403. Menos 3 al Menos 1`,
                                                           year==impact_ano_mas_1 ~ `403. Mas 3 al Mas 1`,
                                                           TRUE ~ NA_real_  ),
                                          p404a = case_when(year==impact_ano_menos_1 & `404a. Menos 3 al Menos 1` =="1. Sí" ~ 1,
                                                           year==impact_ano_menos_1 & `404a. Menos 3 al Menos 1` =="2. No" ~ 0,
                                                           year==impact_ano_mas_1 & `404a. Mas 3 al Mas 1` =="1. Sí" ~ 1,
                                                           year==impact_ano_mas_1 & `404a. Mas 3 al Mas 1` =="2. No" ~ 0,
                                                           TRUE ~ NA_real_  ),
                                          p404b = case_when(year==impact_ano_menos_1 & `404b. Menos 3 al Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `404b. Menos 3 al Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `404b. Mas 3 al Mas 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `404b. Mas 3 al Mas 1` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p404c = case_when(year==impact_ano_menos_1 & `404c. Menos 3 al Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `404c. Menos 3 al Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `404c. Mas 3 al Mas 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `404c. Mas 3 al Mas 1` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p405a = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/a. Su mismo grupo de investigaciones`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/a. Su mismo grupo de investigaciones`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/a. Su mismo grupo de investigaciones`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/a. Su mismo grupo de investigaciones`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405b = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/b. Otras instituciones nacionales (en el mismo departamento)`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/b. Otras instituciones nacionales (en el mismo departamento)`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/b. Otras instituciones nacionales (en el mismo departamento)`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/b. Otras instituciones nacionales (en el mismo departamento)`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405c = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/c. Otras instituciones nacionales (en otros departamentos)`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/c. Otras instituciones nacionales (en otros departamentos)`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/c. Otras instituciones nacionales (en otros departamentos)`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/c. Otras instituciones nacionales (en otros departamentos)`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405d = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/d. Otras instituciones internacionales`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/d. Otras instituciones internacionales`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/d. Otras instituciones internacionales`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/d. Otras instituciones internacionales`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405e = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/e. Sector empresarial`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/e. Sector empresarial`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/e. Sector empresarial`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/e. Sector empresarial`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405f = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/f. Otros grupos de investigación de su misma institución`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/f. Otros grupos de investigación de su misma institución`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/f. Otros grupos de investigación de su misma institución`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/f. Otros grupos de investigación de su misma institución`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405g = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/g. ONG`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/g. ONG`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/g. ONG`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/g. ONG`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405h = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/h. Entidades del sector salud`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/h. Entidades del sector salud`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/h. Entidades del sector salud`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/h. Entidades del sector salud`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405i = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/i. Asociación de pacientes`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/i. Asociación de pacientes`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/i. Asociación de pacientes`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/i. Asociación de pacientes`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p405j = case_when(year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/j. Asociación de cuidadores`==1~1,
                                                            year==impact_ano_menos_1 & `405. Menos 3 al Menos 1/j. Asociación de cuidadores`==0~0,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/j. Asociación de cuidadores`==1 ~1,
                                                            year==impact_ano_mas_1 & `405. Mas 3 al Mas 1/j. Asociación de cuidadores`==0 ~0,
                                                            TRUE ~ NA_real_  ),
                                          p406a = case_when(year==impact_ano_menos_1 & `406a. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `406a. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `406a. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `406a. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p406b = case_when(year==impact_ano_menos_1 & `406b. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `406b. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `406b. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `406b. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p406c = case_when(year==impact_ano_menos_1 & `406c. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `406c. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `406c. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `406c. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p406d = case_when(year==impact_ano_menos_1 & `406d. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `406d. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `406d. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `406d. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p407a = case_when(year==impact_ano_menos_1 & `407a. Menos 1...315` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `407a. Menos 1...315` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `407a. Mas 3...318` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `407a. Mas 3...318` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p407b = case_when(year==impact_ano_menos_1 & `407b. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `407b. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `407b. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `407b. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p407c = case_when(year==impact_ano_menos_1 & `407c. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `407c. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `407c. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `407c. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p407d = case_when(year==impact_ano_menos_1 & `407d. Menos 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `407d. Menos 1` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `407d. Mas 3` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `407d. Mas 3` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p408 = case_when(year==impact_ano_menos_1 & `408. Menos 3 al Menos 1...352` =="1. Sí" ~ 1,
                                                            year==impact_ano_menos_1 & `408. Menos 3 al Menos 1...352` =="2. No" ~ 0,
                                                            year==impact_ano_mas_1 & `408. Mas 3 al Mas 1` =="1. Sí" ~ 1,
                                                            year==impact_ano_mas_1 & `408. Mas 3 al Mas 1` =="2. No" ~ 0,
                                                            TRUE ~ NA_real_  ),
                                          p409 = case_when(year==impact_ano_menos_1 & `409. Menos 3 al Menos 1...359` =="1. Investigador Asociado" ~ 1,
                                                           year==impact_ano_menos_1 & `409. Menos 3 al Menos 1...359` =="3. Investigador Junior" ~ 3,
                                                           year==impact_ano_menos_1 & `409. Menos 3 al Menos 1...359` =="4. Investigador Senior" ~ 4,
                                                           year==impact_ano_menos_1 & `409. Menos 3 al Menos 1...359` =="5. NS/NR" ~ 5,
                                                           year==impact_ano_mas_1 & `409. Mas 3 al Mas 1...360` =="1. Investigador Asociado" ~ 1,
                                                           year==impact_ano_mas_1 & `409. Mas 3 al Mas 1...360` =="3. Investigador Junior" ~ 3,
                                                           year==impact_ano_mas_1 & `409. Mas 3 al Mas 1...360` =="4. Investigador Senior" ~ 4,
                                                           year==impact_ano_mas_1 & `409. Mas 3 al Mas 1...360` =="5. NS/NR" ~ 5,
                                                           TRUE ~ NA_real_  ),
                                          )
                                          
  
   #view(bd_long_plus%>%select(NIM...4,year,starts_with("impact_ano_"),`301. Menos 3 al Menos 1/b. Patentes`,`301. Mas 3 al Mas 1/b. Patentes`,p301a,p301b)%>%filter(NIM...4=="ee.kobotoolbox.org:O396WnvLThVwcUj7"))
  
   
   # Seccion   5 =================================================================
   
   bd_long_plus <- bd_long_plus %>% mutate( p501_a = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/a. Calidad de las instituciones educativas en esta ciudad.`,
                                            p501_b = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/b. Apoyo económico de las empresas y capital privado en esta ciudad`,
                                            p501_c = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/c. Mayor apoyo económico y político de las autoridades locales`,
                                            p501_d = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/d. Mejor salario para los investigadores`,
                                            p501_e = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/e. Amplia oferta de profesionales investigadores`,
                                            p501_f = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/f. Disponibilidad de centros de excelencia en salud`,
                                            p501_g = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/g. Incentivo económico por parte del empleador para innovar`,
                                            p501_h = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/h. Más especialistas, menor carga de trabajo per cápita y más tiempo para investigar`,
                                            p501_i = `501. ¿Cuáles de los siguientes factores contribuyen diferenciadamente a que usted prefiera desarrollar investigación desde la ciudad en donde está basado o estuvo basado para su proyecto de referencia?**/i. Es su lugar de nacimiento / Razones familiares`
   )
   
   
   
   bd_long_plus <- bd_long_plus %>% mutate( p502_ciudad = case_when( ano_menos_1==year ~ `502. Menos 3 al Menos 1`,
                                                                     ano_mas_1==year ~ `502. Mas 3 al Mas 1`,
                                                                     TRUE ~ "") )
   
   bd_long_plus <- bd_long_plus %>% mutate( p503_depto = case_when( ano_menos_1==year ~ `503. Menos 3 al Menos 1`,
                                                                    ano_mas_1==year ~ `503. Mas 3 al Mas 1`,
                                                                    TRUE ~ "") )
   
   
   # Para probarlo
   #xx=bd_long_plus %>% select(NIM...4,status,year,ano_menos_1,ano_mas_1,p215, `502. Menos 3 al Menos 1`, `502. Mas 3 al Mas 1`,p502_ciudad  )
   
   bd_long_plus <- bd_long_plus %>% mutate( p504_investOtrasCiudades = case_when( ano_menos_1==year ~ `504. Menos 3 al Menos 1`,
                                                                                  ano_mas_1==year ~ `504. Mas 3 al Mas 1`,
                                                                                  TRUE ~ "") )  
   
   bd_long_plus <- bd_long_plus %>% mutate( p504_investOtrasCiudadesCual = case_when( ano_menos_1==year ~ `504. Menos 3 al Menos 1_Cuál...489`,
                                                                                      ano_mas_1==year ~ `504. Mas 3 al Mas 1_Cuál...492`,
                                                                                      TRUE ~ "") )     
   
   
   bd_long_plus <- bd_long_plus %>% mutate( p505_datosOtrasCiudades = case_when( ano_menos_1==year ~ `505. Menos 3 al Menos 1`,
                                                                                 ano_mas_1==year ~ `505. Mas 3 al Mas 1`,
                                                                                 TRUE ~ "") )  
   
   bd_long_plus <- bd_long_plus %>% mutate( p505_datosOtrasCiudadesCual = case_when( ano_menos_1==year ~ `505. Menos 3 al Menos 1_Cuál...496`,
                                                                                     ano_mas_1==year ~ `505. Mas 3 al Mas 1_Cuál...499`,
                                                                                     TRUE ~ "") )     
   
   # Poblacion especifica
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_a = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/a. Comunidades rurales`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/a. Comunidades rurales`,
                                                                            TRUE ~ NA) )     
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_b = case_when( ano_menos_1==year ~ `506. Mas 3 al Mas 1/b. Mujeres`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/b. Mujeres`,
                                                                            TRUE ~ NA) )     
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_c = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/c. Comunidades indígenas`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/c. Comunidades indígenas`,
                                                                            TRUE ~ NA) )     
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_d = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/d. Comunidades afrodescendientes`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/d. Comunidades afrodescendientes`,
                                                                            TRUE ~ NA) )     
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_e = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/e. Otros estudiantes/ investigadores`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/e. Otros estudiantes/ investigadores`,
                                                                            TRUE ~ NA) )       
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_f = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/f. Sólo a la institución en la que trabajó`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/f. Sólo a la institución en la que trabajó`,
                                                                            TRUE ~ NA) )        
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_g = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/g. Sólo a su grupo de investigación`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/g. Sólo a su grupo de investigación`,
                                                                            TRUE ~ NA) )        
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_h = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/h. Niños`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/h. Niños`,
                                                                            TRUE ~ NA) )        
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_i = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/i. Comunidad LGBTIQ+`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/i. Comunidad LGBTIQ+`,
                                                                            TRUE ~ NA) )        
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_j = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/j. Comunidad personas con algún tipo de discapacidad`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/j. Comunidad personas con algún tipo de discapacidad`,
                                                                            TRUE ~ NA) )        
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_k = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/k. Personas con ingresos bajos`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/k. Personas con ingresos bajos`,
                                                                            TRUE ~ NA) )        
   
   bd_long_plus <- bd_long_plus %>% mutate( p506_gruposBenef_l = case_when( ano_menos_1==year ~ `506. Menos 3 al Menos 1/l. Salud pública`,
                                                                            ano_mas_1==year ~ `506. Mas 3 al Mas 1/l. Salud pública`,
                                                                            TRUE ~ NA) )         
   
   # Patologia especifica
   
   bd_long_plus <- bd_long_plus %>% mutate( p507_patologiaEsp = case_when( ano_menos_1==year ~ `507. Menos 3  al Menos 1`,
                                                                           ano_mas_1==year ~ `507. Mas 3 al Mas 1`,
                                                                           TRUE ~ "") )     
   
   bd_long_plus <- bd_long_plus %>% mutate( p508_patologiaCual = case_when( ano_menos_1==year ~ `508. Menos 3 al Menos 1`,
                                                                            ano_mas_1==year ~ `508. Mas 3 al Mas 1`,
                                                                            TRUE ~ "") )        
   
   
   bd_long_plus <- bd_long_plus %>% mutate( p508_patologiaOtro = case_when( ano_menos_1==year ~ `508. Menos 3 al Menos 1_Cuál...546`,
                                                                            ano_mas_1==year ~ `508. Mas 3 al Mas 1_Cuál...550`,
                                                                            TRUE ~ "") )        
   
   
   # Seccion  6 y 7 =================================================================
   
   
   bd_long_plus <- bd_long_plus %>% mutate( p601 = `601. Cuál de las siguientes líneas temáticas en salud es la que más se aproxima a su campo de investigación del proyecto de referencia:**`,
                                            p601otra = `601. ¿Cuál otra línea temática?**`,
                                            p701 = `701. Agradecemos que haya atendido a esta encuesta. ¿Quiere hacer alguna observación o dar su opinión sobre esta encuesta?**`,
                                            p702 = `702. Por favor escríbalas a continuación:**`
   )
   
   # Productos, procesos o servicios
   bd_long_plus <- bd_long_plus %>% mutate( p602_nuevosprod = case_when( ano_menos_1==year ~ `602. Menos 3 al Menos 1`,
                                                                         ano_mas_1==year ~ `602. Mas 3 al Mas 1`,
                                                                         TRUE ~ NA) )       
   bd_long_plus <- bd_long_plus %>% mutate( p602_nuevosprod_cual = case_when( ano_menos_1==year ~ `602. Menos 3 al Menos 1_Cuál`,
                                                                              ano_mas_1==year ~ `602. Mas 3 al Mas 1_Cuál`,
                                                                              TRUE ~ NA) )       
   
   bd_long_plus <- bd_long_plus %>% mutate( p603_validaprod = case_when( ano_menos_1==year ~ `603. Menos 3 al Menos 1`,
                                                                         ano_mas_1==year ~ `603. Mas 3 al Mas 1`,
                                                                         TRUE ~ NA) )       
   bd_long_plus <- bd_long_plus %>% mutate( p603_validaprod_cual = case_when( ano_menos_1==year ~ `603. Menos 3 al Menos 1_Cuál`,
                                                                              ano_mas_1==year ~ `603. Mas 3 al Mas 1_Cuál`,
                                                                              TRUE ~ NA) )     
   
   bd_long_plus <- bd_long_plus %>% mutate( p604_mercadoprod = case_when( ano_menos_1==year ~ `604. Menos 3 al Menos 1`,
                                                                          ano_mas_1==year ~ `604. Mas 3 al Mas 1`,
                                                                          TRUE ~ NA) )       
   bd_long_plus <- bd_long_plus %>% mutate( p604_mercadoprod_cual = case_when( ano_menos_1==year ~ `604. Menos 3 al Menos 1_Cuál`,
                                                                               ano_mas_1==year ~ `604. Mas 3 al Mas 1_Cuál`,
                                                                               TRUE ~ NA) )     
   
   # Contibución a comunidades
   
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_a = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1/a. Mejorar el acceso efectivo a los servicios (tiempos de espera, acceso real a tecnologías sanitarias)`,
                                                                          ano_mas_1==year ~ `605. Mas 3 al Mas 1/a. Mejorar el acceso efectivo a los servicios (tiempos de espera, acceso real a tecnologías sanitarias)`,
                                                                          TRUE ~ NA) )     
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_b = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1/b. Mejorar el acceso a la tecnología que afectan a la comunidad (tratamiento para enfermedades altamente prevalentes)`,
                                                                          ano_mas_1==year ~ `605. Mas 3 al Mas 1/b. Mejorar el acceso a la tecnología que afectan a la comunidad (tratamiento para enfermedades altamente prevalentes)`,
                                                                          TRUE ~ NA) )        
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_c = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1/c. Representación de las comunidades en espacios de decisión de la política y administración en salud`,
                                                                          ano_mas_1==year ~ `605. Mas 3 al Mas 1/c. Representación de las comunidades en espacios de decisión de la política y administración en salud`,
                                                                          TRUE ~ NA) )    
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_d = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1/d. Mejora de la calidad de vida del paciente (espiritualidad, cuidado paliativo, etc.)`,
                                                                          ano_mas_1==year ~ `605. Mas 3 al Mas 1/d. Mejora de la calidad de vida del paciente (espiritualidad, cuidado paliativo, etc.)`,
                                                                          TRUE ~ NA) )    
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_e = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1/e. Reconocimiento de conocimientos ancestrales`,
                                                                          ano_mas_1==year ~ `605. Mas 3 al Mas 1/e. Reconocimiento de conocimientos ancestrales`,
                                                                          TRUE ~ NA) )     
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_f = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1/f. Otra, ¿cuál?`,
                                                                          ano_mas_1==year ~ `605. Mas 3 al Mas 1/f. Otra, ¿cuál?`,
                                                                          TRUE ~ NA) )      
   bd_long_plus <- bd_long_plus %>% mutate( p605_contricom_fcual = case_when( ano_menos_1==year ~ `605. Menos 3 al Menos 1_Cuál...584`,
                                                                              ano_mas_1==year ~ `605. Mas 3 al Mas 1_Cuál...593`,
                                                                              TRUE ~ NA) )         
   
   
   # Just keep the relevant variables ============================================
   # colnames(bd_long_plus)

   
   myvars <- c("ID","status","year","G","p201","p215","year_resources",
               "años_estudio","feedback","apelo","ejecuto","control_puro",
               "p205_a","p205_b","p205_c","p205_d","p205_e","p205_f","p205_g","p205_h","p205_i",
               "p214","p217","p218","p220","p301a","p301b","p301c","p301d","p301e","p301f","p302","p303","p303a","p304",
               "p305a","p305b","p305c","p305d","p305e","p305f","p305g","p305h","p306","p307","p402","p403",
               "p404a","p404b","p404c","p405a","p405b","p405c","p405d","p405e","p405f","p405g","p405h","p405i","p405j",
               "p406a","p406b","p406c","p406d","p407a","p407b","p407c","p407d","p408","p409",
               "p501_a","p501_b","p501_c","p501_d","p501_e","p501_f","p501_g","p501_h","p501_i","p502_ciudad","p503_depto",
               "p504_investOtrasCiudades","p504_investOtrasCiudadesCual","p505_datosOtrasCiudades","p505_datosOtrasCiudadesCual",
               "p506_gruposBenef_a","p506_gruposBenef_b","p506_gruposBenef_c","p506_gruposBenef_d","p506_gruposBenef_e","p506_gruposBenef_f","p506_gruposBenef_g","p506_gruposBenef_h","p506_gruposBenef_i","p506_gruposBenef_j","p506_gruposBenef_k","p506_gruposBenef_l",
               "p507_patologiaEsp","p508_patologiaCual","p508_patologiaOtro","p602_nuevosprod","p602_nuevosprod_cual",
               "p603_validaprod","p603_validaprod_cual",
               "p605_contricom_a","p605_contricom_b","p605_contricom_c","p605_contricom_d","p605_contricom_e","p605_contricom_f","p605_contricom_fcual")
   bd_long_final <- bd_long_plus[myvars]  
   
   # Example Callaway Sant’Anna ============================================
   library(did)
   
   # Acá se estima el efecto
   out <- att_gt(yname = "p301f",
                 tname = "year",
                 idname = "ID",
                 gname = "G",
                 xformla = ~1,
                 data = bd_long_final,
                 allow_unbalanced_panel = TRUE,
                 control_group="notyettreated"
   )   
   summary(out)
   
   es <- aggte(out, type = "dynamic", na.rm = TRUE)
   es
   # Acá termina para estimar el efecto

   # Preparo una base para graficar
   bd_graficas <- bd_long_final %>% filter((control_puro==1 & (year==p201 | year == p201-1))|
                                             (control_puro==0 & (year==G | year == G-1) & G<2019)) %>%
     mutate(año = if_else(year==G | year == p201,0,-1)) %>%
     select(ID, control_puro, año, everything())
   bd_graficas <- bd_graficas %>% mutate(tratamiento = if_else(control_puro == 1, 0, 1))
   bd_graficas <- bd_graficas %>% mutate(tratamiento=factor(tratamiento, levels = c(0, 1), labels = c("control", "tratamiento")),
                                             año = factor(año, levels = c(-1, 0), labels = c("Pre", "Post")))
   
   
   # Grafico tendencias promedio
   crear_grafica <- function(data, variable, titulo_base, output_path) {
     # Crear un símbolo para la variable de interés
     variable_sym <- rlang::sym(variable)
     
     # Calcular los resúmenes por año y TyC
     summary_data <- data %>%
       group_by(año, tratamiento) %>%
       summarise(
         mean = mean(!!variable_sym, na.rm = TRUE),
         sd = sd(!!variable_sym, na.rm = TRUE),
         n = n(),
         se = sd / sqrt(n),
         lower = mean - qt(1 - 0.1 / 2, n - 1) * se,
         upper = mean + qt(1 - 0.1 / 2, n - 1) * se
       ) %>%
       ungroup()
     
     # Crear la gráfica
     figura <- ggplot(summary_data, aes(x = año, y = mean, color = tratamiento, group = tratamiento)) +
       geom_line() +
       geom_point() +
       geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
       labs(title = titulo_base, x = "Año", y = "Promedio") +
       theme_minimal()
     
     ggsave(filename = output_path, plot = figura, device = "png")
     figura
     
   }
   
   #crear_grafica(bd_graficas, "p307", "P307","outputs/tendencia_p307.png")
   crear_grafica(bd_graficas, "p403", "Ingresos","outputs/tendencia_ingresos.png")
   crear_grafica(bd_graficas, "p305f", "Referenciados en política pública en salud","outputs/tendencia_ref_pol_publica.png")
   crear_grafica(bd_graficas, "p305a", "Logró mejora salarial","outputs/tendencia_mejora_salarial.png")
   crear_grafica(bd_graficas, "p305b", "Logró publicar artículos","outputs/tendencia_publicó_artículos.png")
   crear_grafica(bd_graficas, "p305c", "Logró publicar manuales","outputs/tendencia_publicó_manuales.png")
   crear_grafica(bd_graficas, "p305d", "Logró publicar libros","outputs/tendencia_publicó_libros.png")
   crear_grafica(bd_graficas, "p305g", "Logró cambiar posición laboral","outputs/tendencia_cambió_posición_lab.png")
   crear_grafica(bd_graficas, "p305h", "Logró cambiar de empleo","outputs/tendencia_cambió_empleo.png")
   crear_grafica(bd_graficas, "p220", "Proyectos en que usó evidencia experimental","outputs/tendencia_evidencia_experimental.png")
   
   # COMENTARIOS
    # Deflectar plata
    # Para variables dummy como la p301a o p304 no hay estimación
   
   # HALLAZGOS
    # La probabilidad de financiar algún prodcto o patente aumentó en 32.4 pp.
    # La probabilidad de mejora salarial aumentó en 33.9 pp.
    # La probabilidad de publicar más artículos aumentó en 44.2 pp.
    # La probabilidad de publicar manuales aumentó en 18.2 pp.
    # La probabilidad de publicar libros aumentó en 21.1 pp.
    # La probabilidad de ser referenciado en documentos de política pública en salud aumentó en 15.3 pp.
    # La probabilidad de cambiar de posición laboral aumentó en 14.8 pp., pero no cambian de empleo
    # La probabilidad de desarrollar una patente o producto de innovación médica aumentó en 13.1 pp
   
   # Tareas:
      # 1. Cruzar temática de encuesta con DB (No hubo ninguna adicional)
      # 2. Gráfica de outcome interesante para pulir gráfica
      # 3. Armar tabla de outcomes y comaprtir avances
   
   
   