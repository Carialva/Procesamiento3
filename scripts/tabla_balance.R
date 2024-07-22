# 
# Proyecto: FIS
# Objetivo: Análisis de impacto del Fondo para Investigación en Salud
# Este script: Genera tablas de balance y es llamado por el script "script"
# Autor: Maria Camila Arias Álvarez - Econometría Consultores S.A.S.
# Fecha de creación: 27/06/2024
# última modificación: 27/06/2024
# 


library(modelsummary)
library(fastDummies)
library(writexl)

# Crear el gráfico de controles y tratamientos por año
e1 <- ggplot(status_counts, aes(x = year, y = count, fill = factor(status))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Año", y = "Investigadores", fill = "Tipo") +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, size = 3) +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
  scale_x_continuous(breaks=seq(2011,2020,1)) 

ggsave("outputs/tipo_por_año.pdf", e1)

# Gráfico de nuevos tratados por año
counts <- bd %>%
  count(`203. ¿En qué año le fue otorgado?`)

e2 <- ggplot(bd, aes(x = `203. ¿En qué año le fue otorgado?`)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue", boundary = 0.5) + 
  geom_text(data = counts, aes(x = `203. ¿En qué año le fue otorgado?`, y = n, label = n), 
            vjust = -0.3, size = 3) +
  labs(x = "Primer año que recibió FIS", y = "Investigadores") +
  scale_x_continuous(breaks=seq(2011,2022,1)) +
  #scale_y_continuous(breaks=seq(0,40,5)) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggsave("outputs/variación_anual.pdf", e2)

cov<- bd %>% 
  dplyr::select(`102. Edad`,
                años_estudio,
                `211. Califique de 1 a 10 el método de evaluación que lo valoró, siendo 1 nada adecuado y 10 muy adecuado**`,
                feedback,
                apelo,
                ejecuto,
                starts_with("status"))

balance_tables <- list()

for (year in 2011:2019) {
  status_var <- paste0("status",year)
  cov_real <- cov %>% dplyr::select(`102. Edad`,
                                    años_estudio,
                                    `211. Califique de 1 a 10 el método de evaluación que lo valoró, siendo 1 nada adecuado y 10 muy adecuado**`,
                                    feedback,
                                    apelo,
                                    ejecuto,
                                    status_var)
  balance_table <- datasummary_balance(as.formula(paste("~",status_var)),cov_real,dinm_statistic="p.value",  output="data.frame")
  balance_tables[[status_var]] <- balance_table
}


# Crear un solo dataframe combinando todas las tablas de balance
combined_df <- do.call(rbind, lapply(names(balance_tables), function(name) {
  df <- balance_tables[[name]]
  df$Year <- name
  df
}))

# Escribir el dataframe combinado en un archivo Excel
write_xlsx(combined_df, "outputs/tabla_balance.xlsx")