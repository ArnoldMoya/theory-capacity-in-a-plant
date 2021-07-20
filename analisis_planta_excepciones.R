library(readxl)
library(writexl)
library(dplyr)
# PRODUCCION
datos.produccion = read_excel("D:/COMACSA/datos_produccion.xlsx")
produccion = data.frame(datos.produccion)
# Seleccion de las filas importantes
produccion <- produccion %>% select(Año,UNIDAD,Producto,NOM_PROD,FECHA,COD,HORAS,
              TON_HORA,N_COMP, Nom..Almacen) %>%
              filter(TON_HORA>0,HORAS>0) %>% 
              filter(!grepl("NO CONFORME",Nom..Almacen,fixed = TRUE))# Filtrado de datos válidos
# summary(produccion)

# PRODUCTOS
datos.productos = read_excel("D:/COMACSA/lista_productos.xlsx")
productos = data.frame(datos.productos)
# summary(productos)

# CAPACIDADES: inicializadas en 0
capacidades <- vector(length = nrow(productos))

#nrow(productos)
for (i in 1:nrow(productos)){ 
  
  if(!is.na(productos$S[i])){
    #print(paste(productos$S[i],i))
    filtro <-  productos %>% filter(S==productos$S[i])
    #head(filtro)
    tabla_filtrada <- produccion %>% filter(UNIDAD==productos$MOLINO[i]) %>%
      filter(COD %in% filtro$COD)# %>% filter(Año != 2020)
    #head(tabla_filtrada)
    capacidades[i] <- quantile(tabla_filtrada$TON_HORA,0.75)
  }
}

# Escribe un archivo excel con las capacidades
write_xlsx(data.frame(capacidades),path = "D:/COMACSA/capacidades_especial_con2020.xlsx",
           col_names = TRUE)