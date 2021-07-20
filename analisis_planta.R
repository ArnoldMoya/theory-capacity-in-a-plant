library(readxl)
library(writexl)
library(dplyr)
# PRODUCCION
datos.produccion = read_excel("D:/datos_produccion.xlsx")
produccion = data.frame(datos.produccion)
# Seleccion de las filas importantes
produccion <- produccion %>% select(Año,UNIDAD,Producto,NOM_PROD,FECHA,COD,HORAS,
              TON_HORA,N_COMP, Nom..Almacen) %>%
              filter(TON_HORA>0,HORAS>0) %>% 
              filter(!grepl("NO CONFORME",Nom..Almacen,fixed = TRUE))
# Filtrado de datos válidos
# summary(produccion)

# PRODUCTOS
datos.productos = read_excel("D:/COMACSA/lista_productos.xlsx")
productos = data.frame(datos.productos)
# summary(productos)

# CAPACIDADES: inicializadas en 0
capacidades <- vector(length = nrow(productos))

for (i in 1:nrow(productos)){ #longitud total nrow(productos)
  if (productos$ES_UNIDAD[i]){
    unidad = productos$NOM_PROD[i]
    #print(unidad)
  }
  if (productos$ES_PRODUCTO[i]){
    nom_prod = productos$NOM_PROD[i]
    tabla_filtrada <- produccion %>% 
      filter(UNIDAD==unidad,nom_prod==NOM_PROD) #%>%
      #filter(Año != 2020) #sin 2020
    capacidades[i] <- quantile(tabla_filtrada$TON_HORA,0.75)
    #print(capacidades[i])
  }
}

# Escribe un archivo excel con las capacidades
write_xlsx(data.frame(capacidades),path = "D:/COMACSA/capacidades_con2020.xlsx",
                  col_names = TRUE)
