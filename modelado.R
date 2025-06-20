install.packages("languageserver")

print("hello")
install.packages("tidyverse")  
install.packages("cluster")    
install.packages("factoextra")
install.packages("arules")    
install.packages("arulesViz") 
install.packages("RColorBrewer")


# Cargamos las librearias necesarias

library(tidyverse)  # Colección de paquetes para ciencia de datos (dplyr, ggplot2, readr, etc.). ùtil para archivos CSV grandes
library(cluster)    # Para algoritmos de clustering
library(factoextra) # Para visualizar y evaluar resultados de clustering
library(arules)     # Para minería de reglas de asociación (Market Basket Analysis)
library(arulesViz)  # Para visualizar reglas de asociación
library(RColorBrewer) # Paletas de colores

# --- Configuraciones ---
theme_set(theme_bw()) # Establecer un tema visual limpio para los gráficos


# Cargar el dataset
# Suponiendo que la primera columna es el ID del pedido
df_pedidos <- read_csv("C:/Users/mocni/OneDrive/Escritorio/Andres/BasketAnalysis/pedidos_transformados.csv", col_types = cols(.default = "i"))

# Vistazo inicial
cat("Dimensiones del dataset:", dim(df_pedidos), "\n")
glimpse(df_pedidos) # Una versión mejorada de str()

# Estadísticas descriptivas
summary(df_pedidos %>% select(-1)) # Excluimos la columna de ID



# Análisis de productos más vendidos
# Necesitamos transformar los datos de formato ancho a largo para graficar con ggplot2
productos_populares <- df_pedidos %>%
  select(-1) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(everything(), names_to = "producto", values_to = "total_unidades") %>%
  arrange(desc(total_unidades))



# Graficar el Top 20
productos_populares %>%
  top_n(20, total_unidades) %>%
  ggplot(aes(x = reorder(producto, total_unidades), y = total_unidades)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Facilita la lectura de los nombres de los productos
  labs(title = "Top 20 Productos Más Vendidos (por unidades)",
       x = "Producto",
       y = "Total de Unidades Vendidas")


