# Instalar paquetes si no los tienes
if(!require(data.table)) install.packages("data.table")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")

# Cargar librerías
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)


# Ruta a tu archivo CSV (ajusta la ruta según sea necesario)
# En RStudio, puedes usar: file_path <- file.choose() para seleccionarlo interactivamente
file_path <- "C:/Users/mocni/OneDrive/Escritorio/Andres/BasketAnalysis/pedidos_transformados.csv"

# Cargar los datos
# Asumimos que la primera columna es ID_Pedido_Transformado y el resto son productos
pedidos_dt <- fread(file_path)

# Ver las dimensiones del dataset (filas = pedidos, columnas = ID + productos)
cat("Dimensiones del dataset:\n")
print(dim(pedidos_dt))

# Ver las primeras filas y los nombres de las columnas
cat("\nPrimeras filas del dataset:\n")
print(head(pedidos_dt[, 1:6])) # Mostramos solo las primeras 6 columnas para que sea legible


#TOP PRODUCTOS MAS VENDIDOS


# Asumimos que la primera columna es el ID del pedido y no es un producto
# Seleccionamos solo las columnas de productos
producto_cols <- setdiff(names(pedidos_dt), "ID_Pedido_Transformado")

# Calculamos la suma de unidades vendidas para cada producto
producto_frecuencia <- sapply(pedidos_dt[, ..producto_cols], sum)

# Creamos un data frame para visualizarlo fácilmente con ggplot2
top_productos_df <- data.frame(
  Producto = names(producto_frecuencia),
  UnidadesVendidas = producto_frecuencia
) %>%
  arrange(desc(UnidadesVendidas)) %>%
  top_n(20, UnidadesVendidas)

# Graficar los top 20 productos
ggplot(top_productos_df, aes(x = reorder(Producto, UnidadesVendidas), y = UnidadesVendidas)) +
  geom_bar(stat = "identity", fill = "#0073C2FF") +
  coord_flip() + # Para que las etiquetas de los productos se lean mejor
  labs(
    title = "Top 20 Productos Más Vendidos",
    subtitle = "Basado en el total de unidades vendidas en todos los pedidos",
    x = "Producto",
    y = "Total de Unidades Vendidas"
  ) +
  theme_minimal()




#DISTRIBUCION DE LOS PEDIDOS

# Asumiendo que 'pedidos_dt' ya está cargado y tiene la columna 'tamano_pedido'

# Primero, veamos dónde se concentra la mayoría de los datos
# Calculamos percentiles para decidir dónde "cortar" para la visualización
percentiles <- quantile(pedidos_dt$tamano_pedido, probs = c(0.9, 0.95, 0.99))
cat("Percentiles del tamaño del pedido:\n")
print(percentiles)
# Esto nos dirá, por ejemplo, que el 99% de los pedidos tienen menos de X ítems.
# Usemos un límite razonable, por ejemplo 60, para la visualización.

# Graficar un histograma filtrando los pedidos extremadamente grandes para una mejor visualización
ggplot(pedidos_dt %>% filter(tamano_pedido > 0 & tamano_pedido < 60), aes(x = tamano_pedido)) +
  geom_histogram(binwidth = 2, fill = "salmon", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = median(tamano_pedido)), color = "blue", linetype = "dashed", size = 1) +
  scale_x_continuous(breaks = seq(0, 60, 5)) + # Ejes más claros
  labs(
    title = "Distribución del Tamaño de los Pedidos (Rango Principal)",
    subtitle = "Enfocado en pedidos de 1 a 60 ítems. La línea azul indica la mediana.",
    x = "Número de Ítems en el Pedido",
    y = "Frecuencia (Nº de Pedidos)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))