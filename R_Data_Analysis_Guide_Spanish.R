# ================================================================
# 📘 GUÍA PARA ANALIZAR DATOS CON R + FUNCIONES
# Autor: Pablo
# Creado: 2025
# Última actualización: 11/04/2025
# Propósito: Guía práctica para análisis de datos con R (tidyverse)
# Licencia: Compartido públicamente solo para portafolio y fines educativos.
# ================================================================

## ================================================================
## 1. USO DE PAQUETES EN R
## ================================================================
# install.packages() → Instalar paquetes en R
install.packages("tidyverse")

# library() → Cargar paquetes
library(tidyverse)


## ================================================================
## 2. VISUALIZAR DATOS
## ================================================================
# read_...() → Importar archivos de datos
hotel <- read_csv(".../datasets/csv/hotel_bookings.csv")

# skim_without_charts() → Resumen detallado sin gráficos
skim_without_charts(penguins)

# head() → Mostrar primeras filas de un dataset
head(diamonds)

# View() → Abrir dataset en una nueva pestaña
View(diamonds)

# str() o glimpse() → Mostrar estructura del dataset
str(diamonds)
glimpse(diamonds)

# colnames() → Mostrar nombres de columnas
colnames(diamonds)

# slice_sample() → Extraer muestra aleatoria con n filas
diamonds_sample <- diamonds |>
  slice_sample(n = 100)


## ================================================================
## 3. ORGANIZAR DATOS
## ================================================================
# arrange() → Ordenar datos (ascendente / descendente)
arrange(penguins, bill_length_mm)
arrange(penguins, -bill_length_mm)
penguins |> arrange(bill_length_mm)

# filter() → Filtrar filas por condiciones
filter(penguins, species == "Adelie")

# pivot_wider() → Convertir de formato largo a ancho
wide_table <- employee |>
  pivot_wider(
    names_from = job_title,
    values_from = first_name
  )

# pivot_longer() → Convertir de ancho a largo
long_table <- wide_table |>
  pivot_longer(
    cols = c(Professional, Programmer, Management, Clerical, Developer),
    names_to = "job_title",
    values_to = "first_name",
    values_drop_na = TRUE
  )


## ================================================================
## 4. LIMPIEZA DE DATOS
## ================================================================
# select() → Seleccionar columnas específicas
select(penguins, species)
select(penguins, -species)
penguins |> select(species)

# drop_na() → Eliminar filas con valores faltantes
drop_na(penguins)

# trimws() → Quitar espacios en blanco en texto
light_power_cars$model <- trimws(light_power_cars$model)

# rename() → Renombrar columnas
rename(diamonds, carat_new = carat)
rename(diamonds, carat_new = carat, cut_new = cut)

# rename_with() → Cambiar formato de nombre de columna
rename_with(diamonds, toupper)
rename_with(diamonds, tolower)

# clean_names() → Asegurar nombres de columna válidos
clean_names(penguins)

# bias() → Revisar sesgo entre datos reales y predichos
actual_sales <- c(150, 203, 137, 247, 116, 287)
predicted_sales <- c(200, 300, 150, 250, 150, 300)
bias(actual_sales, predicted_sales) # -35 → sesgo claro


## ================================================================
## 5. MANIPULACIÓN DE DATOS
## ================================================================
# unite() → Combinar varias columnas en una sola
hotel |> unite(arrival_month_year, c("arrival_date_month", "arrival_date_year"), sep = " ")
employee |> unite(name, first_name, last_name, sep = " ")

# separate() → Dividir una columna en varias
employee |> separate(name, into = c("first_name", "last_name"), sep = " ")

# mutate() → Crear o modificar columnas
diamonds |> mutate(carat_2 = carat * 100)

# group_by() + summarise() → Resúmenes por grupo
penguins |> 
  group_by(island) |> 
  drop_na() |> 
  summarise(mean_bill_length_mm = mean(bill_length_mm))

# Múltiples métricas + filtrado
penguins |> 
  group_by(island, species) |> 
  drop_na() |> 
  summarise(
    min_bl = min(bill_length_mm),
    max_bl = max(bill_length_mm),
    mean_bl = mean(bill_length_mm)
  ) |> 
  filter(species == "Adelie")

# sum(), sd(), cor(), mean()
quartet |> summarise(sd(x))
quartet |> summarise(cor(x, y))
mean(hotel$lead_time)
hotel |> summarise(mean(lead_time))

# Ejemplo de resumen básico
hotel_summary <- hotel |>
  group_by(hotel) |>
  summarise(
    average_lead_time = mean(lead_time),
    min_lead_time = min(lead_time),
    max_lead_time = max(lead_time)
  )


## ================================================================
## 6. VISUALIZAR DATOS
## ================================================================
# ggplot() → Crear visualizaciones
ggplot(diamonds, aes(carat, price, color = cut)) + geom_point()

# facet_wrap() y facet_grid() → Múltiples gráficos por variable(s)
ggplot(diamonds, aes(carat, price, color = cut)) + facet_wrap(~cut)
ggplot(penguins, aes(x = flipper_len, y = body_mass, color = species)) +
  geom_point() +
  facet_grid(sex ~ species)

# reorder(), labs(), annotate(), paste0(), theme()
light_power_cars |>
  ggplot() +
  geom_col(aes(x = reorder(Model, -HorsePower), HorsePower)) +
  labs(
    title = "Cars under 1200kg",
    x = "Car Model",
    y = "Horsepower (HP)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 15, face = "bold")
  )

# combined_plot() → Combinar múltiples gráficos
combined_plot <- (plot1 + plot2) / (plot3 + plot4)

# geom_bar() → Conteo automático por categoría
diamonds |> ggplot(aes(cut)) + geom_bar()

# scale_fill_manual() → Escalas de color personalizadas
scale_fill_manual(values = c("#1F77B4", "#2CA02C", "#FF7F0E", "#9467BD", "#7F7F7F"))

# ggsave() → Guardar el último gráfico
setwd(".../R/plots")
ggsave("Cars_dashboard.png")


## ================================================================
## 7. DOCUMENTACIÓN
## ================================================================
# Documenta tu análisis en Markdown o R Notebooks
# Combina texto + código + gráficos para reproducibilidad


## ================================================================
## APÉNDICE
## ================================================================
# Guarda resultados intermedios como nuevos objetos
# Ejemplo: new_dataframe <- original_dataframe |> filter(...) |> arrange(...)
