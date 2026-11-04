library(tidyverse)
library(pheatmap)

# 1. Cargar el dataset y calcular la matriz de correlación
datos <- read.csv("C:/Users/RENSO/Downloads/datos_ambiental_evaluacion_tema9 - datos_ambiental_evaluacion_tema9.csv")
matriz_corr <- cor(datos[, c("pm10_ugm3", "no2_ppb", "ruido_db")], use = "complete.obs")

print("Matriz de correlación:")
print(round(matriz_corr, 4))

# ----------------------------------------------------------------------
# TAREA 2: ggplot2 (Réplica de Seaborn)
# ----------------------------------------------------------------------

# Convertir la matriz a formato largo (Tidy Data)
corr_larga <- matriz_corr %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlacion") %>%
  mutate(
    Var1 = factor(Var1, levels = colnames(matriz_corr)),
    Var2 = factor(Var2, levels = rev(colnames(matriz_corr)))
  )

# A) Heatmap ggplot2 Completo
p_ggplot_completo <- ggplot(corr_larga, aes(x = Var1, y = Var2, fill = Correlacion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlacion)), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#3B4CC0", mid = "#F2F2F2", high = "#B40426",
    midpoint = 0, limits = c(-1, 1), name = "Correlación"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  ) +
  labs(title = "Mapa de Calor de Correlación (ggplot2)")

# Mostrar en el panel Plots e imprimir/guardar
print(p_ggplot_completo)
ggsave("heatmap_ggplot2.png", plot = p_ggplot_completo, width = 6, height = 5, dpi = 300)

# B) Heatmap ggplot2 con Máscara (Lógica de filtrado corregida)
corr_larga_mascara <- corr_larga %>%
  filter(as.numeric(Var1) >= (4 - as.numeric(Var2)))

p_ggplot_mascara <- ggplot(corr_larga_mascara, aes(x = Var1, y = Var2, fill = Correlacion)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlacion)), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#3B4CC0", mid = "#F2F2F2", high = "#B40426",
    midpoint = 0, limits = c(-1, 1), name = "Correlación"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  ) +
  labs(title = "Mapa de Calor con Máscara (ggplot2)")

# Mostrar en el panel Plots e imprimir/guardar
print(p_ggplot_mascara)
ggsave("heatmap_ggplot2_mascara.png", plot = p_ggplot_mascara, width = 6, height = 5, dpi = 300)


# ----------------------------------------------------------------------
# TAREA 3: pheatmap (Muestra en pantalla Y guarda en archivo)
# ----------------------------------------------------------------------

paleta_coolwarm <- colorRampPalette(c("#3B4CC0", "#F2F2F2", "#B40426"))(100)

# 1. pheatmap CON agrupamiento jerárquico
ph_agrupado <- pheatmap(
  matriz_corr,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = paleta_coolwarm,
  breaks = seq(-1, 1, length.out = 101),
  border_color = "white",
  main = "Mapa de Calor Agrupado (pheatmap)"
)

# Guardar explícitamente a archivo después de renderizar en pantalla
ggsave("heatmap_pheatmap_agrupado.png", plot = ph_agrupado, width = 6, height = 5, dpi = 300)

# 2. pheatmap SIN agrupamiento jerárquico
ph_sin_agrupar <- pheatmap(
  matriz_corr,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = paleta_coolwarm,
  breaks = seq(-1, 1, length.out = 101),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  border_color = "white",
  main = "Mapa de Calor Sin Agrupar (pheatmap)"
)

# Guardar explícitamente a archivo
ggsave("heatmap_pheatmap_sin_agrupar.png", plot = ph_sin_agrupar, width = 6, height = 5, dpi = 300)

