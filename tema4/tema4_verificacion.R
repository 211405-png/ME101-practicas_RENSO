library(tidyverse)

# --- Completa: lee el CSV
df <- read_csv("C:/Users/PC TEC IMPORT/Downloads/estudiantes_limpio.xls")

glimpse(df)

df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  print()

# Completa: aplica drop_na() y replace_na() encadenados con el pipe
df_limpio <- df %>%
  drop_na(nota) %>%
  mutate(asistencia_pct = replace_na(asistencia_pct, mean(asistencia_pct, na.rm = TRUE)))

# Verificación
df_limpio %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% print()

# Completa: promedio de nota agrupado por curso, estilo dplyr
promedio_por_curso <- df_limpio %>%
  group_by(curso) %>%
  summarise(promedio_nota = mean(nota), .groups = "drop")

print(promedio_por_curso)

# --- Guardar ---
write_csv(df_limpio, "estudiantes_limpio_R.csv")

