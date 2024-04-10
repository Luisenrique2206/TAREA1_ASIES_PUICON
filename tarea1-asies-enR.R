# Pregunta 1: ¿La edad media del contagiado de dengue varió a través de los años?
ggplot(datos_abiertos_vigilancia_dengue[datos_abiertos_vigilancia_dengue$ano == 2020,], aes(x = departamento, y = enfermedad, fill = departamento)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Casos de Dengue por Departamento en 2020", x = "Departamento", y = "Número de Casos", fill = "Departamento")

# Pregunta 2: ¿La cantidad de casos varió a través de los años? ¿Fue lo mismo según gravedad?
datos_abiertos_vigilancia_dengue %>%
  group_by(ano, enfermedad) %>%
  summarise(Casos = n()) %>%
  ggplot(aes(x = factor(ano), y = Casos, fill = enfermedad)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Cantidad de Casos de Dengue por Año y Gravedad", x = "Año", y = "Número de Casos", fill = "Gravedad") +
  theme(plot.title = element_text(hjust = 0.5))

edad_media <- datos_abiertos_vigilancia_dengue %>%
  group_by(ano, enfermedad) %>%
  summarise(EdadMedia = mean(`edad`, na.rm = TRUE))
ggplot(edad_media, aes(x = factor(`ano`), y = EdadMedia, color = enfermedad)) +
  geom_line() +
  geom_point() +
  labs(title = "Edad Media del Contagiado de Dengue por Año y Gravedad", x = "Año", y = "Edad Media") +
  theme_minimal()

# Pregunta 3: ¿Cuáles fueron las provincias más afectadas con contagiados a lo largo de los años?
datos_abiertos_vigilancia_dengue %>%
  group_by(ano, provincia) %>%
  summarise(Casos = n()) %>%
  top_n(10, wt = Casos) %>%
  ggplot(aes(x = factor(ano), y = Casos, fill = provincia)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Provincias Más Afectadas con Contagiados de Dengue", x = "Año", y = "Número de Casos", fill = "Provincia") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_tableau()

# Pregunta 4: ¿Qué relación había entre los casos alarmantes no graves y los asintomáticos según departamento a lo largo de los años?
datos_abiertos_vigilancia_dengue %>%
  filter(enfermedad %in% c("Alarmante No Grave", "Asintomático")) %>%
  group_by(ano, departamento, enfermedad) %>%
  summarise(Casos = n()) %>%
  ggplot(aes(x = factor(ano), y = Casos, fill = enfermedad)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Relación entre Casos Alarmantes No Graves y Asintomáticos por Departamento", x = "Año", y = "Número de Casos", fill = "Gravedad") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_tableau()