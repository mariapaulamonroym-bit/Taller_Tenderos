library(dplyr)
library(haven)
library(tidyr)
library(scales)

tenderos <- read_dta("https://raw.githubusercontent.com/mariapaulamonroym-bit/Taller_Tenderos/main/TenderosFU03_Publica.dta")

#Tarea 1
#Objetivo: pasar de la base a nivel de TIENDA a una base a nivel de CIUDAD, calculando la proporción de tenderos que usan internet en cada ciudad.
cobertura_internet <- tenderos %>%
  mutate(uso_internet = as_factor(uso_internet),
         uso_internet_num = if_else(uso_internet == "Sí", 1, 0)) %>%
  group_by(divipola = Munic_Dept, municipio = Municipio) %>%
  summarise(
    n_tiendas = n(),
    internet  = mean(uso_internet_num, na.rm = TRUE),
    .groups = "drop"
  )
#Tarea 2: cobertura de internet por SECTOR ECONÓMICO

actividades <- c(actG1="Tienda", actG2="Comida preparada", actG3="Peluqueria y bellez",
                 actG4="Ropa", actG5="Otras variedades", actG6="Papelería y comunicaciones",
                 actG7="Vida nocturna", actG8="Productos bajo inventario", actG9="Salud",
                 actG10="Servicos", actG11="Ferretería y afines")

internet_por_actividad <- tenderos %>%
  mutate(uso_internet_num = if_else(as_factor(uso_internet) == "Sí", 1, 0)) %>%
  pivot_longer(cols = actG1:actG11, names_to = "actG", values_to = "tiene_actividad") %>%
  filter(tiene_actividad == 1) %>%                       # se queda solo con las actividades marcadas
  mutate(Actividad = recode(actG, !!!actividades)) %>%
  group_by(actG, Actividad) %>%
  summarise(internet = percent(mean(uso_internet_num, na.rm = TRUE), accuracy = 1),
            .groups = "drop")

#Tarea 3: cobertura de internet por CIUDAD y por SECTOR

internet_por_actividad_ciudad <- tenderos %>%
  mutate(uso_internet_num = if_else(as_factor(uso_internet) == "Sí", 1, 0)) %>%
  pivot_longer(cols = actG1:actG11, names_to = "actG", values_to = "tiene_actividad") %>%
  filter(tiene_actividad == 1) %>%                       # se queda solo con las actividades marcadas
  mutate(Actividad = recode(actG, !!!actividades)) %>%
  group_by(Munic_Dept, Municipio, actG, Actividad) %>%
  summarise(internet = percent(mean(uso_internet_num, na.rm = TRUE), accuracy = 1),
            .groups = "drop"
  )

#Tarea 4: No se pudo realizar la tarea 4 debido a un error en la base de datos de https://terridata.dnp.gov.co/
#No se puedo realizar la recolección de los datos propuestos

#Tarea 5: base larga y base extensa (la Tarea 3 fue usada directamente)

# Base larga:
base_larga <- internet_por_actividad_ciudad %>%
  rename(divipola = Munic_Dept, municipio = Municipio)

# Base extensa:
base_extensa <- base_larga %>%
  select(divipola, municipio, actG, internet) %>%
  pivot_wider(names_from = actG, values_from = internet, names_prefix = "internet_")
