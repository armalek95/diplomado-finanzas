
source("scripts/calcular_abs.R", local = knitr::knit_global())
source("scripts/sim_tabletas.R", local = knitr::knit_global())

# Empezar con las opciones de formulacion

formulaciones <- c("E1", "E2") 
# faltan opciones para "E1-E1", "E1-E2", "E1-E1-E2"

# opciones de compuestos activos para formulacion "E1"
opciones_E1 <- c("Phenylephrine Hydrochloride", "Diphenhydramine hydrochloride")

# opciones de compuestos activos para formulacion E2
opciones_E2 <- c("Acetaminophen")

# funcion que da como resultado las dimensiones de la tableta
rand_dimensiones <- function(formulacion){
  if (formulacion == "E1") {
    return(tibble(variable=c("z", "kappa", "alpha", "omega"), 
                  valor=c(0.6, 0.05, 2, 0.05))) 
  } else if (formulacion == "E2") {
    return (tibble(variable=c("z", "kappa", "alpha", "omega"), 
                   valor=c(0.6, 0.05, 2, 0.05)))
  }
}

# Funcion que da como resultado la composicion dependiendo de la formulacion
# de la tableta
ret_composicion <- function(formulacion){
  if (formulacion == "E1") {
    return(composicion[,c("Compound", "Composicion_E1")] 
           %>% filter(Composicion_E1 >0) %>% rename(Composicion=Composicion_E1))
  } else if (formulacion == "E2") {
    return(composicion[,c("Compound", "Composicion_E2")] 
           %>% filter(Composicion_E2 >0) %>% rename(Composicion=Composicion_E2))
  }
}

# cargar composicion de las formulaciones
composicion <- read_csv("datos/composicion.csv", show_col_types = FALSE)

# cargar informacion de los compuestos
info_compuestos <- read_csv("datos/compounds.csv", show_col_types = FALSE)

tipo_cambio = 18

info_compuestos <- info_compuestos[1:5] %>%
  mutate(Precio_mxn_g=Price_kg_usd*tipo_cambio/1000) %>% select(-(Price_kg_usd))


# Definir parametros para simulacion
sim_num <- 10

# Crear tabla para iniciar la simulacion
simulaciones <- tibble(sim = 1:sim_num, 
                       formulacion=sample(formulaciones, 10, replace = TRUE))

# Agregar compuesto activo
simulaciones <- simulaciones %>%
  mutate(compuesto_activo = map_chr(formulacion, compuesto))

# Agregar dosis
simulaciones <- simulaciones %>%
  mutate(dosis = map_dbl(compuesto_activo, dosis),
         dimensiones = map(.f=rand_dimensiones, formulacion),
         composicion = map(.f=ret_composicion, formulacion))

# Calcular los materiales que componen a la tableta de acuerdo a las 
# especificaciones de la misma
simulaciones <- simulaciones %>% 
  mutate(materiales=
           pmap(list(formulacion, 
                compuesto_activo, 
                dosis, dimensiones, composicion), 
                .f=costo_material))

# Funcion para calcular el costo de los materiales

costo_material <- function(materiales, tabletas) {
  costo <- materiales %>% 
    mutate(costo=masa*Precio_mxn_g) %>%
    select(costo) %>%
    sum()
  
  return(costo * tabletas + costo_frasco)
}

# Funcion para calcular el costo de labor asociado

costo_labor <- function(tabletas, t_fabricacion) {
  costo <- (t_fabricacion/tabletas) * salario_trabajador / eficiencia_trabajador
  return(costo)
}

# Funcion para calcular el costo energetico

costo_energia <- function(materiales, T0=25, T1=82, T2=65, t_impresion, 
                          c_electricidad, c_e_impresora, formulacion) {
  if (formulacion == "E1") {
    calor <- materiales %>% 
      filter(Componente == "MI" | Componente == "ME") %>% mutate()
  }
}

simulaciones[2, ]

simulaciones$materiales[[3]]

simulaciones$materiales[[2]] %>% 
  filter(Componente == "MI" | Componente == "ME") %>% 
  mutate(calor = masa * Specific_Heat * (20) + masa * Fusion_Heat) %>%
  select(calor) %>% sum()

simulaciones$materiales[[3]] %>% mutate(costo=masa*Precio_mxn_g) %>%
  select(costo) %>% sum()
