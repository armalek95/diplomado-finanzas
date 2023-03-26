
# funcion para elegir un compuesto de forma aleatoria
compuesto <- function(formulacion) {
  if (formulacion == "E1") {
    return(sample(opciones_E1,1,replace=TRUE))
  } else if (formulacion == "E2") {
    return(sample(opciones_E2,1,replace=TRUE))
  } else{
    return(sample(c(opciones_E1, opciones_E2),1,replace=TRUE))
  }
}

# funcion para elegir la dosis de forma aleatoria
dosis <- function(compuesto_activo) {
  if (compuesto_activo == "Phenylephrine Hydrochloride") {
    return(rnorm(n=1, mean=0.01, sd=0.005))
  } else if (compuesto_activo == "Diphenhydramine hydrochloride") {
    return(rnorm(n=1, mean=0.01, sd=0.005))
  } else{ # Acetaminophen
    return(rnorm(n=1, mean=0.5, sd=0.3))
  }
}

# Funcion para determinar la masa de los compuestos de la matriz interna
compuestos_matriz_interna <- function(compuesto_activo, dosis, composicion, 
                                      rango_bajo=0.04, rango_alto=0.09) {
  
  porcentaje_compuesto_activo <- seq(rango_bajo, rango_alto, by=0.001) %>% 
    sample(1)
  
  masa_matriz = dosis / porcentaje_compuesto_activo
  
  comp <- composicion %>% 
    mutate(masa = masa_matriz * Composicion) %>%
    add_row(Compound=compuesto_activo, Composicion=0, masa=dosis)
  
  return(comp[c(1,3)])
}

# Funcion para determinar la masa de los compuestos de la matriz externa
compuestos_matriz_externa <- function(comp_matriz_interna, compuesto_activo) {
  compuestos_ma_ext <- comp_matriz_interna %>%
    filter(Compound != compuesto_activo) %>%
    mutate(masa = masa * 0.2)
  
  return(compuestos_ma_ext)
}

# Regresar todos los materiales necesarios para la produccion

costo_material <- function(formulacion, CA, dosis, dimensiones, composicion) {
  
  if (formulacion == "E1") {
    
    comp_mi <- compuestos_matriz_interna(compuesto_activo = CA, dosis = dosis,
                                         composicion = composicion)
    comp_me <- compuestos_matriz_externa(comp_matriz_interna = comp_mi, 
                                         compuesto_activo = CA)
  } else if (formulacion == "E2") {
    
    rango_bajo_E2=0.94
    rango_alto_E2=0.98
    
    comp_mi <- compuestos_matriz_interna(compuesto_activo = CA, dosis = dosis,
                                         composicion = composicion, 
                                         rango_bajo=rango_bajo_E2, 
                                         rango_alto=rango_alto_E2)
    
    comp_me <- compuestos_matriz_externa(comp_matriz_interna = comp_mi, 
                                         compuesto_activo = CA)
  } else {
    return(0)
  }
  
  # agregar densidad
  comp_mi <- comp_mi %>% 
    inner_join(info_compuestos[c("Compound", "Density")], by="Compound")
  
  comp_me <- comp_me %>%
    inner_join(info_compuestos[c("Compound", "Density")], by="Compound")
  
  dens_abs <- info_compuestos[info_compuestos$Compound == 
                                "Acrylonitrile Butadiene Styrene",]$Density
  dens_cera <- info_compuestos[info_compuestos$Compound == 
                                 "White Wax",]$Density
  # calcular masa de abs y de cera necesaria
  mabs_cera <- masa_abs_cera(z=dimensiones[1, ]$valor, 
                             kappa=dimensiones[2,]$valor,
                             alpha=dimensiones[3,]$valor, 
                             omega=dimensiones[4,]$valor,
                             densidad_comp_mi = comp_mi$Density,
                             masa_comp_mi = comp_mi$masa,
                             densidad_comp_me = comp_me$Density,
                             masa_comp_me = comp_me$masa,
                             densidad_abs = dens_abs,
                             densidad_cera = dens_cera)
  
  # Agregar etiqueta
  
  comp_me <- comp_me %>%
    mutate(Componente = "ME")
  
  comp_mi <- comp_mi %>%
    mutate(Componente = "MI")
  
  compuestos <- comp_me %>%  
    bind_rows(comp_mi) %>%
    add_row(Compound="White Wax", masa=mabs_cera[2], 
            Density=dens_cera, Componente="CU") %>%
    add_row(Compound="Acrylonitrile Butadiene Styrene", 
            masa=mabs_cera[1], Density=dens_abs, Componente="MO")
  
  # A la lista de compuestos que conforman a nuestra tableta
  # así como al molde, agregamos los valores faltantes
  compuestos <- compuestos %>%
    inner_join(select(info_compuestos,-"Density"), by="Compound")
  
  return(compuestos)
}

info_compuestos %>% select(-"Density")
