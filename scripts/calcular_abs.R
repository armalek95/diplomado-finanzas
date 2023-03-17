# Este script va a alojar el procedimiento para calcular la masa de ABS
# requerida para imprimir los moldes para los componentes de las tabletas
# personalizables

# Paso 1.a: calcular área transversal de la matriz con componente activo
area_maca <- function(mass_comp, densidad_comp, z){
  # mass_comp es la masa de cada compuesto presente en la matriz con compuesto
  # activo en [g]
  # densidad_comp es la densidad de cada compuesto presente en la matriz con 
  # compuesto activo en [g/cm3]
  # z es la longitud de la matriz a lo largo de ese eje
  mass <- sum(mass_comp/densidad_comp)
  
  area_maca <- mass/z
  
  area_maca
}

# Paso 1.b: Calcular x e y
xy_length <- function(relation_y_x, area_maca){
  x <- sqrt(area_maca / (relation_y_x))
  y <- relation_y_x * sqrt(area_maca/(1+relation_y_x))
  
  c(x, y)
}

# Paso 2:
m_maca_abs <- function(x, y, z, k, densidad_abs){
  mass_maca_abs <- 2*k*(x+y+2*k)*densidad_abs
  mass_maca_abs
}

# Paso 3:
v_mas_gamma <- function(mass_comp, densidad_comp, x, y, z){
  v_mas <- sum(mass_comp/densidad_comp)
  gamma <- (-2*(x+y)+sqrt(4*(x+y)^2+16*(sum(mass_comp/densidad_comp)/z)))/8
  c(v_mas, gamma)
}

# Paso 4:
m_mas_abs <- function(densidad_abs, x, y, z, g, k, v_mas){
  v_mas_molde <- ((x+2*(g+k))(y+(g+k))-v_mas) * z
  mass_abs_mas <- v_mas_molde * densidad_abs
  mass_abs_mas
}

# Paso 5:
mv_cera <- function(x, y, z, g, o, densidad_cera){
  volumen_cera <- 2*o*(x+z+2*(g+o))*(y+2*o)
  masa_cera <- volumen_cera * densidad_cera
  c(masa_cera, volumen_cera)
}

# Paso 6:
m_cubierta_abs <- function(x, y, z, o, k , g, densidad_abs, v_cera){
  v_molde_cubierta <- ((x+2*(g+o+k))(z+2*(o+k))-v_cera)*(y+2*g)
  masa_abs_cubierta <- v_molde_cubierta * densidad_abs
  masa_abs_cubierta
}

# Flujo completo:
m_abs_cera <- function(z, k, densidad_comp, mass_comp, relation_y_x, w, 
                       densidad_abs, densidad_cera){
  
  #Paso 1:
  a_maca <- area_maca(mass_comp, density_comp, z)
  
  xy <- xy_length(relation_y_x, a_maca)
  
  x <- xy[1]
  y <- xy[2]
  
  #Paso 2:
  masa_abs_maca <- m_maca_abs(x, y, z, k, densidad_abs)
  
  #Paso 3:
  v_mas_g <- v_mas_gamma(mass_comp, densidad_comp, x=x, y=y, z=z)
  v_mas <- v_mas_g[1]
  g <- v_mas_g[2]
  
  #Paso 4:
  masa_abs_mas <- m_mas_abs(densidad_abs, x, y, z, g, k, v_mas)
  
  #Paso 5:
  mv_cera <- m_v_cera(x, y, z, g, o, densidad_cera)
  masa_cera <- mv_cera[1]
  volumen_cera <- mv_cera[2]
  
  #Paso 6:
  masa_abs_cubierta <- m_abs_cera(x, y, z, o, k , g, densidad_abs, v_cera)
  
  masa_abs_total <- masa_abs_maca + masa_abs_mas + masa_abs_cubierta
  c(masa_abs_total, masa_cera)
}
