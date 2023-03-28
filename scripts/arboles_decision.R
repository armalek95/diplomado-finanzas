# Explicacion #################################################################

## Weighted NPV Function ######################################################
# Esta funcion calcula el valor presente neto del "padre" con base en los
# valores de NPV y de probabilidad de su ocurrencia
w_npv<- function(node) {
  result <- node$prob * node$npv
  if(length(result) == 0) result <- sum(sapply(node$children, w_npv))
  return (result)
}

## Funciones para mejorar la funcionalidad de plotting base de R ##############
# Modificadas de: 
# https://cran.r-project.org/web/packages/data.tree/vignettes/applications.html
# Bajo el encabezado de: Plot with the data.tree plotting facility
GetNodeLabel <- 
  function(node) 
    switch(node$type,
          terminal = 
            paste0( '$ ', format(node$npv, scientific = FALSE, big.mark = ",")),
          decision = 
            paste0(node$name),
          paste0('EMV\n', '$ ', 
                 format(node$npv, scientific = FALSE, big.mark = ",")))

GetEdgeLabel <- function(node) {
  name = AdjustNodeName(node)
  if (!node$isRoot && node$parent$type == 'chance') {
    label = paste0(name, " (", node$prob, ")")
  } else {
    label = name
  }
  return (label)
}

GetNodeShape <- function(node) 
  switch(node$type, decision = "box", chance = "circle", terminal = "none")

AdjustNodeName <- function(node) {
  if (str_starts(node$name, "\\d")) {
    name = str_sub(node$name, 3, str_length(node$name))
  } else {
    name = node$name
  }
  return (name)
}


## Funcion para dibujar el arbol de decision ##################################

PlotDecisionTree <- function(tree){
  SetEdgeStyle(tree, fontname = 'helvetica', label = GetEdgeLabel)
  SetNodeStyle(tree, fontname = 'helvetica', label = GetNodeLabel, 
               shape = GetNodeShape)
  SetGraphStyle(tree, rankdir = "LR")
  
  plot(tree)
}

