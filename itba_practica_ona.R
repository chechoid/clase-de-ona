library(tidyverse)
library(igraph)
library(visNetwork)
library(networkD3)


#### Datos ####

contactos <- read_delim("datos/contactos.csv", delim = ";")

# Buscamos en la base personas con roles de Data Scientist, Data Analyst o Data Analytics
data_scientist <- contactos %>% 
  filter(str_detect(Position, "data.scientist")|str_detect(Position, "data.analyst|analytics"))

# Identificamos los datos únicos de origen
origen <- data_scientist %>% 
  distinct(Origen) %>% 
  rename(label=Origen)

# Identificamos los datos únicos de contacto
contacto <- data_scientist %>% 
  distinct(nombre_apellido) %>% 
  rename(label=nombre_apellido)

# Unificamos los dos dataframes
nodes <- full_join(origen, contacto, by = "label")

# Añadimos una columna de ID
nodes <- nodes %>% rowid_to_column("id")

# Calculamos la cantidad de conexiones entre Origen y Contacto
conexion <- data_scientist %>% 
  group_by(Origen, nombre_apellido) %>% 
  summarise(peso = n()) %>% 
  ungroup()

# Indico a las aristas cuál es el nodo de origen y cuál el de destino
aristas <- conexion %>% 
  left_join(nodes, by = c("Origen" = "label")) %>% 
  rename(from = id)

aristas <- aristas %>% 
  left_join(nodes, by = c("nombre_apellido" = "label")) %>% 
  rename(to = id)

# Ver el dataset
aristas

# Seleccionamos algunas columnas
aristas <- select(aristas, from, to, peso)

# Empezamos a construir el grafo

edges <- mutate(aristas, width = peso/5 + 1)

nodes$color <- c(rep("#DD6B06", 3), rep("#2CAFBB", 261))

# Visualizar el grafo
referidos <- visNetwork(nodes, aristas) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
    visNodes(color = list(background = "#5DBAC3",
                        border = "#01636D")) %>% 
  visEdges(color = list(color = "grey", highlight = "#014D54" )) %>% 
  visOptions(highlightNearest = TRUE)

# visSave(referidos, file = "referidos.html")

referidos

# Otra alternativa
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(aristas, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "color", Value = "peso", 
             opacity = 1, fontSize = 16, zoom = TRUE)


