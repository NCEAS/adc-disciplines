library(igraph)
library(vroom)
library(dplyr)
library(stringr)
library(ggraph)
library(scico)
library(rdflib)

# Load the default categories from DFG, but simplify some labels
dfg <- vroom("adc-disciplines.csv", show_col_types = FALSE) %>%
  mutate(discipline=if_else(discipline=="Agriculture, Forestry, and Veterinary Medicine", "Agricultural Sciences", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Hydrogeology, Hydrology, Limnology, Urban Water Management, Water Chemistry, Integrated Water Resources Management", "Hydrology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Computer Science, Electrical and System Engineering", "CS & ESE", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Epidemiology, Medical Biometry, Medical Informatics", "Epidemiology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Artificial Intelligence, Image and Language Processing", "Artificial Intelligence", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Microbiology, Virology and Immunology", "Microbiology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Geosciences (including Geography)", "Geosciences", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Basic Biological and Medical Research", "Basic Biology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Social and Behavioral Sciences", "Social Sciences", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Geodesy, Photogrammetry, Remote Sensing, Geoinformatics, Cartogaphy", "Geodesy", discipline)) %>% 
  rename(name=discipline, dfg_code=code) %>% 
  arrange(level1, level2, name)

# Create edge list and nodelist from the DFG taxonomy, and set the subject to a discipline-related value
edges <- dfg %>% select(parent, id) %>% filter(id!=0)
nodes <- dfg %>% 
  mutate(subject=if_else(is.na(level2), level1, level1, level1)) %>% 
  mutate(subject=if_else(subject<10, subject*10, subject)) %>% 
  mutate(name=trimws(name, which="both")) %>% 
  select(id, name, subject) %>%
  distinct()

# Create the graph using igraph
discipline_graph <- graph_from_data_frame(edges, vertices=nodes)

# Basic discipline tree plotted
ggraph(discipline_graph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point() +
  geom_node_text(aes(label=name, filter=leaf, color=subject), angle=90 , hjust=1.05, nudge_y = -0.05) +
  geom_node_label(aes(label=name, filter=!leaf, color=subject), repel = TRUE) +
  ylim(-4, NA) +
  scale_color_scico(palette = 'batlow', end=0.7) +
  theme_void() +
  theme(legend.position = "none")

ggsave("adc-disciplines.png", width=12)

create_class <- function(row, graph_, namespaces, prefix) {
  print(paste0("Processing: ", row[[3]]))
  class_id  <- paste0(namespaces$odo, sprintf("%s_%05s", prefix, row[[1]]))
  graph_ <- rdf_add(graph_, 
                   subject=class_id,
                   predicate=paste0(namespaces$rdf, "type"),
                   object=paste0(namespaces$owl, "Class"))
  graph_ <- rdf_add(graph_, 
                    subject=class_id,
                    predicate=paste0(namespaces$rdfs, "label"),
                    object=row[[3]])
  if (!is.na(row[[2]])) {
    parent_id <- paste0(namespaces$odo, sprintf("%s_%05d", prefix, as.numeric(row[[2]])))
    graph_ <- rdf_add(graph_, 
                      subject=class_id,
                      predicate=paste0(namespaces$rdfs, "subClassOf"),
                      object=parent_id)  
  }
  
  # TODO Add additional class metadata
  #     obo:IAO_0000115 "the sensitivity of the given data"@en ;
  #     dc:date "2021-09-30"^^xsd:date ;
  #     terms:creator <https://orcid.org/0000-0003-0077-4738> ;
  
  return(length(graph_))
}

create_ontology <- function(graph_, namespaces, prefix) {

  onto_id  <- paste0(namespaces$odo, sprintf("%s_", prefix))
  rdf_add(graph_, 
          subject=onto_id,
          predicate="rdf:type",
          object=paste0(namespaces$owl, "Ontology"))
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$owl, "versionIRI"),
          object=paste0(namespaces$odo, prefix, "/0.1.0"))
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$owl, "versionInfo"),
          object="Version 0.1.0")
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$terms, "description"),
          object="Ontology to support annotation of datasets housed at the Arctic Data Center (https://arcticdata.io)")
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$terms, "title"),
          object="Arctic Data Center Annotation Terms Ontology (ADCAT)")
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$terms, "license"),
          object="https://creativecommons.org/licenses/by/4.0/")
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$terms, "creator"),
          object="https://orcid.org/0000-0003-0077-4738")
  rdf_add(graph_, 
          subject=onto_id,
          predicate=paste0(namespaces$terms, "created"),
          object=Sys.Date())
  return("")
}

create_rdf <- function(dfg) {
  prefix <- "ADCAT"
  namespaces <- list(
    odo="https://purl.dataone.org/odo/",
    dc="http://purl.org/dc/elements/1.1/",
    obo="http://purl.obolibrary.org/obo/",
    odo="http://purl.dataone.org/odo/",
    owl="http://www.w3.org/2002/07/owl#",
    rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    rdfs="http://www.w3.org/2000/01/rdf-schema#",
    terms="http://purl.org/dc/terms/",
    xsd="http://www.w3.org/2001/XMLSchema#"
  )
  graph <- rdf()
  create_ontology(graph, namespaces, prefix)
  graph_sizes <- apply(dfg, 1, create_class, graph, namespaces, prefix)
  rdf_serialize(graph, paste0(prefix, ".ttl"), base="https://purl.dataone.org/odo/ADCAT_", format="turtle", namespace=unlist(namespaces))
  return(graph)
}

options(rdf_print_format = "turtle")
options(rdf_max_print = 10000)
graph <- create_rdf(dfg)


