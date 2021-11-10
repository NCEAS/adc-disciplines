library(igraph)
library(vroom)
library(dplyr)
library(stringr)
library(ggraph)
library(scico)

# Load the default categories from DFG, but simplify some labels
dfg <- vroom("adc-disciplines.csv") %>%
  mutate(discipline=if_else(discipline=="Agriculture, Forestry, Horticulture and Veterinary Medicine", "Agriculture", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Hydrogeology, Hydrology, Limnology, Urban Water Management, Water Chemistry, Integrated Water Resources Management", "Hydrology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Computer Science, Electrical and System Engineering", "CS & ESE", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Epidemiology, Medical Biometry, Medical Informatics", "Epidemiology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Artificial Intelligence, Image and Language Processing", "Artificial Intelligence", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Microbiology, Virology and Immunology", "Microbiology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Geosciences (including Geography)", "Geosciences", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Basic Biological and Medical Research", "Basic Biology", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Social and Behavioural Sciences", "Social and Behavior", discipline)) %>% 
  mutate(discipline=if_else(discipline=="Geodesy, Photogrammetry, Remote Sensing, Geoinformatics, Cartogaphy", "Geodesy", discipline)) %>% 
  rename(name=discipline, dfg_code=code)

# Create edge list and nodelist from the DFG taxonomy, and set the subject to a discpline-related value
edges <- dfg %>% select(parent, id) %>% filter(id!=0)
nodes <- dfg %>% 
  mutate(subject=if_else(is.na(level2), level1, level2, level1)) %>% 
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
  scale_color_scico(palette = 'batlow') +
  theme_void() +
  theme(legend.position = "none")

ggsave("adc-disciplines.png", width=8)
