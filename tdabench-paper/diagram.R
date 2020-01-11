#####DECISION FLOW DIAGRAM#####
library("DiagrammeR")

decision <- create_graph() %>%
  add_node(label = "TDA method?",
           node_aes = node_aes(shape = "rectangle",
                               width = 1.1)) %>%               # 1
  add_node(label = "GUDHI",
           node_aes = node_aes(shape = "rectangle",
                               width = 0.65)) %>%                     # 2
  add_node(label = "Approx?",
           node_aes = node_aes(shape = "rectangle",
                               width = 0.75)) %>%      # 3
  add_node(label = "TDAstats",
           node_aes = node_aes(shape = "rectangle",
                               width = 0.8)) %>%                  # 4
  add_node(label = "Dim?",
           node_aes = node_aes(shape = "rectangle",
                               width = 0.5)) %>%           # 5
  add_node(label = "GUDHI",
           node_aes = node_aes(shape = "rectangle",
                               width = 0.65)) %>%                     # 6
  add_node(label = "TDAstats",
           node_aes = node_aes(shape = "rectangle",
                               width = 0.8)) %>%                  # 7
  add_edge(from = 1, to = 2,
           edge_aes = edge_aes(label = "Other")) %>%
  add_edge(from = 1, to = 3,
           edge_aes = edge_aes(label = "PHom")) %>%
  add_edge(from = 3, to = 4,
           edge_aes = edge_aes(label = "No")) %>%
  add_edge(from = 3, to = 5,
           edge_aes = edge_aes(label = "Yes")) %>%
  add_edge(from = 5, to = 6,
           edge_aes = edge_aes(label = "<= 3")) %>%
  add_edge(from = 5, to = 7,
           edge_aes = edge_aes(label = "> 3"))

decision %>% render_graph(layout = "tree")