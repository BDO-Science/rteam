library(DiagrammeR)
library(tidyverse)
####very basic flow chart
a_graph <- create_graph() %>%
  add_node() %>%
  add_node() %>%
  add_node() %>%
  add_edge(from = 1, to = 2) %>%
  add_edge(from = 2, to = 3) %>%
  add_edge(from = 3, to = 1)
render_graph(a_graph)


####changing aesthestics of nodes
b_graph <- create_graph() %>%
  add_node(
    node_aes =
      node_aes(shape = 'diamond',
               color = 'darkred',
               fillcolor = 'red')) %>%
  add_node(
    node_aes =
      node_aes(shape = 'circle',
               color = 'darkgreen',
               fillcolor = 'green')
  ) %>%
  add_node(
    node_aes =
      node_aes(shape = 'square',
               color = 'darkblue',
               fillcolor = 'blue')
  ) %>%
  add_edge(from = 1, to = 2) %>%
  add_edge(from = 2, to = 3) %>%
  add_edge(from = 3, to = 1)
render_graph(b_graph)

####changing aesthetics of edges
c_graph <- b_graph %>% add_edge(
    from = 1, to = 3,
    rel = "interacted_with",
    edge_aes = edge_aes(
      color = "red",
      arrowhead = "vee",
      tooltip = "Eats threes"))
render_graph(c_graph)
export_graph(c_graph, file_name = 'diagrammeR/outputs/diagram1.png', file_type = 'PNG', width = 800, height = 800) #save as image

####importing your own data
nodes <- read.csv('diagrammeR/nodes.csv')
edges <- read.csv('diagrammeR/edges.csv')

d_graph <- create_graph() %>%
  add_nodes_from_table(
    table = nodes,
    label_col = label,
    type_col = type) %>%
  add_edges_from_table(
    table = edges,
    from_col = from,
    to_col = to,
    from_to_map = id_external)
render_graph(d_graph, layout = 'circle')


####Assign positions from dataframe
for (i in 1:nrow(nodes)) {
  e_graph <- d_graph %>%
    set_node_position(
      node = nodes$id[i],
      x = nodes$x[i],
      y = nodes$y[i]
    )
}
render_graph(e_graph)

####change node aesthetics using type column
f_graph <- create_graph() %>%
  add_nodes_from_table(
    table = nodes,
    label_col = label,
    type_col = type) %>%
  add_edges_from_table(
    table = edges,
    from_col = from,
    to_col = to,
    from_to_map = id_external) %>%
  select_nodes(conditions = type == 'adult') %>%
  set_node_attrs_ws(node_attr = fillcolor, value = "forestgreen") %>%
  set_node_attrs_ws(node_attr = shape, value = "diamond") %>%
  clear_selection() %>%
  select_nodes(condition = type == 'juvenile') %>%
  set_node_attrs_ws(node_attr = fillcolor, value = 'orange') %>%
  clear_selection() %>%
  select_nodes(condition = type == 'eggs') %>%
  set_node_attrs_ws(node_attr = fillcolor, value = 'blue') %>%
  set_node_attrs_ws(node_attr = shape, value = 'egg') %>%
  clear_selection() %>%
  select_nodes(condition = type == 'steelhead') %>%
  set_node_attrs_ws(node_attr = fillcolor, value = 'darkturquoise') %>%
  set_node_attrs_ws(node_attr = shape, value = 'rectangle') %>%
  clear_selection() %>%
  set_node_attrs(node_attr = fontcolor, value = 'white') %>%
  set_edge_attrs(edge_attr = color, value = 'cornsilk4')
render_graph(f_graph)
export_graph(f_graph, file_name = 'diagrammeR/outputs/diagram2.png', file_type = 'PNG', width = 800, height = 800)
