#####DECISION FLOW DIAGRAM#####
library("DiagrammeR")


diagram <- DiagrammeR::grViz("
digraph graph2 {

graph [layout = dot]

# node definitions with substituted label text
node [shape = ellipse, width = 2, height = 1, fillcolor = Biege]
a [label = 'TDA method']
b [label = 'Data set \u2264 3 dim']
c [label = 'Data dim reducible']
d [label = 'TDAstats \n(Rips Complex)']

x [label = 'GUDHI']
y [label = 'GUDHI \n(Alpha Complex)']
z [label = 'GUDHI \n(Alpha Complex)']

a -> x [taillabel = 'Other', labeldistance = 3.5, labelangle = 27]
b -> y [taillabel = 'Yes', labeldistance = 3, labelangle = 20]
c -> z [taillabel = 'Yes', labeldistance = 3, labelangle = 20]

a -> b [taillabel = 'Persistent \nHomology', labeldistance = 5.5, labelangle = -40]
b -> c [taillabel = 'No', labeldistance = 3, labelangle = -25]
c -> d [taillabel = 'No', labeldistance = 3, labelangle = -20]


}")


png(filename = "decision.png")

diagram

dev.off()
