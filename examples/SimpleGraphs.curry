import ShowDotGraph

-- A simple graph without attributes.
graph1 :: DotGraph
graph1 = Graph "Simple"
               [Node "a" [], Node "b" [], Node "c" []]
               [Edge "a" "b" [], Edge "b" "c" [], Edge "c" "a" []]

test1 :: IO ()
test1 = viewDotGraph graph1

-- Another simple graph with some attributes.
graph2 :: DotGraph
graph2 = Graph "Simple"
               [Node "a" nattrs, Node "b" nattrs, Node "c" nattrs]
               [Edge "a" "b" [], Edge "b" "c" [],
                Edge "c" "a" [("dir","none"),("label","back")]]
 where
  nattrs = [("shape","record"),("style","bold"),
            ("label","{Label|line1\\nline2}")]

test2 :: IO ()
test2 = viewDotGraph graph2
