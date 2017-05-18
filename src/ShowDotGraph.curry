--- A library for graph visualization with [Graphviz](http://www.graphviz.org/).

module ShowDotGraph
  ( DotGraph(..), Node(..), Edge(..)
  , viewDotGraph, getDotViewCmd, setDotViewCmd )
 where

import Char         (isAlphaNum)
import Distribution (rcFileName,getRcVar)
import IO
import IOExts
import List         (intercalate)
import PropertyFile (updatePropertyFile)

--- A Dot graph consists of a name and a list of nodes and edges.
data DotGraph = Graph String [Node] [Edge]

--- A node of a dot graph consists of a name and a list of attributes
--- for this node.
data Node = Node String [(String,String)]

--- An edge of a dot graph consists of the names of the source and target node
--- and a list of attributes for this edge.
data Edge = Edge String String [(String,String)]

--- Visualize a DOT graph with the `dotviewcommand` specified in
--- the rc file of the Curry system.
-- A dependency graph consists of a list of triples of the form (n,as,ms),
-- where n is a node name, as (dot) attributes for node n, and ms the list
-- of direct dependents from n.
viewDotGraph :: DotGraph -> IO ()
viewDotGraph = viewDot . showDotGraph

--- Shows a Dot graph as a string of the DOT language.
showDotGraph :: DotGraph -> String
showDotGraph (Graph name nodes edges) =
  "digraph \"" ++ name ++ "\"" ++
  "{\n" ++ concatMap node2dot nodes ++ concatMap edge2dot edges ++ "}\n"
 where
  node2dot (Node nname attrs) =
    if null attrs
    then showDotID nname ++ ";\n"
    else showDotID nname ++
            '[' : intercalate ","
                              (map (\ (n,v)->n++"=\""++v++"\"") attrs) ++ "]"
                  ++ ";\n"

  edge2dot (Edge i j attrs) =
    showDotID i ++ " -> " ++ showDotID j ++
    (if null attrs then "" else
       '[' : intercalate ","
                         (map (\ (n,v)->n++"=\""++v++"\"") attrs) ++ "]")
    ++ ";\n"

showDotID :: String -> String
showDotID s | all isAlphaNum s = s
            | otherwise        = '"' : concatMap escapeDQ s ++ "\""
 where
  escapeDQ c = if c=='"' then "\\\"" else [c]

--- Visualize a string of the DOT langugage with the `dotviewcommand`
--- from the rc file of the Curry system.
viewDot :: String -> IO ()
viewDot dottxt = do
    dotview <- getDotViewCmd
    dotstr <- connectToCommand dotview
    hPutStr dotstr dottxt
    hClose dotstr

-------------------------------------------------------------------------
--- Read the command for viewing dot files from the rc file of the
--- Curry system.
getDotViewCmd :: IO String
getDotViewCmd = getRcVar "dotviewcommand" >>= return . maybe "" id

--- Sets the command for viewing dot files in the rc file of the
--- Curry system.
setDotViewCmd :: String -> IO ()
setDotViewCmd dvcmd = do
  rcfile <- rcFileName
  updatePropertyFile rcfile "dotviewcommand" dvcmd

-------------------------------------------------------------------------
{-

Example settings in rc file:

dotviewcommand=dot -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx
dotviewcommand=neato -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx
dotviewcommand=circo -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx
dotviewcommand=fdp -Tpdf > /tmp/dotxxx && acroread /tmp/dotxxx

-}
