module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes graph = constructor graph
                where
                    constructor (Connect x y ) = S.union (nodes x) (nodes y)
                    constructor (Overlay x y) = S.union (nodes x) (nodes y)
                    constructor (Node x) = S.fromList [x]
                    constructor _ = S.empty

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges graph = constructor graph
                where
                    constructor (Connect x y ) = S.union (S.cartesianProduct (nodes x) (nodes y)) 
                                                         (edges (Overlay x y))
                    constructor (Overlay x y) = S.union (edges x) (edges y)
                    constructor _ = S.empty

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node graph = out graph
                            where
                                out (Connect x y ) = if (elem node (S.toList (nodes x)))
                                                        then S.union (nodes y) (outNeighbors node (Overlay x y))
                                                        else (outNeighbors node (Overlay x y))
                                out (Overlay x y) = S.union (outNeighbors node x) (outNeighbors node y)
                                out _ = S.empty

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node graph = in_nodes graph
                            where
                                in_nodes (Connect x y ) = if (elem node (S.toList (nodes y)))
                                                        then S.union (nodes x) (inNeighbors node (Overlay x y))
                                                        else (inNeighbors node (Overlay x y))
                                in_nodes (Overlay x y) = S.union (inNeighbors node x) (inNeighbors node y)
                                in_nodes _ = S.empty

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = delete_node graph
                           where
                                delete_node (Connect x y) = Connect (removeNode node x) (removeNode node y)
                                delete_node (Overlay x y) = Overlay (removeNode node x) (removeNode node y)
                                delete_node (Node x) = if (x /= node) then (Node x) else Empty 
                                delete_node _ = Empty

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}

nodes_union :: [AlgebraicGraph a] -> AlgebraicGraph a
nodes_union graphs= foldl (\result graph -> Overlay graph result) Empty graphs

new_graphs :: [a] -> AlgebraicGraph a
new_graphs news = nodes_union (map (\new_node -> Node new_node) news)

splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = split graph
                               where
                                    split (Connect x y) = Connect (splitNode old news x) (splitNode old news y)
                                    split (Overlay x y) = Overlay (splitNode old news x) (splitNode old news y)
                                    split (Node x) = if (x /= old) then (Node x) else (new_graphs news)
                                    split _ = Empty

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}

mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = merge graph
                                 where
                                    merge (Connect x y) = Connect (mergeNodes prop node x) (mergeNodes prop node y)
                                    merge (Overlay x y) = Overlay (mergeNodes prop node x) (mergeNodes prop node y)
                                    merge (Node x) = if (prop x) then (Node node) else (Node x)
                                    merge _ = Empty
                                    

                                   
{-
    Referinte 
        https://nobrakal.github.io/alga-tutorial/
        https://www.staff.ncl.ac.uk/andrey.mokhov/labelled-algebraic-graphs-slides.pdf
-}