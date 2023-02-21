{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
   -> am folosit tipuri inregistrate
   
    data StandardGraph a = Graph {nodes :: S.Set a, edges :: S.Set (a,a)} deriving(Eq, Ord, Show)
    fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
    fromComponents ns es = Graph (S.fromList ns) (S.fromList es)
-}


{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.fromList $ foldr (\x acc -> if (fst x) == node then (snd x) : acc else acc) [] $ S.toList $ edges graph

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-} 
inNeighbors node graph = S.fromList $ foldr (\x acc -> if (snd x) == node then (fst x) : acc else acc) [] $ S.toList $ edges graph

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = if notElem node (nodes graph) then graph 
                        else (fromComponents
                                (foldr (\x acc -> if x == node then acc else x : acc) [] (S.toList (nodes graph)))
                                (foldr (\x acc -> if ((fst x) == node) || ((snd x) == node) 
                                                  then acc else x : acc) [] (S.toList (edges graph))))
{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = let remove_node_nodes = S.toList $ nodes $ removeNode old graph
                               remove_node_edges = S.toList $ edges $ removeNode old graph
                               outh_neighbors = S.toList $ outNeighbors old graph
                               in_neighbors = S.toList $ inNeighbors old graph
                           in (fromComponents
                                (foldr (\x acc -> x : acc) remove_node_nodes news)
                                ( remove_node_edges ++
                                (foldr (\x acc -> (map (\y -> (x, y)) outh_neighbors) ++ acc) [] news) ++
                                (foldr (\x acc -> (map (\y -> (x, y)) news) ++ acc) [] in_neighbors)
                                )
                              )

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph = let edges_list = S.toList (edges graph)
                                 nodes_list = S.toList (nodes graph)
                                 replace_fst = (foldr (\x acc -> if (prop (fst x)) then (node, (snd x)) : acc else x : acc) [] edges_list)
                                 replace_snd = (foldr (\x acc -> if (prop (snd x)) then ((fst x), node) : acc else x : acc) [] replace_fst)
                                 in (fromComponents
                                        (if (edges_list == replace_snd) then nodes_list
                                        else (node : (foldr (\x acc -> if (prop x) then acc else x : acc) [] nodes_list)))
                                        replace_snd
                                    )