module Algorithms where

import qualified Data.Set as S
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

graphTraversal :: Ord a 
                => ([a] -> [a] -> [a]) -- functia de imbinare de noduri
                -> Graph a -- graful
                -> [a] -- lista noduri vizitate
                -> [a] -- lista noduri din stiva/coada
                -> [a] -- lista de parcurgere
graphTraversal f graph set stackQueue = if null stackQueue
                                        then set
                                        else let
                                                current_node = head stackQueue
                                                new_set = set ++ [current_node]
                                                neighbors = S.toList $ outNeighbors current_node graph
                                                new_stackQueue = f 
                                                                    (foldr (\x acc -> if ((elem x new_set) || (elem x stackQueue)) 
                                                                                        then acc else [x] ++ acc) [] neighbors)
                                                                    (tail stackQueue)
                                            in
                                                graphTraversal f graph new_set new_stackQueue


search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = graphTraversal f graph [] [node] 

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs node graph = search (\x y -> y ++ x) node graph

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
dfs node graph = search (\x y -> x ++ y) node graph

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = let bfs_path = bfs from graph
                                      dfs_path = dfs from graph
                                      nodes_to_to_bfs = fst $ span (/=to) bfs_path -- nodurile pana la to in bfs
                                      nodes_to_to_dfs = fst $ span (/=to) dfs_path -- nodurile pana la to in dfs
                                      nodes_to_from_bfs = fst $ span (/=from) bfs_path -- nodurile pana la from in bfs
                                      nodes_to_from_dfs = fst $ span (/=from) dfs_path -- nodurile pana la from in dfs
                                      -- diferentele intre numarul de noduri pana la to si numarul de noduri pana la from
                                      length_bfs = (length nodes_to_to_bfs) - (length nodes_to_from_bfs) - 1
                                      length_dfs = (length nodes_to_to_dfs) - (length nodes_to_from_dfs) - 1
                                  in if (notElem to bfs_path) && (notElem to dfs_path)
                                        then Nothing
                                        else Just(length_bfs, length_dfs)
