-- Exercise:
-- Take a binary tree and renumber nodes going breadth first
-- Here, the idea is that each layer of a tree is a forest
-- We can serialize and deserialize such a forest


data Tree = Leaf | Node Tree Int Tree
  deriving Show

depth :: Tree -> Int
depth Leaf = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

type Forest = [Tree]

-- Ignore leaves
roots :: Forest -> Int
roots []          = 0
roots (Leaf : ts) = roots ts
roots (_ : ts)    = 1 + roots ts

-- descend to the lower level of the forest
descend :: Forest -> Forest
descend [] = []
descend (Node l _ r : ts) = l : r : descend ts
descend (Leaf : ts) = descend ts

type Layer = [Maybe Int]

-- Slice tree horizontally into a list of layers
-- Replace Leaves with Nothing, Nodes with Just the counter
-- Counter increases breadth-first
slice :: Tree -> [Layer]
slice t = go 0 [t]
  where
    go :: Int -> Forest -> [Layer]
    go _ [] = []
    go cnt f =
      layer cnt f : go (cnt + roots f) (descend f)

-- Create top layer from a forest 
-- Number the nodes breadth first, starting with a given counter
layer :: Int -> Forest -> Layer
layer _ [] = []
layer cnt (Leaf : ts) = Nothing : layer cnt ts
layer cnt (Node l x r : ts) = Just cnt : layer (cnt + 1) ts

-- Rebuild a tree from a list of layers
rebuild :: [Layer] -> Tree
rebuild fs = head (go fs)
  where
    go :: [Layer] -> Forest
    go [] = []
    go (layer : layers) = 
      stitch layer (go layers)

-- Stitch a layer to a forest that grows beneath it
stitch :: Layer -> Forest -> Forest
stitch [] [] = []
stitch (Nothing : is) ts = Leaf : stitch is ts
stitch (Just n : is) (tl : tr : ts) = Node tl n tr : stitch is ts

showTree :: Tree -> String
showTree t = concat $ fmap (showLevel [t]) [0..(depth t)]
  where
    showLevel :: Forest -> Int -> String
    showLevel f n = 
      if n == 0
      then showF f
      else showLevel (descend f) (n - 1)
    showF :: Forest -> String
    showF [] = "\n"
    showF (Leaf : ts) = " ." ++ showF ts
    showF (Node _ x _ : ts) = " " ++ show x ++ showF ts

t :: Tree
t = Node (Node (Node Leaf 0 Leaf) 1 (Node (Node Leaf 0 Leaf) 2 Leaf)) 3 (Node (Node Leaf 4 Leaf) 5 (Node (Node Leaf 6 Leaf) 7 Leaf))

main = do
  putStrLn $ showTree t
  let ls = slice t
  putStrLn $ showTree $ rebuild ls
