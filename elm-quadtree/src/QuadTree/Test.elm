module QuadTree.Test exposing (..)

import QuadTree.QuadTree exposing (..)
import QuadTree.Box exposing (create, quadrant)

shape1: (Shape String)
shape1 = 
    { bounds=((5,3),(20,30))
    , data="hei"
    }

shape2: (Shape String)
shape2 = 
    { bounds=((5,32),(20,40))
    , data="hallo"
    }
shape3: (Shape String)
shape3 = 
    { bounds=((30,40),(35,45))
    , data="hipp"
    }

shape4: (Shape String)
shape4 = 
    { bounds=((60,60),(65,75))
    , data="hopp"
    }

shapes=[shape1, shape2, shape3, shape4]

tree: Maybe (Node String)
tree = QuadTree.QuadTree.create 3 shapes

outer=QuadTree.Box.create (0,0) (100,100)

qb=QuadTree.Box.quadrant outer 

shape5: (Shape String)
shape5 = 
    { bounds=((50,60),(65,80))
    , data="hopp"
    }

coll=case tree of 
    Just t -> QuadTree.QuadTree.collisions t shape5
    Nothing -> []