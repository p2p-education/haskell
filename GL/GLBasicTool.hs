module GLBasicTool (
    gl,render,renderArrow,
    mul,sub,add,
    p2v
) where

import Graphics.UI.GLUT as GL 

render = renderPrimitive
gl      :: Float -> GLfloat
gl      = id
v2p (Vertex3 x y z) = [x,y,z]
p2v [x,y,z]         = Vertex3 x y z

n2d [x,y,z] = [-y,x,z]                          -- normal vector in 2d  
l_t p q     = add (m p q) (l2l p q)             -- leftBottom  point of triangle
r_t p q     = add (m p q) (l2l q p)             -- rightBottom point of triangle
l2l p q     = mul (1/16) (n2d  (sub q p))       -- orthogonal vector mid 2 left of tri
m p q       = add p (mul (7/8) (sub q p))       -- midBottom point of triange
l2t [p,q]   = [l_t p q, r_t p q, q]             -- line to triangle 

add [x,y,z] [a,b,c] = [x+a,y+b,z+c]
sub [x,y,z] [a,b,c] = [x-a,y-b,z-c]
mul k       [x,y,z] = [k*x,k*y,k*z]

renderArrow line = do
    render Lines $ mapM_ vertex $ map p2v line
    render Triangles $ mapM_ vertex $ map p2v $ l2t line




