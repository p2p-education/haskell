import GLBasicTool as GT
import Graphics.UI.GLUT

field       [x,y,z] = [x+y,y,z]
hamilton2d  [x,y,z] = [y,(-x),z]
drawField field [x,y,z] = 
            sub [x,y,z] ( mul 0.2 ( hamilton2d ( field [x,y,z] ))) 

main :: IO ()
main = do 
    (_progName, _args)  <- getArgsAndInitialize
    initialWindowSize   $= Size 1000 1000
    _window             <- createWindow "arrow"
    displayCallback     $= display
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    mapM_ (\p -> GT.renderArrow [drawField field p, p]) 
            [ map gl [x,y,0] | x <- [-1,-0.9 .. 1], y <- [-1,-0.9 .. 1] ]
    flush



