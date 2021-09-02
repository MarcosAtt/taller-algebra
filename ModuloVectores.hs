module ModuloVectores
where
    
    norma :: (Floating t) => (t,t,t) -> t
    norma (x,y,z) = sqrt((x)^2 + (y)^2 + (z)^2)

    productoInterno :: (Floating t) => (t,t,t) -> (t,t,t) -> t 
    productoInterno (vx, vy, vz) (wx, wy, wz) = vx*wx + vy*wy + vz*wz