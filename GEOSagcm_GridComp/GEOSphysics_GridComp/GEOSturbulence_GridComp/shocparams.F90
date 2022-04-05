module SHOCPARAMS

 implicit none

 type SHOCPARAMS_TYPE
    integer :: CLDLEN
    integer :: LENOPT
    integer :: BUOYOPT
    real    :: LAMBDA
    real    :: TSCALE
    real    :: VONK
    real    :: CKVAL
    real    :: CEFAC
    real    :: CESFAC
    real    :: LENFAC
 endtype SHOCPARAMS_TYPE

end module SHOCPARAMS
