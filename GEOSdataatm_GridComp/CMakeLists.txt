esma_set_this ()

set (srcs
   cube_sphere.F90
   GEOS_DataAtmGridComp.F90
   ncar_ocean_fluxes.F90
   data_water.F90
#  data_ice.F90
#  data_ObioRad.F90
  )

esma_add_library (${this}
   SRCS ${srcs}
   DEPENDENCIES MAPL LANL_cice GEOS_Shared
   INCLUDES ${INC_ESMF})

target_compile_definitions (${this} PRIVATE USE_CICE USE_R8)

