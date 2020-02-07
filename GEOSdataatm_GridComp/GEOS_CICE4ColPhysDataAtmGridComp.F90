!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_CICE4ColPhysDataAtmGridCompMod -- get ``atmospheric" variables for CICE4 simulations

! !INTERFACE:

module GEOS_CICE4ColPhysDataAtmGridCompMod

! !DESCRIPTION:
!
!   Refer to details in \tt{GEOS\_DataAtmGridComp-pseudoCode.F90}
! 

! !USES:

  use sfclayer              ! using module that contains sfc layer code
  use ESMF
  use MAPL_Mod
  use GEOS_UtilsMod
  use DragCoefficientsMod   ! ??
  use ncar_ocean_fluxes_mod ! instead of using sfc layer code, use NCAR ocean fluxes

! LANL CICE4 Thermodynamics modules
  use ice_kinds_mod
  use ice_constants,      only: Tffresh, puny, c0
  use ice_constants,      only: rad_to_deg
  use ice_constants,      only: m2_to_km2
  use ice_constants,      only: depressT
  use ice_constants,      only: Tocnfrz
  use ice_constants,      only: Lfresh, rhos, cp_ice
  use ice_constants,      only: rhow, rhoi
  use ice_domain_size,    only: init_column_physics
  use ice_itd,            only: init_itd, cleanup_itd
  use ice_therm_vertical, only: init_thermo_vertical, &
                                thermo_vertical,      &
                                frzmlt_bottom_lateral
  use ice_state,          only: nt_Tsfc, nt_iage, nt_volpn, init_trcr_depend
  use ice_shortwave,      only: shortwave_ccsm3,         &
                                shortwave_dEdd_set_snow, &
                                shortwave_dEdd_set_pond, &
                                shortwave_dEdd
  use ice_therm_itd,      only: linear_itd,   &
                                add_new_ice,  &
                                lateral_melt, &
                                freeboard_ccsm
  use ice_init,           only: input_data, set_state_var, &
                                alloc_column_physics, dealloc_column_physics
  use ice_age,            only: iage_converter
  use ice_meltpond,       only: compute_ponds
  use ice_atmo,           only: atmo_boundary_layer
  
  implicit none
  private

  public SetServices

!EOP

  integer, parameter :: ICE = 1
  integer, parameter :: NUM_3D_ICE_TRACERS = 3
  integer, parameter :: NUM_SNOW_LAYERS    = 1

  contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

    !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: This version uses the MAPL\_GenericSetServices, which sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF\_State INTERNAL, which is in the MAPL\_MetaComp. The import
!   and internal variables are allocated and initialized by generic.  Here
!   generic is used for tiles.  

!EOP

!=============================================================================

! ErrLog Variables

    character(len=ESMF_MAXSTR)              :: IAm
    integer                                 :: STATUS
    character(len=ESMF_MAXSTR)              :: COMP_NAME

! Local derived type aliases

    type (MAPL_MetaComp),  pointer          :: MAPL
    type (ESMF_Config)                      :: CF

    integer                                 :: NUM_SUBTILES        ! = NUM_ICE_CATEGORIES 
    integer                                 :: NUM_ICE_LAYERS      ! set via resource parameter
    integer                                 :: NUM_ICE_CATEGORIES  ! set via resource parameter

!=============================================================================

! Begin...

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, CONFIG=CF, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // 'SetServices'

! Get my MAPL_Generic state
!--------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE, Initialize, RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,        Run, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE,  Finalize, RC=STATUS )
    VERIFY_(STATUS)

! Get constants from CF
! ---------------------

    call ESMF_ConfigGetAttribute(CF, NUM_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:" , RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_ConfigGetAttribute(CF, NUM_ICE_LAYERS    , Label="CICE_N_ICE_LAYERS:"     , RC=STATUS)
    VERIFY_(STATUS)
    NUM_SUBTILES  = NUM_ICE_CATEGORIES

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

!  !Imports from sea ICE dynamics: TAUXBOT and TAUYBOT
    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'TAUXBOT',                            &
        LONG_NAME          = 'eastward_stress_at_base_of_ice',     &
        UNITS              = 'N m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        DEFAULT            = 0.0,                                  &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'TAUYBOT',                            &
        LONG_NAME          = 'northward_stress_at_base_of_ice',    &
        UNITS              = 'N m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        DEFAULT            = 0.0,                                  &
        RC=STATUS  )
    VERIFY_(STATUS)

!  !EXPORT STATE:
!
!  from internal state: FRACICE, VOLICE, VOLSNO, ERGICE, ERGSNO, TAUAGE, VOLPOND
!
    call MAPL_AddExportSpec(GC,                    &    
        LONG_NAME          = 'eastward_stress_over_ice',  &
        UNITS              = 'N m-2'                     ,&   
        SHORT_NAME         = 'TAUXI'                     ,&   
        DIMS               = MAPL_DimsTileOnly           ,&   
        VLOCATION          = MAPL_VLocationNone          ,&   
        RC=STATUS  ) 
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &    
        LONG_NAME          = 'northward_stress_over_ice',  &
        UNITS              = 'N m-2'                     ,&   
        SHORT_NAME         = 'TAUYI'                     ,&   
        DIMS               = MAPL_DimsTileOnly           ,&   
        VLOCATION          = MAPL_VLocationNone          ,&   
        RC=STATUS  ) 
     VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FRESH',                     &
    LONG_NAME          = 'fresh_water_flux_to_ocean' ,&
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FSALT',                     &
    LONG_NAME          = 'salt_flux_to_ocean'        ,&
    UNITS              = 'kg m-2 s-1'                ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

!SA: check BZ
  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FHOCN',                     &
    LONG_NAME          = 'actual_ocean_ice_flux',     &
    UNITS              = 'W m-2'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)
!SA

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FRAZIL',                    &
    LONG_NAME          = 'frazil_ice_growth'         ,&
    UNITS              = 'm s-1'           ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

!SA: check BZ

     call MAPL_AddExportSpec(GC                          ,&
        LONG_NAME          = 'latent_heat_of_snow_melt ' ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'SMELT'                    ,&
        DIMS               = MAPL_DimsHorzOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FRAZIL',                    &
    LONG_NAME          = 'frazil_ice_growth'         ,&
    UNITS              = 'm s-1'           ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FSWTHRU',                   &
    LONG_NAME          = 'SW_flux_thru_ice_to_ocean' ,&
    UNITS              = 'W m-2'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'FSWABS',                   &
    LONG_NAME          = 'SW_flux_absorbed_by_skin_layer' ,&
    UNITS              = 'W m-2'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'CONGEL',                    &
    LONG_NAME          = 'congelation_ice_growth'    ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'SNOICE',                    &
    LONG_NAME          = 'snow_ice_formation'        ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTT',                     &
    LONG_NAME          = 'top_ice_melt'              ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTB',                     &
    LONG_NAME          = 'basal_ice_melt'            ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTL',                     &
    LONG_NAME          = 'lateral_ice_melt'          ,&
    UNITS              = 'm s-1'                     ,&
    DIMS               = MAPL_DimsTileOnly           ,&
    VLOCATION          = MAPL_VLocationNone          ,&
                                           RC=STATUS  )
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&
    SHORT_NAME         = 'MELTS',                     &
    LONG_NAME          = 'snow_melt'                 ,&  
    UNITS              = 'm s-1'                     ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&  
    SHORT_NAME         = 'HICE',                     &   
    LONG_NAME          = 'grid_cell_mean_ice_thickness',&
    UNITS              = 'm'                         ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&  
    SHORT_NAME         = 'HSNO',                     &   
    LONG_NAME          = 'grid_cell_mean_snow_thickness',&
    UNITS              = 'm'                         ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&  
    SHORT_NAME         = 'TSKINWCICE',                    &   
    LONG_NAME          = 'CICE_water_skin_temperature',&
    UNITS              = 'K'                         ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&  
    SHORT_NAME         = 'ISTSFC',                    &   
    LONG_NAME          = 'snow_or_ice_surface_temperature',&
    UNITS              = 'C'                         ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&  
    SHORT_NAME         = 'IAGE',                     &   
    LONG_NAME          = 'sea_ice_age'               ,&  
    UNITS              = 'years'                     ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

  call MAPL_AddExportSpec(GC                    ,&  
    SHORT_NAME         = 'SSKINW2',                   &   
    LONG_NAME          = 'sea_skin_layer_salinity',   &   
    UNITS              = 'psu'                       ,&  
    DIMS               = MAPL_DimsTileOnly           ,&  
    VLOCATION          = MAPL_VLocationNone          ,&  
                                           RC=STATUS  )   
  VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                           &   
        SHORT_NAME         = 'DAIDTT',                                &   
        LONG_NAME          = 'ice_area_tendency_dueto_thermodynamics', &
        UNITS              = '% day-1',                               &   
        DIMS               = MAPL_DimsTileOnly,                   &   
        VLOCATION          = MAPL_VLocationNone,                  &   
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                           &
        SHORT_NAME         = 'DVIDTT',                                &
        LONG_NAME          = 'ice_volume_tendency_dueto_thermodynamics', &
        UNITS              = 'cm day-1',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                           &
        SHORT_NAME         = 'FBOT',                                &
        LONG_NAME          = 'net_downward_heat_flux_from_ice_to_ocean', &
        UNITS              = 'W m-2',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'ice_ocean_friction_velocity',         &
        UNITS              = 'm s-1'                   ,&
        SHORT_NAME         = 'USTARI'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'DTS_for_step_1'        ,&
        UNITS              = 'K'                         ,&
        SHORT_NAME         = 'TSKINWinc1'                ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)
     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'DTS_for_step_2'        ,&
        UNITS              = 'K'                         ,&
        SHORT_NAME         = 'TSKINWinc2'                ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)
     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'DTS_for_step_3'        ,&
        UNITS              = 'K'                         ,&
        SHORT_NAME         = 'TSKINWinc3'                ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)
     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'DTS_for_complete_step'     ,&
        UNITS              = 'K'                         ,&
        SHORT_NAME         = 'TSKINWinctotal'            ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
          LONG_NAME          = 'fresh_water_flux_weighted_by_fr',&
          UNITS              = 'kg m-2 s-1'                ,&
          SHORT_NAME         = 'FWFLUX'                    ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
     VERIFY_(STATUS)
!SA

!  !INTERNAL STATE:

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'HSKINI',                            &
        LONG_NAME          = 'ice_skin_layer_mass',               &
        UNITS              = 'kg m-2',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.5*MAPL_RHOWTR,                     &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                                &
         SHORT_NAME         = 'TSKINI',                            &
         LONG_NAME          = 'ice_skin_temperature',              &
         UNITS              = 'K',                                 &
         UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
         DIMS               = MAPL_DimsTileOnly,                   &
         VLOCATION          = MAPL_VLocationNone,                  &
         FRIENDLYTO         = 'SEAICE',                            &
         DEFAULT            = MAPL_TICE-1.8,                       &
                                           RC=STATUS  )
    VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'SSKINI',                            &
        LONG_NAME          = 'ice_skin_salinity',                 &
        UNITS              = 'psu',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 30.0,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'QS',                                &
        LONG_NAME          = 'surface_specific_humidity',         &
        UNITS              = 'kg kg-1',                           &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.01,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CH',                                &
        LONG_NAME          = 'surface_heat_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CM',                                &
        LONG_NAME          = 'surface_momentum_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CQ',                                &
        LONG_NAME          = 'surface_moisture_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'Z0',                                &
        LONG_NAME          = 'aerodynamic_roughness',             &
        UNITS              = 'm',                                 &
        DEFAULT            = 0.00005,                             &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'WW',                                &
        LONG_NAME          = 'vertical_velocity_scale_squared',   &
        UNITS              = 'm+2 s-2',                           &
        DEFAULT            = 0.0,                                 &
        NUM_SUBTILES       = NUM_SUBTILES,                        &
        DIMS               = MAPL_DimsTileTile,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

!SA: check BZ
    call MAPL_AddInternalSpec(GC,                                  &    
        SHORT_NAME         = 'FR',                                &    
        LONG_NAME          = 'subtile_fractions_of_grid_cell',    &    
        UNITS              = '1',                                 &    
         PRECISION          = MAPL_R8,                             &    ! Bin, Yury: Please listen to Matt and Atanas! Kindly work on interfacing  
         DIMS               = MAPL_DimsTileOnly,                   &    ! all the R8 variables- internally, within CICE and doing GEOS computations in 
         UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &    ! R4. SA. Aug.2015
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLICE',                            &
        LONG_NAME          = 'ice_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLSNO',                            &
        LONG_NAME          = 'snow_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLPOND',                           &
        LONG_NAME          = 'pond_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'APONDN',                            &
        LONG_NAME          = 'pond_concentration',                &
        UNITS              = '1',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'HPONDN',                            &
        LONG_NAME          = 'pond_depth',                        &
        UNITS              = 'm',                                 &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'ERGICE',                            &
        LONG_NAME          = 'ice_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_ICE_LAYERS,NUM_ICE_CATEGORIES/),&
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'ERGSNO',                            &
        LONG_NAME          = 'snow_category_layer_internal_energy',&
        UNITS              = 'J m-2',                             &
        PRECISION          = MAPL_R8,                        &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        UNGRIDDED_DIMS     = (/NUM_SNOW_LAYERS,NUM_ICE_CATEGORIES/),&
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'TAUAGE',                            &
        LONG_NAME          = 'volume_weighted_mean_ice_age',      &
        UNITS              = 's',                                 &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
   VERIFY_(STATUS)

   call MAPL_AddInternalSpec(GC,                               &
        SHORT_NAME         = 'SLMASK',                           &
        LONG_NAME          = 'salt_water_lake_mask',             &
        UNITS              = '1',                                &
        DIMS               = MAPL_DimsTileOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
        DEFAULT            = 0.0,                                &
                                                       RC=STATUS  )
   VERIFY_(STATUS)
!SA

!EOS

! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd(GC,    name="INITIALIZE",            RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="RUN",                   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Thermo1",              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Thermo2",              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Albedo",               RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="FINALIZE",            RC=STATUS)
    VERIFY_(STATUS)

! Set generic init and final methods
! ----------------------------------

    call MAPL_GenericSetServices    ( GC,  RC=STATUS )
    VERIFY_(STATUS)

! Set the Run entry point
! -----------------------

    RETURN_(ESMF_SUCCESS)

  end subroutine SetServices

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: Initialize -- Initialize method for the GEOS CICE Thermodynamic

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the CICE4 thermodynamics Gridded Component.
!   It then does a Generic\_Initialize and also CICE4 data structures 

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)          :: IAm
    integer                             :: STATUS
    character(len=ESMF_MAXSTR)          :: COMP_NAME

    integer                             :: NUM_SUBTILES        ! = NUM_ICE_CATEGORIES 
    integer                             :: NUM_ICE_LAYERS      ! set via resource parameter
    integer                             :: NUM_ICE_CATEGORIES  ! set via resource parameter

! Local derived type aliases

    type (MAPL_MetaComp    ), pointer   :: MAPL => null()

    real                                :: DTI
    real                                :: ALBICEV, ALBSNOWV, ALBICEI, ALBSNOWI
    real                                :: USTAR_MIN, AHMAX
    real                                :: KSNO
    real                                :: ICE_REF_SALINITY
    real                                :: SNOWPATCH
    real                                :: DALB_MLT

    character(len=ESMF_MAXSTR)          :: CONDTYPE
    character(len=ESMF_MAXSTR)          :: SHORTWAVE

    integer                             :: DO_POND
    integer                             :: PRES_ICE

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Initialize"
    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, NUM_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:" ,     RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, NUM_ICE_LAYERS    , Label="CICE_N_ICE_LAYERS:"     ,     RC=STATUS)
    VERIFY_(STATUS)
    NUM_SUBTILES  = NUM_ICE_CATEGORIES

    call MAPL_TimerOn(MAPL,"TOTAL")
    call MAPL_TimerOn(MAPL,"INITIALIZE")

    call MAPL_Get(MAPL, HEARTBEAT = DTI, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_GetResource ( MAPL, DTI,       Label="CICE_DT:",           DEFAULT=DTI,               RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, ALBICEV,   Label="ALBICEV:",           DEFAULT=0.73,              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, ALBICEI,   Label="ALBICEI:",           DEFAULT=0.33,              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, ALBSNOWV,  Label="ALBSNOWV:",          DEFAULT=0.96,              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, ALBSNOWI,  Label="ALBSNOWI:",          DEFAULT=0.68,              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, CONDTYPE,  Label="CICE_CONDUCTIVITY:", DEFAULT="bubbly",          RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, SHORTWAVE, Label="CICE_SHORTWAVE:" ,   DEFAULT="shortwave_ccsm" , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DO_POND,   Label="CICE_DO_POND:" ,     DEFAULT=0,                 RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, USTAR_MIN, Label="CICE_USTAR_MIN:",    DEFAULT=0.001,             RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, AHMAX,     Label="CICE_AH_MAX:",       DEFAULT=0.5,               RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, SNOWPATCH, Label="CICE_SNOW_PATCH:",   DEFAULT=0.02,              RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, DALB_MLT,  Label="CICE_DALB_MLT:",     DEFAULT=-0.075,            RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( MAPL, ICE_REF_SALINITY,  Label="ICE_REF_SALINITY:" , DEFAULT=4.0,       RC=STATUS)
    VERIFY_(STATUS)

    ! It is desired to sometimes run the coupled model with prescribed ice. 
    ! 1: prescribe ice, as in AMIP mode.
    call MAPL_GetResource ( MAPL, PRES_ICE,  Label="PRESCRIBED_ICE:" , DEFAULT=1, RC=STATUS)
    VERIFY_(STATUS)

    if (PRES_ICE == 1) then
       KSNO = 2.0  ! sea ice conductivity used in zero-layer ice param. 
    else
       KSNO = 0.3  ! true snow conductivity
    endif

    if(MAPL_AM_I_ROOT()) then
          print*, 'Model time step = ', DTI
          print*, 'Sea ice albedo parameters:'
          print*, 'ALBICEV  = ', ALBICEV
          print*, 'ALBICEI  = ', ALBICEI
          print*, 'ALBSNOWV = ', ALBSNOWV
          print*, 'ALBSNOWI = ', ALBSNOWI
          print*, 'Sea ice conductivity parameterization:'
          print*, 'CONDTYPE = ', CONDTYPE

          if (DO_POND == 1) then
             print*, 'DO explicit melt ponding'
          else
             print*, 'DO NOT do any explicit melt ponding'
          endif

          print*, 'Sea ice shortwave parameterization:'
          print*, 'shortwave = ', SHORTWAVE
          print*, 'ustar_min = ', USTAR_MIN
          print*, 'ahmax     = ', AHMAX
    endif

    call init_column_physics(NUM_ICE_CATEGORIES,NUM_ICE_LAYERS)
    call alloc_column_physics( MAPL_AM_I_Root(), Iam )
    call input_data (DTI, ALBICEV, ALBSNOWV, ALBICEI, ALBSNOWI, CONDTYPE, USTAR_MIN, AHMAX, &
                     KSNO, ICE_REF_SALINITY, SNOWPATCH, DALB_MLT)
    call init_thermo_vertical
    call init_itd
    call init_trcr_depend(.true., (DO_POND==1))  ! 2nd argument must evaluate to a logical, i.e., TR_POND = DO_POND == 1 

! Call Initialize for every Child
!--------------------------------

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)

! All Done
!---------

    call MAPL_TimerOff(MAPL,"INITIALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL")

    RETURN_(ESMF_SUCCESS)

  end subroutine Initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: RUN -- Run stage for the CICE4ColPhy DataAtm component
! !INTERFACE:

  subroutine RUN ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:
  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: Periodically refreshes the surface conditions

!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)      :: IAm
  integer                         :: STATUS
  character(len=ESMF_MAXSTR)      :: COMP_NAME

! Locals

  type (MAPL_MetaComp), pointer   :: MAPL => null()
  type (ESMF_State   )            :: INTERNAL
  type (MAPL_SunOrbit)            :: ORBIT 
  type (ESMF_Config)              :: CF

! pointers to export


  real, pointer, dimension(:)     :: AREA => null()
  real, pointer, dimension(:)     :: LATS => null()
  real, pointer, dimension(:)     :: LONS => null()


  integer                         :: NT
!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

  Iam = "Run"
  call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
  VERIFY_(STATUS)
  Iam = trim(COMP_NAME) // Iam 

! Get my internal MAPL_Generic state
!-----------------------------------

  call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
  VERIFY_(STATUS)

  call MAPL_GetResource ( MAPL, NUM_ICE_CATEGORIES, Label="CICE_N_ICE_CATEGORIES:" ,     RC=STATUS)
  VERIFY_(STATUS)
  call MAPL_GetResource ( MAPL, NUM_ICE_LAYERS    , Label="CICE_N_ICE_LAYERS:"     ,     RC=STATUS)
  VERIFY_(STATUS)
  NUM_SUBTILES  = NUM_ICE_CATEGORIES

! Start Total timer
!------------------

  call MAPL_TimerOn(MAPL,"TOTAL")
  call MAPL_TimerOn(MAPL,"RUN" )

! Get parameters from generic state.
!-----------------------------------

   call MAPL_Get(MAPL,                 &
        TILELATS  = LATS ,             &
        TILELONS  = LONS ,             &
        TILEAREA  = AREA ,             &
        ORBIT     = ORBIT,             &
        INTERNAL_ESMF_STATE = INTERNAL,&
        CF = CF,                       &
   RC=STATUS )
   VERIFY_(STATUS)

! The number of tiles we are working on
!--------------------------------------

  NT = size(LONS)







! All done
!-----------

  call MAPL_TimerOff(MAPL,"RUN"  )
  call MAPL_TimerOff(MAPL,"TOTAL")

  RETURN_(ESMF_SUCCESS)

  end subroutine RUN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! !IROUTINE: Finalize        -- Finalize method for CICEThermo wrapper

! !INTERFACE:

  subroutine Finalize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(INOUT) :: GC     ! Gridded component 
  type(ESMF_State),    intent(INOUT) :: IMPORT ! Import state
  type(ESMF_State),    intent(INOUT) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(INOUT) :: CLOCK  ! The supervisor clock
  integer, optional,   intent(  OUT) :: RC     ! Error code:

!EOP

    type (MAPL_MetaComp), pointer:: MAPL

! ErrLog Variables

    character(len=ESMF_MAXSTR)       :: IAm
    integer                          :: STATUS
    character(len=ESMF_MAXSTR)       :: COMP_NAME

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Finalize"
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=status )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=status)
    VERIFY_(STATUS)

! Profilers
!----------

    call MAPL_TimerOn(MAPL,"TOTAL"   )
    call MAPL_TimerOn(MAPL,"FINALIZE")

    call dealloc_column_physics( MAPL_AM_I_Root(), Iam )

    call MAPL_TimerOff(MAPL,"FINALIZE")
    call MAPL_TimerOff(MAPL,"TOTAL"   )

! Generic Finalize
! ------------------

    call MAPL_GenericFinalize( GC, IMPORT, EXPORT, CLOCK, RC=status )
    VERIFY_(STATUS)

! All Done
!---------

    RETURN_(ESMF_SUCCESS)
  end subroutine Finalize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module GEOS_CICE4ColPhysDataAtmGridCompMod
