!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_OpenWaterDataAtmGridCompMod -- get ``atmospheric" variables for ocean simulations

! !INTERFACE:

module GEOS_OpenWaterDataAtmGridCompMod

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

  implicit none 
  private

! integer            :: DO_DATASEA    ! for Atmosphere-Ocean Interface Layer (AOIL) only. 
!                                     ! perhaps implemented in a future version
  integer            :: DO_SKIN_LAYER ! controls the switch for AOIL: on/off

  public SetServices

!EOP

  integer, parameter            :: WATER        = 1
  integer, parameter            :: NUM_SUBTILES = 1             ! number of sub-tiles
  real,    parameter            :: KUVR         = 0.09
  real,    parameter            :: SALTWATERCAP  = MAPL_CAPWTR

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

    call MAPL_GetResource ( MAPL, DO_SKIN_LAYER, Label="USE_SKIN_LAYER:"  , DEFAULT=0    , RC=STATUS)
    VERIFY_(STATUS)

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

!  !EXPORT STATE:

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'eastward_stress_over_water',&
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUXW'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
        RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &
        LONG_NAME          = 'northward_stress_over_water',&
        UNITS              = 'N m-2'                     ,&
        SHORT_NAME         = 'TAUYW'                     ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
        RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'atmosphere_ocean_net_longwave_radiation' ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'AO_LWFLX'                  ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)
     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'atmosphere_ocean_sensible_heat_flux' ,&
        UNITS              = 'W m-2'                     ,&
        SHORT_NAME         = 'AO_SHFLX'                  ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'atmosphere_ocean_evaporation' ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'AO_QFLUX'                  ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'atmosphere_ocean_snowfall' ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'AO_SNOW'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                     &
        LONG_NAME          = 'atmosphere_ocean_rainfall' ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'AO_RAIN'                   ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC                         ,&
         LONG_NAME          = 'net_surface_downwelling_nir_beam_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'AO_DRNIR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC                         ,&
         LONG_NAME          = 'net_surface_downwelling_nir_diffuse_flux',&
         UNITS              = 'W m-2'                       ,&
         SHORT_NAME         = 'AO_DFNIR'                       ,&
         DIMS               = MAPL_DimsTileOnly             ,&
         VLOCATION          = MAPL_VLocationNone            ,&
                                                  RC=STATUS  )
    VERIFY_(STATUS)

!  !INTERNAL STATE:

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

     call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'TWMTF',                             &
        LONG_NAME          = 'departure_of_mean_interface_temperature_from_foundation_temperature',   &
        UNITS              = 'K',                                 &
        DEFAULT            = 0.0,                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'DELTC',                             &
        LONG_NAME          = 'temperature_drop_across_cool_layer',&
        UNITS              = 'K',                                 &
        DEFAULT            = 0.0,                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)
!
!   !Following additional internal states (HSKINW, SSKINW, TSKINW) are 
!   !for "old skin layer", not needed with the "new AOIL"
!
!    call MAPL_AddInternalSpec(GC,                           &
!       SHORT_NAME         = 'HSKINW',                            &
!       LONG_NAME          = 'water_skin_layer_mass',             &
!       UNITS              = 'kg m-2',                            &
!       DIMS               = MAPL_DimsTileOnly,                   &
!       VLOCATION          = MAPL_VLocationNone,                  &
!       FRIENDLYTO         = 'OCEAN:SEAICE',                      &
!       DEFAULT            = 5.0*MAPL_RHOWTR,                     &
!                                                      RC=STATUS  )
!    VERIFY_(STATUS)
!
!    call MAPL_AddInternalSpec(GC,                           &
!       SHORT_NAME         = 'SSKINW',                            &
!       LONG_NAME          = 'water_skin_salinity',               &
!       UNITS              = 'psu',                               &
!       DIMS               = MAPL_DimsTileOnly,                   &
!       VLOCATION          = MAPL_VLocationNone,                  &
!       FRIENDLYTO         = 'OCEAN:SEAICE',                      &
!       DEFAULT            = 30.0,                                &
!                                                      RC=STATUS  )
!    VERIFY_(STATUS)
!
!    call MAPL_AddInternalSpec(GC,                           &
!       SHORT_NAME         = 'TSKINW',                            &
!       LONG_NAME          = 'water_skin_temperature',            &
!       UNITS              = 'K',                                 &
!       DIMS               = MAPL_DimsTileOnly,                   &
!       VLOCATION          = MAPL_VLocationNone,                  &
!       FRIENDLYTO         = 'OCEAN:SEAICE',                      &
!       DEFAULT            = 280.0,                               &
!                                                      RC=STATUS  )
!    VERIFY_(STATUS)
!
!    call MAPL_AddInternalSpec(GC,                                &
!       SHORT_NAME         = 'TWMTS',                             &
!       LONG_NAME          = 'departure_of_skin_temperature_from_mean_interface_temperature',   &
!       UNITS              = 'K',                                 &
!       DEFAULT            = 0.0,                                 &
!       DIMS               = MAPL_DimsTileOnly,                   &
!       VLOCATION          = MAPL_VLocationNone,                  &
!                                                      RC=STATUS  )
!    VERIFY_(STATUS)
!

!EOS

! Set the Profiling timers
! ------------------------
    call MAPL_TimerAdd(GC,    name="RUN",                   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-OpenWater",            RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="-Albedo"  ,             RC=STATUS)
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
! !IROUTINE: RUN -- Run stage for the OpenWater DataAtm component
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

end module GEOS_OpenWaterDataAtmGridCompMod
