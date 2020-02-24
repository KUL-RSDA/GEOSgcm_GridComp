!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_DataAtmGridCompMod -- ``atmospheric" component for ocean-only simulations

! !INTERFACE:

module GEOS_DataAtmGridCompMod

! !DESCRIPTION:
!
!   Refer to details in \tt{GEOS\_DataAtmGridComp-pseudoCode.F90}
!
!   Serves the role of an atmospheric GCM by providing atmospheric fields and fluxes 
!   needed to {\it drive} ocean simulations. Current version supports:
!   \begin{enumerate}
!     \item Ocean (MOM version 5) $+$ sea ice (CICE version 4),
!     \item Ocean biology and radiation (version numbers ??).
!   \end{enumrate}
!   This present version supports ALL the above at once; future version may be 
!   able to support each OR all of the above components, for e.g., 
!   provide only what is needed for sea ice simulations.
!

! !USES:

  use ESMF
  use MAPL_Mod
  use GEOS_UtilsMod

  use GEOS_OpenWaterDataAtmGridCompMod,     only:  OpenWaterDataAtmSetServices    => SetServices
  use GEOS_CICE4ColPhysDataAtmGridCompMod,  only:  CICE4ColPhysDataAtmSetServices => SetServices
  use GEOS_ObioRadDataAtmGridCompMod,       only:  ObioRadDataAtmSetServices      => SetServices

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!EOP

  integer, parameter :: NUM_SUBTILES  = 2  ! number of subtiles for each tile (ice/water)
  integer, parameter :: ICE           = 1  ! index(id) of two children fixed here 
  integer, parameter :: WATER         = 2  ! AddChild needs to adhere to the specification
  integer, parameter :: OBIORAD       = 3  ! AddChild needs to adhere to the specification

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
    integer                                 :: I
    integer                                 :: DO_OBIO ! default (=0) is to run without ocean bio, rad and chem

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

!   Ocean biology, radiation and chemistry: using or not?
!   ------------------------------------------------------
    call MAPL_GetResource ( MAPL, DO_OBIO, Label="USE_OCEANOBIOGEOCHEM:",DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)

    ! order is important !!!
    ! first: sea-ice, second: openwater, and then others (obio, etc)
    ! changing order requires also changing indices of ICE and WATER (sub-tiles at the top)

    I = MAPL_AddChild(GC,   NAME='SEAICETHERMO', SS=CICE4ColPhysDataAtmSetServices, RC=STATUS)
    VERIFY_(STATUS)

    I = MAPL_AddChild(GC,   NAME='OPENWATER' ,   SS=OpenWaterDataAtmSetServices,    RC=STATUS)
    VERIFY_(STATUS)

    if (DO_OBIO/=0) then
      I = MAPL_AddChild(GC, NAME='OBIORAD' ,     SS=ObioRadDataAtmSetServices,      RC=STATUS)
      VERIFY_(STATUS)
    endif

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE: (ocean to atmosphere)

    call MAPL_AddImportSpec(GC,                                    &    
         SHORT_NAME         = 'UW',                                &    
         LONG_NAME          = 'zonal_velocity_of_surface_water',   &
         UNITS              = 'm s-1 ',                            &    
         DIMS               = MAPL_DimsTileOnly,                   &    
         VLOCATION          = MAPL_VLocationNone,                  &    
         DEFAULT            = 0.0,                                 &
         RC=STATUS)
    VERIFY_(STATUS)
  
    call MAPL_AddImportSpec(GC,                                    &    
         SHORT_NAME         = 'VW',                                &    
         LONG_NAME          = 'meridional_velocity_of_surface_water',&
         UNITS              = 'm s-1 ',                            &    
         DIMS               = MAPL_DimsTileOnly,                   &    
         VLOCATION          = MAPL_VLocationNone,                  &    
         DEFAULT            = 0.0,                                 &
         RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC                                     &   
        SHORT_NAME         = 'KPAR',                               &   
        LONG_NAME          = 'PAR_extinction_coefficient',         &
        UNITS              = 'm-1',                                &   
        DIMS               = MAPL_DimsTileOnly,                    &   
        VLOCATION          = MAPL_VLocationNone,                   &   
        DEFAULT            = 0.0,                                  &
        RC=STATUS)    
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                    &
          SHORT_NAME       = 'TS_FOUND',                           &
          LONG_NAME        = 'foundation_temperature_for_interface_layer',&
          UNITS            = 'K',                                  &
          DIMS             = MAPL_DimsTileOnly,                    &
          VLOCATION        = MAPL_VLocationNone,                   &
          DEFAULT          = 280.0,                                &
          RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                    &
          SHORT_NAME       = 'SS_FOUND',                           &
          LONG_NAME        = 'foundation_salinity_for_interface_layer',&
          UNITS            = 'PSU',                                &
          DIMS             = MAPL_DimsTileOnly,                    &
          VLOCATION        = MAPL_VLocationNone,                   &
          DEFAULT          = 35.0,                                 &
          RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'UI',                                 &
        LONG_NAME          = 'zonal_velocity_of_surface_ice',      &
        UNITS              = 'm s-1 ',                             &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        DEFAULT            = 0.0,                                  &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'VI',                                 &
        LONG_NAME          = 'meridional_velocity_of_surface_ice', &
        UNITS              = 'm s-1 ',                             &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        DEFAULT            = 0.0,                                  &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'FRACICE',                            &
        LONG_NAME          = 'ice_covered_fraction_of_tile',       &
        UNITS              = '1',                                  &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        DEFAULT            = 0.0,                                  &
        RC=STATUS  )
    VERIFY_(STATUS)

!SA: check BZ
    call MAPL_AddImportSpec(GC,                                    &
        SHORT_NAME         = 'FRZMLT',                             &
        LONG_NAME          = 'freeze_melt_potential',              &
        UNITS              = 'W m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        DEFAULT            = 0.0,                                  &
        RC=STATUS  )
    VERIFY_(STATUS)
!SA
!
! sea ICE dynamics imports: TAUXBOT and TAUYBOT are specified in GEOS_CICE4ColPhysDataAtm
!
!  !EXPORT STATE: (atmosphere to ocean)

! HI, TI, SI (friendly variables: MUST GO AWAY!)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'TSKINI'    , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'HSKINI'    , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'SSKINI'    , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
!
!SA: check BZ
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'FRACICE'   , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
!SA
!
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'VOLICE' , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'VOLSNO' , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'ERGICE' , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'ERGSNO' , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'TAUAGE' , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'VOLPOND', CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
!
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'TAUXW'     , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'TAUYW'     , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS) 
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'TAUXI'     , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'TAUYI'     , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        SHORT_NAME         = 'PENPAR',                             &
        LONG_NAME          = 'downwelling_par_direct_flux_at_skin_base',&
        UNITS              = 'W m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        SHORT_NAME         = 'PENPAF',                             &
        LONG_NAME          = 'downwelling_par_diffuse_flux_at_skin_base',&
        UNITS              = 'W m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        SHORT_NAME         = 'PENUVR',                             &
        LONG_NAME          = 'downwelling_uvr_direct_flux_at_skin_base',&
        UNITS              = 'W m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        SHORT_NAME         = 'PENUVF',                             &
        LONG_NAME          = 'downwelling_uvr_diffuse_flux_at_skin_base',&
        UNITS              = 'W m-2',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        LONG_NAME          = 'ocean_ustar_cubed',                  &
        UNITS              = 'm+3 s-3',                            &
        SHORT_NAME         = 'OUSTAR3',                            &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        LONG_NAME          = 'surface_pressure',                   &
        UNITS              = 'Pa',                                 &
        SHORT_NAME         = 'PS',                                 &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        LONG_NAME          = 'river_discharge_at_ocean_points',    &
        UNITS              = 'kg m-2 s-1',                         &
        SHORT_NAME         = 'DISCHARGE',                          &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_LWFLX'  , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_SHFLX'  , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_QFLUX'  , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_SNOW'   , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_RAIN'   , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_DRNIR'  , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'AO_DFNIR'  , CHILD_ID = WATER, RC=STATUS); VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC, SHORT_NAME = 'FRESH'     , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'FSALT'     , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)
    call MAPL_AddExportSpec(GC, SHORT_NAME = 'FHOCN'     , CHILD_ID =   ICE, RC=STATUS); VERIFY_(STATUS)

    if (DO_OBIO/=0) then
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'UU'      , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'CO2SC'   , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'DUDP'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'DUWT'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'DUSD'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'BCDP'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'BCWT'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'OCDP'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'OCWT'    , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'FSBAND'  , CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
      call MAPL_AddExportSpec(GC, SHORT_NAME = 'FSBANDNA', CHILD_ID=OBIORAD, RC=STATUS); VERIFY_(STATUS)
    endif

!
!  !Following exports are for diagnostics purpose
!

    call MAPL_AddExportSpec(GC,                                         &
       LONG_NAME  = '10-meter_eastward_wind',                                &
       UNITS      = 'm s-1',                                                 &
       SHORT_NAME = 'U10M',                                                  &
       DIMS       = MAPL_DimsTileOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                         &
       LONG_NAME  = '10-meter_northward_wind',                               &
       UNITS      = 'm s-1',                                                 &
       SHORT_NAME = 'V10M',                                                  &
       DIMS       = MAPL_DimsTileOnly,                                       &
       VLOCATION  = MAPL_VLocationNone,                                      &
                                                                  RC=STATUS  )
    VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &   
        LONG_NAME          = 'eastward_stress_on_ocean'  ,&  
        UNITS              = 'N m-2'                     ,&  
        SHORT_NAME         = 'TAUXO'                     ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )   
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                    &   
        LONG_NAME          = 'northward_stress_on_ocean', &
        UNITS              = 'N m-2'                     ,&  
        SHORT_NAME         = 'TAUYO'                     ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )   
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&  
        LONG_NAME          = 'absorbed_shortwave_rad'    ,&  
        UNITS              = 'W m-2'                     ,&  
        SHORT_NAME         = 'SWN'                       ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )   
     VERIFY_(STATUS)

      call MAPL_AddExportSpec(GC                          ,&  
        LONG_NAME          = 'net_longwave_rad'          ,&  
        UNITS              = 'W m-2'                     ,&  
        SHORT_NAME         = 'LWN'                       ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )   
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&  
        LONG_NAME          = 'sensible_heat_flux'        ,&  
        UNITS              = 'W m-2'                     ,&  
        SHORT_NAME         = 'SHF'                       ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )   
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&  
        LONG_NAME          = 'latent_heat_flux'          ,&  
        UNITS              = 'W m-2'                     ,&  
        SHORT_NAME         = 'LHF'                       ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )   
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&  
        LONG_NAME          = 'evaporation'               ,&  
        UNITS              = 'kg m-2 s-1'                ,&  
        SHORT_NAME         = 'EVAP'                      ,&  
        DIMS               = MAPL_DimsTileOnly           ,&  
        VLOCATION          = MAPL_VLocationNone          ,&  
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC                          ,&
        LONG_NAME          = 'precipitation'             ,&
        UNITS              = 'kg m-2 s-1'                ,&
        SHORT_NAME         = 'PRECIP'                    ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                                    &
        LONG_NAME          = 'net_surface_heat_flux',              &
        UNITS              = 'W m-2',                              &
        SHORT_NAME         = 'FSURF',                              &
        DIMS               = MAPL_DimsTileOnly,                    &
        VLOCATION          = MAPL_VLocationNone,                   &
        RC=STATUS  )
    VERIFY_(STATUS)

!EOS

! call MAPL_AddConnectivity ( GC,   &
!       !SHORT_NAME  = [character(len=9) :: &
!       !                'FRESH','FSALT','FHOCN',           &
!       !                'FRACI', 'FRACINEW','TFREEZE',     &
!       !                'DRUVRTHRU','DFUVRTHRU',           &
!       !                'DRPARTHRU','DFPARTHRU'],          &
!      SHORT_NAME  = [character(len=8) :: 'FRACI', 'FRACINEW','TFREEZE'],     &
!      DST_ID = WATER,              &
!      SRC_ID = ICE,                &
!      RC=STATUS  )
! VERIFY_(STATUS)

! Set the Profiling timers
! ------------------------
    call MAPL_TimerAdd(GC,    name="INITIALIZE",            RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="RUN",                   RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_TimerAdd(GC,    name="FINALIZE",              RC=STATUS)
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

! !IROUTINE: Initialize -- Initialize method for the composite Surface Gridded Component

! !INTERFACE:

  subroutine Initialize ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! Import state
    type(ESMF_State),    intent(inout) :: EXPORT ! Export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
    integer, optional,   intent(  out) :: RC     ! Error code

! !DESCRIPTION: The Initialize method of the Data Atmosphere Composite Gridded Component.

!EOP

! ErrLog Variables

    character(len=ESMF_MAXSTR)           :: IAm
    integer                              :: STATUS
    character(len=ESMF_MAXSTR)           :: COMP_NAME

! Local derived type aliases

    type (MAPL_MetaComp    ), pointer    :: MAPL
    type (MAPL_MetaComp    ), pointer    :: CHILD_MAPL
    type (MAPL_LocStream   )             :: LOCSTREAM
    type (MAPL_LocStream   )             :: EXCH
    type (ESMF_DELayout    )             :: LAYOUT
    type (ESMF_Config      )             :: CF
    type (ESMF_GridComp    ), pointer    :: GCS(:)

    integer                              :: I

!=============================================================================

! Begin... 

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    call ESMF_GridCompGet ( GC, name=COMP_NAME, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // "Initialize"

! Get my internal MAPL_Generic state
!-----------------------------------

    call MAPL_GetObjectFromGC ( GC, MAPL, RC=STATUS)
    VERIFY_(STATUS)

! Start timers
!-------------

    call MAPL_TimerOn(MAPL,"INITIALIZE", RC=STATUS ); VERIFY_(STATUS)
    call MAPL_TimerOn(MAPL,"TOTAL",      RC=STATUS ); VERIFY_(STATUS)

! Get the ocean tilegrid and the child components
!------------------------------------------------

!SA: Check AT. Which one of following two (1) or (2)?
! (1)
    call MAPL_Get (MAPL, LOCSTREAM=LOCSTREAM, GCS=GCS, RC=STATUS )
    VERIFY_(STATUS)

!(2)
    call MAPL_Get(MAPL, EXCHANGEGRID=EXCH,        RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_LocStreamCreate(LOCSTREAM, EXCH, NAME='OCEAN', &
                                       MASK=(/MAPL_OCEAN/), RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_Set(MAPL, LOCSTREAM=LOCSTREAM,   RC=STATUS )
    VERIFY_(STATUS)
!SA

! Place the tilegrid in the generic state of each child component
!----------------------------------------------------------------

    do I = 1, SIZE(GCS)
       call MAPL_GetObjectFromGC( GCS(I), CHILD_MAPL, RC=STATUS )
       VERIFY_(STATUS)
       call MAPL_Set (CHILD_MAPL, LOCSTREAM=LOCSTREAM, RC=STATUS )
       VERIFY_(STATUS)
    end do

    call MAPL_TimerOff(MAPL,"TOTAL", RC=STATUS ); VERIFY_(STATUS)

! Call Initialize for every Child
!--------------------------------

    call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK,  RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_TimerOff(MAPL,"INITIALIZE", RC=STATUS ); VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine Initialize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: RUN -- Run stage for the DataAtm component
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

  type (MAPL_MetaComp), pointer       :: MAPL => null()
  type (ESMF_Config)                  :: CF
  type (ESMF_State   )                :: INTERNAL
  type (ESMF_GridComp), pointer       :: GCS(:)
  type (ESMF_State),    pointer       :: GIM(:)
  type (ESMF_State),    pointer       :: GEX(:)
  character(len=ESMF_MAXSTR),pointer  :: GCnames(:)

  type (ESMF_Time)                    :: CURRENT_TIME
  type (ESMF_Time)                    :: MODELSTART
  type (ESMF_TimeInterval)            :: DELT

  character(len=ESMF_MAXSTR)          :: DATAFILE


  real, pointer, dimension(:  )  :: LONS  => null()


  real                           :: DT
  integer                        :: NT

! Temporary "forcing" fields

  real, pointer, dimension(:  )  :: rain         => null()
  real, pointer, dimension(:  )  :: snow         => null()

! pointers to export

! pointers to childrens' export

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

  call MAPL_Get(MAPL,                &
                TILELONS  = LONS ,   &
                RC=STATUS )
  VERIFY_(STATUS)

  call MAPL_Get (MAPL, GCS=GCS, GIM=GIM, GEX=GEX, GCnames=GCnames,rc=STATUS)
  VERIFY_(STATUS)

! Start Total timer
!------------------

  call MAPL_TimerOn(MAPL,"TOTAL")
  call MAPL_TimerOn(MAPL,"RUN" )

! Get parameters from generic state.
!-----------------------------------

  call MAPL_Get(MAPL,                          &
       INTERNAL_ESMF_STATE = INTERNAL,         &
       RC=STATUS )
  VERIFY_(STATUS)

! Get the time step
! -----------------

  call MAPL_Get(MAPL, HEARTBEAT = DT, RC=STATUS)
  VERIFY_(STATUS)
  call MAPL_GetResource ( MAPL, DT, Label="DT:", DEFAULT=DT, RC=STATUS)
  VERIFY_(STATUS)

! Get current time from clock
!----------------------------

  call ESMF_ClockGet( CLOCK, currTime=CURRENT_TIME, startTime=MODELSTART, TIMESTEP=DELT,  RC=STATUS )
  VERIFY_(STATUS)

! The number of tiles we are working on
!--------------------------------------

  NT = size(LONS)
  if(NT == 0) then
    call MAPL_TimerOff(MAPL,"RUN" )
    call MAPL_TimerOff(MAPL,"TOTAL")
    RETURN_(ESMF_SUCCESS)
  end if

! Temporary space for reading forcings
!-------------------------------------

  allocate( rain(NT), STAT=STATUS);  VERIFY_(STATUS)
  allocate( snow(NT), STAT=STATUS);  VERIFY_(STATUS)

! Read "forcing fields"
! ---------------------

! !Read sufrace downward fresh water flux from rain rate (mm s-1)
!----------------------------------------------------------------

  call MAPL_GetResource(MAPL, DATAFILE, LABEL = 'RAIN_FILE:', default = 'none', RC = status)
  VERIFY_(status)
  if(trim(DATAFILE) == 'none') then
    rain = 0.0
  else
    call MAPL_ReadForcing( MAPL, 'RAIN', renamefile( DATAFILE, TIME = CURRENT_TIME), CURRENT_TIME, rain, RC=STATUS)
    VERIFY_(status)
  endif

! !Read surface downward fresh water flux from snow rate (mm s-1)
!----------------------------------------------------------------

  call MAPL_GetResource(MAPL, DATAFILE, LABEL = 'SNOW_FILE:', default = 'none', RC = status)
  VERIFY_(status)
  if(trim(datafile) == 'none') then
    snow = 0.0
  else
    call MAPL_ReadForcing( MAPL, 'SNOW', renamefile( DATAFILE, TIME = CURRENT_TIME), CURRENT_TIME, snow, RC=STATUS)
    VERIFY_(status)
  endif

!
!SA: help AT. Please put in code so that variables: rain, snow, ... 
!    can be used by children (GEOS_OpenWaterDataAtmGridCompMod, GEOS_CICE4ColPhysDataAtmGridCompMod)
!    without having to read them for data file. The idea is parent reads these kind of (atmospheric) data
!    and ships it out to the children. They compute their local variables using these atmospheric variables and 
!    produce fields which are then aggregated (over water and ice tiles) by the parent and become exports.
!


! Pointers to outputs
!--------------------
!
! ...



! Clean up
!---------

  deallocate(rain)
  deallocate(snow)






! All done
!----------

  call MAPL_TimerOff(MAPL,"RUN" )
  call MAPL_TimerOff(MAPL,"TOTAL")

  RETURN_(ESMF_SUCCESS)

  end subroutine RUN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!-------------------------------------------------------------------------

  function renamefile (NAME, TIME) result(NAME0)

    character(len = *), intent(in)    :: NAME
    character(len = len(name))        :: NAME0
    type(ESMF_TIME),    intent(inout) :: TIME

    integer :: i, year, month, day
    integer :: STATUS

    NAME0 = trim( NAME)
    i = index(string = NAME, substring = "yyyymmdd")
    if(i == 0) return

    call ESMF_TIMEGET(TIME, yy = year, mm = month, dd = day, RC=STATUS)
    write(unit = NAME0(i:i + 7), fmt = "(i4,i2.2,i2.2)") year, month, day

  end function renamefile

!-------------------------------------------------------------------------

!    subroutine read_atmospheric(state, time, rc)
!      use MAPL ! MAPL_ForcingSpecificationMod
!      type(ESMF_State), intent(in) :: state
!      type(ESMF_Time), intent(in) :: time
!      integer, optional, intent(out) :: rc

!      type (ForcingSpecification) :: spec
!      integer :: k
!      integer :: status

!      spec = ForcingSpec(state, time)
!      call spec%read_forcing(t10, 'T10', rc=status); VERIFY_(status)
!      call spec%read_forcing(t10, 'Q10', default=2.0e-6, rc=status); VERIFY_(status)
!      call spec%read_forcing(u10, 'U10', rc=status); VERIFY_(status)
!      call spec%read_forcing(u10, 'V10', rc=status); VERIFY_(status)
!      ...


!      spec = ForcingSpec(state, currenttime, &
!           & label_format='(%a,i3.3,"_FILE:")', &
!           & var_name_format = '(%a,i3.3)')

!      call spec%read_forcing(dry_clayx, 'DUPD', rc=status); VERIFY_(status)
!      call spec%read_forcing(wet_clayx, 'DUWT', rc=status); VERIFY_(status)
!      call spec%read_forcing(sed_clayx, 'DUSD', rc=status); VERIFY_(status)

!      spec = ForcingSpec(state, currenttime, &
!           & var_name_format = '(%a,"_",i3.3)')

       ! Wasteful - find the same datafile in each iteration
       ! The extra complexity does not seem worth addressing it though.
!      do k = 1, NUM_TAUA
!         call spec%read_forcing( ataua(k)%b,  'TAUA', bin=k, rc=status); VERIFY_(status)
!         call spec%read_forcing(aasymp(k)%b, 'ASYMP', bin=k, rc=status); VERIFY_(status)
!         call spec%read_forcing(assalb(k)%b, 'SSALB', bin=k, rc=status); VERIFY_(status)
!      end do

!    end subroutine read_atmospheric

!-------------------------------------------------------------------------

end module GEOS_DataAtmGridCompMod
