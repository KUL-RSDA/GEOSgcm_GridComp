!  $Id$

#include "MAPL_Generic.h"

!=============================================================================
!BOP

! !MODULE: GEOS_ObioRadDataAtmGridCompMod -- get ``atmospheric" variables for 
!                                            ocean biology and radiation simulations

! !INTERFACE:

module GEOS_ObioRadDataAtmGridCompMod

! !DESCRIPTION:
!
!   Refer to details in \tt{GEOS\_DataAtmGridComp-pseudoCode.F90}
! 

! !USES:

  use ESMF
  use MAPL_Mod

  implicit none
  private

  public SetServices

!EOP

! Following could also be controlled via resource parameter
  integer, parameter :: NUM_DUDP = 5
  integer, parameter :: NUM_DUWT = 5
  integer, parameter :: NUM_DUSD = 5

  integer, parameter :: NUM_BCDP = 2                           ! number of Black Carbon 
  integer, parameter :: NUM_BCWT = 2
  integer, parameter :: NUM_OCDP = 2                           ! number of Organic Carbon 
  integer, parameter :: NUM_OCWT = 2

  integer, parameter :: NB_CHOU_UV  = 5                        ! number of UV bands
  integer, parameter :: NB_CHOU_NIR = 3                        ! number of near-IR bands
  integer, parameter :: NB_CHOU     = NB_CHOU_UV + NB_CHOU_NIR ! total number of bands

  type bandptr
   real, pointer, dimension(:)  :: b => null()
  end type bandptr

  character(len = 2) :: suffix
  character(len = 3) :: label
  integer k

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

! Set the state variable specs.
! -----------------------------

!BOS

!  !IMPORT STATE:

!  !EXPORT STATE:

     call MAPL_AddExportSpec(GC                          ,&
        LONG_NAME          = '10m_wind_speed'            ,&
        UNITS              = 'm s-1'                     ,&
        SHORT_NAME         = 'UU'                        ,&
        DIMS               = MAPL_DimsTileOnly           ,&
        VLOCATION          = MAPL_VLocationNone          ,&
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'CO2SC',                                     &
        LONG_NAME  = 'atmospheric co2 (carbon tracker)',          &
        UNITS      = '1e-6',                                      &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'DUDP'                      ,&
          LONG_NAME          = 'Dust Dry Deposition'       ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUDP/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'DUWT'                      ,&
          LONG_NAME          = 'Dust Wet Deposition'       ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUWT/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'DUSD'                      ,&
          LONG_NAME          = 'Dust Sedimentation'        ,&
          UNITS              = 'kg m-2 s-1'                ,&
          DIMS               = MAPL_DimsTileOnly           ,&
          UNGRIDDED_DIMS     = (/NUM_DUSD/)                ,&
          VLOCATION          = MAPL_VLocationNone          ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'BCDP'                            ,&
          LONG_NAME          = 'Black Carbon Dry Deposition'     ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_BCDP/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'BCWT'                            ,&
          LONG_NAME          = 'Black Carbon Wet Deposition'     ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_BCWT/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'OCDP'                            ,&
          LONG_NAME          = 'Organic Carbon Dry Deposition'   ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_OCDP/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'OCWT'                            ,&
          LONG_NAME          = 'Organic Carbon Wet Deposition'   ,&
          UNITS              = 'kg m-2 s-1'                      ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NUM_OCWT/)                      ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  )
     VERIFY_(STATUS)

! do these two following exports (FSWBAND and FSWBANDNA) satisfy with RRTMG shortwave?
    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'FSWBAND'                         ,                   &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air',&
          UNITS              = 'W m-2'                           ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                       ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  )
     VERIFY_(STATUS)

    call MAPL_AddExportSpec(GC,                    &
          SHORT_NAME         = 'FSWBANDNA'                       ,                                       &
          LONG_NAME          = 'net_surface_downward_shortwave_flux_per_band_in_air_assuming_no_aerosol',&
          UNITS              = 'W m-2'                           ,&
          DIMS               = MAPL_DimsTileOnly                 ,&
          UNGRIDDED_DIMS     = (/NB_CHOU/)                       ,&
          VLOCATION          = MAPL_VLocationNone                ,&
          RC=STATUS  )
     VERIFY_(STATUS)
!
 
     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'CCOVM',                                     &
        LONG_NAME  = 'cloud cover',                               &
        UNITS      = 'fraction (dimensionless)',                  &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'CDREM',                                     &
        LONG_NAME  = 'cloud droplet effective radius',            &
        UNITS      = '',                                          &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'RLWPM',                                     &
        LONG_NAME  = 'cloud liquid water path',                   &
        UNITS      = '',                                          &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'CLDTCM',                                    &
        LONG_NAME  = 'cloud optical thickness',                   &
        UNITS      = '',                                          &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'RH',                                        &
        LONG_NAME  = 'relative humidity',                         &
        UNITS      = 'percent',                                   &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'OZ',                                        &
        LONG_NAME  = 'ozone thickness',                           &
        UNITS      = 'Dobson units',                              &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                                  &
        SHORT_NAME = 'WV',                                        &
        LONG_NAME  = 'water vapor',                               &
        UNITS      = 'cm',                                        &
        DIMS       = MAPL_DimsTileOnly,                           &
        VLOCATION  = MAPL_VLocationNone,                          &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

! gridded export names have a 'g' in the end
 
     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'DUDP_CLAYg',                        &   
        LONG_NAME  = 'dry dust deposition clay',          &   
        UNITS      = 'kg m-2 s-1',                        &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)
     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'DUDP_SUMg',                         &   
        LONG_NAME  = 'dry dust deposition sum',           &   
        UNITS      = 'kg m-2 s-1',                        &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'DUWT_CLAYg',                        &   
        LONG_NAME  = 'wet dust deposition clay',          &   
        UNITS      = 'kg m-2 s-1',                        &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'DUWT_SUMg',                         &   
        LONG_NAME  = 'wet dust deposition sum',           &   
        UNITS      = 'kg m-2 s-1',                        &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'DUSD_CLAYg',                        &   
        LONG_NAME  = 'sed dust deposition clay',          &   
        UNITS      = 'kg m-2 s-1',                        &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'DUSD_SUMg',                         &   
        LONG_NAME  = 'sed dust deposition sum',           &   
        UNITS      = 'kg m-2 s-1',                        &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)
 
     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'CCOVMg',                            &   
        LONG_NAME  = 'cloud cover',                       &   
        UNITS      = 'fraction (dimensionless)',          &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'CDREMg',                            &   
        LONG_NAME  = 'cloud droplet effective radius',    &   
        UNITS      = '',                                  &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'CLDTCMg',                           &   
        LONG_NAME  = 'cloud optical thickness',           &   
        UNITS      = '',                                  &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'RLWPMg',                            &   
        LONG_NAME  = 'cloud liquid waterh path',          &   
        UNITS      = '',                                  &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'RHg',                               &   
        LONG_NAME  = 'relative humidity',                 &   
        UNITS      = 'percent',                           &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &   
        SHORT_NAME = 'OZg',                               &   
        LONG_NAME  = 'ozone thickness',                   &   
        UNITS      = 'Dobson units',                      &   
        DIMS       = MAPL_DimsHorzOnly,                   &   
        VLOCATION  = MAPL_VLocationNone,                  &   
                                               RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddExportSpec(GC,                          &
        SHORT_NAME = 'WVg',                               &
        LONG_NAME  = 'water vapor',                       &
        UNITS      = 'cm',                                &
        DIMS       = MAPL_DimsHorzOnly,                   &
        VLOCATION  = MAPL_VLocationNone,                  &
                                               RC=STATUS  )
     VERIFY_(STATUS)
 
     do k=1, 33
       write(unit = suffix, fmt = '(i2.2)') k

       call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME = 'TAUA_'//suffix,                             &
          LONG_NAME  = 'aerosol optical thickness',                 &
          UNITS      = '',                                          &
          DIMS       = MAPL_DimsTileOnly,                           &
          VLOCATION  = MAPL_VLocationNone,                          &
          RC=STATUS  )
       VERIFY_(STATUS)

       call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME = 'ASYMP_'//suffix,                            &
          LONG_NAME  = 'asymmetry parameter',                       &
          UNITS      = '',                                          &
          DIMS       = MAPL_DimsTileOnly,                           &
          VLOCATION  = MAPL_VLocationNone,                          &
          RC=STATUS  )
       VERIFY_(STATUS)

       call MAPL_AddExportSpec(GC,                                  &
          SHORT_NAME = 'SSALB_'// suffix,                           &
          LONG_NAME  = 'single scattering albedo',                  &
          UNITS      = '',                                          &
          DIMS       = MAPL_DimsTileOnly,                           &
          VLOCATION  = MAPL_VLocationNone,                          &
          RC=STATUS  )
       VERIFY_(STATUS)
       call MAPL_AddExportSpec(GC,                         &
           SHORT_NAME = 'TAUA_'//suffix//'g',               &
           LONG_NAME  = 'aerosol optical thickness',        &
           UNITS      = '',                                 &
           DIMS       = MAPL_DimsHorzOnly,                  &
           VLOCATION  = MAPL_VLocationNone,                 &
           RC=STATUS  )
        VERIFY_(STATUS)

       call MAPL_AddExportSpec(GC,                         &
           SHORT_NAME = 'ASYMP_'//suffix//'g',              &
           LONG_NAME  = 'asymmetry parameter',              &
           UNITS      = '',                                 &
           DIMS       = MAPL_DimsHorzOnly,                  &
           VLOCATION  = MAPL_VLocationNone,                 &
           RC=STATUS  )
        VERIFY_(STATUS)

       call MAPL_AddExportSpec(GC,                         &
           SHORT_NAME = 'SSALB_'//suffix//'g',              &
           LONG_NAME  = 'single scattering albedo',         &
           UNITS      = '',                                 &
           DIMS       = MAPL_DimsHorzOnly,                  &
           VLOCATION  = MAPL_VLocationNone,                 &
           RC=STATUS  )
        VERIFY_(STATUS)
     enddo
!
!  !INTERNAL STATE:
!

!EOS

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

!BOP
! !IROUTINE: RUN -- Run stage for the OBIO, ORAD DataAtm component
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
  type (ESMF_Config)              :: CF
  type (ESMF_State   )            :: INTERNAL
  type (MAPL_LocStream)           :: LOCSTREAM

  type (ESMF_Time)                :: CURRENT_TIME
  type (ESMF_Time)                :: MODELSTART
  type (ESMF_TimeInterval)        :: DELT

  character(len=ESMF_MAXSTR)      :: DATAFILE

! pointers to export

  real, pointer, dimension(:  )  :: UU           => null()

  real, pointer, dimension(:  )  :: dry_clay     => null()
  real, pointer, dimension(:  )  :: wet_clay     => null()
  real, pointer, dimension(:  )  :: sed_clay     => null()
  real, pointer, dimension(:  )  :: ccovm        => null()
  real, pointer, dimension(:  )  :: cldtcm       => null()
  real, pointer, dimension(:  )  :: rlwpm        => null()
  real, pointer, dimension(:  )  :: cdrem        => null()
  real, pointer, dimension(:  )  :: rh           => null()
  real, pointer, dimension(:  )  :: oz           => null()
  real, pointer, dimension(:  )  :: wv           => null()
  real, pointer, dimension(:  )  :: taua         => null()
  real, pointer, dimension(:  )  :: asymp        => null()
  real, pointer, dimension(:  )  :: ssalb        => null()
  real, pointer, dimension(:  )  :: co2sc        => null()
  real, pointer, dimension(:  )  :: ccovmx       => null()
  real, pointer, dimension(:  )  :: cldtcmx      => null()
  real, pointer, dimension(:  )  :: rlwpmx       => null()
  real, pointer, dimension(:  )  :: cdremx       => null()
  real, pointer, dimension(:  )  :: rhx          => null()
  real, pointer, dimension(:  )  :: ozx          => null()
  real, pointer, dimension(:  )  :: wvx          => null()
  real, pointer, dimension(:  )  :: tauax        => null()
  real, pointer, dimension(:  )  :: asympx       => null()
  real, pointer, dimension(:  )  :: ssalbx       => null()
  real, pointer, dimension(:  )  :: co2scx       => null()
  real, pointer, dimension(:,:)  :: dry_clayx    => null()
  real, pointer, dimension(:,:)  :: wet_clayx    => null()
  real, pointer, dimension(:,:)  :: sed_clayx    => null()
  type(bandptr), dimension(33)   :: ataua
  type(bandptr), dimension(33)   :: aasymp
  type(bandptr), dimension(33)   :: assalb
  type(bandptr), dimension(33)   :: atauax
  type(bandptr), dimension(33)   :: aasympx
  type(bandptr), dimension(33)   :: assalbx

!
  real, pointer, dimension(:  )  :: LONS  => null()

  real                           :: DT
  integer                        :: NT
  integer                        :: K
  
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

  call MAPL_Get(MAPL, LOCSTREAM=LOCSTREAM,   RC=STATUS )
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

! Temporary space for reading forcings
!-------------------------------------

  allocate( dry_clay(NT), STAT=STATUS); VERIFY_(STATUS)
! allocate( dry_sum( NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( wet_clay(NT), STAT=STATUS); VERIFY_(STATUS)
! allocate( wet_sum( NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( sed_clay(NT), STAT=STATUS); VERIFY_(STATUS)
! allocate( sed_sum( NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( co2sc(   NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( ccovm(   NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( cldtcm(  NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( rlwpm(   NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( cdrem(   NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( rh(      NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( oz(      NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( wv(      NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( taua(    NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( asymp(   NT), STAT=STATUS); VERIFY_(STATUS)
  allocate( ssalb(   NT), STAT=STATUS); VERIFY_(STATUS)

! Do extra allocation if gridded exports are requested
!-----------------------------------------------------

  call MK_GRID_OUT(EXPORT, GNAME='CCOVMg',  TNAME='CCOVM',  RC=STATUS)
  call MK_GRID_OUT(EXPORT, GNAME='CLDTCMg', TNAME='CLDTCM', RC=STATUS)
  call MK_GRID_OUT(EXPORT, GNAME='RLWPMg',  TNAME='RLWPM',  RC=STATUS)
  call MK_GRID_OUT(EXPORT, GNAME='CDREMg',  TNAME='CDREM',  RC=STATUS)
  call MK_GRID_OUT(EXPORT, GNAME='RHg',     TNAME='RH',     RC=STATUS)
  call MK_GRID_OUT(EXPORT, GNAME='OZg',     TNAME='OZ',     RC=STATUS)
  call MK_GRID_OUT(EXPORT, GNAME='WVg',     TNAME='WV',     RC=STATUS)

  do k=1, 33
    write(unit = suffix, fmt = '(i2.2)') k
    call MK_GRID_OUT(EXPORT, GNAME='TAUA_'//suffix//'g',  &
                             TNAME='TAUA_'//suffix,  RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='ASYMP_'//suffix//'g', &
                             TNAME='ASYMP_'//suffix, RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='SSALB_'//suffix//'g', &
                             TNAME='SSALB_'//suffix, RC=STATUS)
  enddo

! Pointers to Imports
!--------------------
! nothing

! Pointers to Internals
!----------------------
! nothing
 
!  Pointers to Exports
!---------------------

  call GET_POINTER(EXPORT, UU,    'UU'      ,  RC=STATUS); VERIFY_(STATUS)

  call GET_POINTER(EXPORT, dry_clayx, 'DUDP',  RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, wet_clayx, 'DUWT',  RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, sed_clayx, 'DUSD',  RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, co2scx,    'CO2SC', RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, ccovmx,    'CCOVM', RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, cldtcmx,   'CLDTCM',RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, rlwpmx,    'RLWPM', RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, cdremx,    'CDREM', RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, rhx,       'RH',    RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, ozx,       'OZ',    RC=STATUS); VERIFY_(STATUS)
  call GET_POINTER(EXPORT, wvx,       'WV',    RC=STATUS); VERIFY_(STATUS)

  do k=1, 33
     write(unit = suffix, fmt = '(i2.2)') k
     call GET_POINTER(EXPORT, tauax, 'TAUA_'//suffix, RC=STATUS); VERIFY_(STATUS)
     atauax(k)%b => tauax

     call GET_POINTER(EXPORT, asympx, 'ASYMP_'//suffix, RC=STATUS); VERIFY_(STATUS)
     aasympx(k)%b => asympx

     call GET_POINTER(EXPORT, ssalbx, 'SSALB_'//suffix, RC=STATUS); VERIFY_(STATUS)
     assalbx(k)%b => ssalbx
  enddo

! Read "forcing fields"
! ---------------------

! Clay-Sized Dry Atmospheric Dust Depositions
!--------------------------------------------
  do K = 1, NUM_DUDP
    write(label,'(I3.3)') K
    call MAPL_GetResource( MAPL, DATAFILE, LABEL='DUDP'//label//'_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(DATAFILE) == 'none') then
      dry_clay = 0.0
    else 
      call MAPl_ReadForcing( MAPL, 'DUDP'//label, DATAFILE, CURRENT_TIME, dry_clay, RC=STATUS )
      VERIFY_(STATUS)
    endif   
    if (associated(dry_clayx)) dry_clayx(:,K) = dry_clay
  end do  

! Clay-Sized Wet Atmospheric Dust Depositions
!--------------------------------------------
  do K = 1, NUM_DUWT
    write(label,'(I3.3)') K
    call MAPL_GetResource( MAPL, DATAFILE, LABEL='DUWT'//label//'_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(DATAFILE) == 'none') then
      wet_clay = 0.0
    else 
      call MAPl_ReadForcing( MAPL, 'DUWT'//label, DATAFILE, CURRENT_TIME, wet_clay, RC=STATUS )
      VERIFY_(STATUS)
    endif   
    if (associated(wet_clayx)) wet_clayx(:,K) = wet_clay
  end do  

! Clay-Sized Sedimentary Atmospheric Dust Depositions
!----------------------------------------------------
  do K = 1, NUM_DUSD
    write(label,'(I3.3)') K
    call MAPL_GetResource( MAPL, DATAFILE, LABEL='DUSD'//label//'_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(DATAFILE) == 'none') then
      sed_clay = 0.0
    else 
      call MAPl_ReadForcing( MAPL, 'DUSD'//label, DATAFILE, CURRENT_TIME, sed_clay, RC=STATUS )
    VERIFY_(STATUS)
    endif   
    if (associated(sed_clayx)) sed_clayx(:,K) = sed_clay
  end do  

! Atmospheric Clouds (Atmospheric Optics)
!----------------------------------------
  call MAPL_GetResource( MAPL, DATAFILE, LABEL='CCOVM_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    ccovm = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'CCOVM', DATAFILE, CURRENT_TIME, ccovm, RC=STATUS )
    VERIFY_(STATUS)
  endif

  call MAPL_GetResource( MAPL, DATAFILE, LABEL='CLDTCM_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    cldtcm = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'CLDTCM', DATAFILE, CURRENT_TIME, cldtcm, RC=STATUS )
    VERIFY_(STATUS)
  endif

  call MAPL_GetResource( MAPL, DATAFILE, LABEL='RLWPM_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    rlwpm = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'RLWPM', DATAFILE, CURRENT_TIME, rlwpm, RC=STATUS )
    VERIFY_(STATUS)
  endif

  call MAPL_GetResource( MAPL, DATAFILE, LABEL='CDREM_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    cdrem = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'CDREM', DATAFILE, CURRENT_TIME, cdrem, RC=STATUS )
    VERIFY_(STATUS)
  endif

! Atmospheric Properties (Atmospheric Optics)
!--------------------------------------------
  call MAPL_GetResource( MAPL, DATAFILE, LABEL='RH_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    rh = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'RH', DATAFILE, CURRENT_TIME, rh, RC=STATUS )
    VERIFY_(STATUS)
  endif

  call MAPL_GetResource( MAPL, DATAFILE, LABEL='OZ_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    oz = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'OZ', DATAFILE, CURRENT_TIME, oz, RC=STATUS )
    VERIFY_(STATUS)
  endif

  call MAPL_GetResource( MAPL, DATAFILE, LABEL='WV_FILE:', default = 'none', RC=STATUS )
  VERIFY_(STATUS)
  if(trim(DATAFILE) == 'none') then
    wv = 0.0
  else
    call MAPl_ReadForcing( MAPL, 'WV', DATAFILE, CURRENT_TIME, wv, RC=STATUS )
    VERIFY_(STATUS)
  endif

! Atmospheric Carbon Dioxide from Carbon Tracker (_2011_OI)
!----------------------------------------------------------
  call MAPL_GetResource( MAPL, DATAFILE, LABEL='CO2SC_FILE:', RC=STATUS )
  VERIFY_(STATUS)
  call MAPl_ReadForcing( MAPL, 'CO2SC', DATAFILE, CURRENT_TIME, co2sc,  RC=STATUS )
  VERIFY_(STATUS)
  if ( associated(co2scx) ) co2scx = co2sc

! MODIS Aerosols (Atmospheric Optics)
!------------------------------------
  do k=1, 33
   write(unit = suffix, fmt = '(i2.2)') k
   call MAPL_GetResource( MAPL, DATAFILE, LABEL='TAUA_FILE:', default = 'none', RC=STATUS )
   VERIFY_(STATUS)
   if(trim(DATAFILE) == 'none') then
     taua = 0.0
   else
     call MAPL_ReadForcing( MAPL, 'TAUA_' // suffix, trim(DATAFILE) // suffix, CURRENT_TIME, taua, RC=STATUS)
     VERIFY_(STATUS)
   endif
   ataua(k)%b => taua

   call MAPL_GetResource( MAPL, DATAFILE, LABEL='ASYMP_FILE:', default = 'none', RC=STATUS )
   VERIFY_(STATUS)
   if(trim(DATAFILE) == 'none') then
     asymp = 0.0
   else
     call MAPL_ReadForcing( MAPL, 'ASYMP_' // suffix, trim(DATAFILE) // suffix, CURRENT_TIME, asymp, RC=STATUS)
     VERIFY_(STATUS)
   endif
   aasymp(k)%b => asymp

   call MAPL_GetResource( MAPL, DATAFILE, LABEL='SSALB_FILE:', default = 'none', RC=STATUS )
   VERIFY_(STATUS)
   if(trim(DATAFILE) == 'none') then
     ssalb = 0.0
   else
     call MAPL_ReadForcing( MAPL, 'SSALB_' // suffix, trim(DATAFILE) // suffix, CURRENT_TIME, ssalb, RC=STATUS)
     VERIFY_(STATUS)
   endif
   assalb(k)%b => ssalb
  enddo

! Dust depositions
!-----------------

! if ( associated( dry_clayx)) dry_clayx = dry_clay
! if ( associated( dry_sumx) ) dry_sumx  = dry_sum
! if ( associated( wet_clayx)) wet_clayx = wet_clay
! if ( associated( wet_sumx) ) wet_sumx  = wet_sum
! if ( associated( sed_clayx)) sed_clayx = sed_clay
! if ( associated( sed_sumx) ) sed_sumx  = sed_sum

  if ( associated( ccovmx)  ) ccovmx  = ccovm
  if ( associated( cldtcmx) ) cldtcmx = cldtcm
  if ( associated( rlwpmx)  ) rlwpmx  = rlwpm
  if ( associated( cdremx)  ) cdremx  = cdrem
  if ( associated( rhx)     ) rhx     = rh 
  if ( associated( ozx)     ) ozx     = oz 
  if ( associated( wvx)     ) wvx     = wv 
  do k=1, 33
   if ( associated( atauax(k)%b)  ) atauax(k)%b  = ataua(k)%b
   if ( associated( aasympx(k)%b) ) aasympx(k)%b = aasymp(k)%b
   if ( associated( assalbx(k)%b) ) assalbx(k)%b = assalb(k)%b
  enddo

  if(associated(UU))  UU = sqrt(u10*u10 + v10*v10)

! Output gridded variables
! ------------------------

  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUDP_CLAYg', TNAME='DUDP_CLAY', RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUDP_SUMg',  TNAME='DUDP_SUM',  RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUWT_CLAYg', TNAME='DUWT_CLAY', RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUWT_SUMg',  TNAME='DUWT_SUM',  RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUSD_CLAYg', TNAME='DUSD_CLAY', RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUSD_SUMg',  TNAME='DUSD_SUM',  RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='CCOVMg',     TNAME='CCOVM',     RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='CLDTCMg',    TNAME='CLDTCM',    RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='RLWPMg',     TNAME='RLWPM',     RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='CDREMg',     TNAME='CDREM',     RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='RHg',        TNAME='RH',        RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='OZg',        TNAME='OZ',        RC=STATUS)
  call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='WVg',        TNAME='WV',        RC=STATUS)
  do k=1, 33
     write(unit = suffix, fmt = '(i2.2)') k
     call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='TAUA_'//suffix//'g', &
                                        TNAME='TAUA_'//suffix,  RC=STATUS)
     call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='ASYMP_'//suffix//'g',&
                                        TNAME='ASYMP_'//suffix, RC=STATUS)
     call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='SSALB_'//suffix//'g',&
                                        TNAME='SSALB_'//suffix, RC=STATUS)
  enddo

! Clean up
!---------

  deallocate(dry_clay)
  deallocate(wet_clay)
  deallocate(sed_clay)
  deallocate(co2sc)
  deallocate(ccovm)
  deallocate(cldtcm)
  deallocate(rlwpm)
  deallocate(cdrem)
  deallocate(rh)
  deallocate(oz)
  deallocate(wv)
  deallocate(taua)
  deallocate(asymp)
  deallocate(ssalb)

! All done
!-----------

  call MAPL_TimerOff(MAPL,"RUN"  )
  call MAPL_TimerOff(MAPL,"TOTAL")

  RETURN_(ESMF_SUCCESS)

  end subroutine RUN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module GEOS_ObioRadDataAtmGridCompMod
