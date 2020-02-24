!  $Id$

#include "MAPL_Generic.h"

!=============================================================================

module GEOS_DataAtmGridCompMod

!BOP

! !MODULE: GEOS_DataAtm -- A ``fake'' atmospheric component.

! !USES: 

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

!=============================================================================

! !DESCRIPTION:
! 
!   {\tt GEOS\_DataAtm  } is a gridded component that reads the 
!   ocean forcings files. Each forcing field is a a separate file
!   that uses the same format as the SST files used by dataocean, datase, etc. 

!   This module interpolates the SST and sea ice data from 
!   either daily or monthly values to the correct time of the simulation.
!   Data are read only if the simulation time is not in the save interval.
!   Surface Albedo and Surface roughness calculations are also takencare of in
!   this module.
!
!   Santha: This module (as it stands below) uses an old version of CICE Thermodynamics 
!           which was same as in CICEThermo. But CICEThermo has been merged with Saltwater
!           and most of the CICE thermodynamics interface has changed (from what's below)
!           So-- please be aware of that, while using this module. July, 2015.

!EOP


   contains

!BOP

! !IROUTINE: SetServices -- Sets ESMF services for this component

! !INTERFACE:

  subroutine SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

!  !DESCRIPTION: This version uses the MAPL\_GenericSetServices. This function sets
!                the Initialize and Finalize services, as well as allocating
!   our instance of a generic state and putting it in the 
!   gridded component (GC). Here we only need to set the run method and
!   add the state variable specifications (also generic) to our instance
!   of the generic state. This is the way our true state variables get into
!   the ESMF\_State INTERNAL, which is in the MAPL\_MetaComp.
!
!EOP

!=============================================================================
!
! ErrLog Variables


!BOS

!  !IMPORT STATE:


! Imports from CICE dynamics


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

  if (DO_CICE_THERMO == 0) then
     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'TSKINI',                            &
        LONG_NAME          = 'ice_skin_temperature',              &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = MAPL_TICE-1.8,                               &
                                                       RC=STATUS  )
     VERIFY_(STATUS)
  else
     call MAPL_AddInternalSpec(GC,                                &
         SHORT_NAME         = 'TSKINI',                            &
         LONG_NAME          = 'ice_skin_temperature',              &
         UNITS              = 'K',                                 &
         UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &    ! accroding to Atanas, because of this line, TSKINI is now rank 2 array.
         DIMS               = MAPL_DimsTileOnly,                   &    ! and therefore must be protected via DO_CICE_THERMO flag. SA. Aug.2015
         VLOCATION          = MAPL_VLocationNone,                  &
         FRIENDLYTO         = 'SEAICE',                            &
         DEFAULT            = MAPL_TICE-1.8,                       &
                                           RC=STATUS  )
    VERIFY_(STATUS)
  end if



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
        SHORT_NAME         = 'HSKINW',                            &
        LONG_NAME          = 'water_skin_layer_mass',             &
        UNITS              = 'kg m-2',                            &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 5.0*MAPL_RHOWTR,                     &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'SSKINW',                            &
        LONG_NAME          = 'water_skin_salinity',               &
        UNITS              = 'psu',                               &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 30.0,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'TSKINW',                            &
        LONG_NAME          = 'water_skin_temperature',            &
        UNITS              = 'K',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 280.0,                               &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'FR',                                &
        LONG_NAME          = 'subtile_fractions_of_grid_cell',    &
        UNITS              = '1',                                 &
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'OCEAN:SEAICE',                      &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


     call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLICE',                            &
        LONG_NAME          = 'ice_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
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
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_ICE_CATEGORIES/),              &
        VLOCATION          = MAPL_VLocationNone,                  &
        FRIENDLYTO         = 'SEAICE',                            &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'VOLPOND',                            &
        LONG_NAME          = 'pond_category_volume_per_unit_area_of_grid_cell',&
        UNITS              = 'm',                                 &
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
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
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
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
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
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
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
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
#ifdef USE_R8
        PRECISION          = ESMF_KIND_R8,                        &
#endif
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

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'QS',                                &
        LONG_NAME          = 'surface_specific_humidity',         &
        UNITS              = '1',                                 &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.01,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CH',                                &
        LONG_NAME          = 'surface_heat_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CM',                                &
        LONG_NAME          = 'surface_momentum_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'CQ',                                &
        LONG_NAME          = 'surface_moisture_exchange_coefficient', &
        UNITS              = 'kg m-2 s-1',                        &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 1.0e-4,                              &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'Z0',                                &
        LONG_NAME          = 'aerodynamic_roughness',             &
        UNITS              = 'm',                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.00005,                             &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'WW',                                &
        LONG_NAME          = 'vertical_velocity_scale_squared',   &
        UNITS              = 'm+2 s-2',                           &
        DIMS               = MAPL_DimsTileOnly,                   &
        UNGRIDDED_DIMS     = (/NUM_SUBTILES/),                    &
        VLOCATION          = MAPL_VLocationNone,                  &
        DEFAULT            = 0.0,                                 &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                                &
        SHORT_NAME         = 'TWMTS',                             &
        LONG_NAME          = 'departure_of_skin_temperature_from_mean_interface_temperature',   &
        UNITS              = 'K',                                 &
        DEFAULT            = 0.0,                                 &
        DIMS               = MAPL_DimsTileOnly,                   &
        VLOCATION          = MAPL_VLocationNone,                  &
                                                       RC=STATUS  )
     VERIFY_(STATUS)

     call MAPL_AddInternalSpec(GC,                           &
        SHORT_NAME         = 'SLMASK',                       &
        LONG_NAME          = 'salt_water_lake_mask',         &
        UNITS              = '1',                       &
        DIMS               = MAPL_DimsTileOnly,                  &
        VLOCATION          = MAPL_VLocationNone,                 &
        DEFAULT            = 0.0,                                &
                                                       RC=STATUS  )
     VERIFY_(STATUS)


!  !EXPORT STATE:

!EOS


    RETURN_(ESMF_SUCCESS)
  
  end subroutine SetServices



!BOP

! !IROUTINE: INITIALIZE -- Initialize stage for the DataAtm component

! !INTERFACE:

subroutine INITIALIZE ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

  type(ESMF_GridComp), intent(inout) :: GC     ! Gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! Import state
  type(ESMF_State),    intent(inout) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! The clock
  integer, optional,   intent(  out) :: RC     ! Error code:

! !DESCRIPTION: 

!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

! Locals

  type (MAPL_MetaComp), pointer   :: STATE => null()
  type (MAPL_LocStream)               :: LOCSTREAM
  type (MAPL_LocStream)               :: EXCH


!  Begin...
!----------


! Change the location stream to just the ocean part
!--------------------------------------------------

    call MAPL_Get(STATE, EXCHANGEGRID=EXCH,        RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_LocStreamCreate(LOCSTREAM, EXCH, NAME='OCEAN', &
                                       MASK=(/MAPL_OCEAN/), RC=STATUS )
    VERIFY_(STATUS)
    call MAPL_Set(STATE, LOCSTREAM=LOCSTREAM,   RC=STATUS )
    VERIFY_(STATUS)


    RETURN_(ESMF_SUCCESS)

  end subroutine INITIALIZE

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

! !DESCRIPTION: Periodically refreshes the SST and Ice information.

!EOP

! ErrLog Variables

  character(len=ESMF_MAXSTR)          :: IAm
  integer                             :: STATUS
  character(len=ESMF_MAXSTR)          :: COMP_NAME

  integer                                 :: DO_CICE_THERMO  ! default (=0) is to run without CICE

! Locals

  type (MAPL_MetaComp), pointer   :: STATE => null()
  type (MAPL_SunOrbit)                :: ORBIT
  type (ESMF_State)                   :: INTERNAL
  type (ESMF_logical)                 :: FRIENDLY
  type (ESMF_FIELD)                   :: FIELD
  type (ESMF_Time)                    :: CurrentTime
  type (ESMF_GRID)                    :: GRID
  type (ESMF_Config  )                :: CF
  character(len=ESMF_MAXSTR)          :: DATAFILE
  integer                             :: NT
  integer                             :: IFCST
  logical                             :: FCST
  real, pointer, dimension(  :)       :: TILELONS => null()
  real, pointer, dimension(  :)       :: TILELATS => null()

  real, pointer, dimension(  :)       :: rain => null()
  real, pointer, dimension(  :)       :: snow => null()
  real, pointer, dimension(  :)       :: rr => null()
  real, pointer, dimension(  :)       :: swrad => null()
  real, pointer, dimension(  :)       :: lwrad => null()
  real, pointer, dimension(  :)       :: t10 => null()
  real, pointer, dimension(  :)       :: q10 => null()
  real, pointer, dimension(  :)       :: u10, u10m => null()
  real, pointer, dimension(  :)       :: v10, v10m => null()
  real, pointer, dimension(  :)       :: slp => null()
  real, pointer, dimension(  :)       :: sss => null()
  real, pointer, dimension(  :)       :: rhoa => null()
  real, pointer, dimension(  :)       :: qsat => null()
  real, pointer, dimension(  :)       :: pen => null()
  real, pointer, dimension(  :)       :: ustar => null()
  real, pointer, dimension(  :)       :: cd => null()
  real, pointer, dimension(  :)       :: ch => null()
  real, pointer, dimension(  :)       :: ce => null()

  real, pointer, dimension(  :)       :: dry_clay => null()
  real, pointer, dimension(  :)       :: wet_clay => null()
  real, pointer, dimension(  :)       :: sed_clay => null()
  real, pointer, dimension(  :)       :: ccovm => null()
  real, pointer, dimension(  :)       :: cldtcm => null()
  real, pointer, dimension(  :)       :: rlwpm => null()
  real, pointer, dimension(  :)       :: cdrem => null()
  real, pointer, dimension(  :)       :: rh => null()
  real, pointer, dimension(  :)       :: oz => null()
  real, pointer, dimension(  :)       :: wv => null()
  real, pointer, dimension(  :)       :: taua => null()
  real, pointer, dimension(  :)       :: asymp => null()
  real, pointer, dimension(  :)       :: ssalb => null()
  real, pointer, dimension(  :)       :: co2sc
  type(bandptr), dimension( 33)       :: ataua
  type(bandptr), dimension( 33)       :: aasymp
  type(bandptr), dimension( 33)       :: assalb
  real, pointer, dimension(:,:)  :: dry_clayx => null()
  real, pointer, dimension(:,:)  :: wet_clayx => null()
  real, pointer, dimension(:,:)  :: sed_clayx => null()
  real, pointer, dimension(:)  :: ccovmx => null()
  real, pointer, dimension(:)  :: cldtcmx => null()
  real, pointer, dimension(:)  :: rlwpmx => null()
  real, pointer, dimension(:)  :: cdremx => null()
  real, pointer, dimension(:)  :: rhx => null()
  real, pointer, dimension(:)  :: ozx => null()
  real, pointer, dimension(:)  :: wvx => null()
  real, pointer, dimension(:)  :: tauax => null()
  real, pointer, dimension(:)  :: asympx => null()
  real, pointer, dimension(:)  :: ssalbx => null()
  real, pointer, dimension(:)  :: co2scx
  type(bandptr), dimension(33) :: atauax
  type(bandptr), dimension(33) :: aasympx
  type(bandptr), dimension(33) :: assalbx

! Temporary vars
  real, pointer, dimension(  :)       :: var1 => null()
  real, pointer, dimension(  :)       :: var2 => null()
  real, pointer, dimension(  :)       :: var3 => null()


  real                                :: TAUSSS
  real                                :: TAUSST
  real                                :: DT

  real :: MAXWATERDEPTH   
  real :: MINWATERDEPTH   
  real :: MAXICEDEPTH     
  real :: MINICEDEPTH     

  real, parameter :: FRUVR          = 0.07
  real, parameter :: FRPAR          = 0.40
  real, parameter :: FRNIR          = 0.53

  ! partitioning of shortwave radiation for ice 
  real, parameter :: FRVISDIR       = 0.29
  real, parameter :: FRVISDIF       = 0.31
  real, parameter :: FRNIRDIR       = 0.24
  real, parameter :: FRNIRDIF       = 0.16

  real, parameter ::  KUVR          = 0.09
  real, parameter ::  KNIR          = 340.6

  real, parameter :: alb            = 0.066

! pointers to import

   real, pointer, dimension(:)  :: KPAR => null()
   real, pointer, dimension(:)  :: UW => null()
   real, pointer, dimension(:)  :: VW => null()
   real, pointer, dimension(:)  :: UI => null()
   real, pointer, dimension(:)  :: VI => null()

! internal pointers to tile variables

   real, pointer, dimension(:)  :: TW => null()
   real, pointer, dimension(:)  :: TI => null()
   real, pointer, dimension(:)  :: HW => null()
   real, pointer, dimension(:)  :: HI => null()
   real, pointer, dimension(:)  :: SW => null()
   real, pointer, dimension(:)  :: SI => null()

   real, pointer, dimension(:,:)  :: TI8 => null()      ! ice temperature    with LANL CICE uses TI8 not TI.



! pointers to export

   real, pointer, dimension(:)  :: TAUXW => null()
   real, pointer, dimension(:)  :: TAUYW => null()
   real, pointer, dimension(:)  :: TAUXI => null()
   real, pointer, dimension(:)  :: TAUYI => null()
   real, pointer, dimension(:)  :: TAUXO => null()
   real, pointer, dimension(:)  :: TAUYO => null()
   real, pointer, dimension(:)  :: USTR3 => null()
   real, pointer, dimension(:)  :: UU => null()
   real, pointer, dimension(:)  :: PSEX => null()
   real, pointer, dimension(:)  :: PRUVR => null()
   real, pointer, dimension(:)  :: PRPAR => null()
   real, pointer, dimension(:)  :: PRUVF => null()
   real, pointer, dimension(:)  :: PRPAF => null()
   real, pointer, dimension(:)  :: swnx => null()
   real, pointer, dimension(:)  :: lwnx => null()
   real, pointer, dimension(:)  :: shfx => null()
   real, pointer, dimension(:)  :: lhfx => null()
   real, pointer, dimension(:)  :: evapx => null()
   real, pointer, dimension(:)  :: precipx => null()
   real, pointer, dimension(:)  :: smeltx => null()
   real, pointer, dimension(:)  :: FSURFL => null()

   type(MAPL_LocStream) :: LOCSTREAM

! pointers to export

   real, pointer, dimension(:  )  :: EMISS => null()
   real, pointer, dimension(:  )  :: ALBVF => null() 
   real, pointer, dimension(:  )  :: ALBVR => null() 
   real, pointer, dimension(:  )  :: ALBNF => null() 
   real, pointer, dimension(:  )  :: ALBNR => null() 
   real, pointer, dimension(:  )  :: EVAPOUT => null()
   real, pointer, dimension(:  )  :: SNOWOCN => null()
   real, pointer, dimension(:  )  :: RAINOCN => null()
   real, pointer, dimension(:  )  :: SHWTR => null()
   real, pointer, dimension(:  )  :: SHICE => null()
   real, pointer, dimension(:  )  :: SHOUT => null()
   real, pointer, dimension(:  )  :: HLATN => null()
   real, pointer, dimension(:  )  :: HLATW => null()
   real, pointer, dimension(:  )  :: HLATI => null()
   real, pointer, dimension(:  )  :: HLWUP => null()
   real, pointer, dimension(:  )  :: SWNDSRF => null()
   real, pointer, dimension(:  )  :: LWNDSRF => null()
   real, pointer, dimension(:  )  :: SWNDWTR => null()
   real, pointer, dimension(:  )  :: LWNDWTR => null()
   real, pointer, dimension(:  )  :: SWNDICE => null()
   real, pointer, dimension(:  )  :: LWNDICE => null()

   real, pointer, dimension(:  )  :: DELTS => null()
   real, pointer, dimension(:  )  :: DELQS => null()
   real, pointer, dimension(:  )  :: TST => null()
   real, pointer, dimension(:  )  :: QST => null()
   real, pointer, dimension(:  )  :: PENUVR => null()
   real, pointer, dimension(:  )  :: PENUVF => null()
   real, pointer, dimension(:  )  :: PENPAR => null()
   real, pointer, dimension(:  )  :: PENPAF => null()
   real, pointer, dimension(:  )  :: FRI => null()
   real, pointer, dimension(:  )  :: FRAZIL => null()
   real, pointer, dimension(:  )  :: FRZMLTL => null()
   real, pointer, dimension(:  )  :: CONGELO => null()
   real, pointer, dimension(:  )  :: SNOICEO => null()
   real, pointer, dimension(:  )  :: FRESH => null()
   real, pointer, dimension(:  )  :: FSALT => null()
   real, pointer, dimension(:  )  :: FHOCN => null()
   real, pointer, dimension(:  )  :: FSWTRUO => null()
   real, pointer, dimension(:  )  :: FSWABSO => null()
   real, pointer, dimension(:  )  :: MELTL => null()
   real, pointer, dimension(:  )  :: MELTTL => null()
   real, pointer, dimension(:  )  :: MELTBL => null()
   real, pointer, dimension(:  )  :: MELTSL => null()
   real, pointer, dimension(:  )  :: HICE => null()
   real, pointer, dimension(:  )  :: HSNO => null()
   real, pointer, dimension(:  )  :: HICEUNT => null()
   real, pointer, dimension(:  )  :: ISTSFC => null()
   real, pointer, dimension(:  )  :: TSKINWCICE => null()
   real, pointer, dimension(:  )  :: IAGE => null()
   real, pointer, dimension(:  )  :: SSKINW2 => null()
   real, pointer, dimension(:  )  :: DAIDTT => null()
   real, pointer, dimension(:  )  :: DVIDTT => null()
   real, pointer, dimension(:  )  :: FBOTL => null()
   real, pointer, dimension(:  )  :: USTARI => null()
   real, pointer, dimension(:  )  :: TWINC1 => null()
   real, pointer, dimension(:  )  :: TWINC2 => null()
   real, pointer, dimension(:  )  :: TWINC3 => null()
   real, pointer, dimension(:  )  :: TWINCT => null()
   real, pointer, dimension(:  )  :: FWFLUX => null()
   real, pointer, dimension(:  )  :: DISCHARGE => null()

! pointers to internal

   real, pointer, dimension(:,:)  :: TS => null()
   real, pointer, dimension(:,:)  :: TSC => null()
   real, pointer, dimension(:)    :: TSW => null()
#ifdef USE_R8
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:)  :: FR => null()
#else
   real, pointer, dimension(:,:)  :: FR => null()
#endif

#ifdef USE_R8
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:)   :: VOLICE => null()
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:)   :: VOLSNO => null()
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:)   :: VOLPOND => null()
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:)   :: APONDN => null()
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:)   :: HPONDN => null()
#else
   real, pointer, dimension(:,:)   :: VOLICE => null()
   real, pointer, dimension(:,:)   :: VOLSNO => null()
   real, pointer, dimension(:,:)   :: VOLPOND => null()
   real, pointer, dimension(:,:)   :: APONDN => null()
   real, pointer, dimension(:,:)   :: HPONDN => null()
#endif
#ifdef USE_R8
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:,:) :: ERGICE => null()
   real(kind=ESMF_KIND_R8), pointer, dimension(:,:,:) :: ERGSNO => null()
#else
   real, pointer, dimension(:,:,:) :: ERGICE => null()
   real, pointer, dimension(:,:,:) :: ERGSNO => null()
   real, pointer, dimension(:,:) :: ERGSUM => null()
#endif
   real, pointer, dimension(:,:)   :: TAUAGE => null()
   real, pointer, dimension(:  )   :: SLMASK => null()


! pointers to import

   real, pointer, dimension(:)    :: TAUXBOT => null()
   real, pointer, dimension(:)    :: TAUYBOT => null()


! local real allocatables

   real,    pointer,   dimension(:)    :: FRZMLT => null()
   real,    pointer,   dimension(:)    :: SHF => null()
   real,    pointer,   dimension(:)    :: EVP => null()
   real,    pointer,   dimension(:)    :: TXI => null()
   real,    pointer,   dimension(:)    :: TYI => null()
   real,    pointer,   dimension(:)    :: PUR => null()
   real,    pointer,   dimension(:)    :: PUF => null()
   real,    pointer,   dimension(:)    :: PPR => null()
   real,    pointer,   dimension(:)    :: PPF => null()
   real,    pointer,   dimension(:)    :: LHF => null()
   real,    pointer,   dimension(:)    :: LWUP => null()
   real,               dimension(1)    :: LHF0
   real,               dimension(1)    :: LWUP0
   real,               dimension(1)    :: SHF0
   real,    pointer,   dimension(:)    :: ZTH => null()
   real,    pointer,   dimension(:)    :: SLR => null()
   real,    pointer,   dimension(:,:)  :: ALBVRN => null()
   real,    pointer,   dimension(:,:)  :: ALBVFN => null()
   real,    pointer,   dimension(:,:)  :: ALBNRN => null()
   real,    pointer,   dimension(:,:)  :: ALBNFN => null()
   real,    pointer,   dimension(:,:)  :: FCOND => null()
   real,    pointer,   dimension(:,:)  :: FCONDBOT => null()
   real,    pointer,   dimension(:,:)  :: LHCOEFF => null()
   real,    pointer,   dimension(:,:)  :: SHCOEFF => null()
   real,    pointer,   dimension(:)    :: ALBVRI => null()
   real,    pointer,   dimension(:)    :: ALBVFI => null()
   real,    pointer,   dimension(:)    :: ALBNRI => null()
   real,    pointer,   dimension(:)    :: ALBNFI => null()
   real,    pointer,   dimension(:)    :: FSWABSUNDICE => null()

   integer, dimension(NUM_3D_ICE_TRACERS)                     :: TRCRTYPE
   real,    dimension(NUM_3D_ICE_TRACERS,NUM_ICE_CATEGORIES)  :: TRACERS

#ifdef USE_R8
   real(kind=ESMF_KIND_R8), pointer, dimension(:, :) :: AICENINIT => null()
   real(kind=ESMF_KIND_R8), pointer, dimension(:, :) :: VICENINIT => null()
#else
   real,                    pointer, dimension(:, :) :: AICENINIT => null()
   real,                    pointer, dimension(:, :) :: VICENINIT => null()
#endif

   integer                             :: N, Nsub, ICELLS
   integer                             :: K, L
   real,       pointer,  dimension(:)  :: RSIDE => null()
   real,       pointer,  dimension(:)  :: FRESHN => null()
   real,       pointer,  dimension(:)  :: FRESHL => null()
   real,       pointer,  dimension(:)  :: FSALTN => null()
   real,       pointer,  dimension(:)  :: FSALTL => null()
   real,       pointer,  dimension(:)  :: FHOCNN => null()
   real,       pointer,  dimension(:)  :: FHOCNL => null()
   real,       pointer,  dimension(:)  :: FRAZLN => null()
   real,       pointer,  dimension(:)  :: MELTLN => null()
#ifdef USE_R8
   real(kind=ESMF_KIND_R8), pointer, dimension(:) :: FRCICE => null()
#else
   real,                    pointer, dimension(:) :: FRCICE => null()
#endif
   real,    pointer,     dimension(:,:):: FSWTHRU => null()
   real,    pointer,     dimension(:,:):: FSWTHRUWTR => null()
   real,    pointer,     dimension(:)  :: TF => null()
   real,                 dimension(1)  :: FSWSFC
   real,                 dimension(1)  :: FSWINT
   real                                :: SSWABS(NUM_SNOW_LAYERS)
   real                                :: ISWABS(NUM_ICE_LAYERS)
   real,                  dimension(1) :: ALBIN
   real,                  dimension(1) :: ALBSN
   real,                  dimension(1) :: ALBPND
   real,    pointer,     dimension(:)  :: FSURF => null()
   real,    pointer,     dimension(:)  :: FSWABS => null()
   real,    pointer,     dimension(:)  :: MELTT => null()
   real,    pointer,     dimension(:)  :: MELTB => null()
   real,    pointer,     dimension(:)  :: MELTS => null()
   real,    pointer,     dimension(:)  :: SNOICE => null()
   real,    pointer,     dimension(:)  :: CONGEL => null()
   real,    pointer,     dimension(:)  :: DTS => null()
   real,    pointer,     dimension(:)  :: DTSACCUM => null()

   real                                :: MAXSALINITY
   real                                :: MINSALINITY

   type (ESMF_TimeInterval)            :: DELT
   real                                :: DT_SOLAR
   type (ESMF_TimeInterval)            :: TINT
   type (ESMF_Time)                    :: currTime

   real, parameter :: EMSH2O          = 0.99070
   real            :: EMSICE   

   real, parameter :: SALTWATERCAP    = MAPL_CAPWTR
   real, parameter :: SALTWATERICECAP = MAPL_CAPICE

   real, parameter :: FRZMLT_MAX = 1000.


   real, pointer,  dimension(:)    ::  TBOT => null()
   real, pointer,  dimension(:)    ::  FBOT => null()
   real, dimension(1)              ::  LONSD, LATSD
   real, dimension(1)              ::  FRZ_ONSET, MLT_ONSET
   real                            ::  YDAY
   real, dimension(1)              ::  RDUM 

   real, dimension(1)              ::  DRUVRTHRU,   DFUVRTHRU,  &
                                       DRPARTHRU,   DFPARTHRU

   character(len=ESMF_MAXSTR)      ::  SHORTWAVE 

#ifdef USE_R8
   
   real(kind=ESMF_KIND_R8),   dimension(NUM_3D_ICE_TRACERS)  :: TRACERSDB
 
   real(kind=ESMF_KIND_R8),    dimension(NUM_ICE_LAYERS) ::   ERGICEDB , &  
                                                   ISWABSDB  
   real(kind=ESMF_KIND_R8),    dimension(NUM_SNOW_LAYERS) ::   ERGSNODB, &
                                                   SSWABSDB

   real(kind=ESMF_KIND_R8)     DTDB 

   real(kind=ESMF_KIND_R8), dimension(1) :: RDUMDB

   real(kind=ESMF_KIND_R8), dimension(1) ::  FRCICEDB, & 
                   FRZMLTDB, &
                   TSCDB   , & 
                   TFDB    , &
                   TAUXBOTDB , &
                   TAUYBOTDB , & 
                   TBOTDB    , & 
                   FBOTDB    , & 
                   RSIDEDB   , &  
             FRDB       , & 
             VOLICEDB   , & 
             VOLSNODB   , &
             APONDNDB   , & 
             HPONDNDB   , & 
             VSUVRDB    , & 
             VSUVFDB    , &
             DRNIRDB    , & 
             DFNIRDB    , & 
             ALBVRNDB   , & 
             ALBNRNDB   , &
             ALBVFNDB   , &
             ALBNFNDB   , &
             FSWSFCDB   , &
             FSWINTDB   , &
             FSWTHRUDB  , &
             ALBINDB    , &
             ALBSNDB    , &
             LWDNSRFDB  , & 
             SNODB      , &
             FSWABSDB   , &
             FCONDDB    , &
             FCONDBOTDB , &
             EVPDB      , &
             FSURFDB    , & 
             DFSDTDB    , &
             SHF0DB     , & 
             LHF0DB     , & 
             LWUP0DB    , &
             FRESHNDB   , & 
             FSALTNDB   , &
             FHOCNNDB   , &
             MELTTDB    , &
             MELTSDB    , &
             MELTBDB    , &
             CONGELDB   , & 
             SNOICEDB   , & 
             DSHDB      , &
             DLHDTDB    , & 
             BLWDB      , &
             LATSDB     , & 
             LONSDB     , &
             MLT_ONSETDB, & 
             FRZ_ONSETDB 

   real(kind=ESMF_KIND_R8), dimension(1) ::  potTDB,      &    
                                    u10DB,  v10DB,        &
                                    windDB, zlvlDB,       &
                                    QaDB,   rhoaDB,       &
                                    TXIDB,  TYIDB,        &
                                    TrefDB, QrefDB,       &    
                                    deltDB, delqDB,       &    
                                    lhcoeffDB, shcoeffDB 

   real(kind=ESMF_KIND_R8)      YDAYDB, FRT  
 
   real(kind=ESMF_KIND_R8), dimension(NUM_ICE_LAYERS, NUM_ICE_CATEGORIES)  ::   ERGICEDB2
   real(kind=ESMF_KIND_R8), dimension(NUM_SNOW_LAYERS, NUM_ICE_CATEGORIES) ::   ERGSNODB2
   real(kind=ESMF_KIND_R8), dimension(NUM_3D_ICE_TRACERS, NUM_ICE_CATEGORIES) ::  & 
                                                                    TRACERSDB2 
         
   real(kind=ESMF_KIND_R8), dimension(1) ::  & 
       FRWATERDB, &  
       FRAZLNDB , &
       FRESHLDB , &
       FSALTLDB , &
       FHOCNLDB , & 
       MELTLNDB 

   real(kind=ESMF_KIND_R8), dimension(1) ::  & 
        DRUVRDB,       DFUVRDB,      &
        DRPARDB,       DFPARDB,      &
        DRUVRTHRUDB,   DFUVRTHRUDB,  &
        DRPARTHRUDB,   DFPARTHRUDB

#endif
   real, allocatable              :: FR_OLD(:), VOLICE_OLD(:)
#ifdef USE_R8
   real(kind=ESMF_KIND_R8),   dimension(NUM_ICE_CATEGORIES)    ::  VOLICE_PREV
#else
   real,                      dimension(NUM_ICE_CATEGORIES)    ::  VOLICE_PREV
#endif

#ifdef USE_R8
   real(kind=ESMF_KIND_R8),  dimension(1) :: COSZTH
   real(kind=ESMF_KIND_R8)                :: FSN(1), RHOSNWN(NUM_SNOW_LAYERS),&
                                             RSNWN(NUM_SNOW_LAYERS) 
   real(kind=ESMF_KIND_R8),  dimension(1) :: FPN, HPN 
   real(kind=ESMF_KIND_R8),  dimension(1) :: ALBPNDDB, FRAINDB  
#else
   real,                     dimension(1) :: COSZTH
   real                                   :: FSN(1), RHOSNWN(NUM_SNOW_LAYERS), &
                                             RSNWN(NUM_SNOW_LAYERS) 
   real,                     dimension(1) :: FPN, HPN 
   real,                     dimension(1) :: ALBPNDDB, FRAINDB  
#endif
   integer                     :: DO_POND
   logical                     :: TR_POND

!!!!#ifdef USE_CICE
!!!!   logical(kind=log_kind)  :: L_STOP
!!!!   integer(kind=int_kind)              :: IDUM, JDUM
       real (kind=dbl_kind) :: sblx(1,1)
!!!!#else
       logical :: L_STOP = .false.
       integer :: IDUM, JDUM
!!!!   real  :: sblx(1,1)
!!!!#endif

   integer                     ::  DO_DATAATM
   real                        ::  MINSWFRESH
   real                        ::  ICE_THICKNESS_THRESH
   real                        ::  ICE_ARTIFICIAL_MELT

   logical, dimension(1) :: OBSERVE
   real LATSO, LONSO
   type(ESMF_VM)      :: vm
   integer            :: mype

!  Begin...
!----------

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "Run"
    call ESMF_GridCompGet( GC, name=COMP_NAME, GRID=GRID, RC=STATUS )
    VERIFY_(STATUS)
    Iam = trim(COMP_NAME) // Iam

! Get my MAPL_Generic (GG) state
!-------------------------------

    call MAPL_GetObjectFromGC(GC, STATE, STATUS)
    VERIFY_(STATUS)

    call MAPL_Get(STATE, LOCSTREAM=LOCSTREAM,   RC=STATUS )
    VERIFY_(STATUS)

! Start timers
!-------------

    call MAPL_TimerOn(STATE,"TOTAL")
    call MAPL_TimerOn(STATE,"RUN" )

! Get info from the GG state
!---------------------------

    call MAPL_Get(STATE,            &
        INTERNAL_ESMF_STATE = INTERNAL,         &
        TILELONS=TILELONS,                      &
        TILELATS=TILELATS,                      &
        ORBIT   = ORBIT,                        &
                                      RC=STATUS )
    VERIFY_(STATUS)

! The number of tiles we are working on
!--------------------------------------

    NT = size(TILELONS)

! Temporary space for reading forcings
!-------------------------------------

    allocate(rain(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(snow(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(rr(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(swrad(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(lwrad(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(t10(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(q10(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(u10(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(v10(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(slp(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(sss(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(rhoa(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(qsat(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(pen(NT),STAT=STATUS);   VERIFY_(STATUS)
    allocate(ustar(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(cd(NT),STAT=STATUS);    VERIFY_(STATUS)
    allocate(ch(NT),STAT=STATUS);    VERIFY_(STATUS)
    allocate(ce(NT),STAT=STATUS);    VERIFY_(STATUS)

    allocate(var1(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(var2(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(var3(NT),STAT=STATUS);  VERIFY_(STATUS)

    allocate(FSWABSUNDICE(NT),STAT=STATUS);  VERIFY_(STATUS)

    allocate(dry_clay(NT),STAT=STATUS); VERIFY_(STATUS)
!    allocate(dry_sum(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(wet_clay(NT),STAT=STATUS); VERIFY_(STATUS)
!    allocate(wet_sum(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(sed_clay(NT),STAT=STATUS); VERIFY_(STATUS)
!    allocate(sed_sum(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(co2sc(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(ccovm(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(cldtcm(NT),STAT=STATUS);VERIFY_(STATUS)
    allocate(rlwpm(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(cdrem(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(rh(NT),STAT=STATUS);    VERIFY_(STATUS)
    allocate(oz(NT),STAT=STATUS);    VERIFY_(STATUS)
    allocate(wv(NT),STAT=STATUS);    VERIFY_(STATUS)
    allocate(taua(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(asymp(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(ssalb(NT),STAT=STATUS); VERIFY_(STATUS)

    allocate(FRZMLT(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(SHF(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(LWUP(NT),STAT=STATUS); VERIFY_(STATUS)
    allocate(EVP(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(TXI(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(TYI(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(PUR(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(PUF(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(PPR(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(PPF(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(LHF(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(SLR(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ZTH(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FSWABS(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FSURF(NT), STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBVRN(NT, NUM_SUBTILES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBVFN(NT, NUM_SUBTILES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBNRN(NT, NUM_SUBTILES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBNFN(NT, NUM_SUBTILES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FCOND(NT, NUM_iCE_CATEGORIES),   STAT=STATUS);  VERIFY_(STATUS)
    allocate(FCONDBOT(NT, NUM_iCE_CATEGORIES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(LHCOEFF(NT, NUM_iCE_CATEGORIES), STAT=STATUS);  VERIFY_(STATUS)
    allocate(SHCOEFF(NT, NUM_iCE_CATEGORIES), STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBVRI(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBVFI(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBNRI(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(ALBNFI(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FRCICE(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(TF(NT),    STAT=STATUS);  VERIFY_(STATUS)
    allocate(TBOT(NT),  STAT=STATUS);  VERIFY_(STATUS)
    allocate(FBOT(NT),  STAT=STATUS);  VERIFY_(STATUS)
    !allocate(SLMASK(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(RSIDE(NT), STAT=STATUS);  VERIFY_(STATUS)
    allocate(FRESHL(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FRESHN(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FSALTL(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FSALTN(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FHOCNL(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FHOCNN(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FRAZLN(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(MELTLN(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(MELTT(NT), STAT=STATUS);  VERIFY_(STATUS)
    allocate(MELTB(NT), STAT=STATUS);  VERIFY_(STATUS)
    allocate(MELTS(NT), STAT=STATUS);  VERIFY_(STATUS)
    allocate(SNOICE(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(CONGEL(NT),STAT=STATUS);  VERIFY_(STATUS)
    allocate(DTS(NT),   STAT=STATUS);VERIFY_(STATUS)
    allocate(DTSACCUM(NT),STAT=STATUS);VERIFY_(STATUS)
    allocate(AICENINIT(NT, NUM_ICE_CATEGORIES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(VICENINIT(NT, NUM_ICE_CATEGORIES),STAT=STATUS);  VERIFY_(STATUS)
    allocate(FSWTHRU(NT, NUM_SUBTILES),   STAT=STATUS);  VERIFY_(STATUS)
    allocate(FSWTHRUWTR(NT, NUM_SUBTILES),STAT=STATUS);  VERIFY_(STATUS)


! Get the time step
! -----------------

    call MAPL_GetResource ( STATE, DT, Label="RUN_DT:"        , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, DT, Label="DT:", DEFAULT=DT, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, LATSO, Label="LATSO:", DEFAULT=70.0, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, LONSO, Label="LONSO:", DEFAULT=70.0, RC=STATUS)
    VERIFY_(STATUS)

#ifdef USE_R8
    DTDB = DT
#endif


! Get current time from clock
!----------------------------
    call ESMF_ClockGet(CLOCK, currTime=CurrentTime, TIMESTEP=DELT, RC=STATUS)
    VERIFY_(STATUS)

! Do extra allocation if gridded exports are requested
!-----------------------------------------------------
    call MK_GRID_OUT(EXPORT, GNAME='SWNg', TNAME='SWN', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='LWNg', TNAME='LWN', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='SHFg', TNAME='SHF', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='LHFg', TNAME='LHF', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='EVAPg', TNAME='EVAP', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='PRECIPg', TNAME='PRECIP', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='SMELTg', TNAME='SMELT', RC=STATUS)
    call MK_GRID_OUT(EXPORT, GNAME='PSg', TNAME='PS', RC=STATUS)

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
                      TNAME='TAUA_'//suffix, RC=STATUS)
     call MK_GRID_OUT(EXPORT, GNAME='ASYMP_'//suffix//'g', &
                      TNAME='ASYMP_'//suffix, RC=STATUS)
     call MK_GRID_OUT(EXPORT, GNAME='SSALB_'//suffix//'g', &
                      TNAME='SSALB_'//suffix, RC=STATUS)
    enddo

! Pointers to Imports
!--------------------

    call GET_POINTER(IMPORT,KPAR  , 'KPAR'    ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(IMPORT,UW    , 'UW'      ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(IMPORT,VW    , 'VW'      ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(IMPORT,UI    , 'UI'      ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(IMPORT,VI    , 'VI'      ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,TAUXBOT, 'TAUXBOT',    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(IMPORT,TAUYBOT, 'TAUYBOT',    RC=STATUS); VERIFY_(STATUS)

! Pointers to Internals
!----------------------

    call MAPL_GetResource ( STATE, DO_DATAATM,     Label="USE_DATAATM:" ,     DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, DO_CICE_THERMO, Label="USE_CICE_Thermo:" , DEFAULT=0, RC=STATUS); VERIFY_(STATUS)

    call GET_POINTER(INTERNAL, TW ,   'TSKINW',    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(INTERNAL, HW ,   'HSKINW',    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(INTERNAL, SW ,   'SSKINW',    RC=STATUS); VERIFY_(STATUS)
   if ( DO_CICE_THERMO == 0) then
      call MAPL_GetPointer(INTERNAL,TI  ,'TSKINI',    RC=STATUS); VERIFY_(STATUS)
   else
      call MAPL_GetPointer(INTERNAL,TI8 ,'TSKINI' ,   RC=STATUS); VERIFY_(STATUS)
   endif
    call GET_POINTER(INTERNAL, HI ,   'HSKINI',    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(INTERNAL, SI ,   'SSKINI',    RC=STATUS); VERIFY_(STATUS)

   !call MAPL_GetPointer(INTERNAL,TS     ,'TSKIN' ,    RC=STATUS); VERIFY_(STATUS)
   !call MAPL_GetPointer(INTERNAL,TSC    ,'TSKINC',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,FR     ,'FR'    ,    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,VOLICE ,'VOLICE',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,VOLSNO ,'VOLSNO',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,VOLPOND,'VOLPOND',   RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,APONDN, 'APONDN',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,HPONDN, 'HPONDN',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,ERGICE ,'ERGICE',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,ERGSNO ,'ERGSNO',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,TAUAGE ,'TAUAGE',    RC=STATUS); VERIFY_(STATUS)
   call MAPL_GetPointer(INTERNAL,SLMASK ,'SLMASK',    RC=STATUS); VERIFY_(STATUS)
!  Pointers to Exports
!---------------------

    call GET_POINTER(EXPORT, u10m, 'U10M'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, v10m, 'V10M'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, TAUXW, 'TAUXW'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, TAUYW, 'TAUYW'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, TAUXI, 'TAUXI'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, TAUYI, 'TAUYI'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, TAUXO, 'TAUXO'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, TAUYO, 'TAUYO'   ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, USTR3, 'OUSTAR3' ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, UU,    'UU'      ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, PSEX , 'PS'      ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, PRUVF, 'PENUVF'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, PRPAF, 'PENPAF'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, PRUVR, 'PENUVR'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, PRPAR, 'PENPAR'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, swnx, 'SWN'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, lwnx, 'LWN'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, shfx, 'SHF'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, lhfx, 'LHF'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, evapx, 'EVAP'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, precipx, 'PRECIP'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, smeltx, 'SMELT'  ,    RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, FSURFL, 'FSURF'  ,    RC=STATUS); VERIFY_(STATUS)

    call GET_POINTER(EXPORT, dry_clayx, 'DUDP', RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, wet_clayx, 'DUWT', RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, sed_clayx, 'DUSD', RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, ccovmx,  'CCOVM',  RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, cldtcmx, 'CLDTCM', RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, rlwpmx,  'RLWPM',  RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, cdremx,  'CDREM',  RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, rhx,     'RH',     RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, ozx,     'OZ',     RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, wvx,     'WV',     RC=STATUS); VERIFY_(STATUS)
    call GET_POINTER(EXPORT, co2scx, 'CO2SC', RC=STATUS); VERIFY_(STATUS)
    do k=1, 33
     write(unit = suffix, fmt = '(i2.2)') k
     call GET_POINTER(EXPORT, tauax, 'TAUA_'//suffix, RC=STATUS)
     atauax(k)%b => tauax
     VERIFY_(STATUS)

     call GET_POINTER(EXPORT, asympx, 'ASYMP_'//suffix, RC=STATUS)
     aasympx(k)%b => asympx
     VERIFY_(STATUS)

     call GET_POINTER(EXPORT, ssalbx, 'SSALB_'//suffix, RC=STATUS)
     assalbx(k)%b => ssalbx
     VERIFY_(STATUS)
    enddo

    call GET_POINTER(EXPORT, DISCHARGE, 'DISCHARGE', RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,FRAZIL , 'FRAZIL'  ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,CONGELO, 'CONGEL'  ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,SNOICEO, 'SNOICE'  ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,FRESH  , 'FRESH'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,FSALT  , 'FSALT'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,FHOCN  , 'FHOCN'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,FBOTL  , 'FBOT'    ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,DAIDTT , 'DAIDTT'  ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,DVIDTT , 'DVIDTT'  ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,MELTL  , 'MELTL'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,MELTTL , 'MELTT'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,MELTBL , 'MELTB'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,MELTSL , 'MELTS'   ,    RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,TWINC1 , 'TSKINWinc1' , RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,TWINC2 , 'TSKINWinc2' , RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,TWINC3 , 'TSKINWinc3' , RC=STATUS); VERIFY_(STATUS)
    call MAPL_GetPointer(EXPORT,TWINCT , 'TSKINWinctotal' , RC=STATUS); VERIFY_(STATUS)

! Read 10m temperature (K)
!---------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "T10_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; t10 = 290.0; 
    else; call MAPL_ReadForcing(state, "T10", renamefile(datafile, time = currenttime), currenttime, t10, rc = status);
        VERIFY_(status);
    endif; 

! Read 10m specific humidity (kg kg-1)
!---------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "Q10_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; q10 = 2.0e-6; 
    else; call MAPL_ReadForcing(state, "Q10", renamefile(datafile, time = currenttime), currenttime, q10, rc = status);
        VERIFY_(status);
    endif; 

! Read 10m zonal wind speed (m s-1)
!---------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "U10_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; u10 = 0.0; 
    else; call MAPL_ReadForcing(state, "U10", renamefile(datafile, time = currenttime), currenttime, u10, rc = status);
        VERIFY_(status);
    endif; 
    u10m = merge(tsource = u10, fsource = 0.0, mask = (abs(u10) < 1000.0)); 
! Read 10m meridional wind speed (m s-1)
!---------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "V10_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; v10 = 0.0; 
    else; call MAPL_ReadForcing(state, "V10", renamefile(datafile, time = currenttime), currenttime, v10, rc = status);
        VERIFY_(status);
    endif; 
    v10m = merge(tsource = v10, fsource = 0.0, mask = (abs(v10) < 1000.0)); 

! Read sea level pressure (Pa)
!---------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "SLP_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; slp = 90000.0; 
    else; call MAPL_ReadForcing(state, "SLP", renamefile(datafile, time = currenttime), currenttime, slp, rc = status);
        VERIFY_(status);
    endif; 

! Read sea surface salinity (psu)
!---------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "SSS_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; sss = 35.0; 
    else; call MAPL_ReadForcing(state, "SSS", renamefile(datafile, time = currenttime), currenttime, sss, rc = status);
        VERIFY_(status);
    endif; 

! Read surface downward fresh water flux from river runoff (kg m-2 s-1)
!-------------------------------------

    call MAPL_GetResource(state, datafile, label = "RR_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; rr = 0.0; 
    else; call MAPL_ReadForcing(state, "RR", renamefile(datafile, time = currenttime), currenttime, rr, rc = status);
        VERIFY_(status);
    endif; 
    if(associated(DISCHARGE)) DISCHARGE = rr 

! Read sufrace downward fresh water flux from rain (mm s-1)
!-----------------------------------

    call MAPL_GetResource(state, datafile, label = "RAIN_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; rain = 0.0; 
    else; call MAPL_ReadForcing(state, "RAIN", renamefile(datafile, time = currenttime), currenttime, rain, rc = status);
        VERIFY_(status);
    endif; 

! Read surface downward fresh water flux from snow (mm s-1)
!-------------------------------------

    call MAPL_GetResource(state, datafile, label = "SNOW_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; snow = 0.0; 
    else; call MAPL_ReadForcing(state, "SNOW", renamefile(datafile, time = currenttime), currenttime, snow, rc = status);
        VERIFY_(status);
    endif; 

! Read downward long wave flux at ocean surface (W m-2)
!-----------------------------------------------------

    call MAPL_GetResource(state, datafile, label = "LWRAD_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; lwrad = -100.0; 
    else; call MAPL_ReadForcing(state, "LWRAD", renamefile(datafile, time = currenttime), currenttime, lwrad, rc = status);
        VERIFY_(status);
    endif; 

! Read downward short wave flux at the surface (W m-2)
!-----------------------------------------------

    call MAPL_GetResource(state, datafile, label = "SWRAD_FILE:", default = "none", rc = status);
    VERIFY_(status);
    if(trim(datafile) == 'none') then; swrad = 200.0; 
    else; call MAPL_ReadForcing(state, "SWRAD", renamefile(datafile, time = currenttime), currenttime, swrad, rc = status);
        VERIFY_(status);
    endif; 

! Read Clay-Sized Dry Atmospheric Dust Depositions
!-------------------------------------------------
    do K = 1, NUM_DUDP
       write(label,'(I3.3)') K
       call MAPL_GetResource( STATE, DATAfile, LABEL='DUDP'//label//'_FILE:', default = 'none', RC=STATUS )
       VERIFY_(STATUS)
       if(trim(datafile) == 'none') then; dry_clay = 0.0
       else
          call MAPl_ReadForcing( STATE, 'DUDP'//label, DATAFILE, CURRENTTIME, dry_clay, RC=STATUS )
          VERIFY_(STATUS)
       endif
       if (associated(dry_clayx)) dry_clayx(:,K) = dry_clay
    end do

! Read Clay-Sized Wet Atmospheric Dust Depositions
!-------------------------------------------------
    do K = 1, NUM_DUWT
       write(label,'(I3.3)') K
       call MAPL_GetResource( STATE, DATAfile, LABEL='DUWT'//label//'_FILE:', default = 'none', RC=STATUS )
       VERIFY_(STATUS)
       if(trim(datafile) == 'none') then; wet_clay = 0.0
       else
          call MAPl_ReadForcing( STATE, 'DUWT'//label, DATAFILE, CURRENTTIME, wet_clay, RC=STATUS )
          VERIFY_(STATUS)
       endif
       if (associated(wet_clayx)) wet_clayx(:,K) = wet_clay
    end do

! Read Clay-Sized Sedimentary Atmospheric Dust Depositions
!---------------------------------------------------------
    do K = 1, NUM_DUSD
       write(label,'(I3.3)') K
       call MAPL_GetResource( STATE, DATAfile, LABEL='DUSD'//label//'_FILE:', default = 'none', RC=STATUS )
       VERIFY_(STATUS)
       if(trim(datafile) == 'none') then; sed_clay = 0.0
       else
          call MAPl_ReadForcing( STATE, 'DUSD'//label, DATAFILE, CURRENTTIME, sed_clay, RC=STATUS )
          VERIFY_(STATUS)
       endif
       if (associated(sed_clayx)) sed_clayx(:,K) = sed_clay
    end do

! Read Atmospheric Clouds (Atmospheric Optics)
!---------------------------------------------
    call MAPL_GetResource( STATE, DATAfile, LABEL='CCOVM_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; ccovm = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'CCOVM', DATAFILE, CURRENTTIME, ccovm, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( STATE, DATAfile, LABEL='CLDTCM_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; cldtcm = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'CLDTCM', DATAFILE, CURRENTTIME, cldtcm, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( STATE, DATAfile, LABEL='RLWPM_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; rlwpm = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'RLWPM', DATAFILE, CURRENTTIME, rlwpm, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( STATE, DATAfile, LABEL='CDREM_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; cdrem = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'CDREM', DATAFILE, CURRENTTIME, cdrem, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

! Read Atmospheric Properties (Atmospheric Optics)
!-------------------------------------------------
    call MAPL_GetResource( STATE, DATAfile, LABEL='RH_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; rh = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'RH', DATAFILE, CURRENTTIME, rh, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( STATE, DATAfile, LABEL='OZ_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; oz = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'OZ', DATAFILE, CURRENTTIME, oz, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

    call MAPL_GetResource( STATE, DATAfile, LABEL='WV_FILE:', default = 'none', RC=STATUS )
    VERIFY_(STATUS)
    if(trim(datafile) == 'none') then; wv = 0.0; 
    else; call MAPl_ReadForcing( STATE, 'WV', DATAFILE, CURRENTTIME, wv, RC=STATUS )
        VERIFY_(STATUS)
    endif; 

! Read Atmospheric Carbon Dioxide from Carbon Tracker (_2011_OI)
!-----------------------------------------------------
    call MAPL_GetResource( STATE, DATAfile, LABEL='CO2SC_FILE:', RC=STATUS )
    VERIFY_(STATUS)
    call MAPl_ReadForcing( STATE, 'CO2SC', DATAFILE, CURRENTTIME, co2sc,  RC=STATUS )
    VERIFY_(STATUS)
    if ( associated(co2scx) ) co2scx = co2sc


! Read MODIS Aerosols (Atmospheric Optics)
!-----------------------------------------

    do k=1, 33
     write(unit = suffix, fmt = '(i2.2)') k
     call MAPL_GetResource( STATE, DATAfile, LABEL='TAUA_FILE:', default = 'none', RC=STATUS )
     VERIFY_(STATUS)
     if(trim(datafile) == 'none') then; taua = 0.0
     else; call MAPL_ReadForcing( STATE, 'TAUA_' // suffix, trim(DATAFILE) // suffix, CURRENTTIME, taua, RC=STATUS)
         VERIFY_(STATUS)
     endif;
     ataua(k)%b => taua

     call MAPL_GetResource( STATE, DATAfile, LABEL='ASYMP_FILE:', default = 'none', RC=STATUS )
     VERIFY_(STATUS)
     if(trim(datafile) == 'none') then; asymp = 0.0
     else; call MAPL_ReadForcing( STATE, 'ASYMP_' // suffix, trim(DATAFILE) // suffix, CURRENTTIME, asymp, RC=STATUS)
         VERIFY_(STATUS)
     endif;  
     aasymp(k)%b => asymp

     call MAPL_GetResource( STATE, DATAfile, LABEL='SSALB_FILE:', default = 'none', RC=STATUS )
     VERIFY_(STATUS)
     if(trim(datafile) == 'none') then; ssalb = 0.0
     else; call MAPL_ReadForcing( STATE, 'SSALB_' // suffix, trim(DATAFILE) // suffix, CURRENTTIME, ssalb, RC=STATUS)
         VERIFY_(STATUS)
     endif;
     assalb(k)%b => ssalb
    enddo


! Get the relaxation times for SST and SSS from the configuration
!----------------------------------------------------------------

    call MAPL_GetResource(STATE,TAUSST,LABEL="SST_RELAXTIME:", DEFAULT=1. ,RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource(STATE,TAUSSS,LABEL="SSS_RELAXTIME:", DEFAULT=1. ,RC=STATUS)
    VERIFY_(STATUS)

! Get parameters

    call MAPL_GetResource ( STATE, MAXICEDEPTH  , Label="MAX_SEAICE_DEPTH:", DEFAULT=2.0  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, MINICEDEPTH  , Label="MIN_SEAICE_DEPTH:", DEFAULT=1.E-6, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, MAXWATERDEPTH, Label="MAX_WATER_DEPTH:" , DEFAULT=20.0 , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, MINWATERDEPTH, Label="MIN_WATER_DEPTH:" , DEFAULT=0.5  , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, MAXSALINITY, Label="MAX_SALINITY:" , DEFAULT=35.00 , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, MINSALINITY, Label="MIN_SALINITY:" , DEFAULT=33.33 , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, EMSICE,      Label="CICE_EMSICE:"  , DEFAULT=0.99999, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, SHORTWAVE, Label="CICE_SHORTWAVE:" , DEFAULT="shortwave_ccsm" , RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, DO_POND, Label="CICE_DO_POND:" , DEFAULT=0, RC=STATUS)
    VERIFY_(STATUS)
    if (DO_POND == 1) then
       TR_POND = .true.
    else
       TR_POND = .false.
    endif 
    ! CICE uses Tf = -depressT * SSS
    ! where depressT = 0.054 C/psu
    Tf = -depressT * SW  ! default option in CICE 

    TRCRTYPE(nt_tsfc)  = 0  ! ice/snow surface temperature
    TRCRTYPE(nt_iage)  = 1  ! volume-weighted ice age
    TRCRTYPE(nt_volpn) = 0  ! melt pond volume
#ifdef USE_R8
    ! need to initialize some local arrays and scalars
    TBOT     = 0.0
    FBOT     = 0.0
    ALBVRN   = 0.0
    ALBNRN   = 0.0
    ALBVFN   = 0.0
    ALBNFN   = 0.0
    FSWSFC   = 0.0  
    FSWINT   = 0.0
    ISWABS   = 0.0
    ALBIN    = 0.0
    ALBSN    = 0.0
    SSWABS   = 0.0
    FSWABS   = 0.0
    MELTT    = 0.0
    MELTS    = 0.0
    MELTB    = 0.0
    CONGEL   = 0.0 
    SNOICE   = 0.0
#endif
    ! determine those tiles where there is no open ocean connection
    where(abs(UW) >  0.0 .or. abs(VW) > 0.0)
        SLMASK = 0.0
    elsewhere
        SLMASK = 1.0
    endwhere

    if(associated(FSURFL )) FSURFL  = 0.0
    if(associated(SHOUT  )) SHOUT   = 0.0
    if(associated(SHICE  )) SHICE   = 0.0
    if(associated(HLATN  )) HLATN   = 0.0
    if(associated(MELTTL )) MELTTL  = 0.0
    if(associated(MELTBL )) MELTBL  = 0.0
    if(associated(MELTSL )) MELTSL  = 0.0
    if(associated(CONGELO)) CONGELO = 0.0
    if(associated(SNOICEO)) SNOICEO = 0.0
    if(associated(FBOTL  )) FBOTL   = 0.0
    if(associated(TAUXI))   TAUXI = 0.0 
    if(associated(TAUYI))   TAUYI = 0.0 
    if(associated(PRUVF))   PRUVF = 0.0 
    if(associated(PRPAF))   PRPAF = 0.0
    if(associated(PRUVR))   PRUVR = 0.0 
    if(associated(PRPAR))   PRPAR = 0.0

    FSURF  = 0.0
    EVP    = 0.0
    SHF    = 0.0
    LWUP   = 0.0
    LHF    = 0.0
    MELTLN = 0.0
    FRAZLN = 0.0
    FRESHL = 0.0
    FRESHN = 0.0
    FHOCNN = 0.0
    FHOCNL = 0.0
    RSIDE  = 0.0
    TRACERS = 0.0
    FSALTL = 0.0
    FSALTN = 0.0
    FSWTHRU = 0.0
    FSWTHRUWTR = 0.0
    FCOND   = 0.0
    FCONDBOT= 0.0

    MAXWATERDEPTH   = MAXWATERDEPTH*MAPL_RHOWTR
    MINWATERDEPTH   = MINWATERDEPTH*MAPL_RHOWTR
    MAXICEDEPTH     = MAXICEDEPTH  *MAPL_RHOWTR
    MINICEDEPTH     = MINICEDEPTH  *MAPL_RHOWTR

    ! do a cleanup here in case transformation from tripolar
    ! to tile induces round-off errors
    FRCICE = sum(FR(:,ICE:), dim=2)
    do k=1, NT
       
       LATSD = TILELATS(K) *  rad_to_deg
       LONSD = TILELONS(K) *  rad_to_deg
       OBSERVE(1) = abs(LATSD(1)-LATSO) < 1.e-3 .and. abs(LONSD(1)-LONSO) < 1.e-3
       !TRACERS(nt_tsfc,:) = TSC(K,ICE:)
       TRACERS(nt_tsfc,:) = TI8(K,:) - MAPL_TICE
       TRACERS(nt_iage,:) = TAUAGE(K,:)
       TRACERS(nt_volpn,:)= VOLPOND(K,:)
#ifdef USE_R8
       TRACERSDB2  = TRACERS
       FRWATERDB   =  FR(K,WATER)
       FHOCNLDB    =  FHOCNL(K)
       FRESHLDB    =  FRESHL(K)
       FSALTLDB    =  FSALTL(K)
       FRCICEDB    =  FRCICE(K)  
       call cleanup_itd (1,1,1,1,1,1,DTDB, &
            FR(K,ICE:),    TRACERSDB2,  &
            VOLICE(K,:),   VOLSNO(K,:),   &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FRWATERDB,  FRCICEDB,            &
            TRCRTYPE,                        &
            FRESHLDB,     FSALTLDB,          &
            FHOCNLDB,                        &
            .true.,        L_STOP,           &
            IDUM,            JDUM,           & 
            limit_aice_in=.true.)
        FR(K,WATER) =  FRWATERDB(1)    
#else
       call cleanup_itd (1,1,1,1,1,1,DT, &
            FR(K,ICE:),   TRACERS(:,:),  &
            VOLICE(K,:),  VOLSNO(K,:),   &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FR(K,WATER),  FRCICE(K),           &
            TRCRTYPE,                          &
            FRESHL(K),     FSALTL(K),          &
            FHOCNL(K),                         &
            .true.,        L_STOP,             &
            IDUM,            JDUM)
#endif       
       ASSERT_(.not.L_STOP)

#ifdef USE_R8
       TRACERS       = TRACERSDB2
       FRESHL(K)     = FRESHLDB(1)         
       FSALTL(K)     = FSALTLDB(1)         
       FHOCNL(K)     = FHOCNLDB(1)        
#endif
       TI8(K,:)     =   TRACERS(nt_tsfc,:) + MAPL_TICE 
       TAUAGE(K,:)  =   TRACERS(nt_iage,:) 
       VOLPOND(K,:) =   TRACERS(nt_volpn,:)
    enddo

    ! freshwater accumulated previously is not counted
    FRESHL = 0.0

    !*** FR(:,ICE:) returned from CICEDyna
    !*** update FRWATER accordingly 
    FRCICE = sum(FR(:,ICE:), dim=2)
    FR(:,WATER) = max(1.0-FRCICE, 0.0)

    if(associated(DAIDTT)) then
      allocate(FR_OLD(NT))
      FR_OLD = FRCICE 
    endif
    if(associated(DVIDTT)) then
      allocate(VOLICE_OLD(NT))
      VOLICE_OLD = sum(VOLICE,dim=2)
    endif

    !TSC is returned from Dyna 
    !TSC(:,WATER) = TW  - Tffresh     
    !TS           = TSC + Tffresh

    AICENINIT = FR(:,ICE:)
    VICENINIT = VOLICE


    !*** compute oceanic heat potential here
    do k=1, NT
       LATSD = TILELATS(K) *  rad_to_deg
       LONSD = TILELONS(K) *  rad_to_deg
       OBSERVE(1) = abs(LATSD(1)-LATSO) < 1.e-3 .and. abs(LONSD(1)-LONSO) < 1.e-3

       FRZMLT(K) = (TF(K)-(TW(K)-MAPL_TICE))*SALTWATERCAP*HW(K)/DT
       FRZMLT(K) = min(max(FRZMLT(K),-FRZMLT_MAX),FRZMLT_MAX)
       if((TW(K)-MAPL_TICE) < TF(K)) then
          TW(K) = TF(K) + MAPL_TICE
       endif

       if(FRZMLT(K)<0.0) then ! heat the already existing ice from below
          FRZMLTDB  = FRZMLT(K)
          TSCDB     = TW(K) - MAPL_TICE 
          TFDB      = TF(K) 
          TAUXBOTDB = TAUXBOT(K)
          TAUYBOTDB = TAUYBOT(K)
          TBOTDB    = TBOT(K) 
          FBOTDB    = FBOT(K) 
          RSIDEDB   = RSIDE(K) 
          FRCICEDB  = FRCICE(K)
          call frzmlt_bottom_lateral (1,1,1,1,1,1,DTDB, &
                        OBSERVE,                                 &
                        FRCICEDB,         FRZMLTDB,    & ! in
                        ERGICE(K,:,:),    ERGSNO(K,:,:),  &
                        TSCDB,            TFDB,        & ! IN
                        TAUXBOTDB,        TAUYBOTDB,   & ! IN
                        TBOTDB, FBOTDB,   RSIDEDB      ) ! out
          TBOT(K)       =  TBOTDB(1)    
          FBOT(K)       =  FBOTDB(1)    
          RSIDE(K)      =  RSIDEDB(1)   
       else
           TBOT(K)  = TF(K)
           FBOT(K)  = 0.0
           RSIDE(K) = 0.0
       endif
    enddo

! Dust depositions
!-----------------

!    if ( associated(dry_clayx) ) dry_clayx = dry_clay
!    if ( associated(dry_sumx)  ) dry_sumx  = dry_sum
!    if ( associated(wet_clayx) ) wet_clayx = wet_clay
!    if ( associated(wet_sumx)  ) wet_sumx  = wet_sum
!    if ( associated(sed_clayx) ) sed_clayx = sed_clay
!    if ( associated(sed_sumx)  ) sed_sumx  = sed_sum

    if ( associated(ccovmx)  ) ccovmx  = ccovm
    if ( associated(cldtcmx) ) cldtcmx = cldtcm
    if ( associated(rlwpmx)  ) rlwpmx  = rlwpm
    if ( associated(cdremx)  ) cdremx  = cdrem
    if ( associated(rhx)     ) rhx     = rh
    if ( associated(ozx)     ) ozx     = oz
    if ( associated(wvx)     ) wvx     = wv
    do k=1, 33
     if ( associated(atauax(k)%b)  ) atauax(k)%b  = ataua(k)%b
     if ( associated(aasympx(k)%b) ) aasympx(k)%b = aasymp(k)%b
     if ( associated(assalbx(k)%b) ) assalbx(k)%b = assalb(k)%b
    enddo

! Set sea level pressure
!-------------------------------

    if(associated(PSEX)) PSEX = slp

! Total precipitation

    if(associated(precipx)) precipx=rain+(1-FRCICE)*snow

! Solar at base of skin layer ignores salt in computing skin layer depth
!  NIR is all absorbed.
!-----------------------------------------------------------------------

    VAR1 = (HW/MAPL_RHOWTR)
    var2 = (1-alb)*swrad*FRPAR*exp(-KPAR*VAR1)
    var3 = (1-alb)*swrad*FRUVR*EXP(-KUVR*VAR1)
    pen = var2+var3

! For data atmosphere we assume it is all diffuse
!------------------------------------------------
    if(associated(PRUVF)) PRUVF = PRUVF + var2 * FR(:,WATER) 
    if(associated(PRPAF)) PRPAF = PRPAF + var3 * FR(:,WATER)
    if(associated(PRUVR)) PRUVR = 0.0 
    if(associated(PRPAR)) PRPAR = 0.0

! Air density


    var1 = 287.04*t10*(1.+0.608*q10)
    rhoa = slp/var1

! Specific hunidity of saturated air at SST,

    var1 = 0.98*640380.0*exp(-5107.4/TW)
    qsat = var1/rhoa                     
                                         
! Calculate exchange coefs

    var1 = 10.0 ! z of atmospheric fields, needed by ncar_ocean_fluxes
    var2 = 1.0  ! needed by ncar_ocean_fluxes
    var3=0.0    ! needed by ncar_ocean_fluxes

    call ncar_ocean_fluxes(sqrt((u10-uw)**2+(v10-vw)**2),&
         & t10, tw, q10, qsat, var1, var2==1.0,&
         & cd, ch, ce, ustar, var3)
    

! Set zonal wind stress 
 
    cd=sqrt(cd)
    var1 = rhoa*ustar*cd*(u10-uw)
    if(associated(TAUXW)) TAUXW = VAR1
    
! Set meridional wind stress
    
    var1 = rhoa*ustar*cd*(v10-vw)
    if(associated(TAUYW)) TAUYW = VAR1

    do N=ICE,NUM_SUBTILES

        Nsub = n - ice + 1

        do k=1, NT
          !TSCDB      =  TSC(K,N) 
          TSCDB      =  TI8(K,Nsub) - MAPL_TICE 
          potTDB     =  t10(K)
          u10DB      =  u10(K)
          v10DB      =  v10(K)
          windDB     =  sqrt(u10DB*u10DB+v10DB*v10DB)
          zlvlDB     =  10.0
          QaDB       =  q10(K)
          rhoaDB     =  rhoa(K)
          call atmo_boundary_layer (1,1,'ice',1,          &
                                    (/1/),(/1/),          &
                                    TSCDB,  potTDB,       &    
                                    u10DB,  v10DB,        &
                                    windDB, zlvlDB,       &
                                    QaDB,   rhoaDB,       &
                                    TXIDB,  TYIDB,        &
                                    TrefDB, QrefDB,       &    
                                    deltDB, delqDB,       &    
                                    lhcoeffDB, shcoeffDB) 
          TXI(K) = TXIDB(1)    
          TYI(K) = TYIDB(1)    
          LHCOEFF(K,Nsub) = lhcoeffDB(1)
          SHCOEFF(K,Nsub) = shcoeffDB(1)
        enddo
        if(associated(TAUXI)) TAUXI = TAUXI + TXI * FR(:,N)
        if(associated(TAUYI)) TAUYI = TAUYI + TYI * FR(:,N)
    enddo  
    if(associated(TAUXI)) then
       where(FRCICE > 0.0) 
          TAUXI = TAUXI / FRCICE
       endwhere
    endif
    if(associated(TAUYI)) then
       where(FRCICE > 0.0) 
          TAUYI = TAUYI / FRCICE
       endwhere
    endif
    if(associated(TAUXO)) TAUXO = TAUXW * FR(:,WATER) + TAUXI * FRCICE
    if(associated(TAUYO)) TAUYO = TAUYW * FR(:,WATER) + TAUYI * FRCICE


    if(associated(USTR3)) USTR3 = ustar**3
    if(associated(UU))    UU    = sqrt(u10*u10 + v10*v10)

! Update skin values
!-------------------

! Absolute mass of salt and fresh water in kg / m-2

    SW  = .001*SW *HW
    SI  = .001*SI *HI

    HW  = HW - SW
    HI  = HI - SI

! Update values over water

    cd=1.0/cd
    var1=rhoa*ustar*ce*cd*(q10-qsat) ! Mass flux due to evaporation, 
                                     ! positive down
    if(associated(evapx)) evapx=(1-FRCICE)*var1

    HW  = HW + DT*(rain+rr+(1-FRCICE)*(snow+var1))
    VAR2 = max(min(HW,MAXWATERDEPTH),MINWATERDEPTH)-HW
    HW  = HW + VAR2

! Add salt ( Relaxation to Levitus )

    SSS = .001*SSS
    SW  = (SW + (DT/TAUSSS)*SSS*HW) / (1.0 + (DT/TAUSSS)*(1.-SSS))

!    if(associated(SALT)) SALT = (SSS*(HW+SW)-SW)*(1./TAUSSS)


! Update temperature.
! 
    var1 = MAPL_ALHL*var1 ! Latent heat flux, positive down
    if(associated(lhfx)) lhfx=(1-FRCICE)*var1
    if(associated(FSURFL)) FSURFL=FSURFL+(1-FRCICE)*var1

    var2 = (1-alb)*swrad-pen ! Absobed SW rad, positive down
    var1 = var1+var2
    if(associated(swnx)) swnx=(1-FRCICE)*var2
    if(associated(FSURFL)) FSURFL=FSURFL+(1-FRCICE)*var2

    var2 = lwrad - MAPL_STFBOL*(TW)**4 ! Net LW rad, positive down
    var1 = var1+var2
    if(associated(lwnx)) lwnx=(1-FRCICE)*var2
    if(associated(FSURFL)) FSURFL=FSURFL+(1-FRCICE)*var2

    var2 = -MAPL_ALHF*snow ! Latent heat of snow melt
    var1 = var1+var2
    if(associated(smeltx)) smeltx=(1-FRCICE)*var2
    if(associated(FSURFL)) FSURFL=FSURFL+(1-FRCICE)*var2

    var2 = rhoa*MAPL_CP*ustar*ch*cd*(t10-TW) ! Sensible heat flux, 
                                             ! positive down
    var1 = var1+var2           
    if(associated(shfx)) shfx=(1-FRCICE)*var2
    if(associated(FSURFL)) FSURFL=FSURFL+(1-FRCICE)*var2

    VAR2 = MAPL_CAPWTR*HW ! Assume heat capacity of salt = 0.0

    TW  = TW + (1-FRCICE)*DT*(var1/VAR2)
    DTS =  (1-FRCICE)*DT*(var1/VAR2) 
    DTSACCUM = 0.0
    DTSACCUM = DTSACCUM+DTS
    if(associated(TWINC1  )) TWINC1   = DTS 

! Update values over ice.

    HI  = HI
    HI  = max(min(HI,  MAXICEDEPTH),  MINICEDEPTH)

    SI=SI
    
    !TI=TI

! Reset mass variables

    HI  = HI + SI  ! BACK TO TOTAL MASS
    HW  = HW + SW

    SW  = 1000.*(SW/HW) ! BACK TO PSU
    SI  = 1000.*(SI/HI)

    ! output gridded vars
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='SWNg', TNAME='SWN', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='LWNg', TNAME='LWN', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='SHFg', TNAME='SHF', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='LHFg', TNAME='LHF', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='EVAPg', TNAME='EVAP', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='PRECIPg', TNAME='PRECIP', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='SMELTg', TNAME='SMELT', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='PSg', TNAME='PS', RC=STATUS)

    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUDP_CLAYg', TNAME='DUDP_CLAY', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUDP_SUMg',  TNAME='DUDP_SUM', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUWT_CLAYg', TNAME='DUWT_CLAY', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUWT_SUMg',  TNAME='DUWT_SUM', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUSD_CLAYg', TNAME='DUSD_CLAY', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='DUSD_SUMg',  TNAME='DUSD_SUM', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='CCOVMg',  TNAME='CCOVM',  RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='CLDTCMg', TNAME='CLDTCM', RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='RLWPMg',  TNAME='RLWPM',  RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='CDREMg',  TNAME='CDREM',  RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='RHg',     TNAME='RH',     RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='OZg',     TNAME='OZ',     RC=STATUS)
    call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='WVg',     TNAME='WV',     RC=STATUS)
    do k=1, 33
     write(unit = suffix, fmt = '(i2.2)') k
     call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='TAUA_'//suffix//'g', &
                     TNAME='TAUA_'//suffix,  RC=STATUS)
     call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='ASYMP_'//suffix//'g',&
                     TNAME='ASYMP_'//suffix, RC=STATUS)
     call T2G_Regrid(EXPORT, LOCSTREAM, GNAME='SSALB_'//suffix//'g',&
                     TNAME='SSALB_'//suffix, RC=STATUS)
    enddo



    !TS(:,WATER)  = TW
    !TSC(:,WATER) = TW - Tffresh
    call ESMF_VMGetCurrent(VM,                                RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_VMGet(VM, localPet=mype, rc=status)
    VERIFY_(STATUS)

    call MAPL_SunGetInsolation(TILELONS, TILELATS,      &
         ORBIT, ZTH, SLR, &
         INTV  = DELT,    &
         CLOCK = CLOCK,   &
         RC=STATUS )
    VERIFY_(STATUS)

    call MAPL_GetResource ( STATE, MINSWFRESH, Label="FRESH_NEW_ICE_MIN_SALINITY:" , & 
                           DEFAULT=5.0,    RC=STATUS)
    VERIFY_(STATUS)

    ! Loop over ice catgories

    CATEGORIES: do N=ICE,NUM_SUBTILES

       Nsub = n - ice + 1

       TILES: do k=1, NT

          LATSD = TILELATS(K) *  rad_to_deg
          LONSD = TILELONS(K) *  rad_to_deg
          OBSERVE(1) = abs(LATSD(1)-LATSO) < 1.e-3 .and. abs(LONSD(1)-LONSO) < 1.e-3

          HAVE_ICE: if(FR(K,N) > puny) then
#ifdef USE_R8              
             !TSCDB      =  TSC(K,N) 
             TSCDB      =  TI8(K,Nsub) - MAPL_TICE 
             DRUVRDB    =  swrad(K)*FRUVR*0.6
             DFUVRDB    =  swrad(K)*FRUVR*0.4
             DRPARDB    =  swrad(K)*FRPAR*0.6
             DFPARDB    =  swrad(K)*FRPAR*0.4
             VSUVRDB    =  swrad(K)*FRVISDIR
             VSUVFDB    =  swrad(K)*FRVISDIF
             DRNIRDB    =  swrad(K)*FRNIRDIR
             DFNIRDB    =  swrad(K)*FRNIRDIF
             ALBVRNDB   =  ALBVRN(K,N)
             ALBNRNDB   =  ALBNRN(K,N)
             ALBVFNDB   =  ALBVFN(K,N)
             ALBNFNDB   =  ALBNFN(K,N)
             FSWSFCDB   =  FSWSFC 
             FSWINTDB   =  FSWINT
             FSWTHRUDB  =  FSWTHRU(K,N)
             ISWABSDB   =  ISWABS
             ALBINDB    =  ALBIN 
             ALBSNDB    =  ALBSN 
             FRDB       =  FR(K,N)
             VOLICEDB   =  VOLICE(K,Nsub)
             VOLSNODB   =  VOLSNO(K,Nsub)
             if (trim(SHORTWAVE) == 'dEdd') then
                SSWABSDB   =  SSWABS
                !*** ZTH is actually cos() of solar zenith angle
                COSZTH     =  ZTH(K)  
                ! set snow properties
                call shortwave_dEdd_set_snow(1, 1,        &
                              1, (/1/),(/1/),             &
                              FRDB,     VOLSNODB,         & 
                              TSCDB,    FSN,              &
                              RHOSNWN,  RSNWN)
                 if (.not. TR_POND) then
                   ! set pond properties
                   call shortwave_dEdd_set_pond(1, 1,     &
                                 1,  (/1/),(/1/),         &
                                 FRDB,   TSCDB,           &
                                 FSN,    FPN,             &
                                 HPN)
                 else
                   FPN = APONDN(K, Nsub) 
                   HPN = HPONDN(K, Nsub) 
                 endif 
                 call shortwave_dEdd(1,        1,            &
                                 1, (/1/),(/1/),             &
                                 ! Inputs
                                 COSZTH,                     &
                                 FRDB,      VOLICEDB,        &
                                 VOLSNODB,  FSN,             &
                                 RHOSNWN,   RSNWN,           &
                                 FPN,       HPN,             &
                                 OBSERVE,                    & 
                                 DRUVRDB,   DFUVRDB,         &
                                 DRPARDB,   DFPARDB,         & 
                                 VSUVRDB,   VSUVFDB,         &
                                 DRNIRDB,   DFNIRDB,         &
                                 ! Outputs
                                 ! note the order of the following 4
                                 ! parms is different from that in
                                 ! shortwave_ccsm3
                                 ALBVRNDB,      ALBVFNDB,    &
                                 ALBNRNDB,      ALBNFNDB,    &
                                 FSWSFCDB,      FSWINTDB,    &
                                 FSWTHRUDB,     SSWABSDB,    &
                                                ISWABSDB,    &
                                 DRUVRTHRUDB,   DFUVRTHRUDB, &
                                 DRPARTHRUDB,   DFPARTHRUDB, & 
                                 ALBINDB,  ALBSNDB,   ALBPNDDB  )
                  SSWABS  =  SSWABSDB    
                  ALBPND  =  ALBPNDDB     
             else 
               call shortwave_ccsm3 (          &
                  1,1,1,(/1/),(/1/),           &
                                ! Inputs
                  OBSERVE,                     &
                  DRUVRDB,       DFUVRDB,      &
                  DRPARDB,       DFPARDB,      & 
                  FRDB,          VOLICEDB,     &
                  VOLSNODB,      TSCDB,       &
                  VSUVRDB,       VSUVFDB,    &
                  DRNIRDB,       DFNIRDB,    &
                                ! Outputs
                  ALBVRNDB,      ALBNRNDB, &
                  ALBVFNDB,      ALBNFNDB, &
                  FSWSFCDB,      FSWINTDB,      &
                  FSWTHRUDB,     ISWABSDB,   &
                  DRUVRTHRUDB,   DFUVRTHRUDB,   &
                  DRPARTHRUDB,   DFPARTHRUDB,   & 
                  ALBINDB,       ALBSNDB        )
             endif   
             ALBVRN(K,N) = ALBVRNDB(1)   
             ALBNRN(K,N) = ALBNRNDB(1)     
             ALBVFN(K,N) = ALBVFNDB(1)     
             ALBNFN(K,N) = ALBNFNDB(1)    
             FSWSFC      = FSWSFCDB    
             FSWINT      = FSWINTDB     
             FSWTHRU(K,N)= FSWTHRUDB(1)    
             ISWABS      = ISWABSDB    
             ALBIN       = ALBINDB      
             ALBSN       = ALBSNDB     

             !*** compute sw radiation through skin layer
             PEN(K) = exp(-(KUVR/MAPL_RHOWTR)*HW(K))
             PUR(K) = DRUVRTHRUDB(1)*PEN(K)
             PUF(K) = DFUVRTHRUDB(1)*PEN(K)
             PEN(K) = exp(-(KPAR(K)/MAPL_RHOWTR)*HW(K))
             PPR(K) = DRPARTHRUDB(1)*PEN(K)
             PPF(K) = DFPARTHRUDB(1)*PEN(K)
             PEN(K) = PUR(K) + PUF(K) + PPR(K) + PPF(K)
#else
             call shortwave_ccsm3 (            &
                  1,1,1,(/1/),(/1/),           &
                                ! Inputs
                  OBSERVE,                     & 
                  DRUVR(K),       DFUVR(K),    &
                  DRPAR(K),       DFPAR(K),    & 
                  FR(K,N),        VOLICE(K,NSUB), &
                  VOLSNO(K,NSUB), TSC(K,N),    &
                  VSUVR(K),       VSUVF(K),    &
                  DRNIR(K),       DFNIR(K),    &
                               ! Outputs
                  ALBVRN(K,N),    ALBNRN(K,N), &
                  ALBVFN(K,N),    ALBNFN(K,N), &
                  FSWSFC,         FSWINT,      &
                  FSWTHRU(K,N),   ISWABS(1),   &
                  DRUVRTHRU,      DFUVRTHRU,   &
                  DRPARTHRU,      DFPARTHRU,   & 
                  ALBIN,          ALBSN        )

             !*** compute sw radiation through skin layer
             PEN(K) = exp(-(KUVR/MAPL_RHOWTR)*HW(K))
             PUR(K) = DRUVRTHRU(1)*PEN(K)
             PUF(K) = DFUVRTHRU(1)*PEN(K)
             PEN(K) = exp(-(KPAR(K)/MAPL_RHOWTR)*HW(K))
             PPR(K) = DRPARTHRU(1)*PEN(K)
             PPF(K) = DFPARTHRU(1)*PEN(K)
             PEN(K) = PUR(K) + PUF(K) + PPR(K) + PPF(K)
#endif
             FSWTHRUWTR(K,N) = PEN(K) 
             if(associated(PRUVF)) PRUVF(K) = PRUVF(K) + PUF(K) * FR(K,N) 
             if(associated(PRPAF)) PRPAF(K) = PRPAF(K) + PPF(K) * FR(K,N)
             if(associated(PRUVR)) PRUVR(K) = PRUVR(K) + PUR(K) * FR(K,N) 
             if(associated(PRPAR)) PRPAR(K) = PRPAR(K) + PPR(K) * FR(K,N)

             TAUAGE(K,NSUB) = TAUAGE(K,NSUB) + DT

             !TRACERS(nt_tsfc, Nsub) = TSC(K,N)
             TRACERS(nt_tsfc, Nsub) = TI8(K,Nsub) - MAPL_TICE
             TRACERS(nt_iage, Nsub) = TAUAGE(K,NSUB)
             TRACERS(nt_volpn,Nsub) = VOLPOND(K,Nsub)


#ifdef USE_R8
             TRACERSDB      =  TRACERS(:,Nsub)
             LWDNSRFDB      =  lwrad(K) 
             potTDB         =  t10(K)
             QaDB           =  q10(K)
             rhoaDB         =  rhoa(K)
             SNODB          =  snow(K) 
             TBOTDB         =  TBOT(K) 
             FBOTDB         =  FBOT(K) 
             lhcoeffDB      =  LHCOEFF(K,Nsub) 
             shcoeffDB      =  SHCOEFF(K,Nsub) 
             SSWABSDB       =  SSWABS  
             FSWABSDB       =  FSWABS(K)  
             FSURFDB        =  FSURF(K) 
             FCONDDB        =  FCOND(K,NSUB)
             FCONDBOTDB     =  FCONDBOT(K,NSUB)
             EVPDB          =  EVP(K) 
             FRESHNDB       =  FRESHN(K)
             FSALTNDB       =  FSALTN(K)
             FHOCNNDB       =  FHOCNN(K)       
             MELTTDB        =  MELTT(K)
             MELTSDB        =  MELTS(K)
             MELTBDB        =  MELTB(K)
             CONGELDB       =  CONGEL(K)
             SNOICEDB       =  SNOICE(K)
             LATSDB         =  LATSD 
             LONSDB         =  LONSD 
             MLT_ONSETDB    =  0.0
             FRZ_ONSETDB    =  0.0
             YDAYDB         =  0.0 
             FRDB           =  FR(K,N)
             VOLICEDB       =  VOLICE(K,Nsub)
             VOLSNODB       =  VOLSNO(K,Nsub)
             call thermo_vertical(   & 
                  1,1,DTDB,1,(/1/),(/1/),       &
                  FRDB,                         &
                  TRACERSDB,                    &
                  VOLICEDB,     VOLSNODB,       &
                  ERGICE(K,:,NSUB),             &
                  ERGSNO(K,:,NSUB),             &
                  LWDNSRFDB,     potTDB,        &
                  QaDB,          rhoaDB,        &
                  SNODB,                        &
                  FBOTDB,        TBOTDB,        &
                  lhcoeffDB,     shcoeffDB,     &
                  FSWSFCDB,      FSWINTDB,      &
                  FSWTHRUDB,                    &
                  SSWABSDB,                     &
                  ISWABSDB,                     &
                  
                  FSURFDB,             FCONDDB,            &             
                  SHF0DB,              LHF0DB,             &
                  FSWABSDB,            LWUP0DB,            &
                  EVPDB,               FRESHNDB,           &
                  FSALTNDB,            FHOCNNDB,           &
                  MELTTDB,             MELTSDB,            &
                  MELTBDB,                                 &
                  CONGELDB,            SNOICEDB,           &
                  
                  RDUMDB,  RDUMDB,   RDUMDB,  RDUMDB,      &
                  LATSDB, LONSDB, OBSERVE, FCONDBOTDB, sblx,     & 
                  
                  MLT_ONSETDB,   FRZ_ONSETDB,     & 
                  YDAYDB,          L_STOP,        &
                  IDUM,          JDUM, DO_DATAATM/=0 )

             if(L_STOP) then
                print*, 'Failing at PE = ', mype,  ' N = ', N, ' K = ', K, & 
                          ' LAT = ', LATSD, 'LON = ', LONSD 
             endif 
             ASSERT_(.not.L_STOP)
 
             SHF0               =  SHF0DB 
             LHF0               =  LHF0DB 
             LWUP0              =  LWUP0DB
             TRACERS(:, Nsub)   =  TRACERSDB 
             FSWTHRU(K,N)       =  FSWTHRUDB(1)    
             FCOND(K,NSUB)      =  FCONDDB(1)          
             FCONDBOT(K,NSUB)   =  FCONDBOTDB(1)          
             FSURF(K)           =  FSURFDB(1)
             FSWABS(K)          =  FSWABSDB(1)
             EVP(K)             =  EVPDB(1)
             FRESHN(K)          =  FRESHNDB(1)         
             FSALTN(K)          =  FSALTNDB(1)                  
             FHOCNN(K)          =  FHOCNNDB(1)                  
             MELTT(K)           =  MELTTDB(1)                   
             MELTS(K)           =  MELTSDB(1)                   
             MELTB(K)           =  MELTBDB(1)                   
             CONGEL(K)          =  CONGELDB(1)                  
             SNOICE(K)          =  SNOICEDB(1)                  
             FR(K,N)            =  FRDB(1)
             VOLICE(K,Nsub)     =  VOLICEDB(1)       
             VOLSNO(K,Nsub)     =  VOLSNODB(1)        
#else             
             call thermo_vertical(   & 
                  1,1,DT,1,(/1/),(/1/),         &
                  FR(K,N),                      &
                  TRACERS(:,Nsub),                 &
                  VOLICE(K,NSUB),   VOLSNO(K,NSUB),   &
                  ERGICE(K,:,NSUB), ERGSNO(K,:,NSUB), &
                  LWDNSRF(K),    RDUM,          &
                  RDUM,          RDUM,          &
                  SNO(K),                       &
                  FBOT(K),       TBOT(K),       &
                  RDUM,          RDUM,          &
                  FSWSFC,        FSWINT,        &
                  FSWTHRU(K,N),                 &
                  SSWABS,                       &
                  ISWABS,                       &
                  
                  FSURF,               FCOND(K,NSUB),       &
                  SHF0,                LHF0,                &
                  FSWABS,              LWUP0,               &
                  EVP(K),              FRESHN(K),           &
                  FSALTN(K),           FHOCNN(K),           &
                  MELTT(K),            MELTS(K),            &
                  MELTB(K),                                 &
                  CONGEL(K),           SNOICE(K),           &
                  
                  DFSDT,-DSH(K),-DLHDT,-BLW(K),             &
                  ERGSUM(K,NSUB),                           & 
                  LATSD, LONSD, OBSERVE,FCONDBOT(K), sblx,       & 
                  MLT_ONSET,     FRZ_ONSET,     & 
                  YDAY,          L_STOP,        &
                  IDUM,          JDUM, DO_DATAATM/=0 )
             if(L_STOP) then
                print*, 'Failing at N = ', N, ' LAT = ', LATSD, 'LON = ', LONSD 
             endif 
             ASSERT_(.not.L_STOP)
#endif
             ! need to update these for aggregation later 

             SHF(K) = SHF0(1)
             LHF(K) = LHF0(1)
             LWUP(K) = LWUP0(1)

             TI8(K,Nsub)    =   TRACERS(nt_tsfc,Nsub) + MAPL_TICE 
             TAUAGE(K,Nsub) =   TRACERS(nt_iage,Nsub) 

#ifdef USE_R8
             if (TR_POND .and. trim(SHORTWAVE) == 'dEdd') then
                 MELTTDB        =  MELTT(K)
                 MELTSDB        =  MELTS(K)
                 FRDB           =  FR(K,N)
                 VOLICEDB       =  VOLICE(K,Nsub)
                 VOLSNODB       =  VOLSNO(K,Nsub)
                 APONDNDB       =  APONDN(K,Nsub)
                 HPONDNDB       =  HPONDN(K,Nsub)
                 !TRACERSDB      = TRACERS(:,Nsub)
                 FRAINDB        =  rain(K) 
                 call compute_ponds(1, 1,                      &
                               1, 1, 1, 1,                     &
                               MELTTDB, MELTSDB, FRAINDB,      &
                               FRDB, VOLICEDB,                 &
                               VOLSNODB, TRACERSDB,            &
                               APONDNDB, HPONDNDB)
                  TRACERS(:,Nsub) = TRACERSDB 
                  VOLPOND(K,Nsub) = TRACERS(nt_volpn,Nsub) 
                  APONDN (K,Nsub) = APONDNDB(1)       
                  HPONDN (K,Nsub) = HPONDNDB(1)       
             endif
#endif

          end if HAVE_ICE
#if 0
       if(OBSERVE(1)) then
          print*, 'TW ', TW(k)
          print*, Nsub, FR(K,N), TI8(K,Nsub)  
          print*, Nsub, lwrad(K)
       endif
#endif
       end do TILES ! K loop

       ! update TS in kelvin
       !TS(:,N) = TSC(:,N) + Tffresh

! Update surface temperature and moisture
!----------------------------------------
       
       if(associated(SHICE)) SHICE = SHICE + SHF    *FR(:,N)
       if(associated(HLATI)) HLATI = HLATI + LHF    *FR(:,N)

       ! *** some aggregation have to be done here for some 
       ! *** fluxes to be used later in step2 of thermodynamics 

       ! aggregate fluxes into ocean
       FRESHL   = FRESHL   + FRESHN *FR(:,N)
       FSALTL   = FSALTL   + FSALTN *FR(:,N)
       FHOCNL   = FHOCNL   + FHOCNN *FR(:,N)

       if(associated(FSURFL )) FSURFL  = FSURFL + FSURF        * FR(:,N)
       if(associated(evapx  )) evapx   = evapx  + EVP          * FR(:,N)
       if(associated(shfx   )) shfx    = shfx   + SHF          * FR(:,N)
       if(associated(lhfx   )) lhfx    = lhfx   + LHF          * FR(:,N) 
       if(associated(lwnx   )) lwnx    = lwnx   + (lwrad+LWUP) * FR(:,N)
       if(associated(swnx   )) swnx    = swnx   + FSWABS       * FR(:,N)

       if(associated(MELTTL )) MELTTL   = MELTTL   + MELTT   *FR(:,N) / DT ! m per step -> m s-1
       if(associated(MELTBL )) MELTBL   = MELTBL   + MELTB   *FR(:,N) / DT ! m per step -> m s-1
       if(associated(MELTSL )) MELTSL   = MELTSL   + MELTS   *FR(:,N) / DT ! m per step -> m s-1
       if(associated(CONGELO)) CONGELO  = CONGELO  + CONGEL  *FR(:,N) / DT ! m per step -> m s-1

    end do CATEGORIES

    !*** skin layer only absorbs the portion of sw passing thru the bottom of ice MINUS
    !*** the portion passing thru the skin layer and is is save here to be used later  
    FSWABSUNDICE=sum(FR(:,ICE:)*(FSWTHRU(:,ICE:)-FSWTHRUWTR(:,ICE:)),dim=2)

    if(associated(FBOTL  )) FBOTL   = FBOT 

    ! step2 of thermodynamics (step_therm2) has loop over ice
    ! categories within the  subroutines. This redistributes
    ! ice and water mass due to freezing and melting

    TILES_1: do k=1, NT
       
       LATSD = TILELATS(K) *  rad_to_deg
       LONSD = TILELONS(K) *  rad_to_deg
       OBSERVE(1) = abs(LATSD(1)-LATSO) < 1.e-3 .and. abs(LONSD(1)-LONSO) < 1.e-3
       !TRACERS(nt_tsfc,:) = TSC(K,ICE:)
       TRACERS(nt_tsfc,:) = TI8(K,:) - MAPL_TICE
       TRACERS(nt_iage,:) = TAUAGE(K,:)
       TRACERS(nt_volpn,:)= VOLPOND(K,:)
#ifdef USE_R8
       TRACERSDB2  = TRACERS
#endif
       if(FRCICE(K) > 0.0) then 
#ifdef USE_R8
          FRWATERDB  =  FR(K,WATER)
          FRCICEDB   =  FRCICE(K)
          call linear_itd (1,1,1,(/1/),(/1/), &
            TRCRTYPE,     &
            AICENINIT(K,:),&
            VICENINIT(K,:),&
            FR(K,ICE:),    &
            TRACERSDB2,    & 
            VOLICE(K,:),  VOLSNO(K,:), & 
            ERGICE(K,:,:),     &
            ERGSNO(K,:,:),     &
            FRCICEDB,      &
            FRWATERDB,     &
            LATSD , LONSD, &  
            L_STOP,        &
            IDUM,    JDUM )
          FR(K,WATER) =  FRWATERDB(1)    
          FRCICE(K)   =  FRCICEDB(1)
#else
          call linear_itd (1,1,1,(/1/),(/1/), &
            TRCRTYPE,     &
            AICENINIT(K,:),   &
            VICENINIT(K,:),   &
            FR(K,ICE:),   &
            TRACERS(:,:), & 
            VOLICE(K,:),   VOLSNO(K,:), & 
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FRCICE(K),    &
            FR(K,WATER),  &
            L_STOP,       &
            IDUM,    JDUM )
#endif
          ASSERT_(.not.L_STOP)
       endif 

#ifdef USE_R8
       FRZMLTDB       =  FRZMLT(K)
       FRAZLNDB       =  FRAZLN(K)
       FRESHLDB       =  FRESHL(K)
       FSALTLDB       =  FSALTL(K)
       TFDB           =  TF(K)   
       RDUMDB         =  0.0
       YDAYDB         =  0.0 
       FRWATERDB      =  FR(K,WATER)
       call add_new_ice (1,1,1,(/1/),(/1/),(/.true./), DTDB,  &
            FR(K,ICE:),      &
            TRACERSDB2,     &
            VOLICE(K,:), &
            ERGICE(K,:,:), &
            FRWATERDB,    &
            FRCICEDB,      &
            FRZMLTDB,      &
            FRAZLNDB,      &
            SW(K) < MINSWFRESH,   &
            RDUMDB,YDAYDB,      &
            FRESHLDB,      &
            FSALTLDB,      &
            TFDB, L_STOP,  &
            IDUM,       JDUM)
       FR(K,WATER) =  FRWATERDB(1)    
       FRCICE(K)   =  FRCICEDB(1)
#else
       call add_new_ice (1,1,1,(/1/),(/1/),(/.true./), DT,  &
            FR(K,ICE:),          &
            TRACERS(:,:),     &
            VOLICE(K,:), &
            ERGICE(K,:,:), &
            FR(K,WATER),    &
            FRCICE(K),      &
            FRZMLT(K),      &
            FRAZLN(K),.true.,      &
            RDUM,YDAY,      &
            FRESHL(K),      &
            FSALTL(K),      &
            TF(K), L_STOP,  &
            IDUM,       JDUM)
#endif
       ASSERT_(.not.L_STOP)
       

       VOLICE_PREV   =  VOLICE(K,:)
#ifdef USE_R8
       FHOCNLDB      =  FHOCNL(K)
       RSIDEDB       =  RSIDE(K)
       MELTLNDB      =  MELTLN(K)
       call lateral_melt (1,1,1,1,1,1,DTDB, &
            FRESHLDB,      &
            FSALTLDB,      &    
            FHOCNLDB,      &
            RSIDEDB,       &
            MELTLNDB,      &
            FR(K,ICE:),      &
            VOLICE(K,:), &
            VOLSNO(k,:), &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:) )
#else
       call lateral_melt (1,1,1,1,1,1,DT, &
            FRESHL(K),      &
            FSALTL(K),      &    
            FHOCNL(K),      &
            RSIDE(K),       &
            MELTLN(K),      &
            FR(K,ICE:),     &
            VOLICE(K,:), &
            VOLSNO(K,:), &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:) )
#endif

#ifdef USE_R8
       SNOICEDB = 0.0
       call freeboard_ccsm (1,1,1,1,1,1, DTDB, &
                            FR(K,ICE:),  &
                            VOLICE(K,:),  VOLSNO(K,:),   &
                            ERGICE(K,:,:), &
                            ERGSNO(K,:,:), &
                            SNOICEDB,   &
                            FSALTLDB)      
         
       SNOICE(K) = SNOICEDB(1)
#endif

#ifdef USE_R8
       FRWATERDB      =  FR(K,WATER)
       FRCICEDB       =  1 - FR(K,WATER)
       call cleanup_itd (1,1,1,1,1,1,DTDB, &
            FR(K,ICE:),    TRACERSDB2,  &
            VOLICE(K,:),   VOLSNO(K,:),   &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FRWATERDB,    FRCICEDB,          &
            TRCRTYPE,                        &
            FRESHLDB,     FSALTLDB,          &
            FHOCNLDB,                        &
            .true.,        L_STOP,           &
            IDUM,            JDUM,           & 
            limit_aice_in=.true.)
        FR(K,WATER) =  FRWATERDB(1)    
        FRCICE(K)   =  FRCICEDB(1)
#else
       call cleanup_itd (1,1,1,1,1,1,DT, &
            FR(K,ICE:),   TRACERS(:,:),  &
            VOLICE(K,:),  VOLSNO(K,:),   &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FR(K,WATER),  FRCICE(K),           &
            TRCRTYPE,                          &
            FRESHL(K),     FSALTL(K),          &
            FHOCNL(K),                         &
            .true.,        L_STOP,             &
            IDUM,            JDUM)
#endif       
       ASSERT_(.not.L_STOP)

#ifdef USE_R8
       TRACERS       = TRACERSDB2
       FRAZLN(K)     = FRAZLNDB(1)         
       FRESHL(K)     = FRESHLDB(1)         
       FSALTL(K)     = FSALTLDB(1)         
       FHOCNL(K)     = FHOCNLDB(1)        
       MELTLN(K)     = MELTLNDB(1)        
#endif

       !TSC(K,ICE:)  =   TRACERS(nt_tsfc,:) 
       TI8(K,:)     =   TRACERS(nt_tsfc,:) + MAPL_TICE 
       TAUAGE(K,:)  =   TRACERS(nt_iage,:) 
       VOLPOND(K,:) =   TRACERS(nt_volpn,:)
#ifndef USE_R8
       call ColumnSum(ICE, NUM_SUBTILES, FR(K,:), &
                           ERGICE(K,:,:),         &
                           ERGSNO(K,:,:),         & 
                           ERGSUM(K,:))
#endif
    end do TILES_1


    FRCICE = sum(FR(:,ICE:), dim=2)
    FR(:,WATER) = max(1.0-FRCICE, 0.0)

#if 0
    !*** artificially do a lateral melt step over those frozen lake
    !*** tiles if the ice gets too thick

    !*** do not do this step anymore
    !*** allow the ice to grow, such that budget will be computed 
    !*** consistently

    call MAPL_GetResource ( STATE, ICE_THICKNESS_THRESH, Label="CICE_ICE_THICKNESS_THRESH:" , DEFAULT=1.5, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_GetResource ( STATE, ICE_ARTIFICIAL_MELT, Label="CICE_ICE_ARTIFICIAL_MELT:" , DEFAULT=0.1, RC=STATUS)
    VERIFY_(STATUS)

    TILES_2: do k=1, NT
       
      if(SLMASK(K) > 0.5 .and. FRCICE(K) > 0.0 &
         .and. sum(VOLICE(K,:)) > ICE_THICKNESS_THRESH) then

       LATSD = TILELATS(K) *  rad_to_deg
       LONSD = TILELONS(K) *  rad_to_deg
       OBSERVE(1) = abs(LATSD(1)-LATSO) < 1.e-3 .and. abs(LONSD(1)-LONSO) < 1.e-3
       TRACERS(nt_tsfc,:) = TSC(K,ICE:)
       TRACERS(nt_iage,:) = TAUAGE(K,:)
       TRACERS(nt_volpn,:)= VOLPOND(K,:)
#ifdef USE_R8
       TRACERSDB2  = TRACERS
#endif

#ifdef USE_R8
       FRESHLDB       =  FRESHL(K)
       FSALTLDB       =  FSALTL(K)
       RDUMDB         =  0.0
       YDAYDB         =  0.0 
       FRWATERDB      =  FR(K,WATER)
#endif

#ifdef USE_R8
       FHOCNLDB      =  FHOCNL(K)
       RSIDEDB       =  ICE_ARTIFICIAL_MELT
       MELTLNDB      =  MELTLN(K)
       call lateral_melt (1,1,1,1,1,1,DTDB, &
            FRESHLDB,      &
            FSALTLDB,      &    
            FHOCNLDB,      &
            RSIDEDB,       &
            MELTLNDB,      &
            FR(K,ICE:),      &
            VOLICE(K,:), &
            VOLSNO(k,:), &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:) )
#else
       call lateral_melt (1,1,1,1,1,1,DT, &
            FRESHL(K),      &
            FSALTL(K),      &    
            FHOCNL(K),      &
            RSIDE(K),       &
            MELTLN(K),      &
            FR(K,ICE:),     &
            VOLICE(K,:), &
            VOLSNO(K,:), &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:) )
#endif

#ifdef USE_R8
       SNOICEDB = 0.0
       call freeboard_ccsm (1,1,1,1,1,1, DTDB, &
                            FR(K,ICE:),  &
                            VOLICE(K,:),  VOLSNO(K,:),   &
                            ERGICE(K,:,:), &
                            ERGSNO(K,:,:), &
                            SNOICEDB,   &
                            FSALTLDB)      
         
#endif

#ifdef USE_R8
       FRWATERDB      =  FR(K,WATER)
       FRCICEDB       = 1 - FR(K,WATER)
       call cleanup_itd (1,1,1,1,1,1,DTDB, &
            FR(K,ICE:),    TRACERSDB2,  &
            VOLICE(K,:),   VOLSNO(K,:),   &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FRWATERDB,  FRCICEDB,         &
            TRCRTYPE,                        &
            FRESHLDB,     FSALTLDB,          &
            FHOCNLDB,                        &
            .true.,        L_STOP,           &
            IDUM,            JDUM,           & 
            limit_aice_in=.true.)
        FR(K,WATER) =  FRWATERDB(1)    
        FRCICE(K)   =  FRCICEDB(1) 
#else
       call cleanup_itd (1,1,1,1,1,1,DT, &
            FR(K,ICE:),   TRACERS(:,:),  &
            VOLICE(K,:),  VOLSNO(K,:),   &
            ERGICE(K,:,:), &
            ERGSNO(K,:,:), &
            FR(K,WATER),  FRCICE(K),           &
            TRCRTYPE,                          &
            FRESHL(K),     FSALTL(K),          &
            FHOCNL(K),                         &
            .true.,        L_STOP,             &
            IDUM,            JDUM)
#endif       
       ASSERT_(.not.L_STOP)

#ifdef USE_R8
       TRACERS       = TRACERSDB2
#endif
       TSC(K,ICE:)  =   TRACERS(nt_tsfc,:) 
       TAUAGE(K,:)  =   TRACERS(nt_iage,:) 
       VOLPOND(K,:) =   TRACERS(nt_volpn,:)
      endif
    end do TILES_2

#endif

    ! update TS in kelvin again (only over ice categories)
    !TS(:,ICE:) = TSC(:,ICE:) + Tffresh

    ! aggregate ice concentration after step2
    ! These are the final area fractions that are in the internal state

    !FRCICE = sum(FR(:,ICE:), dim=2)
    !FR(:,WATER) = max(1.0-FRCICE, 0.0)

    if(associated(DAIDTT)) then
         DAIDTT = (FRCICE - FR_OLD) / DT * 8640000
         deallocate(FR_OLD)
    endif
    if(associated(SNOICEO)) SNOICEO  = SNOICE / DT ! m per step -> m s-1


    SW  = .001*SW *HW
    HW  = HW - SW

    !*** in a time splitting fasion, we update TW here to account for
    !*** FHOCNL accumulated in step2 of thermodynamics
    !TS(:, WATER) = TS(:,WATER)+ DT*FRCICE*FHOCNL/(SALTWATERCAP*HH(:,WATER))     
    DTS =  DT*(FHOCNL+FSWABSUNDICE)/(SALTWATERCAP*HW)
    TW  =  TW + DTS 
    if(associated(TWINC3  )) TWINC3   = DTS 
    DTSACCUM = DTSACCUM+DTS
    if(associated(TWINCT  )) TWINCT   = DTSACCUM
    !*** added the part accummulated in stepthem2
    if(associated(FHOCN  )) FHOCN   = FHOCN + FHOCNL

    ! account for ice meltwater at the top and bottom surface
    !          or water frozen at the bottom surface  
    !HH(:,WATER) = HH(:,WATER) + DT*FRCICE*FRESHL
    HW = HW + DT*FRESHL
    HW = max(min(HW,MAXWATERDEPTH),MINWATERDEPTH)
    !SW = SS(:,WATER)/HW
    ! account for flux of salt (>0) under melting conditions
    !             or negative flux when sea water is freezing                
    !***multiply by 1000 to account for g->kg conversion
    !SW = (SS(:,WATER)+DT*FRCICE*1.e3*FSALTL)/HW
    SW = SW + DT*FSALTL

    HW  = HW + SW
    SW  = 1000.*(SW/HW) ! BACK TO PSU

    where (SLMASK > 0.5 .and. FRCICE > 0.0)
        SW = max(min(SW,MAXSALINITY),MINSALINITY)
    endwhere
    if(associated(SSKINW2)) SSKINW2 = SW


    if(associated(FRI )) FRI  = FRCICE
    if(associated(ISTSFC)) then
        ! to be consisten with CICE (unit in degC)
        ISTSFC = sum(TSC(:,ICE:)*FR(:,ICE:),dim=2) 
        where(FRCICE > 0.0)
           ISTSFC = ISTSFC / FRCICE
        elsewhere
           ISTSFC = -1.8
        end where
    end if
    if(associated(IAGE)) then
        ! here ice age is treated as an ice area tracer
        IAGE = sum(TAUAGE(:,ICE:)*FR(:,ICE:),dim=2) * iage_converter
        where(FRCICE > 0.0)
           IAGE = IAGE / FRCICE
        elsewhere
           IAGE = 0.0
        end where
    end if


    ! the mean ice/snow thickness is computed as 
    ! sum_n_over_ice_categories(FR(n)*H(n)) which is simply 
    ! sum_n_over_ice_categories(VOL(n)) 
    if(associated(HICE  )) HICE    =  sum(VOLICE(:,:),dim=2)
    if(associated(DVIDTT))  then
        DVIDTT  = (sum(VOLICE,dim=2) - VOLICE_OLD) / DT * 8640000
        deallocate(VOLICE_OLD)
    endif  

    if(associated(HSNO  )) HSNO    =  sum(VOLSNO(:,:),dim=2)

    if(associated(MELTL )) MELTL   =  MELTLN / DT ! m per step -> m s-1
    if(associated(FRAZIL)) FRAZIL  =  FRAZLN / DT ! m per step -> m s-1




! Clean up
!---------

   deallocate(rain)
   deallocate(snow)
   deallocate(rr)
   deallocate(swrad)
   deallocate(lwrad)
   deallocate(t10)
   deallocate(q10)
   deallocate(u10)
   deallocate(v10)
   deallocate(slp)
   deallocate(sss)
   deallocate(pen)
   deallocate(rhoa)
   deallocate(qsat)
   deallocate(ustar)
   deallocate(cd)
   deallocate(ch)
   deallocate(ce)

   deallocate(dry_clay)
   deallocate(wet_clay)
   deallocate(sed_clay)
   deallocate(taua)
   deallocate(asymp)
   deallocate(ssalb)
   deallocate(co2sc)
   deallocate(ccovm)
   deallocate(cldtcm)
   deallocate(rlwpm)
   deallocate(cdrem)
   deallocate(rh)
   deallocate(oz)
   deallocate(wv)

    deallocate(FRZMLT)
    deallocate(SHF)
    deallocate(EVP)
    deallocate(TXI)
    deallocate(TYI)
    deallocate(PUR)
    deallocate(PUF)
    deallocate(PPR)
    deallocate(PPF)
    deallocate(LHF)
    deallocate(LWUP)
    deallocate(SLR)
    deallocate(ZTH)
    deallocate(FSWABS)
    deallocate(FSURF)
    deallocate(ALBVRN)
    deallocate(ALBVFN)
    deallocate(ALBNRN)
    deallocate(ALBNFN)
    deallocate(FCOND)
    deallocate(FCONDBOT)
    deallocate(LHCOEFF)
    deallocate(SHCOEFF)
    deallocate(ALBVRI)
    deallocate(ALBVFI)
    deallocate(ALBNRI)
    deallocate(ALBNFI)
    deallocate(FSWABSUNDICE)
    deallocate(FRCICE)
    deallocate(TF)
    deallocate(TBOT)
    deallocate(FBOT)
    !deallocate(SLMASK)
    deallocate(RSIDE)
    deallocate(FRESHL)
    deallocate(FRESHN)
    deallocate(FSALTL)
    deallocate(FSALTN)
    deallocate(FRAZLN)
    deallocate(MELTLN)
    deallocate(MELTB)
    deallocate(MELTT)
    deallocate(MELTS)
    deallocate(SNOICE)
    deallocate(CONGEL)
    deallocate(DTS)
    deallocate(DTSACCUM)
    deallocate(AICENINIT)
    deallocate(VICENINIT)
    deallocate(FSWTHRU)
    deallocate(FSWTHRUWTR)

   deallocate(var1)
   deallocate(var2)
   deallocate(var3)

!  All done
!-----------

    call MAPL_TimerOff(STATE,"RUN"  )
    call MAPL_TimerOff(STATE,"TOTAL")

    RETURN_(ESMF_SUCCESS)

end subroutine RUN

!----------------------------------------------------------------------------------------------------------------------------------

end module GEOS_DataAtmGridCompMod
