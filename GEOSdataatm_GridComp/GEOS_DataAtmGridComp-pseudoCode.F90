! 
! ------------------------------------------------------------
! Pseudo code for the GEOS Data Atmosphere Griddded Component
! ------------------------------------------------------------
!
! OBJECTIVE:
! ---------
! It is an ``atmospheric" component for ocean-only simulations, 
! therefore will always run with a REALISTIC sea ice (e.g., LANL CICE)

! It does following:
! -----------------
!
! 1. Gets imports from GCM Gridded component, which provides exports from ocean, sea ice, 
!    e.g., UW, VW, UI, VI, KPAR, etc.
!    See following section A for details.
!
! 2. Read atmospheric (surface) fields e.g., T10M, Q10M, U10M, etc  from files, 
!    hence the atmosphere is "prescribed"
!    See following section B for details.
!
! 3. Initialze LANL CICE thermodynamics (as in GEOS_CICE4ColumnPhysGridComp.F90)
! 4. Compute atmospheric boundary (also called "surface") layer transfer coefficients, 
!    e.g., Cd, Ch, Cq
!     a. Currently uses ncar_ocean_fluxes, should use helfsurface
!     b. Code from GEOS_SurfaceGridComp.F90 could be used if needed for above 4.a
! 5. Compute or update total precipitation, surface radiation, heat fluxes, etc
! 6. Update water temperature (SST), salinity (SSS)
! 7. Repeat above steps 4- 6 over sea ice, using LANL CICE (as in GEOS_CICE4ColumnPhysGridComp.F90)
! 8. Update fr of sea ice and snow
! 9. Fill up exports to "drive" the ocean, sea ice models
!
! ------------------------------------------------------------
!
! Section A
! Exchange between the atmosphere and ocean components
! (Following is based on GEOS_GcmGridComp.F90)
!
! A.1 From ocean TO the atmosphere:
! -----------------------------
! variable name (in GEOS_GcmGridComp.F90)       description
!
! UW        surface u- velocity component over water
! VW        surface v- velocity component over water
! KPAR      Chlorophyll concentration to K par for radiation absorption (Beer's law)
! TS_FOUND  ocean model top level temperature (or "foundation" if data ocean)
! SS_FOUND  ocean model top level salinity    (or "foundation" if data ocean)
!    -- sea ice -- 
! UI        surface u- velocity component over sea ice
! VI        surface v- velocity component over sea ice
! FRZMLT    ocean freeze-melt potential (heat lost/gained due to sea formation/melt)
!     -- if CICE -- 
! TAUXBOT   x-stress over sea ice bottom
! TAUYBOT   y-stress over sea ice bottom
!     -- else  -- 
! FRACICE   fraction of sea ice
! 
! A.2 From atmosphere TO ocean:
! -------------------------
! variable name (in GEOS_GcmGridComp.F90)       description
!
! TI (TSKINI)   ice surface temperature
! HI (HSKINI)   thickness of snow/ice layer??
! SI (SSKINI)   ice surface salinity
!
!     -- if CICE -- 
! FRACICE (FR)       fraction of sea ice
! VOLVICE (VOLICE)   volume of sea ice
! VOLSNO  (VOLSNO)   volume of snow on top of sea ice
! ERGICE  (ERGICE)   enthalpy (?) of ice
! ERGSNO  (ERGSNO)   enthalpy (?) of snow
! TAUAGE  (TAUAGE)   ?
! MPOND   (VOLPOND)  volume of melt pond
!     -- else  -- 
! none
!
! TAUXW              x-stress over water
! TAUYW              y-stress over water
! TAUXI              x-stress over sea ice
! TAUYI              y-stress over sea ice
! PENPAR             component of SW radiation 
! PENPAF             component of SW radiation 
! PENUVR             component of SW radiation 
! PENUVF             component of SW radiation 
! OUSTAR3            ustar cubed
! PS                 surface pressure
! DICHRG             discharge from run off (river, glacier)
! AO_LWFLX           longwave 
! AO_SHFLX           shortwave
! AO_QFLUX           mass flux
! AO_SNOW            snow
! AO_RAIN            rain
! AO_DRNIR           component of SW radiation
! AO_DFNIR           component of SW radiation
! FRESH              fresh water flux
! FSALT              salt flux
! FHOCN              ?
!
!  --- if obio, rad --
! UU
! CO2SC
! DUDP
! DUWT
! DUSD
! BCDP
! BCWT
! OCDP
! OCWT
! FSWBAND
! FSWBANDNA
! ------------------------------------------------------------
! ------------------------------------------------------------
!
! Section B
! Read following fields from files
!
! rain (rain rate)
! snow (snow rate)
!
!
!  --- if obio, rad --
! additionally read and export to the ocean:
! -----------------------------------------
! 
! CO2SC
! DUDP
! DUWT
! DUSD
! ??
! CCOVM
! CLDTCM 
! RLWPM
! CDREM
! RH
! OZ
! WV
! TAUA_, ASYMP_, SSALB_
! ??
! ------------------------------------------------------------

