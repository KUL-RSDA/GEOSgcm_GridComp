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
!    e.g., UW, VW, UI, VI, KPAR
! 2. Read atmospheric (surface) fields e.g., T10M, Q10M, U10M, etc  from files, 
!    hence the atmosphere is "prescribed"
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

