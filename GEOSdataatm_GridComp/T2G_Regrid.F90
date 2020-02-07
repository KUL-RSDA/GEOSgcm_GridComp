subroutine T2G_Regrid(MAPL, LOCSTREAM, GNAME, TNAME, RC)

  type(ESMF_State)     :: MAPL
  type(MAPL_LocStream) :: LOCSTREAM
  character(len=*)     :: GNAME
  character(len=*)     :: TNAME
  integer, optional    :: RC

  real, pointer :: GVAR(:,:) => null()
  real, pointer :: TVAR(:) => null()

  character(len=ESMF_MAXSTR)   :: IAm='T2G_Regrid'
  integer                      :: STATUS

  call GET_POINTER(MAPL, GVAR, GNAME, RC=STATUS)
  VERIFY_(STATUS)

  if(associated(GVAR)) then
     call GET_POINTER(MAPL, TVAR, TNAME, RC=STATUS)
     VERIFY_(STATUS)
     call MAPL_LocStreamTransform( LOCSTREAM, GVAR, TVAR, RC=STATUS)
     VERIFY_(STATUS)
  endif

  RETURN_(ESMF_SUCCESS)
end subroutine T2G_Regrid
