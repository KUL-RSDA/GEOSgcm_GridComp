subroutine MK_GRID_OUT(MAPL, GNAME, TNAME, RC)

  type(ESMF_State)               :: MAPL
  character(len=*)               :: GNAME
  character(len=*)               :: TNAME

  integer, optional, intent(OUT) :: RC

  real, pointer                  :: GVAR(:,:) => null()
  real, pointer                  :: TVAR(:)   => null()
  character(len=ESMF_MAXSTR)     :: IAm='MK_GRID_OUT'
  integer                        :: STATUS

  call GET_POINTER(MAPL, GVAR, GNAME, RC=STATUS)
  VERIFY_(STATUS)

  if(associated(GVAR)) then
    call GET_POINTER(MAPL, TVAR, TNAME, alloc=.true., RC=STATUS)
    VERIFY_(STATUS)
    TVAR = MAPL_UNDEF
  end if

  RETURN_(ESMF_SUCCESS)
end subroutine MK_GRID_OUT
