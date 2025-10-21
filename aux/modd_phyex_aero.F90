MODULE MODD_PHYEX_AERO
  
  IMPLICIT NONE


LOGICAL      :: LORILAM     = .FALSE.       ! switch to active aerosols fluxes
REAL              :: XINISIGI      = 1.75    ! dispersion initialization for I mode 
REAL              :: XINISIGJ      = 1.76    ! dispersion initialization for J mode
REAL              :: XINIRADIUSJ   = 0.200   ! mean radius initialization for J mode (um)
CHARACTER(LEN=4)  :: CRGUNIT       = 'NUMB'  ! type of log-normal geometric mean radius
! volumar mass of species i [kg/m3]
REAL, SAVE, DIMENSION(:), ALLOCATABLE ::  XRHOI
INTEGER, PARAMETER :: NCARB=3     ! number of chemically inert species
                                  ! (like black carbon)
INTEGER            :: NSOA = 10    ! number of condensable species that may form
                                   ! secondary aerosols

INTEGER, PARAMETER :: NSP=4        ! number of chemical species
                                   ! for ARES or isorropia NSP=4 these are

INTEGER, PARAMETER :: JP_AER_OC = 5
INTEGER, PARAMETER :: JP_AER_BC = 6
INTEGER, PARAMETER :: JP_AER_DST = 7
INTEGER, PARAMETER :: JP_AER_H2O = 4
INTEGER, PARAMETER :: JP_AER_SO4 = 1

! modd_salt
LOGICAL      :: LSALT     = .FALSE.   ! switch to active pronostic sea salts
INTEGER      :: NMODE_SLT= 3  ! number of sea salt modes (max 3; default = 3)
!Initial dry number median radius (um) from Schultz et al., 2004
REAL, DIMENSION(3)          :: XINIRADIUS_SLT= 0.5*(/0.28, 2.25, 15.28/)
!Initial, standard deviation from Vignati et al., 2001
REAL, DIMENSION(3)          :: XINISIG_SLT =  (/1.9, 2., 2./)
CHARACTER(LEN=4)  :: CRGUNITS   = 'MASS'  ! type of log-normal geometric mean radius


! modd_dust
LOGICAL      :: LDUST     = .FALSE.   ! switch to active pronostic dusts
!2 modes will be mode 2 & 3, whereas 3 modes will modes 1, 2 and 3
INTEGER, DIMENSION(3),PARAMETER  :: JPDUSTORDER = (/3, 2, 1/)
! NEW PARAMETERIZATION FROM AMMA, default
!Initial dry number median radius (um) 
REAL, DIMENSION(3)          :: XINIRADIUS= 0.5*(/0.088, 0.643, 3.75 /)
!Initial, standard deviation from Alfaro et al 1998
REAL, DIMENSION(3)          :: XINISIG =  (/2.0, 1.78, 1.85/)
CHARACTER(LEN=4)  :: CRGUNITD   = 'NUMB'  ! type of log-normal geometric mean radius
!                                          !given in namelist (mass on number)


! modd_csts_salt and modd_csts_dust
REAL, PARAMETER  :: XDENSITY_SALT     = 2.1e3     ![kg/m3] density of dust
REAL, PARAMETER  :: XDENSITY_DUST = 2.5e3         ![kg/m3] density of dust



END MODULE MODD_PHYEX_AERO
