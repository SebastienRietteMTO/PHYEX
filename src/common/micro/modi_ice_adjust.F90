!     ######################
      MODULE MODI_ICE_ADJUST
!     ######################
!
IMPLICIT NONE
INTERFACE
!
      SUBROUTINE ICE_ADJUST (D, CST, ICEP, NEBN, TURBN, PARAMI, BUCONF, KRR,   &
                            &HBUNAME,                                          &
                            &PTSTEP, PSIGQSAT,                                 &
                            &PRHODJ, PEXNREF, PRHODREF, PSIGS, LMFCONV, PMFCONV,&
                            &PPABST, PZZ,                                      &
                            &PEXN, PCF_MF, PRC_MF, PRI_MF, PWEIGHT_MF_CLOUD,   &
                            &PICLDFR, PWCLDFR, PSSIO, PSSIU, PIFR,             &
                            &PRV, PRC, PRVS, PRCS, PTH, PTHS,                  &
                            &OCOMPUTE_SRC, PSRCS, PCLDFR,                      &
                            &PRR, PRI, PRIS, PRS, PRG, TBUDGETS, KBUDGETS,     &
                            &PICE_CLD_WGT,                                     &
                            &PRH,                                              &
                            &POUT_RV, POUT_RC, POUT_RI, POUT_TH,               &
                            &PHLC_HRC, PHLC_HCF, PHLI_HRI, PHLI_HCF,           &
                            &PHLC_HRC_MF, PHLC_HCF_MF, PHLI_HRI_MF, PHLI_HCF_MF)
USE MODD_BUDGET,         ONLY: TBUDGETDATA, TBUDGETCONF_t
USE MODD_CST,            ONLY: CST_t
USE MODD_RAIN_ICE_PARAM_n, ONLY: RAIN_ICE_PARAM_t
USE MODD_NEB_n,          ONLY: NEB_t
USE MODD_TURB_n,         ONLY: TURB_t
USE MODD_PARAM_ICE_n,    ONLY: PARAM_ICE_t
USE MODD_DIMPHYEX,       ONLY: DIMPHYEX_t
IMPLICIT NONE
!
!
!*       0.1   Declarations of dummy arguments :
!
!
TYPE(DIMPHYEX_t),         INTENT(IN)    :: D
TYPE(CST_t),              INTENT(IN)    :: CST
TYPE(RAIN_ICE_PARAM_t),   INTENT(IN)    :: ICEP
TYPE(NEB_t),              INTENT(IN)    :: NEBN
TYPE(TURB_t),             INTENT(IN)    :: TURBN
TYPE(PARAM_ICE_t),        INTENT(IN)    :: PARAMI
TYPE(TBUDGETCONF_t),      INTENT(IN)    :: BUCONF
INTEGER,                  INTENT(IN)    :: KRR      ! Number of moist variables
CHARACTER(LEN=4),         INTENT(IN)    :: HBUNAME  ! Name of the budget
REAL,                     INTENT(IN)   :: PTSTEP    ! Double Time step
                                                    ! (single if cold start)
REAL, DIMENSION(D%NIJT),       INTENT(IN)    :: PSIGQSAT  ! coeff applied to qsat variance contribution
!
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    ::  PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    ::  PEXNREF ! Reference Exner function
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    ::  PRHODREF
!
REAL, DIMENSION(MERGE(D%NIJT,0,NEBN%LSUBG_COND),&
                MERGE(D%NKT,0,NEBN%LSUBG_COND)),           INTENT(IN)    ::  PSIGS   ! Sigma_s at time t
LOGICAL,                                              INTENT(IN)    ::  LMFCONV ! =SIZE(PMFCONV)!=0
REAL, DIMENSION(MERGE(D%NIJT,0,LMFCONV),&
                MERGE(D%NKT,0,LMFCONV)),              INTENT(IN)   ::  PMFCONV ! convective mass flux
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    ::  PPABST  ! Absolute Pressure at t
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    ::  PZZ     ! height of model layer
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    ::  PEXN    ! Exner function
!
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PCF_MF   ! Convective Mass Flux Cloud fraction
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PRC_MF   ! Convective Mass Flux liquid mixing ratio
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PRI_MF   ! Convective Mass Flux ice mixing ratio
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PWEIGHT_MF_CLOUD ! weight coefficient for the mass-flux cloud
!
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PRV     ! Water vapor m.r. to adjust
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PRC     ! Cloud water m.r. to adjust
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)    :: PTH     ! Theta to adjust
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(INOUT) :: PTHS    ! Theta source
LOGICAL,                            INTENT(IN)    :: OCOMPUTE_SRC
REAL, DIMENSION(MERGE(D%NIJT,0,OCOMPUTE_SRC),&
                MERGE(D%NKT,0,OCOMPUTE_SRC)), INTENT(OUT)   :: PSRCS   ! Second-order flux
                                                                       ! s'rc'/2Sigma_s2 at time t+1
                                                                       ! multiplied by Lambda_3
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  ::  PCLDFR  ! Cloud fraction
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  ::  PICLDFR ! ice cloud fraction
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  ::  PWCLDFR ! water or mixed-phase cloud fraction
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  ::  PSSIO   ! Super-saturation with respect to ice in the  
                                                        ! supersaturated fraction
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  ::  PSSIU   ! Sub-saturation with respect to ice in the  
                                                        ! subsaturated fraction 
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(OUT)  ::  PIFR    ! Ratio cloud ice moist part to dry part
!
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(INOUT)::  PRIS ! Cloud ice  m.r. at t+1
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   ::  PRR  ! Rain water m.r. to adjust
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   ::  PRI  ! Cloud ice  m.r. to adjust
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   ::  PRS  ! Aggregate  m.r. to adjust
REAL, DIMENSION(D%NIJT,D%NKT), INTENT(IN)   ::  PRG  ! Graupel    m.r. to adjust
TYPE(TBUDGETDATA), DIMENSION(KBUDGETS),       INTENT(INOUT)::  TBUDGETS
INTEGER,                                      INTENT(IN)   ::  KBUDGETS
REAL, DIMENSION(D%NIJT),       OPTIONAL, INTENT(IN)   ::  PICE_CLD_WGT
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(IN)   ::  PRH  ! Hail       m.r. to adjust
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  POUT_RV ! Adjusted value
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  POUT_RC ! Adjusted value
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  POUT_RI ! Adjusted value
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  POUT_TH ! Adjusted value
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  PHLC_HRC
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  PHLC_HCF
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  PHLI_HRI
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(OUT)  ::  PHLI_HCF
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(IN)   ::  PHLC_HRC_MF
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(IN)   ::  PHLC_HCF_MF
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(IN)   ::  PHLI_HRI_MF
REAL, DIMENSION(D%NIJT,D%NKT), OPTIONAL, INTENT(IN)   ::  PHLI_HCF_MF
!
END SUBROUTINE ICE_ADJUST
!
END INTERFACE
!
END MODULE MODI_ICE_ADJUST

