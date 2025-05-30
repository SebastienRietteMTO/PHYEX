!MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!     ##################
MODULE MODE_INI_CST
IMPLICIT NONE
CONTAINS
      SUBROUTINE INI_CST 
!     ##################
!
!!****  *INI_CST * - routine to initialize the module MODD_CST
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize  the physical constants
!     stored in  module MODD_CST.
!      
!
!!**  METHOD
!!    ------
!!      The physical constants are set to their numerical values 
!!     
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST     : contains physical constants
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation (module MODD_CST, routine INI_CST)
!!      
!!
!!    AUTHOR
!!    ------
!!         V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/05/94 
!!      J. Stein    02/01/95  add the volumic mass of liquid water
!!      J.-P. Pinty 13/12/95  add the water vapor pressure over solid ice
!!      J. Stein    29/06/97  add XTH00
!!      V. Masson   05/10/98  add XRHOLI
!!      C. Mari     31/10/00  add NDAYSEC
!!      V. Masson   01/03/03  add XCONDI
!!      J. Escobar  28/03/2014 for pb with emissivity/aerosol reset XMNH_TINY=1.0e-80 in real8 case
!!      R. El Khatib 04/08/14 add pre-computed quantities
!!      P. Marguinaud 04/10/16 Port to single precision
!!      J.Escobar : 10/2017 : for real*4 , add XMNH_HUGE_12_LOG
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!      J.Escobar : 5/10/2018 : for real*4 ,higher value for XEPS_DT = 1.5e-4
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST
USE MODD_PRECISION, ONLY: MNHREAL, MNHREAL32, MNHREAL64
USE MODE_MSG,       ONLY: PRINT_MSG, NVERB_FATAL
USE YOMHOOK , ONLY : LHOOK, DR_HOOK, JPHOOK
!
IMPLICIT NONE
!  
REAL(KIND=JPHOOK) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INI_CST',0,ZHOOK_HANDLE)
CALL CST_ASSOCIATE()
!
!*       1.     FUNDAMENTAL CONSTANTS
!               ---------------------
!
XPI         = 2.*ASIN(1.)
XKARMAN     = 0.4
XLIGHTSPEED = 299792458.
XPLANCK     = 6.6260755E-34
XBOLTZ      = 1.380658E-23
XAVOGADRO   = 6.0221367E+23
!
!-------------------------------------------------------------------------------
!
!*       2.     ASTRONOMICAL CONSTANTS
!               ----------------------
!
XDAY   = 86400.
XSIYEA = 365.25*XDAY*2.*XPI/ 6.283076
XSIDAY = XDAY/(1.+XDAY/XSIYEA)
XOMEGA = 2.*XPI/XSIDAY
NDAYSEC = 24*3600 ! Number of seconds in a day
!
!-------------------------------------------------------------------------------!
!
!
!*       3.     TERRESTRIAL GEOIDE CONSTANTS
!               ----------------------------
!
XRADIUS = 6371229.
XG      = 9.80665
!
!-------------------------------------------------------------------------------
!
!*       4.     REFERENCE PRESSURE
!               -------------------
!
! Ocean model cst same as in 1D/CMO SURFEX
! values used in ini_cst to overwrite XP00 and XTH00
XRH00OCEAN =1024.
XTH00OCEAN = 286.65
XSA00OCEAN= 32.6
XP00OCEAN = 201.E5
!Atmospheric model
XP00 = 1.E5
XTH00 = 300.
!-------------------------------------------------------------------------------
!
!*       5.     RADIATION CONSTANTS
!               -------------------
!
! Original: XSTEFAN = 2.* XPI**5 * XBOLTZ**4 / (15.* XLIGHTSPEED**2 * XPLANCK**3)
! Juan: XSTEFAN = ( 2.* XPI**5 / 15. ) * ( (XBOLTZ / XPLANCK) * XBOLTZ ) * (XBOLTZ/(XLIGHTSPEED*XPLANCK))**2
! Philippe Marguinaud: XSTEFAN = REAL (2._8* REAL (XPI, 8)**5 * REAL (XBOLTZ, 8)**4 / (15._8* REAL (XLIGHTSPEED, 8)**2 * REAL (XPLANCK, 8)**3))
XSTEFAN = REAL (2._MNHREAL64* REAL (XPI, MNHREAL64)**5 * REAL (XBOLTZ, MNHREAL64)**4 / &
        & (15._MNHREAL64* REAL (XLIGHTSPEED, MNHREAL64)**2 * REAL (XPLANCK, MNHREAL64)**3))
XI0     = 1370.
!
!-------------------------------------------------------------------------------
!
!*       6.     THERMODYNAMIC CONSTANTS
!               -----------------------
!
XMD    = 28.9644E-3
XMV    = 18.0153E-3
XRD    = XAVOGADRO * XBOLTZ / XMD
XRV    = XAVOGADRO * XBOLTZ / XMV
XEPSILO= XMV/XMD
XCPD   = 7.* XRD /2.
XCPV   = 4.* XRV
XRHOLW = 1000.
XRHOLI = 900.
XCONDI = 2.22
XCL    = 4.218E+3
XCI    = 2.106E+3
XTT    = 273.16
XLVTT  = 2.5008E+6
XLSTT  = 2.8345E+6
XLMTT  = XLSTT - XLVTT
XESTT  = 611.14
XGAMW  = (XCL - XCPV) / XRV
XBETAW = (XLVTT/XRV) + (XGAMW * XTT)
XALPW  = LOG(XESTT) + (XBETAW /XTT) + (XGAMW *LOG(XTT))
XGAMI  = (XCI - XCPV) / XRV
XBETAI = (XLSTT/XRV) + (XGAMI * XTT)
XALPI  = LOG(XESTT) + (XBETAI /XTT) + (XGAMI *LOG(XTT))
! Values identical to ones used in CMO1D in SURFEX /could be modified
! Coefficient of thermal expansion of water (K-1)
XALPHAOC = 1.9E-4
! Coeff of Haline contraction coeff (S-1)
XBETAOC= 7.7475E-4
!
!*       7.     PRECOMPUTED CONSTANTS
!               ---------------------
!
RDSRV = XRD/XRV
RDSCPD = XRD/XCPD
RINVXP00 =  1./XP00
!
!*       8.     MACHINE PRECISION VALUE DEPENDING of REAL4/8 USE
!               ---------------------
!
XMNH_EPSILON = EPSILON (XMNH_EPSILON )
XMNH_HUGE    = HUGE    (XMNH_HUGE )
XMNH_HUGE_12_LOG = LOG ( SQRT(XMNH_HUGE)  )

IF (MNHREAL == MNHREAL64) THEN
XMNH_TINY      = 1.0e-80_MNHREAL
XEPS_DT        = 1.0e-5_MNHREAL
XRES_FLAT_CART = 1.0e-12_MNHREAL
XRES_OTHER     = 1.0e-9_MNHREAL
XRES_PREP      = 1.0e-8_MNHREAL
ELSEIF (MNHREAL == MNHREAL32) THEN
XMNH_TINY      = TINY    (XMNH_TINY    )
XEPS_DT        = 1.5e-4_MNHREAL
XRES_FLAT_CART = 1.0e-12_MNHREAL
XRES_OTHER     = 1.0e-7_MNHREAL
XRES_PREP      = 1.0e-4_MNHREAL
ELSE
CALL PRINT_MSG(NVERB_FATAL, 'GEN', 'INI_CST', 'Invalid MNH_REAL')
ENDIF
XMNH_TINY_12 = SQRT    (XMNH_TINY    )
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INI_CST',1,ZHOOK_HANDLE)
END SUBROUTINE INI_CST 

END MODULE MODE_INI_CST
