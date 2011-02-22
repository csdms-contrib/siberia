! 
! 
! ===================================================================|
!                                                                    |
!                    SIBERIA LICENSE AGREEMENT                       |
!                    -------------------------                       |
!                                                                    |
!  Please read the following licence information carefully. This     |
!  computer program ("SIBERIA") is licensed, not sold, to you for use|
!  only under the terms of this license, and the copyright owner     |
!  reserves any rights not expressly granted to you.  You own the    |
!  computer media on which SIBERIA is originally and subsequently    |
!  recorded or fixed, but the copyright owner retains ownership      |
!  of all copies of SIBERIA itself                                   |
!                                                                    |
!  Unless otherwise stated this licence entitles you to              |
!     (a) copy this code onto a single computer,                     |
!     (b) make backup copies of this software,                       |
!                                                                    |
!  You may not                                                       |
!     (a) remove these license agreement, disclaimer, copyright, or  |
!         limitation of damages notices from this source code,       |
!     (b) distribute this software to others,                        |
!     (c) rent,lease, resell, distribute, network, or create         |
!         derivative products works based upon this software, or     |
!         any part thereof.                                          |
!     (d) modify the software in this file without the written       |
!         permission of the copyright owner.                         |
!     (e) disclose this source code and algorithms to                |
!         unlicensed users                                           |
!                                                                    |
!  This Licence is effective until terminated. This Licence will     |
!  terminate automatically without notice from the copyright owner   |
!  If you fail to comply with any provision of this Licence. Upon    |
!  termination of this Licence you must destroy this software and    |
!  all copies thereof. You may terminate the Licence at any time     |
!  by destroying this software and any copies thereof.               |
!                                                                    |
!--------------------------------------------------------------------|
!                                                                    |
!                      COPYRIGHT NOTICE                              |
!                      ----------------                              |
!                                                                    |
!        The SIBERIA software is Copyright 2010 by                   |
!                                                                    |
!     Professor Garry Raymond Willgoose,                             |
!     School of Engineering                                          |
!     University of Newcastle, 2308 Australia                        |
!     garry.willgoose@newcastle.edu.au                               |
!                                                                    |
!    This program is free software: you can redistribute it          |
!    and/or modify it under the terms of the GNU General Public      |
!    License as published by the Free Software Foundation,           |
!    version 3 of the License.                                       |
!                                                                    |
!    This program is distributed in the hope that it will be         |
!    useful, but WITHOUT ANY WARRANTY; without even the              |
!    implied warranty of MERCHANTABILITY or FITNESS FOR A            |
!    PARTICULAR PURPOSE.  See the GNU General Public License         |
!    for more details.                                               |
!                                                                    |
!    You should have received a copy of the GNU General Public       |
!    License along with this program.  If not, see                   |
!    <http://www.gnu.org/licenses/>                                  |
!                                                                    |
!--------------------------------------------------------------------|
!                                                                    |
!                       DISCLAIMER                                   |
!                       ----------                                   |
!                                                                    |
!  SIBERIA is provided 'as is' without warranty of any kind          |
!  either express or implied, including without limitation any       |
!  warranty with respect to its merchantability, or its fitness for  |
!  any particular purpose. The entire risk as to the quality and     |
!  performance of SIBERIA is with you. Should SIBERIA                |
!  prove defective, you (and not the copyright owner), assume the    |
!  entire cost of all necessary servicing, repair or correction.     |
!                                                                    |
!  The copyright owner does not warrant that the functions contained |
!  in SIBERIA will meet your requirements or that the operation      |
!  of SIBERIA will be uninterrrupted or error free or that defects   |
!  in SIBERIA will be corrected                                      |
!                                                                    |
!--------------------------------------------------------------------|
!                                                                    |
!                LIMITATION OF DAMAGES                               |
!                ---------------------                               |
!                                                                    |
!  In no event will the copyright owner be liable (i) to you for any |
!  incidental, consequential or indirect damages (including damages  |
!  for loss of business profits, business interruption, loss of      |
!  business information, and the like) arising out of the use of or  |
!  inability to use SIBERIA even if the copyright owner has been     |
!  advised of the possibility of such damages, or (ii) for any       |
!  claim by any other party.                                         |
!                                                                    |
! ====================================================================
!
MODULE SedAnalysis
  USE SiberiaTypes 

!  public variable
  REAL, PARAMETER :: SedAnalysis_Version=8.27

  INTERFACE Sed_Get
    MODULE PROCEDURE SedGetI
    MODULE PROCEDURE SedGetR
    MODULE PROCEDURE SedGetR8
    MODULE PROCEDURE SedGetL
    MODULE PROCEDURE SedGetStr
    MODULE PROCEDURE SedGetArrayI
    MODULE PROCEDURE SedGetArrayR
    MODULE PROCEDURE SedGetArrayR8
  END INTERFACE Sed_Get
!  Variables for inquiry routines
  INTEGER,PARAMETER :: Sed_ErrorInfo=1

  INTEGER,PARAMETER :: Sed_PotentialQs=101
  INTEGER,PARAMETER :: Sed_ActualQs=102

!  private variables
  integer :: LastError
  TYPE(ArrayR8XY),PRIVATE :: PotentialQs,ActualQs

  type ErrorInfo
    integer :: ErrorNo
    character(30) :: ErrorText
  end type ErrorInfo
  CHARACTER(20),parameter,private :: GenericErrorHeader='MODULE SedAnalysis: '
  integer,parameter,private :: MaxNoErrors=5
  type(ErrorInfo),dimension(MaxNoErrors),parameter,private :: ErrorData=          &
&           (/ ErrorInfo( 0,'No error                      '),                    &
&              ErrorInfo( 1,'Invalid inquiry option        '),                    &
&              ErrorInfo(11,'Array bound out of range      '),                    &
&              ErrorInfo(12,'Array not allocated           '),                    & 
&              ErrorInfo(-1,'Generic unidentified error    ')/)   
! generic error should always be the last error (failsafe for error text return)

CONTAINS

! ============================================================================
! ============================================================================
!                       SED Inquiry Routines
! ============================================================================
! ============================================================================


  SUBROUTINE SedGetI(Variable,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable
      INTEGER,intent(out) :: value
      INTEGER,intent(out) :: Error

      Error=-1
      SELECT CASE (variable)
      CASE DEFAULT
!  Invalid Option
        Error=1
        value=0
      END SELECT
      LastError=Error
  END SUBROUTINE SedGetI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetR(Variable,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable
      REAL,intent(out) :: value
      INTEGER,intent(out) :: Error

      REAL(KIND(0.0D0)) :: dvalue

      Error=-1
      call SedGetR8(Variable,dvalue,Error)
      value=dvalue
      LastError=Error
  END SUBROUTINE SedGetR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetR8(Variable,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable
      REAL(KIND(0.0D0)),intent(out) :: value
      INTEGER,intent(out) :: Error

      Error=-1
      SELECT CASE (variable)
      CASE DEFAULT
!  Invalid Option
        Error=1
        value=0
      END SELECT
      LastError=Error
  END SUBROUTINE SedGetR8
! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetL(Variable,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable
      LOGICAL,intent(out) :: value
      INTEGER,intent(out) :: Error

      Error=-1
      SELECT CASE (variable)
      CASE DEFAULT
!  Invalid Option
        Error=1
        value=.false.
      END SELECT
      LastError=Error
  END SUBROUTINE SedGetL

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetStr(Variable,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable
      CHARACTER(*),intent(out) :: Value
      INTEGER,intent(out) :: Error

      INTEGER :: i

      Error=-1
      SELECT CASE (variable)
      CASE DEFAULT
!  Invalid Option
        Error=1
        value=''
      CASE(Sed_ErrorInfo)
        do i=1,MaxNoErrors
          if (LastError == ErrorData(i)%ErrorNo) exit
        end do
        value=GenericErrorHeader//ErrorData(i)%ErrorText
        Error=0
      END SELECT
      LastError=Error
  END SUBROUTINE SedGetStr

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetArrayI(Variable,i,j,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable,i,j
      INTEGER,intent(out) :: value
      INTEGER,intent(out) :: Error

      Error=-1
      SELECT CASE (variable)
      CASE DEFAULT
!  Invalid Option
        Error=1
        value=0
      END SELECT
      LastError=Error
  END SUBROUTINE SedGetArrayI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetArrayR(Variable,i,j,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable,i,j
      REAL,Intent(out) :: value
      INTEGER,intent(out) :: Error
      
      REAL(KIND(0.0D0)) :: dvalue

      Error=-1
      CALL SedGetArrayR8(Variable,i,j,dvalue,Error)
      value=dvalue
      LastError=Error
  END SUBROUTINE SedGetArrayR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE SedGetArrayR8(Variable,i,j,value,Error)
    USE Support
    IMPLICIT NONE
      INTEGER,intent(in) :: Variable,i,j
      REAL(KIND(0.0D0)),intent(out) :: value
      INTEGER,intent(out) :: Error

      Error=-1
      SELECT CASE (variable)
      CASE DEFAULT
!  Invalid Option
        value=0
        Error=1
      case(Sed_PotentialQs)
        value=PotentialQs%data(i,j)
        Error=0
      case(Sed_ActualQs)
        value=ActualQs%data(i,j)
        Error=0
      END SELECT
      LastError=Error
  END SUBROUTINE SedGetArrayR8


! =====================================================================
! =====================================================================
!             SEDIMENT TRANSPORT ANALYSIS ROUTINES
! =====================================================================
! =====================================================================
!   Routine TO analysis the areas and flow directions for the sediment
!   transport balance. The output is the adjustment in the Elevation
!   at the next time step. This routine assumes that the time scale
!   of sediment transport balance is much smaller than the time scale
!   of catchment Elevation equilibrium. Thus the new Elevation is
!   the equilibrium of the sediment transport balance
! =====================================================================
! =====================================================================

SUBROUTINE SedAnal(Sed,Direct,DirectDinf,DirWeights                      &
&          ,Area,Z,Zoriginal,Y,s0,Dsed,gridX,gridY,iTime,init,iXXX,iYYY  &
&          ,vz,DirChg,TimeStep,dZdX2,Zout,Zin,Domain                     &
&          ,IrregularBoundary,DetCIF,tottime,SumCycle                    &
&          ,cDepth,Region,RegionMap,NoRegions,sumsed                     &
&          ,TwoFluvialProcesses,LowX,HighX,LowY,HighY                    &
&          ,NoFlowIn,FlowInIJ,FlowInAS,Factor,AreaTerm1                  &
&          ,SlopeMax,Erode_m1,Erode_n1,Discharge,SimParameters,Erosion   &
&          ,SedimentTracking,FlowB1,FlowAge,Uplift,PotErosion)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Support
  USE MyModels
  USE UserUpliftAnalysis
  USE UserRunoffAnalysis
  USE UserErosionAnalysis
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(*),iYYY(*),iTime,RegionMap(*),gridX,gridY              &
&          ,NoRegions,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ(2,*)
    INTEGER,DIMENSION(GridX,GridY) :: Direct,Region,DirectDinf
    REAL(KIND(0.0D0)) :: TimeStep,Zout,Zin,SumCycle,tottime                     &
&          ,sumsed(*),FlowInAS(2,*)
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Z,Zoriginal,DirWeights          &
&          ,Sed,Y,cDepth,dZdX2,Dsed,s0,vz,Factor,AreaTerm1,Area                 &
&          ,Erode_m1,Erode_n1,Discharge,Uplift
    TYPE(ArrayR8XY) :: Erosion,FlowB1,FlowAge,PotErosion
    LOGICAL :: DirChg,Domain(GridX,GridY),SlopeMax,IrregularBoundary     &
&          ,DetCIF,TwoFluvialProcesses,SedimentTracking
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: modeSS,ErrorNo
    REAL(KIND(0.0D0)) :: QsHoldT
!
!      CALL TimerShow(' begin sedanal')
      IF (FirstSedAnal) THEN
        IF (SimParameters%ModeSolver == 5) THEN
          CALL Message_Output(Message_WarnContinue,                   &
&                    'ModeSolver=5 no longer supported ... '          &
&                  //'setting ModeSolver=4')
          SimParameters%ModeSolver=4
        END IF
        CALL AllocateArray(PotentialQs,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError                        &
&                      ('PotentialQs','SIBERIA_SEDANAL')
        call AllocateArray(ActualQs,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError                        &
&                      ('ActualQs','SIBERIA_SEDANAL')
      END IF
      PotentialQs%data=0
      ActualQs%data=0
      IF (SimParameters%ModeUplift >= 0) THEN
        CALL MyUplift(tottime,timestep,iTime,Z,s0,GridX,GridY         &
&                    ,Uplift,SumCycle,LowX,HighX,LowY,HighY           &
&                    ,SimParameters)
      ELSE
        CALL UserUplift(tottime,timestep,iTime,Z,s0,GridX,GridY       &
&                    ,Uplift,SumCycle,LowX,HighX,LowY,HighY           &
&                    ,SimParameters)
      END IF
!      CALL TimerShow(' after uplift')
      IF (SimParameters%ModeRunoff >= 0) THEN
        CALL MyRunoff(AreaTerm1,SimParameters%b1                      &
&            ,SimParameters%m1,DetCIF,IrregularBoundary               &
&            ,Domain,DirChg,vz,s0,Y,Z,Area,Direct,GridX,GridY         &
&            ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS        &
&            ,DirectDinf,DirWeights,Discharge,SimParameters,tottime             &
&             ,timestep)
      ELSE
        CALL UserRunoff(AreaTerm1,SimParameters%b1                              &
&            ,SimParameters%m1,DetCIF,IrregularBoundary                         &
&            ,Domain,DirChg,vz,s0,Y,Z,Area,Direct                               &
&            ,GridX,GridY,LowX,HighX,LowY,HighY,SimParameters)
      END IF
      IF (TwoFluvialProcesses) THEN
        IF (SimParameters%ModeRunoff >= 0) THEN
          IF (FirstSedAnal) THEN
            ALLOCATE(AreaTerm2SedAnal(1:GridX,1:GridY),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError                              &
&                      ('AreaTerm2SedAnal','SIBERIA_SEDANAL')
          END IF
          CALL MyRunoff(AreaTerm2SedAnal,SimParameters%b12                      &
&            ,SimParameters%m12,DetCIF,IrregularBoundary                        &
&            ,Domain,DirChg,vz,s0,Y,Z,Area,Direct,GridX,GridY                   &
&            ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                  &
&            ,DirectDinf,DirWeights,Discharge,SimParameters,tottime             &
&             ,timestep)
        ELSE
          CALL UserRunoff(AreaTerm2SedAnal,SimParameters%b12                    &
&            ,SimParameters%m12,DetCIF,IrregularBoundary                        &
&            ,Domain,DirChg,vz,s0,Y,Z,Area,Direct                               &
&            ,GridX,GridY,LowX,HighX,LowY,HighY,SimParameters)
        END IF
      END IF
!      CALL TimerShow(' after runoff')
      Zin=0
      Zout=0
      QsHoldT=SimParameters%QsHold*TimeStep
      IF (SimParameters%ModeErode >= 0) THEN
        CALL MyErosion(Factor,Erode_m1,Erode_n1                        &
&         ,DetCIF,IrregularBoundary,Domain,DirChg                     &
&         ,vz,s0,Y,Z,Zoriginal,Area,Direct                            &
&         ,GridX,GridY,LowX,HighX,LowY,HighY,tottime,SimParameters)
      ELSE
        CALL UserErosion(Factor,Erode_m1,Erode_n1                      &
&         ,DetCIF,IrregularBoundary,Domain,DirChg                     &
&         ,vz,s0,Y,Z,Zoriginal,Area,Direct                            &
&         ,GridX,GridY,LowX,HighX,LowY,HighY,tottime,SimParameters)
      END IF
!      CALL TimerShow(' after factor')
      CALL DiffTransport(IrregularBoundary,Domain,dZdX2               &
&         ,Direct,DirectDinf,DirWeights                               &
&         ,Area,Z,s0,timestep                                         &
&         ,LowX,HighX,LowY,HighY,SlopeMax,SimParameters,gridX,gridY)
!      CALL TimerShow(' after diff')
! 
! ======================================================================
! ======================================================================
! |||||||||||||||       Main Computation Loop        |||||||||||||||||||
! ======================================================================
! ======================================================================
! 
      IF (SimParameters%ModeSolver == 0) THEN
        ModeSS=4
      ELSE
        ModeSS=SimParameters%ModeSolver
      END IF
      SELECT CASE (modeSS)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorStop,' ModeSolver = '         &
&               ,SimParameters%ModeSolver,' not implemented')
! 
! ===========================================================
!                          ModeSolver=1
!        Raw Eulers method method, transport limitation
! ===========================================================
! 
     CASE(1)
       IF (SimParameters%ModeDir == 7                                  &
&          .or.SimParameters%ModeDir == 8) THEN
         CALL Message_Output(Message_ErrorStop,' ModeSolver = '        &
&          ,SimParameters%ModeSolver,' not implemented for Dinfinity')
       END IF
       IF (SimParameters%ModeErode == 3) THEN
         CALL Message_Output(Message_ErrorStop,' ModeSolver = '        &
&           ,SimParameters%ModeSolver                                  &
&           ,' not implemented for ModeErode=3')
       END IF
       CALL SedAnal1(Sed,s0,factor,AreaTerm1,AreaTerm2SedAnal,dZdX2    &
&            ,Uplift,Direct,QsHoldT,IrregularBoundary                  &
&            ,Domain,TwoFluvialProcesses,init,iXXX,iYYY                &
&            ,zout,zin,TimeStep,LowX,HighX,LowY,HighY,SimParameters    &
&            ,gridX,gridY)
! 
! ===========================================================
!                          ModeSolver=3
!    HYBRID Physical/Equilibrium method, transport limitation
!            Explicit handling of Area=1 nodes
! ===========================================================
! 
     CASE(3)
       IF (SimParameters%ModeDir == 7                                  &
&          .or.SimParameters%ModeDir == 8) THEN
         CALL Message_Output(Message_ErrorStop,' ModeSolver = '        &
&          ,SimParameters%ModeSolver                                   &
&          ,' not implemented for Dinfinity')
       END IF
       IF (SimParameters%ModeErode == 3) THEN
         CALL Message_Output(Message_ErrorStop,' ModeSolver = '        &
&           ,SimParameters%ModeSolver                                  &
&           ,' not implemented for ModeErode=3')
       END IF
       CALL SedAnal3(Sed,Dsed,s0,AreaTerm1,AreaTerm2SedAnal,factor     &
&           ,dZdX2,Uplift,Direct,Area                                  &
&           ,IrregularBoundary,Domain,TwoFluvialProcesses              &
&           ,QsHoldT,Zout,Zin,init,iXXX,iYYY,TimeStep                  &
&           ,NoRegions,RegionMap,Region,SumSed                         &
&           ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
! 
! ===========================================================
!                          ModeSolver=4
!              HYBRID Physical/Equilibrium method
!    Two processes hillslope-Channel, transport limitation
!           Explicit handling of Area=1 nodes
! ===========================================================
! 
     CASE(4,14)
       IF (.not.(SimParameters%ModeDir == 7                             &
&               .or.SimParameters%ModeDir == 8)) THEN
         CALL SedAnal4(Sed,Dsed,s0,AreaTerm1,AreaTerm2SedAnal,factor    &
&           ,dZdX2,Uplift,Y,z,Direct,Area                               &
&           ,IrregularBoundary,Domain,TwoFluvialProcesses               &
&           ,QsHoldT,Zout,Zin,init,iXXX,iYYY,TimeStep                   &
&           ,NoRegions,RegionMap,Region,SumSed,DetCIF                   &
&           ,LowX,HighX,LowY,HighY,Erode_m1,Erode_n1,Discharge          &
&           ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge      &
&           ,gridX,gridY,PotErosion)
        ELSE
          CALL SedAnalDinf4(Sed,Dsed,s0,AreaTerm1,AreaTerm2SedAnal      &
&           ,factor,dZdX2,Uplift,Y,z,Direct,DirectDinf,DirWeights       &
&           ,Area,IrregularBoundary,Domain,TwoFluvialProcesses          &
&           ,QsHoldT,Zout,Zin,init,iXXX,iYYY,TimeStep                   &
&           ,NoRegions,RegionMap,Region,SumSed,DetCIF                   &
&           ,LowX,HighX,LowY,HighY,SimParameters,Erosion,gridX,gridY)
        END IF
! 
! ===========================================================
!                          ModeSolver=5
!              Detachment and Transport Limitation
! ===========================================================
!
     CASE(5) 
        CALL Message_Output(Message_ErrorStop,' ModeSolver = '         &
&               ,SimParameters%ModeSolver,' has been superseded by'    &
&                //'ModeSolver = 4')
! 
! ===================================================================
!                           ModeSolver = 6 
!            Shear Stress driven source limitation
! ===================================================================
! 
     CASE(6)
       IF (SimParameters%ModeDir == 7                                    &
&          .or.SimParameters%ModeDir == 8) THEN
         CALL Message_Output(Message_ErrorStop,' ModeSolver = '          &
&           ,SimParameters%ModeSolver                                    &
&           ,' not implemented for Dinfinity')
       END IF
       IF (SimParameters%ModeErode == 3) THEN
         CALL Message_Output(Message_ErrorStop                           &
&           ,' ModeSolver = ',SimParameters%ModeSolver                   &
&           ,' not implemented for ModeErode=3')
       END IF
       CALL SedAnal6(s0,Sed,AreaTerm1,AreaTerm2SedAnal,Uplift,factor     &
&         ,IrregularBoundary,Domain,TwoFluvialProcesses                  &
&         ,init,iXXX,iYYY,TimeStep,LowX,HighX,LowY,HighY,SimParameters   &
&         ,gridX,gridY)
! 
! ===========================================================
!                ModeSolver=7 (modified from ModeSolver=4)
!    Two processes diffusive-Channel with no erosion on hillslopes
! ===========================================================
! 
     CASE(7)
       IF (SimParameters%ModeErode == 3) THEN
         CALL Message_Output(Message_ErrorStop,' ModeSolver = '          &
&           ,SimParameters%ModeSolver                                    &
&           ,' not implemented for ModeErode=3')
       END IF
       IF (SimParameters%ModeDir == 7                                    &
&          .or.SimParameters%ModeDir == 8) THEN
         CALL SedAnalDinf7(Sed,Dsed,s0,AreaTerm1,AreaTerm2SedAnal        &
&           ,factor,dZdX2,Uplift,Y,z,Direct,DirectDinf,DirWeights        &
&           ,Area,IrregularBoundary,Domain,TwoFluvialProcesses           &
&           ,QsHoldT,Zout,Zin,init,iXXX,iYYY,TimeStep                    &
&           ,NoRegions,RegionMap,Region,SumSed,DetCIF                    &
&           ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
       ELSE
         CALL SedAnal7(Sed,Dsed,s0,AreaTerm1,AreaTerm2SedAnal,factor     &
&           ,dZdX2,Uplift,Y,z,Direct,Area                                &
&           ,IrregularBoundary,Domain,TwoFluvialProcesses                &
&           ,QsHoldT,Zout,Zin,init,iXXX,iYYY,TimeStep                    &
&           ,NoRegions,RegionMap,Region,SumSed,DetCIF                    &
&           ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
       END IF
! 
! ===========================================================
!                          ModeSolver=8
!              Simple Detachment and Entrainment Model
! ===========================================================
! 
     CASE(8)
         CALL SedAnal8(Sed,Dsed,s0,AreaTerm1,AreaTerm2SedAnal,factor    &
&           ,dZdX2,Uplift,Y,z,Direct,Area                               &
&           ,IrregularBoundary,Domain,TwoFluvialProcesses               &
&           ,QsHoldT,Zout,Zin,init,iXXX,iYYY,TimeStep                   &
&           ,NoRegions,RegionMap,Region,SumSed,DetCIF                   &
&           ,LowX,HighX,LowY,HighY,Erode_m1,Erode_n1,Discharge          &
&           ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge      &
&           ,gridX,gridY)
      END SELECT
! 
 7001 CONTINUE
!      CALL TimerShow(' end sedanal')
      FirstSedAnal=.false.
      RETURN
END SUBROUTINE sedanal

! 
! ===================================================================
!            DIFFUSIVE TRANSPORT
! ===================================================================
! 


SUBROUTINE DiffTransport(IrregularBoundary,Domain,dZdX2                &
&            ,Direct,DirectDinf,DirWeights,Area,Z,s0,timestep          &
&            ,LowX,HighX,LowY,HighY,SlopeMax,SimParameters,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(GridX,GridY) :: Direct,DirectDinf
    REAL(KIND(0.0D0)) :: timestep
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: dZdX2,s0,Z,Area,DirWeights
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY),SlopeMax
    TYPE(LocalParameters) :: SimParameters
! 
    REAL(KIND(0.0D0)) :: relax,SlopeDiff
    PARAMETER (relax=0.1, SlopeDiff=0.001)
! 
    INTEGER :: i,j,nxti,nxtj,nxti2,nxtj2,ModeSolver,ModeDir
    REAL(KIND(0.0D0)) :: dzdn1,temp,dz,dzn,b3,b5,m3,m5,n5,c1,s0max
    REAL(KIND(0.0D0)) :: work(1:GridX,1:GridY)
    LOGICAL :: Channel
! 
      ModeDir=SimParameters%ModeDir
      ModeSolver=SimParameters%ModeSolver
      dz=SimParameters%dZ
      dzn=SimParameters%dZn
      b3=SimParameters%b3
      b5=SimParameters%b5
      m3=SimParameters%m3
      m5=SimParameters%m5
      n5=SimParameters%n5
      c1=SimParameters%c1
      s0max=SimParameters%s0max
      DO j=LowY,HighY
        DO i=LowX,HighX
          dZdX2(i,j)=0
          work(i,j)=0
        END DO
      END DO
      IF (dZ == 0.0) RETURN
! 
!  Calculate Diffusive Transport
! ===============================
! 
      IF (IrregularBoundary) THEN
! ----------------------------------------------------------
!  Diffusion and areas for irregular boundaries
! ----------------------------------------------------------
        IF (dZn == 0.0) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                work(i,j)=dZ
              END IF
            END DO
          END DO
        ELSE
! 
!  linear diffusion
! 
          IF (dZn == 1.0) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  IF (ModeSolver == 7) THEN
                    IF (b5*(b3*Area(i,j)**m3)**m5*s0(i,j)**n5*c1               &
&                        > 0.005) THEN
                      Channel=.true.
                    ELSE
                      Channel=.false.
                    END IF
                    IF (.not.Channel) THEN
                      work(i,j)=dZ*s0(i,j)
                    END IF
                  ELSE
                    IF (SlopeMax) THEN
                      IF ((s0max-s0(i,j)) > SlopeDiff) THEN
                        temp=dZ*s0(i,j)*s0max/(s0max-s0(i,j))
                      ELSE
                        temp=dZ*s0(i,j)/SlopeDiff
                      END IF
                    ELSE
                      temp=dZ*s0(i,j)
                    END IF
                    work(i,j)=temp
                  END IF
                END IF
              END DO
            END DO
          ELSE
! 
!  general nonlinear diffusion
! 
            dZdn1=dZn
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  work(i,j)=dZ*s0(i,j)**dZdn1
                END IF
              END DO
            END DO
          END IF
        END IF
      ELSE
! ----------------------------------------------------------
!  Diffusion and areas for Regular boundaries
! ----------------------------------------------------------
        IF (dZ /= 0.0) THEN
          IF (dZn == 0.0) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                work(i,j)=dZ
              END DO
            END DO
          ELSE
! 
!  linear diffusion
! 
            IF (dZn == 1.0) THEN
              DO j=LowY,HighY
                DO i=LowX,HighX
                  IF (ModeSolver == 7) THEN
                    IF (b5*(b3*Area(i,j)**m3)**m5*s0(i,j)**n5*c1               &
&                        > 0.005) THEN
                      Channel=.true.
                    ELSE
                      Channel=.false.
                    END IF
                    IF (.not.Channel) THEN
                      work(i,j)=dZ*s0(i,j)
                    END IF
                  ELSE
                    IF (SlopeMax) THEN
                      IF ((s0max-s0(i,j)) > SlopeDiff) THEN
                        temp=dZ*s0(i,j)*s0max/(s0max-s0(i,j))
                      ELSE
                        temp=dZ*s0(i,j)/SlopeDiff
                      END IF
                    ELSE
                      temp=dZ*s0(i,j)
                    END IF
                    work(i,j)=temp
                  END IF
                END DO
              END DO
            ELSE
! 
!  general nonlinear diffusion
! 
              dZdn1=dZn
              DO j=LowY,HighY
                DO i=LowX,HighX
                  work(i,j)=dZ*s0(i,j)**dZdn1
                END DO
              END DO
            END IF
          END IF
        END IF
      END IF
! 
!  ALLOCATE Diffusive Transport
! ==============================
! 
      IF (ModeDir == 7.or.ModeDir == 8) THEN
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                nxti=i+dir1(Direct(i,j))
                nxtj=j+dir2(Direct(i,j))
                dZdX2(i,j)=dZdX2(i,j)-work(i,j)
                IF (DirectDinf(i,j) == 0) THEN
                  dZdX2(nxti,nxtj)=dZdX2(nxti,nxtj)+work(i,j)
                ELSE
!  main direction
                  dZdX2(nxti,nxtj)=dZdX2(nxti,nxtj)+(1-DirWeights(i,j))*work(i,j)
!  subsidary direction
                  nxti2=i+dir1(DirectDinf(i,j))
                  nxtj2=j+dir2(DirectDinf(i,j))
                  dZdX2(nxti2,nxtj2)=dZdX2(nxti2,nxtj2)+DirWeights(i,j)*work(i,j)
                END IF
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              nxti=i+dir1(Direct(i,j))
              nxtj=j+dir2(Direct(i,j))
              dZdX2(i,j)=dZdX2(i,j)-work(i,j)
              IF (DirectDinf(i,j) == 0) THEN
                dZdX2(nxti,nxtj)=dZdX2(nxti,nxtj)+work(i,j)
              ELSE
!  main direction
                dZdX2(nxti,nxtj)=dZdX2(nxti,nxtj)+(1-DirWeights(i,j))*work(i,j)
!  subsidary direction
                nxti2=i+dir1(DirectDinf(i,j))
                nxtj2=j+dir2(DirectDinf(i,j))
                dZdX2(nxti2,nxtj2)=dZdX2(nxti2,nxtj2)+DirWeights(i,j)*work(i,j)
              END IF
            END DO
          END DO
        END IF
      ELSE
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (domain(i,j)) THEN
                nxti=i+dir1(Direct(i,j))
                nxtj=j+dir2(Direct(i,j))
                dZdX2(nxti,nxtj)=dZdX2(nxti,nxtj)+work(i,j)
                dZdX2(i,j)=dZdX2(i,j)-work(i,j)
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              nxti=i+dir1(Direct(i,j))
              nxtj=j+dir2(Direct(i,j))
              dZdX2(nxti,nxtj)=dZdX2(nxti,nxtj)+work(i,j)
              dZdX2(i,j)=dZdX2(i,j)-work(i,j)
            END DO
          END DO
        END IF
      END IF
! 
!  Cleanup
! =========
! 
      DO j=LowY,HighY
        DO i=LowX,HighX
          dZdX2(i,j)=dZdX2(i,j)*timestep
        END DO
      END DO
      RETURN
 END SUBROUTINE DiffTransport

! 
! ===================================================================
!                          SEDANAL1
! ===================================================================
!
! 
! ========================================================================
!                           ModeSolver = 1
!                SOLVING FOR THE PHYSICAL PROBLEM (mass conserving)
! ========================================================================
! 
SUBROUTINE SedAnal1(Sed,s0,factor,AreaTerm1,AreaTerm2                    &
&            ,dZdX2,Uplift,Direct,QsHoldT                                &
&            ,IrregularBoundary,Domain,TwoFluvialProcesses               &
&            ,init,iXXX,iYYY,zout,zin,TimeStep                           &
&            ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
  USE SiberiaConstants
  USE Support
  USE SiberiaTypes
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),gridX,gridY,Direct(GridX,GridY)  &
&             ,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Sed,s0,factor,AreaTerm1,AreaTerm2    &
&         ,dZdX2,Uplift
    REAL(KIND(0.0D0)) :: zout,zin,QsHoldT,TimeStep,b12b1
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY),TwoFluvialProcesses
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,ix,iy,nxti,nxtj
    REAL(KIND(0.0D0)) :: Slope,st1(1:GridX)
! 
      IF (IrregularBoundary.and.FirstSedAnal1) THEN
        CALL Message_Output(Message_WarnContinue,                          &
&              ' ModeSolver=1 untested for Irregular Boundaries')
        FirstSedAnal1=.false.
      END IF
! 
! ========================================================================
!                           ModeSolver = 1
!                SOLVING FOR THE PHYSICAL PROBLEM (mass conserving)
! ========================================================================
! 
        IF (TwoFluvialProcesses) THEN
          b12b1=SimParameters%b12/SimParameters%b1
        END IF
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            Sed(ix,iy)=0.0
          END DO
        END DO
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
        IF (IrregularBoundary) THEN
          DO iy=LowY,HighY
! 
            IF (TwoFluvialProcesses) THEN
              DO ix=LowX,HighX
                IF (Domain(ix,iy)) THEN
                  Slope=s0(ix,iy)**SimParameters%n1*factor(ix,iy)
                  st1(ix)=(AreaTerm1(ix,iy)+b12b1*AreaTerm2(ix,iy))         &
&                    *Slope-QsHoldT
                  st1(ix)=max(0.0d0,dble(st1(ix)))
                  Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                END IF
              END DO
            ELSE    ! TwoFluvialProcesses
              DO ix=LowX,HighX
                IF (Domain(ix,iy)) THEN
                  Slope=s0(ix,iy)**SimParameters%n1
                  st1(ix)=AreaTerm1(ix,iy)*factor(ix,iy)*Slope-QsHoldT
                  st1(ix)=max(0.0d0,dble(st1(ix)))
                  Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                END IF
              END DO
            END IF     ! TwoFluvialProcesses
            DO ix=LowX,HighX
              IF (Domain(ix,iy)) THEN
                nxti=ix+dir1(Direct(ix,iy))
                nxtj=iy+dir2(Direct(ix,iy))
                Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
              END IF
            END DO
! 
          END DO       ! iy=2,ky
!            
!   Mass Balance
! 
          DO i=1,init
            Zout=Zout+Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep               &
&             +dZdX2(abs(iXXX(i)),abs(iYYY(i)))
            Sed(abs(iXXX(i)),abs(iYYY(i)))=0
          END DO
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
          DO iy=LowY,HighY
            DO ix=LowX,HighX
              IF (Domain(ix,iy)) THEN
                Sed(ix,iy)=Sed(ix,iy)*TimeStep/SimParameters%Bulk
! 
!   Diffusion term
! 
                IF (SimParameters%dZ /= 0.0) THEN
                  Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
                END IF
                Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
                Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
              END IF
            END DO
          END DO
        ELSE      !  RegularBoundary
          DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (TwoFluvialProcesses) THEN
              DO ix=LowX,HighX
                Slope=s0(ix,iy)**SimParameters%n1*factor(ix,iy)
                st1(ix)=(AreaTerm1(ix,iy)+b12b1*AreaTerm2(ix,iy))           &
&                  *Slope-QsHoldT
                st1(ix)=max(0.0d0,dble(st1(ix)))
                Sed(ix,iy)=Sed(ix,iy)-st1(ix)
              END DO
            ELSE     !  One FluvialProcesses
              DO ix=LowX,HighX
                Slope=s0(ix,iy)**SimParameters%n1
                st1(ix)=AreaTerm1(ix,iy)*factor(ix,iy)*Slope-QsHoldT
                st1(ix)=max(0.0d0,dble(st1(ix)))
                Sed(ix,iy)=Sed(ix,iy)-st1(ix)
              END DO
            END IF    ! TwoFluvialProcesses
            DO ix=LowX,HighX
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
            END DO
          END DO      ! iy=2,ky
!            
!   Mass Balance
! 
          DO i=1,init
            Zout=Zout+Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep               &
&             +dZdX2(abs(iXXX(i)),abs(iYYY(i)))
            Sed(abs(iXXX(i)),abs(iYYY(i)))=0
          END DO
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
          DO iy=LowY,HighY
            DO ix=LowX,HighX
              Sed(ix,iy)=Sed(ix,iy)*TimeStep/SimParameters%Bulk
! 
!   Diffusion term
! 
              IF (SimParameters%dZ /= 0.0) THEN
                Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
              END IF
              Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
              Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
            END DO
          END DO
        END IF     !  IrregularBoundary
        DO i=1,init
          Sed(abs(iXXX(i)),abs(iYYY(i)))=0
        END DO              
      RETURN
END SUBROUTINE sedanal1
      

! 
! ===================================================================
!                          SEDANAL3
! ===================================================================
!
! 
!  MOdeSolver=3: Equivalent TO ModeSolver=5 except that both the channel and
!                hillslope erosion processes occur at the same time rather
!                than channle processes occurring only in channels and
!                hillslope processes occurring only on hillslopes
! ---------------------------------------------------------------------
! 
!
! 
SUBROUTINE SedAnal3(Sed,Dsed,s0,AreaTerm1,AreaTerm2,factor                  &
&           ,dZdX2,Uplift,Direct,Area,IrregularBoundary                     &
&           ,Domain,TwoFluvialProcesses,QsHoldT,Zout,Zin,init               &
&           ,iXXX,iYYY,TimeStep,NoRegions                                   &
&           ,RegionMap,Region,SumSed,LowX,HighX,LowY,HighY,SimParameters    &
&           ,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),NoRegions,RegionMap(*)            &
&        ,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct,Region
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: Sed,Dsed,s0,AreaTerm1,AreaTerm2       &
&        ,Area,factor,dZdX2,Uplift
    REAL(KIND(0.0D0)) :: QsHoldT,Zout,Zin,TimeStep,SumSed(*),b12b1
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),TwoFluvialProcesses
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ix,iy,nxti,nxtj
    REAL(KIND(0.0D0)) :: st1(1:gridX),sdt1(1:gridX),Slope,slopen1,temp
! 
      IF (TwoFluvialProcesses) THEN
        b12b1=SimParameters%b12/SimParameters%b1
      END IF
      DO j=LowY,HighY
        DO i=LowX,HighX
          Sed(i,j)=0.0
          Dsed(i,j)=0.0
        END DO
      END DO
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
!               Irregular Boundary
! 
      IF (IrregularBoundary) THEN
          DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (TwoFluvialProcesses) THEN
              DO ix=LowX,HighX
                IF (Domain(ix,iy)) THEN
                  slopen1=s0(ix,iy)**SimParameters%n1
                  Slope=s0(ix,iy)
                  st1(ix)=(AreaTerm1(ix,iy)+b12b1*AreaTerm2(ix,iy))               &
&                   *slopen1
                  IF (SimParameters%n1 /= 1.0) THEN 
                    IF (Slope /= 0.0) THEN
                      sdt1(ix)=                &
&                       SimParameters%n1*invlgth(invdir(Direct(ix,iy)))          &
&                          *st1(ix)/Slope
                    ELSE
                      sdt1(ix)=0
                    END IF
                  ELSE
                     sdt1(ix)=               &
&                      SimParameters%n1*invlgth(invdir(Direct(ix,iy)))           &
&                        *(AreaTerm1(ix,iy)+b12b1*AreaTerm2(ix,iy))
                  END IF
                  st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                  st1(ix)=max(0.0d0,dble(st1(ix)))
                  sdt1(ix)=sdt1(ix)*factor(ix,iy)
                  Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                  Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
               END IF
              END DO
            ELSE       ! TwoFluvialProcesses
              DO ix=LowX,HighX
                IF (Domain(ix,iy)) THEN
                  Slope=s0(ix,iy)
                  st1(ix)=AreaTerm1(ix,iy)*Slope**SimParameters%n1
                  IF (SimParameters%n1 /= 1.0) THEN
                    IF (Slope /= 0.0) THEN
                      sdt1(ix)=                                                   &
&                       SimParameters%n1*invlgth(invdir(Direct(ix,iy)))           &
&                         *st1(ix)/Slope
                    ELSE
                      sdt1(ix)=0
                    END IF
                  ELSE
                     sdt1(ix)=                                                    &
&                      SimParameters%n1*invlgth(invdir(Direct(ix,iy)))            &
&                       *AreaTerm1(ix,iy)
                  END IF
                  st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                  st1(ix)=max(0.0d0,dble(st1(ix)))
                  sdt1(ix)=sdt1(ix)*factor(ix,iy)
                  Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                  Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
               END IF
              END DO
            END IF     ! TwoFluvialProcesses
            DO ix=LowX,HighX
              IF (Domain(ix,iy)) THEN
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
              Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
              END IF
            END DO
          END DO       ! iy=2,ky
      ELSE
! 
!               Regular Boundary
! 
          DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (TwoFluvialProcesses) THEN
              DO ix=LowX,HighX
                Slope=s0(ix,iy)
                slopen1=s0(ix,iy)**SimParameters%n1
                st1(ix)=                                                    &
&                 (AreaTerm1(ix,iy)+b12b1*AreaTerm2(ix,iy))*slopen1
                IF (SimParameters%n1 /= 1.0) THEN
                  IF (Slope /= 0.0) THEN 
                    sdt1(ix)=                                               &
&                     SimParameters%n1*invlgth(invdir(Direct(ix,iy)))       &
&                          *st1(ix)/Slope
                  ELSE
                    sdt1(ix)=0
                  END IF
                ELSE
                  sdt1(ix)=                                                 &
&                   SimParameters%n1*invlgth(invdir(Direct(ix,iy)))         &
&                     *(AreaTerm1(ix,iy)+b12b1*AreaTerm2(ix,iy))
                END IF
                st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                st1(ix)=max(0.0d0,dble(st1(ix)))
                sdt1(ix)=sdt1(ix)*factor(ix,iy)
                Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
              END DO
            ELSE      ! TwoFluvialProcesses
              DO ix=LowX,HighX
                Slope=s0(ix,iy)
                st1(ix)=AreaTerm1(ix,iy)*Slope**SimParameters%n1
                IF (SimParameters%n1 /= 1.0) THEN
                  IF (Slope /= 0.0) THEN
                    sdt1(ix)=                                               &
&                     SimParameters%n1*invlgth(invdir(Direct(ix,iy)))       &
&                            *st1(ix)/Slope
                  ELSE
                    sdt1(ix)=0
                  END IF
                ELSE
                  sdt1(ix)=                                                 &
&                     SimParameters%n1*invlgth(invdir(Direct(ix,iy)))       &
&                            *AreaTerm1(ix,iy)
                END IF
                st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                st1(ix)=max(0.0d0,dble(st1(ix)))
                sdt1(ix)=sdt1(ix)*factor(ix,iy)
                Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
              END DO
            END IF    ! TwoFluvialProcesses
            DO ix=LowX,HighX
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
              Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
            END DO
          END DO      ! iy=2,ky
      END IF
! 
!   Mass Balance
! 
      DO i=1,init    
        Zout=Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep               &
&             +dZdX2(abs(iXXX(i)),abs(iYYY(i)))
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
      IF (IrregularBoundary) THEN
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
              IF (Area(ix,iy) /= 1) THEN
                IF(Dsed(ix,iy) /= 0) THEN
                  temp=(1-SimParameters%n1)*Dsed(ix,iy)*timestep
                  IF (temp < 0.99999999) THEN
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)               &
&                     *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                  ELSE
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                  END IF
                ELSE
                  Sed(ix,iy)=0
                END IF
              ELSE
                Sed(ix,iy)=Sed(ix,iy)*timestep
              END IF
              Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
              IF (SimParameters%dZ /= 0.0) THEN
                Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
              END IF
              Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
              Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
            END IF
          END DO
        END DO
      ELSE             ! Regular Boundary
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            IF (Area(ix,iy) /= 1) THEN
              IF(Dsed(ix,iy) /= 0) THEN
                temp=(1-SimParameters%n1)*Dsed(ix,iy)*timestep
                IF (temp < 0.99999999) THEN
                  Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                      &
&                     *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                ELSE
                  Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                END IF
              ELSE
                Sed(ix,iy)=0
              END IF
            ELSE
              Sed(ix,iy)=Sed(ix,iy)*timestep
            END IF
            Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
            IF (SimParameters%dZ /= 0.0) THEN
              Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
            END IF
            Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
            Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
          END DO
        END DO
      END IF           ! regular/irregular boundaries ModeSolver=5
! 
!  zero the points with fixed elevtions
! 
      DO i=1,init    
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!  Work out sediment flux for retention ponds
! 
        IF (NoRegions /= 0) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Region(i,j) /= 0) THEN
                sumsed(RegionMap(Region(i,j)))                             &
&                   =sumsed(RegionMap(Region(i,j)))+Sed(i,j)
                IF (Region(i,j) <= 10) THEN
                  Sed(i,j)=0
                END IF
              END IF
            END DO
          END DO
        END IF
      RETURN
END SUBROUTINE sedanal3


! 
! ===================================================================
!                          SEDANAL4
! ===================================================================
!
!
! ===========================================================
!                          ModeSolver=4
!              HYBRID Physical/Equilibrium method
!    Two processes hillslope-Channel, transport limitation
! ===========================================================
! 
SUBROUTINE SedAnal4(Sed,Dsed,s0,AreaTerm1,AreaTerm2,factor,dZdX2            &
&           ,Uplift,Y,z,Direct,Area,IrregularBoundary,Domain                &
&           ,TwoFluvialProcesses,QsHoldT,Zout,Zin,init,iXXX,iYYY            &
&           ,TimeStep,NoRegions,RegionMap,Region,SumSed,DetCIF              &
&           ,LowX,HighX,LowY,HighY,Erode_m1,Erode_n1,Discharge              &
&           ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge          &
&           ,gridX,gridY,PotErosion)
  USE LayerSupport
  USE LayerConstants
  USE SiberiaConstants
  USE Setup
  USE SiberiaTypes
  USE Support
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),gridX,gridY                                    &
&        ,NoRegions,RegionMap(*),LowX,HighX,LowY,HighY
    INTEGER,DIMENSION(gridX,gridY) :: Direct,Region
    REAL(KIND(0.0D0)) :: QsHoldT,Zout,Zin,TimeStep,SumSed(*),b12b1
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Sed,Dsed,s0,Erode_m1,Erode_n1,AreaTerm1        &
&        ,Area,AreaTerm2,factor,dZdX2,Uplift,Y,z,Discharge
    TYPE(ArrayR8XY) :: Erosion,FlowB1,FlowAge,PotErosion
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)                                 &
&        ,TwoFluvialProcesses,DetCIF,SedimentTracking
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ix,iy,nxti,nxtj,errorNo
    REAL(KIND(0.0D0)) :: Slope,slopen1,temp,temp_n1,detach
    REAL(KIND(0.0D0)) :: st1(LowX:HighX),sdt1(LowX:HighX)
    logical :: Channel(LowX:HighX),mask(LowX:HighX,LowY:HighY)     &
&             ,Detachment_Active
!
!      CALL TimerShow(' begin sedanal4')
!      write (*,*) 'sedanal4 here 1'
      call Layer_Get(Layer_Detachment_Active,Detachment_Active,ErrorNo)
      IF (TwoFluvialProcesses) THEN
        b12b1=SimParameters%b12/SimParameters%b1
      END IF
      do iy=LowY,HighY
        do ix=LowX,HighX
          Sed(ix,iy)=0.0
          Dsed(ix,iy)=0.0
          mask(ix,iy)=.true.
        end do
      end do
      Erosion%Data=0
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
!               Irregular Boundary
! 
      IF (IrregularBoundary) THEN
          do iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (DetCIF) THEN
              do ix=LowX,HighX
                if (Domain(ix,iy)) then
                  if (y(ix,iy) > SimParameters%YHold) then
                    Channel(ix)=.true.
                  else
                    Channel(ix)=.false.
                  end if
                end if
              end do
            ELSE
              do ix=LowX,HighX
                if (Domain(ix,iy)) then
                  if (SimParameters%b5*(SimParameters%b3*Area(ix,iy)**SimParameters%m3)    &
&                     **SimParameters%m5*s0(ix,iy)**SimParameters%n5*SimParameters%c1      &
&                            > 0.005) then
                    Channel(ix)=.true.
                  else
                    Channel(ix)=.false.
                  end if
                end if
              end do
            END IF       ! END of DetCIF
            IF (TwoFluvialProcesses) THEN
              do ix=LowX,HighX
                if (Domain(ix,iy)) then
                  temp_n1=Erode_n1(ix,iy)
                  slopen1=s0(ix,iy)**temp_n1
                  Slope=s0(ix,iy)
                  if (Channel(ix)) THEN    ! Channel
!                    st1(ix)=b12b1*AreaTerm2(ix,iy)*slopen1
                    st1(ix)=b12b1*Discharge(ix,iy)**SimParameters%m12*slopen1
                  ELSE        ! hillslope
                    st1(ix)=Discharge(ix,iy)**Erode_m1(ix,iy)*slopen1
                  END IF
                  IF (temp_n1 /= 1.0) THEN 
                    IF (Slope /= 0.0) THEN
                      sdt1(ix)=                                                       &
&                       temp_n1*invlgth(invdir(Direct(ix,iy)))*st1(ix)/Slope
                    ELSE
                      sdt1(ix)=0
                    END IF
                  ELSE
                    IF (Channel(ix)) THEN    ! Channel
                      sdt1(ix)=                                                       &
&                      temp_n1*invlgth(invdir(Direct(ix,iy)))                         &
!&                        *b12b1*AreaTerm2(ix,iy)
&                        *b12b1*Discharge(ix,iy)**SimParameters%m12
                    ELSE        ! hillslope
                      sdt1(ix)=                                                       &
&                      temp_n1*invlgth(invdir(Direct(ix,iy)))                         &
!&                       *AreaTerm1(ix,iy)
&                       *Discharge(ix,iy)**Erode_m1(ix,iy)
                    END IF
                  END IF
                  st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                  st1(ix)=max(0.0d0,dble(st1(ix)))
                  sdt1(ix)=sdt1(ix)*factor(ix,iy)
                  Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                  Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
                END if
              END do
            ELSE        ! one fluvial process
              do ix=LowX,HighX
                if (Domain(ix,iy)) then
                  temp_n1=Erode_n1(ix,iy)
                  slopen1=s0(ix,iy)**temp_n1
                  Slope=s0(ix,iy)
!                  st1(ix)=AreaTerm1(ix,iy)*slopen1
                  st1(ix)=Discharge(ix,iy)**Erode_m1(ix,iy)*slopen1
                  IF (temp_n1 /= 1.0) THEN 
                    IF (Slope /= 0.0) THEN
                      sdt1(ix)=                                                     &
&                       temp_n1*invlgth(invdir(Direct(ix,iy)))*st1(ix)/Slope
                    ELSE
                      sdt1(ix)=0
                    END IF
                  ELSE
                    sdt1(ix)=                                                       &
&                      temp_n1*invlgth(invdir(Direct(ix,iy)))                       &
!&                     *AreaTerm1(ix,iy)
&                     *Discharge(ix,iy)**Erode_m1(ix,iy)
                  END IF
                  st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                  st1(ix)=max(0.0d0,dble(st1(ix)))
                  sdt1(ix)=sdt1(ix)*factor(ix,iy)
                  Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                  Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
                END if
              END do
            END IF      ! END of IF(TwoFluvialProcesses)
            do ix=LowX,HighX
              if (Domain(ix,iy)) then
                nxti=ix+dir1(Direct(ix,iy))
                nxtj=iy+dir2(Direct(ix,iy))
                Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
              END if
            END do
          END do        ! END of iy=2,ky
      ELSE
! 
!               Regular Boundary
! 
          do iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (DetCIF) THEN
              do ix=LowX,HighX
                if (y(ix,iy) > SimParameters%YHold) THEN
                  Channel(ix)=.true.
                ELSE
                  Channel(ix)=.false.
                END IF
              END do
            ELSE
              do ix=LowX,HighX
                IF (SimParameters%b5*(SimParameters%b3*Area(ix,iy)**SimParameters%m3)      &
&                    **SimParameters%m5*s0(ix,iy)**SimParameters%n5*SimParameters%c1       &
&                              > 0.005) THEN
                  Channel(ix)=.true.
                ELSE
                  Channel(ix)=.false.
                END IF
              END do
            END IF       ! END of IF (DetCIF)
            IF (TwoFluvialProcesses) THEN
              do ix=LowX,HighX
                temp_n1=Erode_n1(ix,iy)
                Slope=s0(ix,iy)
                slopen1=s0(ix,iy)**temp_n1
                IF (Channel(ix)) THEN    ! Channel
!                  st1(ix)=b12b1*AreaTerm2(ix,iy)*slopen1
                  st1(ix)=b12b1*Discharge(ix,iy)**SimParameters%m12*slopen1
                ELSE                     ! hillslope
!                  st1(ix)=AreaTerm1(ix,iy)*slopen1
                  st1(ix)=Discharge(ix,iy)**Erode_m1(ix,iy)*slopen1
                END IF
                IF (temp_n1 /= 1.0) THEN
                  IF (Slope /= 0.0) THEN 
                    sdt1(ix)=                                                      &
&                     temp_n1*invlgth(invdir(Direct(ix,iy)))*st1(ix)               &
&                      /Slope
                  ELSE
                    sdt1(ix)=0
                  END IF
                ELSE
                  IF (Channel(ix)) THEN    ! Channel
                    sdt1(ix)=                                                      &
&                      temp_n1*invlgth(invdir(Direct(ix,iy)))                      &
!&                        *b12b1*AreaTerm2(ix,iy)
&                        *b12b1*Discharge(ix,iy)**SimParameters%m12
                  ELSE                     ! hillslope
                    sdt1(ix)=                                                      &
&                      temp_n1*invlgth(invdir(Direct(ix,iy)))                      &
!&                       *AreaTerm1(ix,iy)
&                       *Discharge(ix,iy)**Erode_m1(ix,iy)
                  END IF
                END IF
                st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                st1(ix)=max(0.0d0,dble(st1(ix)))
                sdt1(ix)=sdt1(ix)*factor(ix,iy)
                Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
              END do
            ELSE       !  one fluvial process
              do ix=LowX,HighX
                temp_n1=Erode_n1(ix,iy)
                Slope=s0(ix,iy)
                slopen1=s0(ix,iy)**temp_n1
!                st1(ix)=AreaTerm1(ix,iy)*slopen1
                st1(ix)=Discharge(ix,iy)**Erode_m1(ix,iy)*slopen1
                IF (temp_n1 /= 1.0) THEN
                  IF (Slope /= 0.0) THEN 
                    sdt1(ix)=                                                    &
&                     temp_n1*invlgth(invdir(Direct(ix,iy)))*st1(ix)             &
&                      /Slope
                  ELSE
                    sdt1(ix)=0
                  END IF
                ELSE
                  sdt1(ix)=                                                      &
&                      temp_n1*invlgth(invdir(Direct(ix,iy)))                    &
!&                       *AreaTerm1(ix,iy)
&                       *Discharge(ix,iy)**Erode_m1(ix,iy)
                END IF
                st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
                st1(ix)=max(0.0d0,dble(st1(ix)))
                sdt1(ix)=sdt1(ix)*factor(ix,iy)
                Sed(ix,iy)=Sed(ix,iy)-st1(ix)
                Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
              END do
            END IF     ! END of IF (TwoFluvialProcesses)
!  This loop is seperate from the remainder of the loop above because 
!  the recurrence on SED and DSED inhibits both vectorisation and parallelisation
!  this could be eliminated by adopting a stride of 2 in the outer do loop.
            do ix=LowX,HighX
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
              Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
            END do
          END do       !  END of iy=LowY,HighY
      END IF
! 
!   FIxed Elevations
! 
!      write (*,*) 'sedanal4 here 2'

      do i=1,init   
!        Zout=Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep               &
!&             +dZdX2(abs(iXXX(i)),abs(iYYY(i)))
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END do
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
      IF (IrregularBoundary) THEN
        do iy=LowY,HighY
          do ix=LowX,HighX
            if (Domain(ix,iy)) then
              temp_n1=Erode_n1(ix,iy)
              IF (Area(ix,iy) /= 1) THEN
                IF (Dsed(ix,iy) /= 0) THEN
                  temp=(1-temp_n1)*Dsed(ix,iy)*timestep
                  IF (temp < 0.99999999) THEN
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                     &
&                     *((1/(1-temp))**(1/(temp_n1-1))-1)
                  ELSE
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                  END IF
                ELSE
                  Sed(ix,iy)=0
                END IF
              ELSE
                Sed(ix,iy)=Sed(ix,iy)*timestep
              END IF
              Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
!  OK this is the sediment transport from fluvial alone
!  apply detachment limitation if necessary
              if (Detachment_Active) then
                call Layer_Get(Layer_Detachment,ix,iy,detach,errorNo)
                PotErosion%data(ix,iy)=Sed(ix,iy)
                Sed(ix,iy)=Sed(ix,iy)*detach
              end if
              IF (SimParameters%dZ /= 0.0) THEN
                Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
              END IF
              Erosion%Data(ix,iy)=Sed(ix,iy)
              if (Detachment_Active) then
                PotErosion%data(ix,iy)=PotErosion%data(ix,iy)+dZdX2(ix,iy)
              end if
            END if
          END do
        END do
      ELSE             ! Regular Boundary
        do iy=LowY,HighY
          do ix=LowX,HighX
            temp_n1=Erode_n1(ix,iy)
            IF (Area(ix,iy) /= 1) THEN
              IF (Dsed(ix,iy) /= 0) THEN
                temp=(1-temp_n1)*Dsed(ix,iy)*timestep
                IF (temp < 0.99999999) THEN
                  Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                         &
&                    *((1/(1-temp))**(1/(temp_n1-1))-1)
                ELSE
                  Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                END IF
              ELSE
                Sed(ix,iy)=0
              END IF
            ELSE
              Sed(ix,iy)=Sed(ix,iy)*timestep
            END IF
            Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
!  OK this is the sediment transport from fluvial alone
!  apply detachment limitation if necessary
              if (Detachment_Active) then
                call Layer_Get(Layer_Detachment,ix,iy,detach,errorNo)
                PotErosion%data(ix,iy)=Sed(ix,iy)
                Sed(ix,iy)=Sed(ix,iy)*detach
              end if
            IF (SimParameters%dZ /= 0.0) THEN
              Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
            END IF
            Erosion%Data(ix,iy)=Sed(ix,iy)
            if (Detachment_Active) then
              PotErosion%data(ix,iy)=PotErosion%data(ix,iy)+dZdX2(ix,iy)
            end if
          END do
        END do
      END IF           ! regular/irregular boundaries 
!      write (*,*) 'sedanal4 here 3'
!  do the detachment limitation re deposition limitation
      if (Detachment_Active) then
        call Layer_FlowTrackingDep(sed,Erosion,Direct,domain,LowX,HighX,LowY,HighY,GridX,GridY)
      end if
!      write (*,*) 'sedanal4 here 4'
!  uplift operator
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            sed(i,j)=sed(i,j)+Uplift(i,j)
          END IF
        END DO
      END DO
! 
!  zero the points with fixed elevations
! 
      do i=1,init    
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END do
!
!  aggrade those points that are aggrading
!
      do i=1,NoAggrade
        sed(AggradeX(i),AggradeY(i))=Aggrade(i)
      end do
! 
!  Work out sediment flux for retention ponds
! 
      IF (NoRegions /= 0) THEN
        do j=LowY,HighY
          do i=LowX,HighX
            if (Region(i,j) /= 0) then
              sumsed(RegionMap(Region(i,j)))                              &
&                   =sumsed(RegionMap(Region(i,j)))+Sed(i,j)
              if (Region(i,j) <= 10) sed(i,j)=0
            END if
          END do
        END do
      END IF
!
!      temp=0
!      do i=HighX,2,-1
!       call Layer_Get(Layer_Detachment,i,3,detach,errorNo)
!       temp=temp-erosion%data(i,3)
!       write (63,1000) ' ',i,sngl(-erosion%data(i,3)),sngl(temp)            &
!&            ,sngl(area(i,3)),sngl(s0(i,3)),sngl(detach)
! 1000 format(a,i3,10(e20.7,' '))
!     end do
!      CALL TimerShow(' end sedanal4')
!      write (*,*) 'sedanal4 here 5'
END SUBROUTINE SedAnal4


! 
! ===========================================================
!  The Dinifinity version of ModeSolver=4
! ===========================================================
! 
SUBROUTINE SedAnalDinf4(Sed,Dsed,s0,AreaTerm1,AreaTerm2,factor               &
&           ,dZdX2,Uplift,Y,z,Direct,DirectDinf,DirWeights                   &
&           ,Area,IrregularBoundary,Domain                                   &
&           ,TwoFluvialProcesses,QsHoldT,Zout,Zin,init,iXXX,iYYY             &
&           ,TimeStep,NoRegions,RegionMap,Region,SumSed,DetCIF               &
&           ,LowX,HighX,LowY,HighY,SimParameters,Erosion,gridX,gridY)
  USE SiberiaConstants
  USE Setup
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),NoRegions,RegionMap(*)             &
&        ,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct,Region,DirectDinf
    REAL(KIND(0.0D0)) :: QsHoldT,Zout,Zin,TimeStep,SumSed(*),b12b1
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Sed,Dsed,s0,DirWeights,AreaTerm1       &
&        ,Area,AreaTerm2,factor,dZdX2,Uplift,Y,z
    TYPE(ArrayR8XY) :: Erosion
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)                         &
&        ,TwoFluvialProcesses,DetCIF
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,nxti,nxtj,nxti2,nxtj2
    REAL(KIND(0.0D0)) :: st1(1:GridX),sdt1(1:GridX),Slope,slopen1,temp
    LOGICAL :: Channel(1:GridX)
! 
      IF (TwoFluvialProcesses) THEN
        b12b1=SimParameters%b12/SimParameters%b1
      END IF
      WHERE (Domain)
        Sed=0.0
        Dsed=0.0
      END WHERE
      Erosion%Data=0
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
!               Irregular Boundary
!              ====================
! 
      IF (IrregularBoundary) THEN
          DO j=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (DetCIF) THEN
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  IF (y(i,j) > SimParameters%YHold) THEN
                    Channel(i)=.true.
                  ELSE
                    Channel(i)=.false.
                  END IF
                END IF
              END DO
            ELSE
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  IF (SimParameters%b5*(SimParameters%b3*Area(i,j)**SimParameters%m3)     &
&                       **SimParameters%m5*s0(i,j)**SimParameters%n5*SimParameters%c1     &
&                             > 0.005) THEN
                    Channel(i)=.true.
                  ELSE
                    Channel(i)=.false.
                  END IF
                END IF
              END DO
            END IF       ! END of DetCIF
            IF (TwoFluvialProcesses) THEN
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  slopen1=s0(i,j)**SimParameters%n1
                  Slope=s0(i,j)
                  IF (Channel(i)) THEN    ! Channel
                    st1(i)=b12b1*AreaTerm2(i,j)*slopen1
                  ELSE        ! hillslope
                    st1(i)=AreaTerm1(i,j)*slopen1
                  END IF
                  IF (SimParameters%n1 /= 1.0) THEN 
                    IF (Slope /= 0.0) THEN
                      sdt1(i)=                                                            &
&                       SimParameters%n1*invlgth(invdir(Direct(i,j)))*st1(i)/Slope
                    ELSE
                      sdt1(i)=0
                    END IF
                  ELSE
                    IF (Channel(i)) THEN    ! Channel
                      sdt1(i)=                                                          &
&                      SimParameters%n1*invlgth(invdir(Direct(i,j)))                   &
&                        *b12b1*AreaTerm2(i,j)
                    ELSE        ! hillslope
                      sdt1(i)=                                                          &
&                      SimParameters%n1*invlgth(invdir(Direct(i,j)))                   &
&                       *AreaTerm1(i,j)
                    END IF
                  END IF
                  st1(i)=st1(i)*factor(i,j)-QsHoldT
                  st1(i)=max(0.0d0,dble(st1(i)))
                  sdt1(i)=sdt1(i)*factor(i,j)
                  Sed(i,j)=Sed(i,j)-st1(i)
                  Dsed(i,j)=Dsed(i,j)+sdt1(i)
                END IF
              END DO
            ELSE        ! one fluvial process
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  slopen1=s0(i,j)**SimParameters%n1
                  Slope=s0(i,j)
                  st1(i)=AreaTerm1(i,j)*slopen1
                  IF (SimParameters%n1 /= 1.0) THEN 
                    IF (Slope /= 0.0) THEN
                      sdt1(i)=                                                          &
&                       SimParameters%n1*invlgth(invdir(Direct(i,j)))*st1(i)/Slope
                    ELSE
                      sdt1(i)=0
                    END IF
                  ELSE
                    sdt1(i)=                                                            &
&                      SimParameters%n1*invlgth(invdir(Direct(i,j)))                   &
&                       *AreaTerm1(i,j)
                  END IF
                  st1(i)=st1(i)*factor(i,j)-QsHoldT
                  st1(i)=max(0.0d0,dble(st1(i)))
                  sdt1(i)=sdt1(i)*factor(i,j)
                  Sed(i,j)=Sed(i,j)-st1(i)
                  Dsed(i,j)=Dsed(i,j)+sdt1(i)
                END IF
              END DO
            END IF      ! END of IF(TwoFluvialProcesses)
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
!  main direction
                nxti=i+dir1(Direct(i,j))
                nxtj=j+dir2(Direct(i,j))
                IF (DirectDinf(i,j) == 0) THEN
                  Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(i)
                  Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(i)
                ELSE
!  main direction
                  Sed(nxti,nxtj)=Sed(nxti,nxtj)                                         &
&                    +(1-DirWeights(i,j))*st1(i)
                  Dsed(nxti,nxtj)=Dsed(nxti,nxtj)                                       &
&                    +(1-DirWeights(i,j))*sdt1(i)
!  subsidary direction
                  nxti2=i+dir1(DirectDinf(i,j))
                  nxtj2=j+dir2(DirectDinf(i,j))
                  Sed(nxti2,nxtj2)=Sed(nxti2,nxtj2)                                     &
&                    +DirWeights(i,j)*st1(i)
                  Dsed(nxti2,nxtj2)=Dsed(nxti2,nxtj2)                                   &
&                    +DirWeights(i,j)*sdt1(i)
                END IF
              END IF
            END DO
          END DO        ! END of iy=2,ky
      ELSE
! 
!               Regular Boundary
!              ==================
! 
          DO j=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
            IF (DetCIF) THEN
              DO i=LowX,HighX
                IF (y(i,j) > SimParameters%YHold) THEN
                  Channel(i)=.true.
                ELSE
                  Channel(i)=.false.
                END IF
              END DO
            ELSE
              DO i=LowX,HighX
                IF (SimParameters%b5*(SimParameters%b3*Area(i,j)**SimParameters%m3)   &
&                    **SimParameters%m5*s0(i,j)**SimParameters%n5*SimParameters%c1    &
&                         > 0.005) THEN
                  Channel(i)=.true.
                ELSE
                  Channel(i)=.false.
                END IF
              END DO
            END IF       ! END of IF (DetCIF)
            IF (TwoFluvialProcesses) THEN
              DO i=LowX,HighX
                Slope=s0(i,j)
                slopen1=s0(i,j)**SimParameters%n1
                IF (Channel(i)) THEN    ! Channel
                  st1(i)=b12b1*AreaTerm2(i,j)*slopen1
                ELSE                     ! hillslope
                  st1(i)=AreaTerm1(i,j)*slopen1
                END IF
                IF (SimParameters%n1 /= 1.0) THEN
                  IF (Slope /= 0.0) THEN 
                    sdt1(i)=                                                           &
&                     SimParameters%n1*invlgth(invdir(Direct(i,j)))*st1(i)           &
&                      /Slope
                  ELSE
                    sdt1(i)=0
                  END IF
                ELSE
                    sdt1(i)=                                                           &
&                      SimParameters%n1*invlgth(invdir(Direct(i,j)))                  &
&                       *AreaTerm1(i,j)
                END IF
                st1(i)=st1(i)*factor(i,j)-QsHoldT
                st1(i)=max(0.0d0,dble(st1(i)))
                sdt1(i)=sdt1(i)*factor(i,j)
                Sed(i,j)=Sed(i,j)-st1(i)
                Dsed(i,j)=Dsed(i,j)+sdt1(i)
              END DO
            ELSE       !  one fluvial process
              DO i=LowX,HighX
                Slope=s0(i,j)
                slopen1=s0(i,j)**SimParameters%n1
                st1(i)=AreaTerm1(i,j)*slopen1
                IF (SimParameters%n1 /= 1.0) THEN
                  IF (Slope /= 0.0) THEN 
                    sdt1(i)=                                                           &
&                     SimParameters%n1*invlgth(invdir(Direct(i,j)))*st1(i)           &
&                      /Slope
                  ELSE
                    sdt1(i)=0
                  END IF
                ELSE
                  sdt1(i)=                                                             &
&                      SimParameters%n1*invlgth(invdir(Direct(i,j)))                  &
&                       *AreaTerm1(i,j)
                END IF
                st1(i)=st1(i)*factor(i,j)-QsHoldT
                st1(i)=max(0.0d0,dble(st1(i)))
                sdt1(i)=sdt1(i)*factor(i,j)
                Sed(i,j)=Sed(i,j)-st1(i)
                Dsed(i,j)=Dsed(i,j)+sdt1(i)
              END DO
            END IF     ! END of IF (TwoFluvialProcesses)
            DO i=LowX,HighX
!  main direction
              nxti=i+dir1(Direct(i,j))
              nxtj=j+dir2(Direct(i,j))
              IF (DirectDinf(i,j) == 0) THEN
                Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(i)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(i)
              ELSE
!  main direction
                Sed(nxti,nxtj)=Sed(nxti,nxtj)                                          &
&                  +(1-DirWeights(i,j))*st1(i)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)                                        &
&                  +(1-DirWeights(i,j))*sdt1(i)
!  subsidary direction
                nxti2=i+dir1(DirectDinf(i,j))
                nxtj2=j+dir2(DirectDinf(i,j))
                Sed(nxti2,nxtj2)=Sed(nxti2,nxtj2)                                      &
&                  +DirWeights(i,j)*st1(i)
                Dsed(nxti2,nxtj2)=Dsed(nxti2,nxtj2)                                    &
&                  +DirWeights(i,j)*sdt1(i)
              END IF
            END DO
          END DO       !  END of j=2,ky
      END IF
! 
!   Mass Balance
! 
      DO i=1,init    
        Zout=Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep                                   &
&             +dZdX2(abs(iXXX(i)),abs(iYYY(i))) 
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (Area(i,j) /= 1) THEN
                IF (Dsed(i,j) /= 0) THEN
                  temp=(1-SimParameters%n1)*Dsed(i,j)*timestep
                  IF (temp < 0.99999999) THEN
                    Sed(i,j)=-Sed(i,j)/Dsed(i,j)                                &
&                     *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                  ELSE
                    Sed(i,j)=-Sed(i,j)/Dsed(i,j)
                  END IF
                ELSE
                  Sed(i,j)=0
                END IF
              ELSE
                Sed(i,j)=Sed(i,j)*timestep
              END IF
              Sed(i,j)=Sed(i,j)/SimParameters%Bulk
              IF (SimParameters%dZ /= 0.0) THEN
                Sed(i,j)=Sed(i,j)+dZdX2(i,j)
              END IF
              Erosion%Data(i,j)=Sed(i,j)
              Zin=Zin+Sed(i,j)
! 
!   Tectonic uplift term
! 
              Sed(i,j)=Sed(i,j)+Uplift(i,j)
            END IF
          END DO
        END DO
      ELSE             ! Regular Boundary
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Area(i,j) /= 1) THEN
              IF (Dsed(i,j) /= 0) THEN
                temp=(1-SimParameters%n1)*Dsed(i,j)*timestep
                IF (temp < 0.99999999) THEN
                  Sed(i,j)=-Sed(i,j)/Dsed(i,j)                                &
&                    *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                ELSE
                  Sed(i,j)=-Sed(i,j)/Dsed(i,j)
                END IF
              ELSE
                Sed(i,j)=0
              END IF
            ELSE
              Sed(i,j)=Sed(i,j)*timestep
            END IF
            Sed(i,j)=Sed(i,j)/SimParameters%Bulk
            IF (SimParameters%dZ /= 0.0) THEN
              Sed(i,j)=Sed(i,j)+dZdX2(i,j)
            END IF
            Erosion%Data(i,j)=Sed(i,j)
            Zin=Zin+Sed(i,j)
! 
!   Tectonic uplift term
! 
            Sed(i,j)=Sed(i,j)+Uplift(i,j)
          END DO
        END DO
      END IF           ! regular/irregular boundaries 
! 
!  zero the points with fixed elevations
! 
      DO i=1,init    
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!  Work out sediment flux for retention ponds
! 
        IF (NoRegions /= 0) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Region(i,j) /= 0) THEN
                sumsed(RegionMap(Region(i,j)))                                 &
&                   =sumsed(RegionMap(Region(i,j)))+Sed(i,j)
                IF (Region(i,j) <= 10) THEN
                  Sed(i,j)=0
                END IF
              END IF
            END DO
          END DO
        END IF
      RETURN
END SUBROUTINE SedAnalDinf4


! 
! ===================================================================
!                          SEDANAL8
! ===================================================================
!
!
! ===========================================================
!                          ModeSolver=8
!              Detachment and Transport Limitation
! ===========================================================
! 
SUBROUTINE SedAnal8(Sed,Dsed,s0,AreaTerm1,AreaTerm2,factor,dZdX2            &
&           ,Uplift,Y,z,Direct,Area,IrregularBoundary,Domain                &
&           ,TwoFluvialProcesses,QsHoldT,Zout,Zin,init,iXXX,iYYY            &
&           ,TimeStep,NoRegions,RegionMap,Region,SumSed,DetCIF              &
&           ,LowX,HighX,LowY,HighY,Erode_m1,Erode_n1,Discharge              &
&           ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge          &
&           ,gridX,gridY)
  USE SiberiaConstants
  USE Setup
  USE SiberiaTypes
  USE openMPsupport
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),gridX,gridY                                &
&        ,NoRegions,RegionMap(*),LowX,HighX,LowY,HighY
    INTEGER,DIMENSION(gridX,gridY) :: Direct,Region
    REAL(KIND(0.0D0)) :: QsHoldT,Zout,Zin,TimeStep,SumSed(*),b12b1
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Sed,Dsed,s0,Erode_m1,Erode_n1,AreaTerm1        &
&        ,Area,AreaTerm2,factor,dZdX2,Uplift,Y,z,Discharge
    TYPE(ArrayR8XY) :: Erosion,FlowB1,FlowAge
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)                                 &
&        ,TwoFluvialProcesses,DetCIF,SedimentTracking
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,nxti,nxtj,ErrorNo
    REAL(KIND(0.0D0)) :: Slope,slopen1,temp,temp_n1,b1max
    REAL(KIND(0.0D0)) :: st1(LowX:HighX),sdt1(LowX:HighX)
    LOGICAL :: Channel(LowX:HighX)
    TYPE(ArrayR8XY) :: PotentialDetachment,ActualDetachment

!  This routine does not distinguish between regular and irregular boundaries
!  It assumes that DOMAIN is set correctly and that we can treat all domains
!  as irregular. The extra overheads for regular boundaries appear to be small
!  and it reduces code complexity by a factor of2

      b1max=maxval(factor(LowX:HighX,LowY:HighY))
      IF (TwoFluvialProcesses) THEN
        b12b1=SimParameters%b12/SimParameters%b1
      END IF
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            Sed(i,j)=0.0
            Dsed(i,j)=0.0
            Erosion%Data(i,j)=0
          END IF
        END DO
      END DO
      CALL AllocateArray(PotentialDetachment,LowX,HighX,LowY,HighY,ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError                        &
&                      ('PotentialDetachment','SIBERIA_SEDANAL8')
      CALL AllocateArray(ActualDetachment,LowX,HighX,LowY,HighY,ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError                        &
&                      ('ActualDetachment','SIBERIA_SEDANAL8')
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
!               Irregular Boundary
! 
      DO J=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
        IF (DetCIF) THEN
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (y(i,j) > SimParameters%YHold) THEN
                Channel(i)=.true.
              ELSE
                Channel(i)=.false.
              END IF
            END IF
          END DO
        ELSE
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (SimParameters%b5*(SimParameters%b3*Area(i,j)**SimParameters%m3)    &
&                     **SimParameters%m5*s0(i,j)**SimParameters%n5*SimParameters%c1      &
&                            > 0.005) THEN
                Channel(i)=.true.
              ELSE
                Channel(i)=.false.
              END IF
            END IF
          END DO
        END IF       ! END of DetCIF
        IF (TwoFluvialProcesses) THEN
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              temp_n1=Erode_n1(i,j)
              slopen1=s0(i,j)**temp_n1
              Slope=s0(i,j)
              IF (Channel(i)) THEN    ! Channel
                st1(i)=b12b1*Discharge(i,j)**SimParameters%m12*slopen1
              ELSE        ! hillslope
                st1(i)=Discharge(i,j)**Erode_m1(i,j)*slopen1
              END IF
              IF (temp_n1 /= 1.0) THEN 
                IF (Slope /= 0.0) THEN
                  sdt1(i)=                                                       &
&                       temp_n1*invlgth(invdir(Direct(i,j)))*st1(i)/Slope
                ELSE
                  sdt1(i)=0
                END IF
              ELSE
                IF (Channel(i)) THEN    ! Channel
                  sdt1(i)=                                                       &
&                      temp_n1*invlgth(invdir(Direct(i,j)))                         &
&                        *b12b1*Discharge(i,j)**SimParameters%m12
                ELSE        ! hillslope
                  sdt1(i)=                                                       &
&                      temp_n1*invlgth(invdir(Direct(i,j)))                         &
&                       *Discharge(i,j)**Erode_m1(i,j)
                END IF
              END IF
              st1(i)=st1(i)*b1max-QsHoldT
              st1(i)=max(0.0d0,dble(st1(i)))
              sdt1(i)=sdt1(i)*b1max
              Sed(i,j)=Sed(i,j)-st1(i)
              Dsed(i,j)=Dsed(i,j)+sdt1(i)
            END IF
          END DO
        ELSE        ! one fluvial process
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              temp_n1=Erode_n1(i,j)
              slopen1=s0(i,j)**temp_n1
              Slope=s0(i,j)
              st1(i)=Discharge(i,j)**Erode_m1(i,j)*slopen1
              IF (temp_n1 /= 1.0) THEN 
                IF (Slope /= 0.0) THEN
                  sdt1(i)=                                                     &
&                       temp_n1*invlgth(invdir(Direct(i,j)))*st1(i)/Slope
                ELSE
                  sdt1(i)=0
                END IF
              ELSE
                sdt1(i)=                                                       &
&                      temp_n1*invlgth(invdir(Direct(i,j)))                       &
&                     *Discharge(i,j)**Erode_m1(i,j)
              END IF
              st1(i)=st1(i)*b1max-QsHoldT
              st1(i)=max(0.0d0,dble(st1(i)))
              sdt1(i)=sdt1(i)*b1max
              Sed(i,j)=Sed(i,j)-st1(i)
              Dsed(i,j)=Dsed(i,j)+sdt1(i)
            END IF
          END DO
        END IF      ! END of IF(TwoFluvialProcesses)
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            nxti=i+dir1(Direct(i,j))
            nxtj=j+dir2(Direct(i,j))
            Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(i)
            Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(i)
          END IF
        END DO
      END DO
! 
!   FIxed Elevations
! 
      DO i=1,init   
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END do
! 
!   the actual amount that Z(i,j) will changed in the next time step
! 
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            temp_n1=Erode_n1(i,j)
            IF (Area(i,j) /= 1) THEN
              IF (Dsed(i,j) /= 0) THEN
                temp=(1-temp_n1)*Dsed(i,j)*timestep
                IF (temp < 0.99999999) THEN
                  Sed(i,j)=-Sed(i,j)/Dsed(i,j)                      &
&                     *((1/(1-temp))**(1/(temp_n1-1))-1)
                ELSE
                  Sed(i,j)=-Sed(i,j)/Dsed(i,j)
                END IF
              ELSE
                Sed(i,j)=0
              END IF
            ELSE
              Sed(i,j)=Sed(i,j)*timestep
            END IF
          END IF
        END DO
      END DO

! Potential Detachment  ... note the sign change
!                    sed +ve => deposition
!    potentialdetachment +ve => erosion

      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            PotentialDetachment%data(i,j)=-sed(i,j)
          END IF
        END DO
      END DO
! Potential Transport
      CALL PotentialTransport8(PotentialQs,PotentialDetachment,Direct,Domain          &
&              ,LowX,HighX,LowY,HighY,GridX,GridY)


! Actual Transport
      CALL ActualTransport8(ActualQs,PotentialQs,ActualDetachment                    &
&              ,PotentialDetachment,Direct,Domain                                    &
&              ,b1max,factor,LowX,HighX,LowY,HighY,GridX,GridY)

      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            sed(i,j)=-ActualDetachment%data(i,j)
          END IF
        END DO
      END DO

      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            sed(i,j)=sed(i,j)/SimParameters%Bulk
          END IF
        END DO
      END DO
!  diffusion operator
      IF (SimParameters%dZ /= 0.0) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              sed(i,j)=sed(i,j)+dZdX2(i,j)
            END IF
          END DO
        END DO
      END IF
!  erosion tracking
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            Erosion%Data(i,j)=sed(i,j)
          END IF
        END DO
      END DO
!  uplift operator
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (Domain(i,j)) THEN
            sed(i,j)=sed(i,j)+Uplift(i,j)
          END IF
        END DO
      END DO

!  zero the points with fixed elevations

      DO i=1,init    
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
        Erosion%data(abs(iXXX(i)),abs(iYYY(i)))=0
      END do
!
!  aggrade those points that are aggrading
!
      DO i=1,NoAggrade
        sed(AggradeX(i),AggradeY(i))=Aggrade(i)
      END DO
      CALL DeAllocateArray(PotentialDetachment,ErrorNo)
      CALL DeAllocateArray(ActualDetachment,ErrorNo)
END SUBROUTINE SedAnal8


SUBROUTINE PotentialTransport8(PotentialQs,PotentialDetachment,Direct,Domain  &
&              ,LowX,HighX,LowY,HighY,GridX,GridY)
  USE SiberiaTypes
  USE SiberiaConstants
  IMPLICIT NONE

    INTEGER :: GridX,GridY,Direct(GridX,GridY),LowX,HighX,LowY,HighY
    LOGICAL :: Domain(GridX,GridY)
    TYPE(ArrayR8XY) :: PotentialQs,PotentialDetachment

! 
    INTEGER :: i,j,k,ii,jj,NoSources,Oldi,Oldj
    INTEGER :: NoIn(LowX:HighX,LowY:HighY)
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY
! 
! 
      NoSources=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          PotentialQs%data(i,j)=PotentialDetachment%data(i,j)
        END DO
      END DO
! 
!  Find the list of the most upstream points of the networks
! 
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              ii=i+dir1(Direct(i,j))
              jj=j+dir2(Direct(i,j))
              IF (i /= ii.or.j /= jj) THEN
                NoIn(ii,jj)=NoIn(ii,jj)+1
              END IF
            END IF
          END DO
        END DO
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j).and.NoIn(i,j) == 0) THEN
              NoSources=NoSources+1
              SourceX(NoSources)=i
              SourceY(NoSources)=j
            END IF
          END DO
        END DO
! 
!  Calc the areas from top TO bottom
! 
      DO k=1,NoSources
        Oldi=SourceX(k)
        Oldj=SourceY(k)
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          PotentialQs%data(i,j)=PotentialQs%data(i,j)+PotentialQs%data(Oldi,Oldj)
          IF (NoIn(i,j) == 1) THEN
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
      RETURN
END SUBROUTINE PotentialTransport8



SUBROUTINE ActualTransport8(ActualQs,PotentialQs,ActualDetachment,PotentialDetachment    &
&             ,Direct,Domain,b1max,factor,LowX,HighX,LowY,HighY,GridX,GridY)
  USE SiberiaTypes
  USE SiberiaConstants
  IMPLICIT NONE

    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    INTEGER,DIMENSION(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)) :: b1max
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Factor
    LOGICAL,DIMENSION(gridX,gridY) :: Domain
    TYPE(ArrayR8XY) :: ActualQs,PotentialQs,PotentialDetachment,ActualDetachment

! 
    INTEGER :: i,j,k,ii,jj,NoSources,Oldi,Oldj
    INTEGER :: NoIn(LowX:HighX,LowY:HighY)
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY
! 
! 
      NoSources=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
!  reduce erosion by the erodibility if erosion is modelled to occur, but let deposition
!  occur at the potential rate.
          if (PotentialDetachment%data(i,j) > 0) then
            PotentialDetachment%data(i,j)=PotentialDetachment%data(i,j)*factor(i,j)/b1max
          end if
          ActualDetachment%data(i,j)=PotentialDetachment%data(i,j)
          ActualQs%data(i,j)=max(0.0d0,PotentialDetachment%data(i,j))
        END DO
      END DO
! 
!  Find the list of the most upstream points of the networks
! 
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              ii=i+dir1(Direct(i,j))
              jj=j+dir2(Direct(i,j))
              IF (i /= ii.or.j /= jj) THEN
                NoIn(ii,jj)=NoIn(ii,jj)+1
              END IF
            END IF
          END DO
        END DO
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j).and.NoIn(i,j) == 0) THEN
              NoSources=NoSources+1
              SourceX(NoSources)=i
              SourceY(NoSources)=j
            END IF
          END DO
        END DO
! 
!  Calc the areas from top TO bottom
! 
      DO k=1,NoSources
        Oldi=SourceX(k)
        Oldj=SourceY(k)
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          ActualQs%data(i,j)=ActualQs%data(i,j)+ActualQs%data(Oldi,Oldj)
          IF (NoIn(i,j) == 1) THEN
!  checking for detachment or 
            if (ActualQs%data(i,j) <= PotentialQs%data(i,j)) then
              ActualDetachment%data(i,j)=PotentialDetachment%data(i,j)
!              write (60,*) 'detachment limited',i,j,PotentialDetachment%data(i,j)           &
!&                 ,ActualDetachment%data(i,j),ActualQs%data(i,j),PotentialQs%data(i,j)
            else
              ActualDetachment%data(i,j)=PotentialDetachment%data(i,j)                  &
&                    -(ActualQs%data(i,j)-PotentialQs%data(i,j))
!              write (60,*) 'transport limited',i,j,PotentialDetachment%data(i,j)           &
!&                 ,ActualDetachment%data(i,j),ActualQs%data(i,j),PotentialQs%data(i,j)
              ActualQs%data(i,j)=PotentialQs%data(i,j)
            end if
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
      RETURN
END SUBROUTINE ActualTransport8


! 
! 
! ===================================================================
!                          SEDANAL6
! ===================================================================
!
!
! ===================================================================
!                           ModeSolver = 6 
!     Shear Stress driven source limitation w/o transport limitation
! ===================================================================
! 
SUBROUTINE SedAnal6(s0,Sed,AreaTerm1,AreaTerm2,Uplift,factor               &
&         ,IrregularBoundary,Domain,TwoFluvialProcesses                    &
&         ,init,iXXX,iYYY,TimeStep,LowX,HighX,LowY,HighY,SimParameters     &
&         ,gridX,gridY)
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),LowX,HighX,LowY,HighY            &
&             ,gridX,gridY
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: s0,Sed,AreaTerm1,AreaTerm2           &
&             ,Uplift,factor
    REAL(KIND(0.0D0)) :: TimeStep,b12b1
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),TwoFluvialProcesses
    TYPE(LocalParameters) :: SimParameters
! 
      INTEGER :: i,j
      REAL(KIND(0.0D0)) :: slopen1
! 
        IF (TwoFluvialProcesses) THEN
          b12b1=SimParameters%b12/SimParameters%b1
        END IF
        IF (IrregularBoundary) THEN
          IF (TwoFluvialProcesses) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  slopen1=s0(i,j)**SimParameters%n1*factor(i,j)            &
&                          *TimeStep/SimParameters%Bulk
                  Sed(i,j)=Sed(i,j)                                        &
&                    -(AreaTerm1(i,j)+b12b1*AreaTerm2(i,j))*slopen1
                  Sed(i,j)=Sed(i,j)+Uplift(i,j)
                END IF
              END DO
            END DO
          ELSE      ! TwoFluvialProcesses
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  Sed(i,j)=Sed(i,j)                                        &
&                    -AreaTerm1(i,j)*factor(i,j)*s0(i,j)**SimParameters%n1 &
&                    *TimeStep/SimParameters%Bulk
                  Sed(i,j)=Sed(i,j)+Uplift(i,j)
                END IF
              END DO
            END DO
          END IF    ! TwoFluvialProcesses
        ELSE        ! IrregularBoundary
          IF (TwoFluvialProcesses) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                slopen1=s0(i,j)**SimParameters%n1*factor(i,j)*TimeStep       &
&                          /SimParameters%Bulk
                Sed(i,j)=Sed(i,j)                                            &
&                    -(AreaTerm1(i,j)+b12b1*AreaTerm2(i,j))*slopen1
                Sed(i,j)=Sed(i,j)+Uplift(i,j)
              END DO
            END DO
          ELSE      ! TwoFluvialProcesses
            DO j=LowY,HighY
              DO i=LowX,HighX
                Sed(i,j)=Sed(i,j)                                            &
&                   -AreaTerm1(i,j)*factor(i,j)*s0(i,j)**SimParameters%n1    &
&                   *TimeStep/SimParameters%Bulk
                Sed(i,j)=Sed(i,j)+Uplift(i,j)
              END DO
            END DO
          END IF    ! TwoFluvialProcesses
        END IF      !  IrregularBoundary
        DO i=1,init    
          Sed(abs(iXXX(i)),abs(iYYY(i)))=0
        END DO
      RETURN
END SUBROUTINE sedanal6

! 
! ===================================================================
!                          SEDANAL7
! ===================================================================
!
!
! ===========================================================
!                          ModeSolver=7
!              HYBRID Physical/Equilibrium method
!             Two processes diffusion on hillslope
!        diffusion and erosion in Channel, transport limitation
! ===========================================================
! 
SUBROUTINE SedAnal7(Sed,Dsed,s0,AreaTerm1,AreaTerm2                          &
&           ,factor,dZdX2,Uplift,Y,z,Direct,Area                             &
&           ,IrregularBoundary,Domain                                        &
&           ,TwoFluvialProcesses,QsHoldT,Zout,Zin,init,iXXX,iYYY             &
&           ,TimeStep,NoRegions,RegionMap,Region,SumSed,DetCIF               &
&           ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init),NoRegions,RegionMap(*)             &
&        ,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct,Region
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: Sed,Dsed,s0,AreaTerm1,AreaTerm2,Area   &
&        ,factor,dZdX2,Uplift,Y,Z
    REAL(KIND(0.0D0)) :: QsHoldT,Zout,Zin,TimeStep,SumSed(*)
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),TwoFluvialProcesses     &
&        ,DetCIF
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ix,iy,nxti,nxtj
    REAL(KIND(0.0D0)) :: st1(1:gridX),sdt1(1:gridX),Slope,slopen1,temp
    LOGICAL :: Channel
! 
      IF (DetCIF) THEN
        CALL Message_Output(Message_ErrorStop,' Error in SedAnal: '          &
&          //'Deterministic channels are not compatible with ModeSolver=7')
      END IF
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          Sed(ix,iy)=0.0
          Dsed(ix,iy)=0.0
        END DO
      END DO
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
!               Irregular Boundary
! 
      IF (IrregularBoundary) THEN
        DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
              slopen1=s0(ix,iy)**SimParameters%n1
              Slope=s0(ix,iy)
              st1(ix)=AreaTerm1(ix,iy)*slopen1
              IF (SimParameters%n1 /= 1.0) THEN 
                IF (Slope /= 0.0) THEN
                  sdt1(ix)=                                               &
&                       SimParameters%n1*invlgth(invdir(Direct(ix,iy)))   &
&                         *st1(ix)/Slope
                ELSE
                    sdt1(ix)=0
                END IF
              ELSE
                sdt1(ix)=                                                 &
&                      SimParameters%n1*invlgth(invdir(Direct(ix,iy)))    &
&                       *AreaTerm1(ix,iy)
              END IF
              st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
              st1(ix)=max(0.0d0,dble(st1(ix)))
              sdt1(ix)=sdt1(ix)*factor(ix,iy)
              Sed(ix,iy)=Sed(ix,iy)-st1(ix)
              Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
            END IF
          END DO
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
              Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
            END IF
          END DO
        END DO       ! iy=2,ky
      ELSE
! 
!               Regular Boundary
! 
        DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
          DO ix=LowX,HighX
            Slope=s0(ix,iy)
            slopen1=s0(ix,iy)**SimParameters%n1
            st1(ix)=AreaTerm1(ix,iy)*slopen1
            IF (SimParameters%n1 /= 1.0) THEN
              IF (Slope /= 0.0) THEN 
                sdt1(ix)=                                                 &
&                    SimParameters%n1*invlgth(invdir(Direct(ix,iy)))      &
&                      *st1(ix)/Slope
              ELSE
                sdt1(ix)=0
              END IF
            ELSE
              sdt1(ix)=                                                   &
&                      SimParameters%n1*invlgth(invdir(Direct(ix,iy)))    &
&                      *AreaTerm1(ix,iy)
            END IF
            st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
            st1(ix)=max(0.0d0,dble(st1(ix)))
            sdt1(ix)=sdt1(ix)*factor(ix,iy)
            Sed(ix,iy)=Sed(ix,iy)-st1(ix)
            Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
          END DO
          DO ix=LowX,HighX
            nxti=ix+dir1(Direct(ix,iy))
            nxtj=iy+dir2(Direct(ix,iy))
            Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
            Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
          END DO
        END DO      ! iy=2,ky
      END IF
! 
!   Mass Balance
! 
      DO i=1,init    
        Zout=Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep                       &
&             +dZdX2(abs(iXXX(i)),abs(iYYY(i)))
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
      IF (IrregularBoundary) THEN
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
              IF (SimParameters%b5*(SimParameters%b3                       &
&                 *Area(ix,iy)**SimParameters%m3)**SimParameters%m5        &
&                 *s0(ix,iy)**SimParameters%n5*SimParameters%c1            &
&                        > 0.005) THEN
                Channel=.true.
              ELSE
                Channel=.false.
              END IF
              IF (Channel) THEN
                IF (Area(ix,iy) /= 1) THEN
                  IF(Dsed(ix,iy) /= 0) THEN
                    temp=(1-SimParameters%n1)*Dsed(ix,iy)*timestep
                    IF (temp < 0.99999999) THEN
                      Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                    &
&                         *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                    ELSE
                      Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                    END IF
                  ELSE
                    Sed(ix,iy)=0
                  END IF
                ELSE
                  Sed(ix,iy)=Sed(ix,iy)*timestep
                END IF
                Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)    ! channel
!                IF (Channel) THEN
!                  Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)    ! channel
!                ELSE
!                  Sed(ix,iy)=dZdX2(ix,iy)
!                Sed(ix,iy)=SimParameters%OTime*Sed(ix,iy)+dZdX2(ix,iy)               ! hillslope
!                END IF
              ELSE
                Sed(ix,iy)=dZdX2(ix,iy)               ! hillslope
              END IF
              Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
              Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
              Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
            END IF
             if (ix == 50 .and. iy == 50) then
              end if
          END DO
        END DO
      ELSE             ! Regular Boundary
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            IF (SimParameters%b5*(SimParameters%b3                         &
&               *Area(ix,iy)**SimParameters%m3)**SimParameters%m5          &
&               *s0(ix,iy)**SimParameters%n5*SimParameters%c1              &
&                   > 0.005) THEN
              Channel=.true.
            ELSE
              Channel=.false.
            END IF
            IF (Channel) THEN
              IF (Area(ix,iy) /= 1) THEN
                IF(Dsed(ix,iy) /= 0) THEN
                  temp=(1-SimParameters%n1)*Dsed(ix,iy)*timestep
                  IF (temp < 0.99999999) THEN
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                       &
&                      *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                  ELSE
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                  END IF
                ELSE
                  Sed(ix,iy)=0
                END IF
              ELSE
                Sed(ix,iy)=Sed(ix,iy)*timestep
              END IF
              Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
            ELSE
              Sed(ix,iy)=dZdX2(ix,iy)
            END IF
!            IF (Channel) THEN
!              Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
!            ELSE
!              Sed(ix,iy)=SimParameters%OTime*Sed(ix,iy)+dZdX2(ix,iy)
!            END IF
            Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
            Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
            Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
          END DO
        END DO
      END IF           ! regular/irregular boundaries ModeSolver=5
! 
!  zero the points with fixed elevations
! 
      DO i=1,init    
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!  Work out sediment flux for retention ponds
! 
      IF (NoRegions /= 0) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Region(i,j) /= 0) THEN
              sumsed(RegionMap(Region(i,j)))                            &
&                   =sumsed(RegionMap(Region(i,j)))+Sed(i,j)
              IF (Region(i,j) <= 10) THEN
                Sed(i,j)=0
              END IF
            END IF
          END DO
        END DO
      END IF
      RETURN
END SUBROUTINE sedanal7

! 
! ===========================================================
!  The Dinifinity version of ModeSolver=7
! ===========================================================
! 
SUBROUTINE SedAnalDinf7(Sed,Dsed,s0,AreaTerm1,AreaTerm2,factor           &
&           ,dZdX2,Uplift,Y,z,Direct,DirectDinf,DirWeights               &
&           ,Area,IrregularBoundary,Domain                               &
&           ,TwoFluvialProcesses,QsHoldT,Zout,Zin,init,iXXX,iYYY         &
&           ,TimeStep,NoRegions,RegionMap,Region,SumSed,DetCIF           &
&           ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
  USE SiberiaConstants
  USE Setup
  USE SiberiaTypes
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: init,iXXX(init),iYYY(init)                                  &
&        ,NoRegions,RegionMap(*),LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct,Region,DirectDinf
    REAL(KIND(0.0D0)) :: QsHoldT,Zout,Zin,TimeStep,SumSed(*)
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Sed,Dsed,s0,DirWeights               &
&        ,AreaTerm1,Area,AreaTerm2,factor,dZdX2,Uplift,Y,z
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)                       &
&        ,TwoFluvialProcesses,DetCIF
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ix,iy,nxti,nxtj,nxti2,nxtj2
    REAL(KIND(0.0D0)) :: st1(1:gridX),sdt1(1:gridX),Slope,slopen1,temp
    LOGICAL :: Channel
! 
      IF (DetCIF) THEN
        CALL Message_Output(Message_ErrorStop,' Error in SedAnal: '        &
&         //'Deterministic channels are not compatible with ModeSolver=7')
      END IF
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          Sed(ix,iy)=0.0
          Dsed(ix,iy)=0.0
        END DO
      END DO
! 
!   looking after the sediment inflow/outflow effects
!   ------------------------------------------
!               Irregular Boundary
! 
      IF (IrregularBoundary) THEN
        DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
              slopen1=s0(ix,iy)**SimParameters%n1
              Slope=s0(ix,iy)
              st1(ix)=AreaTerm1(ix,iy)*slopen1
              IF (SimParameters%n1 /= 1.0) THEN 
                IF (Slope /= 0.0) THEN
                  sdt1(ix)=                                               &
&                       SimParameters%n1*invlgth(invdir(Direct(ix,iy)))   &
&                          *st1(ix)/Slope
                ELSE
                    sdt1(ix)=0
                END IF
              ELSE
                sdt1(ix)=                                                 &
&                      SimParameters%n1*invlgth(invdir(Direct(ix,iy)))    &
&                       *AreaTerm1(ix,iy)
              END IF
              st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
              st1(ix)=max(0.0d0,dble(st1(ix)))
              sdt1(ix)=sdt1(ix)*factor(ix,iy)
              Sed(ix,iy)=Sed(ix,iy)-st1(ix)
              Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
            END IF
          END DO
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
!  main direction
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              IF (DirectDinf(ix,iy) == 0) THEN
                Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
              ELSE
!  main direction
                Sed(nxti,nxtj)=Sed(nxti,nxtj)                            &
&                  +(1-DirWeights(ix,iy))*st1(ix)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)                          &
&                  +(1-DirWeights(ix,iy))*sdt1(ix)
!  subsidary direction
                nxti2=ix+dir1(DirectDinf(ix,iy))
                nxtj2=iy+dir2(DirectDinf(ix,iy))
                Sed(nxti2,nxtj2)=Sed(nxti2,nxtj2)                        &
&                  +DirWeights(ix,iy)*st1(ix)
                Dsed(nxti2,nxtj2)=Dsed(nxti2,nxtj2)                      &
&                  +DirWeights(ix,iy)*sdt1(ix)
              END IF
!              nxti=ix+dir1(Direct(ix,iy))
!              nxtj=iy+dir2(Direct(ix,iy))
!              Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
!              Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
            END IF
          END DO
        END DO       ! iy=2,ky
      ELSE
! 
!               Regular Boundary
! 
        DO iy=LowY,HighY
! 
!   Sed & Dsed are for the Newtons method determination of the
!   equilibrium elevations
! 
          DO ix=LowX,HighX
            Slope=s0(ix,iy)
            slopen1=s0(ix,iy)**SimParameters%n1
            st1(ix)=AreaTerm1(ix,iy)*slopen1
            IF (SimParameters%n1 /= 1.0) THEN
              IF (Slope /= 0.0) THEN 
                sdt1(ix)=                                                 &
&                     SimParameters%n1*invlgth(invdir(Direct(ix,iy)))     &
&                     *st1(ix)/Slope
              ELSE
                sdt1(ix)=0
              END IF
            ELSE
              sdt1(ix)=                                                   &
&                 SimParameters%n1*invlgth(invdir(Direct(ix,iy)))         &
&                 *AreaTerm1(ix,iy)
            END IF
            st1(ix)=st1(ix)*factor(ix,iy)-QsHoldT
            st1(ix)=max(0.0d0,dble(st1(ix)))
            sdt1(ix)=sdt1(ix)*factor(ix,iy)
            Sed(ix,iy)=Sed(ix,iy)-st1(ix)
            Dsed(ix,iy)=Dsed(ix,iy)+sdt1(ix)
          END DO
          DO ix=LowX,HighX
!  main direction
              nxti=ix+dir1(Direct(ix,iy))
              nxtj=iy+dir2(Direct(ix,iy))
              IF (DirectDinf(ix,iy) == 0) THEN
                Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
              ELSE
!  main direction
                Sed(nxti,nxtj)=Sed(nxti,nxtj)                             &
&                  +(1-DirWeights(ix,iy))*st1(ix)
                Dsed(nxti,nxtj)=Dsed(nxti,nxtj)                           &
&                  +(1-DirWeights(ix,iy))*sdt1(ix)
!  subsidary direction
                nxti2=ix+dir1(DirectDinf(ix,iy))
                nxtj2=iy+dir2(DirectDinf(ix,iy))
                Sed(nxti2,nxtj2)=Sed(nxti2,nxtj2)                         &
&                  +DirWeights(ix,iy)*st1(ix)
                Dsed(nxti2,nxtj2)=Dsed(nxti2,nxtj2)                       &
&                  +DirWeights(ix,iy)*sdt1(ix)
              END IF
!            nxti=ix+dir1(Direct(ix,iy))
!            nxtj=iy+dir2(Direct(ix,iy))
!            Sed(nxti,nxtj)=Sed(nxti,nxtj)+st1(ix)
!            Dsed(nxti,nxtj)=Dsed(nxti,nxtj)+sdt1(ix)
          END DO
        END DO      ! iy=2,ky
      END IF
! 
!   Mass Balance
! 
      DO i=1,init    
        Zout=Sed(abs(iXXX(i)),abs(iYYY(i)))*TimeStep                       &
&             +dZdX2(abs(iXXX(i)),abs(iYYY(i)))
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!   the actual amount that Z(ix,iy) will changed in the next time step
! 
      IF (IrregularBoundary) THEN
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            IF (Domain(ix,iy)) THEN
              IF (SimParameters%b5*(SimParameters%b3                       &
&                 *Area(ix,iy)**SimParameters%m3)**SimParameters%m5        &
&                 *s0(ix,iy)**SimParameters%n5*SimParameters%c1            &
&                          > 0.005) THEN
                Channel=.true.
              ELSE
                Channel=.false.
              END IF
              IF (Area(ix,iy) /= 1) THEN
                IF(Dsed(ix,iy) /= 0) THEN
                  temp=(1-SimParameters%n1)*Dsed(ix,iy)*timestep
                  IF (temp < 0.99999999) THEN
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                    &
&                       *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                  ELSE
                    Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                  END IF
                ELSE
                  Sed(ix,iy)=0
                END IF
              ELSE
                Sed(ix,iy)=Sed(ix,iy)*timestep
              END IF
              IF (Channel) THEN
                Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)    ! channel
              ELSE
                Sed(ix,iy)=SimParameters%OTime*Sed(ix,iy)+dZdX2(ix,iy)               ! hillslope
              END IF
              Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
              Zin=Zin+Sed(ix,iy)
! 
!   Tectonic uplift term
! 
              Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
            END IF
          END DO
        END DO
      ELSE             ! Regular Boundary
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            IF (SimParameters%b5*(SimParameters%b3                         &
&               *Area(ix,iy)**SimParameters%m3)**SimParameters%m5          &
&               *s0(ix,iy)**SimParameters%n5*SimParameters%c1              &
&                   > 0.005) THEN
              Channel=.true.
            ELSE
              Channel=.false.
            END IF
            IF (Area(ix,iy) /= 1) THEN
              IF(Dsed(ix,iy) /= 0) THEN
                temp=(1-SimParameters%n1)*Dsed(ix,iy)*timestep
                IF (temp < 0.99999999) THEN
                  Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)                       &
&                      *((1/(1-temp))**(1/(SimParameters%n1-1))-1)
                ELSE
                  Sed(ix,iy)=-Sed(ix,iy)/Dsed(ix,iy)
                END IF
              ELSE
                Sed(ix,iy)=0
              END IF
            ELSE
              Sed(ix,iy)=Sed(ix,iy)*timestep
            END IF
            IF (Channel) THEN
              Sed(ix,iy)=Sed(ix,iy)+dZdX2(ix,iy)
            ELSE
              Sed(ix,iy)=SimParameters%OTime*Sed(ix,iy)*dZdX2(ix,iy)
            END IF
            Sed(ix,iy)=Sed(ix,iy)/SimParameters%Bulk
            Zin=Zin+Sed(ix,iy)
            Sed(ix,iy)=Sed(ix,iy)+Uplift(ix,iy)
          END DO
        END DO
      END IF           ! regular/irregular boundaries ModeSolver=7
! 
!  zero the points with fixed elevations
! 
      DO i=1,init    
        Sed(abs(iXXX(i)),abs(iYYY(i)))=0
      END DO
! 
!  Work out sediment flux for retention ponds
! 
      IF (NoRegions /= 0) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Region(i,j) /= 0) THEN
              sumsed(RegionMap(Region(i,j)))                            &
&                   =sumsed(RegionMap(Region(i,j)))+Sed(i,j)
              IF (Region(i,j) <= 10) THEN
                Sed(i,j)=0
              END IF
            END IF
          END DO
        END DO
      END IF
      RETURN
END SUBROUTINE sedanaldinf7


END MODULE SedAnalysis

