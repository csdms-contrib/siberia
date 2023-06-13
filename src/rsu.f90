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
MODULE RSUOutput

  REAL,PARAMETER :: RSU_Version=8.26

CONTAINS
! 
! ======================================================================
!  Yield analysis for RSU output
! ======================================================================
! 
SUBROUTINE YieldAnal(Area,Direct,kx1,ky1                                &
&               ,Domain,AreaError,IrregularBoundary                     &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS       &
&               ,DATA,DataAve,ModeRunoff,GridX,GridY)
  USE SiberiaConstants
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),kx1,ky1,LowX,HighX,LowY,HighY,NoFlowIn        &
&          ,FlowInIJ(2,*),ModeRunoff
    REAL    :: dataave(GridX,GridY)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),DATA(GridX,GridY),Area(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),AreaError,IrregularBoundary
! 
    INTEGER :: i,j,k,ii,jj,NoSources,Oldi,Oldj
    INTEGER :: NoIn(GridX,GridY)
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY
! 
      areaerror=.false.
! 
      NoSources=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          Area(i,j)=1
          dataave(i,j)=0
        END DO
      END DO
      IF (ModeRunoff == 2) THEN
        DO i=1,NoFlowIn
          Area(FlowInIJ(1,i),FlowInIJ(2,i))=                              &
&            Area(FlowInIJ(1,i),FlowInIJ(2,i))+FlowInAS(1,i)
        END DO
      END IF
! 
!  Find the list of the most upstream points of the networks
! 
      IF (IrregularBoundary) THEN
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
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            ii=i+dir1(Direct(i,j))
            jj=j+dir2(Direct(i,j))
            IF (i /= ii.or.j /= jj) THEN
              NoIn(ii,jj)=NoIn(ii,jj)+1
            END IF
          END DO
        END DO
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (NoIn(i,j) == 0) THEN
              NoSources=NoSources+1
              SourceX(NoSources)=i
              SourceY(NoSources)=j
            END IF
          END DO
        END DO
      END IF
! 
!  Calc the areas from top TO bottom
! 
      DO k=1,NoSources
        Oldi=SourceX(k)
        Oldj=SourceY(k)
        DataAve(Oldi,Oldj)=DATA(Oldi,Oldj)
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          IF (NoIn(i,j) == 1) THEN
            Area(i,j)=Area(i,j)+Area(Oldi,Oldj)
            DataAve(i,j)=DataAve(Oldi,Oldj)+DATA(i,j)+DataAve(i,j)
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
            Area(i,j)=Area(i,j)+Area(Oldi,Oldj)
            DataAve(i,j)=DataAve(Oldi,Oldj)+DataAve(i,j)
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF(Area(i,j) > 0) THEN
            DataAve(i,j)=DataAve(i,j)/Area(i,j)
          ELSE
            DataAve(i,j)=0
          END IF
        END DO
      END DO
      AreaError=.false.
      RETURN
END SUBROUTINE YieldAnal


SUBROUTINE YieldAnal1(Area,Direct,kx1,ky1                                &
&               ,Domain,AreaError,IrregularBoundary                     &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS       &
&               ,DATA,DataAve,ModeRunoff,GridX,GridY)
  USE SiberiaConstants
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),kx1,ky1,LowX,HighX,LowY,HighY,NoFlowIn        &
&          ,FlowInIJ(2,*),ModeRunoff
    REAL    :: dataave(GridX,GridY)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),Area(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),AreaError,IrregularBoundary
    TYPE(ArrayR8XY) :: Data
! 
    INTEGER :: i,j,k,ii,jj,NoSources,Oldi,Oldj
    INTEGER :: NoIn(GridX,GridY)
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY
! 
      areaerror=.false.
! 
      NoSources=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          Area(i,j)=1
          dataave(i,j)=0
        END DO
      END DO
      IF (ModeRunoff == 2) THEN
        DO i=1,NoFlowIn
          Area(FlowInIJ(1,i),FlowInIJ(2,i))=                              &
&            Area(FlowInIJ(1,i),FlowInIJ(2,i))+FlowInAS(1,i)
        END DO
      END IF
! 
!  Find the list of the most upstream points of the networks
! 
      IF (IrregularBoundary) THEN
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
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            ii=i+dir1(Direct(i,j))
            jj=j+dir2(Direct(i,j))
            IF (i /= ii.or.j /= jj) THEN
              NoIn(ii,jj)=NoIn(ii,jj)+1
            END IF
          END DO
        END DO
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (NoIn(i,j) == 0) THEN
              NoSources=NoSources+1
              SourceX(NoSources)=i
              SourceY(NoSources)=j
            END IF
          END DO
        END DO
      END IF
! 
!  Calc the areas from top TO bottom
! 
      DO k=1,NoSources
        Oldi=SourceX(k)
        Oldj=SourceY(k)
        DataAve(Oldi,Oldj)=DATA%data(Oldi,Oldj)
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          IF (NoIn(i,j) == 1) THEN
            Area(i,j)=Area(i,j)+Area(Oldi,Oldj)
            DataAve(i,j)=DataAve(Oldi,Oldj)+DATA%data(i,j)+DataAve(i,j)
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
            Area(i,j)=Area(i,j)+Area(Oldi,Oldj)
            DataAve(i,j)=DataAve(Oldi,Oldj)+DataAve(i,j)
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF(Area(i,j) > 0) THEN
            DataAve(i,j)=DataAve(i,j)/Area(i,j)
          ELSE
            DataAve(i,j)=0
          END IF
        END DO
      END DO
      AreaError=.false.
      RETURN
END SUBROUTINE YieldAnal1
! 
! ======================================================================
!  output of the RSU files
! ======================================================================
! 
SUBROUTINE RSUOut(FileName,Sed,Area,directions,Domain                       &
&              ,Z,Zoriginal,Slope,IrregularBoundary                         &
&              ,NoFlowIn,FlowInIJ,FlowInAS,InitTimeStep1                    &
&              ,LowX,HighX,LowY,HighY,DirWeights,SimParameters              &
&              ,Discharge,Erosion,SM                                        &
&              ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections         &
&              ,Predictor_sed,Corrector_sed,GridX,GridY)
  USE AreaAnalysis
  USE LayerConstants
  USE LayerSupport
  USE Setup
  USE SiberiaConstants
  USE SiberiaTypes
  USE Support
  USE Multipliers
  USE SedAnalysis
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,directions(GridX,GridY),LowX,HighX,LowY,HighY     &
&         ,NoFlowIn,FlowInIJ(2,*)
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Sed,Z,Zoriginal,Slope,Area   &
&         ,DirWeights,Discharge
    TYPE(ArrayR8XY) :: Erosion,SM,BedrockZ,BedRockArea,BedRockSlope          &
&         ,Predictor_sed,Corrector_sed
    TYPE(ArrayIXY) :: BedrockDirections
    REAL(KIND(0.0D0)) ::FlowInAS(2,*),InitTimeStep1
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary
    CHARACTER(*) :: FileName
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,k,lgth,unitno,ii,jj,ErrorNo,itemp,Layer,Tracer
    REAL    :: RSUData(GridX,GridY,NoRSUPara),minvalue
    REAL(KIND(0.0D0)) :: work(GridX,GridY),S0threshold,alpha1,n1inv,temp     &
&             ,b1,b3,m1,n1,m3,dvalue,temp1,temp2
    LOGICAL :: AreaError,ValidData(NoRSUPara),SomeValidData
    character(80) :: Filename1
! 
! 
    DATA unitno / 20 /
! 
      IF (NoRSUPara == 0) RETURN
      DO j=1,SimParameters%ky
        DO i=1,SimParameters%kx
          DO k=1,NoRSUPara
            RSUData(i,j,k)=0
          END DO
        END DO
      END DO
      DO k=1,NoRSUPara
        ValidData(k)=.true.
        select case (RSUPara(k))
        case default
          CALL Message_Output(Message_ErrorContinue                            &
&              ,'Invalid output option in RSUOut '                             &
&              ,k,' RSU parameter requested = ',RSUPara(k)                     &
&              ,' No RSU parameters =',NoRSUPara)
          ValidData=.false.
          RETURN

!  Instantaneous point Elevation changes in mm (assumes elevations are in m)
!  -------------------------------------------------------------------------

        case(1)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                        &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=Erosion%Data(i,j)/InitTimeStep1*1.e+3
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!  Instantaneous spatially averaged Elevation changes  (assumes elevations are in m)
!  ---------------------------------------------------------------------------------

        case(2)
          CALL YieldAnal1(Area,Directions,SimParameters%kx,SimParameters%ky                                    &
&               ,Domain,AreaError,IrregularBoundary                             &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS               &
&               ,Erosion,RSUData(:,:,k),SimParameters%ModeRunoff,GridX,GridY)
!&               ,Sed,RSUData(:,:,k),SimParameters%ModeRunoff)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                        &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=RSUData(i,j,k)/InitTimeStep1*1.e+3
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!  temporally averaged point Elevation changes  (assumes elevations are in m)
!  --------------------------------------------------------------------------
        case(3)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=(Z(i,j)-Zoriginal(i,j))*1.e+3
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!  temporally and spatially averaged Elevation changes  (assumes elevations are in m)
!  ----------------------------------------------------------------------------------

        case(4)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                work(i,j)=(Z(i,j)-Zoriginal(i,j))*1.e+3
              ELSE
                work(i,j)=0
              END IF
            END DO
          END DO
          CALL YieldAnal(Area,Directions,SimParameters%kx,SimParameters%ky                                    &
&               ,Domain,AreaError,IrregularBoundary                                &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                  &
&               ,work,RSUData(:,:,k),SimParameters%ModeRunoff,GridX,GridY)

!  Gully Potential from shear stress
!  ---------------------------------

        case(5)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=Area(i,j)**SimParameters%m1                         &
&                      *Slope(i,j)**SimParameters%n1
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!  log(Gully Potential) from shear stress
!  --------------------------------------

        case(6)
          minvalue=1.0e10
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                IF (Slope(i,j) /= 0.0.and.Area(i,j) /= 0.0) THEN
                  temp=Area(i,j)**SimParameters%m1*Slope(i,j)**SimParameters%n1
                  RSUData(i,j,k)=log10(temp)
                  minvalue=min(minvalue,RSUData(i,j,k))
                END IF
              END IF
            END DO
          END DO
          DO j=LowY,HighY
            DO i=1,SimParameters%kx
              IF (Slope(i,j) == 0.0.or.Area(i,j) == 0.0                             &
&              .or.(IrregularBoundary.and.(.not.Domain(i,j)))) THEN
                RSUData(i,j,k)=minvalue
              END IF
            END DO
          END DO

!  Suggested elevations from QUEL
!  ------------------------------

        case(7)

! Suggested elevations from QUEL (differences from original elevations)
! ---------------------------------------------------------------------

        case(8)

!  Instantaneous point Elevation changes in tonnes/Ha
!  --------------------------------------------------

        case(9)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=-Sed(i,j)/InitTimeStep1                             &
&                           *SimParameters%Bulk*1.0e+4
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!  Instantaneous spatially averaged Elevation changes in T/Ha
!  ----------------------------------------------------------

        case(10)
          CALL YieldAnal(Area,Directions,SimParameters%kx,SimParameters%ky                                    &
&               ,Domain,AreaError,IrregularBoundary                               &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                 &
&               ,Sed,RSUData(:,:,k),SimParameters%ModeRunoff,GridX,GridY)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                          &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=-RSUData(i,j,k)/InitTimeStep1                      &
&                    *SimParameters%Bulk*1.0e+4
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!   Stability number
!   ----------------

        case(11)
          IF (SimParameters%GridXY /= 1) THEN
            S0threshold=1.0e-3*SimParameters%GridXY
          ELSE
            S0threshold=1.0e-3
          END IF
          DO j=LowY,HighY
            DO i=LowX,HighX
              RSUData(i,j,k)=0
              IF ((IrregularBoundary.and.Domain(i,j))                             &
&                .or.(.not.IrregularBoundary)) THEN
                ii=i+dir1(directions(i,j))
                jj=j+dir2(directions(i,j))
                IF (Slope(i,j) > S0threshold.and.                                 &
&                   Slope(ii,jj) > S0threshold) THEN
                  IF (abs(directions(i,j)) < 10) THEN
                    RSUData(i,j,k)=abs(Sed(i,j)-Sed(ii,jj))                       &
&                     /Slope(i,j)/abs(InitTimeStep1)
                  ELSE
                    RSUData(i,j,k)=0.7071*abs(Sed(i,j)-Sed(ii,jj))                &
&                     /Slope(i,j)/abs(InitTimeStep1)
                  END IF
                END IF
              END IF
            END DO
          END DO

!   Area-Slope number
!   -----------------

        case(12)
          alpha1=(SimParameters%m1*SimParameters%m3-1)/SimParameters%n1
          DO j=LowY,HighY
            DO i=LowX,HighX
              RSUData(i,j,k)=0
              IF ((IrregularBoundary.and.Domain(i,j))                             &
&                .or.(.not.IrregularBoundary)) THEN
                    RSUData(i,j,k)=Slope(i,j)*Area(i,j)**alpha1
              END IF
            END DO
          END DO

!   Area-Slope-Elevation number
!   ---------------------------

        case(13)
          alpha1=-(SimParameters%m1*SimParameters%m3-1)/SimParameters%n1
          n1inv=-1.0/SimParameters%n1
          CALL AveAnal(work,z,directions                                         &
&             ,IrregularBoundary,Domain,LowX,HighX,LowY,HighY,GridX,GridY)
          DO j=LowY,HighY
            DO i=LowX,HighX
              RSUData(i,j,k)=0
              IF ((IrregularBoundary.and.Domain(i,j))                            &
&              .or.(.not.IrregularBoundary)) THEN
                  IF (work(i,j) /= 0.0) THEN
                    RSUData(i,j,k)=Slope(i,j)*Area(i,j)**alpha1                  &
&                    *work(i,j)**n1inv
                  ELSE
                    RSUData(i,j,k)=0.0
                  END IF
              END IF
            END DO
          END DO

!   FLUVIAL-DIFFUSION number
!   ------------------------

        case(14)
          alpha1=SimParameters%m1*SimParameters%m3-1
          temp=SimParameters%b1*SimParameters%b3**SimParameters%m1
          DO j=LowY,HighY
            DO i=LowX,HighX
              RSUData(i,j,k)=0
              IF ((IrregularBoundary.and.Domain(i,j))                             &
&              .or.(.not.IrregularBoundary)) THEN
                  IF (Area(i,j) > 0) THEN
                    RSUData(i,j,k)=temp*Area(i,j)**alpha1*Slope(i,j)**SimParameters%n1         &
&                       +SimParameters%dZ*Slope(i,j)**SimParameters%dZn/Area(i,j)
                  ELSE
                    RSUData(i,j,k)=0.0
                  END IF
              END IF
            END DO
          END DO

!   Dinf Weights
!   ------------
        case(15)
          DO j=LowY,HighY
            DO i=LowX,HighX
              RSUData(i,j,k)=0
              IF ((IrregularBoundary.and.Domain(i,j))                             &
&              .or.(.not.IrregularBoundary)) THEN
                  RSUData(i,j,k)=DirWeights(i,j)
              END IF
            END DO
          END DO

!   Mean Annual Discharge
!   ---------------------

        case(16)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=discharge(i,j)
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!   Soil Moisture
!   -------------

        case(17)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                RSUData(i,j,k)=SM%data(i,j)
              ELSE
                RSUData(i,j,k)=0
              END IF
            END DO
          END DO

!   Bedrock Z
!   ---------

        case(18)
          if (associated(BedrockZ%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=BedrockZ%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Bedrock Area
!   ------------

        case(19)
          if (associated(BedrockArea%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=BedrockArea%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Bedrock Directions
!   ------------------

        case(20)
          if (associated(BedrockDirections%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=BedrockDirections%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Bedrock Slope
!   -------------

        case(21)
          if (associated(BedrockSlope%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=BedrockSlope%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Predictor SED
!   -------------

        case(22)
          if (associated(Predictor_sed%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=Predictor_sed%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Corrector SED
!   -------------

        case(23)
          if (associated(Corrector_sed%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=Corrector_sed%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Predictor-Corrector SED difference
!   ----------------------------------

        case(24)
          if (associated(Corrector_sed%data).and.associated(Predictor_sed%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  RSUData(i,j,k)=Predictor_sed%data(i,j)-Corrector_sed%data(i,j)
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if

!   Predictor-Corrector SED ratio
!   -----------------------------

        case(25)
          if (associated(Corrector_sed%data).and.associated(Predictor_sed%data)) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                  if (Predictor_sed%data(i,j) /= 0.0 ) then
                    RSUData(i,j,k)=                                                    &
&                     abs((Predictor_sed%data(i,j)-Corrector_sed%data(i,j))           &
&                        /Predictor_sed%data(i,j))
                  else
                    RSUData(i,j,k)=0
                  end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO
          else
            ValidData(k)=.false.
          end if
!
!   Sediment Flux
!   -------------

        case(26)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF (Layer_InitI()) then
                        call Layer_Get(Layer_b3,i,j,1,b3,ErrorNo)
                        call Layer_Get(Layer_m3,i,j,1,m3,ErrorNo)
                        call Layer_Get(Layer_b1,i,j,1,b1,ErrorNo)
                        call Layer_Get(Layer_m1,i,j,1,m1,ErrorNo)
                        call Layer_Get(Layer_n1,i,j,1,n1,ErrorNo)
                        b3=b3*MultiplierRealVar(18)
                        b1=b1*MultiplierRealVar(19)
                        m3=m3*MultiplierRealVar(17)
                        m1=m1*MultiplierRealVar(20)
                        n1=n1*MultiplierRealVar(21)
                      else
                        b1=SimParameters%b1*MultiplierRealVar(19)
                        b3=SimParameters%b3*MultiplierRealVar(18)
                        m3=SimParameters%m3*MultiplierRealVar(17)
                        m1=SimParameters%m1*MultiplierRealVar(20)
                        n1=SimParameters%n1*MultiplierRealVar(21)
                      end if
                      RSUData(i,j,k)=b1*(b3*(area(i,j)*MultiplierArea)**m3)**m1       &
&                        *(slope(i,j)*MultiplierSlope)**n1
                      if (SimParameters%ModeErode >=20 .or.                  &
&                         SimParameters%ModeRunoff >= 20) then
                        RSUData(i,j,k)=RSUData(i,j,k)/SimParameters%GridXY
                      end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Potential Sediment Flux
!   -----------------------

        case(27)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    call Sed_Get(Sed_PotentialQs,i,j,dvalue,ErrorNo)
                    if (ErrorNo == 0) then
!    normalise for the timestep size
                      RSUData(i,j,k)=dvalue/InitTimeStep1
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Actual Sediment Flux
!   --------------------

        case(28)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    call Sed_Get(Sed_ActualQs,i,j,dvalue,ErrorNo)
                    if (ErrorNo == 0) then
!    normalise for the timestep size
                      RSUData(i,j,k)=dvalue/InitTimeStep1
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO


!   Surface B1 (from Layers)
!   ------------------------

        case(29)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_b1,i,j,1,temp,ErrorNo)
                      RSUData(i,j,k)=temp*MultiplierRealVar(19)
!                      RSUData(i,j,k)=temp
                    else
                      RSUData(i,j,k)=SimParameters%b1*MultiplierRealVar(19)
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Flow B1 (from Layers)
!   ------------------------

        case(30)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_Flowb1,i,j,temp,ErrorNo)
                      RSUData(i,j,k)=temp*MultiplierRealVar(19)
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO


!   Number of Layers
!   ----------------

        case(31)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_NoLayers,i,j,itemp,ErrorNo)
                      RSUData(i,j,k)=itemp
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Layer B1
!   --------

        case(32:36)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_NoLayers,i,j,itemp,ErrorNo)
                      Layer=RSUPara(k)-31
                      if (Layer > itemp) then          !  requested layer doesn't exist
                        RSUData(i,j,k)=0
                      else
                        call Layer_Get(Layer_b1,i,j,Layer,temp,ErrorNo)
                        RSUData(i,j,k)=temp*MultiplierRealVar(19)
                      end if
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Layer Z
!   --------

        case(37:41)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_NoLayers,i,j,itemp,ErrorNo)
                      Layer=RSUPara(k)-36
                      if (Layer > itemp) then          !  requested layer doesn't exist
                        RSUData(i,j,k)=0
                      else
                        call Layer_Get(Layer_Z,i,j,Layer,temp,ErrorNo)
                        RSUData(i,j,k)=temp
                      end if
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO


!   Layer Depth
!   --------

        case(42:46)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_NoLayers,i,j,itemp,ErrorNo)
                      Layer=RSUPara(k)-41
                      if (itemp >= Layer) then
                        if (RSUPara(k) == 42) then          !  top layer
                          temp1=z(i,j)
                          call Layer_Get(Layer_Z,i,j,1,temp2,ErrorNo)
                        else
                          call Layer_Get(Layer_Z,i,j,Layer-1,temp1,ErrorNo)
                          call Layer_Get(Layer_Z,i,j,Layer,temp2,ErrorNo)
                        end if
                        RSUData(i,j,k)=temp1-temp2
                      else
                        RSUData(i,j,k)=0
                      end if
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Domain mask
!   ------------------------

        case(47)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  RSUData(i,j,k)=1
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Flow Detachment (from Layers)
!   ------------------------

        case(48)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_FlowDetachment,i,j,1,temp,ErrorNo)
                      RSUData(i,j,k)=temp
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Layer Detach
!   ------------

        case(49:53)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_NoLayers,i,j,itemp,ErrorNo)
                      Layer=RSUPara(k)-48
                      if (Layer > itemp) then          !  requested layer doesn't exist
                        RSUData(i,j,k)=0
                      else
                        call Layer_Get(Layer_Detachment,i,j,Layer,temp,ErrorNo)
                        RSUData(i,j,k)=temp
                      end if
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Flow Tracer
!   ------------

        case(54:58)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_Number_Tracers,i,j,itemp,ErrorNo)
                      Tracer=RSUPara(k)-53
                      if (Tracer > itemp) then          !  requested tracer doesn't exist
                        RSUData(i,j,k)=0
                      else
                        call Layer_Get(Layer_FlowTracer(Tracer),i,j,temp,ErrorNo)
                        RSUData(i,j,k)=temp
                      end if
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

!   Layer Tracer
!   ------------

! NOTE: This code assumes output is for up to 5 tracers and 5 layers ... otherwise need to change step in loop
        case(59:83)
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (.not.IrregularBoundary                                           &
&                .or.(IrregularBoundary.and.Domain(i,j))) THEN
                    IF(Layer_InitI()) then
                      call Layer_Get(Layer_NoLayers,i,j,itemp,ErrorNo)
                      layer=mod((RSUPara(k)-59)/5,5)+1
                      if (layer > itemp) then          !  requested layer doesn't exist
                        RSUData(i,j,k)=0
                      else
                        tracer=mod(RSUPara(k)-59,5)+1
                        call Layer_Get(Layer_Tracer(tracer),i,j,layer,temp,ErrorNo)
                        RSUData(i,j,k)=temp
                      end if
                    else
                      RSUData(i,j,k)=0
                    end if
                ELSE
                  RSUData(i,j,k)=0
                END IF
              END DO
            END DO

        end select
!       if (ValidData(k)) then
!          CALL Message_Output(Message_Info,' -- RSU Output '//trim(RSUColumnTitles(RSUPara(k))))
!       end if
      END DO
! 
!  Output of DATA TO file
! 
      SomeValidData=.false.
      do i=1,NoRSUPara
        if (ValidData(i)) then
          SomeValidData=.true.
          exit
        end if
      end do
      if (.not.SomeValidData) return
!  RSU file output
      OPEN(UNIT=unitno,FILE=FileName,STATUS='unknown',ERR=9999)
      WRITE (unitno,6000) 'SIBERIA  ',Version
6000  format(a,f4.2)
      WRITE (unitno,*) ' RSU output from SIBERIA'
      WRITE (unitno,*)
      WRITE (unitno,*)
      WRITE (unitno,*) SimParameters%kx,SimParameters%ky,NoRSUPara
      WRITE (unitno,6020)                                                       &
&       (' '''//trim(RSUColumnTitles(RSUPara(i)))//''' ',i=1,NoRSUPara)
 6020 FORMAT (100a)
      DO j=1,SimParameters%ky
        DO i=1,SimParameters%kx
          WRITE (unitno,6010) (RSUData(i,j,k),k=1,NoRSUPara)
 6010     FORMAT(1000(' ',e12.5))
        END DO
      END DO
      CLOSE(UNIT=unitno,STATUS='keep')
      CALL Message_Output(Message_Info,'RSU file '//trim(FileName)//' output')
!  BINARY file output if requested
      do k=1,NoRSUPara
        if (OutputFileType(k) == FileTypeBIN) then
!  construct the filename and assume filename ends with extension .rsu
          filename1=' '
          lgth=len_trim(FileName)
          filename1=filename(1:lgth-4)//'.'//trim(output_abbrev(RSUPara(k)))//'.bin'
          open(unit=unitno,file=filename1,status='unknown',err=9998,form='unformatted'    &
&             ,access='direct',recl=2+SimParameters%kx*SimParameters%ky)
          write (unitno,rec=1) SimParameters%kx,SimParameters%ky                        &
&                ,((rsudata(i,j,k),i=1,SimParameters%kx),j=1,SimParameters%ky)
          close(unit=unitno,status='keep')
        end if
      end do
      RETURN
! 
 9998 CONTINUE
      CALL Message_Output(Message_ErrorContinue,'Cannot OPEN '//trim(FileName1))
      RETURN
 9999 CONTINUE
      CALL Message_Output(Message_ErrorContinue,'Cannot OPEN '//trim(FileName))
      RETURN
END SUBROUTINE RSUOut

END MODULE RSUOutput
