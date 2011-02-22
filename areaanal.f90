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
MODULE AreaAnalysis

  REAL,PARAMETER :: AreaAnalysis_Version=8.19

  INTEGER,private :: LowXX,HighXX,LowYY,HighYY
  LOGICAL,private,allocatable,dimension(:,:) :: DoneCalcs
  INTEGER,private,allocatable,dimension(:,:) :: DirectCalcs         &
&               ,DirectDinfCalcs
  REAL(KIND(0.0D0)),private,allocatable,dimension(:,:)  ::         &
&               AreaCalcs,DirWeightsCalcs
CONTAINS

! 
! ======================================================================
!  Analyse the directions created by DirAnal TO determine drainage areas
!  for all the grids
! ======================================================================
! 
SUBROUTINE AreaAnal(Area,Direct,DirectDinf,DirWeights                       &
&               ,Domain,AreaError,IrregularBoundary                         &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS           &
&               ,SimParameters,GridX,GridY)
  USE Support
  USE SiberiaTypes
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),DirectDinf(GridX,GridY),LowX,HighX,LowY,HighY    &
&          ,NoFlowIn,FlowInIJ(2,*)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),Area(GridX,GridY),DirWeights(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),AreaError,IrregularBoundary
    TYPE(LocalParameters) :: SimParameters
! 
!  IF D8 USE direct solver
! 
      IF (ModeArea == 0) THEN
        IF (.not.(SimParameters%ModeDir == 7.or.SimParameters%ModeDir == 8)) THEN
          ModeArea=2
        ELSE
          ModeArea=3
        END IF
      END IF
! 
      SELECT CASE (ModeArea)
      CASE DEFAULT
        call Message_Output(Message_ErrorStop,'Invalid Area Analysis Mode')
! 
!    Direct solver for D8
! 
      CASE (1)
        CALL AreaAnalD8D(Area,Direct,Domain,IrregularBoundary                 &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
! 
!    RECURSIVE solver for D8
! 
      CASE (2)
        CALL AreaAnalD8R(Area,Direct                                          &
&               ,Domain,AreaError,IrregularBoundary                           &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
! 
!    RECURSIVE solver for Dinf
! 
      CASE (3)
        CALL AreaAnalDinf(Area,Direct,DirectDinf,DirWeights                   &
&               ,Domain,AreaError,IrregularBoundary                           &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
      END SELECT
      RETURN
END SUBROUTINE AreaAnal
! 
! ======================================================================
!  Analyse the directions created by DirAnal TO determine drainage areas
!  for all the grids (Direct Version of the routine)
! ======================================================================
! 
SUBROUTINE AreaAnalD8D(Area,Direct,Domain,IrregularBoundary                  &
&           ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),LowX,HighX,LowY,HighY,NoFlowIn                     &
&         ,FlowInIJ(2,*)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),Area(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary
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
          Area(i,j)=1
        END DO
      END DO
      DO i=1,NoFlowIn
        Area(FlowInIJ(1,i),FlowInIJ(2,i))=                                  &
&            Area(FlowInIJ(1,i),FlowInIJ(2,i))+FlowInAS(1,i)
      END DO
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
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          Area(i,j)=Area(i,j)+Area(Oldi,Oldj)
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
END SUBROUTINE AreaAnalD8D
! 
! ======================================================================
!  Analyse the directions created by DirAnal TO determine drainage areas
!  for all the grids (RECURSIVE Version of the routine). Basically just
!  sets up and calcs the RECURSIVE area engine
! ======================================================================
! 
SUBROUTINE AreaAnalD8R(Area,Direct,Domain,AreaError,IrregularBoundary             &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),LowX,HighX,LowY,HighY             &
&       ,NoFlowIn,FlowInIJ(2,*)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),Area(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),AreaError,IrregularBoundary
! 
    INTEGER :: i,j
! 
      LowXX=LowX
      HighXX=HighX
      LowYY=LowY
      HighYY=HighY
      if (.not.allocated(DoneCalcs)) then
        allocate(DoneCalcs(LowX:HighX,LowY:HighY))
        allocate(AreaCalcs(LowX:HighX,LowY:HighY))
        allocate(DirectCalcs(LowX:HighX,LowY:HighY))
      end if
      DO j=LowY,HighY
        DO i=LowX,HighX
          DoneCalcs(i,j)=.false.
          AreaCalcs(i,j)=Area(i,j)
          DirectCalcs(i,j)=Direct(i,j)
        END DO
      END DO
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (.not.DoneCalcs(i,j)) CALL GetAreaD8(i,j)
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (.not.DoneCalcs(i,j)) CALL GetAreaD8(i,j)
          END DO
        END DO
      END IF
      DO j=LowY,HighY
        DO i=LowX,HighX
          Area(i,j)=AreaCalcs(i,j)
        END DO
      END DO
      AreaError=.false.
      RETURN
END SUBROUTINE AreaAnalD8R
! 
! ======================================================================
!  THE RECURSIVE engine of the D8 area calculations
! ======================================================================
! 
RECURSIVE SUBROUTINE GetAreaD8(i,j)
  USE SiberiaConstants
    IMPLICIT NONE
    INTEGER :: i,j
! 
    INTEGER :: k,ii,jj
    REAL(KIND(0.0D0)) :: AreaTemp
! 
      AreaTemp=1
      DO k=1,8
        ii=i+nodei(k)
        jj=j+nodej(k)
        IF (ii >= LowXX.and.jj >= LowYY                                        &
&           .and.ii <= HighXX.and.jj <= HighYY) THEN
          IF (DirectCalcs(ii,jj) == dircomp(k)) THEN
            IF (.not.DoneCalcs(ii,jj)) CALL GetAreaD8(ii,jj)
            AreaTemp=AreaTemp+AreaCalcs(ii,jj)
          END IF
        END IF
      END DO
      AreaCalcs(i,j)=AreaTemp
      DoneCalcs(i,j)=.true.
      RETURN
END SUBROUTINE GetAreaD8
! 
! ======================================================================
!  Analyse the directions created by DirAnal TO determine drainage areas
!  for all the grids (Direct Version of the routine)
! ======================================================================
! 
SUBROUTINE AreaAnalDinf(Area,Direct,DirectDinf,DirWeights                       &
&               ,Domain,AreaError,IrregularBoundary                             &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
  USE SiberiaConstants
  IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),DirectDinf(GridX,GridY)                                      &
&          ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ(2,*)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),Area(GridX,GridY),DirWeights(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),AreaError,IrregularBoundary
! 
    INTEGER :: i,j
! 
      LowXX=LowX
      HighXX=HighX
      LowYY=LowY
      HighYY=HighY
      if (.not.allocated(DoneCalcs)) then
        allocate(DoneCalcs(LowX:HighX,LowY:HighY))
        allocate(AreaCalcs(LowX:HighX,LowY:HighY))
        allocate(DirWeightsCalcs(LowX:HighX,LowY:HighY))
        allocate(DirectCalcs(LowX:HighX,LowY:HighY))
        allocate(DirectDinfCalcs(LowX:HighX,LowY:HighY))
      end if
      DO j=LowY,HighY
        DO i=LowX,HighX
          DoneCalcs(i,j)=.false.
          AreaCalcs(i,j)=Area(i,j)
          DirectCalcs(i,j)=Direct(i,j)
          DirectDinfCalcs(i,j)=DirectDinf(i,j)
! 
!  'GetAreaDinf' expects the weights TO be all +ve
! 
          DirWeightsCalcs(i,j)=abs(DirWeights(i,j))
        END DO
      END DO
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (.not.DoneCalcs(i,j)) CALL GetAreaDinf(i,j)
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (.not.DoneCalcs(i,j)) CALL GetAreaDinf(i,j)
          END DO
        END DO
      END IF
      DO j=LowY,HighY
        DO i=LowX,HighX
          Area(i,j)=AreaCalcs(i,j)
        END DO
      END DO
      AreaError=.false.
      RETURN
END SUBROUTINE AreaAnalDinf
! 
! ======================================================================
!  THE RECURSIVE engine of the Dinfarea calculations
! ======================================================================
! 
RECURSIVE SUBROUTINE GetAreaDinf(i,j)
  USE SiberiaConstants
    IMPLICIT NONE
    INTEGER :: i,j
! 
    INTEGER :: k,ii,jj
    REAL(KIND(0.0D0)) :: AreaTemp
! 
      AreaTemp=1
      DO k=1,8
        ii=i+nodei(k)
        jj=j+nodej(k)
        IF (ii >= LowXX.and.jj >= LowYY                                   &
&           .and.ii <= HighXX.and.jj <= HighYY) THEN
! 
!  main direction drains that point
! 
          IF (DirectCalcs(ii,jj) == dircomp(k)) THEN
            IF (.not.DoneCalcs(ii,jj)) CALL GetAreaDinf(ii,jj)
            AreaTemp=AreaTemp+(1-DirWeightsCalcs(ii,jj))                  &
&             *AreaCalcs(ii,jj)
! 
!  subsidary direction drains TO that point
! 
          ELSE IF (DirectDinfCalcs(ii,jj) == dircomp(k)) THEN
            IF (.not.DoneCalcs(ii,jj)) CALL GetAreaDinf(ii,jj)
            AreaTemp=AreaTemp+DirWeightsCalcs(ii,jj)*AreaCalcs(ii,jj)
          END IF
        END IF
      END DO
      AreaCalcs(i,j)=AreaTemp
      DoneCalcs(i,j)=.true.
      RETURN
END SUBROUTINE GetAreaDinf
! 
! ======================================================================
!  Analyse the directions created by DirAnal TO determine drainage areas
!  for all the grids (Direct Version of the routine)
! ======================================================================
! 
SUBROUTINE AveAnal(Answer,Indata,Direct                                 &
&          ,IrregularBoundary,Domain,LowX,HighX,LowY,HighY,GridX,GridY)
  USE SiberiaConstants
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: Answer(GridX,GridY),Indata(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary
! 
    INTEGER :: i,j,k,ii,jj,NoSources,Oldi,Oldj
    INTEGER,dimension(LowX:HighX,LowY:HighY) :: NoIn,Sum
    INTEGER,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY
! 
      NoSources=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          Answer(i,j)=1
        END DO
      END DO
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
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          IF (NoIn(i,j) == 1) THEN
            Answer(i,j)=Answer(i,j)+InData(Oldi,Oldj)
            Sum(i,j)=Sum(i,j)+1
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
            Answer(i,j)=Answer(i,j)+Indata(Oldi,Oldj)
            NoIn(i,j)=NoIn(i,j)-1  
            Sum(i,j)=Sum(i,j)+1
          END IF
        END IF
      END DO
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (sum(i,j) /= 0) THEN
            answer(i,j)=answer(i,j)/sum(i,j)
          ELSE
            answer(i,j)=0
          END IF
        END DO
      END DO
      RETURN
END SUBROUTINE AveAnal


! 
! ======================================================================
!  Analyse the directions created by DirAnal TO determine drainage areas
!  for all the grids (Direct Version of the routine)
! ======================================================================
! 
SUBROUTINE AreaAnalD8DXY(Area,Direct,Domain,IrregularBoundary              &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ(2,*)
    REAL(KIND(0.0D0)) :: FlowInAS(2,*)
    TYPE(ArrayIXY) :: Direct
    TYPE(ArrayR8XY) :: Area
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary
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
          Area%data(i,j)=1
        END DO
      END DO
      DO i=1,NoFlowIn
        Area%data(FlowInIJ(1,i),FlowInIJ(2,i))=                            &
&            Area%data(FlowInIJ(1,i),FlowInIJ(2,i))+FlowInAS(1,i)
      END DO
! 
!  Find the list of the most upstream points of the networks
! 
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              ii=i+dir1(Direct%data(i,j))
              jj=j+dir2(Direct%data(i,j))
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
            ii=i+dir1(Direct%data(i,j))
            jj=j+dir2(Direct%data(i,j))
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
        i=SourceX(k)+dir1(Direct%data(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct%data(Oldi,Oldj))
 1060   IF (i /= Oldi.or.j /= Oldj) THEN
          Area%data(i,j)=Area%data(i,j)+Area%data(Oldi,Oldj)
          IF (NoIn(i,j) == 1) THEN
            Oldi=i
            Oldj=j
            i=i+dir1(Direct%data(Oldi,Oldj))
            j=j+dir2(Direct%data(Oldi,Oldj))
            GO TO 1060
          ELSE
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
      RETURN
END SUBROUTINE AreaAnalD8DXY

END MODULE AreaAnalysis
