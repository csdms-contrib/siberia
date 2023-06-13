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
MODULE SoilAnalysis

  REAL,PARAMETER :: SoilAnalysis_Version=8.24

CONTAINS

SUBROUTINE SoilAnal(Area,Slope,Sed,SoilDepth,Z                          &
&                ,SoilZ,Domain,IrregularBoundary,TimeStep               &
&                ,LowX,HighX,LowY,HighY,SimParameters,Erosion           &
&                ,Init,iXXX,iYYY,SM                                     &                                   
&                ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections   &
&                ,gridX,gridY)
  USE SiberiaTypes
  USE Support
  USE PitAnal
  USE DirAnalysis
  USE AreaAnalysis
  USE Others
  USE openMPsupport
    IMPLICIT NONE
! 
! ====================================================================
!    THE SOILS MODULE
! ====================================================================
! 
    INTEGER :: LowX,HighX,LowY,HighY,Init,iXXX(*),iYYY(*),gridX,gridY
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: Slope,Sed,SoilDepth,z,SoilZ,Area
    TYPE(ArrayR8XY) :: Erosion,SM,BedRockZ,BedRockArea,BedRockSlope,BedRockZPit
    TYPE(ArrayIXY) :: BedrockDirections
    REAL(KIND(0.0D0)) :: TimeStep
    LOGICAL :: Domain(gridX,gridY),IrregularBoundary
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ErrorNo,NoPits
    REAL(KIND(0.0D0)),dimension(LowX:HighX,LowY:HighY) :: DepthChange
    integer :: FlowInIJ(2,1),NoFlowIn
    REAL(KIND(0.0D0)) :: FlowInAS(2,1)
    logical :: SMFast,dirchg
! 
    select case (SimParameters%ModeSoil)
    case default
      return
!
!  soil weathering based on soil surface topographic index
!  -------------------------------------------------------
!
    case (1)
      SMFast=.false.
      if (SimParameters%SDExp1 == 1.0) then
        SMFast=.true.
      end if
      DepthChange=0
      SM%data=0
      if (IrregularBoundary) then
        DO j=LowY,HighY
          DO i=LowX,HighX
            if (Domain(i,j)) then
              IF (Slope(i,j) /= 0.0 .and. SoilDepth(i,j) /= 0.0) THEN
                SM%data(i,j)=Area(i,j)*SimParameters%SMThreshold/(Slope(i,j)*SoilDepth(i,j))
              ELSE
                SM%data(i,j)=1
              END IF
              SM%data(i,j)=min(1.0d0,SM%data(i,j))
              if (SMFast) then
                DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&                 *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                    *SM%data(i,j))                                                 &
&                 *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                  +Erosion%Data(i,j)
              else
                DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&                 *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                    *SM%data(i,j)**SimParameters%SDExp1)                           &
&                 *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                  +Erosion%Data(i,j)
              end if
            end if
          END DO
        END DO
      else
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Slope(i,j) /= 0.0 .and. SoilDepth(i,j) /= 0.0) THEN
              SM%data(i,j)=Area(i,j)*SimParameters%SMThreshold/(Slope(i,j)*SoilDepth(i,j))
            ELSE
              SM%data(i,j)=1
            END IF
            SM%data(i,j)=min(1.0d0,SM%data(i,j))
            if (SMFast) then
              DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&               *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                  *SM%data(i,j))                                                 &
&               *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                +Erosion%Data(i,j)
            else
              DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&               *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                  *SM%data(i,j)**SimParameters%SDExp1)                           &
&               *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                +Erosion%Data(i,j)
            end if
          END DO
        END DO
      end if
!
!  soil weathering based on bedrock surface topographic index
!  ----------------------------------------------------------
!   ModeSoil=2 :: analysis without pitfilling of bedrock elevations
!   ModeSoil=3 :: analysis with pitfilling
!
    case (2,3)
      if (FirstSoilAnal) then
        call AllocateArray(BedRockZ,LowX-1,HighX+1,LowY-1,HighY+1,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('BedRockZ','SIBERIA_SOILANAL')
        call AllocateArray(BedRockArea,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('BedRockArea','SIBERIA_SOILANAL')
        call AllocateArray(BedRockSlope,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('BedRockSlope','SIBERIA_SOILANAL')
        call AllocateArray(BedRockDirections,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('BedRockDirections','SIBERIA_SOILANAL')
        FirstSoilAnal=.false.
      end if
      BedRockZ%data(LowX:HighX,LowY:HighY)=Z(LowX:HighX,LowY:HighY)                &
&            -SoilDepth(LowX:HighX,LowY:HighY)
      CALL SetBCXY(BedRockZ,IrregularBoundary,LowX,HighX,LowY,HighY)
      DirChg=.true.
      NoPits=0
      select case (SimParameters%ModeSoil)

      case (2)
        call DirAnal0XY(IrregularBoundary,Domain,BedRockZ,NoPits                      &
&           ,BedRockDirections,BedRockSlope,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
        NoFlowIn=0
        call AreaAnalD8DXY(BedRockArea,BedRockDirections,Domain,IrregularBoundary    &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)

      case(3)
        call AllocateArray(BedRockZPit,LowX-1,HighX+1,LowY-1,HighY+1,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('BedRockZPit','SIBERIA_SOILANAL')
        BedRockZPit%data(LowX-1:HighX+1,LowY-1:HighY+1)=                                  &
&               BedRockZ%data(LowX-1:HighX+1,LowY-1:HighY+1)
!          prepare for pitfilling
 1001   CALL SetBCXY(BedRockZPit,IrregularBoundary,LowX,HighX,LowY,HighY)
        call DirAnal0XY(IrregularBoundary,Domain,BedRockZPit,NoPits                      &
&           ,BedRockDirections,BedRockSlope,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
!          pitfill
        CALL PitAnal2XY(BedRockZPit,BedRockSlope,BedRockDirections,IrregularBoundary   &
&              ,Domain,LowX,HighX,LowY,HighY,GridX,GridY)
!          directions after pitfilling
        CALL SetBCXY(BedRockZPit,IrregularBoundary,LowX,HighX,LowY,HighY)
        call DirAnal0XY(IrregularBoundary,Domain,BedRockZPit,NoPits                      &
&           ,BedRockDirections,BedRockSlope,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
        if (NoPits == 1 ) go to 1000
          go to 1001

 1000   call DeallocateArray(BedRockZPit,ErrorNo)
        NoFlowIn=0
!          areas from pitfilled elevations
        call AreaAnalD8DXY(BedRockArea,BedRockDirections,Domain,IrregularBoundary    &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,GridX,GridY)
!   calc slopes on the unpitfilled elevations
        call DirAnal0XY(IrregularBoundary,Domain,BedRockZ,NoPits                      &
&           ,BedRockDirections,BedRockSlope,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
      end select
      SMFast=.false.
      if (SimParameters%SDExp1 == 1.0) then
        SMFast=.true.
      end if
      DepthChange=0
      SM%data=0
      if (IrregularBoundary) then
        DO j=LowY,HighY
          DO i=LowX,HighX
            if (Domain(i,j)) then
              IF (BedRockSlope%data(i,j) /= 0.0 .and. SoilDepth(i,j) /= 0.0) THEN
                SM%data(i,j)=BedRockArea%data(i,j)*SimParameters%SMThreshold        &
&                                /(BedRockSlope%data(i,j)*SoilDepth(i,j))
              ELSE
                SM%data(i,j)=1
              END IF
              SM%data(i,j)=min(1.0d0,SM%data(i,j))
              if (SMFast) then
                DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&                 *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                    *SM%data(i,j))                                                 &
&                 *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                  +Erosion%Data(i,j)
              else
                DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&                 *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                    *SM%data(i,j)**SimParameters%SDExp1)                           &
&                 *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                  +Erosion%Data(i,j)
              end if
            end if
          END DO
        END DO
      else
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (BedRockSlope%Data(i,j) /= 0.0 .and. SoilDepth(i,j) /= 0.0) THEN
              SM%data(i,j)=BedRockArea%data(i,j)*SimParameters%SMThreshold          &
&                                /(BedRockSlope%data(i,j)*SoilDepth(i,j))
            ELSE
              SM%data(i,j)=1
            END IF
            SM%data(i,j)=min(1.0d0,SM%data(i,j))
            if (SMFast) then
              DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&               *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                  *SM%data(i,j))                                                 &
&               *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                +Erosion%Data(i,j)
            else
              DepthChange(i,j)=TimeStep*(SimParameters%SDRate                     &
&               *((1-SimParameters%SDSMWgt)+SimParameters%SDSMWgt                 &
&                  *SM%data(i,j)**SimParameters%SDExp1)                           &
&               *exp(-SimParameters%SDExp2*SoilDepth(i,j)))                       &
&                +Erosion%Data(i,j)
            end if
          END DO
        END DO
      end if
    end select
!   do i=1,Init
!     DepthChange(iXXX(i),iYYY(i))=0
!   end do
    SoilDepth(LowX:HighX,LowY:HighY)=                                           &
&              SoilDepth(LowX:HighX,LowY:HighY)+DepthChange(LowX:HighX,LowY:HighY)
    DO j=LowY,HighY
      DO i=LowX,HighX
        if (SoilDepth(i,j) < 0) SoilDepth(i,j)=0.0
      end do
    end do
! 
    RETURN
END SUBROUTINE SoilAnal

END MODULE SoilAnalysis
