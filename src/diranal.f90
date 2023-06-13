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
MODULE DirAnalysis

  REAL, PARAMETER ::DirAnalysis_Version=8.19

CONTAINS

!                                               
! ================================================================
!   Analysis of the elevations for the flow directions.
!   This routine implements Discrete flow directions.
! ================================================================
!     
SUBROUTINE DirAnal(Z,Direct,DirectDinf                                        &
&         ,s0,Y,cDepth,DirChg,iself,Domain,IrregularBoundary                  &
&         ,iInit,iXXX,iYYY,DirWeights,LowX,HighX,LowY,HighY,SimParameters     &
&         ,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Support
  USE UserDirAnalysis
    IMPLICIT NONE
! 
    INTEGER :: iself,iInit,iXXX(*),iYYY(*),LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct,DirectDinf
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Z,s0,cDepth,Y,DirWeights
    LOGICAL :: DirChg, Domain(gridX,gridY), IrregularBoundary
    TYPE(LocalParameters) :: SimParameters
!                
    INTEGER :: i,j
    REAL(KIND(0.0D0)) :: ZZ(gridX,gridY)
!
    DirChg=.false.
    SELECT CASE (SimParameters%ModeChannel)
    CASE (1,2)
      IF (IrregularBoundary) THEN
        DO j=LowY-1,HighY+1
          DO i=LowX-1,HighX+1
            IF (Domain(i,j)) THEN
              IF (y(i,j) < 0.85) THEN
                ZZ(i,j)=z(i,j)
              ELSE
                ZZ(i,j)=z(i,j)-cDepth(i,j)
              END IF
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY-1,HighY+1
          DO i=LowX-1,HighX+1
            IF (y(i,j) < 0.85) THEN
              ZZ(i,j)=z(i,j)
            ELSE
              ZZ(i,j)=z(i,j)-cDepth(i,j)
            END IF
          END DO
        END DO
      END IF
      DO i=1,iInit
        IF (Y(abs(iXXX(i)),abs(iYYY(i))) > 0.85) THEN
          ZZ(abs(iXXX(i)),abs(iYYY(i)))=                  &
&             ZZ(abs(iXXX(i)),abs(iYYY(i)))               &
&                +cDepth(abs(iXXX(i)),abs(iYYY(i)))
        END IF
      END DO
    CASE DEFAULT
      DO j=LowY-1,HighY+1
        DO i=LowX-1,HighX+1
          ZZ(i,j)=z(i,j)
        END DO
      END DO
    END SELECT
    IF (SimParameters%ModeDir == 1) THEN
      SimParameters%ModeDir=0
      call Message_Output(Message_WarnContinue,'ModeDir=1 no longer supported ... changing to ModeDir=0')
    END IF
    SELECT CASE (SimParameters%ModeDir)
      CASE(0,1)
!  directions as steepest slopes (no constraints on drainage)
        CALL DirAnal0(IrregularBoundary,Domain,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
!  the following mode was eliminated after it was found to cause major mass balance 
!  problems in certain unusual chapped mining landforms
!      CASE(1)
! directions Channel -> Channel
!        CALL DirAnal1(IrregularBoundary,Domain,y,ZZ,iself               &
!&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,SimParameters)
      CASE(2)
! ctr bank directions constraints
        CALL DirAnal2(IrregularBoundary,Domain,y,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,SimParameters       &
&           ,gridX,gridY)
      CASE(3)
!  directions fixed as in the input .rst2 file
        CALL DirAnal3(IrregularBoundary,Domain,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
      CASE(4)
!  no channels optimised code ... functionally identical to ModeDir=0 but
!  some machines have this code significantly faster than Modedir=0 (others
!  are vice versa). 
        CALL DirAnal4(IrregularBoundary,Domain,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
      CASE(5)
!  random directions based on Moglen multiple directions
        CALL DirAnal5(IrregularBoundary,Domain,y,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY     &
&           ,SimParameters,gridX,gridY)
      CASE(6)
!  directions based wider Region than just adjacent neighbours
        CALL DirAnal6(IrregularBoundary,Domain,y,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,SimParameters       &
&           ,gridX,gridY)
      CASE(7)
!  directions based on tarboton D(infinity) method
        CALL DirAnal78(IrregularBoundary,Domain,ZZ                    &
&           ,iself,Direct,DirectDinf,s0,DirChg,DirWeights               &
&           ,LowX,HighX,LowY,HighY,.true.,gridX,gridY)
      CASE(8)
!  directions based on faster approx. implementation 
!     tarboton D(infinity) method
        CALL DirAnal78(IrregularBoundary,Domain,ZZ                    &
&           ,iself,Direct,DirectDinf,s0,DirChg,DirWeights               &
&           ,LowX,HighX,LowY,HighY,.false.,gridX,gridY)
      CASE(9)
!  an old (<=8.16) version of diranal0 maintained for compatability only
        CALL DirAnal9(IrregularBoundary,Domain,y,ZZ,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
      CASE (:-1)
        CALL UserDirAnal(IrregularBoundary,Domain,y,ZZ                  &
&           ,cDepth,iself,Direct,s0,DirChg                              &
&           ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorStop                           &
&             ,'Invalid Directions Analysis Mode ='                     &
&             ,SimParameters%ModeDir)  
      END SELECT
      DO i=1,iInit
        IF (Direct(iXXX(i),iYYY(i)) /= 5) THEN
          Direct(iXXX(i),iYYY(i))=5
          s0(iXXX(i),iYYY(i))=0
          DirChg=.true.
        END IF
      END DO
      RETURN
END SUBROUTINE diranal
!
! 
! ===================================================================
!                           DIRANAL0
! ===================================================================
!
!
SUBROUTINE DirAnal0(IrregularBoundary,Domain,Z,iself                 &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
  USE SiberiaConstants
    IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
! 
    INTEGER :: ix,iy,imax,direct1
    REAL(KIND(0.0D0)) :: Slope,slopemax
! ========================
!  Irregular Boundaries
! ========================
    iself=0
    IF ( IrregularBoundary) THEN
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            slopemax=-1
            IF (Domain(ix+1,iy)) THEN
              Slope=Z(ix,iy)-Z(ix+1,iy)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=1
              END IF
            END IF
!     
            IF (Domain(ix,iy+1)) THEN
              Slope=Z(ix,iy)-Z(ix,iy+1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=3
              END IF
            END IF
!       
            IF (Domain(ix-1,iy)) THEN
              Slope=Z(ix,iy)-Z(ix-1,iy)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=5
              END IF
            END IF
!       
            IF (Domain(ix,iy-1)) THEN
              Slope=Z(ix,iy)-Z(ix,iy-1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=7
              END IF
            END IF
!       
            IF (Domain(ix+1,iy+1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
            END IF
!       
            IF (Domain(ix-1,iy+1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
            END IF
!       
            IF (Domain(ix-1,iy-1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=6
              END IF
            END IF
!       
            IF (Domain(ix+1,iy-1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END IF
        END DO
      END DO
    ELSE
! ==============================
!        Regular Boundaries
! ==============================
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          slopemax=-1
          Slope=Z(ix,iy)-Z(ix+1,iy)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=1
          END IF
!       
          Slope=Z(ix,iy)-Z(ix,iy+1)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=3
          END IF
!       
          Slope=Z(ix,iy)-Z(ix-1,iy)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=5
          END IF
!       
          Slope=Z(ix,iy)-Z(ix,iy-1)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=7
          END IF
!       
          Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=2
          END IF
!       
          Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=4
          END IF
!       
          Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
          IF (Slope > slopemax) THEN
             slopemax=Slope
            imax=6
          END IF
!       
          Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=8
          END IF
!       
          IF (slopemax <= 0.0) THEN
            IF (slopemax < 0.0) THEN
              iself=iself+1
            END IF
            direct1=5
          ELSE
            direct1=dir(imax)
          END IF
          IF (Direct(ix,iy) /= direct1) THEN
            Direct(ix,iy)=direct1
            DirChg=.true.
          END IF                       
          IF (slopemax < 0.0) THEN
            s0(ix,iy)=0
          ELSE
            s0(ix,iy)=slopemax
          END IF
        END DO
      END DO
    END IF
    RETURN
END SUBROUTINE diranal0


SUBROUTINE DirAnal0XY(IrregularBoundary,Domain,Z,iself                 &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
  USE SiberiaTypes
  USE SiberiaConstants
    IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    TYPE(ArrayIXY) :: direct
    TYPE(ArrayR8XY) :: Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
! 
    INTEGER :: ix,iy,imax,direct1
    REAL(KIND(0.0D0)) :: Slope,slopemax,ZZ
! ========================
!  Irregular Boundaries
! ========================
    iself=0
    IF ( IrregularBoundary) THEN
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            ZZ=Z%data(ix,iy)
            slopemax=-1
            IF (Domain(ix+1,iy)) THEN
              Slope=ZZ-Z%data(ix+1,iy)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=1
              END IF
            END IF
!     
            IF (Domain(ix,iy+1)) THEN
              Slope=ZZ-Z%data(ix,iy+1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=3
              END IF
            END IF
!       
            IF (Domain(ix-1,iy)) THEN
              Slope=ZZ-Z%data(ix-1,iy)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=5
              END IF
            END IF
!       
            IF (Domain(ix,iy-1)) THEN
              Slope=ZZ-Z%data(ix,iy-1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=7
              END IF
            END IF
!       
            IF (Domain(ix+1,iy+1)) THEN
              Slope=0.707*(ZZ-Z%data(ix+1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
            END IF
!       
            IF (Domain(ix-1,iy+1)) THEN
              Slope=0.707*(ZZ-Z%data(ix-1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
            END IF
!       
            IF (Domain(ix-1,iy-1)) THEN
              Slope=0.707*(ZZ-Z%data(ix-1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=6
              END IF
            END IF
!       
            IF (Domain(ix+1,iy-1)) THEN
              Slope=0.707*(ZZ-Z%data(ix+1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              direct1=5
              if (ix > lowX .and. ix < HighX .and. iy > lowY .and. iy < HighY) then
                iself=iself+1
              end if
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct%data(ix,iy) /= direct1) THEN
              Direct%data(ix,iy)=direct1
              DirChg=.true.
            END IF
            IF (slopemax < 0.0) THEN
              s0%data(ix,iy)=0
            ELSE
              s0%data(ix,iy)=slopemax
            END IF
          END IF
        END DO
      END DO
    ELSE
! ==============================
!        Regular Boundaries
! ==============================
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          slopemax=-1
          ZZ=Z%data(ix,iy)
          Slope=ZZ-Z%data(ix+1,iy)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=1
          END IF
!       
          Slope=ZZ-Z%data(ix,iy+1)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=3
          END IF
!       
          Slope=ZZ-Z%data(ix-1,iy)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=5
          END IF
!       
          Slope=ZZ-Z%data(ix,iy-1)
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=7
          END IF
!       
          Slope=0.707*(ZZ-Z%data(ix+1,iy+1))
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=2
          END IF
!       
          Slope=0.707*(ZZ-Z%data(ix-1,iy+1))
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=4
          END IF
!       
          Slope=0.707*(ZZ-Z%data(ix-1,iy-1))
          IF (Slope > slopemax) THEN
             slopemax=Slope
            imax=6
          END IF
!       
          Slope=0.707*(ZZ-Z%data(ix+1,iy-1))
          IF (Slope > slopemax) THEN
            slopemax=Slope
            imax=8
          END IF
!       
          IF (slopemax <= 0.0) THEN
            direct1=5
              if (ix > lowX .and. ix < HighX .and. iy > lowY .and. iy < HighY) then
                iself=iself+1
              end if
          ELSE
            direct1=dir(imax)
          END IF
          IF (Direct%data(ix,iy) /= direct1) THEN
            Direct%data(ix,iy)=direct1
            DirChg=.true.
          END IF                       
          IF (slopemax < 0.0) THEN
            s0%data(ix,iy)=0
          ELSE
            s0%data(ix,iy)=slopemax
          END IF
        END DO
      END DO
    END IF
    RETURN
END SUBROUTINE diranal0XY
!
! 
! ===================================================================
!                           DIRANAL1
! ===================================================================
!
!
!         ModeDir = 1:  Channels constrained TO flow into a channel
!                       cannot flow into a hillslope. No constraints
!                       on hillslope drainage.
! 
! ====================================================================
! 
SUBROUTINE DirAnal1(IrregularBoundary,Domain,y,Z,iself             &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,SimParameters  &
&           ,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: y,Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: ix,iy,imax,iix,iiy,direct1
    REAL(KIND(0.0D0)) :: Slope,slopemax,ChannelThreshold
    LOGICAL :: diff
! 
! ========================================================
!   Irregular Boundaries
! ========================================================
! 
    iself=0
    ChannelThreshold=SimParameters%YHold
    IF ( IrregularBoundary) THEN
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            slopemax=-1
            Diff=y(ix,iy) > ChannelThreshold
            IF (Domain(ix+1,iy)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix+1,iy)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=1
                END IF
              END IF
            END IF
!       
            IF (Domain(ix,iy+1)) THEN
              IF (.not.(Diff.and.Y(ix,iy+1) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix,iy+1)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=3
                END IF
              END IF
            END IF
!       
            IF (Domain(ix-1,iy)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix-1,iy)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=5
                END IF
              END IF
            END IF
!       
            IF (Domain(ix,iy-1)) THEN
              IF (.not.(Diff.and.Y(ix,iy-1) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix,iy-1)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=7
                END IF
              END IF
            END IF
!       
            IF (Domain(ix+1,iy+1)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy+1) < ChannelThreshold)) THEN
                IF ((.not.Domain(ix+1,iy)).or.(.not.Domain(ix,iy+1))       &
&                    .or.(Domain(ix+1,iy).and.Domain(ix,iy+1).and.         &
&                         .not.(Y(ix+1,iy) > ChannelThreshold.and.         &
&                               Y(ix,iy+1) > ChannelThreshold))) THEN
!            IF (((Z(ix+1,iy) > Z(ix+1,iy+1).and.Domain(ix+1,iy))      &
!&                .or.(.not.Domain(ix+1,iy))).and.                      &
!&              ((Z(ix,iy+1) > Z(ix+1,iy+1).and.Domain(ix,iy+1))       &
!&                .or.(.not.Domain(ix,iy+1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=2
                  END IF
                END IF
!            END IF
              END IF
            END IF
!       
            IF (Domain(ix-1,iy+1)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy+1) < ChannelThreshold)) THEN
                IF ((.not.Domain(ix-1,iy)).or.(.not.Domain(ix,iy+1))       &
&                    .or.(Domain(ix-1,iy).and.Domain(ix,iy+1).and.         &
&                         .not.(Y(ix-1,iy) > ChannelThreshold.and.         &
&                               Y(ix,iy+1) > ChannelThreshold))) THEN
!            IF (((Z(ix-1,iy) > Z(ix-1,iy+1).and.Domain(ix-1,iy))       &
!&                .or.(.not.Domain(ix-1,iy))).and.                       &
!&              ((Z(ix,iy+1) > Z(ix-1,iy+1).and.Domain(ix,iy+1))        &
!&                .or.(.not.Domain(ix,iy+1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=4
                  END IF
                END IF
!            END IF
              END IF
            END IF
!       
            IF (Domain(ix-1,iy-1)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy-1) < ChannelThreshold)) THEN
                IF ((.not.Domain(ix-1,iy)).or.(.not.Domain(ix,iy-1))       &
&                    .or.(Domain(ix-1,iy).and.Domain(ix,iy-1).and.         &
&                         .not.(Y(ix-1,iy) > ChannelThreshold.and.         &
&                               Y(ix,iy-1) > ChannelThreshold))) THEN
!            IF (((Z(ix-1,iy) > Z(ix-1,iy-1).and.Domain(ix-1,iy))       &
!&                .or.(.not.Domain(ix-1,iy))).and.                       &
!&              ((Z(ix,iy-1) > Z(ix-1,iy-1).and.Domain(ix,iy-1))        &
!&                .or.(.not.Domain(ix,iy-1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=6
                  END IF
                END IF
!            END IF
              END IF
            END IF
!       
            IF (Domain(ix+1,iy-1)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy-1) < ChannelThreshold)) THEN
                IF ((.not.Domain(ix+1,iy)).or.(.not.Domain(ix,iy-1))       &
&                    .or.(Domain(ix+1,iy).and.Domain(ix,iy-1).and.         &
&                         .not.(Y(ix+1,iy) > ChannelThreshold.and.         &
&                               Y(ix,iy-1) > ChannelThreshold))) THEN
!            IF (((Z(ix+1,iy) > Z(ix+1,iy-1).and.Domain(ix+1,iy))       &
!&                .or.(.not.Domain(ix+1,iy))).and.                       &
!&              ((Z(ix,iy-1) > Z(ix+1,iy-1).and.Domain(ix,iy-1))        &
!&                .or.(.not.Domain(ix,iy-1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=8
                  END IF
                END IF
!            END IF
              END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              IF (Y(ix,iy) < ChannelThreshold.or.direct1 == 5) THEN
                Direct(ix,iy)=direct1
                DirChg=.true.
              ELSE
                iix=ix+dir1(direct1)
                iiy=iy+dir2(direct1)
                IF (Y(iix,iiy) > ChannelThreshold) THEN
                  DirChg=.true.
                  Direct(ix,iy)=direct1
                END IF
              END IF
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END IF
        END DO
      END DO
    ELSE
! ==============================
!        Regular Boundaries
! ==============================
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          slopemax=-1
          Diff=y(ix,iy) > ChannelThreshold
          IF (.not.(Diff.and.Y(ix+1,iy) < ChannelThreshold)) THEN
            Slope=Z(ix,iy)-Z(ix+1,iy)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=1
            END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix,iy+1) < ChannelThreshold)) THEN
            Slope=Z(ix,iy)-Z(ix,iy+1)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=3
            END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix-1,iy) < ChannelThreshold)) THEN
            Slope=Z(ix,iy)-Z(ix-1,iy)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=5
            END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix,iy-1) < ChannelThreshold)) THEN
            Slope=Z(ix,iy)-Z(ix,iy-1)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=7
            END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix+1,iy+1) < ChannelThreshold)) THEN
          IF (.not.(Y(ix+1,iy) > ChannelThreshold.and.Y(ix,iy+1) > ChannelThreshold)) THEN
!            IF (min(Z(ix+1,iy),Z(ix,iy+1)) > Z(ix+1,iy+1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
!            END IF
          END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix-1,iy+1) < ChannelThreshold)) THEN
          IF (.not.(Y(ix-1,iy) > ChannelThreshold.and.Y(ix,iy+1) > ChannelThreshold)) THEN
!            IF (min(Z(ix-1,iy),Z(ix,iy+1)) > Z(ix-1,iy+1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
!            END IF
          END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix-1,iy-1) < ChannelThreshold)) THEN
          IF (.not.(Y(ix-1,iy) > ChannelThreshold.and.Y(ix,iy-1) > ChannelThreshold)) THEN
!            IF (min(Z(ix-1,iy),Z(ix,iy-1)) > Z(ix-1,iy-1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=6
              END IF
!            END IF
          END IF
          END IF
!       
          IF (.not.(Diff.and.Y(ix+1,iy-1) < ChannelThreshold)) THEN
          IF (.not.(Y(ix+1,iy) > ChannelThreshold.and.Y(ix,iy-1) > ChannelThreshold)) THEN
!            IF (min(Z(ix+1,iy),Z(ix,iy-1)) > Z(ix+1,iy-1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
!            END IF
          END IF
          END IF
!       
          IF (slopemax <= 0.0) THEN
            IF (slopemax < 0.0) THEN
              iself=iself+1
            END IF
            direct1=5
          ELSE
            direct1=dir(imax)
          END IF
          IF (Direct(ix,iy) /= direct1) THEN
            IF (Y(ix,iy) < ChannelThreshold.or.direct1 == 5) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            ELSE
              iix=ix+dir1(direct1)
              iiy=iy+dir2(direct1)
              IF (Y(iix,iiy) > ChannelThreshold) THEN
                DirChg=.true.
                Direct(ix,iy)=direct1
              END IF
            END IF
          END IF                       
          IF (slopemax < 0.0) THEN
            s0(ix,iy)=0
          ELSE
            s0(ix,iy)=slopemax
          END IF
        END DO
      END DO
    END IF
    RETURN
END SUBROUTINE diranal1
!
! 
! ===================================================================
!                           DIRANAL2
! ===================================================================
!
!
!              ModeDir=2:  ctr bank directions constraints
! 
! ====================================================================
! 
SUBROUTINE DirAnal2(IrregularBoundary,Domain,y,Z,iself                &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,SimParameters     &
&           ,gridX,gridY)
  USE SiberiaConstants
  USE Support
  USE SiberiaTypes
  USE openMPsupport
  IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: y,Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: ix,iy,imax,iix,iiy,direct1,junk(8),i,j,ikx,iky,k,ErrorNo
    REAL(KIND(0.0D0)) :: Slope,slopemax,ChannelThreshold
    LOGICAL :: diff
    CHARACTER(19) :: line
! 
    ChannelThreshold=SimParameters%YHold
    iself=0
    IF (FirstDiranal2) THEN
      FirstDiranal2=.false.
      IF (SimParameters%FileDirections(1:10) == '          ') THEN
        CALL Message_Output(Message_ErrorStop,'No File Specified for Contour Banks')
      END IF
      OPEN(UNIT=11,FILE=SimParameters%FileDirections,STATUS='old',err=9999)
      READ(11,6001,err=9998,end=9997) line
 6001 FORMAT(a)
      IF(line(1:19) /= ' SIBERIA DIRECTIONS') THEN
        CALL Message_Output(Message_ErrorStop,' Specified directions file invalid'//trim(SimParameters%FileDirections))
      END IF
      DO i=1,3
        READ(11,*,err=9998,end=9997)
      END DO
      READ(11,*,err=9998,end=9997) ikx,iky
      IF (ikx /= SimParameters%kx.or.iky /= SimParameters%ky) THEN
        CALL Message_Output(Message_ErrorStop                                   &
&              ,' -- Dimensions of Grid in directions file ',ikx,',',iky            &
&              ,' DO not match kx,ky',SimParameters%kx,',',SimParameters%ky)
      END IF
      call AllocateArray(allowDir,1,8,1,SimParameters%kx,1,SimParameters%ky,ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError('AllowDir','SIBERIA_DIRANAL2')
      DO j=1,SimParameters%ky
        DO i=1,SimParameters%kx
          READ(11,6000,err=9998,end=9997) (junk(k),k=1,8)
 6000     FORMAT(8i1)
          DO k=1,8
            IF (junk(k) == 0) THEN
              allowDir%data(k,i,j)=.false.
            ELSE
              allowDir%data(k,i,j)=.true.
            END IF
          END DO
        END DO
      END DO
      CLOSE(UNIT=11,STATUS='keep')
    END IF
! 
! ========================================================
!   Irregular Boundaries
! ========================================================
! 
    IF ( IrregularBoundary) THEN
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            slopemax=-1
            Diff=y(ix,iy) > ChannelThreshold
! 
            IF (allowDir%data(1,ix,iy)) THEN
              IF (Domain(ix+1,iy)) THEN
                IF (.not.(Diff.and.Y(ix+1,iy) < ChannelThreshold)) THEN
                  Slope=Z(ix,iy)-Z(ix+1,iy)
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=1
                  END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(3,ix,iy)) THEN
              IF (Domain(ix,iy+1)) THEN
                IF (.not.(Diff.and.Y(ix,iy+1) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix,iy+1)
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=3
                  END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(5,ix,iy)) THEN
              IF (Domain(ix-1,iy)) THEN
                IF (.not.(Diff.and.Y(ix-1,iy) < ChannelThreshold)) THEN
                  Slope=Z(ix,iy)-Z(ix-1,iy)
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=5
                  END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(7,ix,iy)) THEN
              IF (Domain(ix,iy-1)) THEN
                IF (.not.(Diff.and.Y(ix,iy-1) < ChannelThreshold)) THEN
                  Slope=Z(ix,iy)-Z(ix,iy-1)
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=7
                  END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(2,ix,iy)) THEN
              IF (Domain(ix+1,iy+1)) THEN
                IF (.not.(Diff.and.Y(ix+1,iy+1) < ChannelThreshold)) THEN
!            IF (((Z(ix+1,iy) > Z(ix+1,iy+1).and.Domain(ix+1,iy))               &
!&                .or.(.not.Domain(ix+1,iy))).and.               &
!&              ((Z(ix,iy+1) > Z(ix+1,iy+1).and.Domain(ix,iy+1))               &
!&                .or.(.not.Domain(ix,iy+1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=2
                  END IF
!            END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(4,ix,iy)) THEN
              IF (Domain(ix-1,iy+1)) THEN
                IF (.not.(Diff.and.Y(ix-1,iy+1) < ChannelThreshold)) THEN
!            IF (((Z(ix-1,iy) > Z(ix-1,iy+1).and.Domain(ix-1,iy))               &
!&                .or.(.not.Domain(ix-1,iy))).and.               &
!&              ((Z(ix,iy+1) > Z(ix-1,iy+1).and.Domain(ix,iy+1))               &
!&                .or.(.not.Domain(ix,iy+1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=4
                  END IF
!            END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(6,ix,iy)) THEN
              IF (Domain(ix-1,iy-1)) THEN
                IF (.not.(Diff.and.Y(ix-1,iy-1) < ChannelThreshold)) THEN
!            IF (((Z(ix-1,iy) > Z(ix-1,iy-1).and.Domain(ix-1,iy))               &
!&                .or.(.not.Domain(ix-1,iy))).and.               &
!&              ((Z(ix,iy-1) > Z(ix-1,iy-1).and.Domain(ix,iy-1))               &
!&                .or.(.not.Domain(ix,iy-1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=6
                  END IF
!            END IF
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(8,ix,iy)) THEN
              IF (Domain(ix+1,iy-1)) THEN
                IF (.not.(Diff.and.Y(ix+1,iy-1) < ChannelThreshold)) THEN
!            IF (((Z(ix+1,iy) > Z(ix+1,iy-1).and.Domain(ix+1,iy))               &
!&                .or.(.not.Domain(ix+1,iy))).and.               &
!&              ((Z(ix,iy-1) > Z(ix+1,iy-1).and.Domain(ix,iy-1))               &
!&                .or.(.not.Domain(ix,iy-1)))) THEN
                  Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=8
                  END IF
!            END IF
                END IF
              END IF
            END IF
!       
!           IF (slopemax <= 0.0) THEN
            IF (slopemax < 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              IF (Y(ix,iy) < ChannelThreshold.or.direct1 == 5) THEN
                Direct(ix,iy)=direct1
                DirChg=.true.
              ELSE
                iix=ix+dir1(direct1)
                iiy=iy+dir2(direct1)
                IF (Y(iix,iiy) > ChannelThreshold) THEN
                  DirChg=.true.
                  Direct(ix,iy)=direct1
                END IF
              END IF
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END IF
        END DO
      END DO
    ELSE
! 
!  =============================
!   Regular Boundaries
! ==============================
! 
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            slopemax=-1
            Diff=y(ix,iy) > ChannelThreshold
! 
            IF (allowDir%data(1,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix+1,iy)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=1
              END IF
              END IF
            END IF
!       
            IF (allowDir%data(3,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix,iy+1) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix,iy+1)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=3
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(5,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix-1,iy)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=5
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(7,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix,iy-1) < ChannelThreshold)) THEN
                Slope=Z(ix,iy)-Z(ix,iy-1)
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=7
                END IF
              END IF
            END IF
!       
            IF (allowDir%data(2,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy+1) < ChannelThreshold)) THEN
!            IF (min(Z(ix+1,iy),Z(ix,iy+1)) > Z(ix+1,iy+1)) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=2
                END IF
!            END IF
              END IF
            END IF
!       
            IF (allowDir%data(4,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy+1) < ChannelThreshold)) THEN
!            IF (min(Z(ix-1,iy),Z(ix,iy+1)) > Z(ix-1,iy+1)) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=4
                END IF
!            END IF
              END IF
            END IF
!       
            IF (allowDir%data(6,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy-1) < ChannelThreshold)) THEN
!            IF (min(Z(ix-1,iy),Z(ix,iy-1)) > Z(ix-1,iy-1)) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=6
                END IF
!            END IF
              END IF
            END IF
!       
            IF (allowDir%data(8,ix,iy)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy-1) < ChannelThreshold)) THEN
!            IF (min(Z(ix+1,iy),Z(ix,iy-1)) > Z(ix+1,iy-1)) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=8
                END IF
!            END IF
              END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              IF (Y(ix,iy) < ChannelThreshold.or.direct1 == 5) THEN
                Direct(ix,iy)=direct1
                DirChg=.true.
              ELSE
                iix=ix+dir1(direct1)
                iiy=iy+dir2(direct1)
                IF (Y(iix,iiy) > ChannelThreshold) THEN
                  DirChg=.true.
                  Direct(ix,iy)=direct1
                END IF
              END IF
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END DO
        END DO
      END IF
      RETURN
 9999 CALL Message_Output(Message_ErrorStop,'Error opening the contour bank file')
      STOP
 9998 CALL Message_Output(Message_ErrorStop,'Error reading the contour bank file')
      STOP
 9997 CALL Message_Output(Message_ErrorStop,'Premature end of data in the contour bank file')
      STOP
END SUBROUTINE diranal2
!
! 
! ===================================================================
!                           DIRANAL3
! ===================================================================
!
!
!         ModeDir = 3:  Directions fixed as in the input .rst2 file.
! 
! ====================================================================
! 
SUBROUTINE DirAnal3(IrregularBoundary,Domain,Z,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
  USE SiberiaConstants
  USE openMPsupport
  IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
! 
    INTEGER :: ix,iy
    REAL(KIND(0.0D0)) :: Slope
! 
    iself=0
! 
! ========================
!  Irregular Boundaries
! ========================
! 
    IF (IrregularBoundary) THEN
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            SELECT CASE (Direct(ix,iy))
              CASE(3)
                Slope=Z(ix,iy)-Z(ix+1,iy)
              CASE(4)
                Slope=Z(ix,iy)-Z(ix,iy+1)
              CASE(1)
                Slope=Z(ix,iy)-Z(ix-1,iy)
              CASE(2)
                Slope=Z(ix,iy)-Z(ix,iy-1)
              CASE(14)
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
              CASE(11)
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
              CASE(12)
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
              CASE(13)
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
              CASE DEFAULT
                Slope=0
            END SELECT
            IF (slope < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slope
            END IF
          END IF
        END DO
      END DO
! 
! ========================
!  Regular Boundaries
! ========================
! 
    ELSE
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          SELECT CASE (Direct(ix,iy))
            CASE(3)
              Slope=Z(ix,iy)-Z(ix+1,iy)
            CASE(4)
              Slope=Z(ix,iy)-Z(ix,iy+1)
            CASE(1)
              Slope=Z(ix,iy)-Z(ix-1,iy)
            CASE(2)
              Slope=Z(ix,iy)-Z(ix,iy-1)
            CASE(14)
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
            CASE(11)
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
            CASE(12)
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
            CASE(13)
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
            CASE DEFAULT
              Slope=0
          END SELECT
          IF (slope < 0.0) THEN
            s0(ix,iy)=0
          ELSE
            s0(ix,iy)=slope
          END IF
        END DO
      END DO
    END IF
    IF (FirstDiranal3) THEN
      DirChg=.true.
      FirstDiranal3=.false.
    END IF
    RETURN
END SUBROUTINE DirAnal3

!
! 
! ===================================================================
!                           DIRANAL4
! ===================================================================
!
!
!  ModeDir=4: Highly optimised version of ModeDir=0 for RISC processors
!             TO optimise cache hits.
! 
! --------------------------------------------------------------------
! 
!
SUBROUTINE DirAnal4(IrregularBoundary,Domain,Z,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
  USE SiberiaConstants
  IMPLICIT NONE
! 
  INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
  INTEGER,DIMENSION(gridX,gridY) :: Direct
  REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Z,s0
  LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
! 
  INTEGER :: ix,iy,imax,direct1
  REAL(KIND(0.0D0)) :: Slope,slopemax,zx1ym1,zx1y1,zx1yp1,zxp1ym1             &
&           ,zxp1y1,zxp1yp1,zxm1ym1,zxm1y1,zxm1yp1
  LOGICAL :: dx1ym1,dx1y1,dx1yp1,dxp1ym1,dxp1y1,dxp1yp1             &
&           ,dxm1ym1,dxm1y1,dxm1yp1
! 
! ========================================================
!        Irregular Boundaries
! ========================================================
! 
    iself=0
    IF ( IrregularBoundary) THEN
      DO iy=LowY,HighY
        zx1ym1=z(LowX-1,iy-1)
        zx1y1=z(LowX-1,iy)
        zx1yp1=z(LowX-1,iy+1)
        zxp1ym1=z(LowX,iy-1)
        zxp1y1=z(LowX,iy)
        zxp1yp1=z(LowX,iy+1)
        dx1ym1=Domain(LowX-1,iy-1)
        dx1y1=Domain(LowX-1,iy)
        dx1yp1=Domain(LowX-1,iy+1)
        dxp1ym1=Domain(LowX,iy-1)
        dxp1y1=Domain(LowX,iy)
        dxp1yp1=Domain(LowX,iy+1)
        DO ix=LowX,HighX
          zxm1ym1=zx1ym1
          zxm1y1=zx1y1
          zxm1yp1=zx1yp1
          zx1ym1=zxp1ym1
          zx1y1=zxp1y1
          zx1yp1=zxp1yp1
          zxp1ym1=z(ix+1,iy-1)
          zxp1y1=z(ix+1,iy)
          zxp1yp1=z(ix+1,iy+1)
          dxm1ym1=dx1ym1
          dxm1y1=dx1y1
          dxm1yp1=dx1yp1
          dx1ym1=dxp1ym1
          dx1y1=dxp1y1
          dx1yp1=dxp1yp1
          dxp1ym1=Domain(ix+1,iy-1)
          dxp1y1=Domain(ix+1,iy)
          dxp1yp1=Domain(ix+1,iy+1)
          IF (dx1y1) THEN
            slopemax=-1
            IF (dxp1y1) THEN
            Slope=zx1y1-zxp1y1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=1
            END IF
            END IF
!       
            IF (dx1yp1) THEN
            Slope=zx1y1-zx1yp1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=3
            END IF
            END IF
!       
            IF (dxm1y1) THEN
            Slope=zx1y1-zxm1y1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=5
            END IF
            END IF
!       
            IF (dx1ym1) THEN
            Slope=zx1y1-zx1ym1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=7
            END IF
            END IF
!       
            IF (dxp1yp1) THEN
!            IF (((zxp1y1 > zxp1yp1.and.dxp1y1)               &
!&                .or.(.not.dxp1y1)).and.                      &
!&              ((zx1yp1 > zxp1yp1.and.dx1yp1)                &
!&                .or.(.not.dx1yp1))) THEN
                Slope=0.707*(zx1y1-zxp1yp1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
!            END IF
            END IF
!       
            IF (dxm1yp1) THEN
!            IF (((zxm1y1 > zxm1yp1.and.dxm1y1)               &
!&                .or.(.not.dxm1y1)).and.                      &
!&              ((zx1yp1 > zxm1yp1.and.dx1yp1)                &
!&                .or.(.not.dx1yp1))) THEN
                Slope=0.707*(zx1y1-zxm1yp1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
!            END IF
            END IF
!       
            IF (dxm1ym1) THEN
!            IF (((zxm1y1 > zxm1ym1.and.dxm1y1)               &
!&                .or.(.not.dxm1y1)).and.                      &
!&              ((zx1ym1 > zxm1ym1.and.dx1ym1)                &
!&                .or.(.not.dx1ym1))) THEN
                Slope=0.707*(zx1y1-zxm1ym1)
              IF (Slope > slopemax) THEN
                 slopemax=Slope
                imax=6
              END IF
!            END IF
            END IF
!       
            IF (dxp1ym1) THEN
!            IF (((zxp1y1 > zxp1ym1.and.dxp1y1)               &
!&                .or.(.not.dxp1y1)).and.                      &
!&              ((zx1ym1 > zxp1ym1.and.dx1ym1)                &
!&                .or.(.not.dx1ym1))) THEN
                Slope=0.707*(zx1y1-zxp1ym1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
!            END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END IF
        END DO
      END DO
    ELSE
! 
! ==================================
!        Regular Boundaries
! ==================================
! 
        DO iy=LowY,HighY
          zx1ym1=z(LowX-1,iy-1)
          zx1y1=z(LowX-1,iy)
          zx1yp1=z(LowX-1,iy+1)
          zxp1ym1=z(LowX,iy-1)
          zxp1y1=z(LowX,iy)
          zxp1yp1=z(LowX,iy+1)
          DO ix=LowX,HighX
            zxm1ym1=zx1ym1
            zxm1y1=zx1y1
            zxm1yp1=zx1yp1
            zx1ym1=zxp1ym1
            zx1y1=zxp1y1
            zx1yp1=zxp1yp1
            zxp1ym1=z(ix+1,iy-1)
            zxp1y1=z(ix+1,iy)
            zxp1yp1=z(ix+1,iy+1)
            slopemax=-1
            Slope=zx1y1-zxp1y1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=1
            END IF
!       
            Slope=zx1y1-zx1yp1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=3
            END IF
!       
            Slope=zx1y1-zxm1y1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=5
            END IF
!       
            Slope=zx1y1-zx1ym1
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=7
            END IF
!       
!            IF (min(zxp1y1,zx1yp1) > zxp1yp1) THEN
              Slope=0.707*(zx1y1-zxp1yp1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
!            END IF
!       
!            IF (min(zxm1y1,zx1yp1) > zxm1yp1) THEN
              Slope=0.707*(zx1y1-zxm1yp1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
!            END IF
!       
!            IF (min(zxm1y1,zx1ym1) > zxm1ym1) THEN
              Slope=0.707*(zx1y1-zxm1ym1)
              IF (Slope > slopemax) THEN
                 slopemax=Slope
                imax=6
              END IF
!            END IF
!       
!            IF (min(zxp1y1,zx1ym1) > zxp1ym1) THEN
              Slope=0.707*(zx1y1-zxp1ym1)
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
!            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END DO
        END DO
      END IF
      RETURN
END SUBROUTINE DirAnal4
!
! 
! ===================================================================
!                           DIRANAL5
! ===================================================================
!
!
!     ModeDir=5:   R8 implementation
! 
! ====================================================================
! 
SUBROUTINE DirAnal5(IrregularBoundary,Domain,y,Z,iself               &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY   &
&           ,SimParameters,gridX,gridY)
  USE SiberiaConstants
  USE Support
  USE SiberiaTypes
  USE openMPsupport
  IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: y,Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: ix,iy,imax,i
!    INTEGER :: ix,iy,imax,seed,i
    REAL(KIND(0.0D0)) :: slopewgt(8),sum,weight,ChannelThreshold
    LOGICAL :: diff
!    DATA seed / 1 /
!    SAVE seed
! 
! =======================
!  ModeDir = 5
! ========================
!  Irregular Boundaries
! ========================
! 
    ChannelThreshold=SimParameters%YHold
       iself=0
        IF (IrregularBoundary) THEN
        DO iy=LowY,HighY
          DO ix=LowX,HighX
          DO i=1,8
            slopewgt(i)=-1
          END DO
          IF (Domain(ix,iy)) THEN
            Diff=y(ix,iy) > ChannelThreshold
            IF (Domain(ix+1,iy)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy) < ChannelThreshold)) THEN
                slopewgt(1)=Z(ix,iy)-Z(ix+1,iy)
              END IF
            END IF
!       
            IF (Domain(ix,iy+1)) THEN
              IF (.not.(Diff.and.Y(ix,iy+1) < ChannelThreshold)) THEN
                slopewgt(3)=Z(ix,iy)-Z(ix,iy+1)
              END IF
            END IF
!       
            IF (Domain(ix-1,iy)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy) < ChannelThreshold)) THEN
                slopewgt(5)=Z(ix,iy)-Z(ix-1,iy)
              END IF
            END IF
!       
            IF (Domain(ix,iy-1)) THEN
              IF (.not.(Diff.and.Y(ix,iy-1) < ChannelThreshold)) THEN
                slopewgt(7)=Z(ix,iy)-Z(ix,iy-1)
              END IF
            END IF
!       
            IF (Domain(ix+1,iy+1)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy+1) < ChannelThreshold)) THEN
!                IF (((Z(ix+1,iy) > Z(ix+1,iy+1).and.Domain(ix+1,iy))          &
!&                .or.(.not.Domain(ix+1,iy))).and.                              &
!&                ((Z(ix,iy+1) > Z(ix+1,iy+1).and.Domain(ix,iy+1))             &
!&                .or.(.not.Domain(ix,iy+1)))) THEN
                  slopewgt(2)=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
!                END IF
              END IF
            END IF
!       
            IF (Domain(ix-1,iy+1)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy+1) < ChannelThreshold)) THEN
!                IF (((Z(ix-1,iy) > Z(ix-1,iy+1).and.Domain(ix-1,iy))          &
!&                .or.(.not.Domain(ix-1,iy))).and.                              &
!&              ((Z(ix,iy+1) > Z(ix-1,iy+1).and.Domain(ix,iy+1))               &
!&                .or.(.not.Domain(ix,iy+1)))) THEN
                  slopewgt(4)=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
!                END IF
              END IF
            END IF
!       
            IF (Domain(ix-1,iy-1)) THEN
              IF (.not.(Diff.and.Y(ix-1,iy-1) < ChannelThreshold)) THEN
!                IF (((Z(ix-1,iy) > Z(ix-1,iy-1).and.Domain(ix-1,iy))          &
!&                .or.(.not.Domain(ix-1,iy))).and.                              &
!&              ((Z(ix,iy-1) > Z(ix-1,iy-1).and.Domain(ix,iy-1))               &
!&                .or.(.not.Domain(ix,iy-1)))) THEN
                  slopewgt(6)=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
!                END IF
              END IF
            END IF
!       
            IF (Domain(ix+1,iy-1)) THEN
              IF (.not.(Diff.and.Y(ix+1,iy-1) < ChannelThreshold)) THEN
!                IF (((Z(ix+1,iy) > Z(ix+1,iy-1).and.Domain(ix+1,iy))          &
!&                .or.(.not.Domain(ix+1,iy))).and.                              &
!&              ((Z(ix,iy-1) > Z(ix+1,iy-1).and.Domain(ix,iy-1))               &
!&                .or.(.not.Domain(ix,iy-1)))) THEN
                slopewgt(8)=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
!                END IF
              END IF
            END IF
! 
            sum=0
            DO i=1,8
              IF(slopewgt(i) > 0) sum=sum+slopewgt(i)
            END DO
            weight=sum*Ran2(SeedDirAnal5)
            IF (sum > 0.0) THEN
              sum=0
              DO i=1,8
                IF (slopewgt(i) > 0) THEN
                  sum=sum+slopewgt(i)
                  imax=i
                  IF (sum > weight) GO TO 2200
                END IF
              END DO
 2200         Direct(ix,iy)=dir(imax)
              s0(ix,iy)=slopewgt(imax)
            ELSE
              Direct(ix,iy)=5
              s0(ix,iy)=0
            END IF
          END IF
         END DO
       END DO
      ELSE
!  =============================
!  ModeDir = 5, Regular Boundaries
! ==============================
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            DO i=1,8
              slopewgt(i)=-1
            END DO
            Diff=y(ix,iy) > ChannelThreshold
            IF (.not.(Diff.and.Y(ix+1,iy) < ChannelThreshold)) THEN
            slopewgt(1)=Z(ix,iy)-Z(ix+1,iy)
            END IF
!       
            IF (.not.(Diff.and.Y(ix,iy+1) < ChannelThreshold)) THEN
              slopewgt(3)=Z(ix,iy)-Z(ix,iy+1)
            END IF
!       
            IF (.not.(Diff.and.Y(ix-1,iy) < ChannelThreshold)) THEN
              slopewgt(5)=Z(ix,iy)-Z(ix-1,iy)
            END IF
!       
            IF (.not.(Diff.and.Y(ix,iy-1) < ChannelThreshold)) THEN
              slopewgt(7)=Z(ix,iy)-Z(ix,iy-1)
            END IF
!       
            IF (.not.(Diff.and.Y(ix+1,iy+1) < ChannelThreshold)) THEN
!              IF (min(Z(ix+1,iy),Z(ix,iy+1)) > Z(ix+1,iy+1)) THEN
                slopewgt(2)=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
!              END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix-1,iy+1) < ChannelThreshold)) THEN
!              IF (min(Z(ix-1,iy),Z(ix,iy+1)) > Z(ix-1,iy+1)) THEN
                slopewgt(4)=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
!              END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix-1,iy-1) < ChannelThreshold)) THEN
!              IF (min(Z(ix-1,iy),Z(ix,iy-1)) > Z(ix-1,iy-1)) THEN
                slopewgt(6)=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
!              END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix+1,iy-1) < ChannelThreshold)) THEN
!              IF (min(Z(ix+1,iy),Z(ix,iy-1)) > Z(ix+1,iy-1)) THEN
                slopewgt(8)=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
!              END IF
            END IF
!       
            sum=0
            DO i=1,8
              IF(slopewgt(i) > 0) sum=sum+slopewgt(i)
            END DO
            weight=sum*Ran2(SeedDirAnal5)
            IF (sum > 0.0) THEN
              sum=0
              DO i=1,8
                IF (slopewgt(i) > 0) THEN
                  sum=sum+slopewgt(i)
                  imax=i
                  IF (sum > weight) GO TO 2201
                END IF
              END DO
 2201         Direct(ix,iy)=dir(imax)
              s0(ix,iy)=slopewgt(imax)
            ELSE
              Direct(ix,iy)=5
              s0(ix,iy)=0
            END IF
          END DO
        END DO
      END IF
      DirChg=.true.
      RETURN
END SUBROUTINE DirAnal5

!
! 
! ===================================================================
!                           DIRANAL6
! ===================================================================
!
!
!   ModeDir=6: An extremely limuted version of a code TO get the average direction
!              from a region greater than just the adjacent node points (first used
!              in Hancock (1998) PhD thesis). As currently stands it implicitly assumes
!              DirReg=2.
! 
! 
! 
SUBROUTINE DirAnal6(IrregularBoundary,Domain,y,Z,iself                &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,SimParameters     &
&           ,gridX,gridY)
  USE SiberiaConstants
  USE Support
  USE SiberiaTypes
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: y,Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: ix,iy,imax,direct1,DirReg
    REAL(KIND(0.0D0)) :: Slope,slopemax,ChannelThreshold
    LOGICAL :: diff
!    LOGICAL :: First,diff
!    DATA First / .true. /
!    SAVE First
! 
    ChannelThreshold=SimParameters%YHold
      IF (FirstDirAnal6) THEN
        IF (SimParameters%DirReg > 2) THEN
          CALL Message_Output(Message_WarnContinue,'DirReg > 2 not supported. Setting DirReg=2')
          SimParameters%DirReg=2
        END IF
        FirstDirAnal6=.false.
      END IF
! 
! =======================
!  ModeDir = 6
! ========================
!  Irregular Boundaries
! ========================
      DirReg=SimParameters%DirReg
       iself=0
       IF ( IrregularBoundary) THEN
        DO iy=LowY,HighY
          DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            slopemax=-1
            Diff=y(ix,iy) > ChannelThreshold
            IF (ix+DirReg <= SimParameters%kx+1) THEN
              IF (Domain(ix+DirReg,iy)) THEN
                IF (.not.(Diff.and.Y(ix+DirReg,iy) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix+2,iy)+Z(ix+1,iy)               &
&                        +0.5*(Z(ix+2,iy+1)+Z(ix+2,iy-1)))*0.33)/1.67
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=1
                  END IF
                END IF
              END IF
            END IF
!       
            IF (iy+DirReg <= SimParameters%ky+1) THEN
              IF (Domain(ix,iy+DirReg)) THEN
                IF (.not.(Diff.and.Y(ix,iy+DirReg) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix,iy+2)+Z(ix,iy+1)               &
&                   +0.5*(Z(ix+1,iy+2)+Z(ix-1,iy+2)))*0.33)/1.67
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=3
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix-DirReg >= 1) THEN
              IF (Domain(ix-DirReg,iy)) THEN
                IF (.not.(Diff.and.Y(ix-DirReg,iy) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix-2,iy)+Z(ix-1,iy)               &
&                       +0.5*(Z(ix-2,iy-1)+Z(ix-2,iy+1)))*0.33)/1.67
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=5
                  END IF
                END IF
              END IF
            END IF
!       
            IF (iy-DirReg >= 1) THEN
              IF (Domain(ix,iy-DirReg)) THEN
                IF (.not.(Diff.and.Y(ix,iy-DirReg) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix,iy-2)+Z(ix,iy-1)               &
&                       +0.5*(Z(ix+1,iy-2)+Z(ix-1,iy-2)))*0.33)/1.67
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=7
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix+DirReg <= SimParameters%kx+1.and.iy+DirReg <= SimParameters%ky+1) THEN
              IF (Domain(ix+DirReg,iy+DirReg)) THEN
                IF (.not.(Diff.and.Y(ix+DirReg,iy+DirReg) < ChannelThreshold)) THEN
                  IF (((Z(ix+DirReg,iy) > Z(ix+DirReg,iy+DirReg)              &
&                    .and.Domain(ix+DirReg,iy))                                &
&                    .or.(.not.Domain(ix+DirReg,iy))).and.                     &
&                    ((Z(ix,iy+DirReg) > Z(ix+DirReg,iy+DirReg)               &
&                    .and.Domain(ix,iy+DirReg))                                &
&                    .or.(.not.Domain(ix,iy+DirReg)))) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix+2,iy+2)                         &
&                         +Z(ix+1,iy+1)+0.5*(Z(ix+2,iy+1)+Z(ix+1,iy+2)         &
&                          ))*0.33))/1.5
                    IF (Slope > slopemax) THEN
                      slopemax=Slope
                      imax=2
                    END IF
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix-DirReg >= 1.and.iy+DirReg <= SimParameters%ky+1) THEN
              IF (Domain(ix-DirReg,iy+DirReg)) THEN
                IF (.not.(Diff.and.Y(ix-DirReg,iy+DirReg) < ChannelThreshold)) THEN
                  IF (((Z(ix-1,iy) > Z(ix-DirReg,iy+DirReg)                   &
&                    .and.Domain(ix-DirReg,iy))                                &
&                    .or.(.not.Domain(ix-DirReg,iy))).and.                     &
&                    ((Z(ix,iy+DirReg) > Z(ix-DirReg,iy+DirReg)               &
&                    .and.Domain(ix,iy+DirReg))                                &
&                    .or.(.not.Domain(ix,iy+DirReg)))) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix-2,iy+2)                         &
&                     +Z(ix-1,iy+1)+0.5*(Z(ix-1,iy+2)+Z(ix-2,iy+1)             &
&                      ))*0.33))/1.5
                    IF (Slope > slopemax) THEN
                      slopemax=Slope
                      imax=4
                    END IF
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix-DirReg >= 1.and.iy-DirReg >= 1) THEN
              IF (Domain(ix-DirReg,iy-DirReg)) THEN
                IF (.not.(Diff.and.Y(ix-DirReg,iy-DirReg) < ChannelThreshold)) THEN
                  IF (((Z(ix-DirReg,iy) > Z(ix-DirReg,iy-DirReg)               &
&                    .and.Domain(ix-DirReg,iy))                                 &
&                    .or.(.not.Domain(ix-DirReg,iy))).and.                      &
&                    ((Z(ix,iy-DirReg) > Z(ix-DirReg,iy-DirReg)                &
&                    .and.Domain(ix,iy-DirReg))                                 &
&                    .or.(.not.Domain(ix,iy-DirReg)))) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix-2,iy-2)                          &
&                    +Z(ix-1,iy-1)+0.5*(Z(ix-1,iy-2)+Z(ix-2,iy-1)               &
&                      ))*0.33))/1.5
                    IF (Slope > slopemax) THEN
                       slopemax=Slope
                      imax=6
                    END IF
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix+DirReg <= SimParameters%kx+1.and.iy-DirReg >= 1) THEN
              IF (Domain(ix+DirReg,iy-DirReg)) THEN
                IF (.not.(Diff.and.Y(ix+DirReg,iy-DirReg) < ChannelThreshold)) THEN
                  IF (((Z(ix+DirReg,iy) > Z(ix+DirReg,iy-DirReg)               &
&                    .and.Domain(ix+DirReg,iy))                                 &
&                    .or.(.not.Domain(ix+DirReg,iy))).and.                      &
&                    ((Z(ix,iy-DirReg) > Z(ix+DirReg,iy-DirReg)                &
&                    .and.Domain(ix,iy-DirReg))                                 &
&                    .or.(.not.Domain(ix,iy-DirReg)))) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix+2,iy-2)                          &
&                      +Z(ix+1,iy-1)+0.5*(Z(ix+1,iy-2)+Z(ix+2,iy-1)             &
&                        ))*0.33))/1.5
                    IF (Slope > slopemax) THEN
                      slopemax=Slope
                      imax=8
                    END IF
                  END IF
                END IF
              END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF                       
        IF (slopemax < 0.0) THEN
          s0(ix,iy)=0
        ELSE
              s0(ix,iy)=slopemax
        END IF
          END IF
         END DO
       END DO
      ELSE
!  =============================
!   Regular Boundaries
!  =============================
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            slopemax=-1
            Diff=y(ix,iy) > ChannelThreshold
            IF (ix+DirReg <= SimParameters%kx+1) THEN
              IF (.not.(Diff.and.Y(ix+DirReg,iy) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix+2,iy)+Z(ix+1,iy)               &
&                        +0.5*(Z(ix+2,iy+1)+Z(ix+2,iy-1)))*0.33)/1.67
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=1
                END IF
              END IF
            END IF
!       
            IF (iy+DirReg <= SimParameters%ky+1) THEN
              IF (.not.(Diff.and.Y(ix,iy+DirReg) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix,iy+2)+Z(ix,iy+1)               &
&                   +0.5*(Z(ix+1,iy+2)+Z(ix-1,iy+2)))*0.33)/1.67
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=3
                END IF
              END IF
            END IF
!       
            IF (ix-DirReg >= 1) THEN
              IF (.not.(Diff.and.Y(ix-DirReg,iy) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix-2,iy)+Z(ix-1,iy)               &
&                       +0.5*(Z(ix-2,iy-1)+Z(ix-2,iy+1)))*0.33)/1.67
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=5
                END IF
              END IF
            END IF
!       
            IF (iy-DirReg >= 1) THEN
              IF (.not.(Diff.and.Y(ix,iy-DirReg) < ChannelThreshold)) THEN
                  Slope=(Z(ix,iy)-(Z(ix,iy-2)+Z(ix,iy-1)               &
&                       +0.5*(Z(ix+1,iy-2)+Z(ix-1,iy-2)))*0.33)/1.67
                IF (Slope > slopemax) THEN
                  slopemax=Slope
                  imax=7
                END IF
              END IF
            END IF
!       
            IF (ix+DirReg <= SimParameters%kx+1.and.iy+DirReg <= SimParameters%ky+1) THEN
              IF (.not.(Diff.and.Y(ix+DirReg,iy+DirReg) < ChannelThreshold)) THEN
                IF (min(Z(ix+DirReg,iy),Z(ix,iy+DirReg))               &
&                    > Z(ix+DirReg,iy+DirReg)) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix+2,iy+2)               &
&                         +Z(ix+1,iy+1)+0.5*(Z(ix+2,iy+1)+Z(ix+1,iy+2)               &
&                          ))*0.33))/1.5
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=2
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix-DirReg >= 1.and.iy+DirReg <= SimParameters%ky+1) THEN
              IF (.not.(Diff.and.Y(ix-DirReg,iy+DirReg) < ChannelThreshold)) THEN
                IF (min(Z(ix-DirReg,iy),Z(ix,iy+DirReg))               &
&                      > Z(ix-DirReg,iy+DirReg)) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix-2,iy+2)               &
&                     +Z(ix-1,iy+1)+0.5*(Z(ix-1,iy+2)+Z(ix-2,iy+1)               &
&                      ))*0.33))/1.5
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=4
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix-DirReg >= 1.and.iy-DirReg >= 1) THEN
              IF (.not.(Diff.and.Y(ix-DirReg,iy-DirReg) < ChannelThreshold)) THEN
                IF (min(Z(ix-DirReg,iy),Z(ix,iy-DirReg))               &
&                          > Z(ix-DirReg,iy-DirReg)) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix-2,iy-2)               &
&                    +Z(ix-1,iy-1)+0.5*(Z(ix-1,iy-2)+Z(ix-2,iy-1)               &
&                      ))*0.33))/1.5
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=6
                  END IF
                END IF
              END IF
            END IF
!       
            IF (ix+DirReg <= SimParameters%kx+1.and.iy-DirReg >= 1) THEN
              IF (.not.(Diff.and.Y(ix+DirReg,iy-DirReg) < ChannelThreshold)) THEN
                IF (min(Z(ix+DirReg,iy),Z(ix,iy-DirReg))               &
&                            > Z(ix+DirReg,iy-DirReg)) THEN
                  Slope=(0.707*(Z(ix,iy)-(Z(ix+2,iy-2)               &
&                      +Z(ix+1,iy-1)+0.5*(Z(ix+1,iy-2)+Z(ix+2,iy-1)               &
&                        ))*0.33))/1.5
                  IF (Slope > slopemax) THEN
                    slopemax=Slope
                    imax=8
                  END IF
                END IF
              END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF                       
        IF (slopemax < 0.0) THEN
          s0(ix,iy)=0
        ELSE
              s0(ix,iy)=slopemax
        END IF
          END DO
        END DO
      END IF
      RETURN
END SUBROUTINE DirAnal6

!
!   Tarboton Dinifinity Routine   ModeDir = 7
! 
! ===================================================================
!                           DIRANAL78
! ===================================================================
!
!
SUBROUTINE DirAnal78(IrregularBoundary,Domain,Z,iself               &
&           ,Direct,DirectDinf,s0,DirChg,DirWeights                   &
&           ,LowX,HighX,LowY,HighY,Tarboton,gridX,gridY)
  USE SiberiaConstants
  USE Setup
  IMPLICIT NONE
! 
  REAL(KIND(0.0D0)),PARAMETER :: sqrt2=1.414213562373                 &
&          ,PiEighth=0.392699082                                      &
&          ,invPiQuarter= 1.273239544735163
! 
  INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
  INTEGER,DIMENSION(gridX,gridY) :: Direct,DirectDinf
  REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Z,s0,DirWeights
  LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg,Tarboton
! 
  INTEGER :: ix,iy,k,iMax,direct1,iMaxMinus,iMaxPlus
  REAL(KIND(0.0D0)) :: Slope(0:9),SlopeMax,temp2,s2,r
! 
! ========================
!  Irregular Boundaries
! ========================
! 
    iself=0
    IF (IrregularBoundary) THEN
      DO iy=LowY,HighY
        DO ix=LowX,HighX
          DirWeights(ix,iy)=0
          DirectDinf(ix,iy)=0
          IF (Domain(ix,iy)) THEN
            SlopeMax=-1
            DO k=0,9
              Slope(k)=-1
            END DO
            IF (Domain(ix+1,iy)) THEN
              Slope(1)=Z(ix,iy)-Z(ix+1,iy)
              Slope(9)=Slope(1)
              IF (Slope(1) > SlopeMax) THEN
                SlopeMax=Slope(1)
                iMax=1
              END IF
            END IF
!       
            IF (Domain(ix,iy+1)) THEN
              Slope(3)=Z(ix,iy)-Z(ix,iy+1)
              IF (Slope(3) > SlopeMax) THEN
                SlopeMax=Slope(3)
                iMax=3
              END IF
            END IF
!       
            IF (Domain(ix-1,iy)) THEN
              Slope(5)=Z(ix,iy)-Z(ix-1,iy)
              IF (Slope(5) > SlopeMax) THEN
                SlopeMax=Slope(5)
                iMax=5
              END IF
            END IF
!       
            IF (Domain(ix,iy-1)) THEN
              Slope(7)=Z(ix,iy)-Z(ix,iy-1)
              IF (Slope(7) > SlopeMax) THEN
                SlopeMax=Slope(7)
                iMax=7
              END IF
            END IF
!       
            IF (Domain(ix+1,iy+1)) THEN
!              IF (((Z(ix+1,iy) > Z(ix+1,iy+1).and.Domain(ix+1,iy))        &
!&                    .or.(.not.Domain(ix+1,iy))).and.                      &
!&                   ((Z(ix,iy+1) > Z(ix+1,iy+1).and.Domain(ix,iy+1))      &
!&                    .or.(.not.Domain(ix,iy+1)))) THEN
                Slope(2)=0.5*sqrt2*(Z(ix,iy)-Z(ix+1,iy+1))
                IF (Slope(2) > SlopeMax) THEN
                  SlopeMax=Slope(2)
                  iMax=2
                END IF
!              END IF
            END IF
!       
            IF (Domain(ix-1,iy+1)) THEN
!              IF (((Z(ix-1,iy) > Z(ix-1,iy+1).and.Domain(ix-1,iy))        &
!&                   .or.(.not.Domain(ix-1,iy))).and.                       &
!&                   ((Z(ix,iy+1) > Z(ix-1,iy+1).and.Domain(ix,iy+1))      &
!&                   .or.(.not.Domain(ix,iy+1)))) THEN
                Slope(4)=0.5*sqrt2*(Z(ix,iy)-Z(ix-1,iy+1))
                IF (Slope(4) > SlopeMax) THEN
                  SlopeMax=Slope(4)
                  iMax=4
                END IF
!              END IF
            END IF
!       
            IF (Domain(ix-1,iy-1)) THEN
!              IF (((Z(ix-1,iy) > Z(ix-1,iy-1).and.Domain(ix-1,iy))        &
!&                    .or.(.not.Domain(ix-1,iy))).and.                      &
!&                   ((Z(ix,iy-1) > Z(ix-1,iy-1).and.Domain(ix,iy-1))      &
!&                    .or.(.not.Domain(ix,iy-1)))) THEN
                Slope(6)=0.5*sqrt2*(Z(ix,iy)-Z(ix-1,iy-1))
                IF (Slope(6) > SlopeMax) THEN
                  SlopeMax=Slope(6)
                  iMax=6
                END IF
!              END IF
            END IF
!       
            IF (Domain(ix+1,iy-1)) THEN
!              IF (((Z(ix+1,iy) > Z(ix+1,iy-1).and.Domain(ix+1,iy))        &
!&                    .or.(.not.Domain(ix+1,iy))).and.                      &
!&                   ((Z(ix,iy-1) > Z(ix+1,iy-1).and.Domain(ix,iy-1))      &
!&                    .or.(.not.Domain(ix,iy-1)))) THEN
                Slope(8)=0.5*sqrt2*(Z(ix,iy)-Z(ix+1,iy-1))
                Slope(0)=Slope(8)
                IF (Slope(8) > SlopeMax) THEN
                  SlopeMax=Slope(8)
                  iMax=8
                END IF
!              END IF
            END IF
!       
            IF (SlopeMax <= 0.0) THEN
              IF (SlopeMax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(iMax)
              iMaxMinus=iMax-1
              iMaxPlus=iMax+1
! 
!   according TO Tarboton 1997
! 
              IF (Tarboton) THEN
                IF (slope(iMaxPlus) > slope(iMaxMinus)) THEN
                  IF (Slope(iMaxPlus) > 0.0) THEN
                    DirectDinf(ix,iy)=dir(iMaxPlus)
!                    s2=dirlgth(iMaxPlus)*Slope(iMaxPlus)-dirlgth(iMax)*Slope(iMax)
                    s2=sqrt2*Slope(iMaxPlus)-Slope(iMax)
                    SlopeMax=(Slope(iMax)**2+s2**2)**0.5
                    r=atan(s2/Slope(iMax))
                    IF (r > pieighth) THEN
                      DirWeights(ix,iy)=0.5
                    ELSE IF (r <= 0.0) THEN
                      DirWeights(ix,iy)=0.0
                    ELSE
                      DirWeights(ix,iy)=r*invPiQuarter
                    END IF
                  ELSE
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0.0
                  END IF
                ELSE
                  IF (Slope(iMaxMinus) > 0.0) THEN
                    DirectDinf(ix,iy)=dir(iMaxMinus)
!                    s2=dirlgth(iMaxMinus)*Slope(iMaxMinus)-dirlgth(iMax)*Slope(iMax)
                    s2=sqrt2*Slope(iMaxMinus)-Slope(iMax)
                    SlopeMax=(Slope(iMax)**2+s2**2)**0.5
                    r=atan(s2/Slope(iMax))
                    IF (r > pieighth) THEN
                      DirWeights(ix,iy)=-0.5
                    ELSE IF (r < 0.0) THEN
                      DirWeights(ix,iy)=0.0
                    ELSE
                      DirWeights(ix,iy)=-r*invPiQuarter
                    END IF
                  ELSE
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0.0
                  END IF
                END IF
              ELSE
! 
!   my simpler and faster (?) implementation of the concept
! 
                IF (Slope(iMaxMinus) > Slope(iMaxPlus)) THEN
                  IF (Slope(iMaxMinus) > 0.0) THEN
                    temp2=slope(iMaxMinus)/slope(iMax)-0.5*sqrt2
                    IF (temp2 > 0.0) THEN
                      DirWeights(ix,iy)=-temp2/(2.0-sqrt2)
                      DirectDinf(ix,iy)=dir(iMaxMinus)
                    END IF
                  ELSE
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0.0
                  END IF
                ELSE
                  IF (Slope(iMaxPlus) >= 0.0) THEN
                    temp2=slope(iMaxPlus)/slope(iMax)-0.5*sqrt2
                    IF (temp2 > 0.0) THEN
                      DirWeights(ix,iy)=temp2/(2.0-sqrt2)
                      DirectDinf(ix,iy)=dir(iMaxPlus)
                    END IF
                  ELSE
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0.0
                  END IF
                END IF
              END IF     ! Tarboton
! 
!   END of the alternatives
! 
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END IF
        END DO
      END DO
    ELSE
! 
!  =============================
!   Regular Boundaries
!  =============================
! 
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            DirWeights(ix,iy)=0
            DirectDinf(ix,iy)=0
            SlopeMax=-1
            DO k=0,9
              Slope(k)=-1
            END DO
            IF (ix < HighX) THEN
              Slope(1)=Z(ix,iy)-Z(ix+1,iy)
              Slope(9)=Slope(1)
              IF (Slope(1) > SlopeMax) THEN
                SlopeMax=Slope(1)
                iMax=1
              END IF
            END IF
!       
            IF (iy < HighY) THEN
              Slope(3)=Z(ix,iy)-Z(ix,iy+1)
              IF (Slope(3) > SlopeMax) THEN
                SlopeMax=Slope(3)
                iMax=3
              END IF
            END IF
!       
            IF (ix > LowX) THEN
              Slope(5)=Z(ix,iy)-Z(ix-1,iy)
              IF (Slope(5) > SlopeMax) THEN
                SlopeMax=Slope(5)
                iMax=5
              END IF
            END IF
!       
            IF (iy > LowY) THEN
              Slope(7)=Z(ix,iy)-Z(ix,iy-1)
              IF (Slope(7) > SlopeMax) THEN
                SlopeMax=Slope(7)
                iMax=7
              END IF
            END IF
!       
            IF (ix < HighX.and.iy < HighY) THEN
!              IF (min(Z(ix+1,iy),Z(ix,iy+1)) > Z(ix+1,iy+1)) THEN
                Slope(2)=0.5*sqrt2*(Z(ix,iy)-Z(ix+1,iy+1))
                IF (Slope(2) > SlopeMax) THEN
                  SlopeMax=Slope(2)
                  iMax=2
                END IF
!              END IF
            END IF
!       
            IF (ix > LowX.and.iy < HighY) THEN
!              IF (min(Z(ix-1,iy),Z(ix,iy+1)) > Z(ix-1,iy+1)) THEN
                Slope(4)=0.5*sqrt2*(Z(ix,iy)-Z(ix-1,iy+1))
                IF (Slope(4) > SlopeMax) THEN
                  SlopeMax=Slope(4)
                  iMax=4
                END IF
!              END IF
            END IF
!       
            IF (ix > LowX.and.iy > LowY) THEN
!              IF (min(Z(ix-1,iy),Z(ix,iy-1)) > Z(ix-1,iy-1)) THEN
                Slope(6)=0.5*sqrt2*(Z(ix,iy)-Z(ix-1,iy-1))
                IF (Slope(6) > SlopeMax) THEN 
                  SlopeMax=Slope(6)
                  iMax=6
                END IF
!              END IF
            END IF
!       
            IF (ix < HighX.and.iy > LowY) THEN
!              IF (min(Z(ix+1,iy),Z(ix,iy-1)) > Z(ix+1,iy-1)) THEN
                Slope(8)=0.5*sqrt2*(Z(ix,iy)-Z(ix+1,iy-1))
                Slope(0)=Slope(8)
                IF (Slope(8) > SlopeMax) THEN
                  SlopeMax=Slope(8)
                  iMax=8
                END IF
!              END IF
            END IF
!       
            IF (SlopeMax <= 0.0) THEN
              IF (SlopeMax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE    ! OK Valid nonzero positive slope
              direct1=dir(iMax)
              iMaxMinus=iMax-1
              iMaxPlus=iMax+1
! 
!   according TO Tarboton 1997
! 
              IF (Tarboton) THEN
                IF (slope(iMaxPlus) > slope(iMaxMinus)) THEN
                  IF (Slope(iMaxPlus) > 0.0) THEN
                    DirectDinf(ix,iy)=dir(iMaxPlus)
!                  s2=dirlgth(iMaxPlus)*Slope(iMaxPlus)-dirlgth(iMax)*Slope(iMax)
                    s2=sqrt2*Slope(iMaxPlus)-Slope(iMax)
                    SlopeMax=(Slope(iMax)**2+s2**2)**0.5
                    r=atan(s2/Slope(iMax))
                    IF (r > pieighth) THEN
                      DirWeights(ix,iy)=0.5
                    ELSE IF (r < 0.0) THEN
                      DirWeights(ix,iy)=0.0
                    ELSE
                      DirWeights(ix,iy)=r*invPiQuarter
                    END IF
                  ELSE
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0.0
                  END IF
                ELSE
                  IF (Slope(iMaxMinus) > 0.0) THEN
                    DirectDinf(ix,iy)=dir(iMaxMinus)
!                    s2=dirlgth(iMaxMinus)*Slope(iMaxMinus)-dirlgth(iMax)*Slope(iMax)
                    s2=sqrt2*Slope(iMaxMinus)-Slope(iMax)
                    SlopeMax=(Slope(iMax)**2+s2**2)**0.5
                    r=atan(s2/Slope(iMax))
                    IF (r > pieighth) THEN
                      DirWeights(ix,iy)=-0.5
                    ELSE IF (r < 0.0) THEN
                      DirWeights(ix,iy)=0.0
                    ELSE
                      DirWeights(ix,iy)=-r*invPiQuarter
                    END IF
                  ELSE
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0.0
                  END IF
                END IF
              ELSE
! 
!   my simpler and faster implementation of the Dinfinity concept
! 
                IF (Slope(iMaxMinus) > Slope(iMaxPlus)) THEN
                  IF (Slope(iMaxMinus) > 0.0) THEN
                    temp2=slope(iMaxMinus)/slope(iMax)-0.5*sqrt2
                    IF (temp2 > 0.0) THEN
                      DirWeights(ix,iy)=-temp2/(2.0-sqrt2)
                      DirectDinf(ix,iy)=dir(iMaxMinus)
                    ELSE 
                      DirectDinf(ix,iy)=5
                      DirWeights(ix,iy)=0
                    END IF
                  ELSE 
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0
                  END IF
                ELSE 
                  IF (Slope(iMaxPlus) > 0.0) THEN
                    temp2=slope(iMaxPlus)/slope(iMax)-0.5*sqrt2
                    IF (temp2 > 0.0) THEN
                      DirWeights(ix,iy)=temp2/(2.0-sqrt2)
                      DirectDinf(ix,iy)=dir(iMaxPlus)
                    ELSE 
                      DirectDinf(ix,iy)=5
                      DirWeights(ix,iy)=0
                    END IF
                  ELSE 
                    DirectDinf(ix,iy)=5
                    DirWeights(ix,iy)=0
                  END IF
                END IF
              END IF     ! Tarboton
! 
!   END of the alternatives
! 
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
            END IF                       
            s0(ix,iy)=SlopeMax
          END DO
        END DO
      END IF
      DirChg=.true.
      RETURN
END SUBROUTINE DirAnal78

!
! 
! ===================================================================
!                           DIRANAL9
! ===================================================================
!
!
SUBROUTINE DirAnal9(IrregularBoundary,Domain,y,Z,iself                 &
&           ,Direct,s0,DirChg,LowX,HighX,LowY,HighY,gridX,gridY)
  USE SiberiaConstants
    IMPLICIT NONE
! 
    INTEGER :: iself,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: y,Z,s0
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DirChg
! 
    INTEGER :: ix,iy,imax,direct1
    REAL(KIND(0.0D0)) :: Slope,slopemax
    LOGICAL :: diff
! 
! =======================
!  ModeDir = 0
! ========================
!  Irregular Boundaries
! ========================
       iself=0
        IF ( IrregularBoundary) THEN
        DO iy=LowY,HighY
          DO ix=LowX,HighX
          IF (Domain(ix,iy)) THEN
            slopemax=-1
            Diff=y(ix,iy) > 0.85
            IF (Domain(ix+1,iy)) THEN
            IF (.not.(Diff.and.Y(ix+1,iy) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix+1,iy)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=1
            END IF
            END IF
            END IF
!       
            IF (Domain(ix,iy+1)) THEN
            IF (.not.(Diff.and.Y(ix,iy+1) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix,iy+1)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=3
            END IF
            END IF
            END IF
!       
            IF (Domain(ix-1,iy)) THEN
            IF (.not.(Diff.and.Y(ix-1,iy) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix-1,iy)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=5
            END IF
            END IF
            END IF
!       
            IF (Domain(ix,iy-1)) THEN
            IF (.not.(Diff.and.Y(ix,iy-1) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix,iy-1)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=7
            END IF
            END IF
            END IF
!       
            IF (Domain(ix+1,iy+1)) THEN
            IF (.not.(Diff.and.Y(ix+1,iy+1) < 0.85)) THEN
            IF (((Z(ix+1,iy) > Z(ix+1,iy+1).and.Domain(ix+1,iy))     &
&                .or.(.not.Domain(ix+1,iy))).and.                     &
&              ((Z(ix,iy+1) > Z(ix+1,iy+1).and.Domain(ix,iy+1))      &
&                .or.(.not.Domain(ix,iy+1)))) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
            END IF
            END IF
            END IF
!       
            IF (Domain(ix-1,iy+1)) THEN
            IF (.not.(Diff.and.Y(ix-1,iy+1) < 0.85)) THEN
            IF (((Z(ix-1,iy) > Z(ix-1,iy+1).and.Domain(ix-1,iy))      &
&                .or.(.not.Domain(ix-1,iy))).and.                      &
&              ((Z(ix,iy+1) > Z(ix-1,iy+1).and.Domain(ix,iy+1))       &
&                .or.(.not.Domain(ix,iy+1)))) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
            END IF
            END IF
            END IF
!       
            IF (Domain(ix-1,iy-1)) THEN
            IF (.not.(Diff.and.Y(ix-1,iy-1) < 0.85)) THEN
            IF (((Z(ix-1,iy) > Z(ix-1,iy-1).and.Domain(ix-1,iy))     &
&                .or.(.not.Domain(ix-1,iy))).and.                     &
&              ((Z(ix,iy-1) > Z(ix-1,iy-1).and.Domain(ix,iy-1))      &
&                .or.(.not.Domain(ix,iy-1)))) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
              IF (Slope > slopemax) THEN
                 slopemax=Slope
                imax=6
              END IF
            END IF
            END IF
            END IF
!       
            IF (Domain(ix+1,iy-1)) THEN
            IF (.not.(Diff.and.Y(ix+1,iy-1) < 0.85)) THEN
            IF (((Z(ix+1,iy) > Z(ix+1,iy-1).and.Domain(ix+1,iy))      &
&                .or.(.not.Domain(ix+1,iy))).and.                      &
&              ((Z(ix,iy-1) > Z(ix+1,iy-1).and.Domain(ix,iy-1))       &
&                .or.(.not.Domain(ix,iy-1)))) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
            END IF
            END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF
        IF (slopemax < 0.0) THEN
          s0(ix,iy)=0
        ELSE
              s0(ix,iy)=slopemax
        END IF
          END IF
          END DO
        END DO
      ELSE
!  =============================
!  ModeDir = 0, Regular Boundaries
! ==============================
        DO iy=LowY,HighY
          DO ix=LowX,HighX
            slopemax=-1
            Diff=y(ix,iy) > 0.85
            IF (.not.(Diff.and.Y(ix+1,iy) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix+1,iy)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=1
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix,iy+1) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix,iy+1)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=3
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix-1,iy) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix-1,iy)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=5
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix,iy-1) < 0.85)) THEN
            Slope=Z(ix,iy)-Z(ix,iy-1)
            IF (Slope > slopemax) THEN
              slopemax=Slope
              imax=7
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix+1,iy+1) < 0.85)) THEN
            IF (MIN(Z(ix+1,iy),Z(ix,iy+1)) > Z(ix+1,iy+1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix+1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=2
              END IF
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix-1,iy+1) < 0.85)) THEN
            IF (MIN(Z(ix-1,iy),Z(ix,iy+1)) > Z(ix-1,iy+1)) THEN
              Slope=0.707*(Z(ix,iy)-Z(ix-1,iy+1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=4
              END IF
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix-1,iy-1) < 0.85)) THEN
            IF (MIN(Z(ix-1,iy),Z(ix,iy-1)) > Z(ix-1,iy-1)) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix-1,iy-1))
              IF (Slope > slopemax) THEN
                 slopemax=Slope
                imax=6
              END IF
            END IF
            END IF
!       
            IF (.not.(Diff.and.Y(ix+1,iy-1) < 0.85)) THEN
            IF (MIN(Z(ix+1,iy),Z(ix,iy-1)) > Z(ix+1,iy-1)) THEN
                Slope=0.707*(Z(ix,iy)-Z(ix+1,iy-1))
              IF (Slope > slopemax) THEN
                slopemax=Slope
                imax=8
              END IF
            END IF
            END IF
!       
            IF (slopemax <= 0.0) THEN
              IF (slopemax < 0.0) THEN
                iself=iself+1
              END IF
              direct1=5
            ELSE
              direct1=dir(imax)
            END IF
            IF (Direct(ix,iy) /= direct1) THEN
              Direct(ix,iy)=direct1
              DirChg=.true.
            END IF                       
            IF (slopemax < 0.0) THEN
              s0(ix,iy)=0
            ELSE
              s0(ix,iy)=slopemax
            END IF
          END DO
        END DO
      END IF
      RETURN
END SUBROUTINE diranal9

END MODULE DirAnalysis

