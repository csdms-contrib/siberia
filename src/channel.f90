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
module ChannelAnalysis

  REAL,PARAMETER :: ChannelAnalysis_Version=8.17

contains 

! 
! ====================================================================
!  Model TO simulate the depth of channels and gullies
! ====================================================================
! 
SUBROUTINE ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                     &
&                  ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF  &
&                  ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
  USE SiberiaTypes
  USE Support
  USE openMPsupport
  IMPLICIT NONE
! 
!   Model for the depths of channels. Only depths are calculated so that
!   it is not possible TO USE hese models TO DO mass balance calculations.
! 
    INTEGER :: LowX,HighX,LowY,HighY,gridX,gridY
    REAL(KIND(0.0D0)) :: SimTime
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: cDepth,s0,Z,Y,Area,Hill_Channel_Factor
    LOGICAL :: Domain(gridX,gridY),IrregularBoundary,DirChg,DetCIF
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j
    REAL(KIND(0.0D0)) :: b6b3,m3m6,tempAA,TempR,b3Star
! 
    IF (FirstChannel) THEN
      FirstChannel=.false.
      IF (SimParameters%ModeChannel == 0) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            cDepth(i,j)=0.0
          END DO
        END DO
      END IF
    END IF
! 
!   Channel Indication FUNCTION
!  ==============================
! 
!   Hill_Channel_Factor =Yhold (def=0.1)   => fully hillslope erosion
!                       =1                 => fully channel erosion
!        linear interpolation between the two END points
! 
!   the next line is only temporary until Hill_Channel_Factor is fully implemented
    if (SimParameters%ModeChannel == 0) return
    IF (DetCIF) THEN
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (Y(i,j) >= 1.0) THEN
                Hill_Channel_Factor(i,j)=1.0
              ELSE IF (Y(i,j) <= SimParameters%YHold) THEN
                Hill_Channel_Factor(i,j)=0.0
              ELSE
                Hill_Channel_Factor(i,j)=(y(i,j)-SimParameters%YHold)/(1.0-SimParameters%YHold)
              END IF
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Y(i,j) >= 1.0) THEN
              Hill_Channel_Factor(i,j)=1.0
            ELSE IF (Y(i,j) <= SimParameters%YHold) THEN
              Hill_Channel_Factor(i,j)=0.0
            ELSE
              Hill_Channel_Factor(i,j)=(y(i,j)-SimParameters%YHold)/(1.0-SimParameters%YHold)
            END IF
          END DO
        END DO
      END IF
    ELSE
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              tempAA=400*(SimParameters%b5*SimParameters%c1)**(-1/SimParameters%m5)    &
&                     *Area(i,j)**(-SimParameters%m3)
              IF (s0(i,j) /= 0.0) THEN
                tempR=tempAA*s0(i,j)**(-1/SimParameters%m5)
                IF (tempR < 0.0) THEN
                  tempR=0.0
                END IF
                b3Star=(tempR-SimParameters%b3)/SimParameters%b3SDl
                Hill_Channel_Factor(i,j)=PDFNorm(b3Star)
              ELSE
                Hill_Channel_Factor(i,j)=0              
              END IF
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            tempAA=400*(SimParameters%b5*SimParameters%c1)**(-1/SimParameters%m5)      &
&                   *Area(i,j)**(-SimParameters%m3)
            IF (s0(i,j) /= 0.0) THEN
              tempR=tempAA*s0(i,j)**(-1/SimParameters%m5)
              IF (tempR < 0.0) THEN
                tempR=0.0
              END IF
              b3Star=(tempR-SimParameters%b3)/SimParameters%b3SDl
              Hill_Channel_Factor(i,j)=PDFNorm(b3Star)
            ELSE
              Hill_Channel_Factor(i,j)=0              
            END IF
          END DO
        END DO
      END IF
    END IF
! 
!   Channel Dimensions Modules
!  ============================
! 
    IF (SimParameters%ModeChannel == 0) RETURN
    SELECT CASE (SimParameters%ModeChannel)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorStop,' -- This Channel model mode not implemented')
! 
!  --------------------------------
!  Channel model based on Area
!  --------------------------------
! 
      CASE (1)
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                cDepth(i,j)=SimParameters%b6*Area(i,j)**SimParameters%m6
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              cDepth(i,j)=SimParameters%b6*Area(i,j)**SimParameters%m6
            END DO
          END DO
        END IF
! 
!  --------------------------------
!  Channel model based on discharge
!  --------------------------------
! 
      CASE (2)
        b6b3=SimParameters%b6*SimParameters%b3**SimParameters%m6
        m3m6=SimParameters%m3*SimParameters%m6
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                cDepth(i,j)=b6b3*Area(i,j)**m3m6
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              cDepth(i,j)=b6b3*Area(i,j)**m3m6
            END DO
          END DO
        END IF
    END SELECT
    RETURN
END SUBROUTINE ChannelAnal

END module ChannelAnalysis
