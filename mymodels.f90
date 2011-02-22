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
MODULE MyModels

  REAL,PARAMETER :: MyModels_Version=8.24

CONTAINS 

! 
!  ===================================================================
!  ===================================================================
!                            FACTOR
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE MyErosion                                                       &
&            (Factor,Erode_m1,Erode_n1,DetCIF,IrregularBoundary           &
&            ,Domain,DirChg,vz,s0,Y,Z,Zoriginal,Area,Direct,GridX,GridY   &
&            ,LowX,HighX,LowY,HighY,tottime,SimParameters)
  USE LayerConstants
  USE LayerSupport
  USE openMPsupport
  USE SiberiaTypes
  USE Support
  IMPLICIT NONE
! 
!  This routine is the standard MODULE for inclusion of user-defined
!  temporal and spatial variation in erosion rate. ModeErode in the
!  MODULE 'Parameters' controls what TYPE of user defined
!  model will be used. NOTE: ModeErode=0 should not be changed because
!  this is the standard spatially uniform, temporally constant CASE.
!  For new models add options TO the computed GO TO at the start of
!  the routine. Ensure that user options for ModeErode are NEGATIVE
!  as POSITIVE values of ModeErode will be used by the developer during
!  future code development.
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY)                               &
&         ,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: tottime
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Z,Zoriginal,y,s0,vz                    &
&         ,factor,Area,Erode_m1,Erode_n1
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary, DetCIF,DirChg
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ModeErode,ModeChannel,ModeRandom,ErrorNo
    REAL(KIND(0.0D0)) :: tempR,b3star,coeff,exponent,depth,b1,m1,n1,YHold,OTime    &
&           ,FactMx,b5,m5,n5,c1,b3,m3,b3sdl,cover,tempb1
    LOGICAL :: absolute
! 
      ModeErode=SimParameters%ModeErode
      ModeChannel=SimParameters%ModeChannel
      ModeRandom=SimParameters%ModeRandom
      b1=SimParameters%b1
      m1=SimParameters%m1
      n1=SimParameters%n1
      YHold=SimParameters%YHold
      OTime=SimParameters%OTime
      FactMx=SimParameters%FactMx
      b5=SimParameters%b5
      m5=SimParameters%m5
      n5=SimParameters%n5
      c1=SimParameters%c1
      b3=SimParameters%b3
      m3=SimParameters%m3
      b3sdl=SimParameters%b3sdl
      cover=SimParameters%cover
! 
    IF (ModeErode < 0)THEN
      CALL Message_Output(Message_ErrorStop,'Internal error 1 MYUSERFACTOR = ',ModeErode)
    END IF 
    IF (FirstFactor) then
      DirChg=.true.
    end if
      DirChg=.true.
    SELECT CASE (mod(ModeErode,20))
    CASE DEFAULT
      CALL Message_Output(Message_ErrorStop,'ModeErode = ',ModeErode,' is not defined')
! 
!  DEFAULT EROSION MODEL
! -------------------------
!
    CASE (0:3)
      IF (FirstFactor) THEN
! initialise the spatial values for the erosion model
        DO j=LowY,HighY
          DO i=LowX,HighX
            factor(i,j)=b1
            erode_m1(i,j)=m1
            erode_n1(i,j)=n1
          END DO
        END DO
        allocate(tempA(1:GridX,1:GridY),stat=ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('tempA','SIBERIA_MYEROSION')
      ELSE
        IF (ModeChannel == 5) RETURN
      END IF
      IF (IrregularBoundary) THEN
        IF (DetCIF) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                SELECT CASE (ModeChannel)
                  CASE DEFAULT
                    IF (Y(i,j) < YHold) THEN
                      Factor(i,j)=OTime
                    ELSE
                      factor(i,j)=((min(YHold+0.1,y(i,j))-YHold)*10.0           &
&                       *(1.0-OTime)+OTime)
                    END IF
                  CASE (5)
                    Factor(i,j)=1.0
                END SELECT
                IF (ModeRandom == 1) THEN
                  Factor(i,j)=min(factor(i,j),FactMx)*vz(i,j)*b1
                ELSE
                  Factor(i,j)=min(factor(i,j),FactMx)*b1
                END IF
              END IF
            END DO
          END DO
        ELSE
          IF (DirChg) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  TempA(i,j)=400*(b5*c1)**(-1/m5)*Area(i,j)**(-m3)
                END IF
              END DO
            END DO
          END IF
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j).and.s0(i,j) /= 0.0) THEN
                tempR=tempA(i,j)*s0(i,j)**(-1/m5)
!               tempR=max(0.0d0,tempR)
                IF (tempR < 0.0) THEN
                  tempR=0.0
                END IF
                b3Star=(tempR-b3)/b3SDl
                IF (ModeRandom == 1) THEN
                  factor(i,j)=((1-OTime)*PDFNorm(b3Star)+OTime)*vz(i,j)*b1
                ELSE
                  factor(i,j)=((1-OTime)*PDFNorm(b3Star)+OTime)*b1
                END IF
              ELSE
                factor(i,j)=FactMx*b1
              END IF
            END DO
          END DO
        END IF
      ELSE
        IF (DetCIF) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              SELECT CASE (ModeChannel)
                CASE DEFAULT
                  IF (Y(i,j) < YHold) THEN
                    Factor(i,j)=OTime
                  ELSE
                    factor(i,j)=((min(YHold+0.1,y(i,j))-YHold)*10.0               &
&                       *(1.0-OTime)+OTime)
                  END IF
                CASE (5)
                factor(i,j)=1.0
              END SELECT
              IF (ModeRandom == 1) THEN
                Factor(i,j)=min(factor(i,j),FactMx)*vz(i,j)*b1
              ELSE
                Factor(i,j)=min(factor(i,j),FactMx)*b1
              END IF
            END DO
          END DO
        ELSE
          IF (DirChg) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                TempA(i,j)=(b5*c1)**(-1/m5)*Area(i,j)**(-m3)
              END DO
            END DO
          END IF
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (s0(i,j) /= 0.0) THEN
                b3Star=(tempA(i,j)*s0(i,j)**(-1/m5)-b3)/b3SDl
                IF (ModeRandom == 1) THEN
                  factor(i,j)=((1-OTime)*PDFNorm(b3Star)+OTime)               &
&                   *vz(i,j)*b1
                ELSE
                  factor(i,j)=((1-OTime)*PDFNorm(b3Star)+OTime)*b1
                END IF
              ELSE
                factor(i,j)=FactMx*b1
              END IF
            END DO
          END DO
        END IF
      END IF
! 
!  Correction for Cover
! 
      IF(Cover /= 1.0) THEN
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF(Domain(i,j)) THEN
                factor(i,j)=factor(i,j)*Cover
              END IF 
            END DO
          END DO 
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              factor(i,j)=factor(i,j)*Cover
            END DO
          END DO
        END IF
      END IF
! 
!  IF there is an erosion depth dependent erodibility
! 
      SELECT CASE (mod(ModeErode,20))
        CASE(1)
          IF (FirstFactor) THEN
            OPEN(UNIT=20,FILE=SimParameters%FileFactor,STATUS='old')
            DO i=1,4
              READ(20,*)
            END DO
            READ(20,*) coeff,exponent
            CLOSE(UNIT=20,STATUS='keep')
          END IF
          DO j=LowY,HighY
            DO i=LowX,HighX
              depth=Zoriginal(i,j)-Z(i,j)
              IF (depth > 0.0) THEN
                factor(i,j)=factor(i,j)/(coeff*depth**exponent+1.0)
              END IF
            END DO
          END DO
! 
! IF there is input of Regions with different erodibility
! 
        CASE(2)
          IF (FirstFactor) THEN
            allocate(Region_b1(1:GridX,1:GridY),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Region_b1','SIBERIA_MYEROSION')
!  Initialise the Region array TO the DEFAULT value
            DO j=LowY,HighY
              DO i=LowX,HighX
                Region_b1(i,j)=factor(i,j)
                erode_m1(i,j)=m1
                erode_n1(i,j)=n1
              END DO
            END DO
!  Input the Region values
            CALL InputRegion_b1(Region_b1               &
&                 ,GridX,GridY,absolute,LowX,HighX,LowY,HighY,SimParameters)
          END IF
          DO j=LowY,HighY
            DO i=LowX,HighX
              factor(i,j)=Region_b1(i,j)
            END DO
          END DO
! 
! IF there is input of Regions with different erosion models
! 
        CASE(3)
          IF (FirstFactor) THEN
            allocate(Region_b1(1:GridX,1:GridY),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Region_b1','SIBERIA_MYEROSION')
            allocate(Region_m1(1:GridX,1:GridY),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Region_m1','SIBERIA_MYEROSION')
            allocate(Region_n1(1:GridX,1:GridY),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Region_n1','SIBERIA_MYEROSION')
!  Initialise the Region array TO the DEFAULT value
            DO j=LowY,HighY
              DO i=LowX,HighX
                Region_b1(i,j)=factor(i,j)
                Region_m1(i,j)=m1
                Region_n1(i,j)=n1
              END DO
            END DO
!  Input the Region values
            CALL InputRegion_b1m1n1(Region_b1     &
&                 ,Region_m1,Region_n1,GridX,GridY                 &
&                 ,absolute,LowX,HighX,LowY,HighY,SimParameters)
          END IF
          DO j=LowY,HighY
            DO i=LowX,HighX
              factor(i,j)=Region_b1(i,j)
              erode_m1(i,j)=Region_m1(i,j)
              erode_n1(i,j)=Region_n1(i,j)
            END DO
          END DO
      END SELECT
!  erodibility from layers
! =========================
      CASE(4)
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (FirstFactor) THEN
                call Layer_Get(Layer_b1,i,j,1,tempb1,ErrorNo)
              else
                call Layer_Get(Layer_flowb1,i,j,tempb1,ErrorNo)
              end if
              factor(i,j)=tempb1
!             write (60,*) 'b1',i,j,tempb1
              erode_m1(i,j)=m1
              erode_n1(i,j)=n1
            END DO
          END DO
    END SELECT
! 
 9999 FirstFactor=.false.
    RETURN
END SUBROUTINE MyErosion
! 
!  ===================================================================
!  ===================================================================
!              Input spatially distributed Erodibility Model
!                  (NB this mode only inputs erdobility ...
!                   more general capabilities available
!                   with ModeErode=3)
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE InputRegion_b1(Region_b1,GridX,GridY,absolute               &
&             ,LowX,HighX,LowY,HighY,SimParameters)
  USE SiberiaTypes
  USE Multipliers
  USE ModelFile
  USE Support
  USE Setup
  USE InputOutput
  IMPLICIT NONE
! 
    INTEGER,parameter :: unitno=20,LineLgth=1000
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Region_b1
    LOGICAL :: absolute
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ibuff,RegionNo,Region(GridX,GridY)
    REAL(KIND(0.0D0)) :: tmpb1
    CHARACTER(LineLgth) :: line,buffer,regionfile
    LOGICAL :: errorL
    DATA RegionNo / 0 /
! 
    CHARACTER(8) :: tempstr
!
    CHARACTER(80) :: atom
    INTEGER :: SELECT
! 
      OPEN(UNIT=unitno,FILE=SimParameters%FileFactor,STATUS='old',ERR=9999)
      rewind(UNIT=unitno)
! 
!  READ file header
! 
      CALL Message_Output(Message_Info,'-- Region input for Erodibility Model')
      READ(unitno,6000) buffer
      CALL Str_UpperCase(buffer)
      ibuff=1
      CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
      IF (atom(1:) /= 'SIBERIA') THEN
        CALL Message_Output(Message_ErrorStop,'Invalid header in Erodibility Model File'//SimParameters%FileFactor)
      END IF
      DO i=2,4
        READ(unitno,*)
      END DO
 8002 line=' '
      READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
      line=adjustl(line)
 8003 IF (line(1:1) == '#' .or. line(1:1) == '!' .or. line(1:1) == ' ') GO TO 8002
! 
      ibuff=1
      buffer=line
      CALL Str_UpperCase(buffer)
      CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
      CALL Str_mtchcomm1(atom,commands,SELECT)
      SELECT CASE (SELECT)
        CASE(Model_Erodibility)
          RegionNo=RegionNo+1
          CALL Message_Output(Message_Info,' Region ',RegionNo)
          CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
          CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT)
          SELECT CASE (SELECT)
            CASE (Model_Relative)
              absolute=.false.
            CASE(Model_Absolute)
              absolute=.true.
            CASE DEFAULT
              GO TO 8001
          END SELECT
          READ(line(ibuff:),*,ERR=8001,END=9998) tmpb1,regionfile
! 
!  input of Region and storing in Region varibles
! 
          CALL InputRegionFile(regionfile,Region,GridX,GridY                &
&                ,SimParameters%kx,SimParameters%ky,errorL)
          IF (errorL) GO TO 8002
          IF (absolute) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  Region_b1(i,j)=tmpb1/MultiplierRealVar(19)
                END IF
              END DO
            END DO
            tempstr='Absolute'
          ELSE
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  Region_b1(i,j)=Region_b1(i,j)*tmpb1
                END IF
              END DO
            END DO
            tempstr='Relative'
          END IF
          CALL Message_Output(Message_Info,'Input '//tempstr//' Erodibility Region '//trim(regionfile))
          IF (absolute) THEN
            CALL Message_Output(Message_Info,'       b1= ',sngl(tmpb1*MultiplierRealVar(19)))
          ELSE
            CALL Message_Output(Message_Info,'       b1_multiplier= ',sngl(tmpb1))
          END IF
      END SELECT
      GO TO 8002
! 
 8000 CONTINUE
      CLOSE(UNIT=unitno,STATUS='keep')
      RETURN
! 
 8001 CALL Message_Output(Message_WarnContinue,'Incomprehensible input in file '//trim(SimParameters%FileFactor))
      CALL Message_Output(Message_WarnContinue,line)
      GO TO 8002
! 
 9998 CALL Message_Output(Message_WarnContinue,'Missing DATA in file'//trim(SimParameters%FileFactor))
      CALL Message_Output(Message_WarnContinue,line)
      GO TO 8002
 9999 CALL Message_Output(Message_ErrorStop,'Error opening erosion file '//trim(SimParameters%FileFactor))
      STOP
END SUBROUTINE InputRegion_b1
! 
!  ===================================================================
!  ===================================================================
!            Input spatially distributed General Erosion Model
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE InputRegion_b1m1n1(Region_b1,Region_m1,Region_n1              &
&           ,GridX,GridY,absolute,LowX,HighX,LowY,HighY,SimParameters)
  USE Support
  USE Multipliers
  USE ModelFile
  USE Setup
  USE SiberiaTypes
  USE InputOutput
  IMPLICIT NONE
! 
    INTEGER,parameter :: unitno=20,LineLgth=1000
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Region_b1,Region_m1,Region_n1
    LOGICAL :: absolute
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ibuff,RegionNo,Region(GridX,GridY)
    REAL(KIND(0.0D0)) :: tmpb1,tmpm1,tmpn1
    CHARACTER(LineLgth) :: line,buffer,regionfile
    LOGICAL :: errorL
    DATA RegionNo / 0 /
! 
    CHARACTER(8) :: tempstr
!
    CHARACTER(80) :: atom
    INTEGER :: SELECT
! 
      OPEN(UNIT=unitno,FILE=SimParameters%FileFactor,STATUS='old',ERR=9999)
      rewind(UNIT=unitno)
! 
!  READ file header
! 
      CALL Message_Output(Message_Info,'-- Region input for Erosion Model')
      READ(unitno,6000) buffer
      CALL Str_UpperCase(buffer)
      ibuff=1
      CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
      IF (atom(1:) /= 'SIBERIA') THEN
        CALL Message_Output(Message_ErrorStop,'Invalid header in Erosion Model File '//trim(SimParameters%FileFactor))
      END IF
      DO i=2,4
        READ(unitno,*)
      END DO
 8002 line=' '
      READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
      line=adjustl(line)
 8003 IF (line(1:1) == '#' .or. line(1:1) == '!' .or. line(1:1) == ' ') GO TO 8002
! 
      ibuff=1
      buffer=line
      CALL Str_UpperCase(buffer)
      CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
      CALL Str_mtchcomm1(atom,commands,SELECT)
      SELECT CASE (SELECT)
        CASE(Model_Erosion)
          RegionNo=RegionNo+1
          CALL Message_Output(Message_Info,' Region ',RegionNo)
          CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
          CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT)
          SELECT CASE (SELECT)
            CASE (Model_Relative)
              absolute=.false.
            CASE (Model_Absolute)
              absolute=.true.
            CASE DEFAULT
              GO TO 8001
          END SELECT
! 
          CONTINUE
          READ(line(ibuff:),*,ERR=8001,END=9998) tmpb1,tmpm1                      &
&            ,tmpn1,regionfile
! 
!  input of Region and storing in Region varibles
! 
          CALL InputRegionFile(regionfile,Region,GridX,GridY                &
&                ,SimParameters%kx,SimParameters%ky,errorL)
          IF (errorL) GO TO 8002
          IF (absolute) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  Region_b1(i,j)=tmpb1/MultiplierRealVar(19)
                  Region_m1(i,j)=tmpm1/MultiplierRealVar(20)
                  Region_n1(i,j)=tmpn1/MultiplierRealVar(21)
                END IF
              END DO
            END DO
            tempstr='Absolute'
          ELSE
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  Region_b1(i,j)=Region_b1(i,j)*tmpb1
                  Region_m1(i,j)=Region_m1(i,j)*tmpm1
                  Region_n1(i,j)=Region_n1(i,j)*tmpn1
                END IF
              END DO
            END DO
            tempstr='Relative'
          END IF
          CALL Message_Output(Message_Info,'Input '//tempstr//' Erosion Model Region '//trim(regionfile))
          IF (absolute) THEN
            CALL Message_Output(Message_Info,'       b1= '                              &
&            ,sngl(tmpb1*MultiplierRealVar(19))                                         &
&            ,' m1= ',sngl(tmpm1*MultiplierRealVar(20))                                 &
&            ,' n1= ',sngl(tmpn1*MultiplierRealVar(21)))
          ELSE
            CALL Message_Output(Message_Info,'       b1_multiplier= ',sngl(tmpb1)       &
&            ,' m1_multiplier= ',sngl(tmpm1),' n1_multiplier= ',sngl(tmpn1))
          END IF
        CASE(Model_Erodibility)
          RegionNo=RegionNo+1
          CALL Message_Output(Message_Info,' Region ',RegionNo)
          CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
          CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT)
          SELECT CASE (SELECT)
            CASE (Model_Relative)
              absolute=.false.
            CASE(Model_Absolute)
              absolute=.true.
            CASE DEFAULT
              GO TO 8001
          END SELECT
          READ(line(ibuff:),*,ERR=8001,END=9998) tmpb1,regionfile
! 
!  input of Region and storing in Region varibles
! 
          CALL InputRegionFile(regionfile,Region,GridX,GridY                &
&                ,SimParameters%kx,SimParameters%ky,errorL)
          IF (errorL) GO TO 8002
          IF (absolute) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  Region_b1(i,j)=tmpb1/MultiplierRealVar(19)
                END IF
              END DO
            END DO
            tempstr='Absolute'
          ELSE
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  Region_b1(i,j)=Region_b1(i,j)*tmpb1
                END IF
              END DO
            END DO
            tempstr='Relative'
          END IF
          CALL Message_Output(Message_Info,'Input '//tempstr//         &
&                 ' Erodibility Region '//trim(regionfile))
          IF (absolute) THEN
            CALL Message_Output(Message_Info,'       b1= '             &
&                ,sngl(tmpb1*MultiplierRealVar(19)))
          ELSE
            CALL Message_Output(Message_Info,'       b1_multiplier= '  &
&                ,sngl(tmpb1))
          END IF
          CALL Message_Output(Message_Info,' ')
      END SELECT
      GO TO 8002
! 
 8000 CONTINUE
      CLOSE(UNIT=unitno,STATUS='keep')
      RETURN
! 
 8001 CALL Message_Output(Message_WarnContinue                         &
&             ,'Incomprehensible input in file '                       &
&            //trim(SimParameters%FileFactor))
      CALL Message_Output(Message_WarnContinue,line)
      GO TO 8002
! 
 9998 CALL Message_Output(Message_WarnContinue,'Missing DATA in file'  &
&            //trim(SimParameters%FileFactor))
      CALL Message_Output(Message_WarnContinue,line)
      GO TO 8002
 9999 CALL Message_Output(Message_ErrorStop                            &
&           ,'Error opening erosion file '                             &
&          //trim(SimParameters%FileFactor))
      STOP
END SUBROUTINE InputRegion_b1m1n1
! 
!  ===================================================================
!  ===================================================================
!                           CIF Factor
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE MyCIFactor                                                       &
&            (AreaTerm,b,m,n,DetCIF,IrregularBoundary,Domain,DirChg         &
&            ,vz,s0,Y,Z,Area,Direct,GridX,GridY                             &
&            ,LowX,HighX,LowY,HighY,DirectDinf,DirWeights,SimParameters)
  USE Multipliers
  USE SiberiaTypes
  USE Support
  USE Setup
  USE openMPsupport
  IMPLICIT NONE
! 
!  This routine is the standard MODULE for inclusion of user-defined
!  temporal and spatial variation in discharge. ModeRunoff in the
!  MODULE 'Parameters' controls what TYPE of user defined
!  model will be used. NOTE: ModeRunoff=0 should not be changed because
!  this is the standard spatially uniform, temporally constant CASE.
!  For new models add options TO the computed GO TO at the start of
!  the routine.
! 
!  NOTE: For the reasons of efficiency this routine does not RETURN
!  discharge but it returns the FUNCTION of discharge
!                 m
!     AreaTerm = Q
! 
!  When this routine is called by the sediment transport MODULE
!  THEN the values taken passed into this routine are
! 
!     |--------------------------------------|
!     |     routine        |  b  |  m  |  N  |
!     ---------------------------------------|
!     |  CHANNEL           | b5  |  m5 |  N5 |
!     |--------------------------------------|
! 
  INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
  INTEGER,DIMENSION(GridX,GridY) :: Direct,DirectDinf
  REAL(KIND(0.0D0)) :: b,m,n
  REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Z,y,s0,vz,AreaTerm                        &
&         ,Area,DirWeights
  LOGICAL :: Domain(GridX,GridY),IrregularBoundary, DetCIF,DirChg
  TYPE(LocalParameters) :: SimParameters
! 
! 
  INTEGER :: i,j,ModeRunoff,ModeChannel,ErrorNo
  REAL(KIND(0.0D0)) :: b3m,mm3,b5,m5,n5,b3,m3
  LOGICAL :: absolute
! 
    ModeRunoff=SimParameters%ModeRunoff
    ModeChannel=SimParameters%ModeChannel
    b5=SimParameters%b5
    m5=SimParameters%m5
    n5=SimParameters%n5
    b3=SimParameters%b3
    m3=SimParameters%m3
! 
    IF (ModeChannel < 0) THEN
      CALL Message_Output(Message_ErrorStop,'Internal error in MyCIF')
    END IF
    IF (FirstCIF) DirChg=.true.
    SELECT CASE (ModeChannel)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorStop,' ModeChannel = ',ModeChannel,' is not defined')
! 
!  ModeChannel = 3; regional changes in b5 and m5
! --------------------------------------------
! 
       CASE (3)
         IF (FirstCIF) THEN
          allocate(Region_b5(1:GridX,1:GridY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Region_b5','SIBERIA_MyCIFactor')
          allocate(Region_m5(1:GridX,1:GridY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Region_m5','SIBERIA_MyCIFactor')
          allocate(Region_n5(1:GridX,1:GridY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Region_n5','SIBERIA_MyCIFactor')

!  Initialise the Region array TO the DEFAULT value

           DO j=LowY,HighY
             DO i=LowX,HighX
                 Region_b5(i,j)=b5
                 Region_m5(i,j)=m5
                 Region_n5(i,j)=n5
             END DO
           END DO
! 
!  Input the Region DATA
! 
           CALL InputRegionChannel(SimParameters%FileChannels,Region_b5           &
&                 ,Region_m5,Region_n5,GridX,GridY                       &
&                 ,absolute,LowX,HighX,LowY,HighY,SimParameters)
         END IF
         mm3=m*(m3-1)
         b3m=b3**m
         IF(IrregularBoundary) THEN
           DO j=LowY,HighY
             DO i=LowX,HighX
               IF(Domain(i,j))THEN
                 AreaTerm(i,j)=AreaTerm(i,j)**m                                 &
&                      *Area(i,j)**(m*(Region_m5(i,j)-1))
               END IF
             END DO
           END DO
         ELSE
           DO j=LowY,HighY
             DO i=LowX,HighX
               AreaTerm(i,j)=AreaTerm(i,j)**m                                   &
&                      *Area(i,j)**(m*(Region_m5(i,j)-1))
             END DO
           END DO
         END IF
      CASE (4)
        CALL Message_Output(Message_ErrorStop,' ModeChannel = ',ModeChannel,' is not yet implemented')
      END SELECT
! 
!  standard SUBROUTINE exit processing
! 
 9999 FirstCIF=.false.
      RETURN
 9000 CALL Message_Output(Message_ErrorStop,'Error opening file for '       &
&          //'ModeChannel =',ModeChannel,' in '//SimParameters%FileChannels)
END SUBROUTINE MyCIFactor
! 
!  ===================================================================
!  ===================================================================
!                   Input Region Channel DATA
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE InputRegionChannel(FileName,Region_b5,Region_m5                  &
&         ,Region_n5,GridX,GridY,absolute,LowX,HighX,LowY,HighY             &
&         ,SimParameters)
  USE SiberiaTypes
  USE Support
  USE Multipliers
  USE Setup
  USE ModelFile
  USE InputOutput
  IMPLICIT NONE
! 
    INTEGER,parameter :: unitno=20,LineLgth=1000
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Region_b5,Region_m5,Region_n5
    CHARACTER(80) :: FileName
    LOGICAL :: absolute
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ibuff,RegionNo,Region(GridX,GridY)
    REAL(KIND(0.0D0)) :: tmpb5,tmpm5,tmpn5
    CHARACTER(LineLgth) :: line,buffer,regionfile
    LOGICAL :: errorL
    DATA RegionNo / 0/
! 
    CHARACTER(8) :: tempstr
!
    CHARACTER(80) :: atom
    INTEGER :: SELECT
! 
    OPEN(UNIT=unitno,FILE=FileName,STATUS='old',ERR=9999)
    rewind(UNIT=unitno)
! 
!  READ file header
! 
    CALL Message_Output(Message_Info,' -- Region input for Channel Model')
    READ(unitno,6000) buffer
    CALL Str_UpperCase(buffer)
    ibuff=1
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    IF (atom(1:) /= 'SIBERIA') THEN
      CALL Message_Output(Message_ErrorStop,'Invalid header in Channel Model File'//trim(FileName))
    END IF
    DO i=2,4
      READ(unitno,*)
    END DO
 8002 line=' '
    READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
    line=adjustl(line)
 8003 IF (line(1:1) == '#' .or. line(1:1) == '!' .or. line(1:1) == ' ') GO TO 8002
! 
    ibuff=1
    buffer=line
    CALL Str_UpperCase(buffer)
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    CALL Str_mtchcomm1(atom,commands,SELECT)
    SELECT CASE (SELECT)
      CASE (Model_Channel)
        RegionNo=RegionNo+1
        CALL Message_Output(Message_Info,' Region ',RegionNo)
        CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
        CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT)
        SELECT CASE (SELECT)
          CASE DEFAULT
            GO TO 8001
          CASE (Model_Relative)
            absolute=.false.
          CASE (Model_Absolute)
            absolute=.true.
        END SELECT
! 
        READ(line(ibuff:),*) tmpb5,tmpm5,tmpn5,regionfile
! 
!  input of Region and storing in Region varibles
! 
        CALL InputRegionFile(regionfile,Region,GridX,GridY               &
&                  ,SimParameters%kx,SimParameters%ky,errorL)
        IF (errorL) GO TO 8002
        IF (absolute) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Region(i,j) /= 0) THEN
                Region_b5(i,j)=tmpb5/MultiplierRealVar(24)
                Region_m5(i,j)=tmpm5/MultiplierRealVar(27)
                Region_n5(i,j)=tmpn5/MultiplierRealVar(25)
              END IF
            END DO
          END DO
          tempstr='Absolute'
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Region(i,j) /= 0) THEN
                Region_b5(i,j)=Region_b5(i,j)*tmpb5
                Region_m5(i,j)=Region_m5(i,j)*tmpm5
                Region_n5(i,j)=Region_n5(i,j)*tmpn5
              END IF
            END DO
          END DO
          tempstr='Relative'
        END IF 
        CALL Message_Output(Message_Info,'     Input '//tempstr//' Runoff Region '  &
&             //trim(regionfile))
        IF (absolute) THEN
          CALL Message_Output(Message_Info,                                         &
&             '       b5 = ',sngl(tmpb5*MultiplierRealVar(24))                      &
&            ,' m5 = ',sngl(tmpm5*MultiplierRealVar(27))                            &
&            ,' n5 = ',sngl(tmpn5*MultiplierRealVar(25)))
        ELSE
          CALL Message_Output(Message_Info,'       b5_multiplier = ',sngl(tmpb5)    &
&            ,'  m5_multiplier = ',sngl(tmpm5),' n5_multiplier = ',sngl(tmpn5))
        END IF
        CALL Message_Output(Message_Info,' ')
    END SELECT
    GO TO 8002
! 
 8000 CONTINUE
    CLOSE(UNIT=unitno,STATUS='keep')
    RETURN
! 
 8001 CALL Message_Output(Message_WarnContinue,'Incomprehensible input 1 in file '//trim(FileName))
    CALL Message_Output(Message_WarnContinue,line)
    GO TO 8002
! 
 9999 CALL Message_Output(Message_ErrorStop,'Error opening Channel model file '//trim(FileName))
END SUBROUTINE InputRegionChannel
! 
!  ===================================================================
!  ===================================================================
!                           RUNOFF Models
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE MyRunoff                                                       &
&            (AreaTerm,b,m,DetCIF,IrregularBoundary,Domain,DirChg         &
&            ,vz,s0,Y,Z,Area,Direct,GridX,GridY                           &
&            ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS            &
&            ,DirectDinf,DirWeights,Discharge,SimParameters,tottime             &
&             ,timestep)
  USE SiberiaConstants
  USE Multipliers
  USE SiberiaTypes
  USE Support
  USE Setup
  USE AreaAnalysis
  USE InputOutput
  USE openMPsupport
  use hydrology
  IMPLICIT NONE
! 
!  This routine is the standard MODULE for inclusion of user-defined
!  temporal and spatial variation in discharge. ModeRunoff in the
!  MODULE 'Parameters' controls what TYPE of user defined
!  model will be used. NOTE: ModeRunoff=0 should not be changed because
!  this is the standard spatially uniform, temporally constant CASE.
!  For new models add options TO the computed GO TO at the start of
!  the routine.
! 
!  NOTE: For the reasons of efficiency this routine does not RETURN
!  discharge but it returns the FUNCTION of discharge
!                 m
!     AreaTerm = Q
! 
!  When this routine is called by the sediment transport MODULE
!  THEN the values taken passed into this routine are
! 
!     |--------------------------------|
!     |     routine        |  b  |  m  |
!     ---------------------------------|
!     |  Sediment tranport | b1  |  m1 |
!     |--------------------------------|
! 
  INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ(2,*)
  INTEGER,DIMENSION(GridX,GridY) :: Direct,DirectDinf
  REAL(KIND(0.0D0)) :: b,m,FlowInAS(2,*),tottime,timestep
  REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Z,y,s0,vz,AreaTerm                &
&         ,Area,DirWeights,Discharge
  LOGICAL :: Domain(GridX,GridY),IrregularBoundary, DetCIF,DirChg
  TYPE(LocalParameters) :: SimParameters
! 
! 
  INTEGER :: i,j,iounit,ModeRunoff,ModeErode,threadno,kx,ky,ErrorNo
  REAL(KIND(0.0D0)) :: b3m,mm3,b3,m3
  CHARACTER(20) :: line
  LOGICAL :: absolute,AreaError
  DATA iounit / 11/
! 
    ModeRunoff=SimParameters%ModeRunoff
    kx=SimParameters%kx
    ky=SimParameters%ky
    ModeErode=SimParameters%ModeErode
    b3=SimParameters%b3
    m3=SimParameters%m3
!$    threadno=omp_get_thread_num()
! 
    IF (ModeRunoff < 0) CALL Message_Output(Message_ErrorStop,'Internal error in MyRunoff')
    IF (FirstRunoff) DirChg=.true.
    SELECT CASE (MOD(ModeRunoff,20))
    CASE DEFAULT
      CALL Message_Output(Message_ErrorStop,'ModeRunoff = ',ModeRunoff,' is not defined')
! 
!  DEFAULT RUNOFF MODEL: ModeRunoff=0; spatially uniform runoff
! ----------------------------------------------------------
! 
    CASE (0)
      IF (.not.DirChg) GO TO 9999
      b3m=b3**m
      mm3=m*m3
      IF (IrregularBoundary) THEN
! 
!  Diffusion and areas for irregular boundaries
! ----------------------------------------------------------
! 
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (Area(i,j) /= 1) THEN
                AreaTerm(i,j)=b3m*Area(i,j)**mm3
              ELSE
                AreaTerm(i,j)=b3m
              END IF
              Discharge(i,j)=b3*Area(i,j)**m3
            END IF
          END DO
        END DO
      ELSE
!  Diffusion and areas for regular boundaries
! ----------------------------------------------------------
! 
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Area(i,j) /= 1) THEN
              AreaTerm(i,j)=b3m*Area(i,j)**mm3
            ELSE
              AreaTerm(i,j)=b3m
            END IF
            Discharge(i,j)=b3*Area(i,j)**m3
          END DO
        END DO
      END IF
! 
! This spot reserved for new standard models (ie. positive ModeRunoff)
! =================================================================
! 
!  ModeRunoff = 1; spatially variable runoff coefficent
! ------------------------------------------------------
! 
    CASE (1)
      IF(FirstRunoff) THEN
        allocate(runoff(GridX,GridY),stat=ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Runoff','SIBERIA_MYRUNOFF')
        CALL Message_Output(Message_Info,' -- Input of spatially variable runoff')
        IF (SimParameters%FileRunoff(1:10) == '          ') THEN
          CALL Message_Output(Message_ErrorStop                                     &
&           ,'No FileName specified for ModeRunoff =',ModeRunoff)
        END IF
        CALL ReadRunoff(SimParameters%FileRunoff,IOUnit,runoff,GridX,GridY  &
&               ,kx,ky,LowX,HighX,LowY,HighY)
      END IF
      IF (.not.DirChg) GO TO 9999
      CALL RunoffAve(AreaTerm,runoff,Direct,GridX,GridY,kx,ky                        &
&          ,IrregularBoundary,Domain,LowX,HighX,LowY,HighY)
      mm3=m*(m3-1)
      IF(IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF(Domain(i,j))THEN
              Discharge(i,j)=areaterm(i,j)*Area(i,j)**(m3-1)
              AreaTerm(i,j)=AreaTerm(i,j)**m*Area(i,j)**mm3
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            Discharge(i,j)=areaterm(i,j)*Area(i,j)**(m3-1)
            AreaTerm(i,j)=AreaTerm(i,j)**m*Area(i,j)**mm3
          END DO
        END DO
      END IF
! 
!  ModeRunoff = 2; known inflows of water from outside into the Domain
! -----------------------------------------------------------------
!
    CASE (2)
      IF(FirstRunoff) THEN
        CALL Message_Output(Message_Info,' -- Input of upstream inflows')
        IF (SimParameters%FileRunoff(1:10) == '          ') THEN
          CALL Message_Output(Message_ErrorStop                                     &
&            ,'No FileName specified for ModeRunoff =',ModeRunoff)
        END IF
        OPEN(UNIT=IOUnit,FILE=SimParameters%FileRunoff,STATUS='old',ERR=9000)
        READ(IOUnit,6000) line
6000    format(a)
        IF (.not.(line(1:15) /= ' SIBERIA RUNOFF'.or.                               &
&                 line(1:14) /= 'SIBERIA RUNOFF')) THEN
          CALL Message_Output(Message_ErrorContinue                                 &
&              ,'Specified file for ModeRunoff ='                                   &
&              ,ModeRunoff,' is not a SIBERIA runoff DATA file')
          CALL Message_Output(Message_ErrorStop,SimParameters%FileRunoff)
        END IF
        DO i=1,3
          READ(IOUnit,*)
        END DO
        READ(IOUnit,*) NoFlowIn
        IF (NoFlowIn > MaxFlowIn) THEN
          CALL Message_Output(Message_ErrorContinue,'Too many inflow points in '    &
&                 //trim(SimParameters%FileRunoff))
          CALL Message_Output(Message_ErrorStop,'',NoFlowIn,' > ',MaxFlowIn)
        END IF
        DO i=1,NoFlowIn
          READ(IOUnit,*) (FlowInIJ(j,i),j=1,2),(FlowInAS(j,i),j=1,2)
          FlowInAS(1,i)=FlowInAS(1,i)/MultiplierArea
          FlowInAS(2,i)=FlowInAS(2,i)/MultiplierSlope
          IF (FlowInIJ(1,i) < 1.or.FlowInIJ(1,i) > GridX                           &
&            .or.FlowInIJ(2,i) < 1.or.FlowInIJ(2,i) > GridY) THEN
            CALL Message_Output(Message_ErrorContinue                              &
&                   ,'  -- Coordinates of inflow point incorrect in '              &
&                  //trim(SimParameters%FileRunoff))
            CALL Message_Output(Message_ErrorStop,'',FlowInIJ(1,i),''              &
&                   ,FlowInIJ(2,i))
          END IF
        END DO
        CALL Message_Output(Message_Info,' -- Summary inflow DATA Total DATA= '    &
&                   ,NoFlowIn)
        CALL Message_Output(Message_Info,'    First DATA= ('                       &
&             ,FlowInIJ(1,1),',',FlowInIJ(2,1)                                     &
&             ,') ',FlowInAS(1,1)*MultiplierArea                                     &
&             ,' ',FlowInAS(2,1)*MultiplierSlope)
        CALL Message_Output(Message_Info,'     Last DATA= ('                       &
&             ,FlowInIJ(1,NoFlowIn),',',FlowInIJ(2,NoFlowIn)                        &
&             ,') ',FlowInAS(1,NoFlowIn)*MultiplierArea                            &
&             ,' ',FlowInAS(2,NoFlowIn)*MultiplierSlope)
        DirChg=.true.
        CLOSE(UNIT=IOUnit,STATUS='keep')
        CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                           &
&             ,Domain,AreaError,IrregularBoundary                                 &
&             ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                   &
&             ,SimParameters,GridX,GridY)
      END IF
      IF (.not.DirChg) go to 9999
      b3m=b3**m
      mm3=m*m3
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              IF (Area(i,j) /= 1) THEN
                AreaTerm(i,j)=b3m*Area(i,j)**mm3
              ELSE
                AreaTerm(i,j)=b3m
              END IF
              Discharge(i,j)=b3*area(i,j)**m3
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Area(i,j) /= 1) THEN
              AreaTerm(i,j)=b3m*Area(i,j)**mm3
            ELSE
              AreaTerm(i,j)=b3m
            END IF
            Discharge(i,j)=b3*area(i,j)**m3
          END DO
        END DO
      END IF
! 
!  ModeRunoff = 3; regional changes in b3 and m3
! --------------------------------------------
!
    CASE (3) 
        IF (FirstRunoff) THEN
          allocate(Region_b3(1:GridX,1:GridY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Region_b3','SIBERIA_MyRunoff')
          allocate(Region_m3(1:GridX,1:GridY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Region_m3','SIBERIA_MyRunoff')
!  Initialise the Region array TO the DEFAULT value
          if (IrregularBoundary) then
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  Region_b3(i,j)=b3
                  Region_m3(i,j)=m3
                END IF
              END DO
            END DO
          else
            DO j=LowY,HighY
              DO i=LowX,HighX
                Region_b3(i,j)=b3
                Region_m3(i,j)=m3
              END DO
            END DO
          end if
          CALL InputRegionRunoff(SimParameters%FileRunoff,Region_b3             &
&                 ,Region_m3,GridX,GridY                                        &
&                 ,absolute,LowX,HighX,LowY,HighY,SimParameters)
        END IF
        IF (.not.DirChg) go to 9999
        CALL RunoffAve(AreaTerm,Region_b3,Direct,GridX,GridY,kx,ky              &
&          ,IrregularBoundary,Domain,LowX,HighX,LowY,HighY)
        IF(IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF(Domain(i,j))THEN
                if (Region_m3(i,j) == 1.0) then
                  Discharge(i,j)=areaterm(i,j)
                  AreaTerm(i,j)=AreaTerm(i,j)**m
                else
                  Discharge(i,j)=areaterm(i,j)*area(i,j)**(Region_m3(i,j)-1.0)
                  AreaTerm(i,j)=AreaTerm(i,j)**m                                 &
&                      *Area(i,j)**(m*(Region_m3(i,j)-1.0))
                end if
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              if (Region_m3(i,j) == 1.0) then
                Discharge(i,j)=areaterm(i,j)
                AreaTerm(i,j)=AreaTerm(i,j)**m
              else
                Discharge(i,j)=areaterm(i,j)*area(i,j)**(Region_m3(i,j)-1.0)
                AreaTerm(i,j)=AreaTerm(i,j)**m                                   &
&                      *Area(i,j)**(m*(Region_m3(i,j)-1.0))
              end if
            END DO
          END DO
        END IF
!  layering model
    CASE (4)
      CALL Message_Output(Message_ErrorStop,'ModeRunoff = ',ModeRunoff,' is not yet implmented')
!  physically based storage model for Ranger
    CASE (5)
      call analysis_hydrology('hydrology.dat.txt',tottime,timestep              &
&           ,discharge,direct,Domain,GridX,GridY,LowX,HighX,LowY,HighY,ErrorNo)
    END SELECT
! 
!  standard SUBROUTINE exit processing
! 
 9999 FirstRunoff=.false.
    RETURN
 9000 CALL Message_Output(Message_ErrorStop,'Error opening file for ModeRunoff =',ModeRunoff                 &
&          ,'in '//SimParameters%FileRunoff)
END SUBROUTINE MyRunoff
! 
!  ===================================================================
!  ===================================================================
!                    Input Region Runoff DATA
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE InputRegionRunoff(FileName,Region_b3,Region_m3                 &
&         ,GridX,GridY,absolute,LowX,HighX,LowY,HighY,SimParameters)
  USE SiberiaTypes
  USE Support
  USE Multipliers
  USE ModelFile
  USE Setup
  USE InputOutput
  IMPLICIT NONE
! 
    INTEGER,parameter :: unitno=20,LineLgth=1000
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Region_b3,Region_m3
    CHARACTER(80) :: FileName
    LOGICAL :: absolute
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ibuff,RegionNo,Region(GridX,GridY)
    REAL(KIND(0.0D0)) :: tmpb3,tmpm3
    CHARACTER(LineLgth) :: line,buffer,regionfile
    LOGICAL :: errorL
    DATA RegionNo / 0/
! 
    CHARACTER(8) :: tempstr
!
    CHARACTER(80) :: atom
    INTEGER :: SELECT
! 
    OPEN(UNIT=unitno,FILE=FileName,STATUS='old',ERR=9999)
    rewind(UNIT=unitno)
! 
!  READ file header
! 
    CALL Message_Output(Message_Info,' -- Region input for Runoff Model')
    READ(unitno,6000) buffer
    CALL Str_UpperCase(buffer)
    ibuff=1
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    IF (atom(1:) /= 'SIBERIA') THEN
      CALL Message_Output(Message_ErrorStop                                     &
&              ,'Invalid header in Runoff Model File'//trim(FileName))
    END IF
    DO i=2,4
      READ(unitno,*)
    END DO
 8002 line=' '
     READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
     line=adjustl(line)
 8003 IF (line(1:1) == '#' .or. line(1:1) == '!' .or. line(1:1) == ' ') GO TO 8002
! 
     ibuff=1
     buffer=line
     CALL Str_UpperCase(buffer)
     CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
     CALL Str_mtchcomm1(atom,commands,SELECT)
     SELECT CASE (SELECT)
       CASE(Model_Runoff)
         RegionNo=RegionNo+1
         CALL Message_Output(Message_Info,' Region ',RegionNo)
         CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
         CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT)
         SELECT CASE (SELECT)
           CASE(Model_Relative)
             absolute=.false.
           CASE(Model_Absolute)
             absolute=.true.
           CASE DEFAULT
             GO TO 8001
         END SELECT
         READ(line(ibuff:),*) tmpb3,tmpm3,regionfile
! 
!  input of Region and storing in Region varibles
! 
         CALL InputRegionFile(regionfile,Region,GridX,GridY               &
&                  ,SimParameters%kx,SimParameters%ky,errorL)
         IF (errorL) GO TO 8002
         IF (absolute) THEN
           DO j=LowY,HighY
             DO i=LowX,HighX
               IF (Region(i,j) /= 0) THEN
                 Region_b3(i,j)=tmpb3/MultiplierRealVar(18)
                 Region_m3(i,j)=tmpm3/MultiplierRealVar(17)
               END IF
             END DO
           END DO
           tempstr='Absolute'
         ELSE
           DO j=LowY,HighY
             DO i=LowX,HighX
               IF (Region(i,j) /= 0) THEN
                 Region_b3(i,j)=Region_b3(i,j)*tmpb3
                 Region_m3(i,j)=Region_m3(i,j)*tmpm3
               END IF
             END DO
           END DO
           tempstr='Relative'
         END IF 
         CALL Message_Output(Message_Info,'Input '//tempstr//' Runoff Region '        &
&            //trim(regionfile))
         IF (absolute) THEN
           CALL Message_Output(Message_Info,                                                          &
&             '       b3= ',sngl(tmpb3*MultiplierRealVar(18))                         &
&            ,' m3= ',sngl(tmpm3*MultiplierRealVar(17)))
         ELSE
           CALL Message_Output(Message_Info,'       b3_multiplier= ',sngl(tmpb3)      &
&            ,' m3_multiplier= ',sngl(tmpm3))
         END IF
     END SELECT
     GO TO 8002
! 
 8000 CONTINUE
     CLOSE(UNIT=unitno,STATUS='keep')
     RETURN
! 
 8001 CALL Message_Output(Message_WarnContinue,'Incomprehensible input 1 in file '   &
&        //trim(FileName))
     CALL Message_Output(Message_WarnContinue,line)
     GO TO 8002
! 
 9999 CALL Message_Output(Message_ErrorStop,'Error opening runoff model file '             &
&        //trim(FileName))
END SUBROUTINE InputRegionRunoff
! 
!  ===================================================================
!  ===================================================================
!           input spatially variable runoff coefficent
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE ReadRunoff(filename,iounit,runoff,GridX,GridY,kx,ky,LowX,HighX,LowY,HighY)
  USE Support
  USE Others
  IMPLICIT NONE
! 
    INTEGER :: kx,ky,GridX,GridY,IOUnit,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: runoff(GridX,GridY)
    character(*) :: filename
! 
    INTEGER :: i,j,rx,ry,ii,jj
    REAL(KIND(0.0D0)) :: fx,fy,mapx,mapy,mapxx,mapyy                             &
&    ,rawrunoff(GridX,GridY)
    CHARACTER(255) :: line
! 
      OPEN(UNIT=IOUnit,FILE=filename,STATUS='old',ERR=9000)
      READ(IOUnit,6000) line
 6000 FORMAT(a)
      IF (.not.(trim(line) /= ' SIBERIA RUNOFF'.or.                               &
&                 trim(line) /= 'SIBERIA RUNOFF')) THEN
        CALL Message_Output(Message_ErrorContinue,'Specified Runoff file  is not a SIBERIA runoff DATA file')
        CALL Message_Output(Message_ErrorStop,trim(filename))
      END IF
      DO i=1,3
        READ(IOUnit,*)
      END DO
      READ(iounit,*) rx,ry
      DO j=1,ry
        READ(iounit,*) (rawrunoff(i,j),i=1,rx)
      END DO
      fx=float(rx-1)/(kx-2)
      fy=float(ry-1)/(ky-2)
      DO j=LowY,HighY
        DO i=LowX,HighX
          mapx=(i-2)*fx+1
          mapy=(j-2)*fy+1
          IF(i == kx) THEN
            mapxx=1.0
          ELSE
            mapxx=mapx-int(mapx)
          END IF
          IF (j == ky) THEN
            mapyy=1.0
          ELSE
            mapyy=mapy-int(mapy)
          END IF
          IF (mapx > 1.0) THEN
            ii=mapx
          ELSE
            ii=1
          END IF
          ii=min(rx-1,ii)
          IF (mapy > 1.0) THEN
            jj=mapy
          ELSE
            jj=1
          END IF
          jj=min(ry-1,jj)
          runoff(i,j)=BiLinear(mapxx,mapyy,rawrunoff(ii,jj)                    &
&              ,rawrunoff(ii+1,jj),rawrunoff(ii+1,jj+1)                          &
&              ,rawrunoff(ii,jj+1))
        END DO
      END DO
      CLOSE(UNIT=IOUnit,STATUS='keep')
    RETURN
!
 9000 CALL Message_Output(Message_ErrorStop,'Error opening file Rainfall File ='//trim(filename))
END SUBROUTINE ReadRunoff
! 
!  ===================================================================
!  ===================================================================
!    Analyse the directions created by DirAnal TO determine average runoffs
!    for all the grids. Based on areaanald1.
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE RunoffAve(AreaTerm,runoff,Direct,GridX,GridY,kx,ky               &
&               ,IrregularBoundary,Domain,LowX,HighX,LowY,HighY)
  USE SiberiaConstants
  USE Support
! 
!  'runoff' has the coefficent b3 for each point in the Grid
!  and this routine calculates the
!  average value of b3*Area for each subcatchment and stores the result
!  in 'AreaTerm'
! 
!    ie. AreaTerm=ave(b3)*Area
! 
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),kx,ky                           &
&         ,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: runoff,AreaTerm
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary
! 
    INTEGER :: i,j,k,ii,jj, NoSources,Oldi,Oldj
    INTEGER :: NoIn(GridX,GridY)
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY
! 
      NoSources=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          AreaTerm(i,j)=runoff(i,j)
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
            AreaTerm(i,j)=AreaTerm(i,j)+AreaTerm(Oldi,Oldj)
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
            AreaTerm(i,j)=AreaTerm(i,j)+AreaTerm(Oldi,Oldj)
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
      RETURN
END SUBROUTINE RunoffAve
! 
!  ===================================================================
!  ===================================================================
!                           UPLIFT Model
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE MyUplift(tottime,timestep,iTime,z,s0                               &
&                ,GridX,GridY,Uplift,SumCycle,LowX,HighX,LowY,HighY           &
&                ,SimParameters)
  USE SiberiaTypes
  USE Support
  USE openMPsupport
  IMPLICIT NONE

  INTEGER :: iTime,GridX,GridY,LowX,HighX,LowY,HighY
  REAL(KIND(0.0D0)) :: tottime,timestep,Uplift(GridX,GridY),SumCycle                     &
&         ,z(GridX,GridY),s0(GridX,GridY)
  TYPE(LocalParameters) :: SimParameters

  INTEGER :: i,j,ModeUplift,kx,ky,ErrorNo
  REAL(KIND(0.0D0)) :: pi,source1,TAmp,TPeriod,TPhase,Notch,TimeUp,SInit,temp
  PARAMETER ( pi = 3.14159265358979323846 )

    kx=SimParameters%kx
    ky=SimParameters%ky
    ModeUplift=SimParameters%ModeUplift
    TAmp=SimParameters%TAmp
    TPeriod=SimParameters%TPeriod
    TPhase=SimParameters%TPhase
    Notch=SimParameters%Notch
    TimeUp=SimParameters%TimeUp
    SInit=SimParameters%SInit
    SELECT CASE (ModeUplift)

      CASE DEFAULT
        call Message_Output(Message_ErrorStop,'Nonimplemented uplift mode in MYUSER =',ModeUplift)

!    ModeUplift=0
! ------------

      CASE(0)
        if (itime >= SimParameters%TimeUp) then
          forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=0.0
        else
          source1=TimeStep*SInit/Notch
          forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=source1
        end if
! 
!  Spatial uniform uplifts
! -------------------------
! 
      CASE(4)
        source1=TAmp*pi*2/TPeriod*cos(2*pi*(tottime/TPeriod+TPhase))
        SumCycle=SumCycle+Source1*0.5
        IF (iTime < TimeUp) THEN
          source1=source1+TimeStep*SInit/Notch
        END IF
        forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=source1
      CASE(5)
        IF ((mod(tottime,TPeriod)/TPeriod+TPhase) <= 0.5) THEN
          source1=TAmp*timestep
        ELSE
          source1=-TAmp*timestep
        END IF
        SumCycle=SumCycle+Source1*0.5
        IF (iTime < TimeUp) THEN
          source1=source1+TimeStep*SInit/Notch
        END IF
        forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=source1
      CASE(6)
        IF (mod(dble(iTime),dble(TPeriod))+TPhase == 0) THEN
          Source1=TAmp
        ELSE
          Source1=0
        END IF
        SumCycle=SumCycle+Source1*0.5
        IF (iTime < TimeUp) THEN
          source1=source1+TimeStep*SInit/Notch
        END IF
        forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=source1
! 
!  regional input of uplift
! --------------------------
! 
      CASE(3)
        IF (FirstUplift) THEN
          allocate(RegionUplift(1:GridX,1:GridY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('RegionUplift','SIBERIA_MyUplift')

!  Initialise the Region array TO the DEFAULT value

          if (Notch /= 0 .and. TimeUp > 0) then
            temp=SInit/Notch
            forall (i=lowx:highx,j=lowy:highy) RegionUplift(i,j)=temp
          ELSE
            forall (i=lowx:highx,j=lowy:highy) RegionUplift(i,j)=0.0
          END if
          CALL InputRegionUplift(SimParameters%FileUplift,RegionUplift         &
&             ,GridX,GridY,LowX,HighX,LowY,HighY,SimParameters        &
&             ,UpliftStartTime,UpliftEndTime,UpliftType)
        END IF
! 
!  all times
!  
        select case (UpliftType)
        case(1)
          IF (iTime < TimeUp) THEN
            forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=TimeStep*RegionUplift(i,j)
          ELSE
            forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=0.0
          END IF
        case(2,3,4)
          IF (iTime >= UpliftStartTime .and. iTime <= UpliftEndTime) THEN
            forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=TimeStep*RegionUplift(i,j)
          ELSE
            forall (i=lowx:highx,j=lowy:highy) Uplift(i,j)=0.0
          END IF
        end select
! 
!  regional input of aggradation/degradation
! --------------------------
! 
      CASE(2)
        IF (FirstUplift) THEN
! 
!  Initialise the Region array TO the DEFAULT value
! 
          if (.not. associated(Aggrade)) then
            allocate(Aggrade(1:GridX),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Aggrade','SIBERIA_MyUplift')
            allocate(AggradeX(1:GridX),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('AggradeX','SIBERIA_MyUplift')
            allocate(AggradeY(1:GridX),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('AggradeY','SIBERIA_MyUplift')
          end if
          CALL InputRegionAggrade(SimParameters%FileUplift         &
&             ,GridX,GridY,LowX,HighX,LowY,HighY,SimParameters)
        END IF
    END SELECT
! 
!  Clean-up
! ----------
! 
    FirstUplift=.false.
    RETURN
END SUBROUTINE MyUplift
! 
!  ===================================================================
!  ===================================================================
!                   Input Region Uplift DATA
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE InputRegionUplift(FileName,RegionUplift,GridX,GridY               &
&             ,LowX,HighX,LowY,HighY,SimParameters                  &
&             ,UpliftStartTime,UpliftEndTime,UpliftType)
  USE Support
  USE SiberiaTypes
  USE ModelFile
  USE Setup
  USE InputOutput
  IMPLICIT NONE
! 
    INTEGER,parameter :: unitno=20,LineLgth=1000
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY,UpliftType
    REAL(KIND(0.0D0)) :: RegionUplift(GridX,GridY),UpliftStartTime,UpliftEndTime
    CHARACTER(80) :: FileName
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ibuff,RegionNo,Region(GridX,GridY),ErrorNo
    REAL(KIND(0.0D0)) :: tmpUplift
    REAL(KIND(0.0D0)),allocatable :: factor(:,:)
    CHARACTER(LineLgth) :: line,buffer,regionfile
    LOGICAL :: errorL
    DATA RegionNo / 0/
! 
    integer,parameter :: NoOptions=4
    CHARACTER(20) :: options(NoOptions)
    CHARACTER(80) :: atom
    INTEGER :: SELECT
    DATA options(1)   / 'DEFAULT'/     &
&        options(2)   / 'D_T'/     &
&        options(3)   / 'D_XY_T'/     &
&        options(4)   / 'D_REGION_T'/
! 
    OPEN(UNIT=unitno,FILE=FileName,STATUS='old',ERR=9999)
    rewind(UNIT=unitno)
! 
!  READ file header
! 
    CALL Message_Output(Message_Info,' -- Region input for Uplift')
    READ(unitno,6000) buffer
    CALL Str_UpperCase(buffer)
    ibuff=1
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    IF (atom(1:) /= 'SIBERIA') THEN
      CALL Message_Output(Message_ErrorStop,'Invalid header in Tectonics Model File'//trim(FileName))
    END IF
    DO i=2,4
      READ(unitno,*)
    END DO
 8002 line=' '
    READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
    line=adjustl(line)
 8003 IF (line(1:1) == '#' .or. line(1:1) == '!' .or. line(1:1) == ' ') GO TO 8002
! 
    ibuff=1
    buffer=line
    CALL Str_UpperCase(buffer)
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    CALL Str_mtchcomm1(atom,commands,SELECT)
! 
    SELECT CASE (SELECT)
!
!    Process UPLIFT command
!
      CASE(Model_Uplift)
        RegionNo=RegionNo+1
        if (RegionNo > 1) then
          call Message_Output(Message_ErrorStop,'Called UPLIFT commands > once (not allowed) in '//FileName)
        end if
        CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
        CALL Str_mtchcomm1(atom,options,SELECT)
        select case (select)
! 
!  input of Region and storing in Region varibles
! 
          case(1)
            READ(line(ibuff:),*) tmpUplift,regionfile
            CALL InputRegionFile(regionfile,Region,GridX,GridY                  &
&                  ,SimParameters%kx,SimParameters%ky,errorL)
            IF (errorL) GO TO 8002
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  RegionUplift(i,j)=tmpUplift
                else
                  RegionUplift(i,j)=0
                END IF
              END DO
            END DO
            CALL Message_Output(Message_Info,'     Input Uplift Region '//trim(regionfile))
            CALL Message_Output(Message_Info,'       Uplift= ',sngl(tmpUplift))
! 
!  input of uplift for a set range of times
! 
          case(2)
            READ(line(ibuff:),*) UpliftStartTime,UpliftEndTime,tmpUplift
            DO j=LowY,HighY
              DO i=LowX,HighX
                RegionUplift(i,j)=tmpUplift
              END DO
            END DO
            CALL Message_Output(Message_Info,'       Uplift= ',sngl(tmpUplift))
! 
!  input of uplift for a set range of times and for a region
! 
          case(3)
            READ(line(ibuff:),*) UpliftStartTime,UpliftEndTime,tmpUplift,regionfile
            CALL InputRegionFile(regionfile,Region,GridX,GridY                  &
&                  ,SimParameters%kx,SimParameters%ky,errorL)
            IF (errorL) GO TO 8002
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  RegionUplift(i,j)=tmpUplift
                else
                  RegionUplift(i,j)=0
                END IF
              END DO
            END DO
            CALL Message_Output(Message_Info,'     Input Uplift Region '//trim(regionfile))
            CALL Message_Output(Message_Info,'       Uplift= ',sngl(tmpUplift))
! 
!  input of uplift for a set range of times and for a region
! 
          case(4)
            allocate(factor(gridx,gridy),stat=ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('factor','SIBERIA_InputRegionUplift')
            READ(line(ibuff:),*) UpliftStartTime,UpliftEndTime,tmpUplift,regionfile
            CALL InputRegionFile(regionfile,Region,GridX,GridY                  &
&                  ,SimParameters%kx,SimParameters%ky,errorL)
            call ReadInRSUi(regionfile,factor,1,gridx,gridy,.false.,.false.)
            IF (errorL) GO TO 8002
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Region(i,j) /= 0) THEN
                  RegionUplift(i,j)=tmpUplift*factor(i,j)
                else
                  RegionUplift(i,j)=0
                END IF
              END DO
            END DO
            CALL Message_Output(Message_Info,'     Input Uplift Region '//trim(regionfile))
            CALL Message_Output(Message_Info,'       Uplift= ',sngl(tmpUplift))
            deallocate(factor)
        end select
    END SELECT
    GO TO 8002
! 
 8000 CONTINUE
    CLOSE(UNIT=unitno,STATUS='keep')
    RETURN
! 
 8001 CALL Message_Output(Message_WarnContinue,'-- Incomprehensible input 1 in file '//trim(FileName))
    CALL Message_Output(Message_WarnContinue,line)
    GO TO 8002
! 
 9999 CALL Message_Output(Message_ErrorStop,'Error opening Uplift Model file '//trim(FileName))
END SUBROUTINE InputRegionUplift

!  ===================================================================
!  ===================================================================
!                  Input Region Specified Aggradation
!  ===================================================================
!  ===================================================================
! 
SUBROUTINE InputRegionAggrade(FileName,GridX,GridY               &
&             ,LowX,HighX,LowY,HighY,SimParameters)
  USE Support
  USE SiberiaTypes
  USE Setup
  USE ModelFile
  USE InputOutput
  USE openMPsupport
  IMPLICIT NONE
! 
    INTEGER,parameter :: unitno=20,MemoryIncrement=100,LineLgth=1000
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    CHARACTER(80) :: FileName
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,ibuff,RegionNo,Region(GridX,GridY)
    integer, allocatable :: tempi(:)
    REAL(KIND(0.0D0)),allocatable :: tempr(:)
    REAL(KIND(0.0D0)) :: tmpUplift
    CHARACTER(LineLgth) :: line,buffer,regionfile
    LOGICAL :: errorL
    DATA RegionNo / 0/
! 
    CHARACTER(80) :: atom
    INTEGER :: SELECT
! 
    OPEN(UNIT=unitno,FILE=FileName,STATUS='old',ERR=9999)
    rewind(UNIT=unitno)
! 
!  READ file header
! 
    CALL Message_Output(Message_Info,'-- Region input for Aggradation')
    READ(unitno,6000) buffer
    CALL Str_UpperCase(buffer)
    ibuff=1
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    IF (atom(1:) /= 'SIBERIA') THEN
      CALL Message_Output(Message_ErrorStop,'Invalid header in Aggradation Model File'//trim(FileName))
    END IF
    DO i=2,4
      READ(unitno,*)
    END DO
 8002 line=' '
    READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
    line=adjustl(line)
! 
 8003 IF (line(1:1) == '#' .or. line(1:1) == '!' .or. line(1:1) == ' ') GO TO 8002
    ibuff=1
    buffer=line
    CALL Str_UpperCase(buffer)
    CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
    CALL Str_mtchcomm1(atom,commands,SELECT)
    SELECT CASE (SELECT)
      CASE(Model_Aggradation)
        RegionNo=RegionNo+1
        CALL Message_Output(Message_Info,' Region ',RegionNo)
        READ(line(ibuff:),*) tmpUplift,regionfile
! 
!  input of Region and storing in Region varibles
! 
        CALL InputRegionFile(regionfile,Region,GridX,GridY              &
&                  ,SimParameters%kx,SimParameters%ky,errorL)
        IF (errorL) GO TO 8002
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Region(i,j) /= 0) THEN
              NoAggrade=NoAggrade+1
              if (NoAggrade > size(Aggrade)) then
                allocate(tempi(size(Aggrade)))
                tempi=AggradeX
                deallocate(AggradeX)
                allocate(AggradeX(size(tempi)+MemoryIncrement))
                AggradeX(1:size(Aggrade))=tempi
                tempi=AggradeY
                deallocate(AggradeY)
                allocate(AggradeY(size(tempi)+MemoryIncrement))
                AggradeY(1:size(Aggrade))=tempi
                deallocate(tempi)
                allocate(tempr(size(Aggrade)))
                tempr=Aggrade
                deallocate(Aggrade)
                allocate(Aggrade(size(tempr)+MemoryIncrement))
                AggradeY(1:size(Aggrade))=tempr
                deallocate(tempr)
              end if
              Aggrade(NoAggrade)=tmpUplift
              AggradeX(NoAggrade)=i
              AggradeY(NoAggrade)=j
            END IF
          END DO
        END DO
        CALL Message_Output(Message_Info,'Input Known Aggradation Region '//trim(regionfile))
        CALL Message_Output(Message_Info,'     Aggradation Rate  = ',sngl(tmpUplift))
    END SELECT
    GO TO 8002
! 
 8000 CONTINUE
    CLOSE(UNIT=unitno,STATUS='keep')
    RETURN
! 
 8001 CALL Message_Output(Message_WarnContinue,'Incomprehensible input 1 in file '//trim(FileName))
    CALL Message_Output(Message_WarnContinue,line)
    GO TO 8002
! 
 9999 CALL Message_Output(Message_WarnContinue,'Error opening Aggradation Model file '//trim(FileName))
END SUBROUTINE InputRegionAggrade


END MODULE MyModels
