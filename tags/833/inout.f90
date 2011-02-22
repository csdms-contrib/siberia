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
!
! ==========================================================================
! ==========================================================================
!   ||  Control MODULE for operation of InOut. Required so that we USE  ||
!   ||  this both traditional linear fortran programs (start and run    ||
!   ||  until they are finished like non PVM versions of SIBERIA)       || 
!   ||  and for programs that run in an event loop                      ||
!   ||  (graphics programs like VIEWER). DEFAULT behaviour is           || 
!   ||  is for correct behaviour for linear programs.                   ||
! ==========================================================================
! ==========================================================================
! 
MODULE InputOutput
  PUBLIC
  REAL,PARAMETER :: InputOutput_Version=8.17
! 
!    The operation modes that can be modified
!         IF InOutErrorMode =1 THEN after error STOP
!                           =2   "   "      "   CONTINUE
! 
  INTEGER,PARAMETER :: InOutErrorMode=1
! 
!    The current values of the operation modes
! 
  INTEGER,PRIVATE :: ErrorMode=1
  INTEGER :: WrtOutUnit
! 
!    Manipulation routines
! 
  CONTAINS
! 
    SUBROUTINE InOutSetI(Mode,value)
      INTEGER :: mode,value
        SELECT CASE (mode)
        CASE(1)
          ErrorMode=value
        END SELECT
        RETURN
    END SUBROUTINE InOutSetI
! 
    SUBROUTINE InOutGetI(Mode,value)
      INTEGER :: mode,value
        SELECT CASE (mode)
        CASE(1)
          value=ErrorMode
        CASE DEFAULT
          value=0
        END SELECT
        RETURN
    END SUBROUTINE InOutGetI
! 
!   a single line error message
! 
    SUBROUTINE InOutAlert(string)
      IMPLICIT NONE
      CHARACTER(*) :: string
        IF (ErrorMode == 1) THEN
          WRITE (*,*) string
          STOP
        ELSE
          WRITE (*,*) string
        END IF
        RETURN
    END SUBROUTINE InOutAlert
! 
!   a multi line error message
! 
    SUBROUTINE InOutAlertN(string,NoLines)
      IMPLICIT NONE
      INTEGER :: i,NoLines
      CHARACTER(*) :: string(NoLines)

        IF (ErrorMode == 1) THEN
          DO i=1,NoLines
            WRITE (*,*) string(i)
          END DO
          STOP
        ELSE
          DO i=1,NoLines
            WRITE (*,*) string(i)
          END DO
        END IF
        RETURN
    END SUBROUTINE InOutAlertN
! 
!   a single line simple diagnostic message
! 
    SUBROUTINE InOutMessage(string)
      IMPLICIT NONE
      CHARACTER(*) :: string
        WRITE (*,*) string
        RETURN
    END SUBROUTINE InOutMessage
! 
!   a multi line simple diagnostic message
! 
    SUBROUTINE InOutMessageN(string,NoLines)
      IMPLICIT NONE
      INTEGER :: i,NoLines
      CHARACTER(*) :: string(NoLines)

        DO i=1,NoLines
          WRITE (*,*) string(i)
        END DO
        RETURN
    END SUBROUTINE InOutMessageN
! 
!   initialise the wrtout unit so that wrtout is thread safe for openMP
! 
    SUBROUTINE Inout_Init(i)
      IMPLICIT NONE
      INTEGER :: i
!$      WrtOutUnit=100+i
    RETURN     
    END SUBROUTINE InOut_Init
! 
! ====================================================================
! ====================================================================
!  Routines TO input, output and manipulate RST2 and RST3 files
!         in V7.02 routines for manipulation of BND files added
! 
! 
! ================================================
!  Routine TO append a file time TO a file name
! ================================================
! 
SUBROUTINE FAppend(FileName,InFile,lFilenm,iTime,ext)
  USE Support
    IMPLICIT NONE
    INTEGER,parameter :: field=7
! 
!  ----------------------------------------------------------------------
!   SUBROUTINE TO append a time TO a generic FileName and add a specified
!   extension TO the name
!  ----------------------------------------------------------------------
!   Last Modified: GRW 10/95
!  ----------------------------------------------------------------------
! 
!     FileName : Output FileName with appended time
!     InFile   : Input file TO have time appended TO
!     lfilenm  : length of FileName
!     iTime    : Time TO appended
!     ext      : The extension TO put on the FileName
!    
! 
      CHARACTER(*) :: FileName,InFile,ext
      INTEGER :: iTime,lFilenm
! 
      INTEGER :: lgth
      CHARACTER(1000) :: tempstr
! 
      IF(ext(1:len_trim(ext))  ==  '.raw') RETURN
! 
      tempstr(1:lfilenm+1)=InFile(1:lfilenm)//'-'
      CALL Str_AppendIntToStr(FileName,tempstr(1:lfilenm+1)               &
&                 ,iTime,field)
      lgth = lfilenm + field + 1
      FileName(lgth+1:)=trim(ext)
      RETURN
END SUBROUTINE FAppend               
! 
! ======================================================================
!   extract the output time from a RST FileName
! ======================================================================
! 
SUBROUTINE ExtractTime(FileName,time,ctime,lgth)
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: time,lgth
    CHARACTER(*) :: FileName,ctime
! 
    INTEGER :: length,i,dash
    CHARACTER(1000) :: string
! 
      length=len_trim(FileName)
      string(1:length)=FileName(1:length)
      CALL Str_UpperCase(string(1:length))
      IF (string(length-3:length) == 'RST2'               &
&         .or. string(length-3:length) == 'RST3') THEN
        DO i=length-3,1,-1
          dash=i
          IF (string(i:i) == '-') GO TO 1000
        END DO
        GO TO 9999
 1000   READ(string(dash+1:length-5),*,ERR=9999,END=9999) time
        lgth=length-5-dash
        ctime=string(dash+1:length-5)
        RETURN
      ELSE
        CALL InOutAlert(' Unknown filename TYPE in ExtractTime')
      RETURN
!        STOP
      END IF
      RETURN

 9999 CALL InOutAlert(' Could not identify time in filename in ExtractTime')
!      STOP
END SUBROUTINE ExtractTime
! 
! ======================================================================
!  Routine TO output all the details of the run so that I can restart
!  from this point IF I desire TO pursue further evolution
! ======================================================================
! 
SUBROUTINE WrtOut(RanField,yy,Z,Area,gridX,gridY,filenm,init,iXXX               &
&           ,iYYY,Direct,time,IrregularBoundary,Domain,Slope,DetCIF,Version     &
&           ,ofext,iscale,er,nr,Sed,cDepth,SoilDepth,SimParameters,XYZOutput)
  USE SiberiaTypes
! 
!  ----------------------------------------------------------------------
!    SUBROUTINE TO output run DATA TO file
!  ----------------------------------------------------------------------
!    Last Modified: LT, 6/93   for output TO Mindraft .raw file.
!                   GRW 10/95  TO add support for PVM
!                   GRW 4/2002 stripped out the old PVM stuff
!  ----------------------------------------------------------------------
! 
    IMPLICIT NONE
! 
    INTEGER :: gridX,gridY,iXXX(*),iYYY(*),time,init,iscale
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL :: Version
    REAL(8) :: er(2), nr(2)
    REAL(8),dimension(gridX,gridY) :: RanField,yy,Area,Sed               &
&            ,Z,Slope,cDepth,SoilDepth
    CHARACTER(*) :: filenm
    CHARACTER :: ofext*(5)
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DetCIF,XYZOutput
    TYPE(LocalParameters) :: SimParameters
!
    INTEGER :: length
    character(1000) :: XYZFilename
! 
! Only one .raw file is output
! 
      IF(ofext(1:4)  ==  '.raw') THEN
        CALL WrtOutRaw(RanField,yy,Z,area,gridX,gridY,filenm               &
&           ,init,iXXX,iYYY,direct,time                                            &
&           ,IrregularBoundary,Domain,slope,DetCIF,Version                         &
&           , iscale, er, nr, Sed,cDepth,SoilDepth)
      ELSE
        CALL WrtOutRST2(RanField,yy,Z,area,gridX,gridY,filenm              &
&           ,init,iXXX,iYYY,direct                                                 &
&           ,IrregularBoundary,Domain,Slope,DetCIF,Version                         &
&           ,cDepth,SoilDepth,SimParameters)
      ENDIF
      XYZFilename=' '
      if (XYZOutput) then
        length=len_trim(filenm)
        IF(ofext(1:4) .eq. '.raw') THEN
          XYZFilename=filenm(1:length-3)//'grid.xyz'
        else
          XYZFilename=filenm(1:length-4)//'grid.xyz'
        end if
        call WrtOutXYZ(Z,gridX,gridY,XYZFilename,SimParameters%kx          &
&            ,SimParameters%ky,SimParameters%GridXY,SimParameters%East             &
&            ,SimParameters%North,IrregularBoundary,domain)
      end if
      RETURN
END SUBROUTINE WrtOut                                  
!
SUBROUTINE RSTOut1(RanField,yy,Z,Area,filenm,init,iXXX                          &
&           ,iYYY,Direct,time,IrregularBoundary,Domain,Slope,DetCIF,Version     &
&           ,ofext,iscale,er,nr,Sed,cDepth,SoilDepth,SimParameters,XYZOutput    &
&           ,gridX,gridY)
  USE SiberiaTypes
! 
!  ----------------------------------------------------------------------
!    SUBROUTINE TO output run DATA TO file
!  ----------------------------------------------------------------------
!    Last Modified: LT, 6/93   for output TO Mindraft .raw file.
!                   GRW 10/95  TO add support for PVM
!                   GRW 4/2002 stripped out the old PVM stuff
!  ----------------------------------------------------------------------
! 
    IMPLICIT NONE
! 
    INTEGER :: iXXX(*),iYYY(*),time,init,iscale,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL(8),dimension(gridX,gridY) :: RanField,yy,Area,Z,Slope,cDepth     &
&       ,SoilDepth,sed
    REAL :: Version
    REAL(8) :: er(2), nr(2)
    CHARACTER(*) :: filenm
    CHARACTER :: ofext*(5)
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DetCIF,XYZOutput
    TYPE(LocalParameters) :: SimParameters
!
    INTEGER :: length
    character(1000) :: XYZFilename
! 
! Only one .raw file is output
! 
      IF(ofext(1:4)  ==  '.raw') THEN
        CALL WrtOutRaw(RanField,yy,Z,area,GridX,GridY,filenm                       &
&           ,init,iXXX,iYYY,direct,time                                            &
&           ,IrregularBoundary,Domain,slope,DetCIF,Version                         &
&           , iscale, er, nr, Sed,cDepth,SoilDepth)
      ELSE
        CALL WrtOutRST2(RanField,yy,Z,area,GridX,GridY,filenm                      &
&           ,init,iXXX,iYYY,direct                                                 &
&           ,IrregularBoundary,Domain,Slope,DetCIF,Version                         &
&           ,cDepth,SoilDepth,SimParameters)
      ENDIF
      XYZFilename=' '
      if (XYZOutput) then
        length=len_trim(filenm)
        IF(ofext(1:4) .eq. '.raw') THEN
          XYZFilename=filenm(1:length-3)//'grid.xyz'
        else
          XYZFilename=filenm(1:length-4)//'grid.xyz'
        end if
        call WrtOutXYZ(Z,GridX,GridY,XYZFilename,SimParameters%kx                  &
&            ,SimParameters%ky,SimParameters%GridXY,SimParameters%East             &
&            ,SimParameters%North,IrregularBoundary,domain)
      end if
      RETURN
END SUBROUTINE RSTOut1
!
!=======================================================================
!  Routine TO output a raw file
!=======================================================================
!
  subroutine WrtOutXYZ(Z,gridX,gridY,Filename,kx,ky      &
&            ,GridXY,East,North,IrregularBoundary,domain)
  implicit none
!
    integer :: gridX,gridY,kx,ky
    real(8) :: Z(gridX,gridY),GridXY,East,North
    character(*) :: Filename
    logical :: IrregularBoundary,domain(gridX,gridY)
!
    integer,parameter :: UnitNo=32
    integer :: i,j
    real(8) :: x,y
!
!$omp critical
    open(unit=unitNo,file=filename,err=9999,status='unknown')
    do j=1,ky
      do i=1,kx
        x=(i-1)*gridXY+East
        y=(j-1)*gridXY+North
        if (IrregularBoundary) then
          if (Domain(i,j)) then
            write(unitNo,*,err=9998) x,y,z(i,j)
          else
            write(unitNo,*,err=9998) x,y,' -2'
          end if
        else
          write(unitNo,*,err=9998) x,y,z(i,j)
        end if
      end do
    end do
    close(unit=unitNo,status='keep')
!
    go to 2000
 9999 WRITE(*,*) ' ***ERROR*** Unable TO OPEN a XYZ output file'
      WRITE(*,*) '             NAME = ',trim(filename)
      CALL ElegantExit
      STOP
 9998 WRITE(*,*) ' ***ERROR*** Unable TO write to the XYZ output file at (',i,',',j,')'
      WRITE(*,*) '             NAME = ',trim(filename)
      CALL ElegantExit
      STOP
 2000 continue
!$omp end critical
  end subroutine WrtOutXYZ
! 
! ======================================================================
!  Shell routine TO WRITE out rst2 DATA when the calling PROGRAM already
!  has 'parameters.inc' declared instead of 'para1.inc'
! ======================================================================
! 
      SUBROUTINE WrtRST2Shell(RanField,y,Z,Area,gridX,gridY,outfilename          &
&           ,init,iXXX,iYYY,directions,IrregularBoundary,Domain,Slope            &
&           ,DetCIF,Version,cDepth,SoilDepth,SimParameters)
      USE SiberiaTypes
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,directions(gridX,gridY),init                        &
&           ,iXXX(*),iYYY(*)
      REAL :: version
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)         &
&           ,Z(gridX,gridY),Area(gridX,gridY)                            &
&           ,cDepth(gridX,gridY),SoilDepth(gridX,gridY)
      CHARACTER(1000) :: outfilename
      LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DetCIF
    TYPE(LocalParameters) :: SimParameters
! 
      CALL WrtOutRST2(RanField,y,Z,Area,gridX,gridY,outfilename                  &
&           ,init,iXXX                                                           &
&           ,iYYY,directions,IrregularBoundary,Domain,Slope                      &
&           ,DetCIF,Version,cDepth,SoilDepth,SimParameters)
      RETURN
      END SUBROUTINE WrtRST2Shell
! 
! ======================================================================
!  Routine TO output the rst2 file
! ======================================================================
! 
SUBROUTINE WrtOutRST2(RanField,yy,Z,Area,gridX,gridY,filenm                &
&           ,init,iXXX,iYYY,Direct,IrregularBoundary,Domain,Slope          &
&           ,DetCIF,Version,cDepth,SoilDepth,SimParameters)
  USE Multipliers
  USE SiberiaTypes
! 
!  ----------------------------------------------------------------------
!    SUBROUTINE TO output run DATA TO RST2 file
!  ----------------------------------------------------------------------
!    Last Modified: LT, 6/93   for output TO Mindraft .raw file.
!                   GRW 10/95  TO add support for PVM
!                   GRW 4/2002 stripped out all PVM stuff
!  ----------------------------------------------------------------------
! 
    IMPLICIT NONE
! 
! 
    INTEGER :: gridX,gridY,iXXX(*),iYYY(*),Direct(gridX,gridY),init
    REAL :: Version
    REAL(8),dimension(gridX,gridY) :: RanField,yy,Z,Slope,cDepth           &
&        ,SoilDepth,Area
    CHARACTER(*) :: filenm
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DetCIF
    TYPE(LocalParameters) :: SimParameters
! 
    INTEGER :: i,j,k,unitno
    REAL(8) :: aa,y(gridX,gridY) 
! 
!  This routine outputs only the ASCII form of the Restart file
!
!$omp critical
      unitno=30
      OPEN(UNIT=unitno,FILE=filenm,ERR=9999,status='unknown')
! 
      WRITE (unitno,6000) Version
 6000 FORMAT (' SIBERIA',f13.2,'              ')
      WRITE (unitno,6010) SimParameters%RunTime,SimParameters%StatsTime           &
&         ,SimParameters%kx,SimParameters%ky,SimParameters%modeIC                 &
&         ,SimParameters%TimeUp,SimParameters%ModeSolver,SimParameters%ModeDir    &
&         ,SimParameters%ModeUplift,SimParameters%ModeRandom                      &
&         ,SimParameters%ModeErode,SimParameters%ModeRunoff                       &
&         ,SimParameters%ModeChannel,SimParameters%ModeDP,SimParameters%ModeMC    &
&         ,SimParameters%DirReg,SimParameters%ModeSoil                            &
&         ,SimParameters%idummy18,SimParameters%idummy19,SimParameters%idummy20
 6010 FORMAT(10(6i10/))
      WRITE (unitno,6020)SimParameters%dZ*MultiplierRealVar(1),SimParameters%dZn*MultiplierRealVar(2)         &
&         ,SimParameters%dZHold*MultiplierRealVar(3),SimParameters%QsHold*MultiplierRealVar(4)                &
&         ,SimParameters%FactMx*MultiplierRealVar(5),SimParameters%FRanMn*MultiplierRealVar(6)                &
&         ,SimParameters%c1*MultiplierRealVar(7),SimParameters%YFix*MultiplierRealVar(8)                      &
&         ,SimParameters%FRanCV*MultiplierRealVar(9),SimParameters%b3SDs*MultiplierRealVar(10)                &
&         ,SimParameters%b3SDl*MultiplierRealVar(11),SimParameters%TAmp*MultiplierRealVar(12)                 &
&         ,SimParameters%TPeriod*MultiplierRealVar(13),SimParameters%TPhase*MultiplierRealVar(14)             &
&         ,SimParameters%FRanZ*MultiplierRealVar(15),SimParameters%a1*MultiplierRealVar(16)                   &
&         ,SimParameters%m3*MultiplierRealVar(17),SimParameters%b3*MultiplierRealVar(18)                      &
&         ,SimParameters%b1*MultiplierRealVar(19),SimParameters%m1*MultiplierRealVar(20)                      &
&         ,SimParameters%n1*MultiplierRealVar(21),SimParameters%Bulk*MultiplierRealVar(22)                    &
&         ,SimParameters%InitTimeStep*MultiplierRealVar(23),SimParameters%b5*MultiplierRealVar(24)            &
&         ,SimParameters%n5*MultiplierRealVar(25),SimParameters%SInit*MultiplierRealVar(26)                   &
&         ,SimParameters%m5*MultiplierRealVar(27),SimParameters%YHold*MultiplierRealVar(28)                   &
&         ,SimParameters%Notch*MultiplierRealVar(29),SimParameters%Cover*MultiplierRealVar(30)                &
&         ,SimParameters%s0max*MultiplierRealVar(31),SimParameters%DTime*MultiplierRealVar(32)                &
&         ,SimParameters%OTime*MultiplierRealVar(33),SimParameters%GridXY*MultiplierRealVar(34)               &
&         ,SimParameters%East*MultiplierRealVar(35),SimParameters%North*MultiplierRealVar(36)                 &
&         ,SimParameters%b6*MultiplierRealVar(37),SimParameters%m6*MultiplierRealVar(38)                      &
&         ,SimParameters%b12*MultiplierRealVar(39),SimParameters%m12*MultiplierRealVar(40)                    &
&         ,SimParameters%SDRate*MultiplierRealVar(41),SimParameters%SDExp1*MultiplierRealVar(42)              &
&         ,SimParameters%SDExp2*MultiplierRealVar(43)                                                         &
&         ,SimParameters%SMThreshold*MultiplierRealVar(44),SimParameters%SDSMWgt*MultiplierRealVar(45)       &
&         ,SimParameters%rdummy46*MultiplierRealVar(46),SimParameters%rdummy47*MultiplierRealVar(47)          &
&         ,SimParameters%rdummy48*MultiplierRealVar(48),SimParameters%rdummy49*MultiplierRealVar(49)          &
&         ,SimParameters%rdummy50*MultiplierRealVar(50)
 6020 FORMAT(5e16.8,9(/5e16.8))
      WRITE (unitno,6050) SimParameters%FileFactor
      WRITE (unitno,6050) SimParameters%FileRunoff
      WRITE (unitno,6050) SimParameters%FileUplift
      WRITE (unitno,6050) SimParameters%FileDirections
      WRITE (unitno,6050) SimParameters%FileMonteCarlo
      WRITE (unitno,6050) SimParameters%FileChannels
      WRITE (unitno,6050) SimParameters%FileLayers
      WRITE (unitno,6050) SimParameters%FileDummy8
      WRITE (unitno,6050) SimParameters%FileControl
      WRITE (unitno,6050) SimParameters%FileOthers
 6050 FORMAT(a)
      WRITE (unitno,*) init
      DO i=1,init
         WRITE (unitno,*) iXXX(i),iYYY(i)
      END DO
! 
!  Output results TO the file
! 
       DO j=1,SimParameters%ky
         DO i=1,SimParameters%kx
            y(i,j)=0
            IF ((IrregularBoundary.and.Domain(i,j))                                        &
&              .or.(.not.IrregularBoundary)) THEN
              aa=SimParameters%b5*(SimParameters%b3*Area(i,j)**SimParameters%m3)           &
&                  **SimParameters%m5*slope(i,j)**SimParameters%n5
              IF (.not.DetCIF) THEN
                IF (aa*SimParameters%c1 > 0.005) THEN
                  y(i,j)=1.0
                ELSE
                  y(i,j)=0
                END IF
                DO k=1,init
                  y(iXXX(k),iYYY(k))=SimParameters%YFix
                END DO
!                y(i,j)=1
!     &           -PDFNorm(((RealVar1(24)*RealVar1(7)*s0(i,j))**(-1/RealVar1(27))
!     &                 *Area(i,j)**(-RealVar1(17))-RealVar1(18))/RealVar1(11))
              ELSE
                y(i,j)=yy(i,j)
              END IF
            END IF
         END DO
      END DO
      DO j=1,SimParameters%ky
        DO i=1,SimParameters%kx
          IF ((IrregularBoundary.and.Domain(i,j))                                     &
&            .or.(.not.IrregularBoundary)) THEN
!    IF physical coordinates or Dinifinity
            IF (SimParameters%ModeErode >= 20.or.SimParameters%ModeRunoff >= 20       &
&               .or. SimParameters%ModeDir == 7.or.SimParameters%ModeDir == 8) THEN
              WRITE(unitno,6030,ERR=9998)                                             &
&                     slope(i,j)*MultiplierSlope,RanField(i,j),y(i,j),Z(i,j)          &
&                    ,area(i,j)*MultiplierArea,direct(i,j),cDepth(i,j)                &
&                    ,SoilDepth(i,j)
            ELSE
!    IF nondimensional and D8
              WRITE(unitno,6031,ERR=9997)                                             &
&                     slope(i,j),RanField(i,j),y(i,j),Z(i,j)                          &
&                    ,int(area(i,j)),direct(i,j),cDepth(i,j),SoilDepth(i,j)
            END IF
          ELSE
            WRITE(unitno,*) '0 0 0 0 0 5 0 0'
          END IF
 6030     FORMAT(' ',e9.4,2(' ',f6.4),' ',2(e16.10,' '),' ',i2                        &
&               ,2(' ',e10.4),' ',f6.4)
 6031     FORMAT(' ',e9.4,2(' ',f6.4),' ',e16.10,' ',i10,' ',i2                       &
&               ,2(' ',e10.4),' ',f6.4)
          GO TO 1010
 9998     IF (area(i,j) > 99999.or.RanField(i,j) >= 10.0                             &
&                      .or.RanField(i,j) < 0.0) THEN
            WRITE(*,*) ' -- Recoverable error : DATA too big for field'
            WRITE(*,*) '             NAME = ',filenm(1:50)
            WRITE(unitno,*) ' '                                                       &
&                    ,slope(i,j)*MultiplierSlope,RanField(i,j),y(i,j),Z(i,j)          &
&                    ,area(i,j)*MultiplierArea,direct(i,j),cDepth(i,j)                &
&                    ,SoilDepth(i,j)
          END IF
        GO TO 1010
 9997     IF (RanField(i,j) >= 10.0.or.RanField(i,j) < 0.0) THEN
            WRITE(*,*) ' -- Recoverable error : DATA too big for field'
            WRITE(*,*) '             NAME = ',filenm(1:50)
            WRITE(unitno,*) ' '                                                       &
&                    ,slope(i,j),RanField(i,j),y(i,j),Z(i,j)                          &
&                    ,area(i,j),direct(i,j),cDepth(i,j),SoilDepth(i,j)
          END IF
 1010   CONTINUE
        END DO
      END DO
      WRITE (*,*) 'RST2 file ',trim(filenm),' output'
      CLOSE(UNIT=unitno)
 999  go to 2000 
! 
!  error messages
!    
 9995 WRITE(*,*) ' --- ERROR --- Requested Output RST2 file already exists'
      WRITE(*,*) '             NAME = ',trim(filenm)
      CALL ElegantExit
      go to 2000
! 
 9999 WRITE(*,*) ' --- ERROR --- Unable TO OPEN a RST2 output file'
      WRITE(*,*) '             NAME = ',trim(filenm),' unit=',unitno
      CALL ElegantExit
      go to 2000
! 
2000  continue
!$omp end critical
  END SUBROUTINE WrtOutRST2
! 
! ======================================================================
!  Routine TO output a raw file
! ======================================================================
! 
  SUBROUTINE WrtOutRaw(RanField,yy,Z,Area,gridX,gridY,filenm                   &
&           ,init,iXXX,iYYY,Direct,time                                        &
&           ,IrregularBoundary,Domain,Slope,DetCIF,Version                     &
&           ,iscale,er,nr,Sed,cDepth,SoilDepth)
    USE Multipliers
    USE ModelParameters
! 
!  ----------------------------------------------------------------------
!    SUBROUTINE TO output run DATA TO file
!  ----------------------------------------------------------------------
!    Last Modified: LT, 6/93   for output TO Mindraft .raw file.
!                   GRW 10/95  TO add support for PVM
!  ----------------------------------------------------------------------
! 
      IMPLICIT NONE
! 
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*),time,init
      INTEGER,dimension(gridX,gridY) :: Direct
      REAL :: Version
      REAL(8),dimension(gridX,gridY) :: RanField,yy,Z,Slope,Area                &
&            ,cDepth,SoilDepth
      CHARACTER(*) :: filenm
      LOGICAL :: IrregularBoundary,Domain(gridX,gridY),DetCIF
! 
      INTEGER :: i,j,k,Intvar1(MaxNoInts)
      INTEGER,dimension(gridX,gridY) :: direct1
      REAL(8) :: aa,Realvar1(MaxNoReals)
      REAL(8),dimension(gridX,gridY) :: y,slope1,RanB1,y1,z1                     &
&          ,cdepth1,area1,SoilDepth1,sed
      CHARACTER(1000) :: filenm1
!      CHARACTER(80) :: filenm1
! 
      INTEGER :: unitno
! 
! Variables added for Mindraft file
! 
      CHARACTER  :: cvar*59
      REAL(8) ::    er(2), nr(2)
      INTEGER ::   iscale, idom, i1, i2
      LOGICAL ::   lexist
      CHARACTER(132) :: line1
      CHARACTER(15) ::  templine
      CHARACTER(255) :: string255(2)
      DATA lexist /.false./
      DATA cvar                                                                 &
&       /'erosion,Elevation,random,gully,Area,directions,Slope,Domain'/
      SAVE lexist,cvar
! 
!  pvm output stuff
! 
      DO i=1,MaxNoInts
        IntVar1(i)=IntVar(i)
      END DO
      DO i=1,MaxNoReals
        RealVar1(i)=RealVar(i)
      END DO
      DO j=1,IntVar(4)
        DO i=1,IntVar(3)
          Slope1(i,j)=Slope(i,j)
          RanB1(i,j)=RanField(i,j)
          y1(i,j)=yy(i,j)
          z1(i,j)=z(i,j)
          Area1(i,j)=Area(i,j)
          direct1(i,j)=Direct(i,j)
          cdepth1(i,j)=cDepth(i,j)
          SoilDepth1(i,j)=SoilDepth(i,j)
        END DO
      END DO
      filenm1=filenm
! 
!  This routine outputs only the ASCII form of the Restart file
! 
      unitno=10
! 
! Only one .raw file is output
! 
      OPEN(UNIT=unitno,FILE=filenm1, STATUS='unknown')
! 
! Mindraft file
! Only at start of file
! 
        IF(.not. lexist) THEN
          lexist=.true.
! 
! FORMAT of line 1: space separates options, options must not
! contain spaces. SO: we can't USE a FORMAT statement.
! 
7000      FORMAT(f12.6)
7003      FORMAT(f15.3)
7001      FORMAT(i5)
          line1 = 'par scale='
          i1 = 11
          templine = ' '
          WRITE(templine, 7001) iscale
          CALL MoveB(templine(1:5), line1, i1, i2)
          i1 = i2 + 1
          line1(i1:i1+3) = ' er='
          i1 = i1 + 4
          templine = ' '
          er(1) = RealVar1(35)
          er(2) = RealVar1(35) + RealVar1(34)*IntVar1(3)
          WRITE(templine, 7003) er(1)
          CALL MoveB(templine, line1, i1, i2)
          i1 = i2 + 1
          line1(i1:i1) = ','
          i1 = i1 + 1
          templine = ' '
          WRITE(templine, 7003) er(2)
          CALL MoveB(templine, line1, i1, i2)
          i1 = i2 + 1
          line1(i1:i1+3) = ' nr='
          i1 = i1 + 4
          templine = ' '
          nr(1) = RealVar1(36)
          nr(2) = RealVar1(36) + RealVar1(34)*IntVar1(4)
          WRITE(templine, 7003) nr(1)
          CALL MoveB(templine, line1, i1, i2)
          i1 = i2 + 1
          line1(i1:i1) = ','
          i1 = i1 + 1
          templine = ' '
          WRITE(templine, 7003) nr(2)
          CALL MoveB(templine, line1, i1, i2)
          i1 = i2 + 1
          line1(i1:i1+4) = ' var='
          i1 = i1 + 5
          line1(i1:len(line1)) = cvar
          WRITE(unitno, '(a)' ) line1
!          WRITE(unitno, 7001) iscale, er, nr, cvar
!7001      FORMAT('par scale=', i5, ' er=', f12.6, ',', f12.6,
!     +         ' nr=', f12.6, ',', f12.6, ' var=', a)
          WRITE(unitno, 7002) IntVar1(3), IntVar1(4)
7002      FORMAT('line', 2i4)
        ENDIF
! 
! Start of each time interval
!       WRITE(unitno, 7011 ) time
 7011   FORMAT('time', i10)
        WRITE(unitno, 6200)time
 6200   FORMAT('time', i10)
        WRITE(unitno,6210)
 6210   FORMAT( 'newgrid' )
! 
!  Output results TO the file
! 
       DO i=1,IntVar1(3)
         DO j=1,IntVar1(4)
            y(i,j)=0
            IF ((IrregularBoundary.and.Domain(i,j))                                 &
&              .or.(.not.IrregularBoundary)) THEN
              aa= RealVar1(24)*(RealVar1(18)*Area(i,j)**RealVar1(17))               &
&                  **RealVar1(27)*slope1(i,j)**RealVar1(25)
              IF (.not.DetCIF) THEN
                IF (aa*RealVar1(7) > 0.005) THEN
                  y(i,j)=1.0
                ELSE
                  y(i,j)=0
                END IF
                DO k=1,init
                  y(iXXX(k),iYYY(k))=RealVar1(8)
!                  y(iXXX(k),iYYY(k))=1
                END DO
!                y(i,j)=1
!     &           -PDFNorm(((RealVar1(24)*RealVar1(7)*s0(i,j))**(-1/RealVar1(27))
!     &                 *Area(i,j)**(-RealVar1(17))-RealVar1(18))/RealVar1(11))
              ELSE
                y(i,j)=y1(i,j)
!                y(i,j)=yy(i,j)
              END IF
            END IF
         END DO
      END DO
      DO j=1,IntVar1(4)
        DO i=1,IntVar1(3)
          idom = 0
          IF(Domain(i,j)) idom = 1
          IF ((IrregularBoundary.and.Domain(i,j))                             &
&            .or.(.not.IrregularBoundary)) THEN
            WRITE(unitno, 6220,ERR=6231)                                      &
&                 Sed(i,j),Z1(i,j),RanB1(i,j),y1(i,j)                         &
&                ,area1(i,j)*MultiplierArea,direct1(i,j)                      &
&                ,slope1(i,j)*MultiplierSlope,idom
 6220       FORMAT('Grid',2(' ',e11.4),' ',f4.1,' ',e11.4,' '                 &
&            ,i5,' ',i2,' ',e11.4,' ',i1)
          ELSE
            WRITE(unitno,6230) 'Grid 0 0 0 0 0 5 ', Sed(i,j), idom
 6230       FORMAT(a17,e11.4,' ',i1)
          END IF
          GO TO 1010
 6231     WRITE(unitno,*,ERR=9998) ' '                                        &
&                ,Sed(i,j),Z1(i,j),RanB1(i,j),y1(i,j)                         &
&                ,area1(i,j)*MultiplierArea,direct1(i,j)                      &
&                ,slope1(i,j)*MultiplierSlope,idom
 6030     FORMAT(' ',e9.4,2(' ',f6.4),' ',e13.7,' ',i4,' ',i2                 &
&               ,' ',e13.7)
 1010   CONTINUE
        END DO
      END DO
      WRITE (*,*) 'Raw file ',trim(filenm),' output'
 999  RETURN 
! 
!  error messages
!   
 9999 string255(1)=' ***ERROR*** Unable TO OPEN a RAW output file'
      string255(2)='             NAME = '//trim(filenm1)
      CALL InOutAlertN(string255,2)
      CALL ElegantExit
! 
 9998 CALL InOutAlert(' ***ERROR *** Terminal error during writing of the raw file')
      CALL ElegantExit
  END SUBROUTINE WrtOutRaw
! 
! ======================================================================
!  Reads in the HEADER of the Restart file
! ======================================================================
! 
  LOGICAL FUNCTION ReadHeader(filenm,IVersion,iInit)
    USE ModelParameters
    USE Support

!     11/10/04  GRW - removed support for files older than V6.33 (to do with the 
!                     checking of the valid header at top of file)

      IMPLICIT NONE
! 
      INTEGER :: iInit
      CHARACTER(*) :: filenm
      REAL(8) :: IVersion
! 
      INTEGER :: unitno,RstMode,NoPar,lgthname,status
      real :: rversion
! 
!$omp   critical
      lgthname=len_trim(filenm)
      unitno=File_FreeUnitNo()                  
! 
!  RST3 files no longer supported in V7 of SIBERIA
! 
      IF (filenm(lgthname-4:lgthname) == '.rst2'.or.                           &
&           filenm(lgthname-4:lgthname) == '.RST2') THEN
        rstmode=2
      ELSE
        IF (filenm(lgthname-4:lgthname) == '.rst3'.or.                         &
&             filenm(lgthname-4:lgthname) == '.RST3') THEN
          rstmode=3
          WRITE (*,*) ' Header reading with .RST3 files not supported'
          ReadHeader=.false.
          go to 1000
        ELSE
          CALL InOutAlert('FATAL ERROR '//filenm(1:40)                         &
&                    //' is not a supported restart file')
          ReadHeader=.false.
          go to 1000
        END IF
      END IF                  
      IF (rstmode <= 2) THEN
! ========================================================
!  Reading the ASCII Version of the restart file
! ========================================================
        OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999)
        write (*,*) ' -- Opening Input RST file header = ',trim(filenm)
        call file_checkHeaderTxt(unitno,'SIBERIA',status,rversion)
        if (status >= 0) then
          iversion=rversion
        ELSE
          call InOutAlert('--- FATAL ERROR --- RST2 file contains a bad header')
          CLOSE(UNIT=unitno,STATUS='keep')
          go to 1000
        END IF
        IF (IVersion < 7.00) THEN
          CALL RST2v6Header(IVersion,unitno,MaxNoInts,MaxNoReals,nopar           &
&             ,Intvar,realvar,iInit)
        ELSE
          CALL rst2v78Header(IVersion,unitno          &
&             ,Intvar,realvar,FilenameUser,MaxUser,iInit)
        END IF
        CLOSE(UNIT=unitno)
      END IF
! 
!  DO any change in interpretation in parameters from old Versions
!  of SIBERIA.
! 
      CALL UpdateV6(IVersion,IntVar,Realvar,FilenameUser)
      CALL UpdateV7(IVersion,IntVar,Realvar)
      CALL UpdateV8(IVersion,IntVar,Realvar)
      ReadHeader=.true.
      go to 1000
! 
!  fix for event loop GRW 27/9/97
! 
 9999 CONTINUE
      CALL InOutAlert(' -- Unable TO OPEN the RESTART input file '//filenm(1:50))
      ReadHeader=.false.
      go to 1000

 1000 continue            ! can't branch or return outside the OMP critical section
!$omp end critical
   END FUNCTION ReadHeader
! 
! ======================================================================
!  input of RST2 header generated by SIBERIA V6
! ======================================================================
! 
  SUBROUTINE RST2v6Header(Version,unitno,noints,noreals,nopar               &
&         ,Intvar,realvar,iInit)
      IMPLICIT NONE
! 
      INTEGER :: Intvar(*),unitno,NoPar,noints,noreals,iInit
      REAL(8) :: Realvar(*),Version
! 
      INTEGER :: i
! 
      WRITE (*,*) ' -- Input V6 RST2 file'
      DO i=1,noints
        IntVar(i)=0
      END DO
      DO i=1,noreals
        RealVar(i)=0
      END DO
      READ (unitno,*,ERR=9999,END=9998) nopar
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,8)
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,33)
      read (unitno,*) iInit
      RETURN
 9999 CALL InOutAlert('Error reading RST2 file')
      RETURN
!      STOP
 9998 CALL InOutAlert('Premature end of RST2 file')
      RETURN
!      STOP
  END SUBROUTINE RST2v6Header
! 
! ======================================================================
!  input of RST2 header generated by SIBERIA V8
! ======================================================================
! 
  SUBROUTINE rst2v78Header(Version,unitno,Intvar,realvar,FilenameUser,MaxUser,iInit)
      IMPLICIT NONE
! 
      INTEGER :: unitno,MaxUser,IntVar(*),iInit
      REAL(8) :: realvar(*),Version
      CHARACTER(*) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i
! 
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,50)
      DO i=1,MaxUser
        READ (unitno,6060,ERR=9999,END=9998) FilenameUser(i)(1:80)
 6060   FORMAT(a80)
      END DO
      read(unitno,*) iInit
      CLOSE(UNIT=unitno)
      RETURN

 9999 CALL InOutAlert(' -- Error reading header of RST2 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN
!      STOP
  END SUBROUTINE rst2v78Header
! 
! ======================================================================
!  Reads in the Restart file
! ======================================================================
! 
  SUBROUTINE ReadIn(Slope,RanField,y,z,Area,gridX,gridY,filenm,init               &
&         ,iXXX,iYYY,Direct,cDepth,SoilDepth,FlipLR,FlipTB,LowX,HighX,LowY,HighY)
    USE ModelParameters
    USE Support
      IMPLICIT NONE
! 
!  Slope = slopes for DTM
!  RanField    = random field for DTM
!  y     = channelisation of the DTM
!  z     = elevations of the DTM
!  Area  = Area draining through the DTM
!  Direct= flow directions for the DTM
!  gridX,gridY  = storage size of the arrays above
!  filenm= name of restart file TO be READ
!  init  = no of fixed Elevation points
!  iXXX,iYYY = x,y coordinates of the init fixed points
!  FlipLR,FlipTB = true IF the DATA is TO flipped in the left->right or
!                  top->bottom directions
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*),Direct(gridX,gridY),LowX,HighX,LowY,HighY
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)               &
&         ,z(gridX,gridY),cDepth(gridX,gridY),SoilDepth(gridX,gridY)                   &
&         ,Area(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB
!      CHARACTER(80) :: filenm
      CHARACTER(*) :: filenm
! 
! 
      INTEGER :: unitno
      INTEGER :: RstMode,NoPar,init,status,noreals,noints,lgthname
      REAL(8) :: IVersion
      real :: rversion
      CHARACTER(30) :: Line
!      CHARACTER*255 string255
! 
!$omp critical
      noreals=MaxNoReals
      noints=MaxNoInts
      lgthname=len_trim(filenm)
      LowX=1
      HighX=1
      LowY=1
      HighY=1
      unitno=File_FreeUnitNo()                  
! 
!  RST1 files no longer supported in V7 of SIBERIA
! 
      IF (filenm(lgthname-4:lgthname) == '.rst2'.or.                                 &
&           filenm(lgthname-4:lgthname) == '.RST2') THEN
        rstmode=2
      ELSE
        IF (filenm(lgthname-4:lgthname) == '.rst3'.or.                               &
&             filenm(lgthname-4:lgthname) == '.RST3') THEN
          rstmode=3
        ELSE
          WRITE(*,*) 'FATAL ERROR ',filenm(1:40)                                     &
&                    , ' is not a supported restart file'
          STOP
        END IF
      END IF                  
      IF (rstmode <= 2) THEN
! ========================================================
!  Reading the ASCII Version of the restart file
! ========================================================
        OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999)
        write (*,*) ' -- Opening Input RST file body = ',trim(filenm)
! 
        call file_checkHeaderTxt(unitno,'SIBERIA',status,rversion)
        if (status >= 0) then
          iversion=rversion
        ELSE
          call InOutAlert('--- FATAL ERROR --- RST2 file contains a bad header')
          CLOSE(UNIT=unitno,STATUS='keep')
          go to 1000
        END IF
        IF (IVersion < 7.00) THEN
          CALL RST2v6(IVersion,unitno,noints,noreals,nopar,Intvar,realvar            &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,CDepth,SoilDepth        &
&             ,gridX,gridY,FlipLR,FlipTB)
        ELSE IF (IVersion < 8.00) THEN
          CALL RST2v7(IVersion,unitno,Intvar,realvar                          &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&             ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
       ELSE
          CALL RST2v8(IVersion,unitno,Intvar,realvar                  &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&             ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
        END IF
        CLOSE(UNIT=unitno)
      ELSE
! ========================================================
!   READ the binary Version of the file
! ========================================================
        OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999,                         &
&             form='unformatted')
        READ (unitno) nopar
        IF (Line(1:4) == 'BRAN'.or.Line(1:4) == 'SIBE') THEN
          READ(unitno) Line (5:20)
          READ(Line(7:20),*) IVersion
          READ(unitno) NoPar
        ELSE
          IVersion=6.33
        END IF
        IF (IVersion < 7.00) THEN
          CALL RST3v6(IVersion,unitno,noints,noreals,Intvar,realvar                  &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct                         &
&             ,gridX,gridY                                                           &
&             ,FlipLR,FlipTB)
        ELSE IF (IVersion < 8.00) THEN
          CALL RST3v7(IVersion,unitno,noints,noreals,Intvar,realvar                  &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct                         &
&             ,gridX,gridY                                                           &
&             ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
        ELSE
          CALL RST3v8(IVersion,unitno,noints,noreals,Intvar,realvar                  &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct                         &
&             ,gridX,gridY                                                           &
&             ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
        END IF
        CLOSE(UNIT=unitno)
      END IF
      WRITE (*,6020) '  -- SIBERIA Input File Version ',IVersion
 6020 FORMAT(a31,f10.2)
! 
!  DO any change in interpretation in parameters from old Versions
!  of SIBERIA and BRANCH.
! 
      CALL UpdateV6(IVersion,IntVar,Realvar,FilenameUser)
      CALL UpdateV7(IVersion,IntVar,Realvar)
      CALL UpdateV8(IVersion,IntVar,Realvar)
      LowX=1
      HighX=kx
      LowY=1
      HighY=ky
      go to 1000
 9999 CALL InOutAlert(' -- Unable TO OPEN the RESTART input file #'//trim(filenm)//'#')
      CALL ElegantExit
      LowX=1
      HighX=kx
      LowY=1
      HighY=ky
      go to 1000
!
 1000 continue
!$omp end critical
  END SUBROUTINE ReadIn
! 
! ======================================================================
!  Reads in the Restart file
! ======================================================================
! 
  SUBROUTINE ReadBody(Slope,RanField,y,z,Area,gridX,gridY,filenm,init               &
&         ,iXXX,iYYY,Direct,cDepth,SoilDepth,FlipLR,FlipTB,LowX,HighX,LowY,HighY    &
&         ,SkipBC)
    USE ModelParameters
    USE Support
      IMPLICIT NONE
! 
!  Slope = slopes for DTM
!  RanField    = random field for DTM
!  y     = channelisation of the DTM
!  z     = elevations of the DTM
!  Area  = Area draining through the DTM
!  Direct= flow directions for the DTM
!  gridX,gridY  = storage size of the arrays above
!  filenm= name of restart file TO be READ
!  init  = no of fixed Elevation points
!  iXXX,iYYY = x,y coordinates of the init fixed points
!  FlipLR,FlipTB = true IF the DATA is TO flipped in the left->right or
!                  top->bottom directions
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*),Direct(gridX,gridY),LowX,HighX,LowY,HighY
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)               &
&         ,z(gridX,gridY),cDepth(gridX,gridY),SoilDepth(gridX,gridY)                   &
&         ,Area(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB,SkipBC
      CHARACTER(*) :: filenm
!      CHARACTER(80) :: filenm
! 
! 
      INTEGER :: unitno,RstMode,NoPar,init,status,noreals,noints,lgthname
      real :: rversion
      REAL(8) :: IVersion
      CHARACTER(30) :: Line
!      CHARACTER*255 string255
! 
!$omp critical
      noreals=MaxNoReals
      noints=MaxNoInts
      unitno=File_FreeUnitNo()                  
      lgthname=len_trim(filenm)
      if (.not.skipBC) then
        LowX=1
        HighX=1
        LowY=1
        HighY=1
      end if
! 
!  RST1 files no longer supported in V7 of SIBERIA
! 
      IF (filenm(lgthname-4:lgthname) == '.rst2'.or.                                 &
&           filenm(lgthname-4:lgthname) == '.RST2') THEN
        rstmode=2
      ELSE
        IF (filenm(lgthname-4:lgthname) == '.rst3'.or.                               &
&             filenm(lgthname-4:lgthname) == '.RST3') THEN
          rstmode=3
        ELSE
          WRITE(*,*) 'FATAL ERROR ',filenm(1:40)                                     &
&                    , ' is not a supported restart file'
          STOP
        END IF
      END IF                  
      IF (rstmode <= 2) THEN
! ========================================================
!  Reading the ASCII Version of the restart file
! ========================================================
        OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999)
        write (*,*) ' -- Opening Input RST file body = ',trim(filenm)
! 
!   In later Versions (>=V6.34) of the code the First line idenbtifies
!   the Version of thecode that created the restart file. Early Versions
!   DO not have this header line and start straight in with the parameters
! 
        call file_checkHeaderTxt(unitno,'SIBERIA',status,rversion)
        if (status >= 0) then
          iversion=rversion
        ELSE
          call InOutAlert('--- FATAL ERROR --- RST2 file contains a bad header')
          CLOSE(UNIT=unitno,STATUS='keep')
          go to 1000
        END IF
        IF (IVersion < 7.00) THEN
          CALL RST2v6Body(IVersion,unitno,nopar                       &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,CDepth,SoilDepth        &
&             ,gridX,gridY,FlipLR,FlipTB,SkipBC)
        ELSE IF (IVersion < 8.00) THEN
          CALL RST2v7Body(IVersion,unitno                       &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&             ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
       ELSE
          CALL RST2v8Body(IVersion,unitno                       &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&             ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
        END IF
        CLOSE(UNIT=unitno)
      ELSE
! ========================================================
!   READ the binary Version of the file
! ========================================================
        OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999,                         &
&             form='unformatted')
        READ (unitno) nopar
        IF (Line(1:4) == 'BRAN'.or.Line(1:4) == 'SIBE') THEN
          READ(unitno) Line (5:20)
          READ(Line(7:20),*) IVersion
          READ(unitno) NoPar
        ELSE
          IVersion=6.33
        END IF
        IF (IVersion < 7.00) THEN
          CALL RST3v6Body(IVersion,unitno,noints,noreals                       &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct                         &
&             ,gridX,gridY                                                           &
&             ,FlipLR,FlipTB,SkipBC)
        ELSE IF (IVersion < 8.00) THEN
          CALL RST3v7Body(IVersion,unitno,noints,noreals                       &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct                         &
&             ,gridX,gridY                                                           &
&             ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
        ELSE
          CALL RST3v8Body(IVersion,unitno,noints,noreals                       &
&             ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct                         &
&             ,gridX,gridY                                                           &
&             ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
        END IF
        CLOSE(UNIT=unitno)
      END IF
      WRITE (*,6020) '  -- SIBERIA Input File Version ',IVersion
 6020 FORMAT(a31,f10.2)
      if (.not.skipBC) then
        LowX=1
        HighX=kx
        LowY=1
        HighY=ky
      end if
      go to 1000
 9999 CALL InOutAlert(' -- Unable TO OPEN the RESTART input file #'//trim(filenm)//'#')
      CALL ElegantExit
      if (.not.skipBC) then
        LowX=1
        HighX=kx
        LowY=1
        HighY=ky
      end if
      go to 1000
!
 1000 continue
!$omp end critical
  END SUBROUTINE ReadBody
! 
! ======================================================================
!  Reads in the Restart file
! ======================================================================
! 
  SUBROUTINE readinZ(slope,RanField,y,z,area,gridX,gridY,filenm,init               &
&         ,ixxx,iyyy,direct,cDepth,SoilDepth,FlipLR,FlipTB)
    USE ModelParameters
    USE Support
      IMPLICIT NONE
! 
!  slope = slopes for DTM
!  RanField    = random field for DTM
!  y     = channelisation of the DTM
!  z     = elevations of the DTM
!  area  = area draining through the DTM
!  direct= flow directions for the DTM
!  gridX,gridY  = storage size of the arrays above
!  filenm= name of restart file TO be READ
!  init  = no of fixed elevation points
!  ixxx,iyyy = x,y coordinates of the init fixed points
!  FlipLR,FlipTB = true IF the DATA is TO flipped in the left->right or
!                  top->bottom directions
! 
      INTEGER :: gridX,gridY,area,ixxx(*),iyyy(*)                           &
&         ,direct
      REAL(8) :: slope,RanField,y                                           &
&         ,z(gridX,gridY),cDepth,SoilDepth
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(1000) :: filenm
!      CHARACTER(80) :: filenm
! 
!
      INTEGER :: unitno,RstMode,NoPar,init
      INTEGER :: noreals,noints,lgthname
      REAL(8) :: iversion
      CHARACTER(30) :: Line
      CHARACTER(255) :: string255
! 
!$omp critical
      noreals=MaxNoReals
      noints=MaxNoInts
      unitno=File_FreeUnitNo()                  
      lgthname=len_trim(filenm)
! 
!  RST1 files no longer supported in V7 of SIBERIA
! 
      IF (filenm(lgthname-4:lgthname) == '.rst2'.or.                           &
&           filenm(lgthname-4:lgthname) == '.RST2') THEN
        rstmode=2
      ELSE
        IF (filenm(lgthname-4:lgthname) == '.rst3'.or.                         &
&             filenm(lgthname-4:lgthname) == '.RST3') THEN
          rstmode=3
        ELSE
          CALL InOutAlert('--- FATAL ERROR ---'//filenm(1:40)                               &
&                    //' is not a supported restart file')
      go to 1000
        END IF
      END IF                  
      IF (rstmode <= 2) THEN
! ========================================================
!  Reading the ASCII version of the restart file
! ========================================================
        OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999)
        write (*,*) ' -- Opening RST file = ',trim(filenm)
! 
!   In later versions (>=V6.34) of the code the first line idenbtifies
!   the version of thecode that created the restart file. Early versions
!   DO not have this header line and start straight in with the parameters
! 
        READ (unitno,6000) Line
 6000   FORMAT (a30)
        IF (line(2:7) == 'BRANCH'.or.line(1:6) == 'BRANCH'                           &
&          .OR.line(2:8) == 'SIBERIA'.or.line(1:7) == 'SIBERIA') THEN
          READ(line(9:30),6010) iversion
 6010     FORMAT(f21.0)
        ELSE
          CLOSE(UNIT=unitno,STATUS='keep')
          OPEN (UNIT=unitno,FILE=filenm,STATUS='old',ERR=9999)
! 
!   The last version of the code not TO INCLUDE the header line is V6.33
! 
          iversion=6.33
        END IF
        IF (iversion < 7.00) THEN
          CALL rst2v6Z(iversion,unitno,noints,noreals,nopar                          &
&             ,Intvar,realvar                                                        &
&             ,init,ixxx,iyyy,slope,RanField,y,z,area,direct,gridX,gridY             &
&             ,FlipLR,FlipTB)
        ELSE IF (iversion < 8.00) THEN
          CALL rst2v7Z(iversion,unitno,Intvar,realvar                                                        &
&             ,init,ixxx,iyyy,slope,RanField,y,z,area,direct,gridX,gridY             &
&             ,FilenameUser,MaxUser,CDepth,SoilDepth,FlipLR,FlipTB)
       ELSE
          CALL rst2v8Z(iversion,unitno,Intvar,realvar                                                        &
&             ,init,ixxx,iyyy,slope,RanField,y,z,direct,gridX,gridY             &
&             ,FilenameUser,MaxUser,CDepth,SoilDepth,FlipLR,FlipTB)
        END IF
        CLOSE(UNIT=unitno)
      ELSE
        CALL InOutAlert('RST3 files not supported in readinZ')
      END IF
      WRITE (string255,6020) ' -- SIBERIA InputFile Version ',iversion
      WRITE (*,*) String255
!   TextScrollWrite here
 6020 FORMAT(a31,f10.2)
! 
!  DO any change in interpretation in parameters from old versions
!  of SIBERIA and BRANCH.
! 
      CALL updatev6(iversion,IntVar,Realvar,FilenameUser)
      CALL updatev7(iversion,IntVar,Realvar)
      CALL updatev8(iversion,IntVar,Realvar)
      go to 1000
! 
 9999 CALL InOutAlert('-- Unable TO OPEN the RESTART input file #'//trim(filenm)//'#')
      CALL elegantexit
      go to 1000
!
 1000 continue
!$omp end critical
  END SUBROUTINE readinZ

  subroutine ReadInZRXY(Z,FileName,LowX,HighX,LowY,HighY)
    USE ModelParameters
    USE SiberiaTypes
    USE Support
      IMPLICIT NONE
! 
!  slope = slopes for DTM
!  RanField    = random field for DTM
!  y     = channelisation of the DTM
!  z     = elevations of the DTM
!  area  = area draining through the DTM
!  direct= flow directions for the DTM
!  gridX,gridY  = storage size of the arrays above
!  filenm= name of restart file TO be READ
!  init  = no of fixed elevation points
!  ixxx,iyyy = x,y coordinates of the init fixed points
!  FlipLR,FlipTB = true IF the DATA is TO flipped in the left->right or
!                  top->bottom directions
! 
      Type(ArrayRXY) :: Z
      CHARACTER(*) :: FileName
      integer,optional :: LowX,HighX,LowY,HighY
! 
!
      INTEGER :: unitno,RstMode
      INTEGER :: noreals,noints,lgthname
      REAL(8) :: iversion
      character(4) :: extension
      CHARACTER(30) :: Line
      CHARACTER(255) :: string255
! 
!$omp critical
      noreals=MaxNoReals
      noints=MaxNoInts
      unitno=10
      lgthname=len_trim(FileName)
      extension=FileName(lgthname-4:lgthname)
      call Str_UpperCase(Extension)
!   
      IF (extension(1:4) == '.RST2') THEN
        rstmode=2
      ELSE
        IF (extension(1:4) == '.RST3') THEN
          rstmode=3
        ELSE
          CALL InOutAlert('--- FATAL ERROR ---'//trim(FileName)                               &
&                    //' is not a supported restart file')
          go to 1000
        END IF
      END IF
      unitno=File_FreeUnitNo()                  
      IF (rstmode == 2) THEN
! ========================================================
!  Reading the ASCII version of the restart file
! ========================================================
        OPEN (UNIT=unitno,FILE=FileName,STATUS='old',ERR=9999)
        write (*,*) ' -- Opening RST file = ',trim(FileName)
! 
!   In later versions (>=V6.34) of the code the first line idenbtifies
!   the version of thecode that created the restart file. Early versions
!   DO not have this header line and start straight in with the parameters
! 
        READ (unitno,6000) Line
 6000   FORMAT (a30)
        line=adjustl(line)
        IF (line(1:6) == 'BRANCH'.or.line(1:7) == 'SIBERIA') THEN
          READ(line(9:30),6010) iversion
 6010     FORMAT(f21.0)
        ELSE
          CLOSE(UNIT=unitno,STATUS='keep')
          OPEN (UNIT=unitno,FILE=FileName,STATUS='old',ERR=9999)
! 
!   The last version of the code not TO INCLUDE the header line is V6.33
! 
          iversion=6.33
        END IF
        IF (iversion < 7.00) THEN
          CALL rst2v6ZRXY(unitno,Z,LowX,HighX,LowY,HighY)
        ELSE IF (iversion < 8.00) THEN
          CALL rst2v7ZRXY(unitno,Z,LowX,HighX,LowY,HighY)
       ELSE
          CALL rst2v8ZRXY(unitno,Z,LowX,HighX,LowY,HighY)
        END IF
        CLOSE(UNIT=unitno)
      ELSE
        CALL InOutAlert('RST3 files not supported in readinZRXY')
      END IF
      WRITE (string255,6020) ' -- SIBERIA InputFile Version ',iversion
      WRITE (*,*) String255
!   TextScrollWrite here
 6020 FORMAT(a31,f10.2)
      go to 1000
! 
 9999 CALL InOutAlert('-- Unable TO OPEN the RESTART input file #'//trim(FileName)//'#')
      CALL elegantexit
      go to 1000
!
 1000 continue
!$omp end critical
  end subroutine ReadInZRXY


  subroutine rst2v6ZRXY(unitno,Z,LowX,HighX,LowY,HighY)
    USE SiberiaTypes
    implicit none
      integer :: UnitNo
      Type(ArrayRXY) :: Z
      integer,optional :: LowX,HighX,LowY,HighY

      integer, parameter :: NoInts=8,NoReals=33,NoData=6
      integer :: IntVar(NoInts),init,i,j,k,XSize,YSize,ErrorNo
      REAL(KIND(0.0D0)) :: RealVar(NoInts),data(NoData)
      character(255) :: string255

      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,noints)
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,noreals)
      READ (unitno,*,ERR=9999,END=9998) init
      DO i=1,init
         READ(unitno,*,ERR=9999,END=9998)
      END DO
      XSize=IntVar(3)
      YSize=IntVar(4)
      if (present(LowX)) then
        call AllocateArray(Z,LowX,HighX,LowY,HighY,ErrorNo)
      else
        call AllocateArray(Z,1,XSize,1,YSize,ErrorNo)
      end if
      DO j=1,YSize
        DO i=1,XSize
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,NoData)
            z%data(i,j)=DATA(4)
        END DO
      END DO
 9999 CALL InOutAlert(' -- Error reading RST2 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN

 9997 WRITE(string255,*) ' -- Error reading RST2 file body @',i,j
      CALL InOutAlert(string255)
      RETURN
  end subroutine rst2v6ZRXY

  subroutine rst2v7ZRXY(unitno,Z,LowX,HighX,LowY,HighY)
    USE SiberiaTypes
    implicit none
      integer :: UnitNo
      Type(ArrayRXY) :: Z
      integer,optional :: LowX,HighX,LowY,HighY

      integer, parameter :: NoInts=20,NoReals=50,NoData=7,NoUser=10
      integer :: IntVar(NoInts),init,i,j,k,XSize,YSize,ErrorNo
      REAL(KIND(0.0D0)) :: RealVar(NoInts),data(NoData)
      character(255) :: string255

      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,noints)
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,noreals)
      DO i=1,NoUser
         READ(unitno,*,ERR=9999,END=9998)
      END DO
      READ (unitno,*,ERR=9999,END=9998) init
      DO i=1,init
         READ(unitno,*,ERR=9999,END=9998)
      END DO
      XSize=IntVar(3)
      YSize=IntVar(4)
      if (present(LowX)) then
        call AllocateArray(Z,LowX,HighX,LowY,HighY,ErrorNo)
      else
        call AllocateArray(Z,1,XSize,1,YSize,ErrorNo)
      end if
      DO j=1,YSize
        DO i=1,XSize
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,NoData)
            z%data(i,j)=DATA(4)
        END DO
      END DO
 9999 CALL InOutAlert(' -- Error reading RST2 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN

 9997 WRITE(string255,*) ' -- Error reading RST2 file body @',i,j
      CALL InOutAlert(string255)
      RETURN
  end subroutine rst2v7ZRXY

  subroutine rst2v8ZRXY(unitno,Z,LowX,HighX,LowY,HighY)
    USE SiberiaTypes
    implicit none
      integer :: UnitNo
      Type(ArrayRXY) :: Z
      integer,optional :: LowX,HighX,LowY,HighY

      integer, parameter :: NoInts=20,NoReals=50,NoData=8,NoUser=10
      integer :: IntVar(NoInts),init,i,j,k,XSize,YSize,ErrorNo
      REAL(KIND(0.0D0)) :: RealVar(NoInts),data(NoData)
      character(255) :: string255

      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,noints)
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,noreals)
      DO i=1,NoUser
         READ(unitno,*,ERR=9999,END=9998)
      END DO
      READ (unitno,*,ERR=9999,END=9998) init
      DO i=1,init
         READ(unitno,*,ERR=9999,END=9998)
      END DO
      XSize=IntVar(3)
      YSize=IntVar(4)
      if (present(LowX)) then
        call AllocateArray(Z,LowX,HighX,LowY,HighY,ErrorNo)
      else
        call AllocateArray(Z,1,XSize,1,YSize,ErrorNo)
      end if
      DO j=1,YSize
        DO i=1,XSize
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,NoData)
            z%data(i,j)=DATA(4)
        END DO
      END DO
 9999 CALL InOutAlert(' -- Error reading RST2 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN

 9997 WRITE(string255,*) ' -- Error reading RST2 file body @',i,j
      CALL InOutAlert(string255)
      RETURN
  end subroutine rst2v8ZRXY
! 
!  =====================================
!  Modify any change in PARAMETER interpretation 
!  from old Versions of SIBERIA (V6)
!  =====================================
! 
  SUBROUTINE UpdateV6(Version,IntVar,Realvar,FilenameUser)
      IMPLICIT NONE
! 
      INTEGER :: Intvar(*)
      REAL(8) :: realvar(*),Version
      CHARACTER(*) :: FilenameUser(10)
!      CHARACTER(80) :: FilenameUser(10)
! 
      INTEGER :: i,k
! 
!  The interpretation of PARAMETER REALVAR(24) (or b5) has changed
!  from Version 6.33 TO Correct an inconsistency (see SUBROUTINE CONST)
! 
      IF (Version <= 6.33) THEN
        RealVar(24)=RealVar(24)/(RealVar(18)**RealVar(27))
        RealVar(5)=0.0
      END IF
      IF (Version <= 6.38) THEN
        RealVar(3)=0.0
        RealVar(4)=0.0
        RealVar(8)=0.0
        RealVar(10)=0.0
        RealVar(11)=0.0
        RealVar(12)=0.0
        RealVar(13)=0.0
        RealVar(14)=0.0
        Realvar(16)=1.0
        RealVar(17)=1.0
      END IF
      IF (Version <= 6.41) THEN
        RealVar(5)=1000000
      END IF
      IF (Version > 6.99) RETURN
! 
!  YFix is set TO Channel for all V6 files. IMPLICIT in V6.
! 
      Realvar(8)=1
! 
!  user defined modules not available in v6
! 
      DO k=1,10
        DO i=1,80
          FilenameUser(k)(i:i)=' '
        END DO
      END DO
      RETURN
  END SUBROUTINE UpdateV6
! 
!  =====================================
!  Modify any change in PARAMETER interpretation 
!  from old Versions of SIBERIA (V8)
!  =====================================
! 
  SUBROUTINE UpdateV8(Version,IntVar,Realvar)
!  This SUBROUTINE assumes that all modifications for V6,7 changes
!  have already been done. Only V8 modification are in here.
      IMPLICIT NONE
! 
      INTEGER :: Intvar(*)
      REAL(8) :: realvar(*),Version
!  spatial averaging in erosion
      IF (Version <= 8.16) Intvar(16)=1
      RETURN
  END SUBROUTINE UpdateV8
! 
!  =====================================
!  Modify any change in PARAMETER interpretation 
!  from old Versions of SIBERIA (V7)
!  =====================================
! 
  SUBROUTINE UpdateV7(Version,IntVar,Realvar)
!  This SUBROUTINE assumes that all modifications for V6 changes
!  have already been done. Only V7 modification are in here.
      IMPLICIT NONE
! 
      INTEGER :: Intvar(*)
      REAL(8) :: realvar(*),Version
! 
!  fix gridsize, easting and northing TO DEFAULT values
!    (DO this for all Versions)
! 
      IF (Realvar(34) == 0.0) THEN
        realvar(34)=1
        realvar(35)=0
        realvar(36)=0
      END IF
!  Change in interpretation from soil porosity (never used) TO Bulk Density
      IF (Version <= 7.07) realvar(22)=1
!  Cover factor for vegetation
      IF (Version <= 7.07) Realvar(30)=1
!  Monte Carlo Mode
      IF (Version <= 7.07) Intvar(15)=0
!  spatial averaging in erosion
      IF (Version <= 7.07) Intvar(16)=1
      RETURN
  END SUBROUTINE UpdateV7
! 
!  =====================================
!  input of RST2 generated by SIBERIA V6
!  =====================================
! 
  SUBROUTINE RST2v6(Version,unitno,noints,noreals,nopar                               &
&         ,Intvar,realvar,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct               &
&         ,cDepth,SoilDepth,gridX,gridY,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,iXXX(*),iYYY(*)                                      &
&         ,Direct(gridX,gridY),Intvar(*),unitno,NoPar,noints,noreals
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)               &
&         ,z(gridX,gridY),realvar(*),Version,Area(gridX,gridY)                         &
&         ,cDepth(gridX,gridY),SoilDepth(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB
! 
      INTEGER :: i,j,k,bottomi,topi,incri,bottomj,topj,incrj
      REAL(8) :: DATA(6)
      CHARACTER(255) :: string255
! 
      WRITE (*,*) ' -- Input V6 RST2 file'
      CDepth=0
      SoilDepth=0
      DO i=1,noints
        IntVar(i)=0
      END DO
      DO i=1,noreals
        RealVar(i)=0
      END DO
      noints=8
      noreals=33
      READ (unitno,*,ERR=9999,END=9998) nopar
      nopar=noreals+noints
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,noints)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                                       &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,noreals)
      READ (unitno,*,ERR=9999,END=9998) init
! 
!  READ the rest of the restart file
! 
      DO i=1,init
         READ(unitno,*,ERR=9999,END=9998) iXXX(I),iYYY(i)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,6)
            Slope(i,j)=max(0.0d0,DATA(1))
            RanField(i,j)=DATA(2)
            y(i,j)=DATA(3)
            z(i,j)=DATA(4)
            Area(i,j)=DATA(5)
            Direct(i,j)=DATA(6)
!          READ(unitno,*,ERR=9999,END=9998)Slope(i,j),RanField(i,j)               &
!&                          ,y(i,j),z(i,j),Area(i,j),Direct(i,j)
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST2 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN

 9997 WRITE(string255,*) ' -- Error reading RST2 file body @',i,j
      CALL InOutAlert(string255)
      RETURN
  END SUBROUTINE RST2v6
! 
!  =====================================
!  input of RST2 generated by SIBERIA V8
!  =====================================
! 
  SUBROUTINE RST2v8(Version,unitno,Intvar,realvar                                                             &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY                  &
&         ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,iXXX(*),iYYY(*)                                     &
&         ,Direct(gridX,gridY),Intvar(*),unitno,MaxUser
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)              &
&         ,z(gridX,gridY),realvar(*),cDepth(gridX,gridY)                              &
&         ,SoilDepth(gridX,gridY),Version,Area(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(*) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
      CHARACTER(255) :: string255
! 
      INTEGER :: i,j,k,bottomi,topi,incri,bottomj,topj,incrj
      REAL(8) :: DATA(8)
! 
      WRITE (*,*) ' -- Input V8 RST2 file'
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT('  -- Input RST2 file too big ',2i6,                                   &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,50)
      DO i=1,MaxUser
        READ (unitno,6060,ERR=9999,END=9998) FilenameUser(i)(1:80)
 6060   FORMAT(a)
      END DO
      READ (unitno,*,ERR=9999,END=9998) init
      DO i=1,init
        READ(unitno,*,ERR=9999,END=9998) iXXX(I),iYYY(i)
      END DO
! 
!  these flip operations are required in EAMS and are not normally used in SIBERIA
! 
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
! 
!  in V8.08 and earlier the area in the files are in nondimensional units
!  in V8.09 and later the area is the actual area
!       i.e. for ModeErode>=20 the area are in physocal units (i.e. sq metres)
!            for ModeErode <20 the area is in nondimensional units
!       in both base the FORMAT of the area in the file is now in REAL rather than
!       INTEGER FORMAT (this may be important in some visualisation packages)
! 
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,8)
          IF (version < 8.09d0) THEN
!            READ(unitno,*,ERR=9997,END=9998)Slope(i,j),RanField(i,j)               &
!&                          ,y(i,j),z(i,j),Area(i,j),Direct(i,j)                        &
!&                          ,cDepth(i,j),SoilDepth(i,j)
            Slope(i,j)=max(0.0d0,DATA(1))
            RanField(i,j)=DATA(2)
            y(i,j)=DATA(3)
            z(i,j)=DATA(4)
            Area(i,j)=DATA(5)
            Direct(i,j)=DATA(6)
            CDepth(i,j)=max(0.0d0,DATA(7))
            SoilDepth(i,j)=max(0.0d0,DATA(8))
          ELSE
!            READ(unitno,*,ERR=9997,END=9998)rjunk1,RanField(i,j)               &
!&                          ,y(i,j),z(i,j),rjunk2,Direct(i,j)                           &
!&                          ,cDepth(i,j),SoilDepth(i,j)
            slope(i,j)=max(0.0d0,DATA(1)*RealVar(34))
            RanField(i,j)=DATA(2)
            y(i,j)=DATA(3)
            z(i,j)=DATA(4)
            area(i,j)=DATA(5)/Realvar(34)**2
            Direct(i,j)=DATA(6)
            CDepth(i,j)=max(0.0d0,DATA(7))
            SoilDepth(i,j)=max(0.0d0,DATA(8))
          END IF
        END DO
      END DO
      CLOSE(UNIT=unitno)
      RETURN
 9997 WRITE(string255,*) ' -- Error reading body of RST2 file',i,j
      CALL InOutAlert(string255)
      RETURN
!      STOP
 9999 CALL InOutAlert(' -- Error reading header of RST2 file')
      RETURN
!      STOP
 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN
!      STOP
  END SUBROUTINE RST2v8
! 
!  =====================================
!  input of RST2 generated by SIBERIA V7
!  =====================================
! 
  SUBROUTINE RST2v7(Version,unitno,Intvar,realvar                                                        &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&         ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,iXXX(*),iYYY(*)                                &
&         ,Direct(gridX,gridY),Intvar(*),unitno,MaxUser
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)         &
&         ,z(gridX,gridY),realvar(*),cDepth(gridX,gridY)                         &
&         ,SoilDepth(gridX,gridY),Version,Area(gridX,gridY),DATA(7)
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(1000) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i,j,k,bottomi,topi,incri,bottomj,topj,incrj
      CHARACTER(255) :: string255
! 
      WRITE (*,*) ' -- Input V7 RST2 file'
!      nopar=70
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT('  -- Input RST2 file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9997) (Realvar(i),i=1,50)
      IF (Version >= 7.03) THEN
        DO i=1,MaxUser
          READ (unitno,6060,ERR=9999,END=9997) FilenameUser(i)(1:80)
 6060     FORMAT(a80)
        END DO
      ELSE
        READ (unitno,6060,ERR=9999,END=9997) FilenameUser(1)(1:80)
      END IF
      READ (unitno,*,ERR=9999,END=9997) init
! 
!  READ the rest of the restart file
! 
      DO i=1,init
        READ(unitno,*,ERR=9999,END=9997) iXXX(I),iYYY(i)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9999,END=9998) (DATA(k),k=1,7)
          Slope(i,j)=DATA(1)
          RanField(i,j)=DATA(2)
          y(i,j)=DATA(3)
          z(i,j)=DATA(4)
          Area(i,j)=DATA(5)
          Direct(i,j)=DATA(6)
          CDepth(i,j)=max(0.0d0,DATA(7))
!          READ(unitno,*,ERR=9999,END=9998)Slope(i,j),RanField(i,j)               &
!&                          ,y(i,j),z(i,j),Area(i,j),Direct(i,j)
          SoilDepth(i,j)=0
        END DO
      END DO
      CLOSE(UNIT=unitno)
      RETURN
 9997 WRITE(string255,*) ' -- Error reading RST2 file header',i
      CALL InOutAlert(string255)
      RETURN
 9999 WRITE(string255,*) ' -- Error reading RST2 file @',i,j
      CALL InOutAlert(string255)
      RETURN
 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN
  END SUBROUTINE RST2v7
! 
!  =====================================
!  input of RST2 generated by SIBERIA V6
!  =====================================
! 
  SUBROUTINE RST2v6Body(Version,unitno,nopar            &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct               &
&         ,cDepth,SoilDepth,gridX,gridY,FlipLR,FlipTB,SkipBC)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,iXXX(*),iYYY(*)                                  &
&         ,Direct(gridX,gridY),unitno,NoPar
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)           &
&         ,z(gridX,gridY),Version,Area(gridX,gridY)                                &
&         ,cDepth(gridX,gridY),SoilDepth(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB,SkipBC
! 
      INTEGER :: i,j,k,bottomi,topi,incri,bottomj,topj,incrj,Intvar(8)             &
&               ,itemp,itemp1,itemp2
      REAL(8) :: DATA(6),realvar(33)
      CHARACTER(255) :: string255
! 
      WRITE (*,*) ' -- Input V6 RST2 file'
      cDepth=0
      SoilDepth=0
      DO i=1,8
        IntVar(i)=0
      END DO
      DO i=1,33
        RealVar(i)=0
      END DO
      READ (unitno,*,ERR=9999,END=9998) nopar
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,8)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                                       &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,33)
      if (SkipBC) then
        READ (unitno,*,ERR=9999,END=9997) itemp
        DO i=1,itemp
           READ(unitno,*,ERR=9999,END=9997)itemp1,itemp2
        END DO
      else
        READ (unitno,*,ERR=9999,END=9997) init
        DO i=1,init
           READ(unitno,*,ERR=9999,END=9997) iXXX(i),iYYY(i)
        END DO
      end if
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,6)
            Slope(i,j)=max(0.0d0,DATA(1))
            RanField(i,j)=DATA(2)
            y(i,j)=DATA(3)
            z(i,j)=DATA(4)
            Area(i,j)=DATA(5)
            Direct(i,j)=DATA(6)
!          READ(unitno,*,ERR=9999,END=9998)Slope(i,j),RanField(i,j)               &
!&                          ,y(i,j),z(i,j),Area(i,j),Direct(i,j)
        END DO
      END DO
      RETURN
 9999 CALL InOutAlert(' -- Error reading RST2 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN

 9997 WRITE(string255,*) ' -- Error reading RST2 file body @',i,j
      CALL InOutAlert(string255)
      RETURN
  END SUBROUTINE RST2v6Body
! 
!  =====================================
!  input of RST2 generated by SIBERIA V8
!  =====================================
! 
  SUBROUTINE RST2v8Body(Version,unitno                           &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY                  &
&         ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,iXXX(*),iYYY(*)                                     &
&         ,Direct(gridX,gridY),unitno,MaxUser
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)              &
&         ,z(gridX,gridY),cDepth(gridX,gridY)                                         &
&         ,SoilDepth(gridX,gridY),Version,Area(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB,SkipBC
      CHARACTER(255) :: string255
! 
      INTEGER :: i,j,k,bottomi,topi,incri,bottomj,topj,incrj                          &
&               ,Intvar(20),itemp,itemp1,itemp2
      REAL(8) :: DATA(8),realvar(60)
      CHARACTER(1000) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      WRITE (*,*) ' -- Input V8 RST2 file'
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT('  -- Input RST2 file too big ',2i6,                                   &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,50)
      DO i=1,MaxUser
        FilenameUser(i)=' '
        READ (unitno,6060,ERR=9999,END=9998) FilenameUser(i)
 6060   FORMAT(a)
      END DO
      if (SkipBC) then
        READ (unitno,*,ERR=9999,END=9997) itemp
        DO i=1,itemp
           READ(unitno,*,ERR=9999,END=9997)itemp1,itemp2
        END DO
      else
        READ (unitno,*,ERR=9999,END=9997) init
        DO i=1,init
           READ(unitno,*,ERR=9999,END=9997) iXXX(i),iYYY(i)
        END DO
      end if
! 
!  these flip operations are required in EAMS and are not normally used in SIBERIA
! 
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
! 
!  in V8.08 and earlier the area in the files are in nondimensional units
!  in V8.09 and later the area is the actual area
!       i.e. for ModeErode>=20 the area are in physocal units (i.e. sq metres)
!            for ModeErode <20 the area is in nondimensional units
!       in both base the FORMAT of the area in the file is now in REAL rather than
!       INTEGER FORMAT (this may be important in some visualisation packages)
! 
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9997,END=9998) (DATA(k),k=1,8)
          IF (version < 8.09d0) THEN
!            READ(unitno,*,ERR=9997,END=9998)Slope(i,j),RanField(i,j)               &
!&                          ,y(i,j),z(i,j),Area(i,j),Direct(i,j)                    &
!&                          ,cDepth(i,j),SoilDepth(i,j)
            Slope(i,j)=max(0.0d0,DATA(1))
            RanField(i,j)=DATA(2)
            y(i,j)=DATA(3)
            z(i,j)=DATA(4)
            Area(i,j)=DATA(5)
            Direct(i,j)=DATA(6)
            CDepth(i,j)=max(0.0d0,DATA(7))
            SoilDepth(i,j)=max(0.0d0,DATA(8))
          ELSE
!            READ(unitno,*,ERR=9997,END=9998)rjunk1,RanField(i,j)               &
!&                          ,y(i,j),z(i,j),rjunk2,Direct(i,j)                           &
!&                          ,cDepth(i,j),SoilDepth(i,j)
            slope(i,j)=max(0.0d0,DATA(1)*RealVar(34))
            RanField(i,j)=DATA(2)
            y(i,j)=DATA(3)
            z(i,j)=DATA(4)
            area(i,j)=DATA(5)/Realvar(34)**2
            Direct(i,j)=DATA(6)
            CDepth(i,j)=max(0.0d0,DATA(7))
            SoilDepth(i,j)=max(0.0d0,DATA(8))
          END IF
        END DO
      END DO
      CLOSE(UNIT=unitno)
      RETURN
 9997 WRITE(string255,*) ' -- Error reading body of RST2 file',i,j
      CALL InOutAlert(string255)
      RETURN
!      STOP

 9999 CALL InOutAlert(' -- Error reading header of RST2 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN
!      STOP
  END SUBROUTINE RST2v8Body
! 
!  =====================================
!  input of RST2 generated by SIBERIA V7
!  =====================================
! 
  SUBROUTINE RST2v7Body(Version,unitno                      &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&         ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,Direct(gridX,gridY),unitno     &
&         ,MaxUser,init,iXXX(*),iYYY(*)
      REAL(8) :: Slope(gridX,gridY),RanField(gridX,gridY),y(gridX,gridY)         &
&         ,z(gridX,gridY),cDepth(gridX,gridY)                                    &
&         ,SoilDepth(gridX,gridY),Version,Area(gridX,gridY),DATA(7)
      LOGICAL :: FlipLR,FlipTB,SkipBC
! 
      INTEGER :: i,j,k,bottomi,topi,incri,bottomj,topj,incrj,Intvar(20)          &
&               ,itemp,itemp1,itemp2
      REAL(8) :: realvar(50)
      CHARACTER(255) :: string255
      CHARACTER(1000) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      WRITE (*,*) ' -- Input V7 RST2 file'
!      nopar=70
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT('  -- Input RST2 file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9997) (Realvar(i),i=1,50)
      IF (Version >= 7.03) THEN
        DO i=1,MaxUser
          FilenameUser(i)=' '
          READ (unitno,6060,ERR=9999,END=9997) FilenameUser(i)(1:80)
 6060     FORMAT(a80)
        END DO
      ELSE
        FilenameUser(1)=' '
        READ (unitno,6060,ERR=9999,END=9997) FilenameUser(1)(1:80)
      END IF
      if (SkipBC) then
        READ (unitno,*,ERR=9999,END=9997) itemp
        DO i=1,itemp
           READ(unitno,*,ERR=9999,END=9997)itemp1,itemp2
        END DO
      else
        READ (unitno,*,ERR=9999,END=9997) init
        DO i=1,init
           READ(unitno,*,ERR=9999,END=9997) iXXX(i),iYYY(i)
        END DO
      end if
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9999,END=9998) (DATA(k),k=1,7)
          Slope(i,j)=DATA(1)
          RanField(i,j)=DATA(2)
          y(i,j)=DATA(3)
          z(i,j)=DATA(4)
          Area(i,j)=DATA(5)
          Direct(i,j)=DATA(6)
          CDepth(i,j)=max(0.0d0,DATA(7))
!          READ(unitno,*,ERR=9999,END=9998)Slope(i,j),RanField(i,j)               &
!&                          ,y(i,j),z(i,j),Area(i,j),Direct(i,j)
          SoilDepth(i,j)=0
        END DO
      END DO
      CLOSE(UNIT=unitno)
      RETURN
 9997 WRITE(string255,*) ' -- Error reading RST2 file header',i
      CALL InOutAlert(string255)
      RETURN

 9999 WRITE(string255,*) ' -- Error reading RST2 file @',i,j
      CALL InOutAlert(string255)
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN
!      STOP
  END SUBROUTINE RST2v7Body
! 
!  ==============================================================================
!  input of RST2 generated by SIBERIA V6 ((version for inputting only elevation))
!  ==============================================================================
! 
  SUBROUTINE rst2v6Z(version,unitno,noints,noreals,nopar                          &
&         ,Intvar,realvar                                                         &
&         ,init,ixxx,iyyy,slope,RanField,y,z,area,direct,gridX,gridY              &
&         ,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,area,init,ixxx(*),iyyy(*)                            &
&         ,direct,Intvar(*),unitno,NoPar,noints,noreals
      REAL(8) :: slope,RanField,y                                                 &
&         ,z(gridX,gridY),realvar(*),version
      LOGICAL :: FlipLR,FlipTB
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj
! 
      WRITE (*,*) ' -- Input V6 RST2 file'
      DO i=1,noints
        IntVar(i)=0
      END DO
      DO i=1,noreals
        RealVar(i)=0
      END DO
      noints=8
      noreals=33
      READ (unitno,*,ERR=9999,END=9998) nopar
      nopar=noreals+noints
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,noints)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                                    &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,noreals)
      READ (unitno,*,ERR=9999,END=9998) init
! 
!  READ the rest of the restart file
! 
      DO i=1,init
         READ(unitno,*,ERR=9999,END=9998) ixxx(1),iyyy(1)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      area=0
      direct=0
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9999,END=9998)slope,RanField                             &
&                          ,y,z(i,j)
        END DO
      END DO
      RETURN
 9999 CALL InOutAlert(' -- Error reading RST2 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST2 file')
      RETURN
!      STOP

  END SUBROUTINE rst2v6Z
! 
!  ==============================================================================
!  input of RST2 generated by SIBERIA V8 (version for inputting only elevation)
!  ==============================================================================
! 
  SUBROUTINE rst2v8Z(version,unitno,Intvar,realvar                                                          &
&         ,init,ixxx,iyyy,slope,RanField,y,z,direct,gridX,gridY               &
&         ,FilenameUser,MaxUser,CDepth,SoilDepth,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,ixxx(*),iyyy(*)                             &
&         ,direct,Intvar(*),unitno,MaxUser
      REAL(8) :: slope,RanField,y                                                  &
&         ,z(gridX,gridY),realvar(*),version,CDepth,SoilDepth
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(*) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
      CHARACTER(255) :: string255
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj
! 
      WRITE(*,*) '-- Input V8 RST2 file'
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT('-- Input RST2 file too big ',2i6,                                &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,50)
      DO i=1,MaxUser
        READ (unitno,6060,ERR=9999,END=9998) FilenameUser(i)(1:80)
 6060   FORMAT(a80)
      END DO
      READ (unitno,*,ERR=9999,END=9998) init
      DO i=1,init
        READ(unitno,*,ERR=9999,END=9998) ixxx(1),iyyy(1)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      CDepth=0
      SoilDepth=0
      direct=0
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9997,END=9998)slope,RanField                      &
&                          ,y,z(i,j)
        END DO
      END DO
      CLOSE(UNIT=unitno)
      RETURN
 9997 WRITE(string255,*) ' -- Error reading body of RST2 file',i,j
      CALL InOutAlert(string255)
      RETURN
!      STOP
 9999 CALL InOutAlert('-- Error reading header of RST2 file')
      RETURN
!      STOP

 9998 CALL InOutAlert('-- Premature end of RST2 file')
      RETURN
!      STOP

  END SUBROUTINE rst2v8Z
! 
!  ==============================================================================
!  input of RST2 generated by SIBERIA V7 (version for inputting only elevation)
!  ==============================================================================
! 
  SUBROUTINE rst2v7Z(version,unitno,Intvar,realvar                                                         &
&         ,init,ixxx,iyyy,slope,RanField,y,z,area,direct,gridX,gridY              &
&         ,FilenameUser,MaxUser,CDepth,SoilDepth,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,area,init,ixxx(*),iyyy(*)                            &
&         ,direct,Intvar(*),unitno,MaxUser
      REAL(8) :: slope,RanField,y                                                 &
&         ,z(gridX,gridY),realvar(*),version,CDepth                               &
&         ,SoilDepth
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(*) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj
! 
      WRITE (*,*) '-- Input V7 RST2 file'
!      nopar=70
      READ (unitno,*,ERR=9999,END=9998) (Intvar(i),i=1,20)
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT('-- Input RST2 file too big ',2i6,                                &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,*,ERR=9999,END=9998) (Realvar(i),i=1,50)
      IF (version >= 7.03) THEN
        DO i=1,MaxUser
          READ (unitno,6060,ERR=9999,END=9998) FilenameUser(i)(1:80)
 6060     FORMAT(a80)
        END DO
      ELSE
        READ (unitno,6060,ERR=9999,END=9998) FilenameUser(1)(1:80)
      END IF
      READ (unitno,*,ERR=9999,END=9998) init
! 
!  READ the rest of the restart file
! 
      DO i=1,init
        READ(unitno,*,ERR=9999,END=9998) ixxx(1),iyyy(1)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      area=0
      direct=0
      CDepth=0
      SoilDepth=0
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,*,ERR=9999,END=9998)slope,RanField                         &
&                          ,y,z(i,j)
        END DO
      END DO
      CLOSE(UNIT=unitno)
      RETURN

 9999 CALL InOutAlert('-- Error reading RS2 file')
      RETURN
!      STOP

 9998 CALL InOutAlert('-- Premature end of RST2 file')
      RETURN
!      STOP

  END SUBROUTINE rst2v7Z
! 
!  =====================================
!  input of RST3 generated by SIBERIA V6
!  =====================================
! 
  SUBROUTINE RST3v6(Version,unitno,noints,noreals,Intvar,realvar                                                        &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&         ,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*),intvar(*)                           &
&         ,Direct(gridX,gridY),noreals,noints,unitno,init
      REAL(8) :: Version,Slope(gridX,gridY),RanField(gridX,gridY)                &
&         ,y(gridX,gridY),z(gridX,gridY),realvar(*),Area(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj
      INTEGER(2) :: IParameter(8),i1,i2
! 
      NoInts=8
      noreals=33
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=1,6)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=7,8)
      DO i=1,NoInts
        IntVar(i)=IParameter(i)
      END DO
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                               &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=1,5)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=6,10)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=11,15)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=16,20)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=21,25)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=26,30)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=31,33)
      READ (unitno,ERR=9999,END=9998) init
      DO i=1,init
        READ(unitno,ERR=9999,END=9998) iXXX(I),iYYY(i)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,ERR=9999,END=9998)Slope(i,j),RanField(i,j),y(i,j)          &
&               ,z(i,j),i1,i2
          Area(i,j)=i1
          Direct(i,j)=i2
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST3 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST3 file')
      RETURN
!      STOP

  END SUBROUTINE RST3v6
! 
!  =====================================
!  input of RST3 generated by SIBERIA V7
!  =====================================
! 
  SUBROUTINE RST3v7(Version,unitno,noints,noreals                          &
&         ,Intvar,realvar                                                        &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&         ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*),intvar(*)                           &
&         ,Direct(gridX,gridY),noreals,noints,unitno                       &
&         ,init,MaxUser
      REAL(8) :: Version,Slope(gridX,gridY),RanField(gridX,gridY)                &
&         ,y(gridX,gridY),Area(gridX,gridY)                                      &
&         ,z(gridX,gridY),realvar(*),cDepth(gridX,gridY)                         &
&         ,SoilDepth(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(*) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj
      INTEGER(2) :: IParameter(20),i1,i2
! 
      NoInts=20
      noreals=50
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=1,6)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=7,12)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=13,18)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=19,20)
      DO i=1,NoInts
        IntVar(i)=IParameter(i)
      END DO
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=1,5)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=6,10)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=11,15)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=16,20)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=21,25)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=26,30)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=31,35)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=36,40)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=41,45)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=46,50)
      IF (Version >= 7.03) THEN
        DO i=1,MaxUser
          READ (unitno,ERR=9999,END=9998) FilenameUser(i)(1:80)
        END DO
      ELSE
        DO i=1,80
          FilenameUser(1)(i:i)=' '
        END DO
      END IF
      READ (unitno,ERR=9999,END=9998) init
      DO i=1,init
        READ(unitno,ERR=9999,END=9998) iXXX(I),iYYY(i)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,ERR=9999,END=9998)Slope(i,j),RanField(i,j),y(i,j)               &
&               ,z(i,j),i1,i2
          Area(i,j)=i1
          Direct(i,j)=i2
          cDepth(i,j)=0
          SoilDepth(i,j)=0
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST3 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST3 file')
      RETURN
!      STOP

  END SUBROUTINE RST3v7
! 
!  =====================================
!  input of RST3 generated by SIBERIA V8
!  =====================================
! 
  SUBROUTINE RST3v8(Version,unitno,noints,noreals                         &
&         ,Intvar,realvar                                                       &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY            &
&         ,FilenameUser,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*),intvar(*)                          &
&         ,Direct(gridX,gridY),noreals,noints,unitno                      &
&         ,init,MaxUser
      REAL(8) :: Version,Slope(gridX,gridY),RanField(gridX,gridY)               &
&         ,y(gridX,gridY),Area(gridX,gridY)                                     &
&         ,z(gridX,gridY),realvar(*),cDepth(gridX,gridY)                        &
&         ,SoilDepth(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB
      CHARACTER(*) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj
      INTEGER(2) :: IParameter(20),i1,i2
! 
      NoInts=20
      noreals=50
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=1,6)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=7,12)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=13,18)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=19,20)
      DO i=1,NoInts
        IntVar(i)=IParameter(i)
      END DO
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=1,5)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=6,10)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=11,15)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=16,20)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=21,25)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=26,30)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=31,35)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=36,40)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=41,45)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=46,50)
      IF (Version >= 7.03) THEN
        DO i=1,MaxUser
          READ (unitno,ERR=9999,END=9998) FilenameUser(i)(1:80)
        END DO
      ELSE
        DO i=1,80
          FilenameUser(1)(i:i)=' '
        END DO
      END IF
      READ (unitno,ERR=9999,END=9998) init
      DO  i=i1,init
        READ(unitno,ERR=9999,END=9998) iXXX(I),iYYY(i)
      END DO
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,ERR=9999,END=9998)Slope(i,j),RanField(i,j),y(i,j)               &
&               ,z(i,j),i1,i2,cDepth(i,j),SoilDepth(i,j)
          Area(i,j)=i1
          Direct(i,j)=i1
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST3 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST3 file')
      RETURN

  END SUBROUTINE RST3v8
! 
!  =====================================
!  input of RST3 generated by SIBERIA V6
!  =====================================
! 
  SUBROUTINE RST3v6Body(Version,unitno,noints,noreals                      &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&         ,FlipLR,FlipTB,SkipBC)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,init,iXXX(*),iYYY(*)                                &
&         ,Direct(gridX,gridY),noreals,noints,unitno
      REAL(8) :: Version,Slope(gridX,gridY),RanField(gridX,gridY)                &
&         ,y(gridX,gridY),z(gridX,gridY),Area(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB,SkipBC
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj,intvar(8)             &
&               ,itemp,itemp1,itemp2
      REAL(8) :: realvar(33)
      INTEGER(2) :: IParameter(8),i1,i2
! 
      NoInts=8
      noreals=33
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=1,6)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=7,8)
      DO i=1,NoInts
        IntVar(i)=IParameter(i)
      END DO
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                               &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=1,5)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=6,10)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=11,15)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=16,20)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=21,25)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=26,30)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=31,33)
      if (SkipBC) then
        READ (unitno,ERR=9999,END=9998) itemp
        DO i=1,itemp
           READ(unitno,ERR=9999,END=9998)itemp1,itemp2
        END DO
      else
        READ (unitno,ERR=9999,END=9998) init
        DO i=1,init
           READ(unitno,ERR=9999,END=9998) iXXX(i),iYYY(i)
        END DO
      end if
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,ERR=9999,END=9998)Slope(i,j),RanField(i,j),y(i,j)               &
&               ,z(i,j),i1,i2
          Area(i,j)=i1
          Direct(i,j)=i2
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST3 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST3 file')
      RETURN
!      STOP

  END SUBROUTINE RST3v6Body
! 
!  =====================================
!  input of RST3 generated by SIBERIA V7
!  =====================================
! 
  SUBROUTINE RST3v7Body(Version,unitno,noints,noreals                      &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY             &
&         ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*)                                     &
&         ,Direct(gridX,gridY),noreals,noints,unitno,init,MaxUser
      REAL(8) :: Version,Slope(gridX,gridY),RanField(gridX,gridY)                &
&         ,y(gridX,gridY),Area(gridX,gridY)                                      &
&         ,z(gridX,gridY),cDepth(gridX,gridY),SoilDepth(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB,SkipBC
      CHARACTER(1000) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj,intvar(20)            &
&               ,itemp,itemp1,itemp2
      REAL(8) :: realvar(50)
      INTEGER(2) :: IParameter(20),i1,i2
! 
      NoInts=20
      noreals=50
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=1,6)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=7,12)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=13,18)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=19,20)
      DO i=1,NoInts
        IntVar(i)=IParameter(i)
      END DO
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=1,5)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=6,10)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=11,15)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=16,20)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=21,25)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=26,30)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=31,35)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=36,40)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=41,45)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=46,50)
      IF (Version >= 7.03) THEN
        DO i=1,MaxUser
          READ (unitno,ERR=9999,END=9998) FilenameUser(i)(1:80)
        END DO
      ELSE
        DO i=1,80
          FilenameUser(1)(i:i)=' '
        END DO
      END IF
      if (SkipBC) then
        READ (unitno,ERR=9999,END=9998) itemp
        DO i=1,itemp
           READ(unitno,ERR=9999,END=9998)itemp1,itemp2
        END DO
      else
        READ (unitno,ERR=9999,END=9998) init
        DO i=1,init
           READ(unitno,ERR=9999,END=9998) iXXX(i),iYYY(i)
        END DO
      end if
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,ERR=9999,END=9998)Slope(i,j),RanField(i,j),y(i,j)               &
&               ,z(i,j),i1,i2
          Area(i,j)=i1
          Direct(i,j)=i2
          cDepth(i,j)=0
          SoilDepth(i,j)=0
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST3 file')
      RETURN
!      STOP

 9998 CALL InOutAlert(' -- Premature end of RST3 file')
      RETURN
!      STOP

  END SUBROUTINE RST3v7Body
! 
!  =====================================
!  input of RST3 generated by SIBERIA V8
!  =====================================
! 
  SUBROUTINE RST3v8Body(Version,unitno,noints,noreals                     &
&         ,init,iXXX,iYYY,Slope,RanField,y,z,Area,Direct,gridX,gridY            &
&         ,MaxUser,cDepth,SoilDepth,FlipLR,FlipTB,SkipBC)
      IMPLICIT NONE
! 
      INTEGER :: gridX,gridY,iXXX(*),iYYY(*)                                    &
&         ,Direct(gridX,gridY),noreals,noints,unitno                      &
&         ,init,MaxUser
      REAL(8) :: Version,Slope(gridX,gridY),RanField(gridX,gridY)               &
&         ,y(gridX,gridY),Area(gridX,gridY)                                     &
&         ,z(gridX,gridY),cDepth(gridX,gridY),SoilDepth(gridX,gridY)
      LOGICAL :: FlipLR,FlipTB,SkipBC
      CHARACTER(1000) :: FilenameUser(MaxUser)
!      CHARACTER(80) :: FilenameUser(MaxUser)
! 
      INTEGER :: i,j,bottomi,topi,incri,bottomj,topj,incrj,IntVar(20)         &
&               ,itemp,itemp1,itemp2
      REAL(8) :: RealVar(50)
      INTEGER(2) :: IParameter(20),i1,i2
! 
      NoInts=20
      noreals=50
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=1,6)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=7,12)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=13,18)
      READ (unitno,ERR=9999,END=9998) (IParameter(i),i=19,20)
      DO i=1,NoInts
        IntVar(i)=IParameter(i)
      END DO
      IF (Intvar(3) > gridX.or.Intvar(4) > gridY) THEN
        WRITE (*,6010) Intvar(3),Intvar(4),gridX,gridY
 6010   FORMAT(' -- Input RST2 file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=1,5)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=6,10)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=11,15)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=16,20)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=21,25)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=26,30)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=31,35)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=36,40)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=41,45)
      READ (unitno,ERR=9999,END=9998) (Realvar(i),i=46,50)
      IF (Version >= 7.03) THEN
        DO i=1,MaxUser
          READ (unitno,ERR=9999,END=9998) FilenameUser(i)(1:80)
        END DO
      ELSE
        DO i=1,80
          FilenameUser(1)(i:i)=' '
        END DO
      END IF
      if (SkipBC) then
        READ (unitno,ERR=9999,END=9998) itemp
        DO i=1,itemp
           READ(unitno,ERR=9999,END=9998)itemp1,itemp2
        END DO
      else
        READ (unitno,ERR=9999,END=9998) init
        DO i=1,init
           READ(unitno,ERR=9999,END=9998) iXXX(i),iYYY(i)
        END DO
      end if
      IF (FlipLR) THEN
        BottomI=Intvar(3)
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=Intvar(3)
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=Intvar(4)
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=Intvar(4)
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(unitno,ERR=9999,END=9998)Slope(i,j),RanField(i,j),y(i,j)               &
&               ,z(i,j),i1,i2,cDepth(i,j),SoilDepth(i,j)
          Area(i,j)=i1
          Direct(i,j)=i2
        END DO
      END DO
      RETURN

 9999 CALL InOutAlert(' -- Error reading RST3 file')
      RETURN

 9998 CALL InOutAlert(' -- Premature end of RST3 file')
      RETURN

  END SUBROUTINE RST3v8Body
! 
! ======================================================================
! Routine TO left-justify a string from one CHARACTER variable
! TO another.
! ======================================================================
! 
  SUBROUTINE MoveB(strng1, strng2, i1, i2)
! 
! Parameters:
! 
! Input:
!            strng1    input string
!        i1        start CHARACTER position in strng2
! Output:
!        strng2    destination CHARACTER variable
!        i2        last CHARACTER position used in strng2
! 
      CHARACTER :: strng1*(*), strng2*(*)
      INTEGER ::   i1, i2
      INTEGER ::   k, l
! 
      l = len(strng1)
      k = 1
      DO while(strng1(k:k)  ==  ' ' .and.  k  <  l)
        k = k + 1
      END DO
! 
      i2 = i1 + l - k
      IF(i2  >  len(strng2)) THEN
         i2 = len(strng2)
         l  = i2 - i1 + k
      END IF
      strng2(i1:i2) = strng1(k:l)
      RETURN
  END SUBROUTINE MoveB

! ======================================================================
!  Input header from a boundaries file 
! ======================================================================

  SUBROUTINE InputBNDHeader(BoundFile,kx,ky,LowX,HighX,LowY,HighY)
      IMPLICIT NONE
! 
!  BoundFile = name of file TO be READ
!  Domain    = LOGICAL array with the DATA of what nodes are in the Domain
!  Grid  = the declared size of Domain (same in x and y directions)
!  kx,ky     = x and y sizes of the Domain
!  init      = no of fixed Elevation points
!  iXXX,iYYY = the x and y coordinates of the fixed Elevation points
! 
      INTEGER :: kx, ky, LowX,HighX,LowY,HighY
      CHARACTER(*) :: BoundFile
!      CHARACTER(80) :: BoundFile
! 
      INTEGER :: FIOUnit,kxm,kym
      DATA FIOUnit / 10 /
! 
!      CALL PVMBNDFileReady
!$omp critical
      OPEN(UNIT=FIOUnit, FILE=BoundFile, STATUS='old', ERR=9999)
      write (*,*) ' -- Opening BND file = ',trim(BoundFile)
      READ (FIOUnit,1000,err=9090)
 1000 FORMAT(a1)
      READ (FIOUnit,*,err=9090) kxm, kym
      IF (kx < kxm.or.ky < kym) THEN
        WRITE (*,*) ' --- The Grid is too small for input boundaries'
        WRITE (*,*) '     KX, KY increased'
      END IF
      kx=kxm
      ky=kym
      LowX=kx
      LowY=ky
      HighX=1
      HighY=1
      go to 2000
!
 9999 CALL InOutAlert(' -- Cannot OPEN boundary file '//BoundFile)
      CALL ElegantExit
      go to 2000
! 
 9090 CALL InOutAlert(' -- Premature end of BND file in header')
      CALL ElegantExit
      go to 2000
! 
 2000 continue
      CLOSE(UNIT=FIOUnit, STATUS='keep')
!$omp end critical
  END SUBROUTINE InputBndHeader

! ======================================================================
!  Input the boundaries files for irregular Grid
! ======================================================================

  SUBROUTINE InputBoundaries(BoundFile,Domain,GridX,GridY,kx,ky                   &
&                            ,init,iXXX,iYYY,Regions,NoRegions                    &
&                            ,MaxRegions,Region,RegionMap                         &
&                            ,LowX,HighX,LowY,HighY)
      IMPLICIT NONE
! 
!  BoundFile = name of file TO be READ
!  Domain    = LOGICAL array with the DATA of what nodes are in the Domain
!  Grid  = the declared size of Domain (same in x and y directions)
!  kx,ky     = x and y sizes of the Domain
!  init      = no of fixed Elevation points
!  iXXX,iYYY = the x and y coordinates of the fixed Elevation points
! 
      INTEGER,PARAMETER :: NoOutlets=15000
! 
      INTEGER :: GridX,GridY, kx, ky, init,iXXX(*),iYYY(*),Regions(*)             &
&         ,NoRegions,MaxRegions,Region(gridX,gridY)                               &
&         ,RegionMap(MaxRegions),LowX,HighX,LowY,HighY
      LOGICAL :: Domain(GridX,GridY)
      CHARACTER(*) :: BoundFile
!      CHARACTER(80) :: BoundFile
! 
      INTEGER :: FIOUnit, Noout,Area,ix,iy,i,j,kxm,kym,num
      CHARACTER(1),dimension(:),allocatable :: stuff
      CHARACTER(255) :: string255
      DATA FIOUnit / 10 /
! 
!      CALL PVMBNDFileReady
!$omp critical
      OPEN(UNIT=FIOUnit, FILE=BoundFile, STATUS='old', ERR=9999)
      write (*,*) ' -- Opening BND file = ',trim(BoundFile)
      READ (FIOUnit,1000,err=9091)
 1000 FORMAT(a1)
      READ (FIOUnit,*,err=9091) kxm, kym
      IF (kx < kxm.or.ky < kym) THEN
        WRITE (*,*) ' --- The Grid is too small for input boundaries'
        WRITE (*,*) '     KX, KY increased'
      END IF
      IF (kxm > gridX.or.kym > gridY) THEN
        WRITE (*,*) kxm,kym,gridX,gridY
 6010   FORMAT(' -- Input BND file too big ',2i6,                                  &
&              ' for SIBERIA < ',2i6)
        STOP
      END IF
      kx=kxm
      ky=kym
      ix=kx
      iy=ky
      allocate(stuff(1:kxm))
      NoOut=0
      Area=0
      DO i=1,iy
        READ(FIOUnit,1010,END=9090) (stuff(j),j=1,ix)
 1010   FORMAT(10001a1)
        DO j=1,ix
          Region(j,i)=-1
          IF (stuff(j)(1:1) == ' ') THEN
            WRITE (*,6011) ' -- ERROR -- Invalid space in boundary '               &
&             //'file at position (',i,',',j,')'
 6011       FORMAT(a,2(i6,a))
            STOP
          ELSE IF (stuff(j)(1:1) /= '.') THEN
            Domain(j,i)=.true.
            Area=Area+1
! 
!  Fixed Elevation outlet BC points.
! 
            IF (stuff(j)(1:1) == '^') THEN
              NoOut=NoOut+1
              IF (NoOut > NoOutlets) THEN
                WRITE (*,*) ' No of Outlet nodes in',                             &
&                            ' boundary file too large.'
                WRITE (*,*) '   Max Allowed = ',NoOutlets
                STOP
              END IF
              iXXX(NoOut)=j
              iYYY(NoOut)=i
            END IF
! 
!  Fixed Elevation Regions BC Regions (eg. reservoirs identified as 1-0
!  in the .bnd file)
! 
            num=ord(stuff(j)(1:1))
            IF (num >= 1.and.num <= 10) THEN
              Region(j,i)=num
              CALL AddRegion(num,Regions,NoRegions,MaxRegions,RegionMap)
            END IF
          ELSE
! 
!  Outside Domain
! 
            Domain(j,i)=.false.
          END IF
        END DO
      END DO
      IF (NoOut == 0) THEN
        WRITE (*,*) 'No Outlet node has been specified in'                      &
&                  ,' the boundary file'
        STOP
      ELSE
        init=NoOut
      END IF
      CLOSE(UNIT=FIOUnit, STATUS='keep')
!      CALL PVMBNDFileClosed
      LowX=kx
      LowY=ky
      HighX=1
      HighY=1
      DO j=1,ky
        DO i=1,kx
          IF (Domain(i,j)) THEN
            LowX=min(LowX,i)
            HighX=max(HighX,i)
            LowY=min(LowY,j)
            HighY=max(HighY,j)
          END IF
        END DO
      END DO
      DO i=1,ky+1
        Domain(kx+1,i)=.false.
      END DO
      DO i=1,kx+1
        Domain(i,ky+1)=.false.
      END DO
      WRITE (*,*) ' -- Irregular boundaries have been input'
      WRITE (*,*) ' -- Total catchment Area = ',Area
      WRITE (*,*) ' -- No of specified catchment outlets',init
      WRITE (*,*) ' -- DATA range X = ',LowX,HighX
      WRITE (*,*) '               Y = ',LowY,HighY
      go to 2000
!
 9999 CALL InOutAlert(' -- Cannot OPEN boundary file '//BoundFile)
      CALL ElegantExit
      go to 2000
! 
 9090 WRITE (*,*) ' -- Premature end of BND file at Line = ',i
      CALL InOutAlert(string255)
      CALL ElegantExit
      go to 2000
! 
 9091 CALL InOutAlert(' -- Premature end of BND file in header')
      CALL ElegantExit
      go to 2000
! 
 2000 continue
!$omp end critical
  END SUBROUTINE InputBoundaries
! 
! ======================================================================
!  Input the boundaries files for irregular Grid
! ======================================================================
!      
  SUBROUTINE InputRegionFile                                                    &
&         (BoundFile,Domain,GridX,GridY,kx,ky,error)
      IMPLICIT NONE
! 
!  BoundFile = name of file TO be READ
!  Domain    = LOGICAL array with the DATA of what nodes are in the Domain
!  GridX,GridY  = the declared size of Domain
!  kx,ky     = x and y sizes of the Domain
!  init      = no of fixed Elevation points
!  iXXX,iYYY = the x and y coordinates of the fixed Elevation points
! 
      INTEGER,PARAMETER :: NoValidChar=64
      INTEGER :: GridX,GridY, kx, ky
      INTEGER :: Domain(GridX,GridY)
      CHARACTER(*) :: BoundFile
!      CHARACTER(80) :: BoundFile
      LOGICAL :: error
! 
      INTEGER :: FIOUnit,ix,iy,i,j,kxm,kym,k
      CHARACTER(1),dimension(:),allocatable :: stuff
      CHARACTER(NoValidChar) :: letters
      CHARACTER(255) :: string
      DATA letters(1:36) / '1234567890abcdefghijklmnopqrstuvwxyz' /               &
&          letters(37:NoValidChar) / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ*^' /
      DATA FIOUnit / 10 /
! 
!$omp critical
      error=.false.
!      CALL PVMBNDFileReady
      OPEN(UNIT=FIOUnit, FILE=BoundFile, STATUS='old', ERR=9999)
        write (*,*) ' -- Opening RGN file = ',trim(BoundFile)
      READ (FIOUnit,1000)
 1000 FORMAT(a1)
      DO i=2,4
        READ (FIOUnit,*)
      END DO
      READ (FIOUnit,*) kxm, kym
      IF (kxm > gridX .or. kym > gridY) THEN
        WRITE (*,*) kxm,kym,gridX,gridY,BoundFile
 6010   FORMAT(' -- Input Region file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6,a)
        CALL ElegantExit
      ELSE IF (kxm /= kx .or. kym /= ky) THEN
        WRITE (*,*) ' --- ERROR --- Size of region file (',kxm,',',kym,')'
        WRITE (*,*) '               does not match rst2 file (',kx,',',ky,')'
        WRITE (*,*) '               for region file =',boundfile
        CALL ElegantExit
      END IF
      ix=kxm
      iy=kym
      allocate(stuff(1:kxm))
      DO i=1,iy
        READ(FIOUnit,1010,END=9990) (stuff(j),j=1,ix)
 1010   FORMAT(10001a1)
        DO j=1,ix
          IF (stuff(j)(1:1) == ' ') THEN
            WRITE (*,6011) ' -- ERROR -- Invalid space in region '               &
&             //'file at position (',i,',',j,')'
 6011       FORMAT(a,2(i6,a))
            STOP
          ELSE IF (stuff(j)(1:1) /= '.') THEN
            DO k=1,NoValidChar
              Domain(j,i)=0
              IF (stuff(j)(1:1) == letters(k:k))THEN
                Domain(j,i)=k
                GO TO 1001
              END IF
            END DO
!  handle the generic other CHARACTER
            IF (domain(j,i) == 0) domain(j,i)=NoValidChar+1
 1001       CONTINUE
          ELSE
! 
!  Outside Domain
! 
            Domain(j,i)=0
          END IF
        END DO
      END DO
 1100 CLOSE(UNIT=FIOUnit, STATUS='keep')
!      CALL PVMBNDFileClosed
      DO i=1,kym+1
        Domain(kxm+1,i)=0
      END DO
      DO i=1,kxm+1
        Domain(i,kym+1)=0
      END DO
      go to 2000
! 
 9999 string='Error opening region file '//BoundFile
      CALL InOutAlert(string)
      error=.true.
      CALL ElegantExit
      go to 2000
! 
 9990 WRITE (string,*) ' -- Premature end of RGN file at Line = ',i
      CALL InOutAlert(string)
      CALL ElegantExit
      go to 2000
! 
 2000 continue
!$omp end critical
  END SUBROUTINE InputRegionFile
! 
! ======================================================================
!  Input the boundaries files for irregular Grid
! ======================================================================
!      
  SUBROUTINE InputRegionFileLXY(BoundFile,Domain,error)
      USE SiberiaTypes
      IMPLICIT NONE
! 
!  BoundFile = name of file TO be READ
!  Domain    = LOGICAL array with the DATA of what nodes are in the Domain
!  GridX,GridY  = the declared size of Domain
!  kx,ky     = x and y sizes of the Domain
!  init      = no of fixed Elevation points
!  iXXX,iYYY = the x and y coordinates of the fixed Elevation points
! 
      INTEGER,PARAMETER :: NoValidChar=64
      type(ArrayLXY) :: Domain
      CHARACTER(*) :: BoundFile
!      CHARACTER(80) :: BoundFile
      LOGICAL :: error
! 
      INTEGER :: FIOUnit,ix,iy,i,j,kxm,kym,LowX,LowY
      CHARACTER(1),allocatable,dimension(:) :: stuff
      CHARACTER(255) :: string
      DATA FIOUnit / 10 /
! 
!$omp critical
      error=.false.
!      CALL PVMBNDFileReady
      OPEN(UNIT=FIOUnit, FILE=BoundFile, STATUS='old', ERR=9999)
        write (*,*) ' -- Opening RGN file = ',trim(BoundFile)
      READ (FIOUnit,1000)
 1000 FORMAT(a)
      DO i=2,4
        READ (FIOUnit,*)
      END DO
      READ (FIOUnit,*) kxm, kym
!  check array sizes
      IF (kxm > ubound(domain%data,1) .or. kym > ubound(domain%data,2)) THEN
        WRITE (*,*) kxm,kym,ubound(domain%data,1),ubound(domain%data,2),BoundFile
 6010   FORMAT(' -- Input Region file too big ',2i6,                             &
&              ' for SIBERIA < ',2i6,a)
        CALL ElegantExit
      end if
      allocate(stuff(1:kxm))
      ix=kxm
      iy=kym
      LowX=lbound(domain%data,1)
      LowY=lbound(domain%data,2)
      DO i=1,iy
        READ(FIOUnit,1010,END=9990) (stuff(j),j=1,ix)
 1010   FORMAT(10001a1)
        DO j=1,ix
          IF (stuff(j)(1:1) == ' ') THEN
            WRITE (*,6011) ' -- ERROR -- Invalid space in region '               &
&             //'file at position (',i,',',j,')'
 6011       FORMAT(a,2(i6,a))
            STOP
          ELSE
            if (i >= LowX .and. j >= LowY) then
              IF (stuff(j)(1:1) /= '.') THEN
                Domain%data(j,i)=.true.
              ELSE
! 
!  Outside Domain
! 
                Domain%data(j,i)=.false.
              END IF
            END IF
          END IF
        END DO
      END DO
 1100 CLOSE(UNIT=FIOUnit, STATUS='keep')
!      CALL PVMBNDFileClosed
      if (ubound(domain%data,1) > kxm) then
        DO i=1,kym+1
          Domain%data(kxm+1,i)=.false.
        END DO
        DO i=1,kxm+1
          Domain%data(i,kym+1)=.false.
        END DO
      end if
      go to 2000
! 
 9999 string='Error opening region file '//BoundFile
      CALL InOutAlert(string)
      error=.true.
      CALL ElegantExit
      go to 2000
! 
 9990 WRITE (string,*) ' -- Premature end of RGN file at Line = ',i
      CALL InOutAlert(string)
      CALL ElegantExit
      go to 2000
! 
 2000 continue
!$omp end critical
  END SUBROUTINE InputRegionFileLXY
! 
!  ===========================================================================
!   Finding the ordinal number of an input CHARACTER. Returns a -ve value IF
!   the input CHARACTER is not recognised.
!  ===========================================================================
! 
  INTEGER FUNCTION ord(tr)
      IMPLICIT NONE
      CHARACTER(1) :: tr
! 
      INTEGER,PARAMETER :: no=36
      INTEGER :: i
      CHARACTER(no) :: ta
      DATA ta / '1234567890abcdefghijklmnopqrstuvwxyz' /
! 
      DO i=1,no
        IF (ta(i:i) == tr) GO TO 1100
      END DO
      ord=-1
      RETURN
 1100 ord=i
      RETURN
  END FUNCTION ord
! 
!  ===========================================================================
!  Add a Region TO the array keeping track of what Regions have been specified
!  ===========================================================================
! 
  SUBROUTINE AddRegion(num,Regions,noregions,Maxregions,RegionMap)
      IMPLICIT NONE
! 
      INTEGER :: Maxregions,num,Regions(MaxRegions),noregions                      &
&            ,RegionMap(MaxRegions)
! 
      INTEGER :: i
! 
      DO i=1,noregions
! 
!  This condition should never be true
! 
        IF (Regions(i) <= 0) GO TO 1100
! 
!  IF this Region number has been previously identified in the .bnd file
! 
        IF (Regions(i) == num) RETURN
      END DO
! 
!  Too many Regions
! 
      IF(noregions == Maxregions) THEN
        WRITE (*,*) ' -- Too many Regions in InputBoundaries > '                      &
&                   ,Maxregions
        STOP
      ELSE
        noregions=noregions+1
        Regions(noregions)=num
        RegionMap(num)=NoRegions
      END IF
 1100 WRITE (*,*) ' -- Internal error in  AddRegion'
      STOP
  END SUBROUTINE AddRegion
! 
!  ===========================================================================
!  SUBROUTINE TO WRITE a simple boundary file with no Regions, 
!     only boundary and outlets
!  ===========================================================================
! 
  SUBROUTINE WriteBNDFile(Domain,gridX,gridY,FileName,kx,ky,header               &
&          ,init,iXXX,iYYY)
      IMPLICIT NONE
! 
      INTEGER,PARAMETER :: outunit=10,linewidth=200
! 
      INTEGER :: gridX,gridY,kx,ky,init,iXXX(init),iYYY(init)
      LOGICAL :: Domain(gridX,gridY)
      CHARACTER(*) :: header
      CHARACTER(*) :: FileName
!      CHARACTER(80) :: FileName
! 
      INTEGER :: i,j
      CHARACTER(LineWidth) :: line
! 
!$omp critical
      IF (kx >= LineWidth) THEN
        WRITE (*,*) ' -- Error in writing boundary file'
        WRITE (*,*) '    X DIMENSION of the boundary is too large > '               &
&          ,LineWidth
        STOP
      END IF
      OPEN(UNIT=outunit,FILE=FileName,ERR=9999)
!      OPEN(UNIT=outunit,FILE=FileName,STATUS='new',ERR=9999)
      WRITE (outunit,*,ERR=9998) header
      WRITE (outunit,*,ERR=9998) kx,ky
      DO j=1,ky+1
        DO i=1,kx+1
          IF(Domain(i,j)) THEN
            line(i:i)='*'
          ELSE
            line(i:i)='.'
          END IF
        END DO
        DO i=1,init
          IF (iYYY(i) == j) THEN
            line(iXXX(i):iXXX(i))='^'
          END IF
        END DO
        WRITE (outunit,6000,ERR=9998) line(1:kx+1)
 6000   FORMAT(a)
      END DO
      CLOSE(UNIT=outunit,STATUS='keep')
      WRITE (*,*) ' -- BND boundary file ',trim(FileName),' output'
      go to 1000
! 
 9999 CALL InOutAlert(' -- Error in opening new boundary file')
      go to 1000
! 
 9998 CALL InOutAlert(' -- Error in writing TO new boundary file')
      go to 1000
! 
 1000 continue
!$omp end critical
  END SUBROUTINE WriteBNDFile
! 
!  ===========================================================================
!  SUBROUTINE TO WRITE a simple boundary file with no regions,
!     only boundary and outlets
!  ===========================================================================
! 
  SUBROUTINE WriteBNDFileInt(Domain,directions                                    &
&          ,filename,kx,ky,header,signInt)
      IMPLICIT NONE
! 
      INTEGER,PARAMETER :: outunit=10
! 
      INTEGER :: kx,ky,signInt
      INTEGER :: Domain(kx,ky),Directions(kx,ky)
      CHARACTER(*) :: header
      CHARACTER(*) :: filename
!      CHARACTER(80) :: filename
! 
      INTEGER :: i,j
      CHARACTER(kx) :: line
! 
!$omp critical
      OPEN(UNIT=outunit,FILE=filename,ERR=9999)
!      OPEN(UNIT=outunit,FILE=filename,STATUS='new',ERR=9999)
      WRITE (outunit,*,ERR=9998) header
      WRITE (outunit,*,ERR=9998) kx,ky
      DO j=1,ky
        DO i=1,kx
          IF(signInt*domain(i,j) > 0) THEN
            IF (abs(directions(i,j)) == 5) THEN
              line(i:i)='^'
            ELSE
              line(i:i)='*'
            END IF
          ELSE
            line(i:i)='.'
          END IF
        END DO
        WRITE (outunit,6000,ERR=9998) line(1:kx)//'.'
 6000   FORMAT(a)
      END DO
      CLOSE(UNIT=outunit,STATUS='keep')
      WRITE (*,*) ' -- BND boundary file ',trim(FileName),' output'
      go to 1000
! 
 9999 CALL InOutAlert(' -- Error in opening new boundary file')
      go to 1000
! 
 9998 CALL InOutAlert(' -- Error in writing TO new boundary file')
      go to 1000
! 
 1000 continue
!$omp end critical
  END SUBROUTINE WriteBNDFileInt
! 
!  ===========================================================================
!  SUBROUTINE TO WRITE a simple boundary file with no regions,
!     only boundary and outlets
!  ===========================================================================
! 
  SUBROUTINE WriteRegionFileInt(Domain,directions                             &
&          ,filename,kx,ky,header,signInt)
      IMPLICIT NONE
! 
      INTEGER, PARAMETER :: outunit=10
! 
      INTEGER :: kx,ky,signInt
      INTEGER :: Domain(kx,ky),Directions(kx,ky)
      CHARACTER(*) :: header
      CHARACTER(*) :: filename
!      CHARACTER(80) :: filename
! 
      INTEGER :: i,j
      CHARACTER(kx) :: line
      CHARACTER(62) :: letters
      DATA letters(1:36)  / '1234567890abcdefghijklmnopqrstuvwxyz'/
      DATA letters(37:62) / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
! 
!$omp critical
      OPEN(UNIT=outunit,FILE=filename,STATUS='new',ERR=9999)
      WRITE (outunit,*,ERR=9998) header
      DO i=1,3
        WRITE (outunit,*,ERR=9998)
      END DO
      WRITE (outunit,*,ERR=9998) kx,ky
      DO j=1,ky
        DO i=1,kx
          IF(signInt*domain(i,j) > 0) THEN 
           IF (abs(Domain(i,j)) < 62) THEN
              line(i:i)=letters(abs(Domain(i,j)):abs(Domain(i,j)))
            ELSE
              line(i:i)='?'
            END IF
          ELSE
            line(i:i)='.'
          END IF
        END DO
        WRITE (outunit,6000,ERR=9998) line(1:kx)//'.'
 6000   FORMAT(a)
      END DO
      CLOSE(UNIT=outunit,STATUS='keep')
      WRITE (*,*) ' -- RGN Region file ',trim(FileName),' output'
      go to 1000
! 
 9999 CALL InOutAlert(' -- Error in opening new boundary file')
      go to 1000
! 
 9998 CALL InOutAlert(' -- Error in writing TO new boundary file')
      go to 1000
! 
 1000 continue
!$omp end critical
  END SUBROUTINE WriteRegionFileInt
! 
!  ===========================================================================
!   Input of an RSU file that is already OPEN
!  ===========================================================================
! 
  SUBROUTINE ReadInRSU(DATA,gridx,gridy,kx,ky,NoDataSets                             &
&         ,FlipLR,FlipTB)
      IMPLICIT NONE
! 
      INTEGER :: gridx,gridy,kx,ky,NoDataSets
      REAL(8) :: DATA(gridx,gridy,NoDataSets)
      LOGICAL :: FlipLR,FlipTB
! 
      INTEGER i,j,k,BottomI,TopI,IncrI,BottomJ,TopJ,IncrJ
      REAL(8),allocatable :: rjunk(:)
! 
!$omp critical
      allocate (rjunk(NoDataSets))
      IF (FlipLR) THEN
        BottomI=kx
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=kx
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=ky
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=ky
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(20,*) (rjunk(k),k=1,NoDataSets)
          DO k=1,NoDataSets
            DATA(i,j,k)=rjunk(k)
          END DO
        END DO
      END DO
      deallocate (rjunk)
!$omp end critical
      RETURN
  END SUBROUTINE ReadInRSU
! 
!  ===========================================================================
!   Open and input a selected column of an RSU file 
!  ===========================================================================
! 
  SUBROUTINE ReadInRSUi(filename,DATA,column,gridx,gridy,FlipLR,FlipTB)
    IMPLICIT NONE
      integer,parameter :: unitno=40
! 
      INTEGER :: gridx,gridy,column
      REAL(8) :: DATA(gridx,gridy)
      LOGICAL :: FlipLR,FlipTB
      character(*) :: filename
! 
      INTEGER i,j,k,BottomI,TopI,IncrI,BottomJ,TopJ,IncrJ,kx,ky,NoDataSets
      REAL(8),allocatable :: rjunk(:)
! 
!$omp critical
      open(unit=unitno,file=filename,status='old')
      do i=1,4
        read(unitno,*)
      end do
      read (unitno,*) kx,ky,NoDataSets
      if (Column > NoDataSets) then
        write (*,*) ' --- ERROR --- Trying to input an invalid RSU column in ReadInRSUi'
        go to 1000
      end if
      read(unitno,*)
      allocate(rjunk(NoDataSets))
      IF (FlipLR) THEN
        BottomI=kx
        TopI=1
        IncrI=-1
      ELSE
        BottomI=1
        TopI=kx
        IncrI=1
      END IF
      IF (FlipTB) THEN
        BottomJ=ky
        TopJ=1
        IncrJ=-1
      ELSE
        BottomJ=1
        TopJ=ky
        IncrJ=1
      END IF
      DO j=BottomJ,TopJ,IncrJ
        DO i=BottomI,TopI,IncrI
          READ(20,*) (rjunk(k),k=1,NoDataSets)
          DATA(i,j)=rjunk(column)
        END DO
      END DO
      deallocate(rjunk)
      close(unit=unitno,status='keep')
 1000 continue
!$omp end critical
      RETURN
  END SUBROUTINE ReadInRSUi
! 
!  ===========================================================================
! 
!  ===========================================================================
! 
  LOGICAL FUNCTION ReadHeaderRSU(RSUfilename,NoDataSets)
      IMPLICIT NONE
! 
      CHARACTER(*) :: RSUfilename
      INTEGER :: NoDataSets,ijunk1,ijunk2,i
      LOGICAL :: error
! 
      INQUIRE(file=RSUfilename,exist=error)
      IF (.not.error) THEN
        ReadHeaderRSU=.false.
        RETURN
      END IF
!$omp critical
      OPEN(UNIT=20,FILE=RSUfilename,STATUS='old',ERR=9999)
      write (*,*) ' -- Opening RSU file = ',RSUfilename
      DO i=1,4
        READ(20,*,ERR=9999)
      END DO
      READ(20,*,ERR=9999) ijunk1,ijunk2,NoDataSets
      CLOSE(UNIT=20,STATUS='keep',ERR=9999)
      ReadHeaderRSU=.true.
      go to 1000
! 
 9999 ReadHeaderRSU=.false.
      go to 1000
! 
 1000 continue
!$omp end critical
      RETURN
  END FUNCTION ReadHeaderRSU
! 
!  ===========================================================================
! 
!  ===========================================================================
! 
  SUBROUTINE ElegantExit
      WRITE (*,*) 
      WRITE (*,*) ' Press any key TO CONTINUE'
      READ(*,*)
      STOP
      RETURN
  END SUBROUTINE ElegantExit

END MODULE InputOutput
