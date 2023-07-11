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
MODULE MonteCarlo

  REAL,PARAMETER :: MonteCarlo_Version=8.24

CONTAINS

! 
!  =======================================================================================
!             Input the Monte-Carlo DATA for this simulation
!  =======================================================================================
! 
SUBROUTINE MC_Init(MCNoSims,MCRandomSeed,MCRSTFileListExist               &
&       ,MCb1FileListExist,MCb3FileListExist,MCUpliftFileListExist        &
&       ,MCOthersFileListExist,MCParaCharac)
  USE ModelParameters
  USE Support
  USE SiberiaConstants
  USE SiberiaTypes
! 
  IMPLICIT NONE
  INTEGER :: MCNoSims,MCRandomSeed
  LOGICAL :: MCRSTFileListExist                                           &
&       ,MCb1FileListExist,MCb3FileListExist,MCUpliftFileListExist        &
&       ,MCOthersFileListExist
  TYPE(MCParaCharacArray) :: MCParaCharac
!
  INTEGER :: i,start,lineNo,ibuff 
  LOGICAL :: OK
  CHARACTER(255) :: line,buffer
  CHARACTER(20) :: commands(10)
  CHARACTER(80) :: atom
  INTEGER :: SELECT
  SAVE commands
  DATA commands(1)    / 'END'/                   &
&      commands(2)   / 'NO_SIMULATIONS'/       &
&      commands(3)   / 'INITIAL_SEED'/         &
&      commands(4)    / 'PARAMETER'/             &
&      commands(5)   / 'PARAMETER_FILE_LIST'/  &
&      commands(6)   / 'RST_FILE_LIST'/        &
&      commands(7)   / 'B1_FILE_LIST'/         &
&      commands(8)   / 'B3_FILE_LIST'/         &
&      commands(9)   / 'UPLIFT_FILE_LIST'/     &
&      commands(10) / 'OTHERS_FILE_LIST'/
! 
! Some Initialisation
! 
    MCNoSims=1
    MCRandomSeed=1
    MCRSTFileListExist=.false.
    MCb1FileListExist=.false.
    MCb3FileListExist=.false.
    MCUpliftFileListExist=.false.
    MCOthersFileListExist=.false.
!  set each PARAMETER so that sampling NOT selected
    DO i=1,70
      MCParaCharac%PARAMETER(i)%MCDistribution=-1
    END DO
!  set file sampling modes to negative to indicate NOT selected
    MCParaCharac%RST2FileList%SampleMode=-1
! 
   CALL Message_Output(Message_Info,'-- Reading Monte-Carlo Input File')
! 
    INQUIRE(file=FilenameUser(5),exist=OK)
    IF (.not.OK) GO TO 9999
    OPEN(UNIT=GlobalTempUnit,FILE=FilenameUser(5),ERR=9998)
    LineNo=1
 8002 Line=' '
    READ(GlobalTempUnit,6000,END=8000,ERR=9997) line
    lineno=lineno+1
 6000 FORMAT(a)
    DO i=1,80
      start=i
      IF (line(i:i) /= ' ') GO TO 8003
    END DO
    GO TO 8002
 8003 IF (line(start:start) == '#' .or. line(start:start) == '!') GO TO 8002
! 
    ibuff=start
    buffer=' '
    buffer(1:80)=line(1:80)
    CALL Str_UpperCase(buffer)
    CALL Str_nxtat(buffer,ibuff,atom,80)
    CALL Str_mtchcomm1(atom,commands,SELECT)
    SELECT CASE (SELECT)
    CASE DEFAULT
      CALL Message_Output(Message_WarnContinue                         &
&             ,'Ignoring Unknown Monte-Carlo Command at Line='         &
&             ,LineNo,atom)
      GO TO 8002
!  END
    CASE(1)
      GO TO 8000
!  Number of simulations
    CASE(2)
      READ(buffer(ibuff:),*,ERR=9996) MCNoSims
      IF (MCNoSims <= 0) THEN
        CALL Message_Output(Message_ErrorStop                          &
&              ,'Monte-Carlo No of Simulations < 1',MCNoSims)
      END IF
!  Initial Random seed value
    CASE(3)
      READ(buffer(ibuff:),*,ERR=9996) MCRandomSeed
!  Parameters statistics
    CASE(4)
      CALL MC_Parameters(buffer,ibuff,atom,MCParaCharac)
!  Parameters file list
    CASE(5)
      CALL Message_Output(Message_WarnContinue                         &
&            ,'PARAMETER File List Not Yet Implemented')
!  RST2 file list
    CASE(6)
      CALL MC_RST2FileList(buffer,ibuff,atom,MCParaCharac)
!  erodibility (b1) file list
    CASE(7)
      CALL Message_Output(Message_WarnContinue                         &
&            ,'Erodibility File List Not Yet Implemented')
!  runoff (b3) file list
    CASE(8)
      CALL Message_Output(Message_WarnContinue                         &
&             ,'Runoff File List Not Yet Implemented')
!  uplift file list
    CASE(9)
      CALL Message_Output(Message_WarnContinue                         &
&             ,'Uplift File List Not Yet Implemented')
!  others model file list
    CASE(10)
      CALL Message_Output(Message_WarnContinue                         &
&              ,'Other Model File List Not Yet Implemented')
    END SELECT
    GO TO 8002
! 
 8000 CONTINUE
    CLOSE(UNIT=GlobalTempUnit,STATUS='keep')
    CALL Message_Output(Message_Info                                   &
&              ,'Finished Reading Monte-Carlo Input File')
  RETURN
! 
! ---------------------- Error Messages
! 
9999 CALL Message_Output(Message_ErrorStop                         &
&           ,'Monte-Carlo Input File does not exist '              &
&             //trim(FilenameUser(5)))
9998 CALL Message_Output(Message_ErrorStop                         &
&           ,'Monte-Carlo Input File exists but cannot be opened ' &
&             //trim(FilenameUser(5)))
9997 CALL Message_Output(Message_WarnContinue                      &
&         ,'Error reading Monte-Carlo Input File,'                 &
&        //' Line Number=',LineNo,' '//trim(line))
9996 CALL Message_Output(Message_WarnContinue                      &
&         ,'Error reading Number of Monte-Carlo '                  &
&        //'Simulations, Line Number=',LineNo,' '//trim(line))
END SUBROUTINE MC_Init
! 
!  =======================================================================================
!             Input the Monte Carlo Parameter Statistics
!  =======================================================================================
! 
SUBROUTINE MC_Parameters(buffer,ibuff,atom,MCParaCharac)
  USE ModelParameters
  USE Support
  USE SiberiaConstants
  USE SiberiaTypes
! 
  IMPLICIT NONE
  INTEGER ibuff
  CHARACTER(80) :: atom
  CHARACTER(255) :: buffer
  TYPE(MCParaCharacArray) :: MCParaCharac
! 
  INTEGER :: SELECT,parameterNo
  CHARACTER(9) :: Para_Names(38)
  INTEGER :: select1,Para_Map(40)
  DATA Para_Names(1)  / 'TIMEUP'/        &
&      Para_Names(2)  / 'DZ'/            &
&      Para_Names(3)  / 'DZN'/           &
&      Para_Names(4)  / 'DZTHOLD'/       &
&      Para_Names(5)  / 'QSTHOLD'/       &
&      Para_Names(6)  / 'FRANMIN'/       &
&      Para_Names(7)  / '1/AT'/          &
&      Para_Names(8)  / 'FRANCV'/        &
&      Para_Names(9)  / 'B3SDS'/         &
&      Para_Names(10) / 'B3SDL'/         &
&      Para_Names(11) / 'UPAMP'/         &
&      Para_Names(12) / 'UPPERIOD'/      &
&      Para_Names(13) / 'UPPHASE'/       &
&      Para_Names(14) / 'FRANZ'/         &
&      Para_Names(15) / 'B3'/            &
&      Para_Names(16) / 'M3'/            &
&      Para_Names(17) / 'B1'/            &
&      Para_Names(18) / 'M1'/            &
&      Para_Names(19) / 'N1'/            &
&      Para_Names(20) / 'BULKDEN'/       &
&      Para_Names(21) / 'TIMESTEP'/      &
&      Para_Names(22) / 'B5'/            &
&      Para_Names(23) / 'M5'/            &
&      Para_Names(24) / 'N5'/            &
&      Para_Names(25) / 'SINIT'/         &
&      Para_Names(26) / 'NOTCH'/         &
&      Para_Names(27) / 'COVER'/         &
&      Para_Names(28) / 'S0MAX'/         &
&      Para_Names(29) / 'DTIME'/         &
&      Para_Names(30) / 'OTIME'/         &
&      Para_Names(31) / 'B6'/            &
&      Para_Names(32) / 'M6'/            &
&      Para_Names(33) / 'B1(2)'/         &
&      Para_Names(34) / 'M1(2)'/         &
&      Para_Names(35) / 'SDRATE'/        &
&      Para_Names(36) / 'SDEXP1'/        &
&      Para_Names(37) / 'SDEXP2'/        &
&      Para_Names(38) / 'SMTHRESH'/
  CHARACTER(9) :: Distribution(5)
  INTEGER :: select2
  DATA Distribution(1) / 'UNIFORM'/
! &      Distribution(2) / 'NORMAL'/      &
! &      Distribution(3) / 'LOGNORMAL'/
!  mapping of the para_name TO the actual PARAMETER number
  DATA Para_Map /  6, 21, 22, 23, 24, 26, 27, 29, 30, 31            &
&               , 32, 33, 34, 35, 38, 37, 39, 40, 41, 42            &
&               , 43, 44, 47, 45, 46, 49, 50, 51, 52, 53            &
&               , 57, 58, 59, 60, 61, 62, 63, 64, 0 , 0 /
  SAVE Para_Names,Para_map,Distribution
! 
    CALL Str_nxtat(buffer,ibuff,atom,80)
    CALL Str_mtchcomm1(atom,Para_Names,SELECT)
    IF (SELECT <= 0 .or. SELECT >= 40) GO TO 9999
    parameterNo=Para_Map(SELECT)
    CALL Str_nxtat(buffer,ibuff,atom,80)
    CALL Str_mtchcomm1(atom,Distribution,SELECT)
    IF (SELECT <= 0 .or. SELECT >= 2) GO TO 9998
    MCParaCharac%PARAMETER(parameterNo)%MCDistribution=SELECT
    READ(buffer(ibuff:),*,ERR=9997)                                  &
&       MCParaCharac%PARAMETER(parameterNo)%Min                      &
&      ,MCParaCharac%PARAMETER(parameterNo)%Max
    IF (MCParaCharac%PARAMETER(parameterNo)%Min                      &
&       >= MCParaCharac%PARAMETER(parameterNo)%Max) THEN
      CALL Message_Output(Message_Info                               &
&        ,'Invalid Monte-Carlo PARAMETER Range Min >= Max'           &
&        ,MCParaCharac%PARAMETER(parameterNo)%Min,'>'                &
&        ,MCParaCharac%PARAMETER(parameterNo)%Max)
    END IF
  RETURN
! 
9999 CALL Message_Output(Message_ErrorContinue,                      &
&              'Invalid Monte-Carlo PARAMETER Name '//trim(buffer))
     RETURN
! 
9998 CALL Message_Output(Message_ErrorStop,                          &
&              'Invalid Monte-Carlo Distribution Specified '         &
&               //trim(buffer))
! 
9997 CALL Message_Output(Message_ErrorStop,                          &
&              'Invalid Monte-Carlo PARAMETER Range '//trim(buffer))
END SUBROUTINE MC_Parameters
! 
!  =======================================================================================
!             Input the Monte Carlo Initial RST2 files
!  =======================================================================================
! 
SUBROUTINE MC_RST2FileList(buffer,ibuff,atom,MCParaCharac)
  USE ModelParameters
  USE Support
  USE SiberiaConstants
  USE SiberiaTypes
! 
  IMPLICIT NONE
  INTEGER ibuff
  CHARACTER(80) :: atom
  CHARACTER(255) :: buffer
  TYPE(MCParaCharacArray) :: MCParaCharac
! 
  INTEGER :: SELECT,parameterNo,i
  LOGICAL :: lexist
  character(255) :: FileName,junk
  CHARACTER(10) :: FileSample(2)
  INTEGER :: select1
  DATA FileSample(1)  / 'SEQUENTIAL'/        &
&      FileSample(2)  / 'RANDOM'/
  SAVE FileSample
! 
    MCParaCharac%RST2FileList%NoFiles=0 
    CALL Str_nxtat(buffer,ibuff,atom,80)
    CALL Str_mtchcomm1(atom,FileSample,SELECT)
    select case (select)
    case default
      go to 9999
    case (1)
      MCParaCharac%RST2FileList%SampleMode=1
    case (2)
      MCParaCharac%RST2FileList%SampleMode=2
    end select
    FileName=' '
    read(buffer(ibuff:),*,err=9998) FileName
    inquire(file=FileName,exist=lexist)
    if (.not. lexist) go to 9997
    open(unit=GlobalTempUnit2,file=FileName,status='old',err=9996)
    do i=1,MaxNoFileList
      read(GlobalTempUnit2,6000,err=9994,end=7000)                       &
&                  MCParaCharac%RST2FileList%FileList(i)
      MCParaCharac%RST2FileList%NoFiles=i
 6000 format(a)
    end do
!  if you can read another line then there are too many filenames, 
!     otherwise just enough
    read(GlobalTempUnit2,6000,err=9995,end=7000) junk 
    go to 9995
 7000 continue
    close(unit=GlobalTempUnit2,status='keep')
  RETURN
! 
9999 CALL Message_Output(Message_ErrorContinue,                               &
&            'Invalid Monte-Carlo FileList Option ='//trim(buffer))
     RETURN
! 
9998 CALL Message_Output(Message_ErrorStop,                                   &
&            'Invalid Monte-Carlo FileList input file ='//trim(buffer))
! 
9997 CALL Message_Output(Message_ErrorStop,                                   &
&            'Monte-Carlo FileList input file does not exist  ='              &
&             //trim(buffer))
! 
9996 CALL Message_Output(Message_ErrorStop,                                   &
&          'Monte-Carlo FileList input file exists but cannot be opened  ='   &
&           //trim(buffer))
! 
9995 CALL Message_Output(Message_ErrorContinue                                &
&        ,'Monte-Carlo FileList input file has too many files '               &
&         //' > ',MaxNoFileList)
     RETURN
9994 CALL Message_Output(Message_ErrorStop                                    &
&          ,'Monte-Carlo FileList error while reading input '      &
&         //'files at line ',MCParaCharac%RST2FileList%NoFiles+1)
END SUBROUTINE MC_RST2FileList
! 
!  =======================================================================================
!      Save the original input parameters for a MC simulation
!  =======================================================================================
! 
subroutine MC_SaveParameters(OriginalParameters)
!  we need to save the original parameters as modified by the user
!  because each time we input a new MC RST2 file READIN overwrites these
!  parameters. Simplest solution is to store the parameters set by the
!  user. Note we need to be a bit subtle about this because there
!  are parameters in the RST2 file that define the DEM 
!  (kx, ky, Gridxy,East, North) and 
!  the user may not have used a RST2 file at the initalisation stage
!  of the program so we cannot guarantee that there parameters in the
!  global parameters are necessarily correct. This is handled in 
!  MCSimParameters_Init.
  USE SiberiaConstants
  USE ModelParameters
  USE SiberiaTypes
  implicit none
!  
  integer :: i 
  TYPE(LocalParametersArray) :: OriginalParameters
! 
!   setting the INTEGER parameters
    do i=1,20
      OriginalParameters%IntVar(i)=IntVar(i)
    end do
!   setting the REAL parameters
    do i=1,50
      OriginalParameters%RealVar(i)=RealVar(i)
    end do
!   setting the filename parameters
    do i=1,10
      OriginalParameters%FileNames(i)=FileNameUser(i)
    end do
    return
END SUBROUTINE MC_SaveParameters
! 
!  =================================================================
!      Initialise the parameters for a MC simulation
!  =================================================================
! 
SUBROUTINE MCSimParameters_Init(Sim_Parameters,MCParaCharac               &
&               ,MCRandomSeed,OriginalParameters)
  USE SiberiaConstants
  USE ModelParameters
  USE SiberiaTypes
  USE Support
  implicit none
! 
  INTEGER :: MCRandomSeed
  TYPE(MCParaCharacArray) :: MCParaCharac
  TYPE(LocalParameters) :: Sim_Parameters
  TYPE(LocalParametersArray) :: OriginalParameters
! 
  INTEGER :: i
  REAL(KIND(0.0D0)) :: Temp(70)
! 
!  sampling INTEGER parameters 
!  ---------------------------
    Temp(1)=RunTime
    Temp(2)=StatsTime
    Temp(3)=kx
    Temp(4)=ky
    DO i=5,20
      IF (MCParaCharac%PARAMETER(i)%MCdistribution == 1) THEN
        Temp(i)=MCParaCharac%PARAMETER(i)%min                               &
&           +(MCParaCharac%PARAMETER(i)%max-MCParaCharac%PARAMETER(i)%min)  &
&           *ran2(MCRandomSeed)
      ELSE
        Temp(i)=OriginalParameters%IntVar(i)
      END IF
    END DO
! 
!  sampling REAL parameters
!  ------------------------
    DO i=21,53
!  uniform distribution
      IF (MCParaCharac%PARAMETER(i)%MCdistribution == 1) THEN
        Temp(i)=MCParaCharac%PARAMETER(i)%min                               &
&           +(MCParaCharac%PARAMETER(i)%max-MCParaCharac%PARAMETER(i)%min)  &
&           *ran2(MCRandomSeed)
      ELSE
        Temp(i)=OriginalParameters%RealVar(i-20)
      END IF
    END DO
    Temp(54)=GridXY
    Temp(55)=East
    Temp(56)=North
    DO i=57,70
      IF (MCParaCharac%PARAMETER(i)%MCdistribution == 1) THEN
        Temp(i)=MCParaCharac%PARAMETER(i)%min                               &
&           +(MCParaCharac%PARAMETER(i)%max-MCParaCharac%PARAMETER(i)%min)  &
&           *ran2(MCRandomSeed)
      ELSE
        Temp(i)=OriginalParameters%RealVar(i-20)
      END IF
    END DO
!   setting the INTEGER parameters
    Sim_Parameters%RunTime=Temp(1)
    Sim_Parameters%StatsTime=Temp(2)
    Sim_Parameters%kx=Temp(3)
    Sim_Parameters%ky=Temp(4)
    Sim_Parameters%modeIC=Temp(5)
    Sim_Parameters%TimeUp=Temp(6)
    Sim_Parameters%ModeSolver=Temp(7)
    Sim_Parameters%ModeDir=Temp(8)
    Sim_Parameters%ModeUplift=Temp(9)
    Sim_Parameters%ModeRandom=Temp(10)
    Sim_Parameters%ModeErode=Temp(11)
    Sim_Parameters%ModeRunoff=Temp(12)
    Sim_Parameters%ModeChannel=Temp(13)
    Sim_Parameters%ModeDP=Temp(14)
    Sim_Parameters%ModeMC=Temp(15)
    Sim_Parameters%DirReg=Temp(16)
    Sim_Parameters%ModeSoil=Temp(17)
    Sim_Parameters%idummy18=Temp(18)
    Sim_Parameters%idummy19=Temp(19)
    Sim_Parameters%idummy20=Temp(20)
!   setting the REAL parameters
    Sim_Parameters%dZ=Temp(21)
    Sim_Parameters%dZn=Temp(22)
    Sim_Parameters%dZHold=Temp(23)
    Sim_Parameters%QsHold=Temp(24)
    Sim_Parameters%FactMx=Temp(25)
    Sim_Parameters%FRanMn=Temp(26)
    Sim_Parameters%c1=Temp(27)
    Sim_Parameters%YFix=Temp(28)
    Sim_Parameters%FRanCV=Temp(29)
    Sim_Parameters%b3SDs=Temp(30)
    Sim_Parameters%b3SDl=Temp(31)
    Sim_Parameters%TAmp=Temp(32)
    Sim_Parameters%TPeriod=Temp(33)
    Sim_Parameters%TPhase=Temp(34)
    Sim_Parameters%FRanZ=Temp(35)
    Sim_Parameters%a1=Temp(36)
    Sim_Parameters%m3=Temp(37)
    Sim_Parameters%b3=Temp(38)
    Sim_Parameters%b1=Temp(39)
    Sim_Parameters%m1=Temp(40)
    Sim_Parameters%n1=Temp(41)
    Sim_Parameters%Bulk=Temp(42)
    Sim_Parameters%InitTimeStep=Temp(43)
    Sim_Parameters%b5=Temp(44)
    Sim_Parameters%n5=Temp(45)
    Sim_Parameters%SInit=Temp(46)
    Sim_Parameters%m5=Temp(47)
    Sim_Parameters%YHold=Temp(48)
    Sim_Parameters%Notch=Temp(49)
    Sim_Parameters%Cover=Temp(50)
    Sim_Parameters%s0max=Temp(51)
    Sim_Parameters%DTime=Temp(52)
    Sim_Parameters%OTime=Temp(53)
    Sim_Parameters%GridXY=Temp(54)
    Sim_Parameters%East=Temp(55)
    Sim_Parameters%North=Temp(56)
    Sim_Parameters%b6=Temp(57)
    Sim_Parameters%m6=Temp(58)
    Sim_Parameters%b12=Temp(59)
    Sim_Parameters%m12=Temp(60)
    Sim_Parameters%SDRate=Temp(61)
    Sim_Parameters%SDExp1=Temp(62)
    Sim_Parameters%SDExp2=Temp(63)
    Sim_Parameters%SMThreshold=Temp(64)
    Sim_Parameters%SDSMWgt=Temp(65)
    Sim_Parameters%rdummy46=Temp(66)
    Sim_Parameters%rdummy47=Temp(67)
    Sim_Parameters%rdummy48=Temp(68)
    Sim_Parameters%rdummy49=Temp(69)
    Sim_Parameters%rdummy50=Temp(70)
!   setting the filename parameters
    Sim_Parameters%FileFactor=OriginalParameters%FileNames(1)
    Sim_Parameters%FileRunoff=OriginalParameters%FileNames(2)
    Sim_Parameters%FileUplift=OriginalParameters%FileNames(3)
    Sim_Parameters%FileDirections=OriginalParameters%FileNames(4)
    Sim_Parameters%FileMonteCarlo=OriginalParameters%FileNames(5)
    Sim_Parameters%FileChannels=OriginalParameters%FileNames(6)
    Sim_Parameters%FileLayers=OriginalParameters%FileNames(7)
    Sim_Parameters%FileDummy8=OriginalParameters%FileNames(8)
    Sim_Parameters%FileControl=OriginalParameters%FileNames(9)
    Sim_Parameters%FileOthers=OriginalParameters%FileNames(10)
  RETURN
END SUBROUTINE MCSimParameters_Init
! 
!  =======================================================================================
!      Initialise the parameters for a single simulation
!  =======================================================================================
! 
SUBROUTINE MCSimRST2FileName_Init(RSTFile,MCParaCharac,Realisation,Seed)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Support
  implicit none
! 
  integer :: Realisation,Seed
  TYPE(MCParaCharacArray) :: MCParaCharac
  character(*) :: RSTFile
! 
  integer :: Index
! 
    if (MCParaCharac%RST2FileList%SampleMode == 1) then
      Index=mod(Realisation-1,MCParaCharac%RST2FileList%NoFiles)+1
    else
      Index=MCParaCharac%RST2FileList%NoFiles*ran2(Seed)+0.5
      Index=min(MCParaCharac%RST2FileList%NoFiles,Index)
      Index=max(1,Index)
    end if 
    RSTFile=' '
    RSTFile=MCParaCharac%RST2FileList%FileList(Index)
  RETURN
END SUBROUTINE MCSimRST2FileName_Init



END MODULE MonteCarlo