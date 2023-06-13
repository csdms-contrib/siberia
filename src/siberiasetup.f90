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
! ======================================================================
!  SUBROUTINE for inputting the intial options for SIBERIA
! ======================================================================
! 
MODULE SetUp

    INTEGER,PARAMETER :: MaxRSUPara=10
! 
!  DEFAULT BEHAVIOURS
!
    LOGICAL,PARAMETER,private :: PauseAtEndDefault=.false.,RST_Overwrite_Default=.FALSE.
!
!  ACTUAL BEHAVIOURS
!
    INTEGER :: RSUPara(MaxRSUPara),NoRSUPara,No_Threads,OutputFileType(MaxRSUPara)
    LOGICAL :: PauseAtEnd=PauseAtEndDefault,XYZOutput=.false.   &
&             ,Predictor_sed_output=.false.                     &
&             ,Corrector_sed_output=.false.                     &
&             ,RST_Overwrite=RST_Overwrite_Default


!        OUTPUT OPTIONS

    integer,parameter,private :: NoOutputCommands=83 
!    integer,parameter,private :: NoOutputCommands=53 
    CHARACTER(21),private,parameter ::       &
&      output_commands(NoOutputCommands) =   &
              (/ 'YIELD                '     &
&               ,'AVEYIELD             '     &
&               ,'ZCHANGE              '     &
&               ,'AVEZCHANGE           '     &
&               ,'GULLYPOT             '     &
&               ,'LOGGULLYPOT          '     &
&               ,'ZSUGGEST             '     &
&               ,'DZSUGGEST            '     &
&               ,'TONNESHECTARE        '     &
&               ,'AVETONNESHECTARE     '     &
!  11
&               ,'STABILITY            '     &
&               ,'AREASLOPE            '     &
&               ,'AREASLOPEELEVATION   '     &
&               ,'FLUVIALDIFFUSION     '     &
&               ,'DINFWEIGHTS          '     &
&               ,'DISCHARGE_MEANANNUAL '     &
&               ,'SOILMOISTURE         '     &
&               ,'BEDROCK_Z            '     &
&               ,'BEDROCK_AREA         '     &
&               ,'BEDROCK_DIRECTIONS   '     &
!  21
&               ,'BEDROCK_SLOPE        '     &
&               ,'PREDICTOR_SED        '     &
&               ,'CORRECTOR_SED        '     &
&               ,'PREDCORRECT_SED_DIFF '     &
&               ,'PREDCORRECT_SED_RATIO'     &
&               ,'SED_FLUX             '     &
&               ,'SED_FLUX_POTENTIAL   '     &
&               ,'SED_FLUX_ACTUAL      '     &
&               ,'SURFACE_B1           '     &
&               ,'FLOW_B1              '     &
!  31
&               ,'LAYER_NO             '     &
&               ,'LAYER_1_B1           '     &
&               ,'LAYER_2_B1           '     &
&               ,'LAYER_3_B1           '     &
&               ,'LAYER_4_B1           '     &
&               ,'LAYER_5_B1           '     &
&               ,'LAYER_1_Z            '     &
&               ,'LAYER_2_Z            '     &
&               ,'LAYER_3_Z            '     &
&               ,'LAYER_4_Z            '     &
!  41
&               ,'LAYER_5_Z            '     &
&               ,'LAYER_1_DEPTH        '     &
&               ,'LAYER_2_DEPTH        '     &
&               ,'LAYER_3_DEPTH        '     &
&               ,'LAYER_4_DEPTH        '     &
&               ,'LAYER_5_DEPTH        '     &
&               ,'DOMAIN               '     &
&               ,'LAYER_FLOW_DETACHMENT'     &
&               ,'LAYER_1_DETACHMENT   '     &
&               ,'LAYER_2_DETACHMENT   '     &
!  51
&               ,'LAYER_3_DETACHMENT   '     &
&               ,'LAYER_4_DETACHMENT   '     &
&               ,'LAYER_5_DETACHMENT   '     &
&               ,'FLOW_TRACER_1        '     &
&               ,'FLOW_TRACER_2        '     &
&               ,'FLOW_TRACER_3        '     &
&               ,'FLOW_TRACER_4        '     &
&               ,'FLOW_TRACER_5        '     &
&               ,'LAYER_1_TRACER_1     '     &
&               ,'LAYER_1_TRACER_2     '     &
&               ,'LAYER_1_TRACER_3     '     &
&               ,'LAYER_1_TRACER_4     '     &
&               ,'LAYER_1_TRACER_5     '     &
&               ,'LAYER_2_TRACER_1     '     &
&               ,'LAYER_2_TRACER_2     '     &
&               ,'LAYER_2_TRACER_3     '     &
&               ,'LAYER_2_TRACER_4     '     &
&               ,'LAYER_2_TRACER_5     '     &
&               ,'LAYER_3_TRACER_1     '     &
&               ,'LAYER_3_TRACER_2     '     &
&               ,'LAYER_3_TRACER_3     '     &
&               ,'LAYER_3_TRACER_4     '     &
&               ,'LAYER_3_TRACER_5     '     &
&               ,'LAYER_4_TRACER_1     '     &
&               ,'LAYER_4_TRACER_2     '     &
&               ,'LAYER_4_TRACER_3     '     &
&               ,'LAYER_4_TRACER_4     '     &
&               ,'LAYER_4_TRACER_5     '     &
&               ,'LAYER_5_TRACER_1     '     &
&               ,'LAYER_5_TRACER_2     '     &
&               ,'LAYER_5_TRACER_3     '     &
&               ,'LAYER_5_TRACER_4     '     &
&               ,'LAYER_5_TRACER_5     '     &
&                                    /)
    character(50),private,parameter :: output_title(NoOutputCommands)=   &
!                               1-5
&             (/ 'Point yield in mm                        '      &
&               ,'Spatially Average yield in mm            '      &
&               ,'Point Elevation differences in m         '      &
&               ,'Spatially averaged Elevation differences '      &
&               ,'Gully Potential                          '      &
!                               6-10
&               ,'log(Gully Potential)                     '      &
&               ,'Suggested Elevations                     '      &
&               ,'Suggested Elevations Changes from Initial'      &
&               ,'Yield in Tonnes/Ha                       '      &
&               ,'Spatial Ave Yield in Tonnes/Ha           '      &
!                              11-15
&               ,'Stability Number                         '      &
&               ,'Area-Slope Number                        '      &
&               ,'Area-Slope-Elevation Number              '      &
&               ,'Fluvial-Diffusion Number                 '      &
&               ,'DInfinity Weights                        '      &
!                              16-20
&               ,'Mean Annual Discharge                    '      &
&               ,'Soil Moisture                            '      &
&               ,'Bedrock Elevations                       '      &
&               ,'Bedrock Area                             '      &
&               ,'Bedrock Directions                       '      &
!                              21-25
&               ,'Bedrock Slope                            '      &
&               ,'Predictor SED                            '      &
&               ,'Corrector SED                            '      &
&               ,'Predictor-Corrector SED Difference       '      &
&               ,'Predictor-Corrector SED Ratio            '      &
!                              26-30
&               ,'Sediment Flux                            '      &
&               ,'Potential Sediment Flux                  '      &
&               ,'Actual Sediment Flux                     '      &
&               ,'Surface B1                               '      &
&               ,'Flow B1                                  '      &
!                              31- 41
&               ,'No of Layers                             '      &
&               ,'Layer 1 B1                               '      &
&               ,'Layer 2 B1                               '      &
&               ,'Layer 3 B1                               '      &
&               ,'Layer 4 B1                               '      &
&               ,'Layer 5 B1                               '      &
&               ,'Layer 1 Elevation                        '      &
&               ,'Layer 2 Elevation                        '      &
&               ,'Layer 3 Elevation                        '      &
&               ,'Layer 4 Elevation                        '      &
!                           41
&               ,'Layer 5 Elevation                        '      &
&               ,'Layer 1 Depth                            '      &
&               ,'Layer 2 Depth                            '      &
&               ,'Layer 3 Depth                            '      &
&               ,'Layer 4 Depth                            '      &
&               ,'Layer 5 Depth                            '      &
&               ,'Domain                                   '      &
&               ,'Flow Detachment                          '      &
&               ,'Layer 1 Detachment                       '      &
&               ,'Layer 2 Detachment                       '      &
!                              51
&               ,'Layer 3 Detachment                       '       &
&               ,'Layer 4 Detachment                       '       &
&               ,'Layer 5 Detachment                       '       &
&               ,'Flow Tracer 1                            '       &
&               ,'Flow Tracer 2                            '       &
&               ,'Flow Tracer 3                            '       &
&               ,'Flow Tracer 4                            '       &
&               ,'Flow Tracer 5                            '       &
&               ,'Layer 1 Tracer 1                         '       &
&               ,'Layer 1 Tracer 2                         '       &
&               ,'Layer 1 Tracer 3                         '       &
&               ,'Layer 1 Tracer 4                         '       &
&               ,'Layer 1 Tracer 5                         '       &
&               ,'Layer 2 Tracer 1                         '       &
&               ,'Layer 2 Tracer 2                         '       &
&               ,'Layer 2 Tracer 3                         '       &
&               ,'Layer 2 Tracer 4                         '       &
&               ,'Layer 2 Tracer 5                         '       &
&               ,'Layer 3 Tracer 1                         '       &
&               ,'Layer 3 Tracer 2                         '       &
&               ,'Layer 3 Tracer 3                         '       &
&               ,'Layer 3 Tracer 4                         '       &
&               ,'Layer 3 Tracer 5                         '       &
&               ,'Layer 4 Tracer 1                         '       &
&               ,'Layer 4 Tracer 2                         '       &
&               ,'Layer 4 Tracer 3                         '       &
&               ,'Layer 4 Tracer 4                         '       &
&               ,'Layer 4 Tracer 5                         '       &
&               ,'Layer 5 Tracer 1                         '       &
&               ,'Layer 5 Tracer 2                         '       &
&               ,'Layer 5 Tracer 3                         '       &
&               ,'Layer 5 Tracer 4                         '       &
&               ,'Layer 5 Tracer 5                         '       &
&                                              /)
    character(11),parameter :: output_abbrev(NoOutputCommands)=                  &
!                          1-5
&         (/'yield       '         &
&          ,'aveyield    '         &
&          ,'zchange     '         &
&          ,'avezchange  '         &
&          ,'gullypot    '         &
!                          6-10
&          ,'loggullypot '         &
&          ,'zsuggest    '         &
&          ,'dzsuggest   '         &
&          ,'thect       '         &
&          ,'avethect    '         &
!                          11-15
&          ,'stab        '         &
&          ,'as          '         &
&          ,'ase         '         &
&          ,'fluvdiff    '         &
&          ,'dinfwgt     '         &
!                         16-20
&          ,'q2          '         &
&          ,'sm          '         &
&          ,'bedrockz    '         &
&          ,'bedrocka    '         &
&          ,'bedrockdir  '         &
!                         21-25
&          ,'bedrocks    '         &
&          ,'psed        '         &
&          ,'csed        '         &
&          ,'pcseddiff   '         &
&          ,'pcsedratio  '         &
!                         26-30
&          ,'qsflux      '         &
&          ,'qsfluxpot   '         &
&          ,'qsfluxact   '         &
&          ,'b1          '         &
&          ,'flowb1      '         &
!                         31-
&          ,'layerno     '         &
&          ,'layer1b1    '         &
&          ,'layer2b1    '         &
&          ,'layer3b1    '         &
&          ,'layer4b1    '         &
!
&          ,'layer5b1    '         &
&          ,'layer1z     '         &
&          ,'layer2z     '         &
&          ,'layer3z     '         &
&          ,'layer4z     '         &
&          ,'layer5z     '         &
&          ,'layer1depth '         &
&          ,'layer2depth '         &
&          ,'layer3depth '         &
&          ,'layer4depth '         &
&          ,'layer5depth '         &
!                         47-
&          ,'domain      '         &
&          ,'flowdetach  '         &
&          ,'layer1detach'         &
&          ,'layer2detach'         &
&          ,'layer3detach'         &
&          ,'layer4detach'         &
&          ,'layer5detach'         &
&          ,'flowtrace1  '         &
&          ,'flowtrace2  '         &
&          ,'flowtrace3  '         &
&          ,'flowtrace4  '         &
&          ,'flowtrace5  '         &
&          ,'layer1trace1'         &
&          ,'layer1trace2'         &
&          ,'layer1trace3'         &
&          ,'layer1trace4'         &
&          ,'layer1trace5'         &
&          ,'layer2trace1'         &
&          ,'layer2trace2'         &
&          ,'layer2trace3'         &
&          ,'layer2trace4'         &
&          ,'layer2trace5'         &
&          ,'layer3trace1'         &
&          ,'layer3trace2'         &
&          ,'layer3trace3'         &
&          ,'layer3trace4'         &
&          ,'layer3trace5'         &
&          ,'layer4trace1'         &
&          ,'layer4trace2'         &
&          ,'layer4trace3'         &
&          ,'layer4trace4'         &
&          ,'layer4trace5'         &
&          ,'layer5trace1'         &
&          ,'layer5trace2'         &
&          ,'layer5trace3'         &
&          ,'layer5trace4'         &
&          ,'layer5trace5'         &
&                                /)

    character(25),parameter :: RSUColumnTitles(NoOutputCommands)=              &
!                     1-3
        (/ 'Yield_(mm)             '     &
&         ,'Ave_Yield_(mm)         '     &
&         ,'Z_Change_(mm)          '     &
!                     4-6
&         ,'Ave_Z_Change_(mm)      '     &
&         ,'Gully_Potential        '     &
&         ,'log(Gully_Potential)   '     &
!                     7-9
&         ,'Z_Suggest_(m)          '     &
&         ,'dZ_Suggest_(m)         '     &
&         ,'Yield_(T/Ha)           '     &
!                    10-12
&         ,'Ave_Yield_(T/Ha)       '     &
&         ,'Stability_No           '     &
&         ,'Area/Slope_No          '     &
!                    13-15
&         ,'Area/Slope/Z_No        '     &
&         ,'Fluvial/Diffusion_No   '     &
&         ,'DInf_Weights           '     &
!                    16-18
&         ,'Discharge_Mean_Annual  '     &
&         ,'Soil_Moisture          '     &
&         ,'Bedrock_Z              '     &
!                    19-21
&         ,'Bedrock_Area           '     &
&         ,'Bedrock_Directions     '     &
&         ,'Bedrock_Slope          '     &
!                    22-24
&         ,'Predictor_SED          '     &
&         ,'Corrector_SED          '     &
&         ,'PredCorrect_SED_Diff   '     &
!                    25-27
&         ,'PredCorrect_SED_Ratio  '     &
&         ,'Sediment_Flux          '     &
&         ,'Potential_Sediment_Flux'     &
!                    28-30
&         ,'Actual_Sediment_Flux   '     &
&         ,'Surface_B1             '     &
&         ,'Flow_B1                '     &
!                    31-41
&         ,'Layer_Nos              '     &
&         ,'Layer_1_B1             '     &
&         ,'Layer_2_B1             '     &
&         ,'Layer_3_B1             '     &
&         ,'Layer_4_B1             '     &
&         ,'Layer_5_B1             '     &
&         ,'Layer_1_Z              '     &
&         ,'Layer_2_Z              '     &
&         ,'Layer_3_Z              '     &
&         ,'Layer_4_Z              '     &
&         ,'Layer_5_Z              '     &
&         ,'Layer_1_Depth          '     &
&         ,'Layer_2_Depth          '     &
&         ,'Layer_3_Depth          '     &
&         ,'Layer_4_Depth          '     &
&         ,'Layer_5_Depth          '     &
!                    47-
&         ,'Domain                 '     &
&         ,'Flow_Detach            '     &
&         ,'Layer_1_Detach         '     &
&         ,'Layer_2_Detach         '     &
&         ,'Layer_3_Detach         '     &
&         ,'Layer_4_Detach         '     &
&         ,'Layer_5_Detach         '     &
&         ,'Flow_Tracer_1          '     &
&         ,'Flow_Tracer_2          '     &
&         ,'Flow_Tracer_3          '     &
&         ,'Flow_Tracer_4          '     &
&         ,'Flow_Tracer_5          '     &
&         ,'Layer_1_Tracer_1       '     &
&         ,'Layer_1_Tracer_2       '     &
&         ,'Layer_1_Tracer_3       '     &
&         ,'Layer_1_Tracer_4       '     &
&         ,'Layer_1_Tracer_5       '     &
&         ,'Layer_2_Tracer_1       '     &
&         ,'Layer_2_Tracer_2       '     &
&         ,'Layer_2_Tracer_3       '     &
&         ,'Layer_2_Tracer_4       '     &
&         ,'Layer_2_Tracer_5       '     &
&         ,'Layer_3_Tracer_1       '     &
&         ,'Layer_3_Tracer_2       '     &
&         ,'Layer_3_Tracer_3       '     &
&         ,'Layer_3_Tracer_4       '     &
&         ,'Layer_3_Tracer_5       '     &
&         ,'Layer_4_Tracer_1       '     &
&         ,'Layer_4_Tracer_2       '     &
&         ,'Layer_4_Tracer_3       '     &
&         ,'Layer_4_Tracer_4       '     &
&         ,'Layer_4_Tracer_5       '     &
&         ,'Layer_5_Tracer_1       '     &
&         ,'Layer_5_Tracer_2       '     &
&         ,'Layer_5_Tracer_3       '     &
&         ,'Layer_5_Tracer_4       '     &
&         ,'Layer_5_Tracer_5       '     &
&                                           /)




CONTAINS

SUBROUTINE SiberiaSetup
  USE Support
  USE SiberiaConstants
    IMPLICIT NONE
! 
! 
    INTEGER :: rclgth,unitno,ibuff,i,start,atomlgth,lineno,UnitNoTmp,dot     &
&             ,FileType
    LOGICAL :: lexist,back
    CHARACTER(80) :: buffer,line
    CHARACTER(255) :: tempfilename,tempstr
    character(13),parameter :: RCfilename='siberia.setup'
    character(19),parameter :: DebugRCfilename='siberia.debug.setup'
    character(255) :: filename=' '
! 
    integer, parameter :: NoCommands=9
    CHARACTER(20) :: commands(NoCommands)
    CHARACTER(80) :: atom
    INTEGER :: SELECT
    SAVE commands
    DATA commands(1)  / 'OUTPUT'/               &
&        commands(2)  / 'ECHO'/                 &
&        commands(3)  / 'NOECHO'/               &
&        commands(4)  / 'PAUSE_AT_END'/         &
&        commands(5)  / 'NO_THREADS'/           &
&        commands(6)  / 'XYZ_FILE'/             &
&        commands(7)  / 'ECHO_INCR'/            &
&        commands(8)  / 'OUTPUT_BIN'/           &
&        commands(9)  / 'RST_OVERWRITE'/
! 
!  set the defaults
! 
      NoRSUPara=0
      lineno=0
      No_Threads=5
      UnitNo=File_FreeUnitNo()
! 
!  override the defaults with contents of the file 'siberia.setup'
! 
!      If (debug) then
!        filename=DebugRCfilename
!      else
!        filename=RCfilename
!      end if
!
!  if a debug setup file exists use it and set debug mode to true
!
      INQUIRE(file=trim(DefDirectory)//trim(DebugRCfilename), EXIST=lexist)
	  if (lexist) then
	    debug=.true.
	    filename=trim(DefDirectory)//DebugRCfilename
	  else
	    debug=.false.
        filename=trim(DefDirectory)//RCfilename
	  end if
      INQUIRE(file=trim(filename), EXIST=lexist)
      IF (.not.lexist) THEN
	    write (*,*) trim(filename)
        CALL Message_Output(Message_WarnContinue,'No #'//trim(filename)//'# file')
        RETURN
      END IF
      OPEN(UNIT=unitno,FILE=trim(filename),STATUS='old',ERR=9996)
! 
 8002 Line=' '
      READ(unitno,6000,END=8000,ERR=8001) line
      lineno=lineno+1
 6000 FORMAT(a)
      DO i=1,80
        start=i
        IF (line(i:i) /= ' ') GO TO 8003
      END DO
      GO TO 8002
!
 8003 IF (line(start:start) == '#' .or. line(start:start) == '!') GO TO 8002
      ibuff=start
      buffer(1:80)=line(1:80)
      CALL Str_UpperCase(buffer)
      CALL Str_nxtat(buffer,ibuff,atom,80)
      CALL Str_mtchcomm1(atom,commands,SELECT)
      SELECT CASE (SELECT)
! 
      CASE DEFAULT
        GO TO 8001
! 
!  deal with extra output options in RSU files
! 
      CASE(1,8)
        if (select == 1) then
          FileType=FileTypeRSU
        else if (select == 8) then
          FileType=FileTypeBIN
        else
          call Message_Output(Message_WarnContinue,'Invalid file output option: '//trim(line))
        end if
        CALL OutputSetup(buffer,ibuff,rclgth,line,FileType)
! 
!  turn echoing on
! 
      CASE(2,7)
        CALL Str_nxtat1(line,ibuff,atom,80,atomlgth)
        IF (select == 2) then
          tempfilename=' '
          tempfilename=atom(1:atomlgth)
          call Message_EchoSet(EchoFileStart,trim(tempfilename))
        else
          back=.true.
          dot=scan(atom(1:atomlgth),'.',back)
          tempstr=' '
          if (dot == 0) then
            tempstr=atom(1:atomlgth)//'-'
          else
            tempstr=atom(1:dot-1)//'-'
          end if
          do i=1,9999
            tempfilename=' '
            CALL Str_AppendIntToStr(tempFileName,trim(tempstr)               &
&                 ,i,4)
            if (dot /= 0) then
              tempfilename=trim(tempfilename)//atom(dot:atomlgth)
            end if
            inquire(file=trim(tempfilename),exist=lexist)
            if (.not.lexist) exit
          end do
          call Message_EchoSet(EchoFileStart,trim(tempfilename))
        end if
! 
!  turn on other info that you want output TO echo file
!   (make sure code turns them off when echo set off)
! 
        call Support_Get('echofileunit',UnitNoTmp)
        CALL TimerSetFileOutput(.true.,UnitNoTmp)
        call Message_Output(Message_Info,'-- Echoing to file '//trim(tempfilename))
! 
!  turn echoing off
! 
      CASE(3)
        call Message_EchoSet(EchoFileClose,' ')
        CALL TimerSetFileOutput(.false.,0)
! 
!  Pause at END (useful IF output window closes automatically
!  on completion of PROGRAM ... both Windows and Mac).
!
      CASE(4)
        CALL ModeOnOff(PauseAtEnd,Buffer,IBuff,'PAUSE AT END'                &
&           ,PauseAtEndDefault)
! 
!  specifiy maxiumum no of threads TO USE for parallel code
      CASE(5)
        CALL Str_nxtat(buffer,ibuff,atom,80)
        READ(atom,*,ERR=8001,END=8001) No_Threads
!
!  Output an .xyz file
!
      CASE(6)
        XYZOutput=.true.
        call Message_Output(Message_Info,'-- XYZ output files to be generated')
!
! 
!  Pause at END (useful IF output window closes automatically
!  on completion of PROGRAM ... both Windows and Mac).
!
      CASE(9)
        CALL ModeOnOff(RST_Overwrite,Buffer,IBuff,'RST OVERWRITE'          &
&           ,RST_Overwrite_Default)
!
      END SELECT
 7999 GO TO 8002
! 
 8001 CALL Message_Output(Message_ErrorContinue                            &
&              ,'Incomprehensible input 1 in siberia.setup file '          &
&                //trim(line))
      GO TO 8002
! 
 8000 IF (lineno <= 1) THEN
        CALL Message_Output(Message_WarnContinue                           &
&              ,'siberia.setup file appears TO be empty')
      END IF
      CLOSE(UNIT=unitno,STATUS='keep')
      RETURN
! 
 9996 CALL Message_Output(Message_ErrorContinue                            &
&              ,'Cannot OPEN siberia.setup file ... may be OPEN in '       &
&              //'another application')
      RETURN
! 
 9995 CALL Message_Output(Message_ErrorContinue                            &
&              ,'Check that echo file is not already OPEN in another'      &
&             //' application')
      CALL Message_Output(Message_ErrorContinue                            &
&              ,'Continuing without echoing output TO echo file')
      GO TO 7999
END SUBROUTINE SiberiaSetup
! 
! ======================================================================
!   deal with the output options for the PROGRAM
! ======================================================================
! 
SUBROUTINE OutputSetup(buffer,ibuff,rclgth,line,FileType)
  USE Support
  USE openMPsupport
  USE SiberiaConstants
  IMPLICIT NONE
! 
    INTEGER :: ibuff,rclgth,FileType
    CHARACTER(*) :: buffer,line
! 
! 
!  CAUTION: Number of options below must be keep consistent with 
!  the PARAMETER declaration in RSUOut in inout.f
! 
    CHARACTER(80) :: atom
    INTEGER :: SELECT
! 
 8002 CALL Str_nxtat(buffer,ibuff,atom,80)
      IF (atom(1:1) == ' ') RETURN
      IF (FirstOutputSetup) THEN
        FirstOutputSetup=.false.
        CALL Message_Output(Message_Info,' Output Options')
        CALL Message_Output(Message_Info,' --------------')
      END IF
      CALL Str_mtchcomm1(atom,output_commands,SELECT)
      if (select >=1 .and. select <= NoOutputCommands) then
        NoRSUPara=NoRSUPara+1
        if (FileType == FileTypeRSU) then
          call Message_Output(Message_Info,' RSU output       -- '           &
&                //trim(output_title(select)))
        else if (FileType == FileTypeBIN) then
          call Message_Output(Message_Info,' RSU & BIN output -- '           &
&                //trim(output_title(select)))
        end if
      else
        GO TO 8001
      end if
      IF (NoRSUPara > MaxRSUPara) THEN
        CALL Message_Output(Message_ErrorStop                                 &
&            ,' RSU output -- Too many RSU output requested'                  &
&            ,NoRSUPara,'>',MaxRSUPara)
        STOP
      END IF
      RSUPara(NoRSUPara)=SELECT
      OutputFileType(NoRSUPara)=FileType
!
!  this is for special flags that are required for SIBERIA to save for output 
!  what are otherwise temporary internal variables
!
      select case (select)
      case (22)
        Predictor_sed_output=.true.
      case (23)
        Corrector_sed_output=.true.
      case (24,25)
        Predictor_sed_output=.true.
        Corrector_sed_output=.true.
      end select
      GO TO 8002
! 
 8001   CALL Message_Output(Message_ErrorContinue                             &
&               ,'Incomprehensible input 2 in siberia.setup file')
        CALL Message_Output(Message_ErrorStop,line)
      GO TO 8002
! 
END SUBROUTINE OutputSetup

end MODULE SetUp
