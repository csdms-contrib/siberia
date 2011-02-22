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
MODULE SiberiaCompute

CONTAINS

SUBROUTINE compute(Area,cDepth,CIF,S0,RanField,Y,Z,sed,dsed,dZdX2,SoilDepth     &
&        ,SoilZ,Erode_m1,Erode_n1,Discharge,DischargeM1,Hill_Channel_Factor     &
&        ,Direct,DirectDinf,DirWeights,Domain,IrregularBoundary                 &
&        ,GridX,GridY                                                           &
&        ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                      &
&        ,NoRegions,Regions,RegionMap,Region                                    &
&        ,OutFile,LgthRSTFileName,OutputFileExt,Start,LgthOutFileName           &
&        ,iscale,er,nr,NoTime,Time                                              &
&        ,DetCIF,DirChg,TwoFluvialProcesses,SlopeMax,dyc                 &
&        ,iInit,iXXX,iYYY,SimParameters                                         &
&        ,TimerNo,TimerSum,TimerAfter,TimerBefore,MCRealNo,Erosion,ErosionC,SM  &
&        ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections                   &
&        ,SedimentTracking,FlowB1,FlowAge,PotErosion,PotErosionC)
  USE AreaAnalysis
  USE ChannelAnalysis
  USE Control
  USE CtrOutput
  USE DirAnalysis
  USE InputOutput
  USE LayerSupport
  USE LayerConstants
  USE Others
  USE RSUOutput
  USE SedAnalysis
  USE Setup
  USE SiberiaConstants
  USE SiberiaTypes
  USE SoilAnalysis
  USE Support
  USE UserOtherAnalysis
  IMPLICIT NONE
! 
!   Dummy Variables
!   ---------------
! 
    INTEGER :: LowX,HighX,LowY,HighY,iInit,iXXX(*),iYYY(*)                &
&             ,NoFlowIn,FlowInIJ(2,*),NoRegions,Regions(*),RegionMap(*)         &
&             ,LgthRSTFileName,Start,iScale,TimerNo(*),NoTime,Time(*)           &
&             ,LgthOutFileName,GridX,GridY,MCRealNo
    REAL(KIND(0.0D0)) :: FlowInAS(2,*),er(2),nr(2),TimerSum(*),TimerAfter(*)    &
&             ,TimerBefore(*),dyc
    LOGICAL :: IrregularBoundary,DetCIF,DirChg,TwoFluvialProcesses,SlopeMax     &
&             ,SedimentTracking
    INTEGER,DIMENSION(GridX,GridY) :: Direct,DirectDinf,Region
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) ::                                 &
&              Area,cDepth,CIF,S0,RanField,Y,Z                                  &
&             ,DirWeights,sed,dsed,dZdX2,SoilDepth,SoilZ,Erode_m1,Erode_n1      &
&             ,Discharge,DischargeM1,Hill_Channel_Factor
    TYPE(ArrayR8XY) :: Erosion,ErosionC,SM,BedrockZ,BedRockArea,BedRockSlope    &
&             ,FlowB1,FlowAge,PotErosion,PotErosionC
    TYPE(ArrayIXY) :: BedrockDirections
    LOGICAL,DIMENSION(GridX,GridY) :: Domain
    CHARACTER(5) :: OutputFileExt
!    CHARACTER(80) :: OutFile
    CHARACTER(*) :: OutFile
    TYPE(LocalParameters) :: SimParameters
! 
!   Local Variables
!   ---------------
! 
    INTEGER :: iTime,itmptr,iself,iTime1,iTotal                                 &
&             ,InnerLoop,ip,Discrete,Discrete1,iii,isteps,i,j,ErrorNo
    REAL(KIND(0.0D0)) :: SimTime,InitTimeStep1,InitTimeStepj,Zin,Zin1,Zout      &
&             ,Zout1,maxst1,maxst2,MaxDz1,MaxDz2,meanSt,TotTime,OldInt,NewInt   &
&             ,SOutFlow,ZMassA,ZMassP,sumCycle,sumCycle1        &
&             ,sumSed(MaxRegions)
    LOGICAL :: AreaError,predict,RunDone
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: ZOriginal,Factor,ZOld,YOld      &
&             ,SoilDepthOld,yc,sedc,Uplift,tempB1,tempAge
    TYPE(ArrayR8XY) :: Predictor_sed,Corrector_sed
    CHARACTER(1000) :: filename
!    CHARACTER(80) :: filename
! 
!  storing the original elevations
!
      DO j=1,HighY
        DO i=1,HighX
          Zoriginal(i,j)=Z(i,j)
        END DO
      END DO
      tempB1=0
      tempAge=0
      if (Predictor_sed_output) then
        call AllocateArray(Predictor_sed,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Predictor_sed','SIBERIA_COMPUTE')
        Predictor_sed%data=0
      end if
      if (Corrector_sed_output) then
        CALL AllocateArray(Corrector_sed,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Corrector_sed','SIBERIA_COMPUTE')
        Corrector_sed%data=0
      end if
      ZMassA=0
      ZMassP=0
      iTime=0
      itmptr=1
      Zin=0
      Zout=0
      sumCycle=0
      isteps=0
      RunDone=.false.
! 
!  Initialisation for the Adaptive timestepping
! 
      IF (SimParameters%InitTimeStep <= 0.0) THEN
! 
!   Adaptive timestepping
!   ---------------------
! 
!   Small timestep
        if (Layer_InitI()) then
		    CALL Message_Output(Message_ErrorStop,                            &
&            'Adaptive timestepping not currently implemented for Layering')
		end if
        InitTimeStep1=0.0001
        SimTime=0
        itime1=iTime
        CALL SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
        CALL ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                     &
&              ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF            &
&              ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
        CALL DirAnal(Z,Direct,DirectDinf,s0,Y                                  &
&              ,cDepth,DirChg,iself,Domain, IrregularBoundary,iInit            &
&              ,iXXX,iYYY,DirWeights,LowX,HighX,LowY,HighY,SimParameters       &
&              ,gridX,gridY)
        dirchg=.true.
        CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                        &
&              ,Domain,AreaError,IrregularBoundary                             &
&              ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS               &
&              ,SimParameters,GridX,GridY)
        Predict=.true.
        CALL SedAnal(Sed,Direct,DirectDinf,DirWeights,Area,Z,Zoriginal,y       &
&              ,s0,Dsed,gridX,gridY,itime1,iInit,iXXX,iYYY,RanField,DirChg     &
&              ,InitTimeStep1,dZdX2,Zout                                       &
&              ,Zin,Domain, IrregularBoundary,DetCIF,SimTime,SumCycle,cDepth   &
&              ,Region,RegionMap,NoRegions,sumsed,TwoFluvialProcesses          &
&              ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS               &
&              ,Factor,DischargeM1,SlopeMax,Erode_m1,Erode_n1,Discharge        &
&              ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge,Uplift   &
&              ,PotErosion)
! 
!  Initialisation of Discrete
! 
        InitTimeStepj=InitTimeStep1
        CALL Adaptive2(InitTimeStepj,Discrete,Sed,s0,dZdX2                     &
&            ,area,Direct,z,IrregularBoundary,Domain,maxst1,maxst2             &
&            ,MaxDz1,MaxDz2,MeanSt,LowX,HighX,LowY,HighY,SimParameters         &
&            ,gridX,gridY)
        SimParameters%InitTimeStep=-InitTimeStep1
      ELSE
! 
!   Fixed Timestepping
!   ------------------
! 
!   Needs to do all this stuff to make sure loop is initialised correctly.
        InitTimeStep1=0.0001
        SimTime=0
        itime1=iTime
        CALL SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
        CALL ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                       &
&            ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF                &
&            ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
        CALL DirAnal(Z,Direct,DirectDinf                                         &
&            ,s0,Y,cDepth,DirChg,iself,Domain, IrregularBoundary                 &
&            ,iInit,iXXX,iYYY,DirWeights,LowX,HighX,LowY,HighY,SimParameters     &
&            ,gridX,gridY)
        dirchg=.true.
        CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                          &
&            ,Domain,AreaError,IrregularBoundary                                 &
&            ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                   &
&            ,SimParameters,GridX,GridY)
        Predict=.true.
        CALL SedAnal(Sed,Direct,DirectDinf,DirWeights,Area,Z,Zoriginal,y         &
&                ,s0,Dsed,gridX,gridY,itime1,iInit,iXXX,iYYY,RanField,DirChg     &
&                ,InitTimeStep1,dZdX2,Zout                                       &
&                ,Zin,Domain, IrregularBoundary, DetCIF                          &
&                ,SimTime,SumCycle,cDepth,Region,RegionMap,NoRegions             &
&                ,sumsed,TwoFluvialProcesses                                     &
&                ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS               &
&                ,Factor,DischargeM1,SlopeMax,Erode_m1,Erode_n1,Discharge        &
&                ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge,Uplift   &
&                ,PotErosion)
! 
!  Initialisation of Discrete
! 
        InitTimeStepj=InitTimeStep1
        CALL Adaptive2(InitTimeStepj,Discrete,Sed,s0,dZdX2                       &
&            ,area,Direct,z,IrregularBoundary,Domain,maxst1,maxst2               &
&            ,MaxDz1,MaxDz2,meanSt,LowX,HighX,LowY,HighY,SimParameters           &
&            ,gridX,gridY)
      END IF
      tempb1=simparameters%b1
      tempage=1
!	  write (*,*) 'siberia-compute here 0',z(164,10),erosion%data(164,10),sed(164,10)
      CALL Layer_FlowTracking(Erosion,Direct,Domain,LowX,HighX,LowY,HighY        &
&            ,GridX,GridY)
      if (notime >= 1 .and. time(1) == 0) then
           itmptr=itmptr+1
           iTime=0
           CALL SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
           CALL ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                      &
&                  ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF            &
&                  ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
           CALL DirAnal(Z,Direct,DirectDinf,s0,y,cDepth,DirChg,iself               &
&                      ,Domain,IrregularBoundary,iInit,iXXX,iYYY,DirWeights        &
&                      ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
           IF (DirChg) THEN
             CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                       &
&                    ,Domain,AreaError,IrregularBoundary                           &
&                    ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS             &
&                    ,SimParameters,GridX,GridY)
           END IF
           CALL FAppend(FileName,Outfile,LgthOutFileName                           &
&                ,iTime+Start,OutputFileExt)
           if (.not.File_Exist(.not.RST_Overwrite,trim(filename))) then
             CALL RSTOut1(RanField,Y,Z,Area,FileName                               &
&                   ,iInit,iXXX,iYYY,Direct,iTime+Start                            &
&                   ,IrregularBoundary,Domain,s0,DetCIF                            &
&                   ,Version,OutputFileExt,iscale,er,nr,Sed,cDepth                 &
&                   ,SoilDepth,SimParameters,XYZOutput,gridX,gridY)
             CALL FAppend(FileName,Outfile,LgthOutFileName                         &
&                   ,iTime+Start,'.rsu ')
             CALL RSUOut(FileName,Sed,Area,Direct,Domain                             &
&                   ,Z,Zoriginal,s0,IrregularBoundary                              &
&                   ,NoFlowIn,FlowInIJ,FlowInAS,InitTimeStep1                      &
&                   ,LowX,HighX,LowY,HighY,DirWeights,SimParameters                &
&                   ,Discharge,Erosion,SM                                          &
&                   ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections           &
&                   ,Predictor_sed,Corrector_sed,GridX,GridY)
             CALL FAppend(FileName,Outfile,LgthOutFileName                 &
&                    ,iTime+Start,'')
             CALL Layer_OutputData(Filename,Z,GridX,GridY                  &
&                     ,LowX,HighX,LowY,HighY,SimParameters)
           else
             CALL Message_Output(Message_ErrorStop,'RST output file already exists = '      &
&                                //trim(filename))
           end if
      end if
! 
!  ===============================================================
!  ============= Start of the computation loop ===================
!  ===============================================================
! 
      iTotal=0
      IF (SimParameters%RunTime > 0) THEN
        InnerLoop=min(abs(SimParameters%StatsTime),SimParameters%RunTime)
      ELSE
        InnerLoop=abs(SimParameters%StatsTime)
      END IF
   3  IF (SimParameters%RunTime > 0.and.iTotal >= SimParameters%RunTime) GO TO 99
      DO ip=1,InnerLoop
! 
!   Keep track of what time step we are at
! 
         tottime=0.0
         iTime=iTime+1
         IF (SimParameters%InitTimeStep > 0.0) THEN
! 
!   Fixed Timestepping
! 
           Discrete=1.0/SimParameters%InitTimeStep
           CALL Adaptive2(InitTimeStep1,discrete1,Sed,s0,dZdX2                    &
&            ,area,Direct,z,IrregularBoundary,Domain,maxst1,maxst2                &
&            ,MaxDz1,MaxDz2,MeanSt,LowX,HighX,LowY,HighY,SimParameters            &
&            ,gridX,gridY)
! 
!          allow for instabilities from rounding errors 
!          IF we have READ from a file
! 
!           IF (InFile.and.iTime == 1) Discrete=Discrete*5
           InitTimeStep1=1.0/float(Discrete)
         ELSE
! 
!    Adaptive Timestepping
! 
           CALL Adaptive2(InitTimeStep1,discrete1,Sed,s0,dZdX2                     &
&            ,area,Direct,z,IrregularBoundary,Domain,maxst1,maxst2                 &
&            ,MaxDz1,MaxDz2,MeanSt,LowX,HighX,LowY,HighY,SimParameters             &
&            ,gridX,gridY)
           SimParameters%InitTimeStep=-InitTimeStep1
           Discrete=alpha*Discrete+(1-alpha)*discrete1
           Discrete=max(1,Discrete)
           InitTimeStep1=1.0/Discrete
         END IF
! ----------------------------------------------------
!           Timestep Discretisation
! ----------------------------------------------------
         DO iii=1,Discrete    
         TimerBefore(7)=TimerGet()
! 
!  Checking that the timestepping does not exceed the stability thresholds
!  during the timestepping
! 
!           IF (InitTimeStep < 0.0) THEN
!             InitTimeStepj=InitTimeStep1
!             CALL Adaptive1(InitTimeStepj,discrete1,Sed,s0,dZdX2,Direct,GridX,GridY
!     &         ,IrregularBoundary,Domain,maxst1,maxst2,MaxDz1,MaxDz2,MeanSt
!     &         ,LowX,HighX,LowY,HighY,SimParameters)
!             IF (InitTimeStepj < InitTimeStep1) THEN
!              WRITE (*,*) 'Adaptive timestepping threshold exceeded'
!    &         ,' at time ',ip,iii,InitTimeStepj,InitTimeStep1
!             END IF
!           END IF
           SimTime=iTime+tottime-1.0
           isteps=isteps+1
! 
!  TEMP. storage of values for the corrector
! 
           IF (IrregularBoundary) THEN
             DO j=LowY-1,HighY+1
               DO i=LowX-1,HighX+1
                 IF (Domain(i,j)) THEN
                   Zold(i,j)=Z(i,j)
                   YOld(i,j)=y(i,j)
                   SoilDepthOld(i,j)=SoilDepth(i,j)
                 END IF
               END DO
             END DO
           ELSE
             DO j=LowY-1,HighY+1
               DO i=LowX-1,HighX+1
                 Zold(i,j)=Z(i,j)
                 YOld(i,j)=y(i,j)
                 SoilDepthOld(i,j)=SoilDepth(i,j)
               END DO
             END DO
           END IF
! 
!              PREDICTOR STEP
!              ==============
! 
!   set the appropiate B.C. at the edge of the Grid
! 
           TimerBefore(8)=TimerGet()
!			 write (*,*) 'siberia-compute here 3a',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
           CALL SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
!           CALL TimerShow(' before DirAnal')
! 
!    Analyse the Elevation values and determine the directions
!    of drainage of each of the nodes in the Grid
! 
           CALL ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                      &
&                  ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF            &
&                  ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
           TimerAfter(8)=TimerGet()
           TimerNo(8)=TimerNo(8)+1
           TimerSum(8)=TimerSum(8)+TimerAfter(8)-TimerBefore(8)
           TimerBefore(1)=TimerGet()
           CALL DirAnal(Z,Direct,DirectDinf,s0,Y,cDepth,DirChg,iself               &
&                      ,Domain, IrregularBoundary,iInit,iXXX,iYYY,DirWeights       &
&                      ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
           TimerAfter(1)=TimerGet()
           TimerNo(1)=TimerNo(1)+1
           TimerSum(1)=TimerSum(1)+TimerAfter(1)-TimerBefore(1)

! 
!    On the basis of the drainage directions analyse the Grid TO 
!    determine the drainage Area passing through each node
! 
           TimerBefore(2)=TimerGet()
           IF (DirChg) THEN
             CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                        &
&                  ,Domain,AreaError,IrregularBoundary                              &
&                  ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                &
&                  ,SimParameters,GridX,GridY)
             TimerNo(2)=TimerNo(2)+1
           END IF
!			 write (*,*) 'siberia-compute here 3b',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
! 
! ------------------------------------------------------------------
!  Output DATA and user-defined dependent variable routine
!  here so that areas and slopes are consistent with the
!  latest values of elevations
! -------------------------------------------------------------------
! 
           TimerAfter(2)=TimerGet()
           TimerSum(2)=TimerSum(2)+TimerAfter(2)-TimerBefore(2)
!           CALL TimerShow(' after AreaAnal')
           CALL UserOther(CIF,Z,Area,y,s0,Direct,Sed                           &
&                     ,iTime,tottime,IrregularBoundary,Domain,DetCIF           &
&                     ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
!  check for RST2 screen stats output or termination
           IF (ip == 1.and.iii == 1) THEN
             IF (SimParameters%RunTime == -1) THEN
               NewInt=CalcHypsInt(z,LowX,HighX,LowY,HighY                      &
&                        ,IrregularBoundary,Domain,gridX,gridY)
               CALL Message_Output(Message_Info,'Hypsometric Integral = '      &
&                                ,NewInt)
               IF (abs((NewInt-OldInt)/NewInt) < eps) THEN
                 CALL Message_Output(Message_Info                              &
&                            ,'-- Hypsometric curve converged')
                 CALL FAppend(FileName,Outfile,LgthRSTFileName                 &
&                    ,iTime+Start-1,OutputFileExt)
                 if (.not.File_Exist(.not.RST_Overwrite,trim(filename))) then
                   CALL RSTOut1(RanField,Y,Z,Area,FileName                       &
&                     ,iInit,iXXX,iYYY,Direct,iTime+Start                        &
&                     ,IrregularBoundary,Domain,s0,DetCIF,Version                &
&                     ,OutputFileExt,iscale,er,nr,Sed,cDepth,SoilDepth           &
&                     ,SimParameters,XYZOutput,gridX,gridY)
                   CALL FAppend(FileName,Outfile,LgthRSTFileName                 &
&                      ,iTime+Start-1,'.rsu ')
                   CALL RSUOut(FileName,Sed,Area,Direct                          &
&                        ,Domain,Z,Zoriginal,s0,IrregularBoundary              &
&                        ,NoFlowIn,FlowInIJ,FlowInAS,InitTimeStep1             &
&                        ,LowX,HighX,LowY,HighY,DirWeights,SimParameters       &
&                        ,Discharge,Erosion,SM,BedrockZ                        &
&                        ,BedRockArea,BedRockSlope,BedrockDirections           &
&                        ,Predictor_sed,Corrector_sed,GridX,GridY)
                   CALL FAppend(FileName,Outfile,LgthRSTFileName                 &
&                      ,iTime+Start-1,'')
                   CALL Layer_OutputData(Filename,Z,GridX,GridY                  &
&                       ,LowX,HighX,LowY,HighY,SimParameters)
                 else
                   CALL Message_Output(Message_ErrorStop,'RST output file already exists = '      &
&                                //trim(filename))
                 end if
                 CALL CtrOut(CIF,Z,Area,y,s0,iTotal                            &
&                     ,isteps,Direct,Sed,dZdX2,Soutflow,IrregularBoundary      &
&                     ,Domain,iTime,ZmassA,ZMassP,Zin,Zout,DetCIF              &
&                     ,SumCycle,InitTimeStep1,iInit,iXXX,iYYY                  &
&                     ,SumSed,NoRegions,SoilDepth                              &
&                     ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ                 &
&                     ,Factor,DischargeM1,MaxSt2,MaxDz2,MeanSt                 &
&                     ,Hill_Channel_Factor,SimParameters,MCRealNo,gridX,gridY)
                 GO TO 99
               END IF
               OldInt=NewInt
             ELSE   ! IF RunTime /= -1 or -2
               CALL CtrOut(CIF,Z,Area,y,s0,iTotal,isteps,Direct,Sed            &
&                     ,dZdX2,Soutflow,IrregularBoundary                        &
&                     ,Domain,iTime,ZmassA,ZMassP,Zin,Zout,DetCIF              &
&                     ,SumCycle,InitTimeStep1,iInit,iXXX,iYYY                  &
&                     ,SumSed,NoRegions,SoilDepth                              &
&                     ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ                 &
&                     ,Factor,DischargeM1,MaxSt2,MaxDz2,MeanSt                 &
&                     ,Hill_Channel_Factor,SimParameters,MCRealNo,gridX,gridY)
             END IF
           END IF
! 
! -----------------------------------------------------------------
!    Calculating the amount that the Elevation will change in the 
!    next time step and the slopes for velocity determination
! -----------------------------------------------------------------
!           CALL TimerShow(' before SedAnal')
           Predict=.true.
           TimerBefore(3)=TimerGet()
!			 write (*,*) 'siberia-compute here 3c',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
           CALL SedAnal(Sed,Direct,DirectDinf,DirWeights,Area,Z,Zoriginal,y     &
&                ,s0,Dsed,gridX,gridY,iTime,iInit,iXXX,iYYY,RanField,DirChg     &
&                ,InitTimeStep1,dZdX2,Zout                                      &
&                ,Zin,Domain, IrregularBoundary,DetCIF                          &
&                ,SimTime,SumCycle,cDepth                                       &
&                ,Region,RegionMap,NoRegions,sumsed,TwoFluvialProcesses         &
&                ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS              &
&                ,Factor,DischargeM1,SlopeMax,Erode_m1,Erode_n1,Discharge       &
&                ,SimParameters,Erosion,SedimentTracking,FlowB1,FlowAge,Uplift  &
&                ,PotErosion)
           if (Predictor_sed_output) then
             Predictor_sed%data(LowX:HighX,LowY:HighY)=sed(LowX:HighX,LowY:HighY)
           end if
           TimerNo(3)=TimerNo(3)+1
           TimerAfter(3)=TimerGet()
           TimerSum(3)=TimerSum(3)+TimerAfter(3)-TimerBefore(3)
           TimerBefore(9)=TimerGet()
!			 write (*,*) 'siberia-compute here 3d',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
           CALL SoilAnal(Area,s0,Sed,SoilDepth,Z,SoilZ                          &
&                ,Domain,IrregularBoundary,InitTimeStep1,LowX,HighX             &
&                ,LowY,HighY,SimParameters,Erosion,iInit,iXXX,iYYY,SM           &
&                ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections           &
&                ,gridX,gridY)
           TimerNo(9)=TimerNo(9)+1
           TimerAfter(9)=TimerGet()
           TimerSum(9)=TimerSum(9)+TimerAfter(9)-TimerBefore(9)
           TimerBefore(5)=TimerGet()
!			 write (*,*) 'siberia-compute here 3e',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
           CALL Finite                                                          &
&               (DirChg,dyc,InitTimeStep1,CIF,y,Z,Sed,s0,Area                   &
&               ,Domain,IrregularBoundary,DetCIF,LowX,HighX,LowY,HighY          &
&               ,SimParameters,GridX,GridY)
           TimerAfter(5)=TimerGet()
           TimerNo(5)=TimerNo(5)+1
           TimerSum(5)=TimerSum(5)+TimerAfter(5)-TimerBefore(5)
           IF (SimParameters%ModeSolver /= 6) THEN
!           IF (ModeSolver < 6) THEN
! 
!              CORRECTOR STEP
!              ==============
! 
             DO j=LowY-1,HighY+1
               DO i=LowX-1,HighX+1
                 yC(i,j)=y(i,j)
               END DO
             END DO
             TimerBefore(8)=TimerGet()
!			 write (*,*) 'siberia-compute here 3f',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
!			 write (*,*) 'siberia-compute here 3g',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                   &
&                  ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF           &
&                  ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
             TimerAfter(8)=TimerGet()
             TimerNo(8)=TimerNo(8)+1
             TimerSum(8)=TimerSum(8)+TimerAfter(8)-TimerBefore(8)
             TimerBefore(1)=TimerGet()
!			 write (*,*) 'siberia-compute here 3h',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL DirAnal(Z,Direct,DirectDinf                                     &
&                      ,s0,y,cDepth,DirChg,iself,Domain,IrregularBoundary         &
&                      ,iInit,iXXX,iYYY,DirWeights                                &
&                      ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
             TimerAfter(1)=TimerGet()
             TimerNo(1)=TimerNo(1)+1
             TimerSum(1)=TimerSum(1)+TimerAfter(1)-TimerBefore(1)
             TimerBefore(2)=TimerGet()
!			 write (*,*) 'siberia-compute here 3i',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             IF (DirChg) THEN
               CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                    &
&                  ,Domain,AreaError,IrregularBoundary                            &
&                  ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS              &
&                  ,SimParameters,GridX,GridY)
             TimerNo(2)=TimerNo(2)+1
             END IF
             TimerAfter(2)=TimerGet()
             TimerSum(2)=TimerSum(2)+TimerAfter(2)-TimerBefore(2)
             Predict=.false.
             TimerBefore(3)=TimerGet()
!			 write (*,*) 'siberia-compute here 3j',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL SedAnal(sedC,Direct,DirectDinf,DirWeights,Area,Z,Zoriginal,y      &
&               ,s0,Dsed,gridX,gridY,iTime,iInit,iXXX,iYYY,RanField,DirChg          &
&               ,InitTimeStep1,dZdX2,Zout1                                          &
&               ,Zin1,Domain, IrregularBoundary, DetCIF                             &
&               ,SimTime,SumCycle1,cDepth,Region,RegionMap,NoRegions                &
&               ,sumsed,TwoFluvialProcesses                                         &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS                   &
&               ,Factor,DischargeM1,SlopeMax,Erode_m1,Erode_n1,Discharge            &
&               ,SimParameters,ErosionC,SedimentTracking,FlowB1,FlowAge,Uplift      &
&               ,PotErosionC)
!             write (*,*) 'siberia-compute here 1'
             if (Corrector_sed_output) then
               Corrector_sed%data(LowX:HighX,LowY:HighY)=sedC(LowX:HighX,LowY:HighY)
             end if
             TimerNo(3)=TimerNo(3)+1
             TimerAfter(3)=TimerGet()
             TimerSum(3)=TimerSum(3)+TimerAfter(3)-TimerBefore(3)
             TimerBefore(9)=TimerGet()
!			 write (*,*) 'siberia-compute here 3k',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL SoilAnal(Area,s0,sedC,SoilDepth,Z,SoilZ                           &
&                ,Domain,IrregularBoundary,InitTimeStep1                            &
&                ,LowX,HighX,LowY,HighY,SimParameters,ErosionC                       &
&                ,iInit,iXXX,iYYY,SM                                                &
&                ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections               &
&                ,gridX,gridY)
             TimerNo(9)=TimerNo(9)+1
             TimerAfter(9)=TimerGet()
             TimerSum(9)=TimerSum(9)+TimerAfter(9)-TimerBefore(9)
             Soutflow=(Zout+Zout1)*0.5/InitTimeStep1
             TimerBefore(5)=TimerGet()
!			 write (*,*) 'siberia-compute here 3l',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL Finite(DirChg,dyc,InitTimeStep1,CIF,yC,Z,sedC,s0,Area             &
&                 ,Domain,IrregularBoundary,DetCIF,LowX,HighX                       &
&                 ,LowY,HighY,SimParameters,GridX,GridY)
             TimerAfter(5)=TimerGet()
             TimerNo(5)=TimerNo(5)+1
             TimerSum(5)=TimerSum(5)+TimerAfter(5)-TimerBefore(5)
             TimerBefore(6)=TimerGet()
!			 write (*,*) 'siberia-compute here 3m',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL Correct(YOld,y,yC,Zold,Sed,sedC,Z,Zout,Zout1,ZmassA,ZmassP        &
&                ,Zin,Zin1,Domain,IrregularBoundary,DetCIF,SoilDepthOld,SoilDepth   &
&                ,SoilZ,LowX,HighX,LowY,HighY,GridX,GridY,erosion,erosionC)
!             write (60,*) 'here 10 ',z(49,4),sed(49,4),sedc(49,4),erosion%data(49,4)
!             write (*,*) 'siberia-compute here 2'
!			 write (*,*) 'siberia-compute here 3n',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL Layer_Uplift(Uplift,lowX,highX,LowY,HighY,GridX,GridY)
!			 write (*,*) 'siberia-compute here 3o',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             CALL Layer_Sedimentation(SimTime,Z,Erosion,Domain,Direct,GridX,GridY,LowX,HighX,LowY,HighY,tempB1,tempAge)
!			 write (*,*) 'siberia-compute here 3p',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
             TimerAfter(6)=TimerGet()
             TimerNo(6)=TimerNo(6)+1
             TimerSum(6)=TimerSum(6)+TimerAfter(6)-TimerBefore(6)
           ELSE !  if no corrector step 
             CALL Layer_Uplift(Uplift,lowX,highX,LowY,HighY,GridX,GridY)
             tempb1=simparameters%b1
             tempage=1
             CALL Layer_Sedimentation(SimTime,Z,Erosion,Domain,Direct,GridX,GridY,LowX,HighX,LowY,HighY,tempB1,tempAge)
           END IF
!			 write (*,*) 'siberia-compute here 3q',z(164,10),erosion%data(164,10),erosionc%data(164,10),sed(164,10),sedc(164,10)
!             write (*,*) 'siberia-compute here 5'
           tottime=tottime+InitTimeStep1
! 
!  Loop for each discretisation within a unit time
! 
         TimerNo(7)=TimerNo(7)+1
         TimerAfter(7)=TimerGet()
         TimerSum(7)=TimerSum(7)+TimerAfter(7)-TimerBefore(7)
         END DO     ! END of loop for iii=1,Discrete
!  =============================================
!  Stuff TO be done at the END of each unit time 
!  =============================================
         IF (SimParameters%RunTime <= -3) THEN
           RunDone=Termination(SimParameters%RunTime,iTime,Z                       &
&                     ,LowX,HighX,LowY,HighY,Domain,IrregularBoundary              &
&                     ,gridX,gridY)
         END IF
!  output a file for one of two cases
!    1. IF the time during the run matches of the times for which output is requested
!    2. IF the automatic termination criteria above is triggered AND output for
!       any time is required ... NB this means that IF you ONLY want a file for
!       termination time and nothing ELSE you HAVE TO input a dummy time in the 
!       request for output files at the start of the PROGRAM (I suggest a very
!       large number bigger than the run time).
! 
         IF ((notime > 0 .and. iTime == time(itmptr)) .or.                            &
&            (RunDone.and.notime > 0)) THEN
! 
!  Ensure that directions and areas are consistent with updated elevations
! 
           itmptr=itmptr+1
           CALL SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
           CALL ChannelAnal(cDepth,Area,s0,Z,Y,SimTime,Domain                      &
&                  ,IrregularBoundary,DirChg,Hill_Channel_Factor,DetCIF            &
&                  ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
           CALL DirAnal(Z,Direct,DirectDinf,s0,y,cDepth,DirChg,iself               &
&                      ,Domain,IrregularBoundary,iInit,iXXX,iYYY,DirWeights        &
&                      ,LowX,HighX,LowY,HighY,SimParameters,gridX,gridY)
           IF (DirChg) THEN
             CALL AreaAnal(Area,Direct,DirectDinf,DirWeights                       &
&                    ,Domain,AreaError,IrregularBoundary                           &
&                    ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS             &
&                    ,SimParameters,GridX,GridY)
           END IF
           CALL FAppend(FileName,Outfile,LgthOutFileName                           &
&                ,iTime+Start,OutputFileExt)
           if (.not.File_Exist(.not.RST_Overwrite,trim(filename))) then
             CALL RSTOut1(RanField,Y,Z,Area,FileName                                 &
&                   ,iInit,iXXX,iYYY,Direct,iTime+Start                            &
&                   ,IrregularBoundary,Domain,s0,DetCIF                            &
&                   ,Version,OutputFileExt,iscale,er,nr,Sed,cDepth                 &
&                   ,SoilDepth,SimParameters,XYZOutput,gridX,gridY)
             CALL FAppend(FileName,Outfile,LgthOutFileName                           &
&                   ,iTime+Start,'.rsu ')
             CALL RSUOut(FileName,Sed,Area,Direct,Domain                             &
&                   ,Z,Zoriginal,s0,IrregularBoundary                              &
&                   ,NoFlowIn,FlowInIJ,FlowInAS,InitTimeStep1                      &
&                   ,LowX,HighX,LowY,HighY,DirWeights,SimParameters                &
&                   ,Discharge,Erosion,SM                                          &
&                   ,BedrockZ,BedRockArea,BedRockSlope,BedrockDirections           &
&                   ,Predictor_sed,Corrector_sed,GridX,GridY)
             CALL FAppend(FileName,Outfile,LgthOutFileName                 &
&                    ,iTime+Start,'')
             CALL Layer_OutputData(Filename,Z,GridX,GridY                  &
&                     ,LowX,HighX,LowY,HighY,SimParameters)
           else
             CALL Message_Output(Message_ErrorStop,'RST output file already exists = '      &
&                                //trim(filename))
           end if
         END IF
         IF (RunDone) THEN
           iTotal=iTime
           GO TO 99
         END IF
! ================================================
!  END of stuff TO be done at END of each unit time
! ================================================
      END DO    ! END of loop ip=1,InnerLoop
      iTotal=iTotal+InnerLoop
      IF (SimParameters%RunTime > 0) THEN
        InnerLoop=min(abs(SimParameters%StatsTime)                                 &
&                    ,max(0,SimParameters%RunTime-iTotal))
      END IF
      GO TO 3
! 
! ======================
!  STOP the PROGRAM
! ======================
! 
  99  SimParameters%RunTime=0 
      CALL CtrOut(CIF,Z,Area,Y,s0,iTotal,isteps,Direct,Sed                          &
&                ,dZdX2,Soutflow,IrregularBoundary                                  &
&                ,Domain,iTime,ZMassA,ZmassP,Zin,Zout,DetCIF                        &
&                ,SumCycle,InitTimeStep1,iInit,iXXX,iYYY                            &
&                ,SumSed,NoRegions,SoilDepth                                        &
&                ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ                           &
&                ,Factor,DischargeM1,MaxSt2,MaxDz2,MeanSt                           &
&                ,Hill_Channel_Factor,SimParameters,MCRealNo,gridX,gridY)
! 
  RETURN
END SUBROUTINE compute

END MODULE SiberiaCompute
