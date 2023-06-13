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
PROGRAM SIBERIA
  USE Control
  USE SiberiaConstants
  USE InputOutput
  USE CtrOutput
  USE LayerSupport
  USE LayerConstants
  USE SiberiaTypes
  USE ModelParameters
  USE Support
  USE SetUp
  USE SiberiaCompute
  USE InputOutputStreams
  USE MonteCarlo
  USE Others
  USE openMPsupport
! 
! 
  IMPLICIT NONE
! 
  INTEGER :: iXXX(NoOutlets),iYYY(NoOutlets),time(ncont)                      &
&         ,notime,LgthOutFileName,LowX,HighX,LowY,HighY                 &
&         ,LgthRSTFileName,Start,i,j,k,iranz,iInit         &
&         ,izk,Regions(MaxRegions),NoRegions,RegionMap(MaxRegions)            &
&         ,NoFlowIn,FlowInIJ(2,MaxFlowIn),itemp,TimerNo(10)        &
&         ,MCNoSims,SimLgthOutFileName,MCRandomSeed,ErrorNo              &
&         ,GridSizeX,GridSizeY
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: Direct,DirectDinf,Region
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: Sim_Direct
  REAL(KIND(0.0D0)) :: dyc,FlowInAS(2,MaxFlowIn)           &
&         ,TimerSum(10),TimerAfter(10),TimerBefore(10),TimerTotal,junk        &
&         ,InRSTVersion,InitialAge
  REAL(KIND(0.0D0)),DIMENSION(:,:),ALLOCATABLE ::                             &
&          Z,DischargeM1,DirWeights,Area,CIF,RanField,Y,cDepth,s0,Sed,dZdX2   &
&         ,Dsed,SoilDepth,SoilZ,Erode_m1,Erode_n1,Discharge                   &
&         ,Hill_Channel_Factor
  REAL(KIND(0.0D0)),DIMENSION(:,:),ALLOCATABLE ::                             &
&          Sim_Area,Sim_cDepth,Sim_CIF                                        &
&         ,Sim_S0,Sim_RanField,Sim_Y,Sim_Z,Sim_SoilDepth
  LOGICAL :: InRSTFile,DirChg,TwoFluvialProcesses,OK                            &
&         ,IrregularBoundary,FlipLR,FlipTB,SlopeMax,DetCIF,CommandFile        &
&         ,MCRSTFileListExist,MCb1FileListExist,MCb3FileListExist             &
&         ,MCUpliftFileListExist,MCOthersFileListExist,SedimentTracking       &
&         ,SkipBC,IrregularBoundaryFile
  LOGICAL,DIMENSION(:,:),ALLOCATABLE :: Domain
!  CHARACTER(80)  :: RSTfile,Outfile, BoundFile,line,SimOutFile,Title
  CHARACTER(1000)  :: RSTfile,Outfile, BoundFile,line,SimOutFile,Title
  TYPE(LocalParameters),POINTER :: Sim_Parameters => NULL()
  TYPE(LocalParametersArray),POINTER :: OriginalParameters => NULL()
  TYPE(MCParaCharacArray) :: MCParaCharac
  INTEGER,DIMENSION(:),ALLOCATABLE :: MCSeeds
  TYPE(ArrayR8XY) :: Sim_Erosion,Sim_ErosionC,Sim_SM,Sim_BedrockZ,Sim_BedRockArea          &
&         ,Sim_BedRockSlope,Sim_FlowB1,Sim_FlowAge,Sim_PotErosion,Sim_PotErosionC
  TYPE(ArrayIXY) :: Sim_BedrockDirections
!  TYPE(MCParaCharacArray),ALLOCATABLE :: MCParaCharac
! 
!  openMP variables
  INTEGER :: NoProcessors
! 
! New variables required for Mindraft file
  REAL(KIND(0.0D0)) ::    er(2), nr(2)
  INTEGER ::   iscale
  CHARACTER(5) :: OutputFileExt
! 
  DATA iscale/1/

!                          INITIALISATION
!    ==================================================================
!   initialise the internal variables across the whole domain (not just
!   the computational domain) so we don't accidentally crash inout.f 
!   with bogus values outside the computational domain when outputing 
!   .rst2 and .rsu files

  DATA InRSTFile     / .false. /     ! is there a RST input file?
!  filenames
  INTEGER,PARAMETER :: FileArraySize=MaxUser*80
  DATA RSTfile   / ' ' /          ! RST2 filename
  DATA Boundfile / ' ' /          ! Boundary filename
  DATA OutFile   / ' ' /          ! output DATA generic filename
! 
  DATA dyc       / 0.9 /
  DATA DirChg    / .true. /       ! directions changed from last interation?
  DATA DetCIF    /  .true. /      ! deterministic channels
  DATA slopeMax  / .false. /      ! max slope set in diffusion?
  DATA NoRegions / 0 /             ! no regions input
  DATA NoFlowIn  / 0 /             ! no of nodes with specified flow input
  DATA iInit          / 0 /        ! no of fixed elevation nodes
  DATA OutputFileExt  / '.rst2' /  ! file extension for the DEFAULT output file TYPE
  DATA FlipLR,FlipTB  / .false., .false. /  ! flip the DATA left TO right, top TO Bottom on input
  DATA SedimentTracking  / .false. /  ! whether we track sediment characteristics for layering, etc

!                       END OF INITIALISATION
! ===================================================================
    call DefaultDirectory()
    NoProcessors=1
!$    NoProcessors=omp_get_num_procs()
    No_Threads=1
    CALL Layer_Init(ErrorNo)
    CALL openMP_Init()
    call Message_Init(EchoFileClose,' ',ErrorFileStart,'SiberiaErrors.txt')
    DO i=1,10
      TimerSum(i)=0.0
      TimerNo(i)=0
    END DO
! 
    CALL SiberiaSetup
    call Message_Set(Message_DecPt,2)
    CALL Message_Output(Message_Info,'')
    if (debug) then
      CALL Message_Output(Message_Info,'### WARNING ###      DEBUG VERSION        ### WARNING ###')
      CALL Message_Output(Message_Info,'### WARNING ### USING SIBERIA.DEBUG.SETUP ### WARNING ###')
      CALL Message_Output(Message_Info,'')
    end if
    IF (NoProcessors == 1) THEN
      CALL Message_Output(Message_Info,' Scalar SIBERIA  V',Version)
      CALL Message_Output(Message_Info,' ---------------------')
    ELSE
      IF (No_Threads > NoProcessors) No_Threads=NoProcessors
      CALL Message_Output(Message_Info,' Parallel SIBERIA  V',Version)
      CALL Message_Output(Message_Info,' -----------------------')
      CALL Message_Output(Message_Info,'   -- Available Processors = ',NoProcessors)
      CALL Message_Output(Message_Info,'   -- Available Threads    = ',No_Threads)
!$    CALL omp_set_num_threads(No_Threads)
    END IF
    CALL Message_Output(Message_Info,' Copyright 1994-2006, G. R. Willgoose, All rights reserved')
    CALL Message_Output(Message_Info,'')
    call Message_Set(Message_DecPt,0)
!    CALL checklicense
! 
!                    DATA Input
!                   ============
! 
    CALL RunDataInput(RSTfile,LgthRSTFileName,InRSTFile                        &
&              ,CommandFile,BoundFile                                       &
&              ,IrregularBoundaryFile,NoTime,Time,Outfile,LgthOutFileName       &
&              ,OutputFileExt,Start,IRANZ,iInit,iXXX,iYYY)
    IrregularBoundary=IrregularBoundaryFile
! 
! ===========================================================
!                          Let's GO
! ===========================================================
    IF (InRSTFile) THEN
!      CALL ReadIn(s0,RanField,y,Z,Area,GridSizeX,GridSizeY                  &
!&           ,RSTfile,iInit,iXXX,iYYY,Direct,cDepth,SoilDepth                &
!&           ,FlipLR,FlipTB,LowX,HighX,LowY,HighY)
      OK=ReadHeader(RSTfile,InRSTVersion,iInit)
      if (.not.OK) then
        CALL Message_Output(Message_ErrorStop,'Unable to read header of RST file')
      end if
    END IF
    IF (IrregularBoundaryFile) THEN
      call InputBNDHeader(BoundFile,kx,ky,LowX,HighX,LowY,HighY)
    end if
! 
!  Reset any parameters that need TO be set
! 
 150 CALL SetParameters
! 
!  Reset things that may be changed by a changes in parameters or boundary values
! -------------------------------------------------------------------------------
!   Slope threshold in diffusive transport
    IF (s0max <= 0.0) THEN
      SlopeMax=.false.
      CALL Message_Output(Message_Info,' -- Slope Threshold mode ON')
    ELSE
      SlopeMax=.true.
      CALL Message_Output(Message_Info,' -- Slope Threshold mode OFF')
    END IF
!   stochastic channels
    IF (b3SDl == 0.0) THEN
      DetCIF=.true.
      CALL Message_Output(Message_Info,' -- Deterministic channels ON')
    ELSE
      DetCIF=.false.
      CALL Message_Output(Message_Info,' -- Stochastic channels ON')
    END IF
!  two fluvial sediment transport processes
    IF (b12 /= 0.0) THEN
      TwoFluvialProcesses=.true.
      CALL Message_Output(Message_Info,' -- Two Fluvial Processes ON')
    ELSE
      TwoFluvialProcesses=.false.
      CALL Message_Output(Message_Info,' -- Single Fluvial Process ON')
    END IF
!  set statistics times
    IF (RunTime > 0 ) THEN
      StatsTime=sign(min(abs(StatsTime),RunTime),StatsTime)
    END IF
!  make sure timesteps are integral divisor of 1 (round TO nearest value).
    IF (InitTimeStep > 0.0) THEN
      itemp=1/InitTimeStep+0.5
      InitTimeStep=1.0/itemp
    END IF
!  turn on sediment tracking
!    IF (FilenameUser(FileLayers)(1:10) /= '          ') then
    IF (mod(ModeErode,20)==4) then
      SedimentTracking=.true.
      call Message_Output(Message_Info,' -- Sediment Tracking and Layers ON')
    ELSE
      SedimentTracking=.false.
      call Message_Output(Message_Info,' -- Sediment Tracking and Layers OFF')
    END IF
! Set boundary values for Area (used in Mindraft output)
    er(1) = 0.0d0
    er(2) = kx
    nr(1) = 0.0d0
    nr(2) = ky
! 
!  END of things that might be changed by a change in PARAMETER or boundary values
! ---------------------------------------------------------------------------------
! 
!  check TO see IF any control info that modifies is input but Don't crap
!  out IF the control file doesn't exist ... the user may not yet have input it
      CALL InputControlFile(RunTime,0)
! 
!   Carry on with the calcs
! 
  50  WRITE(*,*) ' ? 0=New Parameters, 1=Start [DEFAULT => Start]'
      line=' '
      READ(InputStream,6000,ERR=9000) line
 6000 format(a)
      if (line(1:10) == '          ') then
        izk=1
      else
        READ(line,*,ERR=9000,END=9001) izk
      end if
      IF (izk == 0) GO TO 150

!                       Calculations Proper
!                      =====================

      IF (CommandFile) THEN
        CLOSE(UNIT=InputStream,STATUS='keep')
      END IF
!                       Calcs Initialisation
!                      ----------------------

! Init memory for boundary input
      GridSizeX=kx+1
      GridSizeY=ky+1
      call Message_Output(Message_Info,' -- Setting computational gridsize =(',gridsizex,'x',gridsizey,')')
      allocate(Region(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Region','SIBERIA_MAIN')
      allocate(Domain(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Domain','SIBERIA_MAIN')

!   input the boundary file

      IF (IrregularBoundaryFile) THEN
        CALL InputBoundaries(BoundFile,Domain,GridSizeX,GridSizeY,kx,ky           &
&              ,iInit,iXXX,iYYY,Regions,NoRegions,MaxRegions,Region               &
&              ,RegionMap,LowX,HighX,LowY,HighY)
        IF (LowX < 2.or.LowY < 2) THEN
          CALL Message_Output(Message_WarnContinue,'DEM data too close to '       &
&                 //'boundary --- Resetting Lower Boundaries to 2')
          LowX=max(LowX,2)
          LowY=max(LowY,2)
		  do i=1,kx+1
		    domain(i,1)=.false.
		  end do
		  do j=1,ky+1
		    domain(1,j)=.false.
		  end do
        END IF
      ELSE
        LowX=2
        HighX=kx
        LowY=2
        HighY=ky
!  many operators in the code no longer test for irregular boundaries
!  and assume domain is set correctly. NB: In the Monet Carlo compute loops below
!  Irregular boundaries are repeatidly input because RST2 input overwrites 
!  boundaries definitions but forregular boundaries this is not a problem
!  so setting domain here ONCE ONLY is OK. 
        domain=.false.
        do j=lowy,highy
          do i=lowx,highx
            domain(i,j)=.true.
          end do
        end do
        IrregularBoundary=.true.
        IrregularBoundaryFile=.false.
        IF (iInit <= 0) THEN
          CALL Message_Output(Message_ErrorStop,'This RST2 file must be used '    &
&                  //'with a boundary file')
        END IF
      END IF
!  input stuff that needs TO be input at the start of the run
!  input control file info and IF the control filename doesn't exist then crap out
      CALL InputControlFile(RunTime,1)

!  the following lines can only be executed once
!  --------------------------------------------- 
!
!   Initialise memory
      allocate(Direct(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Direct','SIBERIA_MAIN')
      allocate(z(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Z','SIBERIA_MAIN')
      allocate(DischargeM1(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('DischargeM1','SIBERIA_MAIN')
      allocate(Area(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Area','SIBERIA_MAIN')
      allocate(CIF(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('CIF','SIBERIA_MAIN')
      allocate(RanField(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('RanField','SIBERIA_MAIN')
      allocate(Y(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Y','SIBERIA_MAIN')
      allocate(cDepth(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('cDepth','SIBERIA_MAIN')
      allocate(s0(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('s0','SIBERIA_MAIN')
      allocate(Sed(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Sed','SIBERIA_MAIN')
      allocate(dZdX2(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('dZdX2','SIBERIA_MAIN')
      allocate(Dsed(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Dsed','SIBERIA_MAIN')
      allocate(SoilDepth(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('SoilDepth','SIBERIA_MAIN')
      allocate(SoilZ(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('SoilZ','SIBERIA_MAIN')
      allocate(Erode_m1(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Erode_m1','SIBERIA_MAIN')
      allocate(Erode_n1(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Erode_n1','SIBERIA_MAIN')
      allocate(Discharge(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Discharge','SIBERIA_MAIN')
      allocate(Hill_Channel_Factor(GridSizeX,GridSizeY),stat=ErrorNo)                        
      IF (ErrorNo /= 0) CALL AllocationError('Hill_Channel_Factor','SIBERIA_MAIN')
      direct=0
      area=0
      cDepth=0
      CIF=0
      RanField=0
      Y=0
      Z=0
      Sed=0
      Dsed=0
      dZdX2=0
      SoilDepth=0
      Erode_m1=0
      Erode_n1=0
      Discharge=0
      Dischargem1=0
!   variables for dinfinity
      if (ModeDir == 7 .or. ModeDir == 8) then
        allocate(DirectDinf(GridSizeX,GridSizeY),stat=ErrorNo)                        
        IF (ErrorNo /= 0) CALL AllocationError('DirectDinf','SIBERIA_MAIN')
        allocate(DirWeights(GridSizeX,GridSizeY),stat=ErrorNo)                        
        IF (ErrorNo /= 0) CALL AllocationError('DirWeights','SIBERIA_MAIN')
        DirectDinf=0
        DirWeights=0
      end if
! 
!   Initialise states in the computational domain
! 
      IF (InRSTFile) THEN
        if (IrregularBoundaryFile) then
          SkipBC=.true.
          CALL ReadBody(s0,RanField,y,Z,Area,GridSizeX,GridSizeY                  &
&           ,RSTfile,iInit,iXXX,iYYY,Direct,cDepth,SoilDepth                &
&           ,FlipLR,FlipTB,LowX,HighX,LowY,HighY,SkipBC)
        else
          SkipBC=.false.
          CALL ReadBody(s0,RanField,y,Z,Area,GridSizeX,GridSizeY                  &
&           ,RSTfile,iInit,iXXX,iYYY,Direct,cDepth,SoilDepth                &
&           ,FlipLR,FlipTB,LowX,HighX,LowY,HighY,SkipBC)
        end if
        LowX=max(LowX,2)
        LowY=max(LowY,2)
      ELSE
        DO j=LowY-1,HighY+1
          DO i=LowX-1,HighX+1
            IF ((IrregularBoundary.and.Domain(i,j))                            &
&             .or.(.not.IrregularBoundary)) THEN
              RanField(i,j)=FRanMn*(1.0+FRanCV*(Ran2(iranz)-0.5))
              CIF(i,j)=0.001
              y(i,j)=0.001
              IF (modeIC == 0) THEN
                Z(i,j)=SInit+FRanZ*(Ran2(iranz)-0.5)
              ELSE IF (modeIC == 1) THEN
                Z(i,j)=SInit*(i-1)/float(kx)+FRanZ*(Ran2(iranz)-0.5)
              END IF
              Area(i,j)=1
            ELSE
              RanField(i,j)=0.0
              CIF(i,j)=0.0
              y(i,j)=0.0
              Z(i,j)=0.0
              Area(i,j)=0
            END IF
            Dsed(i,j)=1.0
            dZdX2(i,j)=0.0
            s0(i,j)=0.0
            Sed(i,j)=0.0
            Direct(i,j)=5
            cDepth(i,j)=0
            if (modesoil /= 0) then
              SoilDepth(i,j)=1
            end if
          END DO
        END DO
        DO i=1,iInit
          y(abs(iXXX(i)),abs(iYYY(i)))=YFix
        END DO
      END IF
      InitialAge=0.0
      CALL TimerInit
      CALL TimerShow(' START')
! 
! =====================================
!    Get down TO the Calcs
! =====================================
! 
      SELECT CASE (ModeMC)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorStop                                  &
&                 ,'Invalid Monte-Carlo Mode: ModeMC = ',modeMC)
! 
! =====================================
!    Calculation Loop (Single Run)
! =====================================
! 
      CASE (0)
        k=1
        CALL Inout_Init(k)
        CALL openMP_Init
        ALLOCATE(Sim_Parameters,stat=ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Sim_Parameters','SIBERIA_MAIN')
        CALL InitSimParameters(Sim_Parameters)
        CALL PhysicalStates_Sim(S0,Area,Sim_Parameters,GridSizeX,GridSizeY)
        if (SedimentTracking) then
          CALL Layer_Set(Layer_Module_Active,On,ErrorNo)
          CALL Layer_InputData(Sim_Parameters,Z,GridSizeX,GridSizeY                    &
&                ,LowX,HighX,LowY,HighY)
        else
          CALL Layer_Set(Layer_Module_Active,Off,ErrorNo)
        end if
        k=0
!  allocate work arrays that depend on model options
        call AllocateArray(Sim_Erosion,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Sim_Erosion','SIBERIA_MAIN')
        Sim_Erosion%data=0
        call AllocateArray(Sim_ErosionC,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Sim_ErosionC','SIBERIA_MAIN')
        Sim_ErosionC%data=0
        call AllocateArray(Sim_SM,LowX,HighX,LowY,HighY,ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('Sim_SM','SIBERIA_MAIN')
        Sim_SM%data=0
        if (SedimentTracking) then
            call AllocateArray(Sim_FlowB1,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_FlowB1','SIBERIA_MAIN')
            Sim_FlowB1%data=b1
            call AllocateArray(Sim_FlowAge,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_FlowAge','SIBERIA_MAIN')
            Sim_FlowAge%data=0
            call AllocateArray(Sim_PotErosion,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_PotErosion','SIBERIA_MAIN')
            Sim_PotErosion%data=0
            call AllocateArray(Sim_PotErosionC,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_PotErosionC','SIBERIA_MAIN')
            Sim_PotErosionC%data=0
        end if
        CALL compute(Area,cDepth,CIF,S0,RanField,Y,Z,sed,dsed,dZdX2,SoilDepth        &
&             ,SoilZ,Erode_m1,Erode_n1,Discharge,DischargeM1,Hill_Channel_Factor     &
&             ,Direct,DirectDinf,DirWeights,Domain,IrregularBoundary                 &
&             ,GridSizeX,GridSizeY,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ           &
&             ,FlowInAS,NoRegions,Regions,RegionMap,Region,OutFile,LgthRSTFileName   &
&             ,OutputFileExt,Start,LgthOutFileName,iscale,er,nr,NoTime,Time          &
&             ,DetCIF,DirChg,TwoFluvialProcesses,SlopeMax,dyc,iInit,iXXX      &
&             ,iYYY,Sim_Parameters,TimerNo,TimerSum,TimerAfter,TimerBefore,k         &
&             ,Sim_Erosion,Sim_ErosionC,Sim_SM,Sim_BedrockZ,Sim_BedRockArea,Sim_BedRockSlope      &
&             ,Sim_BedrockDirections,SedimentTracking,Sim_FlowB1,Sim_FlowAge     &
&             ,Sim_PotErosion,Sim_PotErosionC)
        DEALLOCATE(Sim_Parameters)
        CALL DeAllocateArray(Sim_Erosion,ErrorNo)
        CALL DeAllocateArray(Sim_SM,ErrorNo)
! 
! =====================================
!    Calculation Loop (Monte-carlo)
! =====================================
! 
      CASE (2)
!        ALLOCATE(MCParaCharac)  
        CALL MC_Init(MCNoSims,MCRandomSeed,MCRSTFileListExist                      &
&         ,MCb1FileListExist,MCb3FileListExist,MCUpliftFileListExist               &
&         ,MCOthersFileListExist,MCParaCharac)
! must set up random seeds before parallel section otherwise the random numbers
! will vary depending on the random order in which threads are executed
        ALLOCATE(MCSeeds(MCNoSims),stat=ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('MCSeeds','SIBERIA_MAIN')
        DO i=1,MCNoSims
!  2000 should be big enough TO STOP recurrences
          DO j=1,2000
            junk=ran2(MCRandomSeed)
          END DO
          MCSeeds(i)=MCRandomSeed
        END DO
        ALLOCATE(OriginalParameters,stat=ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('OriginalParameters','SIBERIA_MAIN')
        CALL MC_SaveParameters(OriginalParameters)
!$omp   parallel DO DEFAULT(NONE)                                                  &
!$omp     ,PRIVATE(Sim_area,Sim_cdepth,Sim_CIF,Sim_S0,Sim_RanField                 &
!$omp        ,Sim_Y,Sim_Z,Sim_SoilDepth,Sim_Direct,Sim_Parameters,k,j,ErrorNo      &
!$omp        ,Sim_Erosion,Sim_ErosionC,Sim_SM,Sim_BedrockZ,Sim_BedRockArea         &
!$omp        ,Sim_BedRockSlope,Sim_BedrockDirections,Sim_FlowB1,Sim_FlowAge)       &
!$omp     ,lastprivate(TimerNo,TimerSum,TimerAfter,TimerBefore)                    &
!$omp     ,PRIVATE(wrtoutunit,RSTFile,title,lgth)                                  &
!$omp     ,firstprivate(i,sed,dsed,dZdX2,SoilZ,Erode_m1,Erode_n1,Discharge         &
!$omp       ,DischargeM1,Hill_Channel_Factor,DirectDinf,DirWeights,NoFlowIn        &
!$omp       ,FlowInIJ,FlowInAS,NoRegions,Regions,RegionMap,Region,SimOutFile       &
!$omp       ,LgthRSTFileName,OutputFileExt,Start,SimLgthOutFileName,iscale         &
!$omp       ,er,nr,NoTime,Time,DetCIF,DirChg,TwoFluvialProcesses,SlopeMax          &
!$omp       ,dyc,inRSTFile,iInit,iXXX,iYYY,LowX,HighX,LowY,HighY,Domain)           &
!$omp     ,shared(IrregularBoundary,LgthOutFileName                                &
!$omp       ,Outfile,MCNoSims,MCParaCharac,MCSeeds,CIF,FlipLR,FLipTB,BoundFile     &
!$omp       ,kx,ky,OriginalParameters,Area,cDepth,S0,RanField,Y,Z,SoilDepth        &
!$omp       ,Direct)                                                               &
!$omp     ,copyin(/ openMPvarsI /, / openMPvarsR /,/ openMPvarsL /)
        DO k=1,MCNoSims
          CALL Inout_Init(k)
          CALL openMP_Init
!  allocate arrays that are always required
          ALLOCATE(Sim_Area(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_Area','SIBERIA_MAIN')
          ALLOCATE(Sim_cDepth(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_cDepth','SIBERIA_MAIN')
          ALLOCATE(Sim_CIF(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_CIF','SIBERIA_MAIN')
          ALLOCATE(Sim_S0(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_S0','SIBERIA_MAIN')
          ALLOCATE(Sim_RanField(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_RanField','SIBERIA_MAIN')
          ALLOCATE(Sim_Y(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_Y','SIBERIA_MAIN')
          ALLOCATE(Sim_Z(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_Z','SIBERIA_MAIN')
          ALLOCATE(Sim_SoilDepth(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_SoilDepth','SIBERIA_MAIN')
          ALLOCATE(Sim_Direct(GridSizeX,GridSizeY),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_Direct','SIBERIA_MAIN')
          IF (MCParaCharac%RST2FileList%SampleMode > 0) then
            CALL MCSimRST2FileName_Init(RSTFile,MCParaCharac,k,MCSeeds(k))
            CALL ReadIn(Sim_s0,Sim_RanField,Sim_Y,Sim_Z,Sim_Area                   &
&                  ,GridSizeX,GridSizeY,RSTfile,iInit,iXXX,iYYY,Sim_Direct         &
&                  ,Sim_cDepth,Sim_SoilDepth,FlipLR,FlipTB,LowX,HighX,LowY,HighY)
            IF (IrregularBoundaryFile) THEN
              CALL InputBoundaries(BoundFile,Domain,GridSizeX,GridSizeY,kx,ky      &
&                  ,iInit,iXXX,iYYY,Regions,NoRegions,MaxRegions,Region            &
&                  ,RegionMap,LowX,HighX,LowY,HighY)
            end if
            DO j=1,HighY
              DO i=1,HighX
                Sim_CIF(i,j)=CIF(i,j)
              END DO
            END DO
          else
            DO j=1,HighY
              DO i=1,HighX
                Sim_Area(i,j)=Area(i,j)
                Sim_cDepth(i,j)=cDepth(i,j)
                Sim_CIF(i,j)=CIF(i,j)
                Sim_S0(i,j)=S0(i,j)
                Sim_RanField(i,j)=RanField(i,j)
                Sim_Y(i,j)=Y(i,j)
                Sim_Z(i,j)=Z(i,j)
                Sim_SoilDepth(i,j)=SoilDepth(i,j)
                Sim_Direct(i,j)=Direct(i,j)
              END DO
            END DO
          end if
          ALLOCATE(Sim_Parameters,stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_Parameters','SIBERIA_MAIN')
          CALL MCSimParameters_Init(Sim_Parameters,MCParaCharac,MCSeeds(k)             &
&                ,OriginalParameters)
          Title=' '
          call Str_AppendIntToStrCompact(trim(title),'  -- Monte-Carlo Parameters Sim='     &
&                   ,k)
          call CtrOut_Parameters(Title,Sim_Parameters)
          CALL PhysicalStates_Sim(Sim_S0,Sim_Area,Sim_Parameters,GridSizeX,GridSizeY)
          if (SedimentTracking) then
            CALL Layer_Set(Layer_Module_Active,On,ErrorNo)
            CALL Layer_InputData(Sim_Parameters,Sim_Z,GridSizeX,GridSizeY                    &
&                   ,LowX,HighX,LowY,HighY)
          else
            CALL Layer_Set(Layer_Module_Active,Off,ErrorNo)
          end if
          SimLgthOutFileName=LgthOutFileName+3
          CALL Str_AppendIntToStrCompact(trim(SimOutfile)                     &
&               ,Outfile(1:LgthOutFileName)//'-mc',k)
!  allocate work arrays that depend on model options
          call AllocateArray(Sim_Erosion,LowX,HighX,LowY,HighY,ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_Erosion','SIBERIA_MAIN')
          Sim_Erosion%data=0
          call AllocateArray(Sim_ErosionC,LowX,HighX,LowY,HighY,ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_ErosionC','SIBERIA_MAIN')
          Sim_ErosionC%data=0
          call AllocateArray(Sim_SM,LowX,HighX,LowY,HighY,ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError('Sim_SM','SIBERIA_MAIN')
          Sim_SM%data=0
          if (SedimentTracking) then
            call AllocateArray(Sim_FlowB1,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_FlowB1','SIBERIA_MAIN')
            Sim_FlowB1%data=Sim_Parameters%B1
            call AllocateArray(Sim_FlowAge,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_FlowAge','SIBERIA_MAIN')
            Sim_FlowAge%data=0
            call AllocateArray(Sim_PotErosion,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_PotErosion','SIBERIA_MAIN')
            Sim_PotErosion%data=0
            call AllocateArray(Sim_PotErosionC,LowX,HighX,LowY,HighY,ErrorNo)
            IF (ErrorNo /= 0) CALL AllocationError('Sim_PotErosionC','SIBERIA_MAIN')
            Sim_PotErosionC%data=0
          end if
!  do stuff
          CALL compute(Sim_Area,Sim_cDepth,Sim_CIF,Sim_S0,Sim_RanField,Sim_Y           &
&               ,Sim_Z,sed,dsed,dZdX2,Sim_SoilDepth,SoilZ,Erode_m1,Erode_n1            &
&               ,Discharge,DischargeM1,Hill_Channel_Factor,Sim_Direct,DirectDinf       &
&               ,DirWeights,Domain,IrregularBoundary,GridSizeX,GridSizeY               &
&               ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ,FlowInAS,NoRegions            &
&               ,Regions,RegionMap,Region,SimOutFile,LgthRSTFileName,OutputFileExt     &
&               ,Start,SimLgthOutFileName,iscale,er,nr,NoTime,Time,DetCIF,DirChg       &
&               ,TwoFluvialProcesses,SlopeMax,dyc,iInit,iXXX,iYYY               &
&               ,Sim_Parameters,TimerNo,TimerSum,TimerAfter,TimerBefore,k              &
&               ,Sim_Erosion,Sim_ErosionC,Sim_SM,Sim_BedrockZ,Sim_BedRockArea                       &
&               ,Sim_BedRockSlope,Sim_BedrockDirections,SedimentTracking               &
&               ,Sim_FlowB1,Sim_FlowAge,Sim_PotErosion,Sim_PotErosionC)
          DEALLOCATE(Sim_Parameters)
          DEALLOCATE(Sim_Area)
          DEALLOCATE(Sim_cDepth)
          DEALLOCATE(Sim_CIF)
          DEALLOCATE(Sim_S0)
          DEALLOCATE(Sim_RanField)
          DEALLOCATE(Sim_Y)
          DEALLOCATE(Sim_Z)
          DEALLOCATE(Sim_SoilDepth)
          DEALLOCATE(Sim_Direct)
          CALL DeAllocateArray(Sim_Erosion,ErrorNo)
          CALL DeAllocateArray(Sim_ErosionC,ErrorNo)
          CALL DeAllocateArray(Sim_SM,ErrorNo)
        END DO
!$omp   END parallel DO
        DEALLOCATE(OriginalParameters)
!        DEALLOCATE(MCParaCharac)
      END SELECT
! 
! ==============================
!        Wrap up
! ==============================
! 
      call timersetfileoutput(.false.,6)
      CALL TimerShowLong(' END')
      TimerTotal=TimerGetLong()
      Call Message_Set(Message_FieldWidth,10)
      Call Message_Set(Message_DecPt,5)
      CALL Message_Output(Message_Info,' ') 
      IF (No_Threads == 1) THEN
        CALL Message_Output(Message_Info,'                   CPU Timing Summary Output') 
      else
        CALL Message_Output(Message_Info,'        Last Thread CPU Timing Summary Output') 
      end if
      CALL Message_Output(Message_Info,'          Component       No     Time    % Total') 
      CALL Message_Output(Message_Info,' --------------------------------------------------') 
      CALL Message_Output(Message_Info,'  diranal Timed Loop = ',TimerNo(1),'  ',sngl(TimerSum(1))          &
&         ,'  ',sngl(100*TimerSum(1)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,' areaanal Timed Loop = ',TimerNo(2),'  ',sngl(TimerSum(2))          &
&         ,'  ',sngl(100*TimerSum(2)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,'  sedanal Timed Loop = ',TimerNo(3),'  ',sngl(TimerSum(3))          &
&         ,'  ',sngl(100*TimerSum(3)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,'   finite Timed Loop = ',TimerNo(5),'  ',sngl(TimerSum(5))          &
&         ,'  ',sngl(100*TimerSum(5)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,'  correct Timed Loop = ',TimerNo(6),'  ',sngl(TimerSum(6))          &
&         ,'  ',sngl(100*TimerSum(6)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,'  channel Timed Loop = ',TimerNo(8),'  ',sngl(TimerSum(8))         &
&         ,'  ',sngl(100*TimerSum(8)/TimerTotal),'%')
      CALL Message_Output(Message_Info,' soilanal Timed Loop = ',TimerNo(9),'  ',sngl(TimerSum(9))         &
&         ,'  ',sngl(100*TimerSum(9)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,' ') 
      CALL Message_Output(Message_Info,'    Total Timed Loop = ',TimerNo(7),'  ',sngl(TimerSum(7))         &
&         ,' ',sngl(100*TimerSum(7)/TimerTotal),'%') 
      CALL Message_Output(Message_Info,' ') 
      IF (OutputFileExt(1:4)  ==  '.raw') THEN
        CLOSE(UNIT=10, STATUS='keep')
      END IF
      CALL Message_Output(Message_Info,'-- END of SIBERIA run') 
      CALL done
      STOP
 9000 CALL Message_Output(Message_ErrorStop                                       &
&         ,'Error while Reading the Input data or Command File in SIBERIA_MAIN')
 9001 CALL Message_Output(Message_ErrorStop                                       &
&         ,'Premature end to the Command File in SIBERIA_MAIN') 
END PROGRAM SIBERIA
! 
! ====================================================================
!  echoing the Versions of all the main subroutines
! ====================================================================
! 
 SUBROUTINE Versions
   USE SiberiaConstants
   USE Setup
   USE InputOutput
   USE RSUOutput
   USE CtrOutput
   USE Support
   USE SedAnalysis
   USE DirAnalysis
   USE AreaAnalysis
   USE LayerSupport
   USE LayerConstants
   USE ChannelAnalysis
   USE SoilAnalysis
   USE Others
   USE MyModels
   USE UserDirAnalysis
   USE UserErosionAnalysis
   USE UserOtherAnalysis
   USE UserRunoffAnalysis
   USE UserUpliftAnalysis
   USE MonteCarlo
   
   IMPLICIT NONE
! 
     call Message_Output(Message_Info,'')
     call Message_Set(Message_DecPt,2)
     call Message_Output(Message_Info,                               &
&         ' Implementation Limits for SIBERIA V',Version)
 6000 FORMAT(a,f5.2)
     call Message_Output(Message_Info,                               &
&         ' ---------------------------------------------------')
     call Message_Output(Message_Info,                               &
&         '   Maxiumum number of Fixed Elevation nodes =',NoOutlets)
     call Message_Output(Message_Info,                               &
&         '   Maxiumum number of output files/run =',nCont)
     call Message_Output(Message_Info,                               &
&         ' ---------------------------------------------------')
     call Message_Output(Message_Info,'')
     call Message_Output(Message_Info,                               &
&         ' Versions of the internal modules within SIBERIA')
     call Message_Output(Message_Info,                               &
&         ' ---------------------------------------------------')
     call Message_Set(Message_DecPt,2)
     call Message_Output(Message_Info,'Area Analysis          V',AreaAnalysis_Version)

     call Message_Output(Message_Info,'Channel Analyis        V',ChannelAnalysis_Version)

     call Message_Output(Message_Info,'Ctr Output             V',CtrOutput_Version)

     call Message_Output(Message_Info,'Dir Analysis           V',DirAnalysis_Version)
 
     call Message_Output(Message_Info,'Input Output           V',InputOutput_Version)

     call Message_Output(Message_Info,'MyModels               V',MyModels_Version)

     call Message_Output(Message_Info,'RSU Output             V',RSU_Version)

     call Message_Output(Message_Info,'Sed Analysis           V',SedAnalysis_Version)

     call Message_Output(Message_Info,'Soil Analysis          V',SoilAnalysis_Version)

     call Message_Output(Message_Info,'Monte-Carlo Analysis   V',MonteCarlo_Version)

     call Message_Output(Message_Info,'Layer Modelling        V',Layer_Version)

     call Message_Output(Message_Info,'Others                 V',Others_Version)

     call Message_Output(Message_Info,'Support Library        V',Support_Version)

     call Message_Output(Message_Info,'User: Dir Analysis     V',UserDirAnalysis_Version)

     call Message_Output(Message_Info,'User: Erosion Analysis V',UserErosionAnalysis_Version)

     call Message_Output(Message_Info,'User: Runoff Analysis  V',UserRunoffAnalysis_Version)

     call Message_Output(Message_Info,'User: Uplift           V',UserUpliftAnalysis_Version)

     call Message_Output(Message_Info,'User: Other Models     V',UserOtherAnalysis_Version)
     call done()
END SUBROUTINE Versions

! ---------------------------------------------------------------------
!   make sure the window doesn't go away (if required) and clean up
!   any OPEN files in case this has been called from an emergency exit
! ---------------------------------------------------------------------

SUBROUTINE Done
  USE Setup
  USE Support
  IMPLICIT NONE
  CHARACTER(1) :: junk
  logical :: log
! 
      CALL Message_Close(error=log)
      IF (PauseAtEnd) THEN
        WRITE (*,*) 
        if (log) then
          write (*,*) ' --- Errors occurred during the SIBERIA run'
          write (*,*) '     Check SiberiaErrors.txt for more information'
          write (*,*)
        end if
        WRITE (*,*) 'CALCULATIONS DONE ... RETURN to exit'
        READ(*,1000) junk
 1000   FORMAT(a)
      END IF
      STOP
END SUBROUTINE Done

!--------------------------------------------------------------------
!  Set the default directory where all siberia run files can be found
!--------------------------------------------------------------------

subroutine DefaultDirectory()
  USE SiberiaConstants
  implicit none
    logical :: lexist
	integer,parameter :: unitno=20

	  DefDirectory=''
      INQUIRE(file=trim(DefDirectoryFilename), EXIST=lexist)
	  if (lexist) then
	    open(unit=unitno,file=trim(DefDirectoryFilename),err=9999)
		read(unitno,*,end=9999,err=9999) 
		read(unitno,6000,end=9999,err=9999) DefDirectory
6000    format(a)
        DefDirectory=trim(DefDirectory)
		close(unit=unitno,status='keep')
	  end if
9999  continue
      return
end subroutine DefaultDirectory

