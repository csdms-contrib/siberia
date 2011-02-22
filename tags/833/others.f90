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
MODULE Others

  REAL,PARAMETER :: Others_Version=8.17

CONTAINS 

! 
! ======================================================================
!  Routine for setting the parameters
! ======================================================================
! 
SUBROUTINE SetParameters()
  USE SiberiaConstants
  USE Multipliers
  USE ModelParameters
  USE Support
  USE Setup
  USE InputOutputStreams
  IMPLICIT NONE
!     
!                
!               Explanation of the PARAMETER names
!               ==================================
!           Some parameters have notations N/A. These
!       parameters are not used in the latest Version
!       of the code or have been added for compatability
!       with future Version of SIBERIA
! 
!                 INTEGER PARAMETERS
!                 ==================
!  PARAMETER               PARAMETER
!  No.  Name.             Explanation
!  --   ----              -----------
!   1:  RunTime     = total no of time steps TO solve for
!              -1 = stop when hyposmetric integral has converged
!              -3 = stop if hypsometric integral less than value in control file
!              -4 = stop if hypsometric integral greater than value in control file
!   2:  StatsTime   = line printer output of contours at this time increment
!              if -ve then only outputs the table of stats and NOT the contours
!              if +ve outputs both stats table and contours
!   3:  kx    = X DIMENSION of the rectangular Grid
!   4:  ky    = Y DIMENSION of the rectangular Grid
!   5:  modeIC = TYPE of initial condition TO be applied TO initial
!               elevations
!                 0  =  flat level surface with initial value = SInit
!                 1  =  surface sloping upwards in the +ve X direction
!                       with max height = SInit
!   6:  TimeUp= END of time for application of the tetconic uplift
!   7:  ModeSolver = mode of solution of the sediment transport relation
!              0 (disabled) => soln. for elevations by Taylor Series
!              1            => soln. of the physical transport equation
!              3            => as for ModeSolver=4 except the TwoFluvialProcesses
!                              occur everywhere (additive sediment transport processes)
!              4            => corrected Version of ModeSolver=5 with Correct
!                              BC implementation for Area=1. For TwoFluvialProcesses
!                              channel/hillslope processes switch (switching sediment transport proceses)
!              5            => soln. by explicit/analytic soln 
!                              (predictor/corrector) - nonlinear
!              6            => shear stress driven source limitation model
!              7            => equivalent TO ModeSolver=5 except there is no ersoion 
!                              at all on the hillslope
!              8            => simple analyical solution for detachment limitation.
!                   sediment tracking options (modeSolver >=10)
!                   -------------------------------------------
!             14            => equivalent TO ModeSolver=4 with sediment tracking
!   8:  ModeDir = mode of solution of the directions in the DirAnal routine
!              0 (DEFAULT) => directions as steepest Slope. no constraints on
!                             any drainage directions. Major upgrade in 8.17
!              1           => directions for the channels are writ in stone
!                             provided that channels drain into channels
!              2           => USE ModeDir=1 except READ in allowable drainage
!                             directions from a user defined file (used TO 
!                             model contour bank constraints on drainage)
!              3           => directions DO not change (set from RST2 file)
!              4           => Highly optimised version of ModeDir=0 (post 8.17)
!                             for RISC processers
!              5           => R8 implementation
!              6           => Average flow direction for a region rather just adjacent nodes
!              7           => Dinifinity implementation. As per Tarboton.
!              8           => Dinifinity implementation. Simpler than Tarboton.
!              9           => Diranal=0 for <=8.16 TO maintain compatability
!   9:  ModeUplift = mode of tectonic uplift perturbation
!              0 (DEFAULT) => No perturbation
!              4           => Sinusoidal with amplitude TAmp, Period Tper,
!                             and initial phase TPhase
!              5           => Square wave with parameters as for sinusoidal
!              6           => impulse uplift with height TAmp, Period Tper,
!                             and initial phase TPhase
!   10: ModeRandom = mode of Random perturbation using FRanCV
!              0 (DEFAULT) => No Perturbation
!              1           => *b1
!              2           => *b5 
! 
!   11: ModeErode = mode for USE in the user-defined FACTOR routine
!              0 (DEFAULT) => spatially constant FACTOR
!              1           => depth dependent armouring erodibility (1st used in J Bell BE thesis)
!              3           => spatially variable erodibility based on .RGN input
!              4           => erodibility on the basis of layers
!             +20          => erodibility with correction for physical Grid dimensions
! 
!   12: ModeRunoff = mode for USE in the user-defined RUNOFF routine
!              0 (DEFAULT) => spatially constant RUNOFF
!              1           => spatially variable runoff from a file of grid DATA (first
!                             used in Hancock (1998) PhD).
!              2           => known inflows from offsite (first used in J Bell BE thesis)
!              3           => spatially variable runoff based on .RGN input.
!              4           => runoff on the basis of layers
!              5           => runoff from thr distributed hydrology model
!             +20          => runoff with correction for physical Grid dimensions
!   13: ModeChannel = GULLY/Channel dimensions
!              0 (DEFAULT) => zero dimensions
!              1           => REAL width and depth power fns of Area
!              2           => REAL width and depth power fns of discharge
!              3           => CIF spatially variable based on .rgn input
!              4           => channel properties determined from the layer model
!              5           => simply ignore any variations in erodibility between 
!                             channel and hillslope
!                             (i.e. assume Ot=1 no matter what you sent Ot TO be).
!   14: ModeDP = mode for incorporation of DEPENDENT variables (no modes 
!                implemented TO date).
!   15: ModeMC = mode of MonteCarlo run generation
!              0           => DEFAULT deterministic run.
!              1           => multiple input .RST2 files for MC assessment of 
!                             initial conditions (only implmented in the PVM 
!                             version ... latest version 7.05).
!                             NO LONGER SUPPORTED
!              2           => Monte-Carlo runs based on input file definitions
!   16: DirReg = how large is the Region used TO calculate the drainage
!                directions relative TO using the adjacent points in 
!                ModeDir=6 (DEFAULT =1).
!   17: ModeSoil = the soil development model TO be used (work in progress)
!                  0 => no modelling
!                  1 => SM and depth model
!   18-20: idummy18-idummy20 = N/A
! 
! 
!                 REAL PARAMETERS
!                 ===============
!  21: dZ     = Diffusivity in sediment transport
!  22: dZn    = power of nonlinearity in the diffusion of sediment transport
!  23: dZHold = Threshold below which diffusion transport does not occur
!  24: QsHold = Threshold below which fluvial transport does not occur   
!  25: FactMx = The maximum value for FACTOR in SedAnal used in the
!              calculation of Ot.
!  26: FRanMn= Mean factor for random fluctuations (see also FRanCV)
!  27: 1/at  = factor on activator in differentiation equation
!              0.025/c1 = activator threshold, a
!                                               t
!  28: YFix  = The CIF allocated TO fixed Elevation points
!              0  =>  all fixed Elevation points are hillslope
!              1  =>  all fixed Elevation points are channels
!  29: FRanCV= coeff. of var for random fluctuations (see also FRanMn)
!  30: b3SDs = Standard Dev. for short term variations in the runoff rate
!              (used in variation of saturation from below Regions)
!  31: b3SDl = Standard Dev. for long term variations in the runoff rate
!              (used in variation of Channel head position)
!  32: TAmp  = Amplitude of the cyclic uplift
!  33: TPeriod = Period of the cyclic uplift (timesteps)
!  34: TPhase= Phase of the cyclic uplift at t=0
!  35: FRanZ = standard dev. factor for random fluctuations in the Elevation
!              initial conditions
! 
!  36:      N/A
! 
!  37: m3    = power on the Area in discharge
!  38: b3    = coefficient between discharge and Area in the sediment
!              transport formula
! 
!                         m   m     n
!                          3   1     1
!          Q  = b . (b .A   )    .S                (sediment transport
!           s    1    3                                formula )
! 
!
!  39: b1    = coefficient on the front of the sediment transport formula
!  40: m1    = power of the discharge in the sediment transport formula
!  41: n1    = power on the Slope in the sediment transport formula
! 
!  42: Bulk =  Bulk density of soil. Relates the mass rate of 
!               transport with the actual amount of Elevation change.
!  43: InitTimeStep  =  +ve  => time step size TO be used
!                       -ve  => % error criteria TO used in adaptive timestepping
!               
!                                                 
! 
!                           m  m     n
!                            3  5     5
!          a  =  b .  (b A )     . S    (Channel initiation equation)
!                 5     3
! 
!          PRIOR TO V6.34 the interpretation of this equation was
!             (NOTE parameters for Restart files for these early Versions
!                   are automatically adjusted TO comply with the
!                   interpretation above. see SUBROUTINE ReadIn)
! 
!                      m     n
!                       5     5
!          a  =  b . A   . S         (Channel initiation equation)
!                 5
! 
!
!  44: b5    = coefficient on CIF  
!  45: n5    = power on the Slope used in the CIF equation
!  46: SInit = for uniform initial Elevation gives starting value
!  47: m5    = power on Area in the CIF equation
!  48: YHold = threshold used in the determination of the drawdown in the
!              Channel when going from "overland" TO Channel.
!  49: Notch = time over which SInit Elevation change is applied TO the Notch
!  50: Cover = Vegetation Cover factor TO adjust b1 TO reflect veg effects 
!              on soil erodibility (ala USLE)
!  51: s0max = Maximum Slope that can be reached due TO landslide stability 
!              (DEFAULT=0.0 => inactive) 
!  52: DTime = factor TO adjust the time scale of Channel formation
!              timescale = 1/DTime
!  53: OTime = factor TO adjust the relative rates of overland TO 
!              Channel sediment transport adjustment
!  54: GridXY= the spacing of the XY Grid in m
!  55: East  = the easting of the bottom left hand corner of the Grid
!  56: North = the northing of the bottom left hand corner of the Grid
!  57: b6    = the coefficent in the Channel dimensions model
!  58: m6    = the power in the Channel dimensions model
! 
!                           m6
!              depth = b  A             A is used for ModeChannel=1
!                       6               Q is used for ModeChannel=2
! 
!  59: b12   =  b1 for a second fluvial sedimnt transport process
!               IF b12=0.0 THEN model does not USE a second process
!               DEFAULT b12=0
!  60: m12   =  m1 for a second fluvial sediment transport process (IF b12 <> 0)
!  61: SDRate = the rate PARAMETER for the soil development model
!  62: SDExp1 = the First exponent on the soil development model
!               SDModel=1 => exponent of soil mositure
!  63: SDExp2 = the second exponent on the soil development model
!               SDModel=1 => exponent on the soil depth
!  64: SMThreshold = the threshold for saturation excess runoff
!  65: SDSMWgt= minimum weathering rate when SM=0
!  66-70: rdummy45-rdummy50 = N/A
! 
!
!                        FileName PARAMETERS 
!                       =====================
!      These filenames are generically named FilenameUser(index 1..10) and
!      are used in the user defined modules in the model. The internal ModeSolver
!      they are used for determination of follow.
! 
!   -1:  Erosion     = Used in SedAnal TO determine the b1 in the fluvial
!                    erosion MODULE. Accessed in User-Erosion.
!   -2:  Runoff     = Used in SedAnal, CtrOut and Finite TO determine the 
!                    relationship for discharge at any point. Accessed in
!                    User-Runoff.
!   -3:  Uplift     = Used in SedAnal TO determine the tectonic uplift at any
!                    time. Used in User-Uplift.
!   -4:  Directions = Used in DirAnal for determine constraints on drainage
!                    directions TO determine slopes and areas. Used in 
!                    User-DirAnal.
!   -5:  MonteCarlo = DATA used when Monte-Carlo simulation is requested
!   -6:  Channels   = the channel model.
!   -7:  Layers     = The file in which layering and sediment tracking information is
!                     is stored. If this filename is not empty then sediment tracking 
!                     is automatically turned on irrespective of the contents of the
!                     file
!   -8:   N/A
!   -9:  Control    = Control File TO be used for instructions on how TO run the PROGRAM
!   -10: Others     = Used in SIBERIA main loop TO determine user-defined
!                    dependent variables that depend on SIBERIA output but
!                    DO not impact the operation of SIBERA except potentially
!                    through other user defined modules. Accessed in User-Others.
! 
!      INTEGER :: InputStream
!      COMMON / streams / InputStream
! 
      INTEGER :: is,i,itemp
      REAL(KIND(0.0D0)) :: temp
      character(40) :: line
      CHARACTER(255) :: message
! 
  5   call Message_Output(Message_Info,' New PARAMETER # ')
      line=' '
      READ(InputStream,6001,ERR=5) line
 6001 format(a)
      if (line(1:10) == '          ') then
        is=0
      else
        READ(line,*,ERR=5,END=9001) is
      end if
 10   IF (is == 0) GO TO 600
      IF (is < 0) GO TO 610
      IF (is <= 20) GO TO 80
      IF (is <= (nototal+1)) GO TO 100
      IF (is == (nototal+3)) STOP
      GO TO 600
! 
!   INTEGER :: Parameters
! 
  80  call Message_Output(Message_Info,ParameterTitles(is)//' Old =',IntVar(is)        &
&                     ,'  New =?  ')
      READ(InputStream,*,ERR=80,END=9001) itemp
! any checks inputs to make sure they are reasonable here
      IntVar(is)=itemp
      GO TO 5
! 
!  REAL Parameters
! 
 100  call Message_Output(Message_Info,ParameterTitles(is)//' Old ='               &
&            ,RealVar(is-20)*MultiplierRealVar(is-20),'  New =?  ')
      READ(InputStream,*,ERR=100,END=9001) temp
      RealVar(is-20)=temp/MultiplierRealVar(is-20)
      GO TO 5
! 
!   FileName Parameters
! 
 610  is=abs(is)
      call Message_Output(Message_Info,' Modify Model FileName #'       &
&         ,is,' '//FileTitles(is))
      call Message_Output(Message_Info,trim(FilenameUser(is)))
      READ (InputStream,7000,ERR=610,END=9001) FilenameUser(is)
 7000 FORMAT(a)
      GO TO 5
! 
!  =========================================================
!        Clean-up and RETURN TO the calling PROGRAM
!  =========================================================
! 
!  WRITE out the current values for the parameters
!     - INTEGERS
! 
 600  IF (Prompt) THEN 
        call Message_Output(Message_Info,' ')
        call Message_Output(Message_Info,' ') 
        call Message_Output(Message_Info,' INTEGER Parameters')
        call Message_Output(Message_Info,' ------------------')
        do i=1,20,2
          message=' '
          write (message,6000)          &
&                  i,': '//ParameterTitles(i)//'=',IntVar(i)             &
&               ,i+1,': '//ParameterTitles(i+1)//'=',IntVar(i+1)
 6000     format(10('   ',i2,a,i16))
          call Message_Output(Message_Info,trim(message))
        end do
! 
!     - REALS
! 
        call Message_Output(Message_Info,'')
        call Message_Output(Message_Info,' REAL Parameters')
        call Message_Output(Message_Info,' ---------------')
        do i=21,70,2
          message=' '
          write (message,6010)          &
&                  i,': '//ParameterTitles(i)//'=',RealVar(i-20)*MultiplierRealVar(i-20)             &
&               ,i+1,': '//ParameterTitles(i+1)//'=',RealVar(i-19)*MultiplierRealVar(i-19)
 6010     format(10('   ',i2,a,f16.7))
          call Message_Output(Message_Info,trim(message))
        end do
! 
!     - USER DEFINED FILENAMES
! 
        call Message_Output(Message_Info,'')
        call Message_Output(Message_Info,' Model Filenames')
        call Message_Output(Message_Info,' ---------------')
        do i=1,MaxUser
          message=' '
          write(message,6020) -i,': '//FileTitles(i)//': '           &
&                //trim(FilenameUser(i))
          call Message_Output(Message_Info,message)
        end do
 6020   FORMAT(10(' ',i3,a))
        call Message_Output(Message_Info,' ----------------------')
      END IF
! 
!   Check any parameters for validity before RETURN
! 
      IF (StatsTime == 0) THEN
        StatsTime=RunTime
      END IF
!  old no longer used Monte-Carlo mode
      IF (ModeMC == 1) then
        call Message_Output(Message_WarnContinue,'ModeMC=1 no longer supported'    &
&                    //'... resetting to ModeMC=0')
        ModeMC=0
      end if
!  make sure thresholds meet physical or model constraints
      IF (FactMx < 1.0) then
        call Message_Output(Message_WarnContinue,'FactMx ',FactMx                 &
&                          ,' is < 1.0 ... resetting FactMx=1.0')
        FactMx=1.0
      END IF
!  set physical coordinates
      if (modeErode >= 20 .or. modeRunoff >= 20) then
        modeErode=mod(modeErode,20)+20
        modeRunoff=mod(modeRunoff,20)+20
        call Message_Output(Message_WarnContinue                                &
&             ,'Setting Physical Coordinates: modeErode=',modeErode             &
&             ,', modeRunoff=',modeRunoff)
      end if
      RETURN

 9001 CALL Message_Output(Message_ErrorStop,                                    &
&            'Premature END TO the Command Input Stream in SET_PARAMETERS') 
END SUBROUTINE SetParameters
! 
! ==============================================
!   Set the boundary conditions on the Grid
! ==============================================
! 
SUBROUTINE SetBC(Z,IrregularBoundary,LowX,HighX,LowY,HighY,gridX,gridY)
    IMPLICIT NONE
! 
    LOGICAL ::  IrregularBoundary
    INTEGER :: LowX,HighX,LowY,HighY,gridX,gridY
    REAL(KIND(0.0D0)) :: Z(gridX,gridY)
! 
    INTEGER :: i,j
! 
!  These BC's are only required for solver for the regular
!  boundaries. They are irrelevant for the irregular boundaries
! 
      IF (.not.IrregularBoundary) THEN
        DO i=LowX,HighX
          Z(i,HighY+1)=Z(i,HighY)
          Z(i,LowY-1)=Z(i,LowY) 
        END DO       
        DO j=LowY,HighY
          Z(HighX+1,j)=Z(HighX,j)
          Z(LowX-1,j)=Z(LowX,j)
        END DO
! 
!  the corner nodes effect the directions algorithmn
! 
        Z(LowX-1,LowY-1)=(Z(LowX-1,LowY)+Z(LowX,LowY-1))*0.5
        Z(LowX-1,HighY+1)=(Z(LowX-1,HighY)+Z(LowX,HighY+1))*0.5
        Z(HighX+1,LowY-1)=(Z(HighX+1,LowY)+Z(HighX,LowY-1))*0.5
        Z(HighX+1,HighY+1)=(Z(HighX+1,HighY)+Z(HighX,HighY+1))*0.5
      END IF
      RETURN
END SUBROUTINE SetBC
! 
! ==============================================
!   Set the boundary conditions on the Grid
! ==============================================
! 
SUBROUTINE SetBCXY(Z,IrregularBoundary,LowX,HighX,LowY,HighY)
  USE SiberiaTypes
    IMPLICIT NONE
! 
    LOGICAL ::  IrregularBoundary
    INTEGER :: LowX,HighX,LowY,HighY
    TYPE(ArrayR8XY) :: Z
! 
    INTEGER :: i,j
! 
!  These BC's are only required for solver for the regular
!  boundaries. They are irrelevant for the irregular boundaries
! 
      IF (.not.IrregularBoundary) THEN
        DO i=LowX,HighX
          Z%Data(i,HighY+1)=Z%Data(i,HighY)
          Z%Data(i,LowY-1)=Z%Data(i,LowY) 
        END DO       
        DO j=LowY,HighY
          Z%Data(HighX+1,j)=Z%Data(HighX,j)
          Z%Data(LowX-1,j)=Z%Data(LowX,j)
        END DO
! 
!  the corner nodes effect the directions algorithmn
! 
        Z%Data(LowX-1,LowY-1)=(Z%Data(LowX-1,LowY)+Z%Data(LowX,LowY-1))*0.5
        Z%Data(LowX-1,HighY+1)=(Z%Data(LowX-1,HighY)+Z%Data(LowX,HighY+1))*0.5
        Z%Data(HighX+1,LowY-1)=(Z%Data(HighX+1,LowY)+Z%Data(HighX,LowY-1))*0.5
        Z%Data(HighX+1,HighY+1)=(Z%Data(HighX+1,HighY)+Z%Data(HighX,HighY+1))*0.5
      END IF
      RETURN
END SUBROUTINE SetBCXY
! 
! =========================================================
!  Doing the Finite difference evaluations on the states
! =========================================================
! 
SUBROUTINE Finite(DirChg,dyc,InitTimeStep1,a,y,Z,Sed,s0,Area         &
&          ,Domain,IrregularBoundary,DetCIF,LowX,HighX,LowY,HighY    &
&          ,Sim_Parameters,GridX,GridY)
  USE SiberiaTypes
  USE Support
  USE openMPsupport
  IMPLICIT NONE

    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    LOGICAL :: DirChg, Domain(GridX,GridY),IrregularBoundary    &
&           ,DetCIF
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: A,Y,Z,Area,Sed,s0
    REAL(KIND(0.0D0)) :: dyc,InitTimeStep1
    TYPE(LocalParameters) :: Sim_Parameters

    INTEGER :: i,j,ErrorNo
    REAL(KIND(0.0D0)) :: af,yf,yq,b5b3m5,m3m5

      b5b3m5=Sim_Parameters%b5*Sim_Parameters%b3**Sim_Parameters%m5
      m3m5=Sim_Parameters%m3*Sim_Parameters%m5
      if (FirstFinite) then
        allocate(AreaTermFinite(gridx,gridy),stat=ErrorNo)
        IF (ErrorNo /= 0) CALL AllocationError('AreaTermFinite','SIBERIA_Finite')
      end if
      IF (IrregularBoundary) THEN

!   Irregular Boundaries

        IF (DetCIF) THEN
          IF (DirChg.and.Sim_Parameters%ModeChannel /= 5) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Domain(i,j)) THEN
                  IF (Area(i,j) /= 1) THEN
                    AreaTermFinite(i,j)=b5b3m5*Area(i,j)**m3m5
                  ELSE
                    AreaTermFinite(i,j)=b5b3m5
                  END IF
                END IF
              END DO
            END DO
          END IF
          DO j=LowY,HighY                      
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                SELECT CASE (Sim_Parameters%ModeChannel)
                  CASE DEFAULT
                    af=a(i,j)
                    a(i,j)=s0(i,j)**Sim_Parameters%n5*AreaTermFinite(i,j)
                    yf=y(i,j)     
                    yq=yf*yf
                    y(i,j)=yf+Sim_Parameters%DTime*InitTimeStep1*         &
&                     (yf*(dyc-1.0)+yq/(1.0+9.0*yq)+                      &
&                      Sim_Parameters%c1*af+0.0001)
                  CASE (4)
                    a(i,j)=0.0
                    y(i,j)=0.0
                END SELECT
                Z(i,j)=Z(i,j)+Sed(i,j)
              END IF
           END DO
         END DO
       ELSE       ! not DetCIF
          DO j=LowY,HighY                     
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                Z(i,j)=Z(i,j)+Sed(i,j)
              END IF
          END DO
        END DO
       END IF
      ELSE

!  Regular Boundaries

        IF (DetCIF) THEN
          IF (DirChg.and.Sim_Parameters%ModeChannel /= 5) THEN
            DO j=LowY,HighY
              DO i=LowX,HighX
                IF (Area(i,j) /= 1) THEN
                  AreaTermFinite(i,j)=b5b3m5*Area(i,j)**m3m5
                ELSE
                  AreaTermFinite(i,j)=b5b3m5
                END IF
              END DO
            END DO
          END IF
          DO j=LowY,HighY                      
            DO i=LowX,HighX
              SELECT CASE(Sim_Parameters%ModeChannel)
                CASE DEFAULT
                  af=a(i,j)
                  a(i,j)=s0(i,j)**Sim_Parameters%n5*AreaTermFinite(i,j)
                  yf=y(i,j)     
                  yq=yf*yf
                  y(i,j)=yf+Sim_Parameters%DTime*InitTimeStep1*           &
&                   (yf*(dyc-1.0)+yq/(1.0+9.0*yq)+                        &
&                    Sim_Parameters%c1*af+0.0001)
                CASE (5)
                  a(i,j)=0
                  y(i,j)=0
              END SELECT
              Z(i,j)=Z(i,j)+Sed(i,j)
            END DO
          END DO
        ELSE    ! not DetCIF
          DO j=LowY,HighY                      
            DO i=LowX,HighX
              Z(i,j)=Z(i,j)+Sed(i,j)
            END DO
          END DO
        END IF
      END IF
      FirstFinite=.false.
      RETURN
END SUBROUTINE Finite
! 
! =========================================================
!  doing the work of the CORRECTOR step
! =========================================================
! 
SUBROUTINE Correct(YOld,y,yC,Zold,Sed,sedC                    &
&        ,Z,Zout,Zout1,ZmassA,ZmassP,Zin,Zin1,Domain                &
&        ,IrregularBoundary,DetCIF,SoilDepthOld,SoilDepth,SoilZ     &
&        ,LowX,HighX,LowY,HighY,GridX,GridY,Erosion,erosionC)
  USE SiberiaTypes
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),dimension(GridX,GridY) :: YOld,y,yC,Zold        &
&          ,Sed,sedC,Z,SoilDepth,SoilDepthOld,SoilZ
    REAL(KIND(0.0D0)) ::Zout,Zout1,ZmassA,ZMassP,Zin,Zin1
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary,DetCIF
    TYPE(ArrayR8XY) :: Erosion,ErosionC
! 
    INTEGER :: i,j
! 
      IF (DetCIF) THEN
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                y(i,j)=(yC(i,j)+YOld(i,j))*0.5
                Sed(i,j)=(Sed(i,j)+sedC(i,j))*0.5
                erosion%data(i,j)=(erosion%data(i,j)+erosionC%data(i,j))*0.5
                Z(i,j)=Zold(i,j)+Sed(i,j)
                SoilDepth(i,j)=(SoilDepthOld(i,j)+SoilDepth(i,j))*0.5
                SoilZ(i,j)=Z(i,j)-SoilDepth(i,j)
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              y(i,j)=(yC(i,j)+YOld(i,j))*0.5
              Sed(i,j)=(Sed(i,j)+sedC(i,j))*0.5
              erosion%data(i,j)=(erosion%data(i,j)+erosionC%data(i,j))*0.5
              Z(i,j)=Zold(i,j)+Sed(i,j)
              SoilDepth(i,j)=(SoilDepthOld(i,j)+SoilDepth(i,j))*0.5
              SoilZ(i,j)=Z(i,j)-SoilDepth(i,j)
            END DO
          END DO
        END IF
      ELSE
        IF (IrregularBoundary) THEN
          DO j=LowY,HighY
            DO i=LowX,HighX
              IF (Domain(i,j)) THEN
                Sed(i,j)=(Sed(i,j)+sedC(i,j))*0.5
                erosion%data(i,j)=(erosion%data(i,j)+erosionC%data(i,j))*0.5
                Z(i,j)=Zold(i,j)+Sed(i,j)
                SoilDepth(i,j)=(SoilDepthOld(i,j)+SoilDepth(i,j))*0.5
                SoilZ(i,j)=Z(i,j)-SoilDepth(i,j)
              END IF
            END DO
          END DO
        ELSE
          DO j=LowY,HighY
            DO i=LowX,HighX
              Sed(i,j)=(Sed(i,j)+sedC(i,j))*0.5
              erosion%data(i,j)=(erosion%data(i,j)+erosionC%data(i,j))*0.5
              Z(i,j)=Zold(i,j)+Sed(i,j)
              SoilDepth(i,j)=(SoilDepthOld(i,j)+SoilDepth(i,j))*0.5
              SoilZ(i,j)=Z(i,j)-SoilDepth(i,j)
            END DO
          END DO
        END IF
      END IF
      ZmassA=(Zin+Zin1)*0.5
      Zin=(Zin+Zin1)*0.5
      Zout=(Zout+Zout1)*0.5
      IF (ZmassA /= 0.0) THEN
        ZmassP=-Zout/ZmassA
      ELSE
        ZmassP=0.0
      END IF
      ZmassA=ZmassA+Zout
      RETURN
END SUBROUTINE Correct       
! 
!  =========================================================================
!  interpolation routine on a rectangular element by BiLinear interpolation
!  =========================================================================
! 
REAL(KIND(0.0D0)) FUNCTION BiLinear(mapx,mapy,r11,r21,r22,r12)
  IMPLICIT NONE
! 
  REAL(KIND(0.0D0)) :: mapx,mapy,r11,r21,r22,r12
! 
    BiLinear=(r12-r11)*mapy+(r21-r11)*mapx               &
&           +(r22+r11-r12-r21)*mapx*mapy+r11
    RETURN
END FUNCTION BiLinear 
!
!  =========================================================================
!  Routine TO calculate the Adaptive timestep size
!  =========================================================================
! 
SUBROUTINE Adaptive2(InitTimeStep1,Discrete,Sed,s0,dZdX2,area,Direct,z      &
&         ,IrregularBoundary,Domain,MaxSt1,MaxSt2,MaxDz1,MaxDz2,MeanSt      &
&         ,LowX,HighX,LowY,HighY,Sim_Parameters,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Setup
    IMPLICIT NONE
! 
!  Stability threshold determined TO yield good stability and mass balance
! 
    REAL(KIND(0.0D0)) :: Stable,Maxpeclet
    PARAMETER (Stable=0.05, Maxpeclet=0.2)
! 
    INTEGER :: Discrete,LowX,HighX,LowY,HighY,gridX,gridY
    INTEGER,dimension(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: Sed,s0,dZdX2,Area,Z
    REAL(KIND(0.0D0)) :: InitTimeStep1,MeanSt,MaxSt1,MaxSt2,MaxDz1,MaxDz2
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)
    TYPE(LocalParameters) :: Sim_Parameters
! 
    INTEGER :: i,j,steps,TotalArea,TotalArea1
    REAL(KIND(0.0D0)) ::  temp,stablesum,sumz,sumsed          &
&        ,maxstable,maxsed
! 
      stablesum=0
      maxstable=-1
      maxsed=-1
      totalarea=0
      totalarea1=0
      sumz=0
      sumsed=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF ((IrregularBoundary.and.Domain(i,j))                            &
&              .or.(.not.IrregularBoundary)) THEN
            IF (z(i,j) /= 0.0) THEN
              temp=Sim_Parameters%b1*(Sim_Parameters%b3*area(i,j)            &
&                    **Sim_Parameters%m3)**Sim_Parameters%m1                 &
&                    *s0(i,j)**Sim_Parameters%n1
              stablesum=stablesum+temp
              maxstable=max(maxstable,temp)
              sumz=sumz+z(i,j)
              totalarea=totalarea+1
            END IF
            sumsed=sumsed+abs(sed(i,j))
            maxsed=max(maxsed,abs(sed(i,j)))
            totalarea1=totalarea1+1
          END IF
        END DO
      END DO
      sumz=sumz/totalarea
      maxstable=maxstable/sumz
      maxsed=maxsed/sumz/abs(InitTimeStep1)
      IF (sumz > 0.001) THEN
        sumsed=sumsed/totalarea1/abs(InitTimeStep1)/sumz
        stablesum=stablesum/totalarea/sumz
      ELSE
        sumsed=sumsed/totalarea1/abs(InitTimeStep1)/0.001
        stablesum=stablesum/totalarea/0.001
      END IF
!    steps=max(maxsed/0.0001,1.0)
!      steps=max(max(sumsed/0.00001,stablesum/0.00025),1.0)
!      steps=max(max(sumsed/0.00012,stablesum/0.00025),1.0)
      steps=max(dble(sumsed/0.00012),dble(stablesum/0.00025),1.0d0)
      InitTimeStep1=1.0/steps
      discrete=steps
!      WRITE (*,*) 'adaptive2',steps,sumsed,stablesum
!      WRITE (*,*) 'Adaptive =',steps,sngl(MaxSt1)
!      WRITE (*,*) MaxDz1,MaxDz2
! 
      RETURN
END SUBROUTINE Adaptive2
! 
! ---------------------------------------------------------------------------
!   doing units conversion from physical coordinates TO the non-dimensional
!   coordinates used within SIBERIA and making slopes and areas nondimensional
! ---------------------------------------------------------------------------
! 
SUBROUTINE PhysicalStates_Sim(Slope,Area,Sim_Parameters,GridX,GridY)
  USE Multipliers
  USE Support
  USE SiberiaTypes
  IMPLICIT NONE
!
  integer :: GridX,GridY 
  REAL(KIND(0.0D0)),dimension(GridX,GridY) :: Slope,Area
  TYPE(LocalParameters) :: Sim_Parameters
! 
  INTEGER :: i,j
  REAL(KIND(0.0D0)) :: mult
! 
    IF (Sim_Parameters%ModeErode < 20) THEN
      MultiplierSlope=1
      MultiplierArea=1
      DO i=1,MaxNoReals
        MultiplierRealVar(i)=1
      END DO
    ELSE
      MultiplierSlope=1.0/Sim_Parameters%gridXY
      MultiplierArea=Sim_Parameters%gridXY**2
      DO i=1,MaxNoReals
        MultiplierRealVar(i)=1
      END DO
      mult=1.0/Sim_Parameters%gridXY**(2*Sim_Parameters%m3-2)
!      mult=1.0/gridXY**(2*m3-1)
!     b3sds
      MultiplierRealVar(10)=mult
!     b3sdl
      MultiplierRealVar(11)=mult
!     b3
      MultiplierRealVar(18)=mult
      mult=1.0/Sim_Parameters%gridXY**(Sim_Parameters%m1-Sim_Parameters%n1-1)
!     b1
      MultiplierRealVar(19)=mult
!     b12
      MultiplierRealVar(39)=mult
!     Dz
      MultiplierRealVar(1)=Sim_Parameters%gridXY
!     DzHold
      MultiplierRealVar(3)=Sim_Parameters%gridXY
!     QsHold
      MultiplierRealVar(4)=Sim_Parameters%gridXY
      mult=1.0/Sim_Parameters%gridXY**(Sim_Parameters%m5-Sim_Parameters%n5-1)
!     b5
      MultiplierRealVar(24)=mult
!     s0Max
      MultiplierRealVar(31)=1.0/Sim_Parameters%gridXY
!     b6
      SELECT CASE (Sim_Parameters%ModeChannel)
      CASE(0,3,4,5)
!            nominal channels
        MultiplierRealVar(37)=1
!            depth on area
      CASE(1)
        MultiplierRealVar(37)=1.0/Sim_Parameters%gridXY**(2*Sim_Parameters%m6-1)
!            depth on discharge
      CASE(2)
        MultiplierRealVar(37)=1.0/Sim_Parameters%gridXY**(Sim_Parameters%m6-1)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorStop                                &
&          ,' Unimplemented mode for ModeChannel in SUBROUTINE '             &
&         //' PHYSICALPARAMETERS1',Sim_Parameters%ModeChannel)
      END SELECT
    END IF
!
!
!
    DO j=1,Sim_Parameters%ky
      DO i=1,Sim_Parameters%kx
        Area(i,j)=Area(i,j)/MultiplierArea
        Slope(i,j)=Slope(i,j)/MultiplierSlope
      END DO
    END DO
    Sim_Parameters%dZ=Sim_Parameters%dZ/MultiplierRealVar(1)
    Sim_Parameters%dZn=Sim_Parameters%dZn/MultiplierRealVar(2)
    Sim_Parameters%dZHold=Sim_Parameters%dZHold/MultiplierRealVar(3)
    Sim_Parameters%QsHold=Sim_Parameters%QsHold/MultiplierRealVar(4)
    Sim_Parameters%FactMx=Sim_Parameters%FactMx/MultiplierRealVar(5)
    Sim_Parameters%FRanMn=Sim_Parameters%FRanMn/MultiplierRealVar(6)
    Sim_Parameters%c1=Sim_Parameters%c1/MultiplierRealVar(7)
    Sim_Parameters%YFix=Sim_Parameters%YFix/MultiplierRealVar(8)
    Sim_Parameters%FRanCV=Sim_Parameters%FRanCV/MultiplierRealVar(9)
    Sim_Parameters%b3SDs=Sim_Parameters%b3SDs/MultiplierRealVar(10)
    Sim_Parameters%b3SDl=Sim_Parameters%b3SDl/MultiplierRealVar(11)
    Sim_Parameters%TAmp=Sim_Parameters%TAmp/MultiplierRealVar(12)
    Sim_Parameters%TPeriod=Sim_Parameters%TPeriod/MultiplierRealVar(13)
    Sim_Parameters%TPhase=Sim_Parameters%TPhase/MultiplierRealVar(14)
    Sim_Parameters%FRanZ=Sim_Parameters%FRanZ/MultiplierRealVar(15)
    Sim_Parameters%a1=Sim_Parameters%a1/MultiplierRealVar(16)
    Sim_Parameters%m3=Sim_Parameters%m3/MultiplierRealVar(17)
    Sim_Parameters%b3=Sim_Parameters%b3/MultiplierRealVar(18)
    Sim_Parameters%b1=Sim_Parameters%b1/MultiplierRealVar(19)
    Sim_Parameters%m1=Sim_Parameters%m1/MultiplierRealVar(20)
    Sim_Parameters%n1=Sim_Parameters%n1/MultiplierRealVar(21)
    Sim_Parameters%Bulk=Sim_Parameters%Bulk/MultiplierRealVar(22)
    Sim_Parameters%InitTimeStep=Sim_Parameters%InitTimeStep/MultiplierRealVar(23)
    Sim_Parameters%b5=Sim_Parameters%b5/MultiplierRealVar(24)
    Sim_Parameters%n5=Sim_Parameters%n5/MultiplierRealVar(25)
    Sim_Parameters%SInit=Sim_Parameters%SInit/MultiplierRealVar(26)
    Sim_Parameters%m5=Sim_Parameters%m5/MultiplierRealVar(27)
    Sim_Parameters%YHold=Sim_Parameters%YHold/MultiplierRealVar(28)
    Sim_Parameters%Notch=Sim_Parameters%Notch/MultiplierRealVar(29)
    Sim_Parameters%Cover=Sim_Parameters%Cover/MultiplierRealVar(30)
    Sim_Parameters%s0max=Sim_Parameters%s0max/MultiplierRealVar(31)
    Sim_Parameters%DTime=Sim_Parameters%DTime/MultiplierRealVar(32)
    Sim_Parameters%OTime=Sim_Parameters%OTime/MultiplierRealVar(33)
    Sim_Parameters%GridXY=Sim_Parameters%GridXY/MultiplierRealVar(34)
    Sim_Parameters%East=Sim_Parameters%East/MultiplierRealVar(35)
    Sim_Parameters%North=Sim_Parameters%North/MultiplierRealVar(36)
    Sim_Parameters%b6=Sim_Parameters%b6/MultiplierRealVar(37)
    Sim_Parameters%m6=Sim_Parameters%m6/MultiplierRealVar(38)
    Sim_Parameters%b12=Sim_Parameters%b12/MultiplierRealVar(39)
    Sim_Parameters%m12=Sim_Parameters%m12/MultiplierRealVar(40)
    Sim_Parameters%SDRate=Sim_Parameters%SDRate/MultiplierRealVar(41)
    Sim_Parameters%SDExp1=Sim_Parameters%SDExp1/MultiplierRealVar(42)
    Sim_Parameters%SDExp2=Sim_Parameters%SDExp2/MultiplierRealVar(43)
    Sim_Parameters%SMThreshold=Sim_Parameters%SMThreshold/MultiplierRealVar(44)
    Sim_Parameters%SDSMWgt=Sim_Parameters%SDSMWgt/MultiplierRealVar(45)
    Sim_Parameters%rdummy46=Sim_Parameters%rdummy46/MultiplierRealVar(46)
    Sim_Parameters%rdummy47=Sim_Parameters%rdummy47/MultiplierRealVar(47)
    Sim_Parameters%rdummy48=Sim_Parameters%rdummy48/MultiplierRealVar(48)
    Sim_Parameters%rdummy49=Sim_Parameters%rdummy49/MultiplierRealVar(49)
    Sim_Parameters%rdummy50=Sim_Parameters%rdummy50/MultiplierRealVar(50)
    RETURN
END SUBROUTINE PhysicalStates_Sim
! 
! ---------------------------------------------------------------------
!   Input the run DATA
! ---------------------------------------------------------------------
! 
SUBROUTINE RunDataInput(RSTfile,LgthRSTFileName,InFile                   &
&           ,CommandFile,BoundFile                                       &
&           ,IrregularBoundary,NoTime,Time,Outfile,lfilenm               &
&           ,OutputFileExt,Start,IRANZ,iInit,iXXX,iYYY)
  USE SiberiaConstants
  USE Setup
  USE Support
  USE InputOutputStreams
  IMPLICIT NONE
! 
  INTEGER :: Time(*),Start,NoTime,IRANZ,iInit,iXXX(*),iYYY(*),lfilenm
  LOGICAL :: InFile,CommandFile,IrregularBoundary
  CHARACTER(5)  :: OutputFileExt
  CHARACTER(*) :: RSTfile,BoundFile,Outfile
! 
  INTEGER :: i,k,LgthBNDFileName,LgthRSTFileName
  LOGICAL :: RSTFileExists=.true. ,BNDFileExists=.true. ,lexist
  CHARACTER(1) :: yn
  CHARACTER(80) :: line,FileName
! 
!  The starting RST2 file
! ------------------------
! 
      RSTFile=' '
      IF (Prompt) THEN
        WRITE(*,*)
        WRITE(*,1090)
 1090   FORMAT(' Initial RST file ? [DEFAULT => no RST2 file]  : ',$)
      END IF
      READ(InputStream,2010,ERR=9000,END=9001) RSTfile
 2010 FORMAT(a)
      CommandFile=.false.
 1091 LgthRSTFileName=len_trim(RSTfile)
      IF (RSTfile(1:10) /= '          ') THEN
        IF (RSTfile(1:4) == '####') THEN
          CALL Versions
        ELSE IF (RSTfile(1:1) == '@') THEN
          InputStream=CommandUnit
          INQUIRE(file=RSTfile(2:LgthRSTFileName),exist=lexist,ERR=9003)
          IF (.not.lexist) GO TO 9004
          OPEN(UNIT=InputStream,FILE=RSTfile(2:LgthRSTFileName)            &
&                      ,STATUS='old',ERR=9002)
          CommandFile=.true.
          CALL Message_Output(Message_Info,                                &
&                   ' -- Command file input = '//trim(RSTfile))
          rstfile=' '
          READ(InputStream,2010,ERR=9000,END=9001) RSTfile
          GO TO 1091
        END IF
        InFile=.true.
        CALL Message_Output(Message_Info,                                   &
&               ' -- Input RST file = '//trim(RSTfile))
        INQUIRE(file=RSTfile(1:LgthRSTFileName),exist=RSTFileExists)
        IF (.not.RSTFileExists) THEN
          CALL Message_Output(Message_WarnContinue,                         &
&               ' Requested RST file does not exist')
        END IF
      ELSE
        CALL Message_Output(Message_Info,' -- No input RST file')
      END IF
! 
!    BND file input
!   ----------------
! 
      IF (Prompt) THEN
        WRITE(*,*)
        WRITE(*,1200)
 1200   FORMAT(' Initial BOUNDARIES file ?  [DEFAULT => no BND file]  : ',$)
      END IF
      BoundFile=' '                     
      READ(InputStream,2010,ERR=9000,END=9001) BoundFile
      LgthBNDFileName=len_trim(BoundFile)
      IF (BoundFile(1:10) /= '          ') THEN
        IrregularBoundary=.true.
        CALL Message_Output(Message_Info,                                   &
&              ' -- Input boundary file = '//trim(BoundFile))
        INQUIRE(file=Boundfile(1:LgthBNDFileName),exist=BNDFileExists)
        IF (.not.BNDFileExists) THEN
          CALL Message_Output(Message_WarnContinue,                         &
&                    ' Requested BND file does not exist')
        END IF
      ELSE
        IrregularBoundary=.false.
        CALL Message_Output(Message_Info,' -- No input boundary file ')
      END IF
!  IF one of the input files doesn't exist
      IF (.not.(RSTFileExists.and.BNDFileExists)) THEN
        CALL Message_Output(Message_ErrorStop,                              &
&                 ' One of the required input files does not exist ')
      END IF
! 
!   What output files are required
!  --------------------------------
! 
!
!   Input the times at which the output for RESTART or RAW is done
! 
      IF (Prompt) THEN
        WRITE(*,*)
        WRITE(*,1101)
      END IF
 1101 FORMAT(' No of times for RESTART output [DEFAULT => no output files]: ',$)
      line=' '
      READ(InputStream,2010,ERR=9000,END=9001) line
      IF (Line(1:10) == '          ') THEN
        notime=0
      ELSE
        READ(Line(1:80),*,ERR=9000,END=9001) notime
        IF (NoTime > nCont) THEN
        CALL Message_Output(Message_ErrorStop                                  &
&          ,' -- Too many times for .RST2 output ',notime,' > ',ncont)
        END IF
      ENDIF
      IF (notime >= 1 .or. notime < 0) THEN
        IF (Prompt) THEN
          WRITE(*,*)
          WRITE(*,*)' Input of each time from run start'
        END IF
! 
!  READ each time requested ... note in versions < 8.15 the times could
!  be on one line ... they are now required TO be one per line.
! 
!        READ(InputStream,*,ERR=9000,END=9001) (time(i),i=1,notime)
!  IF input no of times is -ve THEN set it TO the maximum possible for 
!    the unformated input
        IF (NoTime < 0) NoTime=nCont
        DO i=1,NoTime
          line=' '
          READ(InputStream,1109,ERR=9000,END=9001) line
 1109     FORMAT(a)
          IF (line(1:10) == '          ') THEN
            NoTime=i-1
            GO TO 1105
          END IF
          READ(line,*,ERR=9000,END=9001) time(i)
        END DO
 1105   CONTINUE
		if (notime > 0 .and. time(1) < 0) then
		    NoTime=nCont
            CALL Message_Output(Message_Info                                  &
&             ,' Regular output times option from t=',abs(time(1)),' to '               &
&             ,abs(time(1)*NoTime),' every ',abs(time(1)),' timesteps')
		    do k=1,NoTime
			  time(k)=abs(time(1))*k
			end do
		end if
        IF (Prompt) THEN
          WRITE (*,*) ' END of ',NoTime,' times input'
          WRITE (*,*) 
        END IF
! 
! Loop here TO get a semantically Correct file name
! 
        lexist = .true.
        OutFile=' '
        DO WHILE (lexist)
        IF (Prompt) THEN
          WRITE (*,*) 
          WRITE (*,1103) 
        END IF
 1103   FORMAT(' Enter Generic FileName (no extension)',               &
&              ' for RST2 output files'/                               &
&              ' or a FileName with ".raw" extension',                 &
&              ' for Mindraft output : ',$)
        READ (InputStream,2010,ERR=9000,END=9001) Outfile
        lfilenm=len_trim(Outfile)
! 
! Determine required TYPE.
! 
        lexist = .false.
        IF(lfilenm > 4) THEN
          IF(Outfile(lfilenm-3:lfilenm) == '.raw') THEN
            OutputFileExt = '.raw'
            FileName = Outfile
            INQUIRE(file=FileName, EXIST=lexist)
            IF(lexist) THEN
              IF (Prompt) WRITE(*, 7100)
 7100         FORMAT(/ 'File exists. Replace (y/n) ? ')
              READ(inputstream, 7011,ERR=9000,END=9001) yn
 7011         FORMAT(a1) 
              IF(yn == 'y'  .or. yn == 'Y') lexist = .false.
            END IF
          ELSE
            OutputFileExt = '.rst2'
          END IF
        ELSE IF(lfilenm > 0) THEN
          OutputFileExt = '.rst2'
        ELSE
          lexist = .true.
          CALL Message_Output(Message_WarnContinue,                    &
&                ' Invalid File Name .. Try again')
        END IF
        END DO            ! DO while (lexist)
        IF (Prompt) WRITE (*,7013)
 7013   FORMAT(/' Absolute Start Time ')
        READ (InputStream,*,ERR=9000,END=9001) Start
      END IF
! 
! Echo
! 
      CALL Message_Output(Message_Info,'',notime,' '//OutputFileExt(2:5)    &
&         //' outputs requested')
      CALL Message_Output(Message_Info,'First 50 times')
      IF (notime /= 0) THEN
        DO i=1,min(notime,50)
          CALL Message_Output(Message_Info,' -- ',time(i))
        END DO
        IF(OutputFileExt == '.rst2') THEN
          CALL Message_Output(Message_Info,                                 &
&                ' -- Output RST2 Generic FileName = '//trim(Outfile))
        ELSE
          CALL Message_Output(Message_Info,                                 &
&                ' -- Output file name = '//trim(Outfile))
        ENDIF
        CALL Message_Output(Message_Info,' -- Absolute start time = ',start)
      ELSE
        CALL Message_Output(Message_Info,' -- No output RST2 files requested')
      END IF
! 
!  IF not restarting from a previous run ask for more information
! 
      IF (.not.InFile) THEN
         IF (Prompt) THEN
           WRITE(*,*) 
           WRITE(*,7014) 
         END IF
         line=' '
 7014    FORMAT(' INPUT the random number [DEFAULT => 1]   : ',$)
         READ(InputStream,1109,ERR=9000,END=9001) line
         if (line(1:10) == '          ') then
           iranz=1
         else
           READ(line,*,ERR=9000,END=9001) IRANZ
         end if
         CALL Message_Output(Message_Info,' -- Input random number = ',iranz)
         IF (.not.IrregularBoundary) THEN
 1019      IF (Prompt) WRITE(*,7015) 
 7015      FORMAT(' INPUT no of outlet nodes [DEFAULT => outlet at (2,2)]: ',$)
           line=' '
           READ(InputStream,1109,ERR=9000,END=9001) line
           IF (line(1:10) == '          ') then
             iInit=-1
           else
             READ(line,*,ERR=9000,END=9001) iInit
           end if
           IF (iInit > NoOutlets) THEN
             CALL Message_Output(Message_WarnContinue                        &
&                   ,'Too many outlet nodes. Maximum Allowed = ',NoOutlets)
             GO TO 1019
           ELSE IF (iinit < 1) THEN
             iinit=1
             ixxx(1)=2
             iyyy(1)=2
           ELSE
             DO i=1,iInit
               IF (Prompt) WRITE(*,1010)
 1010          FORMAT(' INPUT the position of the outlet node : ',$)
               READ(InputStream,*,ERR=9000,END=9001) iXXX(i),iYYY(i)
             END DO
           END IF
         END IF
      END IF
      RETURN
! 
!   Error Handling
!  ---------------- 
! 
 9000 CALL Message_Output(Message_ErrorStop,                                &
&        'Error while Reading the Command File in SIBERIA_MAIN') 
 9001 CALL Message_Output(Message_ErrorStop,                                &
&        'Premature END TO the Command File in SIBERIA_MAIN') 
 9002 CALL Message_Output(Message_ErrorStop,                                &
&        'Cannot OPEN Command File ... is it already OPEN in another '      &
&         //'application? '//trim(RSTfile)) 
 9003 CALL Message_Output(Message_ErrorStop,                                &
&        'Error on INQUIRE on Command File '//trim(RSTfile)) 
 9004 CALL Message_Output(Message_ErrorStop,                                &
&        'Requested Command File does not exist'//trim(RSTfile)) 
END SUBROUTINE RunDataInput
! 
!  =======================================================================================
!      Initialise the parameters for a single simulation
!  =======================================================================================
! 
SUBROUTINE InitSimParameters(Sim_Parameters)
  USE ModelParameters
  USE SiberiaTypes
! 
  TYPE(LocalParameters) :: Sim_Parameters
!   setting the INTEGER parameters
    Sim_Parameters%RunTime=RunTime
    Sim_Parameters%StatsTime=StatsTime
    Sim_Parameters%kx=kx
    Sim_Parameters%ky=ky
    Sim_Parameters%modeIC=modeIC
    Sim_Parameters%TimeUp=TimeUp
    Sim_Parameters%ModeSolver=ModeSolver
    Sim_Parameters%ModeDir=ModeDir
    Sim_Parameters%ModeUplift=ModeUplift
    Sim_Parameters%ModeRandom=ModeRandom
    Sim_Parameters%ModeErode=ModeErode
    Sim_Parameters%ModeRunoff=ModeRunoff
    Sim_Parameters%ModeChannel=ModeChannel
    Sim_Parameters%ModeDP=ModeDP
    Sim_Parameters%ModeMC=ModeMC
    Sim_Parameters%DirReg=DirReg
    Sim_Parameters%ModeSoil=ModeSoil
    Sim_Parameters%idummy18=idummy18
    Sim_Parameters%idummy19=idummy19
    Sim_Parameters%idummy20=idummy20
!   setting the REAL parameters
    Sim_Parameters%dZ=dZ
    Sim_Parameters%dZn=dZn
    Sim_Parameters%dZHold=dZHold
    Sim_Parameters%QsHold=QsHold
    Sim_Parameters%FactMx=FactMx
    Sim_Parameters%FRanMn=FRanMn
    Sim_Parameters%c1=c1
    Sim_Parameters%YFix=YFix
    Sim_Parameters%FRanCV=FRanCV
    Sim_Parameters%b3SDs=b3SDs
    Sim_Parameters%b3SDl=b3SDl
    Sim_Parameters%TAmp=TAmp
    Sim_Parameters%TPeriod=TPeriod
    Sim_Parameters%TPhase=TPhase
    Sim_Parameters%FRanZ=FRanZ
    Sim_Parameters%a1=a1
    Sim_Parameters%m3=m3
    Sim_Parameters%b3=b3
    Sim_Parameters%b1=b1
    Sim_Parameters%m1=m1
    Sim_Parameters%n1=n1
    Sim_Parameters%Bulk=Bulk
    Sim_Parameters%InitTimeStep=InitTimeStep
    Sim_Parameters%b5=b5
    Sim_Parameters%n5=n5
    Sim_Parameters%SInit=SInit
    Sim_Parameters%m5=m5
    Sim_Parameters%YHold=YHold
    Sim_Parameters%Notch=Notch
    Sim_Parameters%Cover=Cover
    Sim_Parameters%s0max=s0max
    Sim_Parameters%DTime=DTime
    Sim_Parameters%OTime=OTime
    Sim_Parameters%GridXY=GridXY
    Sim_Parameters%East=East
    Sim_Parameters%North=North
    Sim_Parameters%b6=b6
    Sim_Parameters%m6=m6
    Sim_Parameters%b12=b12
    Sim_Parameters%m12=m12
    Sim_Parameters%SDRate=SDRate
    Sim_Parameters%SDExp1=SDExp1
    Sim_Parameters%SDExp2=SDExp2
    Sim_Parameters%SMThreshold=SMThreshold
    Sim_Parameters%SDSMWgt=SDSMWgt
    Sim_Parameters%rdummy46=rdummy46
    Sim_Parameters%rdummy47=rdummy47
    Sim_Parameters%rdummy48=rdummy48
    Sim_Parameters%rdummy49=rdummy49
    Sim_Parameters%rdummy50=rdummy50
!   setting the filename parameters
    Sim_Parameters%FileFactor=FileNameUser(FileFactor)
    Sim_Parameters%FileRunoff=FileNameUser(FileRunoff)
    Sim_Parameters%FileUplift=FileNameUser(FileUplift)
    Sim_Parameters%FileDirections=FileNameUser(FileDirections)
    Sim_Parameters%FileMonteCarlo=FileNameUser(FileMonteCarlo)
    Sim_Parameters%FileChannels=FileNameUser(FileChannels)
    Sim_Parameters%FileLayers=FileNameUser(FileLayers)
    Sim_Parameters%FileDummy8=FileNameUser(FilenameDummy8)
    Sim_Parameters%FileControl=FileNameUser(FileControl)
    Sim_Parameters%FileOthers=FileNameUser(FileOthers)
  RETURN
END SUBROUTINE InitSimParameters

END MODULE Others
