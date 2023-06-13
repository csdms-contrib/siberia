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
! 
!  =======================================================================================
!  =======================================================================================
!                      MODULE for Layer Management and Input
!  =======================================================================================
!  =======================================================================================
! 
! 

MODULE LayerSupport
  USE SiberiaTypes
  USE LayerConstants

!  Version number

  REAL, PARAMETER ::Layer_Version=8.32

!  subroutines for control of the LayerSupport module
  INTERFACE Layer_Set
    MODULE PROCEDURE LayerSetI
    MODULE PROCEDURE LayerSetR
    MODULE PROCEDURE LayerSetR8
    MODULE PROCEDURE LayerSetL
    MODULE PROCEDURE LayerSetStr
    MODULE PROCEDURE LayerSetLayerDataR
    MODULE PROCEDURE LayerSetLayerDataR8
  END INTERFACE Layer_Set
  INTERFACE Layer_Get
    MODULE PROCEDURE LayerGetI
    MODULE PROCEDURE LayerGetR
    MODULE PROCEDURE LayerGetR8
    MODULE PROCEDURE LayerGetL
    MODULE PROCEDURE LayerGetStr
    MODULE PROCEDURE LayerGetArrayI
    MODULE PROCEDURE LayerGetArrayR
    MODULE PROCEDURE LayerGetArrayR8
    MODULE PROCEDURE LayerGetLayerDataI
    MODULE PROCEDURE LayerGetLayerDataR
    MODULE PROCEDURE LayerGetLayerDataR8
  END INTERFACE Layer_Get
  INTERFACE Layer_AddLayer
    MODULE PROCEDURE LayerAddLayerArray
    MODULE PROCEDURE LayerAddLayerPt
  END INTERFACE Layer_AddLayer
  INTERFACE Layer_Uplift
    MODULE PROCEDURE LayerUpliftArray
    MODULE PROCEDURE LayerUpliftConstant
  END INTERFACE Layer_Uplift


!  The layers data structure
!  -------------------------
!    z = elevation of the top of the layer ... the bottommost layer is assumed to be
!        semi-infinite to negative elevations
!    age = age since the start of the simulation of the material in the layer
!    b1 = erodibility of the layer
!
!    The ordering of the array is that %layer(1) is the topmost layer and is the 
!    layer in which all erosion and deposition is occuring. Thus most of the time it 
!    is sufficent to examine the first element of the layer array (i.e.  for node
!    (i,j) looking at %data(i,j)%layer(1))

! NOTE: Most of the layer code is hardwired to a maximum of up to 5 tracers. MaxNumberTracers controls the
!  amount of memory allocated to tracer tracking. In principle this should be 5 but by setting it lower unless
!  necesary we reduce memory usage. In principle this would be more cleanly handled by allocatable arrays but ...
!  MaxNumberTracers should be a minimum of 1 ... not sure what happens if set to 0 (code probably crashes).
  integer, parameter :: MaxNumberTracers=2

  TYPE layer_data
    REAL(KIND(0.0d0)) :: b1,m1,n1,b3,m3,dZ,s0max,z,age,detach,tracer(1:MaxNumberTracers)
  END TYPE layer_data

  TYPE layers_xy
    TYPE(layer_data), POINTER, DIMENSION(:) :: layer
  END TYPE layers_xy

  TYPE layer_array
    TYPE(layers_xy), POINTER, DIMENSION(:,:) :: data
  END TYPE layer_array

  TYPE flowlayer_data
!    REAL(KIND(0.0d0)) :: b1,m1,n1,b3,m3,dZ,s0max,age
    REAL(KIND(0.0d0)) :: b1,age,detach,tracer(1:MaxNumberTracers)
  END TYPE flowlayer_data

  TYPE flowlayer_array
!    TYPE(flowlayer_data), POINTER, DIMENSION(:,:) :: data
    TYPE(layer_data), POINTER, DIMENSION(:,:) :: data
  END TYPE flowlayer_array

  TYPE Layer_DefaultProperties
    REAL(KIND(0.0D0)) :: b1,m1,n1,b3,m3,dZ,s0Max,detach,age,Z,tracer(1:MaxNumberTracers)
  END TYPE Layer_DefaultProperties

  TYPE Layer_grading_fraction
    real(kind(0.0d0)),pointer, dimension(:) :: fraction
  end type Layer_grading_fraction

  TYPE Layer_grading_defaults
    real(kind(0.0d0)),pointer, dimension(:) :: High_Diameter,surface
  end type Layer_grading_defaults

!    DEFAULTS
!    ========

!   Number of tracers
  integer,private,parameter :: NumberTracersDefault=0
!   Minimum elevation allowed
  REAL,PRIVATE,PARAMETER :: MinimumZ=-999999.0
!   The default vertical resolution of each layer
  REAL,PRIVATE,PARAMETER :: LayerResolutionDefault=1.5,LayerDetachmentDefault=1.0
  REAL,PRIVATE,PARAMETER :: MixingAlphaDefault=0.75
!
  REAL,PRIVATE :: TracerDefault(1:MaxNumberTracers)=0.0

!    VARIABLES
!    =========

  Integer,private :: NumberTracers=NumberTracersDefault
  REAL,PRIVATE :: LayerResolution=LayerResolutionDefault                 &
&           ,LayerDetachment=LayerDetachmentDefault                      &
&           ,MixingAlpha=MixingAlphaDefault                              &
&           ,MixingAlphaInv=1.0/MixingAlphaDefault
!   Logical for first time through the subroutines
  LOGICAL,PRIVATE :: FirstLayerInit=.true.,FirstLayerInitSize=.true.     &
&      ,Module_Active=.false.,LayerRegionMaskActive=.false.              &
&      ,Detachment_Limitation=.false.
  logical,private :: firstCosmo=.true.
  type(ArrayLXY),private :: Layer_Mask
  CHARACTER(14),PARAMETER,private :: LayerFileHeader='SIBERIA LAYERS'
!   The layer data
  TYPE(layer_array),PRIVATE :: layers
  TYPE(flowlayer_array),PRIVATE :: flow
  TYPE(Layer_DefaultProperties),PRIVATE :: Layer_Defaults
!
!  Error variables
!
  type ErrorInfo
    integer :: ErrorNo
    character(30) :: ErrorText
  end type ErrorInfo
  CHARACTER(20),parameter,private :: GenericErrorHeader='MODULE Layers: '
  integer,parameter,private :: MaxNoErrors=12
  type(ErrorInfo),dimension(MaxNoErrors),parameter,private :: ErrorData=        &
&           (/ ErrorInfo( 0,  'No error'),                                      &
&              ErrorInfo( 1,  'Invalid inquiry/set option'),                    &
&              ErrorInfo(10,  'Array allocation failed'),                       &
&              ErrorInfo(11,  'Array bound out of range'),                      &
&              ErrorInfo(12,  'Array not allocated'),                           & 
&              ErrorInfo(20,  'Invalid coordinates'),                           & 
&              ErrorInfo(1000,'INIT has been called twice'),                    & 
&              ErrorInfo(1001,'INITSIZE has been called twice'),                & 
&              ErrorInfo(1002,'INIT must be called before INITSIZE'),           & 
&              ErrorInfo(1100,'Module LAYER not initialised'),                  & 
&              ErrorInfo(1101,'Invalid Layer No'),                              & 
&              ErrorInfo(-1,  'Generic unidentified error')/)   
! generic error should always be the last error (failsafe for error text return)
!
  INTEGER,PRIVATE :: LastError


CONTAINS

! ============================================================================
!             INITIALISATION OF LAYERS
! ============================================================================

  SUBROUTINE Layer_Init(ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(OUT) :: ErrorNo

    ErrorNo=0
    IF (FirstLayerInit) THEN
      Module_Active=.true.
!      Module_Active=.false.      ! set false during development when don't want to activate layers
      NULLIFY(layers%Data)
      FirstLayerInit=.false.
      IF (associated(Layer_Mask%data)) call DeallocateArray(Layer_Mask)
    ELSE
      CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Subroutine LAYER_INIT has been called twice')
      ErrorNo=1000
    END IF
    LastError=ErrorNo
  END SUBROUTINE Layer_Init

  logical function Layer_InitI()
    implicit none
    Layer_InitI=Module_Active
  end function Layer_InitI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE Layer_InitSize(lowX,highX,LowY,HighY,Sim_Parameters,age,ErrorNo)
    USE Support
    USE SiberiaTypes
    IMPLICIT NONE
      INTEGER :: lowX,highX,LowY,HighY
      REAL(KIND(0.0D0)) :: age
      INTEGER,INTENT(OUT) :: ErrorNo
      TYPE(LocalParameters) :: Sim_Parameters

      INTEGER :: i,j

    ErrorNo=0
    IF (FirstLayerInit) THEN
      call Message_Output(Message_ErrorStop                                     &
&         ,'Subroutine LAYER_INIT must be called before LAYER_INITSIZE')
      ErrorNo=1002
    END IF
    IF (.not.Module_Active) RETURN
!
!   set defaults
!
    Layer_Defaults%b1=Sim_Parameters%b1
    Layer_Defaults%m1=Sim_Parameters%m1
    Layer_Defaults%n1=Sim_Parameters%n1
    Layer_Defaults%b3=Sim_Parameters%b3
    Layer_Defaults%m3=Sim_Parameters%m3
    Layer_Defaults%dZ=Sim_Parameters%dZ
    Layer_Defaults%s0max=Sim_Parameters%s0max
    Layer_Defaults%age=0
    Layer_Defaults%Z=MinimumZ
    Layer_Defaults%Tracer=TracerDefault
!
    IF (FirstLayerInitSize) THEN
!  allocate the main layer array so that when you have real data can allocate safely
!------------------------------------------------------------------------------------
        ALLOCATE(layers%data(lowX:highX,LowY:HighY),stat=ErrorNo)
        IF (ErrorNo /= 0) then
          CALL AllocationError('Layers array','LAYER_INITSIZE')
          ErrorNo=10
        end if
!  nullify each point so we can do data assignment later
        DO j=LowY,HighY
          DO i=LowX,HighX
            ALLOCATE(layers%data(i,j)%layer(1:1),stat=ErrorNo)
            IF (ErrorNo /= 0) then
              CALL AllocationError('Layers data','LAYER_INITSIZE')
              ErrorNo=10
            end if
            layers%data(i,j)%layer(1)%z=MinimumZ
            layers%data(i,j)%layer(1)%age=age
            call Layer_InitLayer(layers%data(i,j)%layer(1))
          END DO
        END DO
!  allocate the flow array so that when you have real data can allocate safely
!------------------------------------------------------------------------------
        ALLOCATE(flow%data(lowX:highX,LowY:HighY),stat=ErrorNo)
        IF (ErrorNo /= 0) then
          CALL AllocationError('Flow array','LAYER_INITSIZE')
          ErrorNo=10
        end if
      FirstLayerInitSize=.false.
    ELSE
      CALL Message_Output(Message_ErrorStop                                      &
&                   ,'Subroutine LAYER_INITSIZE has been called twice')
      ErrorNo=1001
    END IF
    LastError=ErrorNo
  END SUBROUTINE Layer_InitSize

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE Layer_InitFlowLayer(lowX,highX,LowY,HighY,ErrorNo)
    USE Support
    USE SiberiaTypes
    IMPLICIT NONE
      INTEGER :: lowX,highX,LowY,HighY
      INTEGER,INTENT(OUT) :: ErrorNo

      INTEGER :: i,j

    ErrorNo=0
    IF (FirstLayerInit) THEN
      call Message_Output(Message_ErrorStop                                     &
&         ,'Subroutine LAYER_INIT must be called before Layer_InitFlowLayer')
      ErrorNo=1002
    END IF
    IF (.not.Module_Active) RETURN
!  allocate the flow array so that when you have real data can allocate safely
!------------------------------------------------------------------------------
!    flow%data(lowX:highX,LowY:HighY)=layers%data(lowX:highX,LowY:HighY)%layer(1)
    do j=LowY,HighY
      do i=LowX,HighX
        flow%data(i,j)=layers%data(i,j)%layer(1)
      end do
    end do
    LastError=ErrorNo
  END SUBROUTINE Layer_InitFlowLayer



  SUBROUTINE Layer_InitDetach()
    IMPLICIT NONE
    Detachment_Limitation=.true.
  END SUBROUTINE Layer_InitDetach


!  Put the default values in a new layer (then you can update whichever
!  ones you need to)

  SUBROUTINE Layer_InitLayer(layer)
    implicit none
    TYPE(layer_data) :: layer
    
    integer :: i

    layer%b1=Layer_Defaults%b1
    layer%m1=Layer_Defaults%m1
    layer%n1=Layer_Defaults%n1
    layer%b3=Layer_Defaults%b3
    layer%m3=Layer_Defaults%m3
    layer%dZ=Layer_Defaults%dZ
    layer%s0max=Layer_Defaults%s0max
    layer%detach=LayerDetachmentDefault
    layer%age=Layer_Defaults%age
    layer%Z=Layer_Defaults%Z
    do i=1,MaxNumberTracers
      layer%tracer(i)=Layer_Defaults%tracer(i)
    end do
  END SUBROUTINE Layer_InitLayer



! ============================================================================
!             SET CHARACTERISTICS OF LAYERS
! ============================================================================

  SUBROUTINE LayerSetI(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERSET = '   &
&                 ,Variable)
        ErrorNo=1
      CASE(Layer_Number_Tracers)
        NumberTracers=Value
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerSetI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerSetR(Variable,value,ErrorNo)
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      REAL,INTENT(IN) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      REAL(KIND(0.0D0)) :: dvalue

      dvalue=value
      CALL LayerSetR8(variable,dvalue,ErrorNo)
      LastError=ErrorNo
  END SUBROUTINE LayerSetR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerSetR8(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      REAL(KIND(0.D0)),INTENT(IN) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERSET = '   &
&                 ,Variable)
        ErrorNo=1
      CASE(Layer_Resolution)
        LayerResolution=value
      CASE(Layer_Detachment)
        LayerDetachment=value
      CASE(Layer_Tracer1_Default:Layer_Tracer5_Default)
        TracerDefault(variable-Layer_Tracer1_Default+1)=value
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerSetR8

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerSetL(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      LOGICAL,INTENT(IN) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERSET = '   &
&                 ,Variable)
        ErrorNo=1
      CASE (Layer_Module_Active)
        Module_Active=value
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerSetL


! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerSetLayerDataR(Variable,i,j,Layer,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j,Layer
      REAL,INTENT(IN) :: value
      INTEGER,INTENT(OUT) :: ErrorNo
      
      REAL(KIND(0.0D0)) :: dvalue

      ErrorNo=0
      Dvalue=value
      CALL LayerSetLayerDataR8(Variable,i,j,Layer,dvalue,ErrorNo)
      LastError=ErrorNo
  END SUBROUTINE LayerSetLayerDataR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerSetLayerDataR8(Variable,i,j,Layer,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j,Layer
      REAL(KIND(0.0D0)),INTENT(IN) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERSET = '     &
&                 ,Variable)
        ErrorNo=1
      CASE (Layer_b1,Layer_m1,Layer_n1,Layer_b3,Layer_m3,Layer_dZ      &
&          ,Layer_s0max,Layer_Z,Layer_Age,Layer_Detachment,Layer_Tracer1:Layer_Tracer5)
        IF (ASSOCIATED(layers%data)) THEN
          IF (i >= LBOUND(layers%data,1) .and. i <= UBOUND(layers%data,1)             &
&              .and. j >= LBOUND(layers%data,2) .and. j <= UBOUND(layers%data,2)) THEN
            IF (ASSOCIATED(layers%data(i,j)%Layer)) THEN
              IF (layer >= 1 .and. layer <= size(layers%data(i,j)%Layer)) THEN
                SELECT CASE (variable)
                CASE (Layer_b1)
                  layers%data(i,j)%layer(Layer)%b1=value
                CASE (Layer_m1)
                  layers%data(i,j)%layer(Layer)%m1=value
                CASE (Layer_n1)
                  layers%data(i,j)%layer(Layer)%n1=value
                CASE (Layer_b3)
                  layers%data(i,j)%layer(Layer)%b3=value
                CASE (Layer_m3)
                  layers%data(i,j)%layer(Layer)%m3=value
                CASE (Layer_dZ)
                  layers%data(i,j)%layer(Layer)%dZ=value
                CASE (Layer_s0max)
                  layers%data(i,j)%layer(Layer)%s0max=value
                CASE (Layer_Z)
                  layers%data(i,j)%layer(Layer)%Z=value
                CASE (Layer_Age)
                  layers%data(i,j)%layer(Layer)%Age=value
                CASE (Layer_Detachment)
                  layers%data(i,j)%layer(Layer)%Detach=value
                CASE (Layer_Tracer1:Layer_Tracer5)
                  if (variable-Layer_Tracer1+1 <= MaxNumberTracers) then
                    layers%data(i,j)%layer(Layer)%tracer(variable-Layer_Tracer1+1)=value
                  end if
                END SELECT
              ELSE
                write (*,*) Variable,i,j,Layer,value,ErrorNo
                write (*,*)size(layers%data(i,j)%Layer)
                CALL Message_Output(Message_ErrorStop                                 &
&                  ,'Invalid Layer No in LAYERSET: Layer= ',Layer)
                ErrorNo=1101
              END IF
            ELSE
            CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Layers not initialised before LAYERSET & LayerSetLayerDataR8 #1' &
&                  //': variable =',variable)
            ErrorNo=1100
            END IF
          ELSE
            CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Invalid Coordinates in LAYERSET: Layer=',Layer,' (x,y)=('        &
&                  ,i,',',j,')')
            ErrorNo=20
          END IF
        ELSE
            CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Layers not initialised before LAYERSET & LayerSetLayerDataR8 #2' &
&                  //': variable =',variable)
            ErrorNo=1100
        END IF
      CASE (Layer_FlowB1,Layer_FlowDetachment)
        IF (ASSOCIATED(flow%data)) THEN
          SELECT CASE (variable)
          CASE (Layer_Flowb1)
            flow%data(i,j)%b1=value
          CASE (Layer_FlowDetachment)
            flow%data(i,j)%detach=value
          END SELECT
        end if
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerSetLayerDataR8

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerSetStr(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      CHARACTER(*),INTENT(IN) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERSET = '   &
&                 ,Variable)
        ErrorNo=1
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerSetStr

! ============================================================================
!             GET CHARACTERISTICS OF LAYERS
! ============================================================================

  SUBROUTINE LayerGetI(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      INTEGER,INTENT(OUT) :: value,ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        value=0
        ErrorNo=1
      CASE(Layer_Number_Tracers)
        value=NumberTracers
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetR(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      REAL,INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      REAL(KIND(0.0D0)) :: dvalue

      ErrorNo=0
      CALL LayerGetR8(Variable,dvalue,ErrorNo)
      value=dvalue
      LastError=ErrorNo
  END SUBROUTINE LayerGetR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetR8(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      REAL(KIND(0.0D0)),INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        ErrorNo=1
      CASE(Layer_Resolution)
        value=LayerResolution
      CASE(Layer_Detachment)
        value=LayerDetachment
      CASE(Layer_Tracer1_Default:Layer_Tracer5_Default)
        value=TracerDefault(variable-Layer_Tracer1_Default+1)
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetR8

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetL(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      LOGICAL,INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        ErrorNo=1
      CASE (Layer_Module_Active)
        value=Module_Active
      CASE (Layer_Detachment_Active)
        if (Module_Active) then
          value=Detachment_Limitation
        else
          value=.false.
        end if
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetL

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetStr(Variable,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable
      CHARACTER(*),INTENT(OUT) :: Value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        ErrorNo=1
        value=''
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetStr

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetArrayI(Variable,i,j,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j
      INTEGER,INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        ErrorNo=1
      CASE(Layer_NoLayers)
        value=0
        IF (ASSOCIATED(layers%data)) THEN
          IF (ASSOCIATED(layers%data(i,j)%Layer)) THEN
            value=size(layers%data(i,j)%layer)
          END IF
        END IF
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetArrayI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetArrayR(Variable,i,j,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j
      REAL,INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo
      
      REAL(KIND(0.0D0)) :: dvalue

      CALL LayerGetArrayR8(Variable,i,j,dvalue,ErrorNo)
      value=dvalue
      LastError=ErrorNo
  END SUBROUTINE LayerGetArrayR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetArrayR8(Variable,i,j,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j
      REAL(KIND(0.0D0)),INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        ErrorNo=1
        value=0
!  note this same code can be called through the layerarray generic interface
      CASE (Layer_FlowB1,Layer_FlowDetachment)
        value=0
        IF (ASSOCIATED(flow%data)) THEN
          SELECT CASE (variable)
          CASE (Layer_Flowb1)
            value=flow%data(i,j)%b1
          CASE (Layer_FlowDetachment)
            value=flow%data(i,j)%detach
          CASE (Layer_FlowTracer1:Layer_FlowTracer5)
            if (variable-Layer_FlowTracer1+1 <= MaxNumberTracers) then
              value=flow%data(i,j)%tracer(variable-Layer_FlowTracer1+1)
            else
              value=0
            end if
          END SELECT
        end if
!  surface detachment rate
      CASE (Layer_Detachment)
        call LayerGetLayerDataR8(Layer_Detachment,i,j,1,value,ErrorNo)
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetArrayR8

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetLayerDataI(Variable,i,j,Layer,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j,Layer
      INTEGER,INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '   &
&                 ,Variable)
        Value=0
        ErrorNo=1
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetLayerDataI

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetLayerDataR(Variable,i,j,Layer,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j,Layer
      REAL,INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo
      
      REAL(KIND(0.0D0)) :: dvalue

      ErrorNo=0
      CALL LayerGetLayerDataR8(Variable,i,j,Layer,dvalue,ErrorNo)
      value=dvalue
      LastError=ErrorNo
  END SUBROUTINE LayerGetLayerDataR

! ----------------------------------------------------------------------------
! ----------------------------------------------------------------------------

  SUBROUTINE LayerGetLayerDataR8(Variable,i,j,Layer,value,ErrorNo)
    USE Support
    IMPLICIT NONE
      INTEGER,INTENT(IN) :: Variable,i,j,Layer
      REAL(KIND(0.0D0)),INTENT(OUT) :: value
      INTEGER,INTENT(OUT) :: ErrorNo

      ErrorNo=0
      SELECT CASE (variable)
      CASE DEFAULT
        CALL Message_Output(Message_ErrorContinue,'Invalid option in LAYERGET = '     &
&                 ,Variable)
        ErrorNo=1
      CASE (Layer_b1,Layer_m1,Layer_n1,Layer_b3,Layer_m3,Layer_dZ      &
&          ,Layer_s0max,Layer_Z,Layer_Age,Layer_Detachment,Layer_Tracer1:Layer_Tracer5)
        value=0
        IF (ASSOCIATED(layers%data)) THEN
          IF (i >= LBOUND(layers%data,1) .and. i <= UBOUND(layers%data,1)             &
&              .and. j >= LBOUND(layers%data,2) .and. j <= UBOUND(layers%data,2)) THEN
            IF (ASSOCIATED(layers%data(i,j)%Layer)) THEN
              IF (layer >= 1 .and. layer <= size(layers%data(i,j)%Layer)) THEN
                SELECT CASE (variable)
                CASE (Layer_b1)
                  value=layers%data(i,j)%layer(Layer)%b1
                CASE (Layer_m1)
                  value=layers%data(i,j)%layer(Layer)%m1
                CASE (Layer_n1)
                  value=layers%data(i,j)%layer(Layer)%n1
                CASE (Layer_b3)
                  value=layers%data(i,j)%layer(Layer)%b3
                CASE (Layer_m3)
                  value=layers%data(i,j)%layer(Layer)%m3
                CASE (Layer_dZ)
                  value=layers%data(i,j)%layer(Layer)%dZ
                CASE (Layer_s0max)
                  value=layers%data(i,j)%layer(Layer)%s0max
                CASE (Layer_Z)
                  value=layers%data(i,j)%layer(Layer)%Z
                CASE (Layer_Age)
                  value=layers%data(i,j)%layer(Layer)%Age
                CASE (Layer_Detachment)
                  value=layers%data(i,j)%layer(Layer)%Detach
                CASE (Layer_Tracer1:Layer_Tracer5)
                  if (variable-Layer_Tracer1+1 <= MaxNumberTracers) then
                    value=layers%data(i,j)%layer(Layer)%tracer(variable-Layer_Tracer1+1)
                  else
                    value=0
                  end if
                END SELECT
              ELSE
                write (*,*) Variable,i,j,Layer,value,ErrorNo
                write (*,*)size(layers%data(i,j)%Layer)
                CALL Message_Output(Message_ErrorStop                                 &
&                  ,'Invalid Layer No in LAYERGET: Layer= ',Layer)
                ErrorNo=1101
              END IF
            ELSE
            CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Layers not initialised before LAYERGET & LayerGetLayerDataR8 #1' &
&                  //': variable =',variable)
            ErrorNo=1100
            END IF
          ELSE
            CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Invalid Coordinates in LAYERGET: Layer=',Layer,' (x,y)=('        &
&                  ,i,',',j,')')
            ErrorNo=20
          END IF
        ELSE
            CALL Message_Output(Message_ErrorStop                                     &
&                  ,'Layers not initialised before LAYERGET & LayerGetLayerDataR8 #2' &
&                  //': variable =',variable)
            ErrorNo=1100
        END IF
      CASE (Layer_FlowB1,Layer_FlowDetachment)
        value=0
        IF (ASSOCIATED(flow%data)) THEN
          SELECT CASE (variable)
          CASE (Layer_Flowb1)
            value=flow%data(i,j)%b1
          CASE (Layer_FlowDetachment)
            value=flow%data(i,j)%detach
          END SELECT
        end if
      END SELECT
      LastError=ErrorNo
  END SUBROUTINE LayerGetLayerDataR8


! ============================================================================
!             CREATE A LAYER
! ============================================================================
!             Add a layer to all points in the domain

  SUBROUTINE LayerAddLayerArray(z,lowX,highX,LowY,HighY,b1,age,tracer,GridX,GridY)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: lowX,highX,LowY,HighY,GridX,GridY
    REAL(KIND(0.0D0)) :: b1
    REAL(KIND(0.0D0)), OPTIONAL :: age
    REAL(KIND(0.0D0)), OPTIONAL,dimension(1:NumberTracers) :: tracer
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: z
! 
    INTEGER :: ii,i,j,size,ErrorNo
    LOGICAL :: ValidData
    TYPE(Layers_XY) :: temp1

      IF (.not.Module_Active) RETURN
!       check that the original array has been allocated
      IF (.not.ASSOCIATED(layers%data)) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array must be  '            &
&               //'initialised before LAYER_ADDLAYER')
      END IF
!       if the array has been allocated check that it is big enough
      IF (LBOUND(layers%data,1) > LowX .or. UBOUND(layers%data,1) < HighX             &
&          .or. LBOUND(layers%data,2) > LowY .or. UBOUND(layers%data,2) < HighY) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array bounds ('             &
&             ,LBOUND(layers%data,1),',',UBOUND(layers%data,1),')x('                &
&             ,LBOUND(layers%data,2),',',UBOUND(layers%data,2)                      &
&             ,') too small for requested data ('                                   &
&             ,LowX,',',HighX,')x(',LowY,',',HighY,')')
      END IF
      CALL Message_Output(Message_Info,'-- Adding Layer')
!       OK layer array is good ... add the data
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF (ASSOCIATED(layers%data(i,j)%Layer)) then
            ValidData=z(i,j) > layers%data(i,j)%Layer(1)%z
          ELSE
            ValidData=.true.
          END IF
          IF (validData) then
            IF (ASSOCIATED(layers%data(i,j)%Layer)) then
              size=UBOUND(layers%data(i,j)%Layer,1)
              ALLOCATE(temp1%layer(size),stat=ErrorNo)
              IF (ErrorNo /= 0) CALL AllocationError                                   &
&                                 ('Layer_Data_temp1','LayerAddLayerArray')
              temp1%layer=layers%data(i,j)%Layer
              DEALLOCATE(layers%data(i,j)%Layer)
              ALLOCATE(layers%data(i,j)%Layer(size+1),stat=ErrorNo)
              IF (ErrorNo /= 0) CALL AllocationError                                  &
&                                 ('Layer_Data_Expand','LayerAddLayerArray')
              layers%data(i,j)%Layer(1:size)=temp1%layer(1:size)
            ELSE
              ALLOCATE(layers%data(i,j)%Layer(1),stat=ErrorNo)
              IF (ErrorNo /= 0) CALL AllocationError                                  &
&                                 ('Layer_Data_Init','LayerAddLayerArray')
            END IF
!                   push all layers down one
            layers%data(i,j)%Layer(2:size+1)=temp1%Layer(1:size)
            call Layer_InitLayer(layers%data(i,j)%layer(1))
!                   layer 1 is the top layer
            layers%data(i,j)%Layer(1)%z=z(i,j)
            layers%data(i,j)%Layer(1)%b1=b1
            IF (PRESENT(age)) THEN
              layers%data(i,j)%Layer(1)%age=age
            ELSE
              layers%data(i,j)%Layer(1)%age=-1
            END IF
            if (Detachment_Limitation) then
              layers%data(i,j)%Layer(1)%detach=LayerDetachment
            end if
            IF (PRESENT(tracer)) THEN
              layers%data(i,j)%Layer(1)%tracer(1:NumberTracers)=tracer(1:NumberTracers)
            else
              layers%data(i,j)%Layer(1)%tracer(1:NumberTracers)=tracerdefault(1:NumberTracers)
            end if
          END IF
        END DO
      END DO
  END SUBROUTINE LayerAddLayerArray

! ----------------------------------------------------------------------------
!   Add an extra layer to just a single pt in the domain
! ----------------------------------------------------------------------------

  SUBROUTINE LayerAddLayerPt(z,X,Y,b1,age,detach,tracer,mandatory)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: X,Y
    REAL(KIND(0.0D0)) :: b1,Z
    REAL(KIND(0.0D0)), OPTIONAL :: age,detach
    REAL(KIND(0.0D0)), OPTIONAL, dimension(1:NumberTracers) :: tracer
    LOGICAL, OPTIONAL :: mandatory
! 
    INTEGER :: ii,size,ErrorNo
    LOGICAL :: ValidData
    TYPE(Layers_XY) :: temp1

      IF (.not.Module_Active) RETURN
!       check that the original array has been allocated
      IF (.not.ASSOCIATED(layers%data)) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array must be  '            &
&               //'initialised before LAYER_ADDLAYER')
      END IF
!       if the array has been allocated check that it is big enough
      IF (LBOUND(layers%data,1) > X .or. UBOUND(layers%data,1) < X             &
&        .or. LBOUND(layers%data,2) > Y .or. UBOUND(layers%data,2) < Y) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array bounds ('             &
&             ,LBOUND(layers%data,1),',',UBOUND(layers%data,1),')x('                &
&             ,LBOUND(layers%data,2),',',UBOUND(layers%data,2)                      &
&             ,') too small for requested data (',X,',',Y,')')
      END IF
      IF (PRESENT(mandatory)) THEN
        ValidData=mandatory
      ELSE
        IF (ASSOCIATED(layers%data(X,Y)%Layer)) THEN
          ValidData=z >= layers%data(X,Y)%Layer(1)%z
        ELSE
          ValidData=.true.
        END IF
      END IF
!	  write (*,*) 'LayerAddLayerPt here 1'
      IF (validData) THEN
        IF (ASSOCIATED(layers%data(X,Y)%Layer)) THEN
          size=UBOUND(layers%data(X,Y)%Layer,1)
!	  write (*,*) 'LayerAddLayerPt here 2',size,x,y
          ALLOCATE(temp1%layer(size),stat=ErrorNo)
          IF (ErrorNo /= 0) CALL AllocationError                                  &
&                                 ('Layer_Data_temp1','LayerAddLayerPt')
          temp1%layer=layers%data(X,Y)%Layer
!	  write (*,*) 'LayerAddLayerPt here 3'
          DEALLOCATE(layers%data(X,Y)%Layer,stat=errorno)
!	  write (*,*) 'LayerAddLayerPt here 4',errorno
          ALLOCATE(layers%data(X,Y)%Layer(size+1),stat=ErrorNo)
!	  write (*,*) 'LayerAddLayerPt here 5',errorno
          IF (ErrorNo /= 0) then
            CALL AllocationError('Layer_Data_Expand','LayerAddLayerPt')
          end if
          layers%data(x,y)%Layer(2:size+1)=temp1%Layer(1:size)
!	  write (*,*) 'LayerAddLayerPt here 5a'
          DEALLOCATE(temp1%layer,stat=errorno)
!	  write (*,*) 'LayerAddLayerPt here 5b',errorno
        ELSE
!	  write (*,*) 'LayerAddLayerPt here 6'
          ALLOCATE(layers%data(X,Y)%Layer(1),stat=ErrorNo)
!	  write (*,*) 'LayerAddLayerPt here 7'
          IF (ErrorNo /= 0) CALL AllocationError                                  &
&                                 ('Layer_Data_Init','LayerAddLayerPt')
        END IF
!           push all layers down one
!        layers%data(x,y)%Layer(2:size+1)=temp1%Layer(1:size)
!           layer 1 is the new layer
!	  write (*,*) 'LayerAddLayerPt here 8'
        call Layer_InitLayer(layers%data(X,Y)%layer(1))
!	  write (*,*) 'LayerAddLayerPt here 9'
        layers%data(X,Y)%Layer(1)%z=Z
        layers%data(X,Y)%Layer(1)%b1=b1
        IF (PRESENT(age)) THEN
          layers%data(X,Y)%Layer(1)%age=age
        ELSE
          layers%data(X,Y)%Layer(1)%age=-1
        END IF
        if (Detachment_Limitation) then
          IF (PRESENT(detach)) THEN
            layers%data(X,Y)%Layer(1)%detach=detach
          ELSE
            layers%data(X,Y)%Layer(1)%detach=LayerDetachment
          END IF
        end if
        IF (PRESENT(tracer)) THEN
          layers%data(x,y)%Layer(1)%tracer(1:NumberTracers)=tracer(1:NumberTracers)
        else
          layers%data(x,y)%Layer(1)%tracer(1:NumberTracers)=tracerdefault(1:NumberTracers)
        end if
      END IF
!	  write (*,*) 'LayerAddLayerPt here 10'
!      deallocate(temp1%layer)
!	  write (*,*) 'LayerAddLayerPt here 11'
  END SUBROUTINE LayerAddLayerPt

! ----------------------------------------------------------------------------
!   Remove layers for a single pt in the domain down to the level of z
! ----------------------------------------------------------------------------

  SUBROUTINE LayerDeleteLayerPt(z,X,Y)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: X,Y
    REAL(KIND(0.0D0)) :: Z
! 
    INTEGER :: i,DeleteDownTo,NoLayers,NoNewLayers,ErrorNo
    TYPE(Layers_XY) :: temp1

      IF (.not.Module_Active) RETURN
!       check that the original array has been allocated
      IF (.not.ASSOCIATED(layers%data)) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array must be  '            &
&               //'initialised before LAYER_ADDLAYER')
      END IF
!       if the array has been allocated check that it is big enough
      IF (LBOUND(layers%data,1) > X .or. UBOUND(layers%data,1) < X             &
&        .or. LBOUND(layers%data,2) > Y .or. UBOUND(layers%data,2) < Y) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array bounds ('             &
&             ,LBOUND(layers%data,1),',',UBOUND(layers%data,1),')x('                &
&             ,LBOUND(layers%data,2),',',UBOUND(layers%data,2)                      &
&             ,') too small for requested data (',X,',',Y,')')
      END IF
!      determine what layers need to be deleted
      DeleteDownTo=1
      NoLayers=ubound(layers%data(x,y)%layer,1)
      DO i=1,NoLayers
        IF (Z > layers%data(x,y)%layer(i)%z) EXIT
        DeleteDownTo=i+1
      END DO
!       erosion has not penetrated the top layer so no need to delete the top layer
!       note this code also ensures that there is at least one layer
      if (DeleteDownTo == 1) RETURN
!       OK lets delete some layers
      NoNewLayers=NoLayers-DeleteDownTo+1
      ALLOCATE(temp1%layer(NoNewLayers),stat=ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError('Layer_Data_temp1','LayerDeleteLayerPt')
      temp1%layer(1:NoNewLayers)=layers%data(X,Y)%Layer(DeleteDownTo:NoLayers)
      DEALLOCATE(layers%data(X,Y)%Layer)
      ALLOCATE(layers%data(X,Y)%Layer(NoNewLayers),stat=ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError('Layer_Data_Delete','LayerDeleteLayerPt')
      layers%data(X,Y)%Layer(1:NoNewLayers)=temp1%layer(1:NoNewLayers)
      DEALLOCATE(temp1%Layer)
  END SUBROUTINE LayerDeleteLayerPt

! ============================================================================
!             GENERIC SEDIMENTATION INTERFACE
! ============================================================================

  SUBROUTINE Layer_Sedimentation(SimTime,Z,DeltaZ,Domain,Direct,GridX,GridY,LowX,HighX,LowY,HighY,b1,age,tracer)
!    This subroutine calculates the adjustments required to the soil layers so that they
!    are consistent with the erosion/deposition that is calculated by the mass balance SEDANAL.
!    First the subrutine calculates the sediment in the flow properties so that the material deposited
!    reflects what is in the flow. The the layers are adjusted based on the mass balance
!
!    NB ... In this subroutine we are post-processing elevations that have already been adjusted 
!           so that the elevations passed in though Z are the elevations AFTER erosion/deposition
!           and for the layer updating we need the elevations prior to the updating
!           so we need to pass in the layer routines Z-DeltaZ not Z for the elevations

    USE SiberiaTypes
    USE Support
    IMPLICIT NONE
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    INTEGER,DIMENSION(GridX,GridY) :: Direct
    REAL(KIND(0.0D0)) :: SimTime
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Z,b1
    REAL(KIND(0.0D0)),OPTIONAL,DIMENSION(LowX:HighX,LowY:HighY) :: age
    REAL(KIND(0.0D0)),OPTIONAL,DIMENSION(1:NumberTracers,LowX:HighX,LowY:HighY) :: tracer
    LOGICAL,DIMENSION(GridX,GridY) :: Domain
    TYPE(ArrayR8XY) :: DeltaZ

    INTEGER :: i,j

      if (.not.Module_Active) return
      Call Layer_FlowTracking(DeltaZ,Direct,domain,LowX,HighX,LowY,HighY,GridX,GridY)
      DO j=LowY,HighY
        DO i=LowX,HighX
! NOTE: Every node must be processed by one of erode or deposition otherwise layers
!       will be incorrectly handled.
          IF (DeltaZ%data(i,j) > 0.0d0) THEN
!  deposition
            CALL Layer_DepositSediment(Z(i,j)-DeltaZ%data(i,j),DeltaZ%data(i,j),i,j)
          ELSE
!  erosion
            CALL Layer_ErodeSediment(Z(i,j)-DeltaZ%data(i,j),DeltaZ%data(i,j),i,j,b1(i,j),age(i,j))
          END IF
        END DO
      END DO
      call cosmogenics(SimTime,Z,Domain,GridX,GridY,LowX,HighX,LowY,HighY)
      RETURN
  END SUBROUTINE Layer_Sedimentation


! ============================================================================
!                FLOW ATTRIBUTES TRACKING
! ============================================================================

  subroutine Layer_FlowTracking(Erosion,Direct,domain,LowX,HighX,LowY,HighY,GridX,GridY)
  
!  This routine works out the characteristics of the sediment in flow based on the eroded and
!  deposited sediment. There are a number of limitations in this routine.
!  1. Eroded sediment charcateristics are considered to be identical to the top layer. For small
!     erosions/timestep this is fine but if the code has a large enough timestep that in one timestep
!     it erodes more than one layer then this approach is inaccurate. That is, the approach in this
!     routine is OK for small timesteps but may be suspect for large timesteps.
!  2. The solution for the entrained sediment is a steady state solution. That is no account is made
!     of the sediment that was entrained in the previous timestep when mixing in the sediment that
!     is eroded or deposited. This assumes that flow/erosion conditions change slowly with time.
!
!  Computational NB: This subroutine assumes that Domain is always correctly set (it doesn't have
!      seperate section  of code for regular boundaries).

    USE SiberiaConstants
    USE SiberiaTypes
    USE Support
    implicit none
    integer :: LowX,HighX,LowY,HighY,GridX,GridY
    integer,DIMENSION(GridX,GridY) :: Direct
    TYPE(ArrayR8XY) :: Erosion
    logical,DIMENSION(GridX,GridY) :: domain

    integer :: i,j,k,ii,jj,NoSources,Oldi,Oldj,errorno
    REAL(KIND(0.0D0)) :: b1temp,DetachTemp,sum,TracerTemp
!  FluxDt the amount of material being transported in this timestep 
!    ... note this may be different from the flux equations because of the nonlinear solver 
!    ... we use this this defn here to ensure a mass balance and internal consistency.
    REAL(KIND(0.0D0)),dimension(LowX:HighX,LowY:HighY) :: FluxDt
    INTEGER,dimension(LowX:HighX,LowY:HighY) :: NoIn
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY

      if (.not.Module_Active) return
      NoSources=0
      sum=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          FluxDt(i,j)=0
!          FluxDt(i,j)=-Erosion%data(i,j)
          sum=sum+Erosion%data(i,j)
          if (Erosion%data(i,j) <= 0.0d0) then
            call Layer_Get(Layer_b1,i,j,1,b1temp,errorno)
            flow%data(i,j)%b1=b1temp
            call Layer_Get(Layer_detachment,i,j,1,Detachtemp,errorno)
            flow%data(i,j)%detach=Detachtemp
            do ii=1,NumberTracers
              call Layer_Get(Layer_Tracer(ii),i,j,1,tracertemp,errorno)
              flow%data(i,j)%tracer(ii)=tracertemp
            end do
!
            FluxDt(i,j)=-Erosion%data(i,j)
          else
!  set -ve (i.e. infeasible value) so we know the first time we go through a node
            flow%data(i,j)%b1=-1.0
            flow%data(i,j)%detach=-1.0
          end if
        END DO
      END DO
! 
!  Find the list of the most upstream points of the networks
! 
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
! 
!  Calc the areas from top TO bottom
! 
      DO k=1,NoSources
        Oldi=SourceX(k)
        Oldj=SourceY(k)
!  handle source node
        call Layer_Get(Layer_b1,Oldi,Oldj,1,b1temp,errorno)
        flow%data(Oldi,Oldj)%b1=b1temp
        call Layer_Get(Layer_Detachment,Oldi,Oldj,1,Detachtemp,errorno)
        flow%data(Oldi,Oldj)%detach=Detachtemp
        do ii=1,NumberTracers
            call Layer_Get(Layer_Tracer(ii),Oldi,Oldj,1,tracertemp,errorno)
            flow%data(Oldi,Oldj)%tracer(ii)=tracertemp
        end do
        FluxDt(Oldi,Oldj)=max(0.0d0,-Erosion%data(Oldi,Oldj))
!  handle downstream nodes
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
!  handle a null pit with nothing draining into it (so can't use upstream values)
        if (i == Oldi.and.j == Oldj) THEN
          call Layer_Get(Layer_b1,i,j,1,b1temp,errorno)
          flow%data(i,j)%b1=b1temp
          call Layer_Get(Layer_Detachment,i,j,1,Detachtemp,errorno)
          flow%data(i,j)%detach=Detachtemp
          do ii=1,NumberTracers
              call Layer_Get(Layer_Tracer(ii),i,j,1,tracertemp,errorno)
              flow%data(i,j)%tracer(ii)=tracertemp
          end do
!
          FluxDt(i,j)=max(0.0d0,-Erosion%data(i,j))
        else
!  adjust characteristic for property being entrained 
 1060     continue
          if (Erosion%data(i,j) <= 0.0) then
            flow%data(i,j)%b1=Layer_B1MixingModel(flow%data(i,j)%b1,FluxDt(i,j)    &
&                    ,flow%data(Oldi,Oldj)%b1,FluxDt(Oldi,Oldj),i,j)
            flow%data(i,j)%detach=Layer_LinearMixingModel(flow%data(i,j)%detach,FluxDt(i,j)    &
&                    ,flow%data(Oldi,Oldj)%detach,FluxDt(Oldi,Oldj),i,j)
            do ii=1,NumberTracers
              flow%data(i,j)%tracer(ii)=Layer_LinearMixingModel(flow%data(i,j)%tracer(ii),FluxDt(i,j)    &
&                    ,flow%data(Oldi,Oldj)%tracer(ii),FluxDt(Oldi,Oldj),i,j)
            end do
!  do the check for 0.0 just to eliminate numerical effects
            FluxDt(i,j)=max(0.0d0,FluxDt(i,j)+FluxDt(Oldi,Oldj))
! deposition
          else 
            if (flow%data(i,j)%b1 < 0.0 ) then
!  first inflow into the node ... initialise
              flow%data(i,j)%b1=flow%data(oldi,oldj)%b1
              flow%data(i,j)%detach=flow%data(oldi,oldj)%detach
              flow%data(i,j)%tracer(1:NumberTracers)=flow%data(oldi,oldj)%tracer(1:NumberTracers)
              FluxDt(i,j)=FluxDt(Oldi,Oldj)    ! note that this is required for the mixing model
            else
!  rest of inflows just use the mixing model
              flow%data(i,j)%b1=Layer_B1MixingModel(flow%data(i,j)%b1,FluxDt(i,j)    &
&                    ,flow%data(Oldi,Oldj)%b1,FluxDt(Oldi,Oldj),i,j)
              flow%data(i,j)%detach=Layer_LinearMixingModel(flow%data(i,j)%detach,FluxDt(i,j)    &
&                    ,flow%data(Oldi,Oldj)%detach,FluxDt(Oldi,Oldj),i,j)
              do ii=1,NumberTracers
                flow%data(i,j)%tracer(ii)=Layer_LinearMixingModel(flow%data(i,j)%tracer(ii),FluxDt(i,j)    &
&                    ,flow%data(Oldi,Oldj)%tracer(ii),FluxDt(Oldi,Oldj),i,j)
              end do
              FluxDt(i,j)=max(0.0d0,FluxDt(i,j)+FluxDt(Oldi,Oldj))
            end if
          end if
          IF (NoIn(i,j) == 1) THEN
!  if all inflows into node processed proceed downstream
!  this is required for mass balance downstream
            if (Erosion%data(i,j) > 0.0) then
              FluxDt(i,j)=max(0.0d0,FluxDt(i,j)-Erosion%data(i,j))
            end if
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            IF (i /= Oldi.or.j /= Oldj) GO TO 1060
          ELSE
!  if there are still some unprocessed inflows to node ... stop ... and go to next source
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
  end subroutine Layer_flowtracking



  subroutine Layer_FlowTrackingDep(sed,Erosion,Direct,domain,LowX,HighX,LowY,HighY,GridX,GridY)

!  This subroutine simply tracks the amount of sediment in the flow for the case of detachment 
!  limitation (i.e. when sedimentation doesn't occur).
!
!  NB This subroutine assumes that Domain is always correctly set

    USE SiberiaConstants
    USE SiberiaTypes
    USE Support
    implicit none
    integer :: LowX,HighX,LowY,HighY,GridX,GridY
    integer,DIMENSION(GridX,GridY) :: Direct
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: sed
    TYPE(ArrayR8XY) :: Erosion
    logical,DIMENSION(GridX,GridY) :: domain

    integer :: i,j,k,ii,jj,NoSources,Oldi,Oldj
    REAL(KIND(0.0D0)) :: sum
!  FluxDt the amount of material being transported in this timestep 
!    ... note this may be different from the flux equations because of the nonlinear solver 
!    ... we use this this defn here to ensure a mass balance and internal consistency.
    REAL(KIND(0.0D0)),dimension(LowX:HighX,LowY:HighY) :: FluxDt
    INTEGER,dimension(LowX:HighX,LowY:HighY) :: NoIn
    integer,dimension((HighX-LowX+1)*(HighY-LowY+1)) :: SourceX,SourceY

      NoSources=0
      sum=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          NoIn(i,j)=0
          FluxDt(i,j)=0
          FluxDt(i,j)=-Erosion%data(i,j)
          sum=sum+Erosion%data(i,j)
        END DO
      END DO
! 
!  Find the list of the most upstream points of the networks
! 
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
! 
!  Calc the areas from top TO bottom
! 
      DO k=1,NoSources
        Oldi=SourceX(k)
        Oldj=SourceY(k)
        i=SourceX(k)+dir1(Direct(Oldi,Oldj))
        j=SourceY(k)+dir2(Direct(Oldi,Oldj))
!  handle a null pit with nothing draining into it (so can't use upstream values)
        if (i == Oldi.and.j == Oldj) THEN
!  A single node cannot have deposition
          if (Erosion%data(i,j) > 0) then
            Erosion%data(i,j)=0
          end if
          if (sed(i,j) > 0) then
            sed(i,j)=0
          end if
          FluxDt(i,j)=-Erosion%data(i,j)
        else
!  adjust characteristic for property being entrained 
 1060     if (Erosion%data(i,j) <= 0.0) then
            FluxDt(i,j)=max(0.0d0,FluxDt(i,j)+FluxDt(Oldi,Oldj))
! deposition
          else
            if (FluxDt(i,j)+FluxDt(Oldi,Oldj) < 0) then 
              Erosion%data(i,j)=FluxDt(Oldi,Oldj)
              sed(i,j)=FluxDt(Oldi,Oldj)
              FluxDt(i,j)=0
            else
              FluxDt(i,j)=max(0.0d0,FluxDt(i,j)+FluxDt(Oldi,Oldj))
            end if
          end if
          IF (NoIn(i,j) == 1) THEN
!  if all inflows into node processed proceed downstream
!  this is required for mass balance downstream
            if (Erosion%data(i,j) > 0.0) then
              if (FluxDt(i,j)+Erosion%data(i,j) < 0) then
                FluxDt(i,j)=0
                Erosion%data(i,j)=FluxDt(Oldi,Oldj)
                sed(i,j)=FluxDt(Oldi,Oldj)
              end if
            end if
            Oldi=i
            Oldj=j
            i=i+dir1(Direct(Oldi,Oldj))
            j=j+dir2(Direct(Oldi,Oldj))
            IF (i /= Oldi.or.j /= Oldj) GO TO 1060
          ELSE
!  if there are still some unprocessed inflows to node ... stop ... and go to next source
            NoIn(i,j)=NoIn(i,j)-1
          END IF
        END IF
      END DO
  end subroutine Layer_flowtrackingDep

! ============================================================================
!             ADD DEPOSITED SEDIMENT TO A LAYER
! ============================================================================

  SUBROUTINE Layer_DepositSediment(Z,DeltaZ,X,Y,b1,age,tracer,detach)
    USE Support
    IMPLICIT NONE
      INTEGER :: X,Y
      REAL(KIND(0.0D0)) :: Z,DeltaZ
      REAL(KIND(0.0D0)),OPTIONAL :: age,b1,detach
      REAL(KIND(0.0D0)),OPTIONAL,dimension(1:NumberTracers) :: tracer

      REAL(KIND(0.0D0)) :: tempZ,tempB1,tempAge,TempDeltaZ,tDeltaZ,tempDetach
      REAL(KIND(0.0D0)),dimension(1:NumberTracers) :: tempTracer
      LOGICAL :: CreateLayer

      IF (.not.Module_Active) RETURN
!       check that the original array has been allocated
      IF (.not.ASSOCIATED(layers%data)) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array must be  '            &
&               //'initialised before LAYER_ADDLAYER')
      END IF
!       if the array has been allocated check that it is big enough
      IF (LBOUND(layers%data,1) > X .or. UBOUND(layers%data,1) < X             &
&          .or. LBOUND(layers%data,2) > Y .or. UBOUND(layers%data,2) < Y) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data array bounds ('             &
&             ,LBOUND(layers%data,1),',',UBOUND(layers%data,1),')x('                &
&             ,LBOUND(layers%data,2),',',UBOUND(layers%data,2)                      &
&             ,') too small for requested data (',X,',',Y,')')
      END IF
      tDeltaZ=DeltaZ
      IF (SIZE(layers%data(x,y)%layer) ==0) THEN
        CALL Message_Output(Message_ErrorStop,'Layer data not initialised')
      END IF
!       the arrays look OK
      CreateLayer=.false.
      IF (SIZE(layers%data(x,y)%layer) ==1) THEN
!  if there is only one layer then this must be semi-infinite so we must create a 2nd layer
!  of finite depth to allow the mixing model to work correctly.
        tempZ=z
        
!  NOTE: Assume that if we are interested in tracers then we MUST be doing flow tracking (otherwise the whole thing
!        makes no sense). but we have have some stupid dummy code just in case.
        if (present(b1))  then
! assume if b1 is specified that age is as well
          tempB1=b1
          tempAge=age
          tempDetach=detach
        else
          tempB1=flow%data(x,y)%b1
          tempDetach=flow%data(x,y)%detach
          tempAge=flow%data(x,y)%age
        end if
        if (present(tracer))  then
          temptracer(1:NumberTracers)=tracer(1:NumberTracers)
        else
          tempTracer(1:NumberTracers)=flow%data(x,y)%Tracer(1:NumberTracers)
        end if
        call LayerAddLayerPt(tempZ,x,y,tempB1,tempAge,tempdetach,temptracer,Mandatory=.true.)
      ELSE
        if (present(b1)) then
          tempB1=b1
        else
          tempB1=flow%data(x,y)%b1
        end if
        if (present(age)) then
          tempAge=age
        else
          tempAge=flow%data(x,y)%age
        end if
        if (present(detach)) then
          tempDetach=detach
        else
          tempDetach=flow%data(x,y)%detach
        end if
        if (present(tracer)) then
          tempTracer=detach
        else
          tempTracer(1:NumberTracers)=flow%data(x,y)%tracer(1:NumberTracers)
        end if
!  if the existing layer is already thicker than layerResolution
        IF ((Z-layers%data(x,y)%layer(1)%z) > LayerResolution) THEN
          tempZ=Z
          CALL LayerAddLayerPt(tempZ,x,y,tempB1,tempAge,tempDetach              &
&             ,temptracer,Mandatory=.true.)
!  if layer is less than resolution but resulting would be greater than
        ELSE IF ((Z-layers%data(x,y)%layer(1)%z+tDeltaZ) > LayerResolution) THEN
!         note if after deposition the current layer is > layer resolution the code will 
!         fill up the existing layer and then create a single layer
!         with depth that may exceed the specified layer resolution 
!                ... generally this should be OK but if its a problem you
!                    really ought to be using smaller timesteps

!   fill up current top layer
          TempDeltaZ=LayerResolution-(Z-layers%data(x,y)%layer(1)%z)
          layers%data(x,y)%layer(1)%b1=Layer_B1MixingModel                      &
&            (layers%data(x,y)%layer(1)%b1                                      &
&            ,z-layers%data(x,y)%layer(1)%z                                     &
&            ,tempb1,TempDeltaZ,x,y)
          IF (PRESENT(age)) THEN
            layers%data(x,y)%layer(1)%age=(layers%data(x,y)%layer(1)%age        &
&             *(layers%data(x,y)%layer(1)%z-layers%data(x,y)%layer(2)%z)        &
&             +tempage*TempDeltaZ)/LayerResolution
          END IF
          IF (PRESENT(tracer)) THEN
            layers%data(x,y)%layer(1)%tracer(1:NumberTracers)=                  &
&             (layers%data(x,y)%layer(1)%tracer(1:NumberTracers)                &
&             *(layers%data(x,y)%layer(1)%z-layers%data(x,y)%layer(2)%z)        &
&             +temptracer(1:NumberTracers)*TempDeltaZ)/LayerResolution
          END IF
          IF (PRESENT(detach)) THEN
            layers%data(x,y)%layer(1)%detach=(layers%data(x,y)%layer(1)%detach  &
&             *(layers%data(x,y)%layer(1)%z-layers%data(x,y)%layer(2)%z)        &
&             +tempdetach*TempDeltaZ)/LayerResolution
          END IF
!  base of layer current surface elevation
          tempZ=z+TempDeltaZ
!          if (present(b1)) then
!           tempB1=b1
!         else
!           tempB1=flow%data(x,y)%b1
!         end if
!          if (present(age)) then
!           tempAge=age
!         else
!           tempAge=flow%data(x,y)%age
!         end if
          CALL LayerAddLayerPt(tempZ,x,y,tempB1,tempAge,tempDetach,tempTracer   &
&             ,Mandatory=.true.)
!         tdeltaz=tdeltaz-tempDeltaZ
!       ELSE
!         tempZ=Z
        END IF
!             mix into existing top layer
!        temp=layers%data(x,y)%layer(1)%z+tDeltaZ
!        layers%data(x,y)%layer(1)%b1=Layer_B1MixingModel                      &
!&          (layers%data(x,y)%layer(1)%b1                                           &
!&          ,tempZ-layers%data(x,y)%layer(1)%z                &
!&          ,tempb1,tDeltaZ,x,y)
!        IF (PRESENT(age)) THEN
!          layers%data(x,y)%layer(1)%age=(layers%data(x,y)%layer(1)%age                &
!&           *(layers%data(x,y)%layer(1)%z-layers%data(x,y)%layer(2)%z)                &
!&           +Tempage*tdeltaz)/(temp-layers%data(x,y)%layer(2)%z,x,y)
!       END IF
      END IF
  END SUBROUTINE Layer_DepositSediment

! ============================================================================
!             ERODE SEDIMENT FROM A LAYER
! ============================================================================

  SUBROUTINE Layer_ErodeSediment(Z,DeltaZ,X,Y,b1,Age,tracer)

!   this sediment erodes sediment, removes layers when they are eroded and returns
!   the average b1,age, and tracer (note detach is missing) of the eroded layers 
!   (must implement these results for flow tracking when I get a chance so that its OK
!    for large timesteps).

!  DeltaZ is assumed to be -ve
    USE Support
    IMPLICIT NONE
      INTEGER :: X,Y
      REAL(KIND(0.0D0)) :: Z,DeltaZ,b1
      REAL(KIND(0.0D0)),OPTIONAL :: Age
      REAL(KIND(0.0D0)),OPTIONAL,dimension(1:NumberTracers) :: tracer

      INTEGER :: FinalLayer,i,ii
      REAL(KIND(0.0D0)) :: tempZ,tempB1,tempAge,FinalZ,LayerZ                   &
&         ,temptracer(1:NumberTracers)

    IF (.not.Module_Active) RETURN
!     check that the original array has been allocated
    IF (.not.ASSOCIATED(layers%data)) THEN
      CALL Message_Output(Message_ErrorStop,'Layer data array must be  '        &
&               //'initialised before LAYER_ADDLAYER')
    END IF
!     if the array has been allocated check that it is big enough
    IF (LBOUND(layers%data,1) > X .or. UBOUND(layers%data,1) < X                &
&      .or. LBOUND(layers%data,2) > Y .or. UBOUND(layers%data,2) < Y) THEN
      CALL Message_Output(Message_ErrorStop,'Layer data array bounds ('         &
&             ,LBOUND(layers%data,1),',',UBOUND(layers%data,1),')x('            &
&             ,LBOUND(layers%data,2),',',UBOUND(layers%data,2)                  &
&             ,') too small for requested data (',X,',',Y,')')
    END IF
    IF (SIZE(layers%data(x,y)%layer) ==0) THEN
      CALL Message_Output(Message_ErrorStop,'Layer data not initialised')
    END IF
!   the arrays look OK
!    IF ((Z-layers%data(x,y)%layer(1)%z) > ABS(DeltaZ)) THEN
    IF (Z+DeltaZ > layers%data(x,y)%layer(1)%z) THEN
!  all the sediment erodes form the topmost layer
      b1=layers%data(x,y)%layer(1)%b1
      IF (PRESENT(age)) THEN
        Age=layers%data(x,y)%layer(1)%age
      END IF
      IF (PRESENT(tracer)) THEN
        tracer(1:NumberTracers)=layers%data(x,y)%layer(1)%tracer(1:NumberTracers)
      END IF
    ELSE
!  check how many layers down we will be eroding
      finalZ=Z+DeltaZ
!  loop=2 => final height must be lower than surface
      FinalLayer=2
      DO i=2,SIZE(layers%data(x,y)%layer)-1
        if (finalZ > layers%data(x,y)%layer(i)%z) EXIT
        FinalLayer=i+1
!       FinalLayer=i
      END DO
      tempZ=Z-DeltaZ-layers%data(x,y)%layer(1)%z
      tempB1=layers%data(x,y)%layer(1)%b1
      tempAge=layers%data(x,y)%layer(1)%Age
      DO i=2,FinalLayer
        IF (i /= FinalLayer) THEN
          layerZ=layers%data(x,y)%layer(i-1)%z-layers%data(x,y)%layer(i)%z
        ELSE
          layerZ=finalZ-layers%data(x,y)%layer(FinalLayer)%z
        END IF
        if (tempZ <0) then
        end if
        tempB1=Layer_B1MixingModel                                              &
&                   (layers%data(x,y)%layer(i)%b1,LayerZ,tempb1,tempZ,x,y)
        IF (PRESENT(age)) THEN
          tempAge=Layer_LinearMixingModel                                       &
&                   (layers%data(x,y)%layer(i)%Age,LayerZ,tempAge,tempZ,x,y)
        END IF
        IF (PRESENT(tracer)) THEN
          do ii=1,NumberTracers 
            temptracer(ii)=Layer_LinearMixingModel                              &
&                   (layers%data(x,y)%layer(i)%tracer(ii),LayerZ                &
&                    ,temptracer(ii),tempZ,x,y)
          end do
        END IF
        tempZ=tempZ+layerZ
      END DO
      b1=tempB1
      IF (PRESENT(age)) THEN
        age=tempAge
      END IF
      CALL LayerDeleteLayerPt(finalZ,x,y)
    END IF
  END SUBROUTINE Layer_ErodeSediment

! ============================================================================
!              SEDIMENT MIXING MODELS
! ============================================================================

  REAL(KIND(0.0D0)) FUNCTION Layer_LinearMixingModel(stuff1,z1,stuff2,z2,i,j)
!  this subroutine assumes that both z11 and z12 are +ve
    USE Support
    IMPLICIT NONE
      REAL(KIND(0.0D0)) :: stuff1,z1,stuff2,z2
      integer :: i,j

      if (stuff1 == stuff2) then
        Layer_LinearMixingModel=stuff1
      else
        if (stuff1+stuff2 /= 0.0d0) then
          Layer_LinearMixingModel=(stuff1*z1+stuff2*z2)/(z1+z2)
        else
          Layer_LinearMixingModel=0.5*(stuff1+stuff2)
        end if
      end if
      if (z1 < 0.0 .or. z2 < 0.0) then
        call Message_Output                                                     &
&          (Message_Warning,'Linear Layer Mixing Model Internal Error #1 '      &
&             ,stuff1,' ',z1,' ',stuff2,' ',z2)
      end if
  END FUNCTION Layer_LinearMixingModel


  REAL(KIND(0.0D0)) FUNCTION Layer_B1MixingModel(b1,z1,b2,z2,i,j)
!  this subroutine assumes that both z11 and z12 are +ve
    USE Support
    IMPLICIT NONE
      REAL(KIND(0.0D0)),parameter :: defaultbadb1=-1.0
      REAL(KIND(0.0D0)) :: b1,z1,b2,z2
      integer :: i,j

      if (b1 < 0.0 .and. b2 < 0.0) then
        Layer_B1MixingModel=defaultbadb1
      else if (b1 < 0.0) then
        Layer_B1MixingModel=b2
      else if (b2 < 0.0 ) then
        Layer_B1MixingModel=b1
      else if (b1 == b2) then
        Layer_B1MixingModel=b1
      else
        if (z1 <= 0) then
          Layer_B1MixingModel=b2
        else if (z2 <= 0) then
          Layer_B1MixingModel=b1
        else
          Layer_B1MixingModel=((z1+z2)                                          &
&             /(z1/b1**MixingAlphaInv+z2/b2**MixingAlphaInv))**MixingAlpha
        end if
      end if
      if (z1 < 0.0 .or. z2 < 0.0) then
        call Message_Output(Message_Warning,                                    &
&             'B1 Layer Mixing Model negative depth Internal Error #1 b1 '       &
&             ,b1,' z1 ',z1,' b2 ',b2,' z2 ',z2)
      end if
      if (Layer_B1MixingModel < 0.0) then
        call Message_Output(Message_Warning,                                    &
&             'B1 Layer Mixing Model negative B1 Internal Error #2 '         &
&             //'Layer_B1MixingModel ',Layer_B1MixingModel,' b1 '       &
&             ,b1,' z1 ',z1,' b2 ',b2,' z2 ',z2)
        call Message_Output(Message_Warning,'index i ',i,'j ',j)
      end if
  END FUNCTION Layer_B1MixingModel

! ============================================================================
!                INPUT LAYER DATA
! ============================================================================

  SUBROUTINE Layer_InputData(Sim_Parameters,Z,GridSizeX,GridSizeY,LowX,HighX,LowY,HighY)
    USE InputOutput
    USE Multipliers
    USE SiberiaTypes
    USE Support
    USE ModelFile
    IMPLICIT NONE

    INTEGER, PARAMETER :: LineLgth=1000,UnitNo=20

    INTEGER :: GridSizeX,GridSizeY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),DIMENSION(GridSizeX,GridSizeY) :: Z
    TYPE(LocalParameters) :: Sim_Parameters

    INTEGER :: ii,i,j,ibuff,select1,ErrorNo,lowXX,HighXX,LowYY,HighYY,Tracer
    REAL(KIND(0.0D0)) :: temp1,temp2,temp3,Xlow,Xhigh,Ylow,YHigh                &
&       ,z11,z21,z12,z22,thickness,age,detach
    LOGICAL :: absolute,AnswerOn,errorL
    CHARACTER(LineLgth) :: buffer, line,LayerDEMFile,LayerRegionFile

    CHARACTER(80) :: atom
    INTEGER :: SELECT

    integer,parameter :: NoOptions=21
    CHARACTER(20) :: Options(NoOptions)
    SAVE Options
    DATA options(1)  / 'EROSION'/          &    ! default erosion properties
&        options(2)  / 'ERODIBILITY'/      &    ! default erodibility
&        options(3)  / 'RUNOFF'/           &    ! default runoff properties
&        options(4)  / 'ANGLE_OF_REPOSE'/  &    ! default angle of repose
&        options(5)  / 'CREEP'/            &    ! default creep rate
&        options(6)  / 'REGION_MASK'/      &    ! region file for mask
&        options(7)  / 'REGION_ACTIVE'/    &    ! region mask active or not
&        options(8)  / 'CAPPING'/          &    ! elevations from a capping layer
&        options(9)  / 'Z'/                &    ! elevations above a fixed elevation (horizontal layers)
&        options(10) / 'BILINEAR'/         &    ! elevations above a bilinear interpolation
&        options(11) / 'BILINEAR_CLAMPED'/ &    ! elevations above a bilinear interpolation
&        options(12) / 'DEM'/              &    ! elevations from a RST2 file
&        options(13) / 'THICKNESS'/        &    ! maximum thickness of layers
&        options(14) / 'DETACHMENT'/       &    ! maximum thickness of layers
&        options(15) / 'REGION_CLIP'/      &    ! a clipping region
&        options(16) / 'TRACER_1'/         &    ! default for tracer 1
&        options(17) / 'TRACER_2'/         &    ! default for tracer 2
&        options(18) / 'TRACER_3'/         &    ! default for tracer 3
&        options(19) / 'TRACER_4'/         &    ! default for tracer 4
&        options(20) / 'TRACER_5'/         &    ! default for tracer 5
&        options(21) / 'NUMBER_TRACERS'/        ! how many tracers in this run

      age=0.0
      CALL Layer_InitSize(lowX,highX,LowY,HighY,Sim_Parameters,age,ErrorNo)
!
!
!    quick and dirty fix to allow the layer model to be used with Moscow. This
!    will always select a layer file if input but will revert to an erosion file
!    if a layer file is not input.
!
      if (mod(Sim_Parameters%modeErode,20) == 4) then
        IF (TRIM(Sim_Parameters%FileLayers) == '') then
          if (TRIM(Sim_Parameters%FileFactor) /= '') then
            Sim_Parameters%FileLayers=Sim_Parameters%FileFactor
          end if
        end if
      end if
      IF (TRIM(Sim_Parameters%FileLayers) == '') return
!
! OK Layers are active so lets set them up.
!  set defaults
      Layer_Defaults%b1=Sim_Parameters%b1
      Layer_Defaults%m1=Sim_Parameters%m1
      Layer_Defaults%n1=Sim_Parameters%n1
      Layer_Defaults%b3=Sim_Parameters%b3
      Layer_Defaults%m3=Sim_Parameters%m3
      Layer_Defaults%dZ=Sim_Parameters%dZ
      Layer_Defaults%s0Max=Sim_Parameters%s0Max
      Layer_Defaults%tracer(1:MaxNumberTracers)=TracerDefault(1:MaxNumberTracers)
! Read in the starting layer data
      OPEN(UNIT=unitno,FILE=trim(Sim_Parameters%FileLayers),STATUS='old',ERR=9999)
      REWIND(UNIT=unitno)
! 
!  READ file header
! 
      CALL Message_Output(Message_Info,'-- Reading Layer Data')
      READ(unitno,6000) buffer
 6000 format(a)
      CALL Str_UpperCase(buffer)
      ibuff=1
      CALL Str_nxtat(buffer,ibuff,atom,80)
      IF (atom(1:) /= 'SIBERIA') THEN
        CALL Message_Output(Message_ErrorStop                                   &
&         ,'Invalid header in LAYER Model File'                                 &
&         //TRIM(Sim_Parameters%FileLayers))
      END IF
      DO i=2,4
        READ(unitno,*)
      END DO
!
!  Read the file content 
!    - check for comments
!
 8900 line=' '
      READ(unitno,6000,END=9000,ERR=9001) line
      line=adjustl(line)
      IF (line(1:1) == '#' .or. line(1:1) == '!'.or. line(1:1) == ' ') GO TO 8900

!    - check for a LAYER command

      ibuff=1
      buffer=' '
      buffer(1:LineLgth)=line(1:LineLgth)
      CALL Str_UpperCase(buffer)
      CALL Str_nxtat(buffer,ibuff,atom,LineLgth)
      CALL Str_mtchcomm1(atom,commands,SELECT)

!  Flush anything other than a LAYER command

      select case (select)
        case (Model_Layer)

!  Read the LAYER option

         CALL Str_nxtat(buffer,ibuff,atom,80)
         CALL Str_mtchcomm1(atom,options,SELECT)
!  common preliminaries
         SELECT CASE (SELECT)
           CASE (1:5)
             CALL Str_nxtat(buffer,ibuff,atom,80)
             CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT1)
             SELECT CASE (SELECT1)
               CASE(Model_Relative)
                 absolute=.false.
               CASE(Model_Absolute)
                 absolute=.true.
               CASE(Model_Default)
                 select case (select)
                 case(1)
                   Layer_Defaults%b1=Sim_Parameters%b1
                   Layer_Defaults%m1=Sim_Parameters%m1
                   Layer_Defaults%n1=Sim_Parameters%n1
                 case(2)
                   Layer_Defaults%b1=Sim_Parameters%b1
                 case(3)
                   Layer_Defaults%b3=Sim_Parameters%b3
                   Layer_Defaults%m3=Sim_Parameters%m3
                 case(4)
                   Layer_Defaults%s0max=Sim_Parameters%s0max
                 case(5)
                   Layer_Defaults%dZ=Sim_Parameters%dZ
                 end select
                 go to 8900
               CASE DEFAULT
                 GO TO 9002
             END SELECT
          CASE(7)
             CALL Str_nxtat(buffer,ibuff,atom,80)
             CALL Str_mtchcomm1(atom,OptionsOnOff,SELECT1)
             SELECT CASE (SELECT1)
               CASE(Model_On)
                 AnswerOn=.true.
               CASE(Model_Off)
                 AnswerOn=.false.
               CASE DEFAULT
                 GO TO 9002
             END SELECT
         END SELECT
         SELECT CASE (SELECT)

!  default erosion model
           CASE(1)
             READ(buffer(ibuff:),*,err=9002) temp1,temp2,temp3
             IF (absolute) THEN
               Layer_Defaults%b1=temp1/MultiplierRealVar(19)
               Layer_Defaults%m1=temp2/MultiplierRealVar(20)
               Layer_Defaults%n1=temp3/MultiplierRealVar(21)
             ELSE
               Layer_Defaults%b1=temp1*Layer_Defaults%b1
               Layer_Defaults%m1=temp2*Layer_Defaults%m1
               Layer_Defaults%n1=temp3*Layer_Defaults%n1
             END IF

!  default erodibility model
           CASE(2)
             READ(buffer(ibuff:),*,err=9002) temp1
             IF (absolute) THEN
               Layer_Defaults%b1=temp1/MultiplierRealVar(19)
             ELSE
               Layer_Defaults%b1=temp1*Layer_Defaults%b1
             END IF

!  default runoff model
           CASE(3)
             READ(buffer(ibuff:),*,err=9002) temp1,temp2
             IF (absolute) THEN
               Layer_Defaults%b3=temp1/MultiplierRealVar(17)
               Layer_Defaults%m3=temp2/MultiplierRealVar(18)
             ELSE
               Layer_Defaults%b3=temp1*Layer_Defaults%b3
               Layer_Defaults%m3=temp2*Layer_Defaults%m3
             END IF

!  default angle of repose
           CASE(4)
             READ(buffer(ibuff:),*,err=9002) temp1
             IF (absolute) THEN
               Layer_Defaults%s0max=temp1/MultiplierRealVar(31)
             ELSE
               Layer_Defaults%s0max=temp1*Layer_Defaults%s0max
             END IF

!  default creep rate
           CASE(5)
             READ(buffer(ibuff:),*,err=9002) temp1
             IF (absolute) THEN
               Layer_Defaults%dZ=temp1/MultiplierRealVar(1)
             ELSE
               Layer_Defaults%dZ=temp1*Layer_Defaults%dZ
             END IF

!  Input Masking Region
           CASE(6)
             LayerRegionFile=' '
             READ(buffer(ibuff:),*,err=9002) LayerRegionFile
             LayerRegionMaskActive=.true.
             call AllocateArray(Layer_Mask,LowX,HighX,LowY,HighY,errorNo)
             call InputRegionFileLXY(LayerRegionFile,Layer_Mask,errorL)

!  Input Masking Region Active or not
           CASE(7)
             LayerRegionMaskActive=AnswerOn

!  Input Layer: capping
           CASE(8)
             READ(buffer(ibuff:),*,err=9002) temp1            ! cap thickness
             CALL Layer_Cap(Z,GridSizeX,GridSizeY,temp1,LowX,HighX,LowY,HighY)

!  Input Layer: constant elevation
           CASE(9)
             READ(buffer(ibuff:),*,err=9002) temp1            ! bottom elevation of layer
             CALL Layer_ConstantZ(Z,GridSizeX,GridSizeY,temp1,LowX,HighX,LowY,HighY)

!  Input Layer: bilinear 
           CASE(10)
             READ(buffer(ibuff:),*,err=9002) Xlow,Xhigh,Ylow,YHigh,z11,z21,z12,z22
             call Layer_BiLinear(Z,GridSizeX,GridSizeY,Xlow,Xhigh,Ylow,YHigh,z11,z21,z12,z22  &
&                          ,LowX,HighX,LowY,HighY)

!  Input Layer: bilinear 
           CASE(11)
             

!  Input Layer: DEM file with vertical offset
           CASE(12)
             LayerDEMFile=' '
             READ(buffer(ibuff:),*,err=9002) temp1,LayerDEMFile
             Call Layer_DEM(Z,GridSizeX,GridSizeY,temp1,LayerDEMFile,LowX,HighX,LowY,HighY)

!  Input Layer: maximum layer thickness
           CASE(13)
             READ(buffer(ibuff:),*,err=9002) Thickness
             CALL Layer_Set(Layer_Resolution,thickness,ErrorNo)

!  Input Layer: layer detachability
           CASE(14)
             CALL Str_nxtat(buffer,ibuff,atom,80)
             CALL Str_mtchcomm1(atom,OptionsRelAbs,SELECT1)
             if ((select1 == Model_Relative) .or. (select1 == Model_Absolute))  &
&                  READ(buffer(ibuff:),*,err=9002) detach
             SELECT CASE (SELECT1)
               CASE(Model_Relative)
                 absolute=.false.
                 CALL Layer_Set(Layer_Detachment,detach,ErrorNo)
               CASE(Model_Absolute)
                 absolute=.true.
                 CALL Layer_Set(Layer_Detachment,detach,ErrorNo)
               CASE(Model_Default)
                 CALL Layer_Set(Layer_Detachment,LayerDetachmentDefault,ErrorNo)
                 go to 8900
               CASE(Model_RelAbs_On)
                 CALL Layer_InitDetach()
                 go to 8900
               CASE DEFAULT
                 GO TO 9002
             END SELECT

!  Input Masking Clipping Region
           CASE(15)
             READ(buffer(ibuff:),*,err=9002) lowXX,HighXX,LowYY,HighYY
             LayerRegionMaskActive=.true.
             call AllocateArray(Layer_Mask,LowX,HighX,LowY,HighY,errorNo)
             Layer_Mask%data=.false.
             do j=LowYY,HighYY
               do i=LowXX,HighXX
                 Layer_Mask%data(i,j)=.true.
               end do 
             end do 

!  default for tracers
           CASE(16:20)
             tracer=select-15
             if (tracer > NumberTracers) then
               call Message_Output(Message_WarnContinue,'Requested tracer = '   &
&                       ,tracer,' not being modelled.')
               call Message_Output(Message_WarnContinue,                        &
&                       'so command below has been ignored')
               call Message_Output(Message_WarnContinue,buffer(1:80))
             else
               READ(buffer(ibuff:),*,err=9002) temp1
               Layer_Defaults%tracer(tracer)=temp1
             end if

!  Input number of tracers to track
           CASE(21)
             READ(buffer(ibuff:),*,err=9002) NumberTracers
             if (NumberTracers > MaxNumberTracers) then
               call Message_Output(Message_WarnContinue                         &
&                     ,'Requested too many tracers',NumberTracers,' > '         &
&                     ,MaxNumberTracers)
               call Message_Output(Message_WarnContinue                         &
&                     ,'Resetting number of tracers to = ',MaxNumberTracers)
               NumberTracers=MaxNumberTracers
             end if

           CASE DEFAULT
             GO TO 9002
         END SELECT
      case (Model_Erodibility)
        write (*,*) 'found some LAYER erodibility commands ... '                &
&             //'did not do anything with them'
      END select
!     END IF
      GO TO 8900
      RETURN
!   clean up before return
 9000 if (associated(Layer_mask%data)) call DeallocateArray(layer_mask)
!    NB FLOW LAYER INITIALISATION
!       =========================
!  initialise the flow layer with the parameters for the surface so that if we
!  deposit in the first iteration we get sensible values (otherwise we get
!  whatever junk is in the flowlayer at initialisation ... typically -1)
!    NB Make sure that all parameters of the flow layer are initialised!!!!!
      CALL Layer_InitFlowLayer(lowX,highX,LowY,HighY,ErrorNo)
      CALL Message_Output(Message_Info,'-- END of LAYER input')
      CLOSE(UNIT=unitno,STATUS='keep')
      RETURN
! 
 9001 CALL Message_Output(Message_WarnContinue                                  &
&           ,'Incomprehensible input #1 in file '                               &
&        //trim(Sim_Parameters%FileLayers))
      CALL Message_Output(Message_WarnContinue,line)
      GO TO 8900
!
 9002 CALL Message_Output(Message_WarnContinue,'Unknown option #1 in file '     &
&        //trim(Sim_Parameters%FileLayers))
      CALL Message_Output(Message_WarnContinue,line)
      CALL Message_Output(Message_WarnContinue,'Attempting recovery')
      GO TO 8900
!
 9999 CALL Message_Output(Message_ErrorStop                                     &
&           ,'Error opening erosion file '                                      &
&          //trim(Sim_Parameters%FileLayers))
  END SUBROUTINE Layer_InputData

  logical function InMask(i,j)
    implicit none
    integer :: i,j

    if (LayerRegionMaskActive) then
      InMask=Layer_Mask%data(i,j)
    else
      InMask=.true.
    end if
  end function InMask

! --------------------------------------------------------------
!  place a capping layer
! --------------------------------------------------------------

  SUBROUTINE Layer_Cap(Z,GridSizeX,GridSizeY,CapThickness,LowX,HighX,LowY,HighY)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: GridSizeX,GridSizeY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: Z(GridSizeX,GridSizeY),CapThickness
!
    INTEGER :: i,j
    REAL(KIND(0.0D0)) :: base
!
      DO j=LowY,HighY
        DO I=LowX,HighX
          If (Inmask(i,j)) then
            base=Z(i,j)-CapThickness
            call Layer_InsertLayer(base,z(i,j),layers%data(i,j),.true.)
          end if
        END DO 
      END DO 
      RETURN
  END SUBROUTINE Layer_Cap

! --------------------------------------------------------------
! place a constant elevation layer
! --------------------------------------------------------------

  SUBROUTINE Layer_ConstantZ(Z,GridSizeX,GridSizeY,Elevation,LowX,HighX,LowY,HighY)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: GridSizeX,GridSizeY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: Z(GridSizeX,GridSizeY),Elevation
!
    INTEGER :: i,j
!
      DO j=LowY,HighY
        DO I=LowX,HighX
          If (Inmask(i,j)) then
            call Layer_InsertLayer(Elevation,z(i,j),layers%data(i,j),.true.)
          end if
        END DO 
      END DO 
      RETURN
  END SUBROUTINE Layer_ConstantZ


! --------------------------------------------------------------
! place a layer given by a DEM
! --------------------------------------------------------------

  SUBROUTINE Layer_DEM(Z,GridSizeX,GridSizeY,ZOffset,FileName,LowX,HighX,LowY,HighY)
    USE Support
    USE SiberiaTypes
    USE InputOutput
    IMPLICIT NONE

    integer :: LowX,HighX,LowY,HighY,GridSizeX,GridSizeY
    REAL(KIND(0.0D0)) :: ZOffset
    CHARACTER(*) :: FileName
    REAL(KIND(0.0D0)) :: Z(GridSizeX,GridSizeY)

    integer :: i,j
    REAL(KIND(0.0D0)) :: Elevation
    type(ArrayRXY) :: DEM
!
      call ReadInZRXY(DEM,FileName,LowX,HighX,LowY,HighY)
      DO j=LowY,HighY
        DO I=LowX,HighX
          If (Inmask(i,j)) then
            Elevation=DEM%data(i,j)-ZoffSet
            call Layer_InsertLayer(Elevation,z(i,j),layers%data(i,j),.true.)
          end if
        END DO 
      END DO 
      RETURN
  END SUBROUTINE Layer_DEM

! --------------------------------------------------------------
! place a bilinear layer
! --------------------------------------------------------------

  SUBROUTINE Layer_BiLinear(Z,GridSizeX,GridSizeY,Xlow,Xhigh,Ylow,YHigh         &
&             ,z11,z21,z12,z22,LowX,HighX,LowY,HighY)
    IMPLICIT NONE
    integer :: GridSizeX,GridSizeY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: Z(GridSizeX,GridSizeY),Xlow,Xhigh,Ylow,YHigh           &
&             ,z11,z21,z12,z22

    integer :: i,j
    REAL(KIND(0.0D0)) :: Elevation,Z1,Z2,YGrad

      DO j=LowY,HighY
          Ygrad=(j-YLow)/(YHigh-Ylow)
          Z1=Z11+YGrad*(Z12-Z11)
          Z2=Z21+YGrad*(Z22-Z21)
          DO I=LowX,HighX
              If (Inmask(i,j)) then
                Elevation=z1+(z2-z1)*(i-Xlow)/(XHigh-XLow)
                call Layer_InsertLayer(Elevation,z(i,j),layers%data(i,j),.true.)
              end if
          END DO
      END DO 
  END SUBROUTINE Layer_BiLinear



! --------------------------------------------------------------
! place a new layer at a given point
! --------------------------------------------------------------

  SUBROUTINE Layer_InsertLayer(base,z,LayerPt,delete)
    USE Support
    IMPLICIT NONE
!
    REAL(KIND(0.0D0)) :: base,z
    TYPE(layers_xy) :: LayerPt,temp1
    LOGICAL :: delete
!
    INTEGER :: i,size,ErrorNo,LayerNo
!
    IF (base >= z) RETURN
!  OK point is below surface
    size=UBOUND(LayerPt%Layer,1)
    ALLOCATE(temp1%layer(size),stat=ErrorNo)
    IF (ErrorNo /= 0) CALL AllocationError                                      &
&                                 ('Layer_Data_temp','Layer_InsertLayer')
    temp1%layer=LayerPt%Layer
    LayerNo=0
    DO i=1,size
      IF (z > temp1%layer(i)%z) THEN
        LayerNo=i
        GO TO 8000
      END IF
    END DO
    CALL Message_Output(Message_ErrorStop                                       &
&           ,'Cannot find layer data to insert layer in Layer_InsertLayer')
 8000 DEALLOCATE(LayerPt%Layer)
! if we remove all layers between base and the surface
    IF (delete) THEN
      ALLOCATE(LayerPt%Layer(size-LayerNo+2),stat=ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError                                    &
&                                 ('Layer_Data_Expand1','LayerAddLayerPt')
      call Layer_InitLayer(LayerPt%layer(1))
      LayerPt%Layer(1)%z=base
      LayerPt%Layer(1)%detach=LayerDetachment
      DO i=2,size-LayerNo+2
        LayerPt%Layer(i)=temp1%layer(layerNo-2+i)
      END DO
    ELSE
! if we slot this layer in between the others
      ALLOCATE(LayerPt%Layer(size+1),stat=ErrorNo)
      IF (ErrorNo /= 0) CALL AllocationError                                    &
&                                 ('Layer_Data_Expand2','LayerAddLayerPt')
      DO i=1,LayerNo-1
        LayerPt%Layer(i)=temp1%Layer(i)
      END DO
      call Layer_InitLayer(LayerPt%layer(LayerNo))
      LayerPt%Layer(LayerNo)%z=base
      LayerPt%Layer(LayerNo)%detach=LayerDetachment
      DO i=LayerNo,size
        LayerPt%Layer(i+1)=temp1%Layer(i)
      END DO
    END IF
  END SUBROUTINE Layer_InsertLayer

! ================================================================
!                    tectonic uplift modules
! ================================================================

  SUBROUTINE LayerUpliftArray(Uplift,lowX,highX,LowY,HighY,GridX,GridY)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: lowX,highX,LowY,HighY,GridX,GridY
    REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Uplift
!
    INTEGER :: i,j,k
!
    IF (.not.Module_Active) RETURN
    DO j=LowY,HighY
      DO i=LowX,HighX
        if (Uplift(i,j) /= 0.0d0) then
          DO k=1,UBOUND(Layers%data(i,j)%Layer,1)
            if (Layers%data(i,j)%Layer(k)%z > MinimumZ) THEN
              Layers%data(i,j)%Layer(k)%z=Layers%data(i,j)%Layer(k)%z+Uplift(i,j)
            END IF
          END DO
        end if
      END DO
    END DO
  END SUBROUTINE LayerUpliftArray

  SUBROUTINE LayerUpliftConstant(Uplift,lowX,highX,LowY,HighY)
    USE Support
    IMPLICIT NONE
!
    INTEGER :: lowX,highX,LowY,HighY
    REAL(KIND(0.0D0)) :: Uplift
!
    INTEGER :: i,j,k
!
    IF (.not.Module_Active) RETURN
    if (Uplift == 0.0d0) return
    DO j=LowY,HighY
      DO i=LowX,HighX
        DO k=1,UBOUND(Layers%data(i,j)%Layer,1)
          IF (Layers%data(i,j)%Layer(k)%z > MinimumZ) THEN
            Layers%data(i,j)%Layer(k)%z=Layers%data(i,j)%Layer(k)%z+Uplift
          END IF
        END DO
      END DO
    END DO
  END SUBROUTINE LayerUpliftConstant


! ============================================================================
!                OUTPUT LAYER DATA
! ============================================================================

  SUBROUTINE Layer_OutputData(Filename,Z,GridSizeX,GridSizeY,LowX,HighX         &
&                   ,LowY,HighY,SimParameters)
    USE Multipliers
    USE SiberiaTypes
    USE Support
    IMPLICIT NONE

    INTEGER, PARAMETER :: UnitNo=20,NumberTracerHeaders=5
    CHARACTER(8),PARAMETER :: FilenameExt='.layers '                 &
&              ,tracerheader(1:NumberTracerHeaders)=(/'tracer_1','tracer_2'     &
&               ,'tracer_3','tracer_4','tracer_5'/)

    INTEGER :: GridSizeX,GridSizeY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)),DIMENSION(GridSizeX,GridSizeY) :: Z
    CHARACTER(*) :: Filename
    TYPE(LocalParameters) :: SimParameters

    INTEGER :: ii,i,j,k,NoLayers

!  assume filename does not have any extension on it

    if (.not.Module_Active) return
    if (NumberTracers > NumberTracerHeaders) then
      CALL Message_Output(Message_WarnContinue,'Can only output headers for '   &
&           ,NumberTracerHeaders,' tracers')
    end if
    filename=trim(filename)//trim(FilenameExt)//' '
    open(unit=unitno,file=filename,status='unknown',err=9999)
    write (unitno,6000) LayerFileHeader,' ',Layer_Version
6000 format(2a,f4.2)
    write (unitno,*) LowX,HighX,LowY,HighY
    write (unitno,*) SimParameters%GridXY,SimParameters%East,SimParameters%North
	if (Detachment_Limitation) then
      write (unitno,*) min(NumberTracerHeaders,NumberTracers)+3
      write (unitno,*) 'z b1 detach',(' ',trim(tracerheader(ii)),ii=1           &
&                                     ,min(NumberTracerHeaders,NumberTracers))
      do j=LowY,HighY
        do i=LowX,HighX
          NoLayers=ubound(layers%data(i,j)%layer,1)
          write (unitno,*) i,j,NoLayers,z(i,j) 
          do k=1,NoLayers
            write(unitno,6020) sngl(layers%data(i,j)%layer(k)%z)                &
&                     ,sngl(layers%data(i,j)%layer(k)%b1)                       &
&                    ,sngl(layers%data(i,j)%layer(k)%detach)                    &
&                    ,(sngl(layers%data(i,j)%layer(k)%tracer(ii))               &
&                                                       ,ii=1,NumberTracers)
6020        format(' ',1000(e15.6,' '))
          end do
        end do
      end do
	else
      write (unitno,*) min(NumberTracerHeaders,NumberTracers)+2
      write (unitno,*) 'z b1',(' ',trim(tracerheader(ii))                       &
&                                 ,ii=1,min(NumberTracerHeaders,NumberTracers))
      do j=LowY,HighY
        do i=LowX,HighX
          NoLayers=ubound(layers%data(i,j)%layer,1)
          write (unitno,*) i,j,NoLayers,z(i,j)
          do k=1,NoLayers
            write(unitno,6010) sngl(layers%data(i,j)%layer(k)%z)                &
&                             ,sngl(layers%data(i,j)%layer(k)%b1)               &
&                      ,(sngl(layers%data(i,j)%layer(k)%tracer(ii))             &
&                                                       ,ii=1,NumberTracers)
6010        format(' ',1000(e15.6,' '))
          end do
        end do
      end do
	end if
    close(unit=unitno,status='keep')
    call Message_Output(Message_Info,'LAYER data file '//trim(filename)//' output')
    return

!  Error Handling

9999 call Message_Output(Message_ErrorContinue,filename//' cannot be opened')
    return
  END SUBROUTINE Layer_OutputData


  SUBROUTINE Layer_Dump(gridX,GridY,Z,where)
    IMPLICIT NONE
    integer :: gridX,GridY
    REAL(KIND(0.0D0)),DIMENSION(gridX,GridY),optional :: Z
    character(*),optional :: where

    integer :: ii,i,j,k,NoLayers

    if (present(where)) then
      write (60,*) 'LayerDump @',where
    else
      write (60,*) 'LayerDump @'
    end if
    write (60,*) 'Elevations'
    if (present(Z)) then
      do j=1,gridY
        do i=1,gridX
          write (60,*) i,j,z(i,j)
        end do
      end do
    end if
    write (60,*) 'Flow'
    do j=lbound(layers%data,2),ubound(layers%data,2)
      do i=lbound(layers%data,1),ubound(layers%data,1)
        write (60,*) i,j,sngl(flow%data(i,j)%b1)
      end do
    end do
    write (60,*) 'Layers'
    do j=lbound(layers%data,2),ubound(layers%data,2)
      do i=lbound(layers%data,1),ubound(layers%data,1)
        NoLayers=ubound(layers%data(i,j)%layer,1)
        write (60,*) i,j,NoLayers
        do k=1,NoLayers
          write (60,*) sngl(layers%data(i,j)%layer(k)%z)                        &
&             ,sngl(layers%data(i,j)%layer(k)%b1)                               &
&             ,(layers%data(i,j)%layer(k)%tracer(ii),ii=1,NumberTracers)
        end do
      end do
    end do
  END SUBROUTINE Layer_Dump



! ============================================================================
!                COSMOGENICS ANALYSIS
! ============================================================================



  subroutine cosmogenics(NewSimTime,Z,Domain,GridX,GridY,LowX,HighX,LowY,HighY)
  
!  I have passed in the full array Z so you can do topgraphic shading for 
!                 cosmogenic generation if you want.
!  VARIABLES
!  =========
!     OldSimTime  :: the time in the simulation that this routine was last called
!     NewSimTime  :: the current time in the simulation. (NewSimTime-OldSimTime) is the duration between 
!                   subroutine calls, which is required to determine absolute amounts of generation.
!     Z           :: is the array of elevations. Note that in the call that this is not exactly the elevation
!                   at the start of the timestep. THis cannot be passed in because of memory incompatabilities
!                   between Z and DeltaZ in the calling routines (historical ugliness as a result of using static
!                   memory in the original SIBERIA code (fortran 77) and needing
!                   to use dynamic memory (fortran 95) for the layering model).
!     Domain      :: This is a mask for what points on the grid are actually used in the calculations (true its 
!                    used in the calculations, false not).
!     GridX,GridY :: The size of arrays using static memory.
!     LowX,HighX,LowY,HighY :: the coordinates of the bounding box used in the calculations. Only nodes in or 
!                   the border are used in calculations. Note that some of the arrays in newer parts of the code
!                   use these as the bounds of the array (though caution needs to be used in passing these arrays
!                   into subroutines because by default fortran rules make the lower bound=1 not the lower bound
!                   set in the calling array i.e. LowX). These bounds are most useful for memory efficient array
!                   declarations that are local to this subroutine.

    USE SiberiaTypes
    USE Support
    USE LayerConstants
    implicit none
      integer :: GridX,GridY,LowX,HighX,LowY,HighY
      REAL(KIND(0.0D0)) :: NewSimTime
      REAL(KIND(0.0D0)),DIMENSION(GridX,GridY) :: Z
      LOGICAL,DIMENSION(GridX,GridY) :: Domain
      
!  local declarations
 
      integer :: i,j,k,ii,NumberLayers,ErrorNo
      integer,save :: NumberTracers
      real(kind(0.0d0)) :: whatever,ZTop,ZBottom,Depth,tracer
      real(kind(0.0d0)),dimension(LowX:HighX,LowY:HighY) :: shading
      
      
!   do any initialisation for first time through this routine

     if (firstCosmo) then
          call Layer_get(Layer_Number_Tracers,NumberTracers,ErrorNo)
!  if you want to read in a file with cosmogenic parameters, etc that don't change through the simulation 
!  here is the place to do it
          firstCosmo=.false.
     end if
     
      
!  calculate shading and/or atmospheric cosmogenic generation rate (constant for all tracers?)

      do i=LowX,HighX
        do j=LowY,HighY
          shading(i,j)=whatever
        end do
      end do

!  calculate generation rate for each layer of the soil and each cosmogenic tracer

      do i=LowX,HighX
        do j=LowY,HighY
!  you could always get this value directly from the dat structire but best to do it through my bottlenecks because 
!  this allows me to change the data structure format if needed without impacting on the code below.
          call Layer_get(Layer_NoLayers,i,j,NumberLayers,ErrorNo)
!  note that this skips the bottom most layer because it extends to infinity so an average depth can't be determined
!   => assumes that the bottom depth is so deeply buried that it can't generate cosmogenic tracers. Now this may be problem
!      when you have deep erosion that cuts through all the initial layers because then at that point SIBERIA will assume there
!      are no layers anymore ... and no cosmogenic generation. My initial thought here would be to put an minimum number of
!      layers that SIBERIA should have at any point at any time (probably based on the layer resolution parameter). Have a 
!      think about this Tibi and let me know what you think. This is a problem that I have only just thought of and have not 
!      thought it through re cosmogenics.

          do k=1,NumberLayers-1
            call Layer_Get(Layer_Z,i,j,k,Ztop,ErrorNo)
            call Layer_Get(Layer_Z,i,j,k+1,Zbottom,ErrorNo)
!  get depth of burial of that layer (note max() is required because of slight inconsistency with z and it also good 
!  to ensure nonnegative burial depths)
            depth=max(0.0,Z(i,j)-(Ztop+ZBottom)*0.5)
            do ii=1,NumberTracers
              call Layer_Get(Layer_Tracer(ii),i,j,k,tracer,ErrorNo)
!  your code adding to the cosmogenic load for tracer ii should go here.
              call Layer_Set(Layer_Tracer(ii),i,j,k,tracer,ErrorNo)
            end do
          end do
        end do
      end do
      
  end subroutine cosmogenics





END MODULE LayerSupport

