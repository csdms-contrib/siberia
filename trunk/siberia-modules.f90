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

MODULE SiberiaConstants
! =================================================================
!          Global Constants declarations used in SIBERIA
! =================================================================
!
!   Global Variable Definitions
! =======================================
! 
  IMPLICIT NONE

  REAL,PARAMETER :: version=8.33
  Logical :: Debug=.false.

  INTEGER,PARAMETER :: nCont=10000
  INTEGER,PARAMETER :: NoOutlets=15000 
  INTEGER,PARAMETER :: MaxRegions=36
  INTEGER,PARAMETER :: CommandUnit=31
  INTEGER,PARAMETER :: GlobalTempUnit=28
  INTEGER,PARAMETER :: GlobalTempUnit2=27
  INTEGER,PARAMETER :: MaxFlowIn=2000
  REAL(KIND(0.0D0)),PARAMETER :: alpha=0.9
  REAL(KIND(0.0D0)),PARAMETER :: eps=0.01
  LOGICAL,PARAMETER :: Prompt=.true.
!  FILE TYPES
  integer,parameter :: FileTypeRSU=1, FileTypeBIN=2
!  default location
  character(255) :: DefDirectory=''
  character(255),parameter :: DefDirectoryFilename='default-directory.txt'
! 
!     Drainage Direction Definitions
! =======================================
! 
!         the delta X component for the directions
!         from the current node
  INTEGER,DIMENSION(14),PARAMETER :: dir1=(/-1,0,1,0,0,0,0,0,0,0,-1,-1,1,1/)
!         the delta Y component for the directions
!         from the current node
  INTEGER,DIMENSION(14),PARAMETER :: dir2=(/0,-1,0,1,0,0,0,0,0,0,1,-1,-1,1/)
!         the inverse direction from the delta x,y, components
  INTEGER,DIMENSION(14),PARAMETER :: invdir=(/5,7,1,3,0,0,0,0,0,0,4,6,8,2/)
!
!         The following arrays are 0 -> 9, even though there
!         are only 8 adjacent nodes, TO ease cyclic references
!         so that e.g. nodei(0)=nodei(8) and nodei(9)=nodei(1)
!
!         the delta X component for the 8 adjacent nodes
!         from the current node
  INTEGER,DIMENSION (0:9),PARAMETER :: nodei=(/  1,1,1,0,-1,-1,-1, 0, 1,1 /)
!         the delta Y component for the 8 adjacent nodes
!         from the current node
  INTEGER,DIMENSION (0:9),PARAMETER :: nodej=(/ -1,0,1,1, 1, 0,-1,-1,-1,0 /)
!         the directions TO the 8 adjacent nodes from the current node
  INTEGER,DIMENSION (0:9),PARAMETER :: dir=(/ 13,3,14,4,11,1,12,2,13,3 /)
!         the directions required from the 8 adjacent nodes so that they
!         drain TO the current node
  INTEGER,DIMENSION (0:9),PARAMETER :: dircomp=(/ 11,1,12,2,13,3,14,4,11,1 /)
!         the lengths associated with dircomp
  REAL(KIND(0.0D0)),DIMENSION (0:9),PARAMETER :: invlgth=(/ 0.707107,1.0,0.707107,1.0,0.707107                  &
&                    ,1.0,0.707107,1.0,0.707107,1.0 /)
!         the length TO the 8 adjacent nodes
  REAL(KIND(0.0D0)),DIMENSION (0:9),PARAMETER :: dirlgth=(/ 1.41421,1.0,1.41421,1.0,1.41421                     &
&                    ,1.0,1.41421,1.0,1.41421,1.0 /)
END MODULE SiberiaConstants

!  ====================================================================
!  ====================================================================
!  ====================================================================

MODULE SiberiaTypes
!  ==================================
!   global TYPE definitions
!  ==================================
!
! Model parameter structures
! --------------------------
!
  TYPE LocalParameters
    INTEGER :: RunTime,StatsTime,kx,ky,modeIC,TimeUp                       &
&         ,ModeSolver,ModeDir,ModeUplift                                   &
&         ,ModeRandom,ModeErode,ModeRunoff,ModeChannel                     &
&         ,ModeDP,ModeMC,DirReg,ModeSoil                                   &
&         ,idummy18,idummy19,idummy20
    REAL(KIND(0.0D0)) :: dZ,dZn,dZHold,QsHold,FactMx                       &
&         ,FRanMn,c1,YFix,FRanCV,b3SDs,b3SDl,TAmp,TPeriod,TPhase,FRanZ     &
&         ,a1,m3,b3,b1,m1,n1,Bulk,InitTimeStep,b5,n5,SInit,m5              &
&         ,YHold,Notch,Cover,s0max,DTime,OTime,GridXY,East                 &
&         ,North,b6,m6,b12,m12,SDRate,SDExp1,SDExp2                        &
&         ,SMThreshold,SDSMWgt,rdummy46,rdummy47,rdummy48,rdummy49         &
&         ,rdummy50
!    CHARACTER(80) :: FileFactor,FileRunoff,FileUplift                      &
    CHARACTER(1000) :: FileFactor,FileRunoff,FileUplift                      &
&         ,FileDirections,FileMonteCarlo,FileChannels,FileLayers           &
&         ,FileDummy8,FileControl,FileOthers
  END TYPE LocalParameters
  TYPE LocalParametersArray
    INTEGER :: IntVar(1:20)
    REAL(KIND(0.0D0)) :: RealVar(1:50)
    CHARACTER(1000) :: FileNames(1:10)
!    CHARACTER(80) :: FileNames(1:10)
  END TYPE LocalParametersArray
  TYPE PtXYI
    INTEGER :: x,y
  END TYPE PtXYI
  TYPE PtXYZI
    INTEGER :: x,y,z
  END TYPE PtXYZI
  TYPE PtXYR
    REAL :: x,y
  END TYPE PtXYR
  TYPE PtXYZR
    REAL :: x,y,z
  END TYPE PtXYZR
  TYPE PtXYR8
    REAL(KIND(0.0D0)) :: x,y
  END TYPE PtXYR8
  TYPE PtXYZR8
    REAL(KIND(0.0D0)) :: x,y,z
  END TYPE PtXYZR8
  TYPE PtXYvalueR
    INTEGER :: x,y
    REAL :: value
  END TYPE PtXYvalueR
  TYPE PtXYZvalueR
    INTEGER :: x,y,z
    REAL :: value
  END TYPE PtXYZvalueR
  TYPE PtXYvalueR8
    INTEGER :: x,y
    REAL(KIND(0.0D0)) :: value
  END TYPE PtXYvalueR8
  TYPE PtXYZvalueR8
    INTEGER :: x,y,z
    REAL(KIND(0.0D0)) :: value
  END TYPE PtXYZvalueR8

!
! =======================================
! 
  INTEGER,PARAMETER :: MaxNoFileList=100
!
  TYPE MCParaCharacType
    REAL :: Min,Max,Mean,SD
    INTEGER :: MCDistribution
  END TYPE MCParaCharacType
  TYPE MCFileListType
    INTEGER :: SampleMode,NoFiles
    CHARACTER(1000),DIMENSION(MaxNoFileList) :: FileList
!    CHARACTER(80),DIMENSION(MaxNoFileList) :: FileList
  END TYPE MCFileListType
  TYPE MCParaCharacArray
    TYPE(MCParaCharacType), DIMENSION(70) :: Parameter
    TYPE(MCFileListType) :: RST2FileList
  END TYPE MCParaCharacArray
!
!  RESIZEABLE ARRAYS
!  -----------------
!  INTEGER ARRAYS
  type ArrayIX
    SEQUENCE
    INTEGER :: lowX=0,highX=0
    integer,dimension(:),pointer :: data => null()
  end type ArrayIX

  type ArrayIXY
    SEQUENCE
    INTEGER :: lowX=0,highX=0,lowY=0,highY=0
    integer,dimension(:,:),pointer :: data => null()
  end type ArrayIXY

  type ArrayIXYZ
    SEQUENCE
    INTEGER :: lowX=0,highX=0,lowY=0,highY=0,lowZ=0,highZ=0
    integer,dimension(:,:,:),pointer :: data => null()
  end type ArrayIXYZ

!  REAL(4) ARRAYS
  type ArrayRX
    SEQUENCE
    INTEGER :: lowX=0,highX=0
    REAL,dimension(:),pointer :: data => null()
  end type ArrayRX

  type ArrayRXY
    SEQUENCE
    INTEGER :: lowX=0,highX=0,lowY=0,highY=0
    REAL,dimension(:,:),pointer :: data => null()
  end type ArrayRXY

  type ArrayRXYZ
    SEQUENCE
    INTEGER :: lowX=0,highX=0,lowY=0,highY=0,lowZ=0,highZ=0
    REAL,dimension(:,:,:),pointer :: data => null()
  end type ArrayRXYZ

!  REAL(KIND(0.0D0)) ARRAYS
  type ArrayR8X
    SEQUENCE
    INTEGER :: lowX=0,highX=0
    REAL(KIND(0.0D0)),dimension(:),pointer :: data => null()
  end type ArrayR8X

  type ArrayR8XY
    SEQUENCE
    INTEGER :: lowX,highX,lowY,highY
    REAL(KIND(0.0D0)),dimension(:,:),pointer :: data
  end type ArrayR8XY

  type ArrayR8XYZ
    SEQUENCE
    INTEGER :: lowX=0,highX=0,lowY=0,highY=0,lowZ=0,highZ=0
    REAL(KIND(0.0D0)),dimension(:,:,:),pointer :: data => null()
  end type ArrayR8XYZ

!  LOGICAL ARRAYS
  type ArrayLX
    SEQUENCE
    INTEGER :: lowX=0,highX=0
    LOGICAL,dimension(:),pointer :: data => null()
  end type ArrayLX

  type ArrayLXY
    SEQUENCE
    INTEGER :: lowX,highX,lowY,highY
    LOGICAL,dimension(:,:),pointer :: data
  end type ArrayLXY

  type ArrayLXYZ
    SEQUENCE
    INTEGER :: lowX,highX,lowY,highY,lowZ,highZ
    LOGICAL,dimension(:,:,:),pointer :: data
  end type ArrayLXYZ
!
!  LIST TYPES
!  Integer Lists
  type ListIX
    SEQUENCE
    integer :: No=0
    integer,dimension(:),pointer :: X => null()
  end type ListIX

  type ListIXY
    SEQUENCE
    integer :: No=0
    integer,dimension(:),pointer :: X=> null(),Y => null()
  end type ListIXY

  type ListIXYZ
    SEQUENCE
    integer :: No=0
    integer,dimension(:),pointer :: X=> null(),Y => null(),Z => null()
  end type ListIXYZ

!  Real Lists
  type ListRX
    SEQUENCE
    integer :: No=0
    REAL,dimension(:),pointer :: X => null()
  end type ListRX

  type ListRXY
    SEQUENCE
    integer :: No=0
    REAL,dimension(:),pointer :: X=> null(),Y => null()
  end type ListRXY

  type ListRXYZ
    SEQUENCE
    integer :: No=0
    REAL,dimension(:),pointer :: X=> null(),Y => null(),Z => null()
  end type ListRXYZ

!  REAL(KIND(0.0D0)) Lists
  type ListR8X
    SEQUENCE
    integer :: No=0
    REAL(KIND(0.0D0)),dimension(:),pointer :: X => null()
  end type ListR8X

  type ListR8XY
    SEQUENCE
    integer :: No=0
    REAL(KIND(0.0D0)),dimension(:),pointer :: X=> null(),Y => null()
  end type ListR8XY

  type ListR8XYZ
    SEQUENCE
    integer :: No=0
    REAL(KIND(0.0D0)),dimension(:),pointer :: X=> null(),Y => null(),Z => null()
  end type ListR8XYZ

  INTERFACE AllocateArray
    MODULE PROCEDURE AllocateArrayIX
    MODULE PROCEDURE AllocateArrayIXY
    MODULE PROCEDURE AllocateArrayIXYZ
    MODULE PROCEDURE AllocateArrayRX
    MODULE PROCEDURE AllocateArrayRXY
    MODULE PROCEDURE AllocateArrayRXYZ
    MODULE PROCEDURE AllocateArrayR8X
    MODULE PROCEDURE AllocateArrayR8XY
    MODULE PROCEDURE AllocateArrayR8XYZ
    MODULE PROCEDURE AllocateArrayLX
    MODULE PROCEDURE AllocateArrayLXY
    MODULE PROCEDURE AllocateArrayLXYZ
    MODULE PROCEDURE AllocateListIX
    MODULE PROCEDURE AllocateListIXY
    MODULE PROCEDURE AllocateListIXYZ
    MODULE PROCEDURE AllocateListRX
    MODULE PROCEDURE AllocateListRXY
    MODULE PROCEDURE AllocateListRXYZ
    MODULE PROCEDURE AllocateListR8X
    MODULE PROCEDURE AllocateListR8XY
    MODULE PROCEDURE AllocateListR8XYZ
  END INTERFACE

  INTERFACE DeAllocateArray
    MODULE PROCEDURE DeAllocateArrayIX
    MODULE PROCEDURE DeAllocateArrayIXY
    MODULE PROCEDURE DeAllocateArrayIXYZ
    MODULE PROCEDURE DeAllocateArrayRX
    MODULE PROCEDURE DeAllocateArrayRXY
    MODULE PROCEDURE DeAllocateArrayRXYZ
    MODULE PROCEDURE DeAllocateArrayR8X
    MODULE PROCEDURE DeAllocateArrayR8XY
    MODULE PROCEDURE DeAllocateArrayR8XYZ
    MODULE PROCEDURE DeAllocateArrayLX
    MODULE PROCEDURE DeAllocateArrayLXY
    MODULE PROCEDURE DeAllocateArrayLXYZ
    MODULE PROCEDURE DeAllocateListIX
    MODULE PROCEDURE DeAllocateListIXY
    MODULE PROCEDURE DeAllocateListIXYZ
    MODULE PROCEDURE DeAllocateListRX
    MODULE PROCEDURE DeAllocateListRXY
    MODULE PROCEDURE DeAllocateListRXYZ
    MODULE PROCEDURE DeAllocateListR8X
    MODULE PROCEDURE DeAllocateListR8XY
    MODULE PROCEDURE DeAllocateListR8XYZ
  END INTERFACE

CONTAINS

!  allocation subroutines
  subroutine AllocateArrayIX(array,lowX,highX,ErrorNo)
    type(ArrayIX) :: array
    integer :: lowX,highX,ErrorNo
    allocate(Array%Data(LowX:HighX),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    RETURN
  end subroutine AllocateArrayIX

  subroutine AllocateArrayIXY(array,lowX,highX,lowY,highY,ErrorNo)
    type(ArrayIXY) :: array
    integer :: lowX,highX,lowY,highY,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    RETURN
  end subroutine AllocateArrayIXY

  subroutine AllocateArrayIXYZ(array,lowX,highX,lowY,highY,lowZ,highZ,ErrorNo)
    type(ArrayIXYZ) :: array
    integer :: lowX,highX,lowY,highY,lowZ,highZ,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY,LowZ:HighZ),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    Array%LowZ=LowZ
    Array%HighZ=HighZ
    RETURN
  end subroutine AllocateArrayIXYZ  

  subroutine AllocateArrayRX(array,lowX,highX,ErrorNo)
    type(ArrayRX) :: array
    integer :: lowX,highX,ErrorNo
    allocate(Array%Data(LowX:HighX),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    RETURN
  end subroutine AllocateArrayRX

  subroutine AllocateArrayRXY(array,lowX,highX,lowY,highY,ErrorNo)
    type(ArrayRXY) :: array
    integer :: lowX,highX,lowY,highY,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    RETURN
  end subroutine AllocateArrayRXY

  subroutine AllocateArrayRXYZ(array,lowX,highX,lowY,highY,lowZ,highZ,ErrorNo)
    type(ArrayRXYZ) :: array
    integer :: lowX,highX,lowY,highY,lowZ,highZ,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY,LowZ:HighZ),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    Array%LowZ=LowZ
    Array%HighZ=HighZ
    RETURN
  end subroutine AllocateArrayRXYZ

  subroutine AllocateArrayR8X(array,lowX,highX,ErrorNo)
    type(ArrayR8X) :: array
    integer :: lowX,highX,ErrorNo
    allocate(Array%Data(LowX:HighX),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    RETURN
  end subroutine AllocateArrayR8X

  subroutine AllocateArrayR8XY(array,lowX,highX,lowY,highY,ErrorNo)
    type(ArrayR8XY) :: array
    integer :: lowX,highX,lowY,highY,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    RETURN
  end subroutine AllocateArrayR8XY

  subroutine AllocateArrayR8XYZ(array,lowX,highX,lowY,highY,lowZ,highZ,ErrorNo)
    type(ArrayR8XYZ) :: array
    integer :: lowX,highX,lowY,highY,lowZ,highZ,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY,LowZ:HighZ),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    Array%LowZ=LowZ
    Array%HighZ=HighZ
    RETURN
  end subroutine AllocateArrayR8XYZ  

  subroutine AllocateArrayLX(array,lowX,highX,ErrorNo)
    type(ArrayLX) :: array
    integer :: lowX,highX,ErrorNo
    allocate(Array%Data(LowX:HighX),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    RETURN
  end subroutine AllocateArrayLX

  subroutine AllocateArrayLXY(array,lowX,highX,lowY,highY,ErrorNo)
    type(ArrayLXY) :: array
    integer :: lowX,highX,lowY,highY,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    RETURN
  end subroutine AllocateArrayLXY

  subroutine AllocateArrayLXYZ(array,lowX,highX,lowY,highY,lowZ,highZ,ErrorNo)
    type(ArrayLXYZ) :: array
    integer :: lowX,highX,lowY,highY,lowZ,highZ,ErrorNo
    allocate(Array%Data(LowX:HighX,LowY:HighY,LowZ:HighZ),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Array%LowX=LowX
    Array%HighX=HighX
    Array%LowY=LowY
    Array%HighY=HighY
    Array%LowZ=LowZ
    Array%HighZ=HighZ
    RETURN
  end subroutine AllocateArrayLXYZ

!  deallocation subroutines  
  subroutine DeAllocateArrayIX(array,ErrorNo)
    type(ArrayIX) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    return
  end subroutine DeAllocateArrayIX

  subroutine DeAllocateArrayIXY(array,ErrorNo)
    type(ArrayIXY) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    return
  end subroutine DeAllocateArrayIXY

  subroutine DeAllocateArrayIXYZ(array,ErrorNo)
    type(ArrayIXYZ) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    Array%LowZ=0
    Array%HighZ=0
    return
  end subroutine DeAllocateArrayIXYZ 
 
  subroutine DeAllocateArrayRX(array,ErrorNo)
    type(ArrayRX) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    return
  end subroutine DeAllocateArrayRX

  subroutine DeAllocateArrayRXY(array,ErrorNo)
    type(ArrayRXY) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    return
  end subroutine DeAllocateArrayRXY

  subroutine DeAllocateArrayRXYZ(array,ErrorNo)
    type(ArrayRXYZ) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    Array%LowZ=0
    Array%HighZ=0
    return
  end subroutine DeAllocateArrayRXYZ

  subroutine DeAllocateArrayR8X(array,ErrorNo)
    type(ArrayR8X) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    return
  end subroutine DeAllocateArrayR8X

  subroutine DeAllocateArrayR8XY(array,ErrorNo)
    type(ArrayR8XY) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    return
  end subroutine DeAllocateArrayR8XY

  subroutine DeAllocateArrayR8XYZ(array,ErrorNo)
    type(ArrayR8XYZ) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    Array%LowZ=0
    Array%HighZ=0
    return
  end subroutine DeAllocateArrayR8XYZ

  subroutine DeAllocateArrayLX(array,ErrorNo)
    type(ArrayLX) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    return
  end subroutine DeAllocateArrayLX

  subroutine DeAllocateArrayLXY(array,ErrorNo)
    type(ArrayLXY) :: array
    integer,optional :: ErrorNo

    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    return
  end subroutine DeAllocateArrayLXY

  subroutine DeAllocateArrayLXYZ(array,ErrorNo)
    type(ArrayLXYZ) :: array
    integer,optional :: ErrorNo
    if (present(ErrorNo)) then
      DeAllocate(Array%Data,STAT=ErrorNo)
      if (ErrorNo /= 0) return
    else
      DeAllocate(Array%Data)
    end if
    Array%LowX=0
    Array%HighX=0
    Array%LowY=0
    Array%HighY=0
    Array%LowZ=0
    Array%HighZ=0
    return
  end subroutine DeAllocateArrayLXYZ
!
!  ALLOCATE LISTS
!
  subroutine AllocateListIX(List,No,ErrorNo)
    type(ListIX) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListIX

  subroutine AllocateListIXY(List,No,ErrorNo)
    type(ListIXY) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    allocate(List%Y(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListIXY

  subroutine AllocateListIXYZ(List,No,ErrorNo)
    type(ListIXYZ) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    allocate(List%Y(No),STAT=ErrorNo)
    allocate(List%Z(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListIXYZ

  subroutine AllocateListRX(List,No,ErrorNo)
    type(ListRX) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListRX

  subroutine AllocateListRXY(List,No,ErrorNo)
    type(ListRXY) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    allocate(List%Y(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListRXY

  subroutine AllocateListRXYZ(List,No,ErrorNo)
    type(ListRXYZ) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    allocate(List%Y(No),STAT=ErrorNo)
    allocate(List%Z(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListRXYZ

  subroutine AllocateListR8X(List,No,ErrorNo)
    type(ListR8X) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListR8X

  subroutine AllocateListR8XY(List,No,ErrorNo)
    type(ListR8XY) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    allocate(List%Y(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListR8XY

  subroutine AllocateListR8XYZ(List,No,ErrorNo)
    type(ListR8XYZ) :: List
    integer :: No,ErrorNo
    allocate(List%X(No),STAT=ErrorNo)
    allocate(List%Y(No),STAT=ErrorNo)
    allocate(List%Z(No),STAT=ErrorNo)
    if (ErrorNo /= 0) return
    List%No=0
    RETURN
  end subroutine AllocateListR8XYZ

  subroutine DeAllocateListIX(List,ErrorNo)
    type(ListIX) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    List%No=0
    return
  end subroutine DeAllocateListIX

  subroutine DeAllocateListIXY(List,ErrorNo)
    type(ListIXY) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    DeAllocate(List%Y,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Y)
    List%No=0
    return
  end subroutine DeAllocateListIXY

  subroutine DeAllocateListIXYZ(List,ErrorNo)
    type(ListIXYZ) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    DeAllocate(List%Y,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Y)
    DeAllocate(List%Z,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Z)
    List%No=0
    return
  end subroutine DeAllocateListIXYZ

  subroutine DeAllocateListRX(List,ErrorNo)
    type(ListRX) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    List%No=0
    return
  end subroutine DeAllocateListRX

  subroutine DeAllocateListRXY(List,ErrorNo)
    type(ListRXY) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    DeAllocate(List%Y,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Y)
    List%No=0
    return
  end subroutine DeAllocateListRXY

  subroutine DeAllocateListRXYZ(List,ErrorNo)
    type(ListRXYZ) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    DeAllocate(List%Y,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Y)
    DeAllocate(List%Z,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Z)
    List%No=0
    return
  end subroutine DeAllocateListRXYZ

  subroutine DeAllocateListR8X(List,ErrorNo)
    type(ListR8X) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    List%No=0
    return
  end subroutine DeAllocateListR8X

  subroutine DeAllocateListR8XY(List,ErrorNo)
    type(ListR8XY) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    DeAllocate(List%Y,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Y)
    List%No=0
    return
  end subroutine DeAllocateListR8XY

  subroutine DeAllocateListR8XYZ(List,ErrorNo)
    type(ListR8XYZ) :: List
    integer :: ErrorNo
    DeAllocate(List%X,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%X)
    DeAllocate(List%Y,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Y)
    DeAllocate(List%Z,STAT=ErrorNo)
    if (ErrorNo /= 0) return
    Nullify(List%Z)
    List%No=0
    return
  end subroutine DeAllocateListR8XYZ
!
end MODULE SiberiaTypes

! =================================================================
! =================================================================
! =================================================================

MODULE InputOutputStreams 
! =================================================================
!        Input/Output Units Definition
! =================================================================
    IMPLICIT NONE
    INTEGER :: InputStream
    DATA InputStream / 5 /
END MODULE InputOutputStreams

! =================================================================
! =================================================================
! =================================================================

MODULE ModelFile
! =================================================================
!    Global information for reading of model files
! =================================================================
    integer,parameter :: NoCommands=8
    CHARACTER(20),parameter :: Commands(NoCommands)=                    &
!             1-5
&        (/ 'EROSION    '                          &
&          ,'RUNOFF     '                           &
&          ,'UPLIFT     '                           &
&          ,'AGGRADATION'                      &
&          ,'CHANNEL    '                          &
!             6-
&          ,'ERODIBILITY'                      &
&          ,'LAYER      '                            &
&          ,'TRANSPORT  '/)
    integer,parameter :: Model_Erosion=1,Model_Runoff=2,Model_Uplift=3  &
&       ,Model_Aggradation=4,Model_Channel=5,Model_Erodibility=6        &
&       ,Model_Layer=7,Model_Transport=8

    integer,parameter :: NoOptionsRelAbs=5
    CHARACTER(9),parameter :: OptionsRelAbs(NoOptionsRelAbs)=            &
&        (/ 'RELATIVE'     &
&          ,'ABSOLUTE'     &
&          ,'DEFAULT '       &
&          ,'ON      '           &
&          ,'OFF     ' /)                ! DEFAULT needed by layers
    integer,parameter :: Model_Relative=1,Model_Absolute=2              &
&         ,Model_Default=3,Model_RelAbs_On=4,Model_RelAbs_Off=5

    integer,parameter :: NoOptionsOnOff=3
    CHARACTER(8),parameter :: OptionsOnOff(NoOptionsOnOff)=              &
&        (/ 'ON     '          &
&          ,'OFF    '         &
&          ,'DEFAULT'/)
    integer,parameter :: Model_On=1,Model_Off=2
!  NB. Model_Default is defined above

    integer,parameter :: NoOptionsYN=2
    CHARACTER(2),parameter :: OptionsYN(NoOptionsYN)=                    &
&        (/ 'Y'            &
&          ,'N'/)
    integer,parameter :: Model_Yes=1,Model_No=2

END MODULE ModelFile

! =================================================================
! =================================================================
! =================================================================

MODULE Parameters
    INTEGER :: MaxNoInts,MaxNoReals,MaxUser,NoTotal
    PARAMETER (MaxNoInts=20,MaxNoReals=50,MaxUser=10                       &
&        ,NoTotal=MaxNoInts+MaxNoReals)
END MODULE Parameters

MODULE ModelParameters 
! =================================================================
!        The model parameters ... global to the whole code
!   NB This MODULE should be kept consistent with TYPE LOCALPARAMETERS in
!      MODULE SiberiaTypes.
! =================================================================
!                                                    
! 
  USE Parameters
    IMPLICIT NONE
    INTEGER :: RunTime,StatsTime,kx,ky,modeIC,TimeUp                       &
&         ,ModeSolver,ModeDir,ModeUplift                                   &
&         ,ModeRandom,ModeErode,ModeRunoff,ModeChannel                     &
&         ,ModeDP,ModeMC,DirReg,ModeSoil                                   &
&         ,idummy18,idummy19,idummy20
    REAL(KIND(0.0D0)) :: dZ,dZn,dZHold,QsHold,FactMx                       &
&         ,FRanMn,c1,YFix,FRanCV,b3SDs,b3SDl,TAmp,TPeriod,TPhase,FRanZ     &
&         ,a1,m3,b3,b1,m1,n1,Bulk,InitTimeStep,b5,n5,SInit,m5              &
&         ,YHold,Notch,Cover,s0max,DTime,OTime,GridXY,East                 &
&         ,North,b6,m6,b12,m12,SDRate,SDExp1,SDExp2                        &
&         ,SMThreshold,SDSMWgt,rdummy46,rdummy47,rdummy48,rdummy49         &
&         ,rdummy50
    INTEGER,PARAMETER :: FileFactor=1,FileRunoff=2,FileUplift=3            &
&         ,FileDirections=4,FileMonteCarlo=5,FileChannels=6,FileLayers=7   &
&         ,FilenameDummy8=8,FileControl=9,FileOthers=10
! 
!   NOTE: 'MaxNoRealsMult' in 'multipliers.inc'
!         should always be the same as 'MaxNoReals' 
! 
! 
    INTEGER :: IntVar(MaxNoInts)
    REAL(KIND(0.0D0)) :: Realvar(MaxNoReals)
    CHARACTER(1000) :: FileNameUser(MaxUser)
!    CHARACTER(80) :: FileNameUser(MaxUser)
! 
    EQUIVALENCE                                                             &
&          (RunTime,Intvar(1)),(StatsTime,Intvar(2)),(kx,Intvar(3))         &
&         ,(ky,Intvar(4)),(modeIC,Intvar(5)),(TimeUp,Intvar(6))             &
&         ,(ModeSolver,Intvar(7)),(ModeDir,Intvar(8))                       &
&         ,(ModeUplift,Intvar(9)),(ModeRandom,Intvar(10))                   &
&         ,(ModeErode,Intvar(11)),(ModeRunoff,Intvar(12))                   &
&         ,(ModeChannel,Intvar(13)),(ModeDP,Intvar(14))                     &
&         ,(ModeMC,Intvar(15)),(DirReg,Intvar(16))                          &
&         ,(ModeSoil,Intvar(17)),(idummy18,Intvar(18))                      &
&         ,(idummy19,Intvar(19)),(idummy20,Intvar(20))
    EQUIVALENCE                                                             &
&          (dZ,RealVar(1)),(dZn,RealVar(2)),(dZHold,RealVar(3))             &
&         ,(QsHold,RealVar(4)),(FactMx,RealVar(5)),(FRanMn,RealVar(6))      &
&         ,(c1,RealVar(7)),(YFix,RealVar(8)),(FRanCV,RealVar(9))            &
&         ,(b3SDs,RealVar(10))                                              &
&         ,(b3SDl,RealVar(11)),(TAmp,RealVar(12)),(TPeriod,RealVar(13))     &
&         ,(TPhase,RealVar(14)),(FRanZ,RealVar(15)),(a1,RealVar(16))        &
&         ,(m3,RealVar(17)),(b3,RealVar(18)),(b1,RealVar(19))               &
&         ,(m1,RealVar(20)),(n1,RealVar(21)),(Bulk,RealVar(22))             &
&         ,(InitTimeStep,RealVar(23))                                       &
&         ,(b5,RealVar(24)),(n5,RealVar(25)),(SInit,RealVar(26))            &
&         ,(m5,RealVar(27)),(YHold,RealVar(28)),(Notch,RealVar(29))         &
&         ,(Cover,RealVar(30))                                              &
&         ,(s0max,RealVar(31)),(DTime,RealVar(32)),(OTime,RealVar(33))      &
&         ,(GridXY,RealVar(34)),(East,RealVar(35)),(North,RealVar(36))      &
&         ,(b6,RealVar(37)),(m6,RealVar(38)),(b12,RealVar(39))              &
&         ,(m12,RealVar(40)),(SDRate,RealVar(41)),(SDExp1,RealVar(42))      &
&         ,(SDExp2,RealVar(43))                                             &
&         ,(SMThreshold,RealVar(44)),(SDSMWgt,RealVar(45))                  &
&         ,(rdummy46,RealVar(46))                                           &
&         ,(rdummy47,RealVar(47)),(rdummy48,RealVar(48))                    &
&         ,(rdummy49,RealVar(49)),(rdummy50,RealVar(50))
! 
! Initialise the parameters
! =========================
! 
! INTEGER PARAMETERS
! ------------------
!  PARAMETER 1
! 
    DATA RunTime / 1000 /  ,StatsTime / 100 /                   &
&       ,kx      / 40 /    ,ky        / 40 /                    &
!  PARAMETER 5
&       ,modeIC     / 0 /  ,TimeUp    / 100 /                   &
&       ,ModeSolver / 4 /  ,ModeDir   / 0 /                     &
&       ,ModeUplift / 0 /                                       &
!  PARAMETER 10
&       ,ModeRandom  / 0 / ,ModeErode   / 0 /                   &
&       ,ModeRunoff  / 0 / ,ModeChannel / 0 /                   &
&       ,ModeDP      / 0 /                                      &
!  PARAMETER 15
&       ,ModeMC   / 0 /    ,DirReg   / 1 /                      &
&       ,ModeSoil / 0 /                                         &
&       ,idummy18,idummy19,idummy20                             &
&                     / 0,0,0 /
! 
! REAL PARAMETERS
! ---------------
! PARAMETER 21
    DATA dZ      / 0.0 /   ,dZn     / 1.0 /                    &
&       ,dZHold  / 0.0 /   ,QsHold  / 0.0 /                    &
! PARAMETER 25
&       ,FactMx  / 1.0 /   ,FRanMn  / 1.0 /                    &
&       ,c1      / 0.0004 /,YFix    / 1.0 /                    &
&       ,FRanCV  / 0.0 /                                       &
! PARAMETER 30
&       ,b3SDs   / 0.0 /   ,b3SDl   / 0.0 /                    &
&       ,TAmp    / 0.0 /   ,TPeriod / 0.0 /                    &
&       ,TPhase  / 0.0 /                                       &
! PARAMETER 35
&       ,FRanZ  / 0.005 /  ,a1      / 1.0 /                    &
&       ,m3     / 1.0 /    ,b3      / 1.0 /                    &
&       ,b1     / 0.01 /                                       &
! PARAMETER 40
&       ,m1       / 1.8 /  ,n1      / 2.1 /                    &
&       ,Bulk     / 1.0 /  ,InitTimeStep  / -0.025 /           &
&       ,b5       / 2.5 /                                      &
! PARAMETER 45
&       ,n5     / 0.3 /    ,SInit   / 10.0 /                   &
&       ,m5     / 0.4 /    ,YHold   / 0.1 /                    &
&       ,Notch  / 100.0 /                                      &
! PARAMETER 50
&       ,Cover    / 1.0 /  ,s0max   / 0.0 /                    &
&       ,DTime    / 1.0 /  ,OTime   / 1.0 /                    &
&       ,GridXY   / 1.0 /                                      &
! PARAMETER 55
&       ,East  / 0.0 /     ,North   / 0.0 /                    &
&       ,b6    / 0.0 /     ,m6      / 1.0 /                    &
&       ,b12   / 0.0 /                                         &
! PARAMETER 60
&       ,m12    / 1.8 /    ,SDRate  / 0.00001 /                &
&       ,SDExp1 / 1.0 /    ,SDExp2  / 1.0 /                    &
&       ,SMThreshold / 1.0 /                                   &
! PARAMETER 65
&       ,SDSMWgt / 0.0 /                                       &
&       ,rdummy46,rdummy47,rdummy48,rdummy49,rdummy50          &
&             / 0,0,0,0,0 /
! 
    INTEGER, PRIVATE :: i
    CHARACTER(10),parameter :: ParameterTitles(nototal)=        &                   
!              names for parameters 1 - 10
&     (/'RunTime   ','StatsTime ','Domain_X  ','Domain_Y  '     &
&      ,'ModeIC    ','TimeUplift'                               &
&      ,'ModeSolver','ModeDir   ','ModeUplift','ModeRandom'     &
!              names for parameters 11 - 20
&      ,'ModeErode ','ModeRunoff','ModeChanel','ModeDP    '     &
&      ,'ModeMCarlo','DirReg    '                               &
&      ,'ModeSoil  ','          ','          ','          '     &
!              names for parameters 21 - 30
&      ,'dZ        ','dZn       ','dZTHold   ','QsTHold   '     &
&      ,'FactMax   ','FRanMin   '                               &
&      ,'1/at      ','YFix      ','FRanCV    ','b3SDs     '     &
!              names for parameters 31 - 40
&      ,'b3SDl     ','UpAmp     ','UpPeriod  ','UpPhase   '     &
&      ,'FRanZ     ','          '                               &
&      ,'m3        ','b3        ','b1        ','m1        '     &
!              names for parameters 41 - 50
&      ,'n1        ','BulkDen   ','TimeStep  ','b5        '     &
&      ,'n5        ','SInit     '                               &
&      ,'m5        ','YHold     ','Notch     ','Cover     '     &
!              names for parameters 51 - 60
&      ,'s0max     ','DTime     ','OTime     ','GridXY    '     &
&      ,'East      ','North     '                               &
&      ,'b6        ','m6        ','b1(2)     ','m1(2)     '     &
!              names for parameters 61 - 70
&      ,'SoilRate  ','SoilExp1  ','SoilExp2  ','SMTHold   '     &
&      ,'SoilSMWgt ','          '                               &
&      ,'          ','          ','          ','          '/)
! 
    CHARACTER(10),parameter :: FileTitles(MaxUser)=             &
&      (/'Erosion   '     &
&       ,'Runoff    '     &
&       ,'Uplift    '     &
&       ,'Directions'     &
&       ,'MonteCarlo'     &
&       ,'Channel   '     &
&       ,'Layers    '     &
&       ,'          '     &
&       ,'Control   '     &
&       ,'Others    ' /)
! 
    DATA (FilenameUser(i),i=1,MaxUser) / MaxUser*' ' /
! 
END MODULE ModelParameters

! =================================================================
! =================================================================
! =================================================================

MODULE Multipliers 
  USE Parameters
! =================================================================
!        Physical Multipliers
! =================================================================
!   multipliers TO correct for physical coordinates
! 
    IMPLICIT NONE
! 
    REAL(KIND(0.0D0)) :: MultiplierSlope,MultiplierArea                           &
&           ,MultiplierRealVar(MaxNoReals)
    DATA MultiplierRealVar  / MaxNoReals*1.0 /
    DATA MultiplierSlope    / 1.0 /
    DATA MultiplierArea     / 1.0 /
 END MODULE Multipliers


