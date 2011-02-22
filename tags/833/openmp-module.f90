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

MODULE openMPsupport
! =================================================================
!   Declarations for variables that need TO be global PRIVATE for 
!   openMP implementations. Mostly these are variables that are private
!   to a subroutine but which also need to be saved from call to call
!   ... mostly variables that indicate whether this is the first time
!   the subroutine has been called and whether as a result to initilise
!   things. THere are some partial results that need to be saved (for
!   efficiency) from call to call. Eventually all of the arrays
!   should become pointers to arrays for memory efficency.
! =================================================================

  USE SiberiaTypes
  IMPLICIT NONE
!
!  if this is modified or added to remember to make sure the initialisation
!  is done correctly in openMP_init
!
  INTEGER :: ModeArea,SeedDirAnal5,UpliftType,NoAggrade
  REAL(KIND(0.0D0)) :: UpliftStartTime,UpliftEndTime                           
  LOGICAL :: FirstChannel,FirstCtrOut,FirstSedAnal      &
&           ,FirstDirAnal2,FirstDirAnal3,FirstDirAnal6,FirstFactor         &
&           ,FirstCIF,FirstRunoff,FirstUplift,FirstDZ,FirstSedAnal1        &
&           ,FirstOutputSetup,FirstUserDir,FirstUserFactor,FirstUserOther  &
&           ,FirstUserRunoff,FirstUserUplift,FirstAreaAnal,FirstSoilAnal   &
&           ,FirstSedimentTracking,FirstFinite
  REAL(KIND(0.0D0)),dimension(:,:),allocatable :: AreaTerm2SedAnal,TempA   &
&           ,Region_b1,Region_m1,Region_n1,Runoff,Region_b3,Region_m3      &
&           ,Region_b5,Region_m5,Region_n5,RegionUplift,AreaTermFinite
  type(ArrayLXYZ) :: allowDir
  integer,pointer :: AggradeX(:),AggradeY(:)
  real(KIND(0.0D0)),pointer :: Aggrade(:)
  common / openMPvarsI / ModeArea,SeedDirAnal5,UpliftType,NoAggrade
  common / openMPvarsR / UpliftStartTime,UpliftEndTime
  common / openMPvarsL / FirstChannel,FirstCtrOut,FirstSedAnal             &
&           ,FirstDirAnal2,FirstDirAnal3,FirstDirAnal6,FirstFactor         &
&           ,FirstCIF,FirstRunoff,FirstUplift,FirstDZ,FirstSedAnal1        &
&           ,FirstOutputSetup,FirstUserDir,FirstUserFactor,FirstUserOther  &
&           ,FirstUserRunoff,FirstUserUplift,FirstAreaAnal,FirstSoilAnal   &
&           ,FirstSedimentTracking,FirstFinite
  common / openMPptrs / AggradeX,AggradeY,Aggrade
  save /openMPvarsI/, /openMPvarsR/,/openMPvarsL/,/openMPptrs/
!$omp threadprivate(/openMPvarsI/, /openMPvarsR/,//openMPvarsL/,/openMPptrs/)

CONTAINS

!
! -------------------------------------------------------------------
! Initialise the variables ... intialisation in the data statement
! is incompatabile with 'PRIVATE()' declarations on the SGI compiler 
! for openMP.
! -------------------------------------------------------------------
! 
  SUBROUTINE openMP_Init()
  IMPLICIT NONE
!  INCLUDE 'openmpvariables.inc'
! 
!  arrays typically DO not need TO be initialised in the code because
!  they are only used for temporary storage
! 
!  integers
    ModeArea=0
    SeedDirAnal5=1
!  logicals
    FirstAreaAnal=.true.
    FirstChannel=.true.
    FirstCtrOut=.true.
    FirstDirAnal2=.true.
    FirstDirAnal3=.true.
    FirstDirAnal6=.true.
    FirstFactor=.true.
    FirstCIF=.true.
    FirstRunoff=.true.
    FirstSoilAnal=.true.
    FirstUplift=.true.
    FirstDZ=.true.
    FirstSedAnal=.true.
    FirstSedAnal1=.true.
    FirstOutputSetup=.true.
    FirstUserDir=.true.
    FirstUserFactor=.true.
    FirstUserOther=.true.
    FirstUserRunoff=.true.
    FirstUserUplift=.true.
    FirstSedimentTracking=.true.
    FirstFinite=.true.
!
    NoAggrade=0
    Nullify(Aggrade)
    Nullify(AggradeX)
    Nullify(AggradeY)
  RETURN
  END SUBROUTINE openMP_Init

END MODULE openMPsupport
