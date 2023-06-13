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
MODULE UserRunoffAnalysis

  REAL,PARAMETER :: UserRunoffAnalysis_Version=8.17

CONTAINS

SUBROUTINE UserRunoff                                                   &
&            (AreaTerm,b,m,DetCIF,IrregularBoundary,Domain,DirChg       &
&            ,vz,s0,Y,Z,Area,Direct,GridX,GridY                         &
&            ,LowX,HighX,LowY,HighY,SimParameters)
  USE SiberiaTypes
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
    INTEGER :: GridX,GridY,Direct(GridX,GridY),LowX,HighX               &
&         ,LowY,HighY
    REAL(KIND(0.0D0)) :: Z(GridX,GridY),y(GridX,GridY)                            &
&         ,s0(GridX,GridY),vz(GridX,GridY)                              &
&         ,AreaTerm(GridX,GridY),b,m,Area(GridX,GridY)
    LOGICAL :: Domain(GridX,GridY),IrregularBoundary                    &
&         ,DetCIF,DirChg
    TYPE(LocalParameters) :: SimParameters
! 
!  NOTE: For the reasons of efficiency this routine does not RETURN
!  discharge but it returns the FUNCTION of discharge
!                   m
!     AreaTerm = b*Q
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
!  INPUT PARAMETERS
!  ----------------
! 
!   b        REAL      multiplicative factor as indicated
!   m        REAL      power as indicated
!   DetCIF   LOGICAL   true IF deterministic Channel mode is set, false otherwise
!   IrregularBoundary
!            LOGICAL   true IF irregular boundaries are set otherwise boundaries
!                      are rectangular with dimensions kx by ky.
!   Domain   LOGICAL   array setting the boundaries IF irregular boundaries are set
!                      true IF the node is inside the boundary, false otherwise.
!   DirChg   LOGICAL   true IF the drainage directions have changed since the last
!                      CALL TO SedAnal.
!   vz       REAL      the array of random multipliers
!   s0       REAL      the array of slopes
!   Y        REAL      the array indicating the location of channels
!   z        REAL      the array of elevations
!   Area     INTEGER   the array of Area draining through that node
!   Direct   INTEGER   the array of drainage directions
!   GridX,GridY     INTEGER   the array size
!   SimParameters
!            record    The parameters for the simulation
! 
!  OUTPUT PARAMETERS
!  -----------------
! 
!   AreaTerm REAL     the array with the output runoffs in it
! 
! ------------------------------------------------
!  ADD LOCAL VARIABLES AFTER HERE 
! ------------------------------------------------
! 
    INTEGER :: TModeRunoff
! 
      TModeRunoff=-SimParameters%ModeRunoff
      select case(TModeRunoff)
      case default
        WRITE (*,*) ' -- Internal error in user defined runoff MODULE'
        STOP
! 
! ------------------------------------------------
!  ADD NEW MODELS AFTER HERE (ie. negative ModeRunoff)
! ------------------------------------------------
! 
      case (1)
        WRITE (*,*) ' -- User defined mode not implemented'
      end select
! 
! -------------------------------------------------
!  Doing standard SUBROUTINE exit stuff
! -------------------------------------------------
! 
 9999 FirstUserRunoff=.false.
      RETURN
END SUBROUTINE UserRunoff

END MODULE UserRunoffAnalysis
