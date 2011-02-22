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
MODULE UserUpliftAnalysis

  REAL,PARAMETER :: UserUpliftAnalysis_Version=8.17

CONTAINS

! 
!  ===================================================================
!                   UPLIFT
!  ===================================================================
! 
SUBROUTINE UserUplift(tottime,timestep,iTime,z,s0                   &
&          ,GridX,GridY,Uplift,SumCycle,LowX,HighX,LowY,HighY       &
&          ,SimParameters)
  USE SiberiaTypes
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER iTime,GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: tottime,timestep,Uplift(GridX,GridY)               &
&           ,SumCycle,z(GridX,GridY),s0(GridX,GridY)
    TYPE(LocalParameters) :: SimParameters
! 
!  INPUT PARAMETERS
!  ----------------
! 
!   iTime    INTEGER   current simulation time truncated TO the next lowest INTEGER
!   tottime  REAL      iTime+tottime = current time from start of simulation
!   timestep REAL      the size of the time step (note this may vary with each CALL
!                      of this SUBROUTINE)
!   z        REAL      the array of elevations
!   s0       REAL      the array of slopes
!   GridX,GridY     INTEGER   array size
!   SimParameters
!            record    The parameters for the simulation
! 
!  OUTPUT PARAMETERS
!  -----------------
! 
!   Uplift   REAL     the uplift for the next 'timestep'
!   SumCycle REAL     a running total of the uplift TO date (used in the output
!                     routine)
! 
! ------------------------------------------------
!  ADD LOCAL VARIABLES AFTER HERE 
! ------------------------------------------------
! 
    INTEGER :: TModeUplift
!    LOGICAL :: First
!    DATA First / .true. /
!    SAVE First
! 
      TModeUplift=-SimParameters%ModeUplift
      select case (TModeUplift)
      case default
        WRITE (*,*)                &
&       ' -- Internal error in user defined uplift MODULE',SimParameters%ModeUplift
        STOP
! 
! ------------------------------------------------
!  ADD NEW MODELS AFTER HERE (ie. negative ModeErode)
! ------------------------------------------------
! 
      case (1) 
        WRITE (*,*) ' -- User uplift MODULE option not implemented'
      end select
! 
! -------------------------------------------------
!  Doing standard SUBROUTINE exit stuff
! -------------------------------------------------
! 
      FirstUserUplift=.false.
      RETURN
END SUBROUTINE UserUplift


end MODULE UserUpliftAnalysis
