! 
! 
! ===================================================================|
!                                                                    |
!                    SIBERIA LICENSE AGREEMENT                       |
!                    -------------------------                       |
!                                                                    |
!  Please READ the following licence information carefully. This     |
!  computer PROGRAM ("SIBERIA") is licensed, not sold, TO you for USE|
!  only under the terms of this license, and the copyright owner     |
!  reserves any rights not expressly granted TO you.  You own the    |
!  computer media on which SIBERIA is originally and subsequently    |
!  recorded or fixed, but the copyright owner retains ownership      |
!  of all copies of SIBERIA itself                                   |
!                                                                    |
!  Unless otherwise stated this licence entitles you TO              |
!     (a) copy this code onto a single computer,                     |
!     (b) make backup copies of this software,                       |
!                                                                    |
!  You may not                                                       |
!     (a) remove these license agreement, disclaimer, copyright, or  |
!         limitation of damages notices from this source code,       |
!     (b) distribute this software TO others,                        |
!     (c) rent,lease, resell, distribute, network, or create         |
!         derivative products works based upon this software, or     |
!         any part thereof.                                          |
!     (d) modify the software in this file without the written       |
!         permission of the copyright owner.                         |
!     (e) disclose this source code and algorithms TO                |
!         unlicensed users                                           |
!                                                                    |
!  This Licence is effective until terminated. This Licence will     |
!  terminate automatically without notice from the copyright owner   |
!  IF you fail TO comply with any provision of this Licence. Upon    |
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
!  warranty with respect TO its merchantability, or its fitness for  |
!  any particular purpose. The entire risk as TO the quality and     |
!  performance of SIBERIA is with you. Should SIBERIA                |
!  prove defective, you (and not the copyright owner), assume the    |
!  entire cost of all necessary servicing, repair or correction.     |
!                                                                    |
!  The copyright owner does not warrant that the functions contained |
!  in SIBERIA will meet your requirements or that the operation      |
!  of SIBERIA will be uninterrrupted or error free or that defects   |
!  in SIBERIA will be corrected                                      |
!                                                                    |
! -------------------------------------------------------------------|
!                                                                    |
!                LIMITATION OF DAMAGES                               |
!                ---------------------                               |
!                                                                    |
!  In no event will the copyright owner be liable (i) TO you for any |
!  incidental, consequential or indirect damages (including damages  |
!  for loss of business profits, business interruption, loss of      |
!  business information, and the like) arising out of the USE of or  |
!  inability TO USE SIBERIA even IF the copyright owner has been     |
!  advised of the possibility of such damages, or (ii) for any       |
!  claim by any other party.                                         |
!                                                                    |
! ====================================================================
! 
!
! This file includes dummy calls TO PVm routines that we can USE
! compiling serial Version without PVM
! 
!   Latest revision 21/2/1996   GRW
!                   11/8/2000   GRW: changed INTERFACE so that area is REAL(8) ::  
! 
!
      SUBROUTINE PVMDataGet(ivar,ino,rvar,rno,Slope,RanField,y,z,Area               &
&          ,Direct,cDepth,SoilDepth,GridX,GridY,filenm)
      INTEGER :: ino,rno,GridX,GridY,Direct,ivar,j,info
      REAL(8) :: Slope,RanField,y,z,cDepth,SoilDepth,rvar,Area
      CHARACTER(*) :: filenm
      RETURN
      END
! 
      SUBROUTINE PVMDataSend(ivar,ino,rvar,rno,Slope,RanField,y,z               &
&          ,Direct,cDepth,SoilDepth,GridX,GridY,filenm)
      INTEGER :: ino,rno,GridX,GridY,Direct,ivar,j,info
      REAL(8) :: Slope,RanField,y,z,cdepthi,SoilDepth,rvar,Area
      CHARACTER(*) :: filenm
      RETURN
      END
! 
      SUBROUTINE PVMBNDFileReady
      RETURN
      END
! 
      SUBROUTINE PVMBNDFileClosed
      RETURN
      END
