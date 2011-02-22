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
! ---------------------------------------------------------------------
!   Control MODULE
! ---------------------------------------------------------------------
! 
MODULE Control
  IMPLICIT NONE
  INTEGER :: MaxNoIterations
  REAL(KIND(0.0D0)), PRIVATE :: TerminationCriteria
  CONTAINS
! 
! ---------------------------------------------------------------------
!   Calculate the termination criteria
! ---------------------------------------------------------------------
! 
  LOGICAL FUNCTION Termination(RunTime,iTime,Z,LowX,HighX,LowY,HighY             &
&             ,Domain,IrregularBoundary,gridX,gridY)
    USE Support
    USE CtrOutput
    IMPLICIT NONE
! 
    INTEGER :: RunTime,iTime,LowX,HighX,LowY,HighY,gridX,gridY
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: Z
    LOGICAL :: IrregularBoundary
    LOGICAL,DIMENSION(gridX,gridY) :: Domain
! 
    REAL(KIND(0.0D0)) :: NewInt
! 
    Termination=.false.
    IF (iTime >= MaxNoIterations) THEN
      Termination=.true.
      CALL Message_Output(Message_Info                                           &
&             ,' Maximum Iterations in Termination Criteria ',abs(RunTime)       &
&             ,' > ',MaxNoIterations)
    ELSE
      SELECT CASE (RunTime)
      CASE (-3)
        NewInt=CalcHypsInt(z,LowX,HighX,LowY,HighY,IrregularBoundary,Domain      &
&              ,gridX,gridY)
        IF (NewInt < TerminationCriteria) THEN
          CALL Message_Output(Message_Info                                       &
&             ,' Hypsometric Integral < Termination Criteria',NewInt             &
&             ,' < ',TerminationCriteria)
          Termination=.true.
        ELSE
          Termination=.false.
!      CALL done
        END IF
      CASE (-4)
        NewInt=CalcHypsInt(z,LowX,HighX,LowY,HighY,IrregularBoundary,Domain      &
&                  ,gridX,gridY)
        IF (NewInt > TerminationCriteria) THEN
          CALL Message_Output(Message_Info                                       &
&               ,' Hypsometric Integral > Termination Criteria',NewInt           &
&               ,' > ',TerminationCriteria)
          Termination=.true.
        ELSE
          Termination=.false.
!      CALL done
        END IF
      CASE DEFAULT
        Termination=.false.
        CALL Message_Output(Message_ErrorStop                                     &
&             ,' Invalid termination criteria selected with PARAMETER RUNTIME')
      END SELECT
    END IF
! 
    RETURN
  END FUNCTION Termination
! 
! ---------------------------------------------------------------------
!   Input information from the control file
! ---------------------------------------------------------------------
! 
  SUBROUTINE InputControlFile(RunMode,ErrorMode)
    USE ModelParameters
    USE Support
    IMPLICIT NONE
! 
    INTEGER :: RunMode,ErrorMode
! 
    INTEGER,PARAMETER :: unitno=60
    INTEGER :: lgth,start,i,ibuff
    LOGICAL :: lexist
    CHARACTER(80) :: line,buffer
! 
    CHARACTER(20) :: commands(4)
    CHARACTER(80) :: atom
    INTEGER :: SELECT
    SAVE commands
    DATA commands(1)    / 'NULL'/                    &
&        commands(2)    / 'NULL'/                    &
&        commands(3)    / 'HYP_INT_<'/               &
&        commands(4)    / 'HYP_INT_>'/
! 
!  RETURN IF control is not required by user inputs
    IF (RunTime >= -2) RETURN
! 
    Lgth=len_trim(FilenameUser(FileControl))
    IF (lgth >= 1) THEN
      INQUIRE(file=FilenameUser(FileControl)(1:lgth), EXIST=lexist)
    ELSE 
      lexist=.false.
    END IF
    IF (ErrorMode == 0) THEN
      IF (.not.lexist) RETURN
    ELSE
      IF (.not.lexist)  THEN
      CALL Message_Output(Message_ErrorStop                              &
&          ,' Required Control File Does Not Exist = '                   &
&         //trim(FilenameUser(FileControl)))
      END IF
    END IF
    OPEN(UNIT=unitno,FILE=FilenameUser(FileControl)(1:lgth)              &
&       ,STATUS='old',ERR=9996)
!  READ HEADER
    Line=' '
    READ(unitno,6000,END=8000,ERR=8001) line
    ibuff=1
    buffer(1:80)=line(1:80)
      CALL Str_nxtat(buffer,ibuff,atom,80)
      CALL Str_UpperCase(atom)
      IF (atom(1:7) /= 'SIBERIA') THEN
        CALL Message_Output(Message_ErrorStop                             &
&            ,' Invalid Header in Control File = '//trim(buffer))
      END IF
! START READING BODY
 8002 Line=' '
    READ(unitno,6000,END=8000,ERR=8001) line
 6000 FORMAT(a)
    DO i=1,80
      start=i
      IF (line(i:i) /= ' ') GO TO 8003
    END DO
    GO TO 8002
 8003 IF (line(start:start) == '#' .or. line(start:start) == '!') GO TO 8002
! 
    ibuff=start
    buffer(1:80)=line(1:80)
    CALL Str_UpperCase(buffer)
    CALL Str_nxtat(buffer,ibuff,atom,80)
    CALL Str_mtchcomm1(atom,commands,SELECT)
! 
!  because there is potentially all sort of junk in the control file
!  ignore most stuff and check that RunTime matches with a termination
!  criteria specified in the file and THEN process this one.
! 
!     -1 -2 are termination modes handled elsewhere in SIBERIA
! 
    SELECT CASE (SELECT)
!   HYPSOMETRIC INTEGRAL
    CASE(3,4)
      IF (-RunTime == SELECT) THEN
        READ(buffer(ibuff:),*,ERR=9997,END=9997)                              &
&             TerminationCriteria,MaxNoIterations
      END IF
    END SELECT
    GO TO 8002
! 
 8000 CLOSE(UNIT=unitno,STATUS='keep')
    RETURN
!  error handling
 9997 CALL Message_Output(Message_ErrorStop                                    &
&         ,' Invalid Termination Criteria in Control File = '//trim(buffer))
    stop
 9996 CALL Message_Output(Message_ErrorStop,' Error Opening Control file = '   &
&         //trim(FilenameUser(FileControl)))
    stop
! 
 8001 CALL Message_Output(Message_ErrorContinue                                &
&         ,'Incomprehensible input 1 in Control File '//trim(line))
    GO TO 8002
  END SUBROUTINE InputControlFile
END MODULE Control
