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
! These routines are TO maintain compatability with Viewer software or
! any other graphics based software where output to the default  
! console may be unacceptable as well as:
!  - automated handling of echoing to a file
!  - automated echoing of warnings and errors to an error log file
!
! These libraries are thread safe for use in openMP (Version 2). 
! You need to declare the module as PRIVATE in the openMP parallel command.
!
! These libraries are NOT threadsafe for openMP (Version 1).
! 
!
! 
! ---------------------------------------------------------------------
!  standard message handler 
! ---------------------------------------------------------------------
!
MODULE Support

!-----------------------------------------------------------------------
!                    PUBLIC DEFINITIONS
!-----------------------------------------------------------------------

  REAL, PARAMETER,public :: Support_Version = 8.33
  REAL (kind(0.0d0)),parameter :: double=0.0

  integer, parameter,public ::       &
&    Message_Info=0                  & ! just output a message and RETURN
&   ,Message_ErrorStop=1             & ! output an error message and STOP
&   ,Message_ErrorContinue=2         & ! output an error message and RETURN
&   ,Message_WarnStop=3              & ! output a warning message and STOP
&   ,Message_WarnContinue=4          & ! output a warning message and RETURN
!   default behaviours
&   ,Message_Error=Message_ErrorStop          & ! default error behaviour
&   ,Message_Warning=Message_WarnContinue     & ! default warning behaviour
&   ,Fatal=Message_ErrorStop          & ! default error behaviour
&   ,Warning=Message_WarnContinue     & ! default warning behaviour

&   ,Message_FieldWidth=1            &
&   ,Message_RealFieldWidth=2            &
&   ,Message_IntegerFieldWidth=3            &
&   ,Message_DecPt=4

  integer, parameter,public ::       &
&    EchoFileStart=0                 &
&   ,EchoFileClose=1                 &
&   ,EchoFileSuspend=2               &
&   ,EchoFileResume=3

  integer, parameter,public ::       &
&    ErrorFileStart=0                &
&   ,ErrorFileClose=1                &
&   ,ErrorFileSuspend=2              &
&   ,ErrorFileResume=3               &
&   ,ErrorFileWarnOnly=4             &
&   ,ErrorFileErrorOnly=5            &
&   ,ErrorFileErrorWarn=6
  logical,parameter,public :: On=.true., Off=.false.

!-----------------------------------------------------------------------
!                    PRIVATE DEFINITIONS
!-----------------------------------------------------------------------

  integer,parameter,private :: FileLineLgth=1000
  integer,parameter :: keywordlength=20

  Integer,private :: EchoFileUnit,ErrorFileUnit,EchoFileStatus             &
&     ,ErrorFileStatus,MessageField=0,MessageIntegerField=0                &
&     ,MessageRealField=0,MessageDecPt=0
  logical,private :: EchoFileActive=.false.,ErrorFileActive=.false.        &
&     ,FirstInit=.true.,PauseAtEnd=.true.,EchoFileOutput=.false.           &
&     ,ErrorFileOutput=.true.,ErrorFileOpen=.false.,EchoFileOpen=.false.   &
&     ,prompt=.true.
  character(255),private :: ErrorFileName='errors.txt'                     &
&     ,EchoFileName='echo.txt'

  INTERFACE Support_Set
    MODULE PROCEDURE SupportSetI
    MODULE PROCEDURE SupportSetR
    MODULE PROCEDURE SupportSetR8
    MODULE PROCEDURE SupportSetL
    MODULE PROCEDURE SupportSetStr
  END INTERFACE Support_Set
  INTERFACE Support_Get
    MODULE PROCEDURE SupportGetI
    MODULE PROCEDURE SupportGetR
    MODULE PROCEDURE SupportGetR8
    MODULE PROCEDURE SupportGetL
    MODULE PROCEDURE SupportGetStr
  END INTERFACE Support_Get
  INTERFACE Message_Output
    MODULE PROCEDURE MessageOutput
    MODULE PROCEDURE MessageOutputI
    MODULE PROCEDURE MessageOutputR
    MODULE PROCEDURE MessageOutputR8
    MODULE PROCEDURE MessageOutputIR
    MODULE PROCEDURE MessageOutputIR8
    MODULE PROCEDURE MessageOutputIIR
    MODULE PROCEDURE MessageOutputIIR8
    MODULE PROCEDURE MessageOutputRIIR
    MODULE PROCEDURE MessageOutputR8IIR8
  END INTERFACE Message_Output
  INTERFACE Message_Set
    MODULE PROCEDURE MessageSetI
  END INTERFACE Message_Set
  INTERFACE Message_Get
    MODULE PROCEDURE MessageGetI
  END INTERFACE Message_Get

! Timer Routine Definitions

  integer,parameter :: UpperCase=1,LowerCase=2,IgnoreCase=3
  integer,parameter :: MaxNoThreads=100
  INTEGER,PRIVATE :: TimerFileUnit
  integer,private :: CountStart(0:MaxNoThreads),TimeArray(8,0:MaxNoThreads)
  REAL,PRIVATE :: TimeStart
  LOGICAL,PRIVATE :: TimerEcho=.false.

CONTAINS


! =========================================================================
!          ROUTINES FOR CONTROLLING THE SUPPORT LIBRARY
!          --------------------------------------------
!
!      All publically accesssible routines start with Support_
!      All routines private to the module start with Support (no trailing underline)
! =========================================================================

subroutine SupportSetI(Variable,value)
  implicit none
    character(*) :: Variable
    integer :: value
!
    character(20) :: temp
!
    temp=' '
    temp=variable
    call Str_UpperCase(temp)
    select case (temp)
      case default
        call Message_Output   &
&           (Message_ErrorStop,'Unknown Variable in call to SUPPORT_SET = '//trim(variable))
!  echo file variables
      case('ECHOFILESTATUS')
        EchoFileStatus=value
! error file variables
      case('ERRORFILESTATUS')
        ErrorFileStatus=value
    end select
end subroutine SupportSetI

subroutine SupportSetR(Variable,value)
  implicit none
    character(*) :: Variable
    real :: value
!
end subroutine SupportSetR

subroutine SupportSetR8(Variable,value)
  implicit none
    character(*) :: Variable
    REAL(KIND(0.0D0)) :: value
!
end subroutine SupportSetR8

subroutine SupportSetStr(Variable,value)
  implicit none
    character(*) :: Variable,value
!
end subroutine SupportSetStr

subroutine SupportSetL(Variable,value)
  implicit none
    character(*) :: Variable
    logical :: value
!
    character(20) :: temp
!
    temp=' '
    temp=variable
    call Str_UpperCase(temp)
    select case (temp)
      case default
        call Message_Output   &
&           (Message_ErrorStop,'Unknown Variable in call to SUPPORT_SET = '//trim(variable))
! error file variables
      case('PROMPT')
        prompt=value
    end select
end subroutine SupportSetL

subroutine SupportGetI(Variable,value)
  implicit none
    character(*) :: Variable
    integer :: value
!
    character(20) :: temp
!
    temp=' '
    temp=variable
    call Str_UpperCase(temp)
    select case (temp)
      case default
        call Message_Output   &
&           (Message_ErrorStop,'Unknown Variable in call to SUPPORT_GET = '//trim(variable))
!  echo file variable
      case('ECHOFILESTATUS')
        value=EchoFileStatus
!  echo file unit
      case('ECHOFILEUNIT')
        value=EchoFileUnit
! error file variable
      case('ERRORFILESTATUS')
        value=ErrorFileStatus
! error file unit
      case('ERRORFILEUNIT')
        value=ErrorFileUnit
    end select
end subroutine SupportGetI

subroutine SupportGetR(Variable,value)
  implicit none
    character(*) :: Variable
    real :: value
!
end subroutine SupportGetR

subroutine SupportGetR8(Variable,value)
  implicit none
    character(*) :: Variable
    REAL(KIND(0.0D0)) :: value
!
end subroutine SupportGetR8

subroutine SupportGetL(Variable,value)
  implicit none
    character(*) :: Variable
    logical :: value
!
end subroutine SupportGetL


subroutine SupportGetStr(Variable,value)
  implicit none
    character(*) :: Variable,value
!
end subroutine SupportGetStr

! =========================================================================
!          STRING SUPPORT ROUTINES (replaces strsupport.f90)
!          -------------------------------------------------
!
!    All routines start with Str_
! =========================================================================

integer function Str_Count(buffer,char)

!  how many characters 'char' are there in the string 'buffer'

  implicit none
  character(*) :: buffer
  character(1) :: char

  integer :: num,i

  num=0
  do i=1,len_trim(buffer)
    if (buffer(i:i) == char) then
      num=num+1
    end if
  end do
  Str_Count=num
end function Str_Count

  subroutine Str_ChangeCase(string,case,length)
  
!  change the case of the string 'string' in the range 1:length

    implicit none
    character(*) :: string
    integer :: case
    integer,optional :: length
    
    character(1),parameter :: la='a',lz='z',ua='A',uz='Z'
    integer :: i,ila,ilz,iua,iuz,it,pt
  
      if (present(length)) then
        pt=length
      else
        pt=len_trim(string)
      end if
      ila=iachar(la)
      ilz=iachar(lz)
      iua=iachar(ua)
      iuz=iachar(uz)
      if (case == UpperCase) then
        do i=1,pt
          it=iachar(string(i:i))
          if (it >= ila .and. it <= ilz) then
            string(i:i)=achar(it-ila+iua)
          end if
        end do
      else if (case == LowerCase) then
        do i=1,pt
          it=iachar(string(i:i))
          if (it >= iua .and. it <= iuz) then
            string(i:i)=achar(it-iua+ila)
          end if
        end do
      end if
  end subroutine Str_ChangeCase

SUBROUTINE Str_UpperCase(buffer)
  character(*) :: buffer
  call str_ChangeCase(buffer,case=UpperCase)
END SUBROUTINE Str_UpperCase

character(keywordlength) function Str_GetWord(line,case)
    implicit none
    integer,optional :: case
    character(*) :: line
    
    integer :: pt
    character(1000) :: temp
    
    line=adjustl(line)
    pt=scan(line,' ')
    temp=' '
    if (pt > 1) then
      temp=line(1:pt-1)
    else
      temp=' '
    end if
    if (present(case)) then
      call Str_ChangeCase(temp,case)
    end if
    Str_GetWord=temp
end function Str_GetWord


SUBROUTINE Str_nxtat1(buffer,ibuff,atom,n,atomlgth)
      CHARACTER(*) :: buffer,atom
      INTEGER :: ibuff,i,ii,iii,n,atomlgth
      DO i=1,n
        atom(i:i)=' '
      END DO
      DO i=ibuff,n
         ii=i
         IF(buffer(i:i) /= ' ') GO TO 1010
      END DO
      atom(1:1)=' '
      atomlgth=0
      ibuff=n
      RETURN
! 
 1010 DO i=ii,n
         iii=i
         atom(i-ii+1:i-ii+1)=buffer(i:i)
         IF(buffer(i:i) == ' ') GO TO 1030
      END DO
      atom(1:1)=' '
      atomlgth=0
 1030 atomlgth=iii-ii
      ibuff=iii+1
      RETURN
END SUBROUTINE Str_nxtat1

SUBROUTINE Str_nxtat(buffer,ibuff,atom,n)
      CHARACTER(*) :: buffer,atom
      INTEGER :: ibuff,i,ii,iii,n
      DO i=1,n
        atom=' '
      END DO
      DO i=ibuff,n
        ii=i
        IF(buffer(i:i) /= ' ') GO TO 1010
      END DO
      atom(1:1)=' '
      ibuff=n
      RETURN
! 
 1010 DO i=1,n
        atom=' '
      END DO
      DO i=ii,n
        iii=i
        IF(buffer(i:i) == ' ') GO TO 1030
        atom(i-ii+1:i-ii+1)=buffer(i:i)
      END DO
      atom(1:1)=' '
 1030 ibuff=iii+1
      IF (iii-ii < 20) THEN
        DO i=iii-ii+1,20
          atom(i:i)=' '
        END DO
        CALL Str_UpperCase(atom)
      END IF
      RETURN
END SUBROUTINE Str_nxtat


subroutine str_nxtat2(buffer,atom)
  implicit none
    character(*) :: buffer,atom
    
    integer :: position
    
    buffer=adjustl(buffer)
    position=index(buffer, ' ')
    if (position >=1 ) then
      atom=' '
      atom=buffer(1:position-1)
      buffer(1:position-1)=' '
    else
      atom=' '
    end if
    buffer=adjustl(buffer)
end subroutine str_nxtat2

!                
! =====================================================================
!  Match the passed atom with appropiate command and RETURN error IF
!  command is unknown
! =====================================================================
! 
SUBROUTINE Str_mtchcomm(atom,command,lgthcomm,SELECT)
      INTEGER :: lgthcomm(*),SELECT,i,lgth,ii
      CHARACTER(*) :: command(*),atom
      SELECT=0
      i=1
      ii=0
      lgth=len_trim(atom)
 1000 IF (atom(1:lgth) == command(i)(1:lgth)) THEN
! 
!  IF the length of the atom also exactly matches the command
!  THEN declare a perfect match and forget other potential partial
!  matches
! 
        SELECT=i
        IF (lgth == lgthcomm(i)) THEN
          ii=1
          GO TO 9999
        ELSE
! 
!  otherwise it may be just one of a number of partial matches TO
!  the First part of the command
! 
          ii=ii+1
        END IF
      ELSE
         IF (lgthcomm(i) <= 0) GO TO 9999
      END IF
      i=i+1
      GO TO 1000
! 
!   IF the command wasn't matched
! 
 9999 CONTINUE
      IF (ii > 1) THEN
         SELECT=0
!         CALL error(4,atom)
      END IF 
      CONTINUE
      RETURN         
END SUBROUTINE Str_mtchcomm


!                
! =====================================================================
!  Match the passed atom with appropiate command and RETURN error IF
!  command is unknown
! =====================================================================
! 

subroutine Str_MatchKeyword(keyword,keywordlist,SELECT)
  character(*) :: keyword,keywordlist(:)
  integer :: select
  call Str_mtchcomm1(keyword,keywordlist,SELECT)
end subroutine Str_MatchKeyword


SUBROUTINE Str_mtchcomm1(atom,command,SELECT)
  implicit none
!
    INTEGER :: SELECT
    CHARACTER(*) :: command(:),atom
!
    INTEGER :: i,lgth,PartialSelect,ii
!
      SELECT=0
      ii=0
      do i=lbound(command,1),ubound(command,1)
        IF (len_trim(command(i)) == 0) exit
! if the atom is shorter than the command allow a partial match
        if (len_trim(atom) <= len_trim(command(i))) then
          lgth=min(len_trim(atom),len_trim(command(i)))
        else
          lgth=len_trim(command(i))
        end if
! 
!  IF the length of the atom also exactly matches the command
!  THEN declare a perfect match and forget other potential partial
!  matches
! 
        IF (trim(atom) == trim(command(i))) THEN
          SELECT=i
          exit
! 
!  otherwise it may be just one of a number of partial matches TO
!  the First part of the command
! 
        ELSE if (atom(1:lgth) == command(i)(1:lgth)) then
            ii=ii+1
            PartialSelect=i
        END IF
      end do
!
! if no full matches but a single partial match then declare OK
!
      if (Select == 0 .and. ii == 1) then
        Select=PartialSelect
      end if
      RETURN         
END SUBROUTINE Str_mtchcomm1

! 
! -----------------------------------------------------------
!   this routine appends an INTEGER TO a CHARACTER string
!   with leading zeroes IF the field width is greater than
!   the length of the INTEGER
! -----------------------------------------------------------
! 
SUBROUTINE Str_AppendIntToStr(Outstr,Instr,no,field)
      IMPLICIT NONE
! 
      INTEGER :: no,field,lstr
      CHARACTER(*) :: Outstr,Instr
! 
      INTEGER :: jno,lno,i
      CHARACTER(5) :: fmt
! 
      lstr=len_trim(instr)
      Outstr=' '
      Outstr(1:lstr)=trim(Instr)
! 
! Compute decimal digits in iTime
! 
      jno = abs(no)
      lno = 1
      DO i=1,98         ! 98 guarantees OK with -ve numbers
        jno = jno / 10
        IF(jno  ==  0) exit
        lno = lno + 1
      END DO
! 
!  pad from left with zeroes
! 
      if ( no >= 0 ) then
        if (lno <= 9 ) then
          WRITE (OutStr(lstr+1:lstr+field-lno),6000) ('0',i=1,field-lno)
 6000     FORMAT(1000a1)
          fmt = '(i )'
          WRITE(fmt(3:3), 7010 ) lno
 7010     FORMAT(i1)
        else
          WRITE (OutStr(lstr+1:lstr+field-lno-1),6000) ('0',i=1,field-lno-1)
          fmt = '(i  )'
          WRITE(fmt(3:4), 7011 ) lno
 7011     FORMAT(i2)
        end if
      else               ! negative
        if (lno <= 8 ) then
          WRITE (OutStr(lstr+1:lstr+field-lno),6000) ('0',i=1,field-lno-1)
          fmt = '(i )'
          WRITE(fmt(3:3), 7010 ) lno+1
        else
          WRITE (OutStr(lstr+1:lstr+field-lno-1),6000) ('0',i=1,field-lno-2)
          fmt = '(i  )'
          WRITE(fmt(3:4), 7011 ) lno+1
        end if
      end if
      WRITE(Outstr(lstr+field-lno+1:lstr+field), fmt) no 
      RETURN
END SUBROUTINE Str_AppendIntToStr
! 
! -----------------------------------------------------------
!   this routine appends an INTEGER TO a CHARACTER string
!   without leading zeroes IF the field width is greater than
!   the length of the INTEGER (compare with AppendIntToStr)
! -----------------------------------------------------------
! 
SUBROUTINE Str_AppendIntToStrCompact(Outstr,Instr,no)
      IMPLICIT NONE
! 
      INTEGER :: no
      CHARACTER(*) :: Outstr,Instr
! 
      INTEGER :: jno,lno,i
      CHARACTER(5) :: fmt
! 
      Outstr=' '
      Outstr(1:len_trim(instr))=trim(Instr)
! 
! Compute decimal digits in 'no'
! 
      jno = abs(no)
      lno = 1
      DO i=1,98             ! 98 guarantees handling -ve numbers OK
        jno = jno / 10
        IF(jno  ==  0) exit
        lno = lno + 1
      ENDDO
! 
!  WRITE the number
!
      if (no >= 0) then
        if(lno <= 9) then 
          fmt = '(i )'
          WRITE(fmt(3:3), 7010 ) lno
 7010     FORMAT(i1)
        else
          fmt = '(i  )'
          WRITE(fmt(3:4), 7011 ) lno
 7011     FORMAT(i2)
        end if
      else     ! i.e. negative
        if(lno <= 8) then 
          fmt = '(i )'
          WRITE(fmt(3:3), 7010 ) lno+1
        else
          fmt = '(i  )'
          WRITE(fmt(3:4), 7011 ) lno+1
        end if
      end if
      WRITE(Outstr(len_trim(instr)+1:),fmt) no 
      RETURN
END SUBROUTINE Str_AppendIntToStrCompact

! 
! -----------------------------------------------------------
!   this routine appends an single precision real to a CHARACTER string
!   with leading zeroes if the field width is greater than
!   the length of the real
! -----------------------------------------------------------
! 
SUBROUTINE Str_AppendRealToStr(Outstr,Instr,no,field,decpt)
      IMPLICIT NONE
! 
      INTEGER :: field,decpt
      CHARACTER(*) :: Outstr,Instr
      REAL :: no
      REAL(kind(0.0d0)) :: no1

      no1=no
      CALL Str_AppendR8ToStr(Outstr,Instr,no1,field,decpt)
      return
end subroutine Str_AppendRealToStr
! 
! -----------------------------------------------------------
!   this routine appends an double precision real to a CHARACTER string
!   with leading zeroes if the field width is greater than
!   the length of the real
! -----------------------------------------------------------
! 
SUBROUTINE Str_AppendR8ToStr(Outstr,Instr,no,field,decpt)
      IMPLICIT NONE
! 
      INTEGER :: field,decpt
      CHARACTER(*) :: Outstr,Instr
      REAL(kind(0.0d0)) :: no
! 
      INTEGER :: lno,lstr
      REAL(kind(0.0d0)) :: jno
      CHARACTER(10) :: fmt
      character(100) :: cwork
! 
      lstr=len_trim(instr)
      Outstr=' '
      Outstr(1:lstr)=trim(Instr)
! 
! Compute decimal digits in iTime
! 
      if (field <= 0) then
        jno = abs(no)
        if (jno /= 0.0d0) then
          lno=log(jno)+1
        else
          lno=1
        end if
        if (no < 0.0) lno=lno+1
        field=lno+20         ! 20 is a suitably big number to ensure enough after dec pt
      end if
      cwork=' '
      fmt='(f'
      cwork=' '
      write(cwork,*) field
      fmt=trim(fmt)//adjustl(cwork)
      fmt=trim(fmt)//'.'
      if (decpt <= 0) then
        write(cwork,*) '20'
      else
        write(cwork,*) decpt
      end if
      fmt=trim(fmt)//adjustl(cwork)
      WRITE(Outstr(lstr+1:), fmt) no 
      RETURN
END SUBROUTINE Str_AppendR8ToStr

!                
! =====================================================================
!  Match the passed atom with appropiate command and DO not 
!  RETURN error IF acommand is unknown (let it pass)
! =====================================================================
! 
SUBROUTINE Str_MtchCommNoError(atom,command,lgthcomm,SELECT)
      INTEGER :: lgthcomm(*),SELECT,i,lgth,ii
      CHARACTER(*) :: command(*),atom
      SELECT=0
      i=1
      ii=0
      lgth=len_trim(atom)
      IF (lgth == 0) THEN
! 
!  IF there is nothing on the command line
! 
        SELECT=0
      ELSE
 1000   IF (atom(1:lgth) == command(i)(1:lgth)) THEN
           SELECT=i
           ii=ii+1
        ELSE
!       
!   Have we hit the END of the list of commands ?
!       
           IF (lgthcomm(i) <= 0) GO TO 9999
        END IF
        i=i+1    
        GO TO 1000
! 
!  IF the command wasn't matched
! 
 9999   IF (ii == 0) SELECT=-1
        IF (ii > 1) THEN
          SELECT=1
!          CALL error(4,atom)
        END IF
      END IF
      RETURN
END SUBROUTINE Str_MtchCommNoError              


! 
! ======================================================================
!   Turn a mode ON or OFF
! ======================================================================
! 
SUBROUTINE ModeOnOff(Mode,Buffer,IBuff,ModeText,default)
  IMPLICIT NONE
! 
    INTEGER :: IBuff
    LOGICAL :: Mode,default
    CHARACTER(*) :: ModeText,Buffer
! 
    CHARACTER(20) :: commands(3)
    CHARACTER(80) :: atom
    INTEGER :: select
    SAVE commands
    DATA   commands(1)  / 'ON'/                    &
&          commands(2)  / 'OFF'/                   &
&          commands(3)  / 'DEFAULT'/
! 
 8002 CALL Str_nxtat(buffer,ibuff,atom,80)
      IF (atom(1:1) == ' ') RETURN
      CALL Str_mtchcomm1(atom,commands,select)
      select case (select)
      case default
        CALL Message_Output(Message_WarnContinue,                    &
&           'Invalid option in siberia.input for '//trim(ModeText)   &
&           //'##'//trim(Buffer)//'##')
        RETURN
! 
      case(1)
        Mode=.true.
        CALL Message_Output(Message_Info,'-- Mode '//trim(ModeText)  &
&                  //' set ON')
! 
      case(2)
        Mode=.false.
        CALL Message_Output(Message_Info,'-- Mode '                  &
&                 //trim(ModeText)//' set OFF')
! 
      case(3)
        IF (DEFAULT) THEN
          Mode=.true.
          CALL Message_Output(Message_Info,'-- Mode '//trim(ModeText)    &
&           //' set to default value=ON')
        ELSE
          Mode=.false.
          CALL Message_Output(Message_Info,'-- Mode '//trim(ModeText)    &
&           //' set TO DEFAULT VALUE=OFF')
        END IF
      end select
! 
      RETURN
END SUBROUTINE ModeOnOff

! =========================================================================
!          FILE SUPPORT ROUTINES
!          ---------------------
! 
!    All routines start with File_
! =========================================================================

integer function File_FreeUnitNo()
  integer :: i
  logical :: stat
    do i=99,1,-1
      inquire(unit=i,opened=stat)
      if (.not.stat) then
        File_FreeUnitNo=i
        return
      end if
    end do
end function File_FreeUnitNo

! 
SUBROUTINE File_SearchUntil(unitno,string1,string2,string3)
  IMPLICIT NONE
! 
  INTEGER :: unitno
  CHARACTER(*) :: string1
  CHARACTER(*),optional :: string2,string3
  CHARACTER(255) line,buffer,explanation
! 
  INTEGER :: i,lgth,ibuff,SELECT
  CHARACTER(20) :: commands(4)
  CHARACTER(80) :: atom,stringTemp
  INTEGER :: lgthcomm(4)
!  Initialisation
    DO i=1,4
      commands(i)=' '
    END DO
    lgth=len(string1)
    StringTemp=string1
    CALL Str_UpperCase(stringTemp(1:lgth))
    commands(1)=stringTemp(1:lgth)
    lgthcomm(1)=lgth
    lgthcomm(2)=-1
    explanation=string1
    IF (PRESENT(string2)) THEN
      lgth=len(string2)
      StringTemp=string2
      CALL Str_UpperCase(stringTemp(1:lgth))
      commands(2)=stringTemp
      lgthcomm(2)=lgth
      lgthcomm(3)=-1
      explanation=trim(explanation)//','//string2
    END IF
    IF (PRESENT(string3)) THEN
      lgth=len(string3)
      StringTemp=string3
      CALL Str_UpperCase(stringTemp(1:lgth))
      commands(3)=stringTemp
      lgthcomm(3)=lgth
      lgthcomm(4)=-1
      explanation=trim(explanation)//','//string3
    END IF
!   line processing
 7000 CONTINUE
    line=' '
    READ(unitno,6000,END=9999,ERR=9998) Line
 6000 FORMAT(a)
    ibuff=1
    buffer=line
    CALL Str_UpperCase(buffer)
    CALL Str_nxtat(buffer,ibuff,atom,80)
    CALL Str_mtchcomm(atom,commands,lgthcomm,SELECT)
    IF (SELECT == 0) GO TO 7000
    RETURN
!  error reading the file
 9998 CALL Message_Output(Message_ErrorStop,                              &
&              'Error reading file while searching for END strings = '    &
&               //trim(explanation))
!  END of file while searching the string
 9999 CALL Message_Output(Message_ErrorStop,                              &
&              'END of file while searching for END strings = '           &
&               //trim(explanation))
END SUBROUTINE File_SearchUntil

!                
! =====================================================================
!  check the first line of a file for an identifying string and
!  file/program version number
! =====================================================================
! 
subroutine File_CheckHeaderTxt(UnitNo,filetype,Status,FileVersion)
  implicit none

  integer :: UnitNo,Status
  character(*) :: FileType
  real,optional :: FileVersion

  integer :: ibuffer
  character(FileLineLgth) :: line,atom

    read(UnitNo,6000) line
 6000 format(a)
    ibuffer=1
    call Str_NxtAt(line,ibuffer,atom,FileLineLgth)
    call Str_UpperCase(atom)
    if (trim(atom) == trim(FileType)) then
      Status=0
      if (present(FileVersion)) then
        call Str_NxtAt(line,ibuffer,atom,FileLineLgth)
        read(atom,*,err=9000,end=9000) FileVersion
      end if
    else
      Status=-1
    end if
    return
 9000 status=-2
    return
end subroutine File_CheckHeaderTxt

! =========================================================================
!  check if a file exists if a flag is set true (if a file exists and a flag set)
! =========================================================================

logical function File_Exist(ExistFlag,filename)
  implicit none
  logical :: ExistFlag
  character(*) :: filename

  logical :: test

  if (ExistFlag) then
    inquire(file=trim(filename),exist=test)
  else
    test=.false.
  end if
  File_Exist=test
end function File_Exist


! =========================================================================
!          DIAGNOSTIC MESSAGE SUPPORT ROUTINES
!          -----------------------------------
!
!     All routines start with Message_
! =========================================================================

SUBROUTINE Message_Init(EchoFileStatusIn,EchoFileNameIn,ErrorFileStatusIn,ErrorFileNameIn)
  IMPLICIT NONE
  integer :: EchoFileStatusIn,ErrorFileStatusIn
  character(*) :: EchoFileNameIn,ErrorFileNameIn
!
  if (.not.FirstInit) then
    return
!    write (*,*) ' ---- Internal error -- cannot execute subroutine MESSAGE_INIT more than once'
!    stop
  else
    FirstInit=.false.
  end if
  call Message_EchoSet(EchoFileStatusIn,EchoFileNameIn)
  call Message_ErrorSet(ErrorFileStatusIn,ErrorFileNameIn)
end subroutine Message_Init

SUBROUTINE Message_Close(error)
  implicit none
  logical,optional :: error

      if (present(error)) error=ErrorFileOpen
      IF (EchoFileOutput) THEN
        if (ErrorFileOpen) then
          write (EchoFileUnit,*) '-- ERRORS occurred during the SIBERIA run'
          write (EchoFileUnit,*) '   Check '//trim(ErrorFileName)//' for more information'
          write (EchoFileUnit,*)
        end if
        CLOSE(UNIT=EchoFileUnit,STATUS='keep')
        EchoFileOutput=.false.
      END IF
      IF (ErrorFileOpen) THEN
        CLOSE(unit=ErrorFileUnit,status='keep')
      END IF
END SUBROUTINE Message_Close

subroutine Message_EchoSet(EchoFileStatusIn,EchoFileNameIn)
  IMPLICIT NONE
  integer :: EchoFileStatusIn
  character(*) :: EchoFileNameIn
!
  logical :: stat
  character(8) :: date
  character(10) :: time
!
    select case (EchoFileStatusIn)
      case (EchoFileStart)
        if (.not.EchoFileActive) then
          EchoFileUnit=File_FreeUnitNo()
          if(trim(EchoFileNameIn) /= '') then
            EchoFileName=trim(EchoFileNameIn)
          end if
          open(unit=EchoFileUnit,file=trim(EchoFileName),status='replace')
          EchoFileActive=.true.
          EchoFileOutput=.true.
        end if
        call date_and_time(date,time)
        write (EchoFileUnit,*) ' Echo File : '                         &
&              ,'Time = ',time(1:2),':',time(3:4),':',time(5:10)          &
&              ,' Date = ',date(7:8),'-',date(5:6),'-',date(1:4)
        write (EchoFileUnit,*) ' --------------------------------------------------'
        write (EchoFileUnit,*)
      case (EchoFileClose)
        if (EchoFileActive) then
          EchoFileActive=.false.
          EchoFileOutput=.false.
          inquire(file=trim(EchoFileName),opened=stat)
          if (stat) then
            close(unit=EchoFileUnit,status='keep')
          end if
        end if
      case (EchoFileSuspend)
        EchoFileOutput=.false.
      case (EchoFileResume)
        EchoFileOutput=.true.
      end select
      return
END SUBROUTINE Message_EchoSet

subroutine Message_ErrorSet(ErrorFileStatusIn,ErrorFileNameIn)
  IMPLICIT NONE
  integer :: ErrorFileStatusIn
  character(*) :: ErrorFileNameIn
!
  logical :: stat
!
    select case (ErrorFileStatusIn)
      case (ErrorFileStart)
        if(trim(ErrorFileNameIn) /= '') then
          ErrorFileName=trim(ErrorFileNameIn)
          inquire(file=trim(ErrorFileName),exist=stat)
          if (stat) then
            ErrorFileUnit=File_FreeUnitNo()
            open(unit=ErrorFileUnit,file=trim(ErrorFileName))
            close(unit=ErrorFileUnit,status='delete')
          end if
        end if
        ErrorFileActive=.true.
        ErrorFileOutput=.true.
      case (ErrorFileClose)
        if (ErrorFileActive) then
          ErrorFileActive=.false.
          inquire(file=trim(ErrorFileName),opened=stat)
          if (stat) close(unit=ErrorFileUnit,status='keep')
          ErrorFileOpen=.false.
          ErrorFileOutput=.false.
        end if
      case (ErrorFileSuspend)
        ErrorFileOutput=.false.
      case (ErrorFileResume)
        ErrorFileOutput=.true.
      end select
      return
END SUBROUTINE Message_ErrorSet

SUBROUTINE Message_OpenErrorLogFile()
!  defer opening the error log file until a logable error message occurs
!  otherwise we might open a log file that has nothing in it suggesting to the user
!  that an error occured when in fact nothing did occur.
  implicit none
  character(8) :: date
  character(10) :: time
!
  if (ErrorFileOpen) return
  if (ErrorFileActive) then
     ErrorFileUnit=File_FreeUnitNo()
     open(unit=ErrorFileUnit,file=trim(ErrorFileName),status='replace')
     call date_and_time(date,time)
     write (ErrorFileUnit,*) ' Error Log File : '                         &
&              ,'Time = ',time(1:2),':',time(3:4),':',time(5:10)          &
&              ,' Date = ',date(7:8),'-',date(5:6),'-',date(1:4)
     write (ErrorFileUnit,*) ' --------------------------------------------------'
     write (ErrorFileUnit,*)
     ErrorFileOpen=.true.
  end if
end SUBROUTINE Message_OpenErrorLogFile


subroutine MessageOutputI(severity,str1,i1,str2,i2,str3,i3,str4,i4          &
&                   ,str5,i5,str6,i6,str7,i7,str8,i8,str9,i9,str10,i10)
  implicit none
!
    INTEGER :: severity,i1
    character(*) :: str1
    integer, optional :: i2,i3,i4,i5,i6,i7,i8,i9,i10
    character(*),optional :: str2,str3,str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(20) :: temp,IntegerFormatStr
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    if (MessageField <= 0) then
      write (temp,*) i1
    else
      IntegerFormatStr=' '
      Integerformatstr(1:2)='(i'
      write(Integerformatstr(3:),*) MessageField
      IntegerFormatStr(3:)=trim(adjustl(IntegerFormatStr(3:)))//')'
      write (temp,IntegerFormatStr) i1
    end if
    message=message(1:index)//trim(adjustl(temp))
    if (present(str2)) then 
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
    if (present(i2)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i2
      else
        write (temp,IntegerFormatStr) i2
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str3)) then 
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
    if (present(i3)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i3
      else
        write (temp,IntegerFormatStr) i3
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(i4)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i4
      else
        write (temp,IntegerFormatStr) i4
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(i5)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i5
      else
        write (temp,IntegerFormatStr) i5
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(i6)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i6
      else
        write (temp,IntegerFormatStr) i6
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(i7)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i7
      else
        write (temp,IntegerFormatStr) i7
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(i8)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) i8
      else
        write (temp,IntegerFormatStr) i8
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(i9)) then
        temp=' '
        if (MessageField <= 0) then
          write (temp,*) i9
        else
          write (temp,IntegerFormatStr) i9
        end if
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(i10)) then
            temp=' '
            if (MessageField <= 0) then
              write (temp,*) i10
            else
              write (temp,IntegerFormatStr) i10
            end if
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputI


subroutine MessageOutputR(severity,str1,R1,str2,R2,str3,R3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    real :: R1
    character(*) :: str1
    real, optional :: R2,R3,R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str2,str3,str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index,lgth,itemp
    character(20) :: temp,FormatStr
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    if (MessageDecPt > 0 .or. MessageField > 0) then
      FormatStr=' '
      formatstr(1:2)='(f'
      if (MessageField <= 0) then
!  make it some nominal but large size
        itemp=20
      else
        itemp=MessageField
      end if
      write(formatstr(3:),*) itemp
      FormatStr(3:)=trim(adjustl(FormatStr(3:)))//'.'
      lgth=len_trim(FormatStr)+1
      write(formatstr(lgth:),*) MessageDecPt
      FormatStr(lgth:)=trim(adjustl(FormatStr(lgth:)))//')'
      write (temp,FormatStr) r1
    else
      write (temp,*) r1
    end if
    message=message(1:index)//trim(adjustl(temp))
    if (present(str2)) then 
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
    if (present(R2)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R2
      else
        write (temp,FormatStr) r2
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str3)) then 
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
    if (present(R3)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R3
      else
        write (temp,FormatStr) r3
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R4
      else
        write (temp,FormatStr) r4
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R5
      else
        write (temp,FormatStr) r5
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R6
      else
        write (temp,FormatStr) r6
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R7
      else
        write (temp,FormatStr) r7
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      if (MessageField <= 0) then
        write (temp,*) R8
      else
        write (temp,FormatStr) r8
      end if
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        if (MessageField <= 0) then
          write (temp,*) R9
        else
          write (temp,FormatStr) r9
        end if
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            if (MessageField <= 0) then
              write (temp,*) R10
            else
              write (temp,FormatStr) r10
            end if
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputR


subroutine MessageOutputR8(severity,str1,R1,str2,R2,str3,R3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    REAL(KIND(0.0D0)) :: R1
    character(*) :: str1
    REAL(KIND(0.0D0)), optional :: R2,R3,R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str2,str3,str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(50) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) R1
    message=message(1:index)//trim(adjustl(temp))
    if (present(str2)) then 
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
    if (present(R2)) then
      temp=' '
      write (temp,*) R2
      message=message(1:index)//trim(adjustl(temp))
    if (present(str3)) then 
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
    if (present(R3)) then
      temp=' '
      write (temp,*) R3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputR8


subroutine MessageOutputIR(severity,str1,i1,str2,R2,str3,R3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    integer :: i1
    real :: r2
    character(*) :: str1,str2
    real, optional :: R3,R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str3,str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(20) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) i1
    message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
      temp=' '
      write (temp,*) R2
      message=message(1:index)//trim(adjustl(temp))
    if (present(str3)) then 
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
    if (present(R3)) then
      temp=' '
      write (temp,*) R3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputIR


subroutine MessageOutputIR8(severity,str1,i1,str2,R2,str3,R3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    integer :: i1
    REAL(KIND(0.0D0)) :: r2
    character(*) :: str1,str2
    REAL(KIND(0.0D0)), optional :: R3,R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str3,str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(50) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) i1
    message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
      temp=' '
      write (temp,*) R2
      message=message(1:index)//trim(adjustl(temp))
    if (present(str3)) then 
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
    if (present(R3)) then
      temp=' '
      write (temp,*) R3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputIR8

subroutine MessageOutputIIR(severity,str1,i1,str2,i2,str3,R3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    integer :: i1,i2
    real :: r3
    character(*) :: str1,str2,str3
    real, optional :: R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(20) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) i1
    message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
      temp=' '
      write (temp,*) i2
      message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
      temp=' '
      write (temp,*) R3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputIIR

subroutine MessageOutputIIR8(severity,str1,i1,str2,i2,str3,R3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    integer :: i1,i2
    REAL(KIND(0.0D0)) :: r3
    character(*) :: str1,str2,str3
    REAL(KIND(0.0D0)), optional :: R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(50) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) i1
    message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
      temp=' '
      write (temp,*) i2
      message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
      temp=' '
      write (temp,*) R3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputIIR8



subroutine MessageOutputRIIR(severity,str1,r1,str2,i2,str3,i3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    integer :: i2,i3
    real :: r1
    character(*) :: str1,str2,str3
    real, optional :: R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(20) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) r1
    message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
      temp=' '
      write (temp,*) i2
      message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
      temp=' '
      write (temp,*) i3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputRIIR

subroutine MessageOutputR8IIR8(severity,str1,r1,str2,i2,str3,i3,str4,R4           &
&                ,str5,R5,str6,R6,str7,R7,str8,R8,str9,R9,str10,R10)
  implicit none
    INTEGER :: severity
    integer :: i2,i3
    REAL(KIND(0.0D0)) :: r1
    character(*) :: str1,str2,str3
    REAL(KIND(0.0D0)), optional :: R4,R5,R6,R7,R8,R9,R10
    character(*),optional :: str4,str5,str6,str7,str8,str9,str10
!
    integer,parameter :: StrSize=1000
    integer :: index
    character(50) :: temp
    character(StrSize) :: message
!
    message=' '
    index=len(str1)
    message=str1
    temp=' '
    write (temp,*) r1
    message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str2)
      if (index > StrSize) go to 9000
      message=trim(message)//str2
      temp=' '
      write (temp,*) i2
      message=message(1:index)//trim(adjustl(temp))
      index=len_trim(message)+len(str3)
      if (index > StrSize) go to 9000
      message=trim(message)//str3
      temp=' '
      write (temp,*) i3
      message=message(1:index)//trim(adjustl(temp))
    if (present(str4)) then 
      index=len_trim(message)+len(str4)
      if (index > StrSize) go to 9000
      message=trim(message)//str4
    if (present(R4)) then
      temp=' '
      write (temp,*) R4
      message=message(1:index)//trim(adjustl(temp))
    if (present(str5)) then 
      index=len_trim(message)+len(str5)
      if (index > StrSize) go to 9000
      message=trim(message)//str5
    if (present(R5)) then
      temp=' '
      write (temp,*) R5
      message=message(1:index)//trim(adjustl(temp))
    if (present(str6)) then 
      index=len_trim(message)+len(str6)
      if (index > StrSize) go to 9000
      message=trim(message)//str6
    if (present(R6)) then
      temp=' '
      write (temp,*) R6
      message=message(1:index)//trim(adjustl(temp))
    if (present(str7)) then 
      index=len_trim(message)+len(str7)
      if (index > StrSize) go to 9000
      message=trim(message)//str7
    if (present(R7)) then
      temp=' '
      write (temp,*) R7
      message=message(1:index)//trim(adjustl(temp))
    if (present(str8)) then 
      index=len_trim(message)+len(str8)
      if (index > StrSize) go to 9000
      message=trim(message)//str8
    if (present(R8)) then
      temp=' '
      write (temp,*) R8
      message=message(1:index)//trim(adjustl(temp))
    if (present(str9)) then 
      index=len_trim(message)+len(str9)
      if (index > StrSize) go to 9000
      message=trim(message)//str9
      if (present(R9)) then
        temp=' '
        write (temp,*) R9
        message=message(1:index)//trim(adjustl(temp))
        if (present(str10)) then 
          index=len_trim(message)+len(str10)
          if (index > StrSize) go to 9000
          message=trim(message)//str10
          if (present(R10)) then
            temp=' '
            write (temp,*) R10
            message=message(1:index)//trim(adjustl(temp))
          end if
        end if
      end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
    end if
 9000 call MessageOutput(Severity,trim(message))
end subroutine MessageOutputR8IIR8


SUBROUTINE MessageOutput(severity,str1)
  IMPLICIT NONE
! 
    INTEGER :: severity
    CHARACTER(*) :: str1
! 
      select case (severity)
      case default
        RETURN
!
      case (Message_Info) 
        if (Prompt) WRITE (*,6000) trim(str1)
        IF (EchoFileOutput) WRITE (EchoFileUnit,6000) trim(str1)
 6000   format(' ',a)
! 
      case(Message_ErrorStop)
        if (Prompt) WRITE (*,6000) ' --- ERROR --- '//trim(str1)
        IF (EchoFileOutput) WRITE (EchoFileUnit,6000) ' --- ERROR --- '//trim(str1)
        if (ErrorFileActive.and.ErrorFileOutput) then
          call Message_OpenErrorLogFile()
          WRITE (ErrorFileUnit,6000) ' --- ERROR --- '//trim(str1)
        end if
        CALL done_support
!
      case(Message_ErrorContinue) 
        if (Prompt) WRITE (*,6000) ' --- ERROR --- '//trim(str1)
        IF (EchoFileOutput) WRITE (EchoFileUnit,6000) ' --- ERROR --- '//trim(str1)
        if (ErrorFileActive.and.ErrorFileOutput) then
          call Message_OpenErrorLogFile()
          WRITE (ErrorFileUnit,6000) ' --- ERROR --- '//trim(str1)
        end if
! 
      case(Message_WarnStop)
        if (Prompt) WRITE (*,6000) ' --- WARNING --- '//trim(str1)
        IF (EchoFileOutput) WRITE (EchoFileUnit,6000) ' --- WARNING --- '//trim(str1)
        if (ErrorFileActive.and.ErrorFileOutput) then
          call Message_OpenErrorLogFile()
          WRITE (ErrorFileUnit,6000) ' --- WARNING --- '//trim(str1)
        end if
        CALL done_support
! 
      case(Message_WarnContinue)
        if (Prompt) WRITE (*,6000) ' --- WARNING --- '//trim(str1)
        IF (EchoFileOutput) WRITE (EchoFileUnit,6000) ' --- WARNING --- '//trim(str1)
        if (ErrorFileActive.and.ErrorFileOutput) then
          call Message_OpenErrorLogFile()
          WRITE (ErrorFileUnit,6000) ' --- WARNING --- '//trim(str1)
        end if
      end select
END SUBROUTINE MessageOutput


subroutine ErrorMessage(severity,message,routine,errorno)
  implicit none
  
  integer :: severity
  integer,optional :: errorno
  character(*) :: message,routine
  
  character(10) :: work
  
  if (present(errorno)) then
    work=' '
    write(work,6000) errorno
6000 format(i9)
    call MessageOutput(severity,'Routine='//routine//': Error Number ='//work//': '//message)
  else
    call MessageOutput(severity,'Routine='//routine//': '//message)
  end if
end subroutine ErrorMessage

! 
! ---------------------------------------------------------------------
!  standard message handler for siberia for multiple line messages
! ---------------------------------------------------------------------
! 

SUBROUTINE Message_OutputN(severity,string,NoLines)
  IMPLICIT NONE
! 
    INTEGER :: severity,NoLines
    CHARACTER(*) :: string(NoLines)
!
    integer :: i
! 
      select case (severity)
      case default
        RETURN
!
      case(Message_Info)
        if (prompt) then 
          DO i=1,NoLines
            WRITE (*,6000) trim(string(i))
          END DO
        end if
        IF (EchoFileOutput) THEN
          DO i=1,NoLines
            WRITE (EchoFileUnit,6000) trim(string(i))
          END DO
        END IF
 6000   format(' ',a)
! 
      case(Message_ErrorStop)
        if (prompt) then 
          DO i=1,NoLines
            WRITE (*,6000) ' --- ERROR --- '//trim(string(i))
          END DO
        end if
        IF (EchoFileOutput) THEN
          DO i=1,NoLines
            WRITE (EchoFileUnit,6000) ' --- ERROR --- '//trim(string(i))
          END DO
        END IF
        IF (ErrorFileActive.and.ErrorFileOutput) THEN
          call Message_OpenErrorLogFile()
          DO i=1,NoLines
            WRITE (ErrorFileUnit,6000) ' --- ERROR --- '//trim(string(i))
          END DO
        END IF
        call done_support
!
      case(Message_ErrorContinue) 
        if (prompt) then 
          DO i=1,NoLines
            WRITE (*,6000) ' --- ERROR --- '//trim(string(i))
          END DO
        end if
        IF (EchoFileOutput) THEN
          DO i=1,NoLines
            WRITE (EchoFileUnit,6000) ' --- ERROR --- '//trim(string(i))
          END DO
        END IF
        IF (ErrorFileActive.and.ErrorFileOutput) THEN
          call Message_OpenErrorLogFile()
          DO i=1,NoLines
            WRITE (ErrorFileUnit,6000) ' --- ERROR --- '//trim(string(i))
          END DO
        END IF
! 
      case(Message_WarnStop)
        if (prompt) then 
          DO i=1,NoLines
            WRITE (*,6000) ' --- WARNING --- '//trim(string(i))
          END DO
        end if
        IF (EchoFileOutput) THEN
          DO i=1,NoLines
            WRITE (EchoFileUnit,6000) ' --- WARNING --- '//trim(string(i))
          END DO
        END IF
        IF (ErrorFileActive.and.ErrorFileOutput) THEN
          call Message_OpenErrorLogFile()
          DO i=1,NoLines
            WRITE (ErrorFileUnit,6000) ' --- WARNING --- '//trim(string(i))
          END DO
        END IF
        call done_support
! 
      case(Message_WarnContinue)
        if (prompt) then 
          DO i=1,NoLines
            WRITE (*,6000) ' --- WARNING --- '//trim(string(i))
          END DO
        end if
        IF (EchoFileOutput) THEN
          DO i=1,NoLines
            WRITE (EchoFileUnit,6000) ' --- WARNING --- '//trim(string(i))
          END DO
        END IF
        IF (ErrorFileActive.and.ErrorFileOutput) THEN
          call Message_OpenErrorLogFile()
          DO i=1,NoLines
            WRITE (ErrorFileUnit,6000) ' --- WARNING --- '//trim(string(i))
          END DO
        END IF
      end select
      RETURN
END SUBROUTINE Message_OutputN

subroutine MessageSetI(Variable,value)
  implicit none
    integer :: Variable,value
!
    character(20) :: temp
!
    select case (variable)
      case default
        temp=' '
        write(temp,*) variable
        call Message_Output   &
&           (Message_ErrorStop,'Unknown Variable in call to MESSAGESETI = '//trim(temp))
!  generic number field width
      case(Message_FieldWidth)
        MessageField=value
        MessageIntegerField=value
        MessageRealField=value
!  real field width
      case(Message_RealFieldWidth)
        MessageRealField=value
!  integer field width
      case(Message_IntegerFieldWidth)
        MessageIntegerField=value
!  numbers after Decimal pt
      case(Message_DecPt)
        MessageDecPt=value
    end select
end subroutine MessageSetI

subroutine MessageGetI(Variable,value)
  implicit none
    integer :: Variable,value
!
    character(20) :: temp
!
    select case (variable)
      case default
        temp=' '
        write(temp,*) variable
        call Message_Output   &
&           (Message_ErrorStop,'Unknown Variable in call to MESSAGEGETI = '//trim(temp))
!  number field width
      case(Message_FieldWidth)
        value=MessageField
! error file variables
      case(Message_DecPt)
        value=MessageDecPt
    end select
end subroutine MessageGetI

! =========================================================================
!          TIMER ROUTINES
!          --------------
! 
! =========================================================================

!  initialise the timer

        SUBROUTINE TimerInit()
        IMPLICIT NONE
        INTEGER :: CountRate,CountMax,NoThreads,ThreadNo,i,TimeArray1(8)
        REAL :: Time
!   F90 version
!  -------------
! 
        NoThreads=1
!$      NoThreads=omp_get_num_threads()
        if (NoThreads > MaxNoThreads) then
          write (*,*) ' --- ERROR --- Maximum number of parallel threads exceeded in TIMER > ',MaxNoThreads
          stop
        end if
        ThreadNo=1
!$      ThreadNo=omp_get_thread_num()
        CALL date_and_time(values=TimeArray1)
        CALL system_clock(CountStart(ThreadNo),CountRate,CountMax)
        do i=1,8
          timearray(i,ThreadNo)=timearray1(i)
        end do
!    F95 version
!   -------------
        CALL cpu_time(time)
        TimeStart=time
        RETURN
        END SUBROUTINE TimerInit


! 
!  whether the timer DATA is TO be output TO a file and IF
!  so what is the file unit number
! 
        SUBROUTINE TimerSetFileOutput(File,FileUnit)
        IMPLICIT NONE
        INTEGER :: FileUnit
        LOGICAL :: File
! this routine is not thread safe
        IF (File) THEN
          TimerEcho=.true.
          TimerFileUnit=FileUnit
        ELSE
          TimerEcho=.false.
        END IF
        RETURN
        END SUBROUTINE TimerSetFileOutput


! 
!  RETURN the CPU time (short time version)
! 
        FUNCTION TimerGet()
        IMPLICIT NONE
        REAL :: TimerGet
! 
        INTEGER :: Count,CountRate,CountMax,ThreadNo
        REAL :: result,time
!
        CALL system_clock(count,CountRate,CountMax)
        ThreadNo=1
!$      ThreadNo=omp_get_thread_num()
        result=float(Count-CountStart(ThreadNo))/float(CountRate)
!   F90 timer
!   --------
!   simple allowance for once round the counter (most
!   architectures seem TO allow up TO about an hour with
!   this timer ... for longer periods USE LibGetTimerLong)
! 
!
!        IF (result >= 0.0) THEN
!          TimerGet=result
!        ELSE
!          TimerGet=float(Count-CountStart(ThreadNo)+CountMax)/float(CountRate)
!        END IF
!   F95 timer
!   ---------
        CALL cpu_time(time)
        TimerGet=time-TimeStart
        RETURN
        END FUNCTION TimerGet


! 
!  RETURN the CPU time (long time version)
! 
        FUNCTION TimerGetLong()
        IMPLICIT NONE
        REAL :: TimerGetLong
! 
        INTEGER :: TimeArray1(8)
        INTEGER :: Count,CountRate,CountMax,ThreadNo
        REAL :: Result1,Time
! 
!    F90 version
!   -------------
! 
! this routine is not thread safe
        CALL system_clock(count,CountRate,CountMax)
        CALL date_and_time(values=TimeArray1)
        ThreadNo=1
!$      ThreadNo=omp_get_thread_num()
!    doesn't DO times spanning a month boundary
! 
!    with absoft 6.1 on the mac timearray(8) is always zero and
!    result2 is rubbish so can't DO sub-second timings
        IF (TimeArray1(2) /= TimeArray(2,ThreadNo)) THEN
          result1=-1.
        ELSE
          result1=(TimeArray1(3)-TimeArray(3,ThreadNo))*86400.                    &
     &      +(TimeArray1(5)-TimeArray(5,ThreadNo))*3600.                          &
     &      +(TimeArray1(6)-TimeArray(6,ThreadNo))*60.                            &
     &      +float(TimeArray1(7)-TimeArray(7,ThreadNo))                           &
     &      +(TimeArray1(8)-TimeArray(8,ThreadNo))/1000.
        END IF
        TimerGetLong=result1
! 
!    F95 version
!   -------------
! 
        CALL cpu_time(time)
        TimerGetLong=time-timestart
        RETURN
        END FUNCTION TimerGetLong


! 
!   print CPU (short time version)
! 
        SUBROUTINE TimerShow(Message)
        IMPLICIT NONE
        CHARACTER(*) :: Message
        REAL :: result
        result=TimerGet()
        WRITE (*,*) trim(message),' : CPU = ',result
        IF (TimerEcho) THEN
          WRITE (TimerFileUnit,*) trim(message),' : CPU = ',result
        END IF
        RETURN
        END SUBROUTINE TimerShow


! 
!   print CPU (long time version)
! 
        SUBROUTINE TimerShowLong(Message)
        IMPLICIT NONE
        CHARACTER(*) :: Message
        REAL :: result
        result=TimerGetLong()
        WRITE (*,*) trim(message),' : CPU = ',result
        IF (TimerEcho) THEN
          WRITE (TimerFileUnit,*) trim(message),' : CPU = ',result
        END IF
        RETURN
        END SUBROUTINE TimerShowLong

! =========================================================================
!          Random Number Generators
!          ------------------------
! =========================================================================

  REAL FUNCTION Ran2(idum)
! 
!  random number from Numerical recipes Ran2
! 
    PARAMETER (m=714025, ia=1366, ic=150889, rm=1./m)
    DIMENSION ir(97)
    SAVE 
    DATA iff /0/
    IF (idum < 0.or.iff == 0) THEN
      iff=1
      idum=mod(ic-idum,m)
      DO j=1,97
       idum=mod(ia*idum+ic,m)
       ir(j)=idum
      END DO
      idum=mod(ia*idum+ic,m)
      iy=idum
    END IF
    j=1+(97*iy)/m
    IF (j > 97.or.j < 1) THEN
      WRITE(*,*) 'error in SUBROUTINE Ran2',j
      STOP
    END IF
    iy=ir(j)
    Ran2=iy*rm
    idum=mod(ia*idum+ic,m)
    ir(j)=idum
    RETURN
  END FUNCTION Ran2
!  ==================================================
!   SUBROUTINE gives the probability of not being exceeded
!   using a N(0,1)
!  ==================================================
  REAL(KIND(0.0D0)) FUNCTION PDFNorm(z)
    REAL(KIND(0.0D0)) :: z
! 
! Array fn stores P(Z<zs) for unit normal z where zs ranges from 0 TO 3
! 
    REAL(KIND(0.0D0)) :: fn(300),p
    INTEGER :: i
    DATA (fn(i),i=1,50)/               &
&      0.50531906, 0.50797832, 0.51329553, 0.51595342, 0.52126664,               &
&      0.52392220, 0.52922928, 0.53188139, 0.53718024, 0.53982788,               &
&      0.54511636, 0.54775846, 0.55303454, 0.55567002, 0.56093168,               &
&      0.56355947, 0.56880462, 0.57142377, 0.57665026, 0.57925975,               &
&      0.58446562, 0.58706445, 0.59224778, 0.59483492, 0.59999365,               &
&      0.60256815, 0.60770035, 0.61026126, 0.61536509, 0.61791140,               &
&      0.62298495, 0.62551582, 0.63055730, 0.63307178, 0.63807929,               &
&      0.64057642, 0.64554828, 0.64802730, 0.65296173, 0.65542173,               &
&      0.66031712, 0.66275728, 0.66761196, 0.67003143, 0.67484385,               &
&      0.67724186, 0.68201047, 0.68438625, 0.68910950, 0.69146246/
    DATA (fn(i),i=51,100)/               &
&      0.69613892, 0.69846821, 0.70309651, 0.70540142, 0.70998025,               &
&      0.71226025, 0.71678835, 0.71904272, 0.72351879, 0.72574687,               &
&      0.73016977, 0.73237109, 0.73673970, 0.73891366, 0.74322695,               &
&      0.74537307, 0.74962997, 0.75174773, 0.75594735, 0.75803626,               &
&      0.76217777, 0.76423746, 0.76831990, 0.77034992, 0.77437264,               &
&      0.77637267, 0.78033483, 0.78230447, 0.78620559, 0.78814453,               &
&      0.79198384, 0.79389191, 0.79766899, 0.79954576, 0.80326009,               &
&      0.80510545, 0.80875659, 0.81057030, 0.81415796, 0.81593984,               &
&      0.81946367, 0.82121360, 0.82467335, 0.82639116, 0.82978666,               &
&      0.83147234, 0.83480346, 0.83645689, 0.83972353, 0.84134471/
    DATA (fn(i),i=101,150)/               &
&      0.84454679, 0.84613580, 0.84927332, 0.85083002, 0.85390317,               &
&      0.85542768, 0.85843652, 0.85992885, 0.86287355, 0.86433393,               &
&      0.86721468, 0.86864305, 0.87146020, 0.87285686, 0.87561053,               &
&      0.87697554, 0.87966627, 0.88099986, 0.88362795, 0.88493025,               &
&      0.88749617, 0.88876754, 0.89127171, 0.89251232, 0.89495522,               &
&      0.89616531, 0.89854759, 0.89972740, 0.90204954, 0.90319943,               &
&      0.90546221, 0.90658242, 0.90878630, 0.90987730, 0.91202301,               &
&      0.91308498, 0.91517317, 0.91620666, 0.91823810, 0.91924334,               &
&      0.92121875, 0.92219609, 0.92411643, 0.92506629, 0.92693210,               &
&      0.92785490, 0.92966717, 0.93056333, 0.93232280, 0.93319279/
    DATA (fn(i),i=151,200)/               &
&      0.93490034, 0.93574446, 0.93740100, 0.93821979, 0.93982613,               &
&      0.94062001, 0.94217712, 0.94294649, 0.94445527, 0.94520068,               &
&      0.94666195, 0.94738382, 0.94879860, 0.94949740, 0.95086658,               &
&      0.95154274, 0.95286727, 0.95352131, 0.95480210, 0.95543450,               &
&      0.95667255, 0.95728374, 0.95848000, 0.95907044, 0.96022588,               &
&      0.96079606, 0.96191156, 0.96246201, 0.96353853, 0.96406966,               &
&      0.96510822, 0.96562046, 0.96662194, 0.96711588, 0.96808118,               &
&      0.96855724, 0.96948731, 0.96994591, 0.97084177, 0.97128338,               &
&      0.97214592, 0.97257102, 0.97340107, 0.97381014, 0.97460860,               &
&      0.97500211, 0.97576988, 0.97614819, 0.97688627, 0.97724986/
    DATA (fn(i),i=201,250)/               &
&      0.97795904, 0.97830832, 0.97898942, 0.97932482, 0.97997868,               &
&      0.98030072, 0.98092824, 0.98123717, 0.98183918, 0.98213553,               &
&      0.98271281, 0.98299694, 0.98355025, 0.98382258, 0.98435277,               &
&      0.98461360, 0.98512143, 0.98537123, 0.98585737, 0.98609650,               &
&      0.98656178, 0.98679060, 0.98723567, 0.98745453, 0.98788011,               &
&      0.98808938, 0.98849612, 0.98869616, 0.98908484, 0.98927587,               &
&      0.98964709, 0.98982954, 0.99018395, 0.99035811, 0.99069637,               &
&      0.99086255, 0.99118519, 0.99134362, 0.99165130, 0.99180245,               &
&      0.99209571, 0.99223977, 0.99251914, 0.99265635, 0.99292248,               &
&      0.99305314, 0.99330652, 0.99343085, 0.99367195, 0.99379033/
    DATA (fn(i),i=251,300)/               &
&      0.99401969, 0.99413222, 0.99435031, 0.99445736, 0.99466467,               &
&      0.99476635, 0.99496335, 0.99505997, 0.99524701, 0.99533874,               &
&      0.99551642, 0.99560350, 0.99577206, 0.99585468, 0.99601460,               &
&      0.99609292, 0.99624455, 0.99631888, 0.99646258, 0.99653298,               &
&      0.99666917, 0.99673587, 0.99686486, 0.99692798, 0.99705005,               &
&      0.99710989, 0.99722546, 0.99728203, 0.99739134, 0.99744481,               &
&      0.99754822, 0.99759883, 0.99769646, 0.99774432, 0.99783659,               &
&      0.99788171, 0.99796891, 0.99801159, 0.99809384, 0.99813414,               &
&      0.99821186, 0.99824983, 0.99832308, 0.99835891, 0.99842799,               &
&      0.99846178, 0.99852687, 0.99855876, 0.99860513, 0.99865007/
! 
    IF (abs(z) <= 3.0) THEN
      index=abs(z)*100
      index=max(1,index)
      p=fn(index)
      IF (z > 0) THEN
         PDFNorm=1-p
      ELSE
         PDFNorm=p
      END IF
    ELSE
      IF (z > 0) THEN
        PDFNorm=0
      ELSE
        PDFNorm=1
      END IF
    END IF
    RETURN
  END FUNCTION PDFNorm
! 
!      
!  ==================================================
!   SUBROUTINE gives the normalised SD deviate of not being exceeded
!   using a N(0,1) for proabaility p
!  ==================================================
  REAL(KIND(0.0D0)) FUNCTION PDFNormInv(p)
    REAL(KIND(0.0D0)) :: p
! 
! Array fn stores P(Z<zs) for unit normal z where zs ranges from 0 TO 3
! 
    REAL(KIND(0.0D0)) :: fn(300),z
    INTEGER :: i
    DATA (fn(i),i=1,50)/                                                           &
&      0.50531906, 0.50797832, 0.51329553, 0.51595342, 0.52126664,               &
&      0.52392220, 0.52922928, 0.53188139, 0.53718024, 0.53982788,               &
&      0.54511636, 0.54775846, 0.55303454, 0.55567002, 0.56093168,               &
&      0.56355947, 0.56880462, 0.57142377, 0.57665026, 0.57925975,               &
&      0.58446562, 0.58706445, 0.59224778, 0.59483492, 0.59999365,               &
&      0.60256815, 0.60770035, 0.61026126, 0.61536509, 0.61791140,               &
&      0.62298495, 0.62551582, 0.63055730, 0.63307178, 0.63807929,               &
&      0.64057642, 0.64554828, 0.64802730, 0.65296173, 0.65542173,               &
&      0.66031712, 0.66275728, 0.66761196, 0.67003143, 0.67484385,               &
&      0.67724186, 0.68201047, 0.68438625, 0.68910950, 0.69146246/
    DATA (fn(i),i=51,100)/                                                         &
&      0.69613892, 0.69846821, 0.70309651, 0.70540142, 0.70998025,               &
&      0.71226025, 0.71678835, 0.71904272, 0.72351879, 0.72574687,               &
&      0.73016977, 0.73237109, 0.73673970, 0.73891366, 0.74322695,               &
&      0.74537307, 0.74962997, 0.75174773, 0.75594735, 0.75803626,               &
&      0.76217777, 0.76423746, 0.76831990, 0.77034992, 0.77437264,               &
&      0.77637267, 0.78033483, 0.78230447, 0.78620559, 0.78814453,               &
&      0.79198384, 0.79389191, 0.79766899, 0.79954576, 0.80326009,               &
&      0.80510545, 0.80875659, 0.81057030, 0.81415796, 0.81593984,               &
&      0.81946367, 0.82121360, 0.82467335, 0.82639116, 0.82978666,               &
&      0.83147234, 0.83480346, 0.83645689, 0.83972353, 0.84134471/
    DATA (fn(i),i=101,150)/                                                        &
&      0.84454679, 0.84613580, 0.84927332, 0.85083002, 0.85390317,               &
&      0.85542768, 0.85843652, 0.85992885, 0.86287355, 0.86433393,               &
&      0.86721468, 0.86864305, 0.87146020, 0.87285686, 0.87561053,               &
&      0.87697554, 0.87966627, 0.88099986, 0.88362795, 0.88493025,               &
&      0.88749617, 0.88876754, 0.89127171, 0.89251232, 0.89495522,               &
&      0.89616531, 0.89854759, 0.89972740, 0.90204954, 0.90319943,               &
&      0.90546221, 0.90658242, 0.90878630, 0.90987730, 0.91202301,               &
&      0.91308498, 0.91517317, 0.91620666, 0.91823810, 0.91924334,               &
&      0.92121875, 0.92219609, 0.92411643, 0.92506629, 0.92693210,               &
&      0.92785490, 0.92966717, 0.93056333, 0.93232280, 0.93319279/
    DATA (fn(i),i=151,200)/                                                        &
&      0.93490034, 0.93574446, 0.93740100, 0.93821979, 0.93982613,               &
&      0.94062001, 0.94217712, 0.94294649, 0.94445527, 0.94520068,               &
&      0.94666195, 0.94738382, 0.94879860, 0.94949740, 0.95086658,               &
&      0.95154274, 0.95286727, 0.95352131, 0.95480210, 0.95543450,               &
&      0.95667255, 0.95728374, 0.95848000, 0.95907044, 0.96022588,               &
&      0.96079606, 0.96191156, 0.96246201, 0.96353853, 0.96406966,               &
&      0.96510822, 0.96562046, 0.96662194, 0.96711588, 0.96808118,               &
&      0.96855724, 0.96948731, 0.96994591, 0.97084177, 0.97128338,               &
&      0.97214592, 0.97257102, 0.97340107, 0.97381014, 0.97460860,               &
&      0.97500211, 0.97576988, 0.97614819, 0.97688627, 0.97724986/
    DATA (fn(i),i=201,250)/                                                        &
&      0.97795904, 0.97830832, 0.97898942, 0.97932482, 0.97997868,               &
&      0.98030072, 0.98092824, 0.98123717, 0.98183918, 0.98213553,               &
&      0.98271281, 0.98299694, 0.98355025, 0.98382258, 0.98435277,               &
&      0.98461360, 0.98512143, 0.98537123, 0.98585737, 0.98609650,               &
&      0.98656178, 0.98679060, 0.98723567, 0.98745453, 0.98788011,               &
&      0.98808938, 0.98849612, 0.98869616, 0.98908484, 0.98927587,               &
&      0.98964709, 0.98982954, 0.99018395, 0.99035811, 0.99069637,               &
&      0.99086255, 0.99118519, 0.99134362, 0.99165130, 0.99180245,               &
&      0.99209571, 0.99223977, 0.99251914, 0.99265635, 0.99292248,               &
&      0.99305314, 0.99330652, 0.99343085, 0.99367195, 0.99379033/
    DATA (fn(i),i=251,300)/                                                        &
&      0.99401969, 0.99413222, 0.99435031, 0.99445736, 0.99466467,               &
&      0.99476635, 0.99496335, 0.99505997, 0.99524701, 0.99533874,               &
&      0.99551642, 0.99560350, 0.99577206, 0.99585468, 0.99601460,               &
&      0.99609292, 0.99624455, 0.99631888, 0.99646258, 0.99653298,               &
&      0.99666917, 0.99673587, 0.99686486, 0.99692798, 0.99705005,               &
&      0.99710989, 0.99722546, 0.99728203, 0.99739134, 0.99744481,               &
&      0.99754822, 0.99759883, 0.99769646, 0.99774432, 0.99783659,               &
&      0.99788171, 0.99796891, 0.99801159, 0.99809384, 0.99813414,               &
&      0.99821186, 0.99824983, 0.99832308, 0.99835891, 0.99842799,               &
&      0.99846178, 0.99852687, 0.99855876, 0.99860513, 0.99865007/
! 
    IF (p < 0.5) THEN
      pp=1.0-p
    ELSE
      pp=p
    END IF
    DO i=1,300
      Index=i
      IF (pp > fn(i)) GO TO 1000
    END DO
    Index=300
1000 CONTINUE
    IF (p > 0.5) THEN
      z=Index/100.0
    ELSE
      z=-Index/100.0
    END IF
    PDFNormInv=z
    RETURN
  END FUNCTION PDFNormInv

! =========================================================================
!          ASSORTED OTHER SUPPORT ROUTINES
!          -------------------------------
! 
! =========================================================================

!  =======================================================================================
!      Handling Allocation Error
!  =======================================================================================

SUBROUTINE Memory_AllocationError(why,routine,errorNumber)
!  this routine is needed TO avoid illegal branches
!  out of parallel regions in openMP after memory errors
  IMPLICIT NONE
  CHARACTER(*) :: why,routine
  integer, optional :: ErrorNumber
! 
    IF (present(ErrorNumber)) then
      CALL Message_Output(Message_ErrorStop,                                 &
&            'Out of Memory: Allocation of '//trim(why)//' in '              &
&             //trim(routine)//': Error Number =',ErrorNumber) 
    else
      CALL Message_Output(Message_ErrorStop,                                 &
&            'Out of Memory: Allocation of '//trim(why)//' in '              &
&             //trim(routine)) 
    end if
END SUBROUTINE Memory_AllocationError

SUBROUTINE Memory_DeallocationError(why,routine,errorNumber)
!  this routine is needed TO avoid illegal branches
!  out of parallel regions in openMP after memory errors
  IMPLICIT NONE
  CHARACTER(*) :: why,routine
  integer, optional :: ErrorNumber
! 
    IF (present(ErrorNumber)) then
      CALL Message_Output(Message_ErrorStop,                                 &
&            'Deallocation of '//trim(why)//' in '//trim(routine)//          &
&            ': Error Number=',ErrorNumber)
    else
      CALL Message_Output(Message_ErrorStop,                                 &
&            'Deallocation of '//trim(why)//' in '//trim(routine))
    end if 
END SUBROUTINE Memory_DeallocationError

SUBROUTINE AllocationError(why,routine)
!  this routine is needed TO avoid illegal branches
!  out of parallel regions in openMP in SIBERIA after memory errors
  IMPLICIT NONE
  CHARACTER(*) :: why,routine
! 
    CALL Message_Output(Message_ErrorStop,                                 &
&            'Out of Memory: Allocation of '//trim(why)//' in '            &
&             //trim(routine)) 
END SUBROUTINE AllocationError
! 
! ---------------------------------------------------------------------
!   just a little prompt TO reassure the watcher the code is working
! ---------------------------------------------------------------------
! 
SUBROUTINE Working(iterations)
  IMPLICIT NONE
! 
  INTEGER,OPTIONAL,INTENT(IN) :: iterations
! 
  REAL :: result
! 
    result=TimerGetLong()
    IF (present(iterations)) then
      CALL Message_Output(Message_Info,' .... working ... CPU=',result     &
&           ,': Time Steps = ',float(iterations))
    else
      CALL Message_Output(Message_Info,' .... working ... CPU=',result)
    end if
    RETURN
END SUBROUTINE Working

subroutine done_support
  implicit none
  logical :: log
  character(1) :: junk

      CALL Message_Close(error=log)
      IF (PauseAtEnd) THEN
        WRITE (*,*) 
        if (log) then
          write (*,*) '-- ERRORS occurred during the SIBERIA run'
          write (*,*) 'Check '//trim(ErrorFileName)//' for more information'
          write (*,*)
        end if
        WRITE (*,*) 'CALCULATIONS DONE ... RETURN to exit'
        READ(*,1000) junk
 1000   FORMAT(a)
      END IF
      STOP
end subroutine done_support

!
END MODULE Support
