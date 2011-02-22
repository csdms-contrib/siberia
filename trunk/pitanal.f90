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
! ===============================================================
!
module pitanal

  type PitPosition
    integer :: x,y
    REAL(KIND(0.0D0)) :: z
  end type PitPosition

contains

SUBROUTINE pitanal2(z,Slope,gridX,gridY,kx,ky,direct,IrregularBoundary           &
&                  ,Domain,filename)
  USE SiberiaConstants
  USE Support
  IMPLICIT NONE
!           
!   Parameters
!
    integer, parameter :: Array_Increment = 100
    INTEGER :: gridX,GridY,direct(gridX,GridY),kx,ky
    REAL(KIND(0.0D0)) :: z(gridX,GridY),Slope(gridX,GridY)
    LOGICAL :: IrregularBoundary,Domain(gridX,GridY)
    character(*) :: filename
!                 
    INTEGER :: i,j,k,kk,CurrentPit,inew,jnew,iold,jold,No_Pits                   &
&       ,ErrorNo,No_Unresolved,Current_Pit,Array_Size
    integer,dimension(:,:),allocatable :: Pit_Region
    REAL(KIND(0.0D0)) :: Min_Slope,Max_Z,ZNew
    logical :: PitIdentified
    type(PitPosition),allocatable, dimension(:) :: Pit,Pass,Unresolved,Temp
!
!  finding all the pits
!
      allocate(Pit(Array_Increment),stat=ErrorNo)
      IF (ErrorNo /= 0)                                                          &
&           call AllocationError('Setting up initial storage #1','pitanal2')
      allocate(Unresolved(Array_Increment),stat=ErrorNo)
      IF (ErrorNo /= 0)                                                          &
&           call AllocationError('Setting up initial storage #2','pitanal2')
      CurrentPit=0
      if (irregularboundary) then
        do j=1,ky
          do i=1,kx
            if (domain(i,j)) then
              if (direct(i,j) == 5) then
                PitIdentified=.true.
                do k=1,8
                  inew=i+nodei(k)
                  jnew=j+nodej(k)
                  if (inew < 1 .or. jnew < 1 .or. inew > kx .or. jnew > ky) then
                    PitIdentified=.false.
                    exit
                  end if
                  if (.not. domain(inew,jnew)) then
                    PitIdentified=.false.
                    exit
                  end if
                end do
                if (PitIdentified) then
                  CurrentPit=CurrentPit+1
                  Array_Size=Size(Pit)
                  if (CurrentPit > Array_Size) then
                    allocate(temp(Array_Size),stat=ErrorNo)
                    IF (ErrorNo /= 0)                                                &
&                      call AllocationError('Expanding pit storage #1','pitanal2')
                    temp=pit
                    deallocate(pit)
                    Array_Size=Array_Size+Array_Increment
                    allocate(pit(Array_Size),stat=ErrorNo)
                    IF (ErrorNo /= 0)                                                &
&                      call AllocationError('Expanding pit storage #2','pitanal2')
                    pit(1:size(temp))=temp
                    deallocate(temp)
                  end if
                  Pit(CurrentPit)%X=i
                  Pit(CurrentPit)%Y=j
                end if
              end if
            end if
          end do
        end do
      else
! regular BC
        do j=1,ky
          do i=1,kx
            if (direct(i,j) == 5) then
              PitIdentified=.true.
              do k=1,8
                inew=i+nodei(k)
                jnew=j+nodej(k)
                if (inew < 1 .or. jnew < 1 .or. inew > kx .or. jnew > ky) then
                  PitIdentified=.false.
                  exit
                end if
                if (.not. domain(inew,jnew)) then
                  PitIdentified=.false.
                  exit
                end if
              end do
              if (PitIdentified) then
                CurrentPit=CurrentPit+1
                Array_Size=Size(Pit)
                if (CurrentPit > Array_Size) then
                  allocate(temp(Array_Size),stat=ErrorNo)
                  IF (ErrorNo /= 0)                                                   &
&                    call AllocationError('Expanding pit storage #1','pitanal2')
                  temp=pit
                  deallocate(pit)
                  Array_Size=Array_Size+Array_Increment
                  allocate(pit(Array_Size),stat=ErrorNo)
                  IF (ErrorNo /= 0)                                                   &
&                    call AllocationError('Expanding pit storage #2','pitanal2')
                  pit(1:size(temp))=temp
                  deallocate(temp)
                end if
                Pit(CurrentPit)%X=i
                Pit(CurrentPit)%Y=j
              end if
            end if
          end do
        end do
      end if
      No_Pits=CurrentPit
!   if there is only one pit then the whole domain flows into and no pitfilling required
      if (No_Pits <= 1) return
!
!   determine the pit regions
!
      allocate(Pit_Region(GridX,GridY),stat=ErrorNo)
      IF (ErrorNo /= 0)                                                               &
&            CALL AllocationError('Setting up Storage for Pit_Region','pitanal2')
      do j=1,ky
        do i=1,kx
          Pit_Region(i,j)=0
        end do
      end do
      do k=1,No_Pits
        Pit_Region(Pit(k)%X,Pit(k)%Y)=-k
      end do
!      write (*,*) ' -- Identifying Pit Regions'
      if (irregularboundary) then
        do j=1,ky
          do i=1,kx
            if (domain(i,j)) then
              if (Pit_Region(i,j) == 0) then
                iold=i
                jold=j
                inew=i
                jnew=j
                do
                  if (dir1(direct(iold,jold)) == 0 .and. dir2(direct(iold,jold)) == 0) then
                    Pit_Region(i,j)=abs(Pit_Region(inew,jnew))
                    exit  
                  else
                    inew=iold+dir1(direct(iold,jold))
                    jnew=jold+dir2(direct(iold,jold))
                  end if
                  iold=inew
                  jold=jnew
                end do
              end if
            end if
          end do
        end do
      else
!  regular BC
        do j=1,ky
          do i=1,kx
            if (Pit_Region(i,j) == 0) then
              iold=i
              jold=j
              inew=i
              jnew=j
              do
                if (dir1(direct(iold,jold)) == 0 .and. dir2(direct(iold,jold)) == 0) then
                  Pit_Region(i,j)=abs(Pit_Region(inew,jnew))
                  exit  
                else
                  inew=iold+dir1(direct(iold,jold))
                  jnew=jold+dir2(direct(iold,jold))
                end if
                iold=inew
                jold=jnew
              end do
            end if
          end do
        end do
      end if
!
!  find the minimum slope to resolve the flats
!
      Min_Slope=1.e6
      Max_Z=-1e6
      if (IrregularBoundary) then
        do j=1,ky
          do i=1,kx
            if (domain(i,j)) then
              if (slope(i,j) > 0.0) then
                Min_Slope=min(Min_Slope,Slope(i,j))
                Max_Z=max(Max_Z,z(i,j))
              end if
            end if
          end do
        end do
      else
!  regular BC
        do j=1,ky
          do i=1,kx
            if (slope(i,j) > 0.0) then
              Min_Slope=min(Min_Slope,Slope(i,j))
              Max_Z=max(Max_Z,z(i,j))
            end if
          end do
        end do
      end if
!      write (*,*) ' -- Minimum slope     =',min_slope
!      write (*,*) ' -- Maximum Elevation =',Max_Z
!
!   find the passes
!
!      write (*,*) ' -- Pass Identification'
      allocate(Pass(0:No_Pits),stat=ErrorNo)
      IF (ErrorNo /= 0) call AllocationError('Setting up storage for pass','pitanal2')
      do k=1,No_Pits
        Pass(k)%Z=Max_Z
        Pass(k)%X=-1
        Pass(k)%Y=-1
      end do
      if (IrregularBoundary) then
        do j=1,ky
          do i=1,kx
            if (domain(i,j) .and.(.not. Pit_Region(i,j) == 0)) then
              do k=1,8
                inew=i+nodei(k)
                jnew=j+nodej(k)
                if (inew < 1 .or. inew > kx .or. jnew < 1 .or. jnew > ky) exit
                if (.not.domain(inew,jnew)) exit
                kk=abs(Pit_Region(i,j))
                if (kk /= abs(Pit_Region(inew,jNew))) then      ! pt in another region
                  if (inew /= i .and. jnew /= j) then            ! pt diagonal
                    znew=max(z(i,j),z(inew,jnew)+1.414*min_slope)
                  else
                    znew=max(z(i,j),z(inew,jnew)+min_slope)
                  end if 
! is this hgt the lowest pass out of catchment
                  if (znew < pass(kk)%Z) then
                    Pass(kk)%X=i
                    Pass(kk)%Y=j
                    Pass(kk)%Z=znew
                  end if
                end if
              end do
            end if
          end do
        end do
      else
!  regular BC
        do j=1,ky
          do i=1,kx
            if (.not. Pit_Region(i,j) == 0) then
              do k=1,8
                inew=i+nodei(k)
                jnew=j+nodej(k)
                if (inew < 1 .or. inew > kx .or. jnew < 1 .or. jnew > ky) exit
                kk=abs(Pit_Region(i,j))
                if (kk /= abs(Pit_Region(inew,jNew))) then      ! pt in another region
                  if (inew /= i .and. jnew /= j) then            ! pt diagonal
                    znew=max(z(i,j),z(inew,jnew)+1.414*min_slope)
                  else
                    znew=max(z(i,j),z(inew,jnew)+min_slope)
                  end if 
! is this hgt the lowest pass out of catchment
                  if (znew < pass(kk)%Z) then
                    Pass(kk)%X=i
                    Pass(kk)%Y=j
                    Pass(kk)%Z=znew
                  end if
                end if
              end do
            end if
          end do
        end do
      end if
!
!  flat pit filling
!
!      write (*,*) ' -- Pit Resolution'
      if (IrregularBoundary) then
        do j=1,ky
          do i=1,kx
            if (domain(i,j) .and. Pit_Region(i,j) /= 0) then
              z(i,j)=max(z(i,j),Pass(abs(Pit_Region(i,j)))%Z)
            end if
          end do
        end do
      else
!  regular BC
        do j=1,ky
          do i=1,kx
            if (Pit_Region(i,j) /= 0) then
              z(i,j)=max(z(i,j),Pass(abs(Pit_Region(i,j)))%Z)
            end if
          end do
        end do
      end if
!
!  resolving the flat regions
!
      if (IrregularBoundary) then
        do k=1,No_Pits
          Current_Pit=abs(Pit_Region(Pass(k)%X,Pass(k)%Y))
          No_Unresolved=1
          UnResolved(1)%X=Pass(k)%X
          UnResolved(1)%Y=Pass(k)%Y
          do
            if (No_Unresolved == 0) exit
            iOld=UnResolved(No_Unresolved)%X
            jOld=UnResolved(No_Unresolved)%Y
            No_Unresolved=No_Unresolved-1
            z(iold,jold)=-abs(z(iold,jold))
            do kk=1,8
              iNew=iOld+nodei(kk)
              jNew=jOld+nodej(kk)
              if (inew >= 1 .and. jnew >= 1 .and. inew <= kx .and. jnew <= ky) then
                if (domain(inew,jnew)) then
                  if (Current_Pit == abs(Pit_Region(inew,jnew))) then
                    if (z(inew,jnew) > 0.0) then
                      if (abs(Z(iOld,jOld))+min_slope > abs(Z(inew,jnew))) then
                        Z(inew,jnew)=-(abs(Z(iOld,jOld))+min_slope)
                        No_Unresolved=No_Unresolved+1
                        Array_Size=Size(UnResolved)
                        if (No_Unresolved > Array_Size) then
                          allocate(temp(Array_Size),stat=ErrorNo)
                          IF (ErrorNo /= 0) call AllocationError                        &
&                               ('Expanding UnResolved storage #1','pitanal2')
                          temp=UnResolved
                          deallocate(UnResolved)
                          Array_Size=Array_Size+Array_Increment
                          allocate(UnResolved(Array_Size),stat=ErrorNo)
                          IF (ErrorNo /= 0) call AllocationError                        &
&                               ('Expanding UnResolved storage #2','pitanal2')
                          UnResolved(1:size(temp))=temp
                          deallocate(temp)
                        end if
                        UnResolved(No_Unresolved)%X=inew
                        UnResolved(No_Unresolved)%Y=jnew
                        Pit_Region(inew,jnew)=-abs(Pit_Region(inew,jnew))
                      end if
                    end if
                  end if
                end if
              end if
            end do   ! round the neighbours
          end do     ! main calc loop
        end do
      else
!  regular boundary
        do k=1,No_Pits
          Current_Pit=abs(Pit_Region(Pass(k)%X,Pass(k)%Y))
          No_Unresolved=1
          UnResolved(1)%X=Pass(k)%X
          UnResolved(1)%Y=Pass(k)%Y
          do
            if (No_Unresolved == 0) exit
            iOld=UnResolved(No_Unresolved)%X
            jOld=UnResolved(No_Unresolved)%Y
            No_Unresolved=No_Unresolved-1
            z(iold,jold)=-abs(z(iold,jold))
            do kk=1,8
              iNew=iOld+nodei(kk)
              jNew=jOld+nodej(kk)
              if (inew >= 1 .and. jnew >= 1 .and. inew <= kx .and. jnew <= ky) then
                if (Current_Pit == abs(Pit_Region(inew,jnew))) then
                  if (z(inew,jnew) > 0.0) then
                    if (abs(Z(iOld,jOld))+min_slope > abs(Z(inew,jnew))) then
                      Z(inew,jnew)=-(abs(Z(iOld,jOld))+min_slope)
                        No_Unresolved=No_Unresolved+1
                      Array_Size=Size(UnResolved)
                      if (No_Unresolved > Array_Size) then
                        allocate(temp(Array_Size),stat=ErrorNo)
                        IF (ErrorNo /= 0) call AllocationError                     &
&                             ('Expanding pass storage #1','pitanal2')
                        temp=pass
                        deallocate(pass)
                        Array_Size=Array_Size+Array_Increment
                        allocate(pass(Array_Size),stat=ErrorNo)
                        IF (ErrorNo /= 0) call AllocationError                     &
&                             ('Expanding pass storage #2','pitanal2')
                        pass(1:size(temp))=temp
                        deallocate(temp)
                      end if
                      UnResolved(No_Unresolved)%X=inew
                      UnResolved(No_Unresolved)%Y=jnew
                      Pit_Region(inew,jnew)=-abs(Pit_Region(inew,jnew))
                    end if
                  end if
                end if
              end if
            end do   ! round the neighbours
          end do     ! main calc loop
        end do
      end if
      do j=1,ky
        do i=1,kx
          z(i,j)=abs(z(i,j))
        end do
      end do
!
!    write the pit stuff out to file if requested (filename is expected to be .rst2 file name)
!
      if (filename(1:1) /= ' ') then
        call Message_Output(Message_Info,' -- Writing RSU file')
        open(unit=11,file=filename(1:len_trim(filename)-5)//'.rsu')
        write (11,*) ' SIBERIA    8.17'
        write (11,*) 
        write (11,*) 
        write (11,*) 
        write (11,*) kx,ky,' 2'
        write (11,*) 'Pit_No Pass_Hgt'
        do j=1,ky
          do i=1,kx
            write (11,*) Pit_Region(i,j),Pass(abs(Pit_Region(i,j)))%Z
          end do
        end do
        close(unit=11,status='keep')
      end if
!
      deallocate(Pit)
      deallocate(Pass)
      deallocate(UnResolved)
      deallocate(Pit_Region)
      RETURN
END SUBROUTINE pitanal2


SUBROUTINE pitanal2XY(z,Slope,direct,IrregularBoundary,Domain           &
&                  ,LowX,HighX,LowY,HighY,GridX,GridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Support
  IMPLICIT NONE
!           
!   Parameters
!
    integer, parameter :: Array_Increment = 1000
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    Type(ArrayIXY) :: direct
    Type(ArrayR8XY) :: z,Slope
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
!                 
    INTEGER :: i,j,k,kk,CurrentPit,inew,jnew,iold,jold,No_Pits                   &
&       ,ErrorNo,No_Unresolved,Current_Pit,Array_Size
    integer,dimension(:,:),allocatable :: Pit_Region
    REAL(KIND(0.0D0)) :: Min_Slope,Max_Z,ZNew
    logical :: PitIdentified
    type(PitPosition),allocatable, dimension(:) :: Pit,Pass,Unresolved,Temp
!
!  finding all the pits
!
      allocate(Pit(Array_Increment),stat=ErrorNo)
      IF (ErrorNo /= 0)                                                          &
&           call AllocationError('Setting up initial storage #1','pitanal2')
      allocate(Unresolved(Array_Increment),stat=ErrorNo)
      IF (ErrorNo /= 0)                                                          &
&           call AllocationError('Setting up initial storage #2','pitanal2')
!      write (*,*) ' -- finding pits'
      CurrentPit=0
      if (irregularboundary) then
        do j=LowY,HighY
          do i=LowX,HighX
            if (domain(i,j)) then
              if (direct%data(i,j) == 5) then
                PitIdentified=.true.
                do k=1,8
                  inew=i+nodei(k)
                  jnew=j+nodej(k)
                  if (inew < LowX .or. jnew < LowY .or. inew > HighX .or. jnew > HighY) then
                    PitIdentified=.false.
                    exit
                  end if
                  if (.not. domain(inew,jnew)) then
                    PitIdentified=.false.
                    exit
                  end if
                end do
                if (PitIdentified) then
                  CurrentPit=CurrentPit+1
                  Array_Size=Size(Pit)
                  if (CurrentPit > Array_Size) then
                    allocate(temp(Array_Size),stat=ErrorNo)
                    IF (ErrorNo /= 0)                                                &
&                      call AllocationError('Expanding pit storage #1','pitanal2')
                    temp=pit
                    deallocate(pit)
                    Array_Size=Array_Size+Array_Increment
                    allocate(pit(Array_Size),stat=ErrorNo)
                    IF (ErrorNo /= 0)                                                &
&                      call AllocationError('Expanding pit storage #2','pitanal2')
                    pit(1:size(temp))=temp
                    deallocate(temp)
                  end if
                  Pit(CurrentPit)%X=i
                  Pit(CurrentPit)%Y=j
                end if
              end if
            end if
          end do
        end do
      else
! regular BC
        do j=LowY,HighY
          do i=LowX,HighX
            if (direct%data(i,j) == 5) then
              PitIdentified=.true.
              do k=1,8
                inew=i+nodei(k)
                jnew=j+nodej(k)
                if (inew < LowX .or. jnew < LowY .or. inew > HighX .or. jnew > HighY) then
                  PitIdentified=.false.
                  exit
                end if
              end do
              if (PitIdentified) then
                CurrentPit=CurrentPit+1
                Array_Size=Size(Pit)
                if (CurrentPit > Array_Size) then
                  allocate(temp(Array_Size),stat=ErrorNo)
                  IF (ErrorNo /= 0)                                                   &
&                    call AllocationError('Expanding pit storage #1','pitanal2')
                  temp=pit
                  deallocate(pit)
                  Array_Size=Array_Size+Array_Increment
                  allocate(pit(Array_Size),stat=ErrorNo)
                  IF (ErrorNo /= 0)                                                   &
&                    call AllocationError('Expanding pit storage #2','pitanal2')
                  pit(1:size(temp))=temp
                  deallocate(temp)
                end if
                Pit(CurrentPit)%X=i
                Pit(CurrentPit)%Y=j
              end if
            end if
          end do
        end do
      end if
      No_Pits=CurrentPit
!
!   determine the pit regions
!
      allocate(Pit_Region(LowX:HighX,LowY:HighY),stat=ErrorNo)
      IF (ErrorNo /= 0)                                                               &
&            CALL AllocationError('Setting up Storage for Pit_Region','pitanal2')
      do j=LowY,HighY
        do i=LowX,HighX
          Pit_Region(i,j)=0
        end do
      end do
      do k=1,No_Pits
        Pit_Region(Pit(k)%X,Pit(k)%Y)=-k
      end do
!      write (*,*) ' -- Identifying Pit Regions'
      if (irregularboundary) then
        do j=LowY,HighY
          do i=LowX,HighX
            if (domain(i,j)) then
              if (Pit_Region(i,j) == 0) then
                iold=i
                jold=j
                inew=i
                jnew=j
                do
                  if (dir1(direct%data(iold,jold)) == 0 .and. dir2(direct%data(iold,jold)) == 0) then
                    Pit_Region(i,j)=abs(Pit_Region(inew,jnew))
                    exit  
                  else
                    inew=iold+dir1(direct%data(iold,jold))
                    jnew=jold+dir2(direct%data(iold,jold))
                  end if
                  iold=inew
                  jold=jnew
                end do
              end if
            end if
          end do
        end do
      else
!  regular BC
        do j=LowY,HighY
          do i=LowX,HighX
            if (Pit_Region(i,j) == 0) then
              iold=i
              jold=j
              inew=i
              jnew=j
              do
                if (dir1(direct%data(iold,jold)) == 0 .and. dir2(direct%data(iold,jold)) == 0) then
                  Pit_Region(i,j)=abs(Pit_Region(inew,jnew))
                  exit  
                else
                  inew=iold+dir1(direct%data(iold,jold))
                  jnew=jold+dir2(direct%data(iold,jold))
                end if
                iold=inew
                jold=jnew
              end do
            end if
          end do
        end do
      end if
!
!  find the minimum slope to resolve the flats
!
      Min_Slope=1.e6
      Max_Z=-1e6
      if (IrregularBoundary) then
        do j=LowY,HighY
          do i=LowX,HighX
            if (domain(i,j)) then
              if (slope%data(i,j) > 0.0) then
                Min_Slope=min(Min_Slope,Slope%data(i,j))
                Max_Z=max(Max_Z,z%data(i,j))
              end if
            end if
          end do
        end do
      else
!  regular BC
        do j=LowY,HighY
          do i=LowX,HighX
            if (slope%data(i,j) > 0.0) then
              Min_Slope=min(Min_Slope,Slope%data(i,j))
              Max_Z=max(Max_Z,z%data(i,j))
            end if
          end do
        end do
      end if
!      write (*,*) ' -- Minimum slope     =',min_slope
!      write (*,*) ' -- Maximum Elevation =',Max_Z
!
!   find the passes
!
!      write (*,*) ' -- Pass Identification'
      allocate(Pass(0:No_Pits),stat=ErrorNo)
      IF (ErrorNo /= 0) call AllocationError('Setting up storage for pass','pitanal2')
      do k=1,No_Pits
        Pass(k)%Z=Max_Z
        Pass(k)%X=-1
        Pass(k)%Y=-1
      end do
      if (IrregularBoundary) then
        do j=LowY,HighY
          do i=LowX,HighX
            if (domain(i,j) .and.(.not. Pit_Region(i,j) == 0)) then
              kk=abs(Pit_Region(i,j))
              do k=1,8
                inew=i+nodei(k)
                jnew=j+nodej(k)
                if (inew < LowX .or. jnew < LowY .or. inew > HighX .or. jnew > HighY) exit
                if (.not.domain(inew,jnew)) exit
                if (kk /= abs(Pit_Region(inew,jNew))) then      ! pt in another region
                  if (inew /= i .and. jnew /= j) then            ! pt diagonal
                    znew=max(z%data(i,j),z%data(inew,jnew)+1.414*min_slope)
                  else
                    znew=max(z%data(i,j),z%data(inew,jnew)+min_slope)
                  end if 
! is this hgt the lowest pass out of catchment
                  if (znew < pass(kk)%Z) then
                    Pass(kk)%X=i
                    Pass(kk)%Y=j
                    Pass(kk)%Z=znew
                  end if
                end if
              end do
            end if
          end do
        end do
      else
!  regular BC
        do j=LowY,HighY
          do i=LowX,HighX
            if (.not. Pit_Region(i,j) == 0) then
              kk=abs(Pit_Region(i,j))
              do k=1,8
                inew=i+nodei(k)
                jnew=j+nodej(k)
                if (inew < LowX .or. jnew < LowY .or. inew > HighX .or. jnew > HighY) exit
                if (kk /= abs(Pit_Region(inew,jNew))) then      ! pt in another region
                  if (inew /= i .and. jnew /= j) then            ! pt diagonal
                    znew=max(z%data(i,j),z%data(inew,jnew)+1.414*min_slope)
                  else
                    znew=max(z%data(i,j),z%data(inew,jnew)+min_slope)
                  end if 
! is this hgt the lowest pass out of catchment
                  if (znew < pass(kk)%Z) then
                    Pass(kk)%X=i
                    Pass(kk)%Y=j
                    Pass(kk)%Z=znew
                  end if
                end if
              end do
            end if
          end do
        end do
      end if
!
!  flat pit filling
!
!      write (*,*) ' -- Pit Resolution'
      if (IrregularBoundary) then
        do j=LowY,HighY
          do i=LowX,HighX
            if (domain(i,j) .and. Pit_Region(i,j) /= 0) then
              z%data(i,j)=max(z%data(i,j),Pass(abs(Pit_Region(i,j)))%Z)
            end if
          end do
        end do
      else
!  regular BC
        do j=LowY,HighY
          do i=LowX,HighX
            if (Pit_Region(i,j) /= 0) then
              z%data(i,j)=max(z%data(i,j),Pass(abs(Pit_Region(i,j)))%Z)
            end if
          end do
        end do
      end if
!
!  resolving the flat regions
!
      if (IrregularBoundary) then
        do k=1,No_Pits
          Current_Pit=abs(Pit_Region(Pass(k)%X,Pass(k)%Y))
          No_Unresolved=1
          UnResolved(1)%X=Pass(k)%X
          UnResolved(1)%Y=Pass(k)%Y
          do
            if (No_Unresolved == 0) exit
            iOld=UnResolved(No_Unresolved)%X
            jOld=UnResolved(No_Unresolved)%Y
            No_Unresolved=No_Unresolved-1
            z%data(iold,jold)=-abs(z%data(iold,jold))
            do kk=1,8
              iNew=iOld+nodei(kk)
              jNew=jOld+nodej(kk)
              if (inew >= LowX .and. jnew >= LowY .and. inew <= HighX .and. jnew <= HighY) then
                if (domain(inew,jnew)) then
                  if (Current_Pit == abs(Pit_Region(inew,jnew))) then
                    if (z%data(inew,jnew) > 0.0) then
                      if (abs(Z%data(iOld,jOld))+min_slope > abs(Z%data(inew,jnew))) then
                        Z%data(inew,jnew)=-(abs(Z%data(iOld,jOld))+min_slope)
                        No_Unresolved=No_Unresolved+1
                        Array_Size=ubound(UnResolved,1)
                        if (No_Unresolved > Array_Size) then
                          allocate(temp(Array_Size),stat=ErrorNo)
                          IF (ErrorNo /= 0) call AllocationError                        &
&                               ('Expanding UnResolved storage #1','pitanal2')
                          temp=UnResolved
                          deallocate(UnResolved)
                          Array_Size=Array_Size+Array_Increment
                          allocate(UnResolved(Array_Size),stat=ErrorNo)
                          IF (ErrorNo /= 0) call AllocationError                        &
&                               ('Expanding UnResolved storage #2','pitanal2')
                          UnResolved(1:ubound(temp,1))=temp
                          deallocate(temp)
                        end if
                        UnResolved(No_Unresolved)%X=inew
                        UnResolved(No_Unresolved)%Y=jnew
                        Pit_Region(inew,jnew)=-abs(Pit_Region(inew,jnew))
                      end if
                    end if
                  end if
                end if
              end if
            end do   ! round the neighbours
          end do     ! main calc loop
        end do
      else
!  regular boundary
        do k=1,No_Pits
          Current_Pit=abs(Pit_Region(Pass(k)%X,Pass(k)%Y))
          No_Unresolved=1
          UnResolved(1)%X=Pass(k)%X
          UnResolved(1)%Y=Pass(k)%Y
          do
            if (No_Unresolved == 0) exit
            iOld=UnResolved(No_Unresolved)%X
            jOld=UnResolved(No_Unresolved)%Y
            No_Unresolved=No_Unresolved-1
            z%data(iold,jold)=-abs(z%data(iold,jold))
            do kk=1,8
              iNew=iOld+nodei(kk)
              jNew=jOld+nodej(kk)
              if (inew >= LowX .and. jnew >= LowY .and. inew <= HighX .and. jnew <= HighY) then
                if (Current_Pit == abs(Pit_Region(inew,jnew))) then
                  if (z%data(inew,jnew) > 0.0) then
                    if (abs(Z%data(iOld,jOld))+min_slope > abs(Z%data(inew,jnew))) then
                      Z%data(inew,jnew)=-(abs(Z%data(iOld,jOld))+min_slope)
                      No_Unresolved=No_Unresolved+1
                      Array_Size=ubound(UnResolved,1)
                      if (No_Unresolved > Array_Size) then
                        allocate(temp(Array_Size),stat=ErrorNo)
                        IF (ErrorNo /= 0) call AllocationError                     &
&                             ('Expanding Unresolved storage #1','pitanal2')
                        temp=UnResolved
                        deallocate(UnResolved)
                        Array_Size=Array_Size+Array_Increment
                        allocate(UnResolved(Array_Size),stat=ErrorNo)
                        IF (ErrorNo /= 0) call AllocationError                     &
&                             ('Expanding Unresolved storage #2','pitanal2')
                        UnResolved(1:ubound(temp,1))=temp
                        deallocate(temp)
                      end if
                      UnResolved(No_Unresolved)%X=inew
                      UnResolved(No_Unresolved)%Y=jnew
                      Pit_Region(inew,jnew)=-abs(Pit_Region(inew,jnew))
                    end if
                  end if
                end if
              end if
            end do   ! round the neighbours
          end do     ! main calc loop
        end do
      end if
      do j=LowY,HighY
        do i=LowX,HighX
          z%data(i,j)=abs(z%data(i,j))
        end do
      end do
!
!        write (*,*) ' -- Writing RSU file'
!        open(unit=11,file='wi20-soil-pitfill-001000.rsu')
!        write (11,*) ' SIBERIA    8.24'
!        write (11,*) 
!        write (11,*) 
!        write (11,*)
!     write (11,*) HighX,HighY,' 2'
!     write (11,*) 'Pit_No Pass_Hgt'
!     do j=1,HighY
!       do i=1,HighX
!         if (i >= LowX .and. j >= LowY) then
!           write (11,*) Pit_Region(i,j),Pass(abs(Pit_Region(i,j)))%Z
!         else
!           write (11,*) ' 0.0 0.0'
!         end if
!       end do
!     end do
!     close(unit=11,status='keep')
!     stop
      deallocate(Pit)
      deallocate(Pass)
      deallocate(UnResolved)
      deallocate(Pit_Region)
      RETURN
END SUBROUTINE pitanal2XY

end module pitanal
