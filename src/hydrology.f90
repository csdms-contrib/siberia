module hydrology
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
! ====================================================================
! ====================================================================
!   this is the simple storage routing runoff-routing hydrology model  
!   developed for eriss
!     - GRW October 2006
! ====================================================================
! ====================================================================

  real, parameter :: hydrology_version = 1.00

  real(kind(double)),private :: RainRate,OldTime,PotentialET,SorpExp
  logical, private :: first_time=.true.
!  parameters and states
  real(kind(double)), private :: SorpStoreCapacity,Porosity,FieldCapacity       &
&         ,MaxInfiltrationRate
  real(kind(double)),dimension(:,:),allocatable,private :: SorpStore            &
&         ,SurfaceStore,GWStore,Sorp,Phi,SurfaceK,SorpK,GWK,aet                 &
&         ,tempinfiltration,tempdeepdrainage,PotInfiltration

contains


! ====================================================================
!    do the initialisation and start up of the hydrology model
! ====================================================================

subroutine init_hydrology(inputfile,totaltime,LowX,HighX,LowY,HighY,status)
  USE support
  implicit none
  integer :: LowX,HighX,LowY,HighY,status
  real(kind(double)) :: totaltime
  character(*) :: inputfile
  
  integer :: ErrorNo
  
!  allocate storage for the hydrology model

  allocate(SorpStore(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('SorpStore','SIBERIA_HYDROLOGY')
  allocate(SurfaceStore(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('SurfaceStore','SIBERIA_HYDROLOGY')
  allocate(GWStore(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('GWStore','SIBERIA_HYDROLOGY')
  allocate(Sorp(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('Sorp','SIBERIA_HYDROLOGY')
  allocate(Phi(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('Phi','SIBERIA_HYDROLOGY')
  allocate(SurfaceK(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('SurfaceK','SIBERIA_HYDROLOGY')
  allocate(SorpK(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('SorpK','SIBERIA_HYDROLOGY')
  allocate(GWK(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('GWK','SIBERIA_HYDROLOGY')
  allocate(aet(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('aet','SIBERIA_HYDROLOGY')
  allocate(tempinfiltration(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('tempinfiltration','SIBERIA_HYDROLOGY')
  allocate(potinfiltration(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('potinfiltration','SIBERIA_HYDROLOGY')
  allocate(tempdeepdrainage(LowX:HighX,LowY:HighY),stat=ErrorNo)
  IF (ErrorNo /= 0) CALL AllocationError('tempdeepdrainage','SIBERIA_HYDROLOGY')
  
!  read in the parameters from the model file

!  set some testing parameters

! NB: these parameters are units of metres and seconds
  Sorp=60.0*0.001/sqrt(3600.0)
  Phi=6.0*0.001/3600.0
  MaxInfiltrationRate=50000.0*0.001/3600.0
!  set k's to 0.1/hour for testing
  SorpK=0.1/3600.0
  SurfaceK=0.2/3600.0
  GWK=0.1/3600.0
!  capacities
  SorpStoreCapacity=1000*0.001
!  Porosity=0.4
  FieldCapacity=0.1
!  set catchment dry initially
  SurfaceStore=0.0
  SorpStore=SorpStoreCapacity*FieldCapacity*0.1
  GWStore=0.0
  SorpExp=-0.5

!  read in the time series data for the rainfall and evaporation
!  jusy set a rainfall=10mm/hr and pet=4.0mm/day as a test case and convert to m/s

  RainRate=100.0*0.001/3600.0
  PotentialET=4.0*0.001/(3600.0*24.0)

  first_time=.false.
  oldtime=totaltime
  if (SorpExp < -1.0) then
    call message_output(Fatal,' Sorpexp < -1.0 not allowed')
  end if
  
end subroutine init_hydrology

! ====================================================================
!    do the hydrology calcs
! ====================================================================

subroutine analysis_hydrology(inputfile,totaltime,timestep,discharge,direct     &
&              ,Domain,GridX,GridY,LowX,HighX,LowY,HighY,status)
  use support
  use parameters
  use siberiaconstants
  implicit none
  integer :: GridX,GridY,LowX,HighX,LowY,HighY,status
  real(kind(double)) :: totaltime,timestep
  INTEGER,DIMENSION(GridX,GridY) :: Direct
  real(kind(double)),DIMENSION(GridX,GridY) :: discharge
  logical,DIMENSION(GridX,GridY) :: domain
  character(*) :: inputfile
  
  integer :: i,j,ii,jj,Oldi,Oldj
  real(kind(double)) :: area,SurfaceOutflow,SorpOutflow,temp,SM,rf              &
&         ,dt,Infiltration,SurfaceStoreTemp,ppet,SorpStoreTemp,DeepDrainage     &
&         ,rainfall,pet
  integer,dimension(LowX:HighX,LowY:HighY) :: NoIn
  real(kind(double)),dimension(LowX:HighX,LowY:HighY) :: Sum

! NB all calcs are done in units of m/s until right at the end when things
!   are converted to volume units

!  check subroutine has been initialised  
  if (first_time) then
    call Init_Hydrology(inputfile,totaltime,LowX,HighX,LowY,HighY,status)
  end if
!  do the calcs
  NoIn=0
!  Sum is used to accumulate the surface discharges
  Sum=0
  aet=0
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
!  track flow down from top of catchment to bottom
! units conversion assume that each timestep is an hour in length
!  dt=3600.0*(totaltime-oldtime)
  dt=60.0*(totaltime-oldtime)
!  absolute amount of rainfall and et in this timestep
  rainfall=dt*RainRate
  pet=dt*PotentialET
  DO j=LowY,HighY
    DO i=LowX,HighX
      IF (Domain(i,j)) THEN
!  Only enter this loop if this node is the most upstream node in a drainage path
      if (noIn(i,j) == 0) then
        Oldi=i
        Oldj=j
!   DO THE SURFACE STORAGE WATER BALANCE
!  --------------------------------------
        SurfaceOutflow=SurfaceK(i,j)*SurfaceStore(i,j)*dt
        if (SorpStore(i,j) <= 0.0) then
          Infiltration=MaxInfiltrationRate*dt
        else
          if (SorpExp == -0.5) then
            Infiltration=(phi(i,j)+2*Sorp(i,j)**2/SorpStore(i,j))*dt
          else if (SorpExp == -1.0) then
            Infiltration=(phi(i,j)+Sorp(i,j)*exp(SorpStore(i,j)/Sorp(i,j)))*dt
          else
            Infiltration=(phi(i,j)+                                             &
&                 (((1+SorpExp)*SorpStore(i,j))**(SorpExp/(1.0+SorpExp))        &
&             *Sorp(i,j)**(1.0/(1.0+SorpExp))))*dt
          end if
        end if
        Infiltration=min(MaxInfiltrationRate,Infiltration)
        potInfiltration(i,j)=infiltration
        aet(i,j)=pet
        SurfaceStoreTemp=SurfaceStore(i,j)+(rainfall-Infiltration               &
&                         -SurfaceOutflow-pet)
        if (i == 25 .and. j== 5 .and. dt /= 0.) then
        WRITE (*,*) 'here 1a ',totaltime,SurfaceStoreTemp,SurfaceStore(i,j)     &
&                       ,Infiltration,SorpExp,dt,sorp(i,j),sorpstore(i,j)
        end if
        if (SurfaceStoreTemp < 0.0 ) then
!  Too much is taken out of the surface store if 
!  potential rates are used. This is the amount.
          temp=abs(SurfaceStoreTemp)
          if (temp < pet) then
            aet(i,j)=temp
          else if (temp < (pet+infiltration)) then
            Infiltration=SurfaceStore(i,j)+rainfall
            aet(i,j)=0
          else
            SurfaceOutflow=temp-(pet+infiltration)
            Infiltration=0
            aet(i,j)=0
          end if
          SurfaceStore(i,j)=0
        else
          SurfaceStore(i,j)=SurfaceStoreTemp
        end if
        sum(i,j)=sum(i,j)+SurfaceOutflow
        tempinfiltration(i,j)=infiltration
!   DO THE SOIL WATER BALANCE
!  ---------------------------
! soil moisture restriction on bare soil evaporation
        SM=SorpStore(i,j)/SorpStoreCapacity
        ppet=max(0.0,(pet-aet(i,j))*SM)
        if (SM > FieldCapacity) then
!          DeepDrainage=0
          DeepDrainage=Phi(i,j)*dt
        else
          DeepDrainage=0.0
        end if
        SorpStoreTemp=SorpStore(i,j)+(Infiltration-DeepDrainage-ppet)
!     check for saturation and exfiltrate if necessary
        if (SorpStoreTemp > SorpStoreCapacity) then
!     exfiltrate
          sum(i,j)=sum(i,j)+(SorpStoreTemp-SorpStoreCapacity)
          SorpStore(i,j)=SorpStoreCapacity
! check for dryout of the sorptivity store
        else if (SorpStoreTemp < 0) then
          temp=abs(SorpStoreTemp)
          if (temp < ppet) then
            aet(i,j)=aet(i,j)+ppet-temp
          else
            DeepDrainage=phi(i,j)-temp
            ppet=0
          end if
          SorpStore(i,j)=SorpStore(i,j)+(Infiltration-DeepDrainage-ppet)
        else
          aet(i,j)=aet(i,j)+ppet
          SorpStore(i,j)=SorpStoreTemp
        end if
        tempdeepdrainage(i,j)=deepdrainage
!   DO THE GW BALANCE
!  -------------------
        gwstore(i,j)=gwstore(i,j)+DeepDrainage
!  go to the next node D/S
        ii=i+dir1(Direct(Oldi,Oldj))
        jj=j+dir2(Direct(Oldi,Oldj))
 1060   IF (ii /= Oldi.or.jj /= Oldj) THEN
!  OK we have got to a point where all inflows are accted for
          IF (NoIn(ii,jj) == 1) THEN
!            AreaTerm(ii,jj)=AreaTerm(ii,jj)+AreaTerm(Oldi,Oldj)
            Oldi=ii
            Oldj=jj
            ii=ii+dir1(Direct(Oldi,Oldj))
            jj=jj+dir2(Direct(Oldi,Oldj))
            GO TO 1060
          ELSE
!  OK just add the flow to that node and go to next U/S node
!            AreaTerm(ii,jj)=AreaTerm(ii,jj)+AreaTerm(Oldi,Oldj)
            NoIn(ii,jj)=NoIn(ii,jj)-1
          END IF
        END IF
      end if
      end if
    END DO
  END DO
!  cleanup and finalisation of calcs for this timestep
  oldtime=totaltime
  write (51,6111) totaltime,sum(25,5),surfacestore(25,5),sorpstore(25,5)           &
&         ,gwstore(25,5),aet(25,5),potinfiltration(25,5),tempinfiltration(25,5)&
&         ,tempdeepdrainage(25,5),phi(25,5)*dt,rainfall
 6111 format(' ',f6.2,20(' ',e15.5))
end subroutine analysis_hydrology

end module hydrology