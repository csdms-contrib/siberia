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
MODULE CtrOutput
 
  REAL,PARAMETER :: CtrOutput_Version=8.17

CONTAINS

! 
! ===============================================================
!    Routine TO output contours of states and other useful spatial
!    information.
! ===============================================================
! 
SUBROUTINE CtrOut(a,z,Area,yy,s0,itot,isteps,Direct,Sed                &
&         ,dZdX2,Soutflow,IrregularBoundary                            &
&         ,Domain,iTime,ZmassA,ZmassP,Zin,Zout                         &
&         ,DetCIF,SumCycle,InitTimeStep1,iInit,iXXX,iYYY               &
&         ,SumSed,NoRegions,SoilDepth                                  &
&         ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ                     &
&         ,Factor,DischargeM1,MaxSt2,MaxDz2,MeanSt                     &
&         ,Hill_Channel_Factor,SimParameters,MCRealNo,gridX,gridY)
  USE SiberiaConstants
  USE SiberiaTypes
  USE Multipliers
  USE SiberiaTypes
  USE Setup
  USE Support
  USE Others
  USE openMPsupport
    IMPLICIT NONE
! 
    INTEGER :: itot,isteps,iTime,iInit,iXXX(*),iYYY(*),NoRegions       &
&       ,LowX,HighX,LowY,HighY,NoFlowIn,FlowInIJ(2,*),MCRealNo         &
&       ,gridX,gridY
    INTEGER,DIMENSION(gridX,gridY) :: Direct
    REAL(KIND(0.0D0)) :: ZmassA,ZmassP,Zin,Zout,SumCycle,SumSed(*)     &
&       ,MaxSt2,MaxDz2,MeanSt,Soutflow,InitTimeStep1
    REAL(KIND(0.0D0)),DIMENSION(gridX,gridY) :: a,z,yy,SoilDepth       &
&       ,s0,Sed,dZdX2,Factor,DischargeM1,Area,Hill_Channel_Factor
    LOGICAL :: DetCIF,IrregularBoundary,Domain(gridX,gridY)
    TYPE(LocalParameters) :: SimParameters
!                 
    INTEGER :: i,j,TotalArea,SumPits,ii,jj,dout,titlelgth,lgtht
    REAL(KIND(0.0D0)) :: MaxZ,MinZ,Maxs0,MaxOA,MaxSed,Mins0,MinSed          &
&        ,MaxArea,MinArea,MaxSt1,MaxDiff,Dd,MaxNCIF,TotalMass,PitS0    &
&        ,MinOA,MinCA,MaxCA,HypsoInt,MaxPitS0,MinPitS0                 &
&        ,MinSoil,MaxSoil,MinTop,MaxTop,MeanZ,S0threshold              &
&        ,Lower,Upper,MeanS0,MinFactor,MaxFactor,MinDischargeM1        &
&        ,MaxDischargeM1,MinWork,MaxWork,Multiplier,stability
    REAL(KIND(0.0D0)),dimension(gridX,gridY) :: y,TopIndex,WorkArray
    CHARACTER(20) :: iitime,iitimet
    CHARACTER(80) :: title
    type(PtXYValueR8) :: MaxA

!$omp critical
      MaxSt1=MaxSt2
      MaxA%value=0.0
      MaxOA=0.0
      MaxCA=0
      MinOA=1e30
      MinCA=1e30
      MaxArea=0.0
      MaxZ=-100000
      MinZ=100000
      Maxs0=-100000
      Mins0=100000
      MeanS0=0
      MaxDiff=0.0
      MaxSed=-1000000 
      MinSed=1000000
      MinSoil=1000000
      MaxSoil=-1000000
      MinTop=1000000
      MaxTop=-1000000
      MinFactor=1.e30
      MaxFactor=-1.e30
      MinDischargeM1=1.e30
      MaxDischargeM1=-1.e30
      TotalMass=0
      Dd=0
      TotalArea=0
      stability=0
      DO j=1,SimParameters%ky
        DO i=1,SimParameters%kx
          topindex(i,j)=0
        END DO
      END DO
      CALL SumPitsSub(SimParameters%kx,SimParameters%ky,Direct,s0,GridX,GridY,iInit      &
&          ,iXXX,iYYY,sumpits,PitS0,IrregularBoundary,Domain,MinPitS0,MaxPitS0)
        DO j=lowY,highY
          DO i=lowX,highX
            y(i,j)=0
            IF ((IrregularBoundary.and.Domain(i,j))                                      &
&              .or.(.not.IrregularBoundary)) THEN
              a(i,j)= SimParameters%b5*(SimParameters%b3*Area(i,j)**SimParameters%m3)    &
&                    **SimParameters%m5*s0(i,j)**SimParameters%n5
              IF (.not.DetCIF) THEN
                IF (a(i,j)*SimParameters%c1 > 0.005) THEN
                  y(i,j)=1.0
                ELSE
                  y(i,j)=0
                END IF
!                y(i,j)=1
!     &           -PDFNorm(((b5*c1*s0(i,j))**(-1/m5)*Area(i,j)**(-m3)-b3)
!     &            /b3SDl)
              ELSE
                y(i,j)=yy(i,j)
              END IF
              TotalMass=TotalMass+z(i,j)
              if (a(i,j) > MaxA%value) then
                MaxA%value=max(MaxA%value,a(i,j))
                MaxA%x=i
                MaxA%y=j
              end if
              IF (y(i,j) < 0.85) THEN
                MaxOA=max(MaxOA,a(i,j))
                MinOA=min(MinOA,a(i,j))
              ELSE
                MaxCA=max(MaxCA,a(i,j))
                MinCA=min(MinCA,a(i,j))
              END IF
              MaxArea=max(MaxArea,Area(i,j))
              Maxz=max(Maxz,z(i,j))
              Maxs0=max(Maxs0,s0(i,j))
              MeanS0=MeanS0+s0(i,j)
              Minz=min(Minz,z(i,j))
              MaxSed=max(MaxSed,Sed(i,j))
              MinSed=min(MinSed,Sed(i,j))
              IF (s0(i,j) /= 0.0) THEN
                Mins0=min(Mins0,s0(i,j))
                TopIndex(i,j)=a(i,j)/s0(i,j)
                MinTop=min(MinTop,TopIndex(i,j))
                MaxTop=max(MaxTop,TopIndex(i,j))
              ELSE
                TopIndex(i,j)=0
              END IF
              IF (SimParameters%dZ /= 0.0) MaxDiff=max(MaxDiff,abs(dZdX2(i,j)))
              IF (y(i,j) > 0.85) THEN
                Dd=Dd+1
              END IF
              TotalArea=TotalArea+1
              IF (SimParameters%modesoil /= 0) THEN
                MinSoil=min(MinSoil,SoilDepth(i,j))
                MaxSoil=max(MaxSoil,SoilDepth(i,j))
              END IF
              MinFactor=min(MinFactor,Factor(i,j))
              MaxFactor=max(MaxFactor,Factor(i,j))
              IF (Area(i,j) /= 0) THEN
                WorkArray(i,j)=               &
&                   (DischargeM1(i,j)/Area(i,j)**SimParameters%m1*SimParameters%m3)    &
&                    **(1.0/SimParameters%m1)
              ELSE
                WorkArray(i,j)=0
              END IF
              MinDischargeM1=min(MinDischargeM1,WorkArray(i,j))
              MaxDischargeM1=max(MaxDischargeM1,WorkArray(i,j))
              stability=stability+SimParameters%b1*(SimParameters%b3**SimParameters%m3   &
&                *area(i,j))**SimParameters%m1*s0(i,j)**SimParameters%n1
            END IF
          END DO
      END DO 
! 
      S0threshold=1.0e-3*SimParameters%GridXY
!      DO j=2,ky
!        DO i=2,kx
!          IF ((IrregularBoundary.and.Domain(i,j))
!     &         .or.(.not.IrregularBoundary)) THEN
!            ii=i+dir1(Direct(i,j))
!            jj=j+dir2(Direct(i,j))
!            IF (s0(i,j) > S0threshold.and.
!     &          s0(ii,jj) > S0threshold) THEN
!              IF (abs(Direct(i,j)) < 10) THEN
!                MaxSt1=max(MaxSt1
!     &            ,abs(Sed(i,j)-Sed(ii,jj))/s0(i,j)/abs(InitTimeStep1))
!              ELSE
!                MaxSt1=max(MaxSt1
!     &            ,0.7071*abs(Sed(i,j)-Sed(ii,jj))/s0(i,j)/abs(InitTimeStep1))
!              END IF
!            END IF
!          END IF
!        END DO
!      END DO
! 
!  remove the fixed Elevation points from the calcs
! 
      TotalArea=TotalArea-iInit
      DO i=1,iInit
        MeanS0=MeanS0-s0(iXXX(i),iYYY(i))
        TotalMass=TotalMass-z(iXXX(i),iYYY(i))
        IF (y(iXXX(i),iYYY(i)) > 0.85) THEN
          Dd=Dd-1
        END IF
      END DO
      MeanS0=MeanS0/float(TotalArea)
      MeanZ=TotalMass/TotalArea
      HypsoInt=(TotalMass-TotalArea*MinZ)/(MaxZ-MinZ)/TotalArea
      Dd=Dd/TotalArea
      MaxNCIF=MaxOA*SimParameters%c1/0.0025
      stability=stability/TotalArea
      if (SimParameters%ModeMC == 0) then
         call Str_AppendIntToStrCompact(iitime,'t=',itot)
      else
         call Str_AppendIntToStrCompact(iitimet,'mc=',MCRealNo)
         iitimet(lgtht+1:lgtht+3) = ' t='
         lgtht=lgtht+3
         call Str_AppendIntToStrCompact(trim(iitime),trim(iitimet),itot)
      end if
      dout=6
      CALL OutStatsTable(itot,isteps,trim(iitime),MaxA              &
&         ,MinOA,MaxOA,MinCA                                          &
&         ,MaxCA,MaxNCIF,MaxArea,Mins0,MaxS0,MeanS0                   &
&         ,Minz,Maxz,MinSed                                           &
&         ,MaxSed,Soutflow,SimParameters%dZ,MaxDiff,SumCycle,MaxSt1   &
&         ,MaxDz2,InitTimeStep1,SumPits                               &
&         ,MinPitS0,MaxPitS0,TotalMass,HypsoInt,noregions             &
&         ,SumSed,SimParameters%ModeSoil                              &
&         ,MinSoil,MaxSoil,MinTop,MaxTop,MeanZ,TotalArea              &
&         ,MeanSt,Stability,Dd)
! 
!
! 
!     MaxA=MaxA*0.5
      IF (SimParameters%StatsTime >= 0) go to 9999
      IF (Maxs0 == 0.0) Maxs0=1.e10
! 
!  Echoing Input DATA
! --------------------
! 
      IF (FirstCtrOut) THEN
! 
!  The erosion Factor
! 
        IF (mod(SimParameters%ModeErode,20) == 3) THEN
          title(1:12)=' Erodibility'
          titlelgth=12
          Multiplier=1
          CALL DisplayBlockReal(Factor,GridX,GridY                    &
&           ,minFactor,maxFactor                                      &
&           ,title,titlelgth,IrregularBoundary,Domain                 &
&           ,SimParameters%kx,SimParameters%ky,Multiplier)
        END IF
        IF (mod(SimParameters%ModeRunoff,20) == 3) THEN
          title(1:11)=' Approx. b3'
          titlelgth=11
          Multiplier=1
          CALL DisplayBlockReal(WorkArray,GridX,GridY                 &
&           ,minDischargeM1,maxDischargeM1,title,titlelgth            &
&           ,IrregularBoundary,Domain,SimParameters%kx                &
&           ,SimParameters%ky,Multiplier)
        END IF
        FirstCtrOut=.false.
      END IF
! 
!  Outputting Simulation DATA
! ----------------------------
! 
!  Channels
! 
      title='  Channels'
      titlelgth=10
      Upper=1
      Lower=0
      Multiplier=1
      CALL DisplayBlockReal(y,GridX,GridY,Lower,Upper,title,titlelgth    &
&           ,IrregularBoundary,Domain,SimParameters%kx,SimParameters%ky  &
&           ,Multiplier)
! 
!   Channel Initiation FUNCTION
! 
      title='  Nondimensional Channel Initiation FUNCTION'
      titlelgth=44
      Lower=0
      Multiplier=1
      CALL DisplayBlockReal(a,GridX,GridY,Lower,maxa%value                    &
&           ,title,titlelgth,IrregularBoundary,Domain,SimParameters%kx  &
&           ,SimParameters%ky,Multiplier)
! 
!   Channel Initiation FUNCTION
! 
      title='  Hillslope-Channel Factor'
      titlelgth=26
      Lower=-0.00001
      Multiplier=1
      maxa%value=0.00001
      CALL DisplayBlockReal(Hill_Channel_Factor,GridX,GridY             &
&           ,Lower,maxa%value,title,titlelgth                                 &
&           ,IrregularBoundary,Domain,SimParameters%kx                  &
&           ,SimParameters%ky,Multiplier)
! 
!   Erosion
! 
!      CALL DisplayBlockErosion(Sed,GridX,GridY,MinSed,MaxSed
!     &        ,IrregularBoundary,Domain,SimParameters%kx,SimParameters%ky)
! 
!   Erodibility
! 
        IF (SimParameters%ModeErode < 0                                &
&               .or. mod(SimParameters%ModeErode,20) == 1) THEN
          title=' Erodibility'
          titlelgth=12
          Multiplier=1
          CALL DisplayBlockReal(Factor,GridX,GridY                     &
&           ,minFactor,maxFactor,title,titlelgth                       &
&           ,IrregularBoundary,Domain,SimParameters%kx                 &
&           ,SimParameters%ky,Multiplier)
        END IF
! 
!  Slopes
! 
      title='  Slope'
      titlelgth=7
      CALL DisplayBlockReal(s0,GridX,GridY,minS0,maxS0                 &
&           ,title,titlelgth,IrregularBoundary,Domain                  &
&           ,SimParameters%kx,SimParameters%ky,MultiplierSlope)
! 
!    Elevations
! 
      title='  Elevations'
      titlelgth=12
      Multiplier=1
      CALL DisplayBlockReal(z,GridX,GridY,minz,maxz,title              &
&           ,titlelgth,IrregularBoundary,Domain,SimParameters%kx       &
&           ,SimParameters%ky,Multiplier)
! 
!    AREAS
! 
      title='  Areas'
      titlelgth=7
      MinArea=1
      CALL DisplayBlockReal(Area,GridX,GridY,minarea,maxarea            &
&           ,title,titlelgth,IrregularBoundary,Domain                   &
&           ,SimParameters%kx,SimParameters%ky,MultiplierArea)
! 
!    Soil Depths
! 
  678 IF (SimParameters%modesoil /= 0) THEN
        title='  Soil Depths'
        titlelgth=13
        Multiplier=1
        CALL DisplayBlockReal(SoilDepth,GridX,GridY                     &
&             ,minSoil,maxSoil,title,titlelgth,IrregularBoundary        &
&             ,Domain,SimParameters%kx,SimParameters%ky,Multiplier)
        title='  Topographic Index'
        titlelgth=19
        Multiplier=MultiplierArea/MultiplierSlope
        CALL DisplayBlockReal(TopIndex,GridX,GridY                      &
&             ,minTop,maxTop,title,titlelgth,IrregularBoundary          &
&             ,Domain,SimParameters%kx,SimParameters%ky,Multiplier)
      END IF
! 
!   Stability Number
! 
      S0threshold=1.0e-3*SimParameters%GridXY
      MinWork=1000000
      MaxWork=-1000000
      DO j=lowY,HighY
        DO i=lowX,highX
          WorkArray(i,j)=0
          IF ((IrregularBoundary.and.Domain(i,j))                           &
&              .or.(.not.IrregularBoundary)) THEN
            ii=i+dir1(Direct(i,j))
            jj=j+dir2(Direct(i,j))
            IF (s0(i,j) > S0threshold.and.                                  &
&               s0(ii,jj) > S0threshold) THEN
              IF (abs(Direct(i,j)) < 10) THEN
                WorkArray(i,j)=abs(Sed(i,j)-Sed(ii,jj))                     &
&                   /s0(i,j)/abs(InitTimeStep1)
!               tempDz=abs(dZdX2(i,j)-dZdX2(ii,jj))/s0(i,j)
              ELSE
                WorkArray(i,j)=0.7071*abs(Sed(i,j)-Sed(ii,jj))              &
&                   /s0(i,j)/abs(InitTimeStep1)
!               tempDz=0.7071*abs(dZdX2(i,j)-dZdX2(ii,jj))/s0(i,j)
              END IF
              MinWork=min(MinWork,WorkArray(i,j))
              MaxWork=max(MaxWork,WorkArray(i,j))
            END IF
          END IF
        END DO
      END DO
      title='  Fluvial Stability No'
      titlelgth=22
      Multiplier=1
      CALL DisplayBlockReal(WorkArray,GridX,GridY             &
&             ,minWork,maxWork,title,titlelgth                        &
&             ,IrregularBoundary,Domain,SimParameters%kx              &
&             ,SimParameters%ky,Multiplier)
! 
!   Drainage Directions
! 
      CALL DisplayBlockDirections(Direct,GridX,GridY                  &
&            ,IrregularBoundary,Domain,SimParameters%kx               &
&            ,SimParameters%ky)
 934  FORMAT (80a1)
      CALL working(itot)
 9999 continue
!$omp END critical
      RETURN
END SUBROUTINE ctrout
! 
! ==================================================================
!  routine TO calc the hypsometric integral of a Elevation Domain
! ==================================================================
! 
REAL(KIND(0.0D0)) FUNCTION CalcHypsInt(z,LowX,HighX,LowY,HighY   &
&         ,IrregularBoundary,Domain,gridX,gridY)
    IMPLICIT NONE
! 
    INTEGER :: LowX,HighX,LowY,HighY,gridX,gridY
    REAL(KIND(0.0D0)) :: z(gridX,gridY)
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)
! 
    INTEGER :: i,j
    REAL(KIND(0.0D0)) :: TotalMass,TotalArea,Minz,Maxz
! 
      Minz=100000
      Maxz=-100000
      TotalMass=0
      TotalArea=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF ((IrregularBoundary.and.Domain(i,j))                   &
&              .or.(.not.IrregularBoundary)) THEN
            TotalMass=TotalMass+z(i,j)
            TotalArea=TotalArea+1
            Minz=min(Minz,z(i,j))
            Maxz=max(Maxz,z(i,j))
          END IF
        END DO
      END DO
      CalcHypsInt=(TotalMass-TotalArea*MinZ)/(MaxZ-MinZ)/TotalArea
      RETURN
END FUNCTION CalcHypsInt
! 
! ==================================================================
!  routine TO calc the mass of a Elevation Domain
! ==================================================================
! 
REAL(KIND(0.0D0)) FUNCTION CalcMass(z,LowX,HighX,LowY,HighY                  &
&        ,IrregularBoundary,Domain,TotalArea,gridX,gridY)
    IMPLICIT NONE
! 
    INTEGER :: LowX,HighX,LowY,HighY,TotalArea,gridX,gridY
    REAL(KIND(0.0D0)) :: z(gridX,gridY)
    LOGICAL :: IrregularBoundary,Domain(gridX,gridY)
! 
    INTEGER :: i,j
    REAL(KIND(0.0D0)) :: TotalMass
! 
      TotalMass=0
      TotalArea=0
      DO j=LowY,HighY
        DO i=LowX,HighX
          IF ((IrregularBoundary.and.Domain(i,j))                   &
&              .or.(.not.IrregularBoundary)) THEN
            TotalMass=TotalMass+z(i,j)
            TotalArea=TotalArea+1
          END IF
        END DO
      END DO
      CalcMass=TotalMass
      RETURN
END FUNCTION CalcMass
! 
! ==========================================================================
!  Utility routine
! ==========================================================================
! 
SUBROUTINE MinMaxData(thedata,GridX,GridY,mindata,maxdata               &
&         ,IrregularBoundary,Domain,LowX,HighX,LowY,HighY)
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,LowX,HighX,LowY,HighY
    REAL(KIND(0.0D0)) :: thedata(GridX,GridY),mindata,maxdata
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
! 
    INTEGER :: i,j
!  Initialisation
      IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              mindata=thedata(i,j)
              maxdata=thedata(i,j)
              GO TO 1000
            END IF
          END DO
        END DO
      ELSE
        mindata=thedata(2,2)
        maxdata=thedata(2,2)
      END IF
!  Calcs
 1000 IF (IrregularBoundary) THEN
        DO j=LowY,HighY
          DO i=LowX,HighX
            IF (Domain(i,j)) THEN
              mindata=min(mindata,thedata(i,j))
              maxdata=max(maxdata,thedata(i,j))
            END IF
          END DO
        END DO
      ELSE
        DO j=LowY,HighY
          DO i=LowX,HighX
            mindata=min(mindata,thedata(i,j))
            maxdata=max(maxdata,thedata(i,j))
          END DO
        END DO
      END IF
      RETURN
END SUBROUTINE MinMaxData
! 
! ==================================================================
!  Display the drainage directions
! ==================================================================
! 
SUBROUTINE DisplayBlockDirections(Direct,GridX,GridY               &
&            ,IrregularBoundary,Domain,kx,ky)
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,Direct(GridX,GridY),kx,ky
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
! 
    INTEGER :: ix,iy,k,itop,ibottom,NoBlocks
    CHARACTER(82) :: dash,typ
! 
      DO k=1,82
        dash(k:k)='-'
      END DO
      NoBlocks=(kx-1)/80+1
      call Message_Output(Message_Info,'  Drainage Directions')
      DO k=1,NoBlocks
        IF (k > 1) THEN
          call Message_Output(Message_Info,'  Drainage Directions'//          &
&                    ': (continued)  Panel ',k)
        END IF
        ibottom=1+(k-1)*80
        IF (k /= NoBlocks) THEN
          itop=80+(k-1)*80
        ELSE
          itop=kx        
        END IF
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
 6001   FORMAT(a)
        DO IY=2,KY              
          DO IX=ibottom,itop
            typ(ix-ibottom+1:ix-ibottom+1)='.'
            IF ((IrregularBoundary.and.Domain(ix,iy))                         &
&               .or.(.not.IrregularBoundary)) THEN
              IF (Direct(ix,iy) == 1.or.Direct(ix,iy) == 3) THEN
               typ(ix-ibottom+1:ix-ibottom+1)='-'
              ELSE IF (Direct(ix,iy) == 5.or.Direct(ix,iy) == 6) THEN
               typ(ix-ibottom+1:ix-ibottom+1)='O'
              ELSE IF (Direct(ix,iy) == 2.or.Direct(ix,iy) == 4) THEN
               typ(ix-ibottom+1:ix-ibottom+1)='|'                                   
              ELSE IF (Direct(ix,iy) == 11.or.Direct(ix,iy) == 13) THEN
               typ(ix-ibottom+1:ix-ibottom+1)='/'
              ELSE IF (Direct(ix,iy) == 12.or.Direct(ix,iy) == 14) THEN
               typ(ix-ibottom+1:ix-ibottom+1)='\\'
              END IF
             END IF
          END DO
          call Message_Output(Message_Info,typ(1:itop-ibottom+1))
        END DO
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
        call Message_Output(Message_Info,' ')
      END DO
      RETURN
! 
 9998   CALL Message_Output(Message_ErrorContinue                          &
&         ,' --INTERNAL ERROR 2 -- DisplayBlockDirections, '               &
&         ,itop,',',ibottom) 
      RETURN
 9999   CALL Message_Output(Message_ErrorContinue                          &
&         ,' --INTERNAL ERROR 1 -- DisplayBlockDirections, '               &
&         ,itop,',',ibottom)
      RETURN
END SUBROUTINE DisplayBlockDirections
! 
! ==========================================================================
!  Standard display block of contour DATA between a max and min value
!                       Erosion/Deposition DATA
! ==========================================================================
! 
SUBROUTINE DisplayBlockErosion(Sed,GridX,GridY,MinSed,MaxSed       &
&             ,IrregularBoundary,Domain,kx,ky)
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,kx,ky
    REAL(KIND(0.0D0)) :: Sed(GridX,GridY),MinSed,MaxSed
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
! 
    INTEGER :: k,ibottom,itop,ix,iy,itemp,NoBlocks
    CHARACTER(82) :: dash,typ
! 
    CHARACTER(26) :: LCLetters,UCLetters
    DATA LCLetters /'abcdefghijklmnopqrstuvwxyz'/,                 &
&          UCLetters /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
! 
      call Message_Output(Message_Info                             &
&          ,'  Sediment EROSION/Deposition'                        &
&         //':       Min = ',sngl(MinSed)                          &
&          ,' Max = ',sngl(MaxSed))
      maxsed=max(abs(minsed),abs(maxsed))
      IF (maxsed == 0.0) maxsed=0.000001
      NoBlocks=(kx-1)/80+1
      DO k=1,82
        dash(k:k)='-'
      END DO
      DO k=1,NoBlocks
        IF (k > 1) THEN
          call Message_Output(Message_Info                         &
&                   ,'  Sediment EROSION/Deposition'//             &
&                    ': (continued)  Panel ',k)
        END IF
        ibottom=1+(k-1)*80
        IF (k /= NoBlocks) THEN
          itop=80+(k-1)*80
        ELSE
          itop=kx        
        END IF
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
 6001   FORMAT(a)
        DO iy=2,ky
          DO ix=ibottom,itop
            typ(ix-ibottom+1:ix-ibottom+1)='.'
            IF ((IrregularBoundary.and.Domain(ix,iy))               &
&               .or.(.not.IrregularBoundary)) THEN
              typ(ix-ibottom+1:ix-ibottom+1)=' '
              IF (Maxsed /= 0.0) THEN
                itemp=abs(Sed(ix,iy)+1.e-15)*20/abs(maxsed)
              ELSE
                itemp=0
              END IF
              itemp=min(itemp,20)
              IF (itemp /= 0) THEN
                IF (Sed(ix,iy) < 0) THEN
!                 EROSION
                  typ(ix-ibottom+1:ix-ibottom+1)=UCLetters(itemp:itemp)
                ELSE
!                 DEPOSITION
                  typ(ix-ibottom+1:ix-ibottom+1)=LCLetters(itemp:itemp)
                END IF
              END IF
            END IF
          END DO
          call Message_Output(Message_Info,typ(1:itop-ibottom+1))
        END DO
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
        call Message_Output(Message_Info,' ')
      END DO
      RETURN
 9998   CALL Message_Output(Message_ErrorContinue                       &
&         ,' --INTERNAL ERROR 2 -- DisplayBlockErosion, '               &
&         ,itop,',',ibottom)
      RETURN
 9999   CALL Message_Output(Message_ErrorContinue                       &
&         ,' --INTERNAL ERROR 1 -- DisplayBlockErosion, '               &
&         ,itop,',',ibottom)
      RETURN
END SUBROUTINE DisplayBlockErosion
! 
! ==========================================================================
!  Standard display block of contour DATA between a max and min value
!                       REAL DATA
! ==========================================================================
! 
SUBROUTINE DisplayBlockReal(thedata,GridX,GridY                          &
&         ,mindata,maxdata,title                                         &
&         ,titlelgth,IrregularBoundary                                   &
&         ,Domain,kx,ky,Multiplier)
  USE Support
  USE Setup
    IMPLICIT NONE
! 
!  This SUBROUTINE contours with letters a Grid of DATA. 
!  It assumes all DATA >= 0
! 
    REAL(KIND(0.0D0)) :: delta
    PARAMETER (delta=1.0d-20)
! 
    INTEGER :: GridX,GridY,kx,ky,titlelgth
    REAL(KIND(0.0D0)) :: thedata(GridX,GridY),mindata,maxdata,Multiplier
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
    CHARACTER(80) :: title
! 
    INTEGER :: ibottom,itop,i,j,k,itemp,NoBlocks
    REAL(KIND(0.0D0)) :: range,delta1
    CHARACTER(82) :: dash,typ

    CHARACTER(26) :: LCLetters
    DATA LCLetters /'abcdefghijklmnopqrstuvwxyz'/
    DATA typ(1:1) / '!' /
! 
      NoBlocks=(kx-1)/80+1
      call Message_Output(Message_Info,title(1:titlelgth)           &
&         //':       Min = ',sngl(mindata*Multiplier)               &
&          ,' Max = ',sngl(maxdata*Multiplier))
      DO k=1,82
        dash(k:k)='-'
      END DO
      range=max(abs(maxdata-mindata),abs(1.0d-6*maxdata))
      range=max(range,delta)
      delta1=max(abs(1.0d-6*maxdata),delta)
      DO k=1,NoBlocks
        IF (k > 1) THEN
          call Message_Output(Message_Info,title(1:titlelgth)       &
&                  //': (continued)  Panel ',k)
        END IF
        ibottom=1+(k-1)*80
        IF (k /= NoBlocks) THEN
          itop=80+(k-1)*80
        ELSE
          itop=kx        
        END IF
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
 6001   FORMAT(a)
        DO j=2,ky
          DO i=ibottom,itop
            typ(i-ibottom+1:i-ibottom+1)='.'
            IF ((IrregularBoundary.and.Domain(i,j))               &
&               .or.(.not.IrregularBoundary)) THEN
              typ(i-ibottom+1:i-ibottom+1)=' '
              itemp=(thedata(i,j)-mindata+delta1)*20/range
              itemp=min(itemp,20)
              IF (itemp >= 1) THEN
                typ(i-ibottom+1:i-ibottom+1)=LCLetters(itemp:itemp)
              END IF
            END IF
          END DO
          call Message_Output(Message_Info,typ(1:itop-ibottom+1))
        END DO
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
        call Message_Output(Message_Info,' ')
      END DO
! 
      RETURN
 9998   CALL Message_Output(Message_ErrorContinue                           &
&         ,' --INTERNAL ERROR 2 -- DisplayBlockReal, '                      &
&         ,itop,',',ibottom)
      RETURN
 9999   CALL Message_Output(Message_ErrorContinue                           &
&         ,' --INTERNAL ERROR 1 -- DisplayBlockReal, '                      &
&         ,itop,',',ibottom)
      RETURN
END SUBROUTINE DisplayBlockReal
! 
! ==========================================================================
!  Standard display block of contour DATA between a max and min value
!                         INTEGER DATA
! ==========================================================================
! 
SUBROUTINE DisplayBlockInteger(thedata,GridX,GridY                          &
&         ,mindata,maxdata,title,titlelgth,IrregularBoundary                &
&         ,Domain,kx,ky,NoFlowIn,FlowInIJ,Multiplier)
  USE Support
    IMPLICIT NONE
! 
!  This SUBROUTINE contours with letters a Grid of DATA. 
!  It assumes all DATA >= 0
! 
    INTEGER :: GridX,GridY,kx,ky,titlelgth,thedata(GridX,GridY)              &
&            ,mindata,maxdata,NoFlowIn,FlowInIJ(2,*)
    REAL(KIND(0.0D0)) :: Multiplier
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
    CHARACTER(80) :: title
! 
    INTEGER :: ibottom,itop,i,j,k,itemp,NoBlocks,range
    CHARACTER(82) :: dash,typ
! 
    CHARACTER(26) :: LCLetters  
    DATA LCLetters /'abcdefghijklmnopqrstuvwxyz'/
    DATA typ(1:1) / '!' /
! 
      range=abs(max(MaxData-MinData,1))
      NoBlocks=(kx-1)/80+1
      call Message_Output(Message_Info,title(1:titlelgth)                    &
&         //':       Min = ',sngl(mindata*Multiplier)                        &
&          ,' Max = ',sngl(maxdata*Multiplier))
      DO k=1,82
        dash(k:k)='-'
      END DO
      DO k=1,NoBlocks
        IF (k > 1) THEN
          call Message_Output(Message_Info                                   &
&                   ,title(1:titlelgth)//': (continued)  Panel ',k)
        END IF
        ibottom=1+(k-1)*80
        IF (k /= NoBlocks) THEN
          itop=80+(k-1)*80
        ELSE
          itop=kx        
        END IF
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
 6001   FORMAT(a)
        DO j=2,ky
          DO i=ibottom,itop
            typ(i-ibottom+1:i-ibottom+1)='.'
            IF ((IrregularBoundary.and.Domain(i,j))                          &
&               .or.(.not.IrregularBoundary)) THEN
              typ(i-ibottom+1:i-ibottom+1)=' '
              itemp=(thedata(i,j)-mindata)*20.0/range
              itemp=min(itemp,20)
              IF (itemp >= 1) THEN
                typ(i-ibottom+1:i-ibottom+1)=LCLetters(itemp:itemp)
              END IF
            END IF
          END DO
          DO i=1,NoFlowIn
            IF (FlowInIJ(2,i) == j) THEN
              IF (FlowInIJ(1,i) >= ibottom                                    &
&                  .and.FlowInIJ(1,i) <= itop) THEN
                IF ((IrregularBoundary.and.Domain(FlowInIJ(1,i),j))           &
&                      .or.(.not.IrregularBoundary)) THEN
                  typ(FlowInIJ(1,i)-ibottom+1:FlowInIJ(1,i)-ibottom+1)        &
&                     ='@'
                ELSE
                  typ(FlowInIJ(1,i)-ibottom+1:FlowInIJ(1,i)-ibottom+1)        &
&                     ='?'
                END IF
              END IF
            END IF
          END DO
          call Message_Output(Message_Info,typ(1:itop-ibottom+1))
        END DO
        call Message_Output(Message_Info,dash(1:itop-ibottom+1))
        call Message_Output(Message_Info,' ')
      END DO
! 
      RETURN
 9998   CALL Message_Output(Message_ErrorContinue                             &
&           ,' --INTERNAL ERROR 2 -- DisplayBlockInteger, '                   &
&           ,itop,',',ibottom)
      RETURN
 9999   CALL Message_Output(Message_ErrorContinue                             &
&           ,' --INTERNAL ERROR 1 -- DisplayBlockInteger, '                   &
&           ,itop,',',ibottom)
      RETURN
END SUBROUTINE DisplayBlockInteger
! 
! ==========================================================================
!  some pit statistics calculations
! ==========================================================================
! 
SUBROUTINE SumPitsSub                                                         &
&          (kx,ky,Direct,s0,GridX,GridY,iInit,iXXX,iYYY,sumpits,PitS0         &
&          ,IrregularBoundary,Domain,MinPitS0,MaxPitS0)
  USE SiberiaConstants
    IMPLICIT NONE
! 
    INTEGER :: GridX,GridY,kx,ky,Direct(GridX,GridY),iInit                     &
&        ,iXXX(*),iYYY(*),sumpits
    REAL(KIND(0.0D0)) :: PitS0,s0(GridX,GridY),MinPitS0,MaxPitS0
    LOGICAL :: IrregularBoundary,Domain(GridX,GridY)
! 
    INTEGER :: k,itempx,itempy,ii,jj,i,j
! 
      SumPits=0
      MinPitS0=1000000
      MaxPitS0=-1000000
      DO j=2,ky
        DO i=2,kx
          IF ((IrregularBoundary.and.Domain(i,j))               &
&              .or.(.not.IrregularBoundary)) THEN
            IF (Direct(i,j) == 5) THEN
              DO k=1,iInit
                itempx=abs(iXXX(k))
                itempy=abs(iYYY(k))
                IF ((i == itempx).and.(j == itempy)) GO TO 1200
              END DO
              SumPits=SumPits+1
              PitS0=0
              DO k=1,8
                ii=i+nodei(k)
                jj=j+nodej(k)
                IF ((IrregularBoundary.and.Domain(ii,jj))               &
&                    .or.(.not.IrregularBoundary)) THEN
                  IF (Direct(ii,jj) == dircomp(k)) THEN
                    PitS0=min(PitS0,s0(ii,jj))
                  END IF
                END IF
              END DO
            END IF
 1200       MinPitS0=min(MinPitS0,PitS0)
            MaxPitS0=max(MaxPitS0,PitS0)
          END IF
        END DO
      END DO
! 
      RETURN
END SUBROUTINE SumPitsSub
! 
! ==========================================================================
!  Output table of statistics
! ==========================================================================
! 
SUBROUTINE OutStatsTable                                                    &
&        (itot,isteps,iitime,MaxA,MinOA,MaxOA,MinCA                         &
&         ,MaxCA,MaxNCIF,MaxArea,Mins0,MaxS0,MeanS0,Minz,Maxz               &
&         ,MinSed,MaxSed,Soutflow,dZ,MaxDiff,SumCycle,MaxSt2,MaxDz2         &
&         ,InitTimeStep1,SumPits                                            &
&         ,MinPitS0,MaxPitS0,TotalMass,HypsoInt,NoRegions,SumSed            &
&         ,ModeSoil,MinSoil,MaxSoil,MinTop,MaxTop,MeanZ,TotalArea           &
&         ,MeanSt,Stability,Dd)
  USE Multipliers
  USE SiberiaTypes
  USE Support
    IMPLICIT NONE
! 
    INTEGER :: itot,isteps,SumPits,NoRegions,ModeSoil,TotalArea
    REAL(KIND(0.0D0)) :: MinOA,MaxOA,MinCA,MaxDz2,MaxArea                       &
&         ,MaxCA,MaxNCIF,Mins0,MaxS0,MeanS0,Minz,Maxz,MinSed               &
&         ,MaxSed,Soutflow,dZ,MaxDiff,SumCycle,MaxSt2,InitTimeStep1        &
&         ,MinPitS0,MaxPitS0,TotalMass,HypsoInt,SumSed(*),MeanSt           &
&         ,MinSoil,MaxSoil,MinTop,MaxTop,MeanZ,Stability,Dd
    CHARACTER(*) :: iitime
    character(100) :: string
    type(PtXYvalueR8) :: MaxA
! 
    INTEGER :: i
!
      string=' '
      WRITE(string,*) ' time = ',itot,' : iterations = ',isteps
      call timershowlong(string)
      call Message_Output(Message_Info,' ')
      call Message_Output(Message_Info,iitime//'  Max. CIF                = '    &
&           ,sngl(MaxA%value*sqrt(MultiplierArea)),' @ (',MaxA%x,',',MaxA%y,')')
      call Message_Output(Message_Info,iitime//'  Range Overland CIF      = '    &
&           ,sngl(MinOA*sqrt(MultiplierArea))                                    &
&           ,' , ',sngl(MaxOA*sqrt(MultiplierArea)))
      call Message_Output(Message_Info,iitime//'  Range Channel CIF       = '    &
&           ,sngl(MinCA*sqrt(MultiplierArea))                                    &
&           ,' , ',sngl(MaxCA*sqrt(MultiplierArea)))
      call Message_Output(Message_Info,iitime//'  Max. Normalised CIF     = '    &
&           ,sngl(MaxNCIF))
      call Message_Output(Message_Info,iitime//'  Max. Area               = '    &
&           ,sngl(MaxArea*MultiplierArea))
      call Message_Output(Message_Info,iitime//'  Slope Range             = '    &
&           ,sngl(Mins0*MultiplierSlope),' , ',sngl(MaxS0*MultiplierSlope))
      call Message_Output(Message_Info,iitime//'  Mean Slope              = '    &
&           ,sngl(MeanS0*MultiplierSlope))
      call Message_Output(Message_Info,iitime//'  Elevation Range         = '    &
&           ,sngl(Minz),' , ',sngl(Maxz))
      call Message_Output(Message_Info,iitime//'  Mean Elevation          = '    &
&           ,sngl(MeanZ))
      call Message_Output(Message_Info,iitime//'  Elevation Change Range  = '    &
&           ,sngl(MinSed),' , ',sngl(MaxSed))
      call Message_Output(Message_Info,iitime//'  Potential Elevation '          &
&               //'change at outlet/unit time = ',sngl(Soutflow))
      call Message_Output(Message_Info,iitime//'  Drainage Density        = '    &
&                ,sngl(Dd))
      IF (dZ > 0.0) THEN
        call Message_Output(Message_Info,iitime//'  Max. Diffusion          = '  &
&           ,sngl(MaxDiff))
      ELSE    
        MaxDiff=0
      END IF
      call Message_Output(Message_Info,iitime//'  Sum of cyclic erosion   = '     &
&           ,sngl(SumCycle))
      call Message_Output(Message_Info,' ')
      call Message_Output(Message_Info,iitime                                    &
&           //'  Mean Stability No.      = ',sngl(MeanSt))
      call Message_Output(Message_Info,iitime                                    &
&           //'  Extreme Stability No.   = ',sngl(MaxSt2))
      call Message_Output(Message_Info,iitime                                    &
&           //'  Other Stability No.     = ',sngl(stability))
      IF (dZ > 0.0) THEN
        call Message_Output(Message_Info,iitime//'  Diffusion Peclet No    = '   &
&           ,sngl(dZ*InitTimeStep1))
        call Message_Output(Message_Info,iitime//'  Diffusion Stability No = '   &
&           ,sngl(MaxDz2))
      END IF
      IF (SumPits >= 1) THEN
        call Message_Output(Message_Info,' ')
        call Message_Output(Message_Info,iitime//'  No of Pits       = '         &
&           ,SumPits)
        call Message_Output(Message_Info,iitime//'  Pit Slope Range  = '         &
&           ,sngl(MinPitS0*MultiplierSlope)                                      &
&           ,' , ',sngl(MaxPitS0*MultiplierSlope))
      END IF
      call Message_Output(Message_Info,' ')
      call Message_Output(Message_Info,iitime//'  Total Active Mass    = '       &
&           ,TotalMass*MultiplierArea)
      call Message_Output(Message_Info,iitime//'  Total Active Area    = '       &
&           ,TotalArea*MultiplierArea)
      call Message_Output(Message_Info,iitime//'  Hypsometric Integral = '       &
&           ,sngl(HypsoInt))
      call Message_Output(Message_Info,' ')
! 
!  Regions sediment accounting
! 
      IF (noregions /= 0) THEN
        call Message_Output(Message_Info                                         &
&                    ,iitime//'  Regions Sediment Accounting')
        call Message_Output(Message_Info                                         &
&                    ,iitime//'  ===========================')
        DO i=1,NoRegions
          call Message_Output(Message_Info,iitime,i,'   ',SumSed(i))
        ENDDO
        call Message_Output(Message_Info,' ')
      END IF 
! 
!  IF Soils model invoked
! 
      IF (ModeSoil /= 0) THEN
        call Message_Output(Message_Info,iitime//'  Min, Max Soil Depths = '     &
&            ,sngl(MinSoil),' , ',sngl(MaxSoil))
        call Message_Output(Message_Info,iitime//'  Min, Max Top. Index = '      &
&            ,sngl(MinTop*MultiplierArea/MultiplierSlope)                        &
&            ,' , ',sngl(MaxTop*MultiplierArea/MultiplierSlope))
        call Message_Output(Message_Info,' ')
      END IF
      RETURN
END SUBROUTINE OutStatsTable
!
!
!
subroutine CtrOut_Parameters(title,SimParameters)
  USE SiberiaTypes
  USE Setup
  USE Support
    implicit none
    TYPE(LocalParameters) :: SimParameters
    character(*) :: title
!
        call Message_Output(Message_Info,' ') 
        call Message_Output(Message_Info,trim(title(1:80)))
        call Message_Output(Message_Info,'Integer Parameters')
        call Message_Output(Message_Info,' '                                 &
&         ,SimParameters%RunTime,' , ',SimParameters%StatsTime               &
&         ,' , ',SimParameters%kx,' , ',SimParameters%ky,' , '               &
&         ,SimParameters%modeIC)
        call Message_Output(Message_Info,' '                                 &
&         ,SimParameters%TimeUp,' , ',SimParameters%ModeSolver               &
&         ,' , ',SimParameters%ModeDir,' , ',SimParameters%ModeUplift        &
&         ,' , ',SimParameters%ModeRandom)
        call Message_Output(Message_Info,' ',SimParameters%ModeErode         &
&         ,' , ',SimParameters%ModeRunoff,' , ',SimParameters%ModeChannel    &
&         ,' , ',SimParameters%ModeDP,' , ',SimParameters%ModeMC)
        call Message_Output(Message_Info,' ',SimParameters%DirReg            &
&         ,' , ',SimParameters%ModeSoil,' , ',SimParameters%idummy18         &
&         ,' , ',SimParameters%idummy19,' , ',SimParameters%idummy20)
        call Message_Output(Message_Info,' Real Parameters')
        call Message_Output(Message_Info,' ',SimParameters%dZ                &
&         ,' , ',SimParameters%dZn                                           &
&         ,' , ',SimParameters%dZHold,' , ',SimParameters%QsHold,' , '       &
&         ,SimParameters%FactMx)
        call Message_Output(Message_Info,' ',SimParameters%FRanMn            &
&         ,' , ',SimParameters%c1,' , ',SimParameters%YFix                   &
&         ,' , ',SimParameters%FRanCV,' , ',SimParameters%b3SDs)
        call Message_Output(Message_Info,' ',SimParameters%b3SDl             &
&         ,' , ',SimParameters%TAmp,' , ',SimParameters%TPeriod,' , '        &
&         ,SimParameters%TPhase,' , ',SimParameters%FRanZ)
        call Message_Output(Message_Info,' ',SimParameters%a1                &
&         ,' , ',SimParameters%m3,' , ',SimParameters%b3                     &
&         ,' , ',SimParameters%b1,' , ',SimParameters%m1)
        call Message_Output(Message_Info,' ',SimParameters%n1                &
&         ,' , ',SimParameters%Bulk,' , ',SimParameters%InitTimeStep,' , '   &
&         ,SimParameters%b5,' , ',SimParameters%n5)
        call Message_Output(Message_Info,' ',SimParameters%SInit             &
&         ,' , ',SimParameters%m5                                            &
&         ,' , ',SimParameters%YHold,' , ',SimParameters%Notch,' , '         &
&         ,SimParameters%Cover)
        call Message_Output(Message_Info,' ',SimParameters%s0max             &
&         ,' , ',SimParameters%DTime,' , ',SimParameters%OTime               &
&         ,' , ',SimParameters%GridXY,' , ',SimParameters%East)
        call Message_Output(Message_Info,' ',SimParameters%North             &
&         ,' , ',SimParameters%b6,' , ',SimParameters%m6                     &
&         ,' , ',SimParameters%b12,' , ',SimParameters%m12)
        call Message_Output(Message_Info,' ',SimParameters%SDRate            &
&         ,' , ',SimParameters%SDExp1,' , ',SimParameters%SDExp2             &
&         ,' , ',SimParameters%SMThreshold,' , ',SimParameters%SDSMWgt)
        call Message_Output(Message_Info,' ',SimParameters%rdummy46          &
&         ,' , ',SimParameters%rdummy47                                      &
&         ,' , ',SimParameters%rdummy48,' , ',SimParameters%rdummy49         &
&         ,' , ',SimParameters%rdummy50)
        call Message_Output(Message_Info,' File Parameters')
        call Message_Output(Message_Info,SimParameters%FileFactor)
        call Message_Output(Message_Info,SimParameters%FileRunoff)
        call Message_Output(Message_Info,SimParameters%FileUplift)
        call Message_Output(Message_Info,SimParameters%FileDirections)
        call Message_Output(Message_Info,SimParameters%FileMonteCarlo)
        call Message_Output(Message_Info,SimParameters%FileChannels)
        call Message_Output(Message_Info,SimParameters%FileLayers)
        call Message_Output(Message_Info,SimParameters%FileDummy8)
        call Message_Output(Message_Info,SimParameters%FileControl)
        call Message_Output(Message_Info,SimParameters%FileOthers)
        call Message_Output(Message_Info,' -- Done outputting Parameters')
end subroutine CtrOut_Parameters


END MODULE CtrOutput

