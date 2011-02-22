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

MODULE LayerConstants


!  Variable Number for setting and inquiring variables (these variables
!  need to be public so that calling program can use them).

  INTEGER,PARAMETER :: Layer_Resolution=1
  INTEGER,PARAMETER :: Layer_Module_Active=2
  INTEGER,PARAMETER :: Layer_Detachment_Active=3
  INTEGER,PARAMETER :: Layer_Number_Tracers=4

  INTEGER,PARAMETER :: Layer_NoLayers=100
  INTEGER,PARAMETER :: Layer_Tracer1_Default=101
  INTEGER,PARAMETER :: Layer_Tracer2_Default=102
  INTEGER,PARAMETER :: Layer_Tracer3_Default=103
  INTEGER,PARAMETER :: Layer_Tracer4_Default=104
  INTEGER,PARAMETER :: Layer_Tracer5_Default=105
  INTEGER,PARAMETER,DIMENSION(1:5) :: Layer_Tracer_Default=(/ 101,102,103,104,105 /)

  INTEGER,PARAMETER :: Layer_Z=200
  INTEGER,PARAMETER :: Layer_Age=201
!  layer properties in the layer data structure below
  INTEGER,PARAMETER :: Layer_b1=202
  INTEGER,PARAMETER :: Layer_m1=203
  INTEGER,PARAMETER :: Layer_n1=204
  INTEGER,PARAMETER :: Layer_b3=205
  INTEGER,PARAMETER :: Layer_m3=206
!  INTEGER,PARAMETER :: Layer_n3=207
  INTEGER,PARAMETER :: Layer_dZ=208
  INTEGER,PARAMETER :: Layer_s0max=209
  INTEGER,PARAMETER :: Layer_Detachment=210
  INTEGER,PARAMETER :: Layer_Tracer1=211
  INTEGER,PARAMETER :: Layer_Tracer2=212
  INTEGER,PARAMETER :: Layer_Tracer3=213
  INTEGER,PARAMETER :: Layer_Tracer4=214
  INTEGER,PARAMETER :: Layer_Tracer5=215
  INTEGER,PARAMETER,DIMENSION(1:5) :: Layer_Tracer=(/211,212,213,214,215/)

  INTEGER,PARAMETER :: Layer_FlowAge=301
  INTEGER,PARAMETER :: Layer_Flowb1=302
  INTEGER,PARAMETER :: Layer_FlowDetachment=303
  INTEGER,PARAMETER :: Layer_FlowTracer1=304
  INTEGER,PARAMETER :: Layer_FlowTracer2=305
  INTEGER,PARAMETER :: Layer_FlowTracer3=306
  INTEGER,PARAMETER :: Layer_FlowTracer4=307
  INTEGER,PARAMETER :: Layer_FlowTracer5=308
  INTEGER,PARAMETER,DIMENSION(1:5) :: Layer_FlowTracer=(/304,305,306,307,308/)

END MODULE LayerConstants
