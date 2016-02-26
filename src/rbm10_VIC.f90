!
      PROGRAM RBM10_VIC
!
!     Dynamic river basin model for simulating water quality in
!     branching river systems with freely-flowing river segments. 
!
!     This version uses Reverse Particle Tracking in the Lagrangian
!     mode and Lagrangian interpolation in the Eulerian mode.
!
!     This version of the model software has the following limited features:
!     
!     1. Hydrologic and meteorologic forcings are from a direct access file
!        prepared by the routing program.  Hydraulic parameters are estimated
!        by the routing program using Leopold relationships describing depth
!        and speed as a function of flow.
!
!     2. Output is written to Unit 20 (specified in the command line for executing
!        RBM) and includes the simulated temperatures for the first computational cell
!        in each segment in the same order as that of the *.network file and at the 
!        same time step.  
!
!     Topology and routing is set up to be consistent with output
!     from the Variable Infiltration Capacity (VIC) model developed by the
!     Land Surface Hydrology Group at the University of Washington.
!     Model details are described in
!    
!     Yearsley, J. (2012), A grid-based approach for simulating stream temperature,
!     Water Resour. Res., 48, W03506, doi:10.1029/2011WR011515
!
!     For additional information contact:
!
!     John Yearsley
!     Land Surface Hydrology Group
!     Dept. of Civil and Environmental Engineering
!     Box 352700
!     University of Washington
!     Seattle, Washington
!     98195-2700
!
use BGIN
use SYSTM
!
implicit none
!
!
character (len=200 ):: inPrefix
character (len=200 ):: outPrefix
character (len=200 ):: flow_file
character (len=200 ):: heat_file
character (len=200 ):: net_file
character (len=200 ):: param_file
character (len=200 ):: spatial_file
character (len=8)   :: start_data,end_data     
!
! Source files
!
character (len=200 ):: chloride_file
character (len=200 ):: thermal_file
character (len=200 ):: cl_background
character (len=200 ):: cl_headwaters
!
! Output files
!
character (len=200 ):: temp_out_file
character (len=200 ):: cl_out_file
!
integer iargc
integer numarg

!
! Command line input
!
numarg = iargc ( )
if (numarg .lt. 2) then
  write (*,*) 'Too few arguments were given'
  write (*,*) ' '
  write (*,*) 'First:  Location and prefix of input files'
  write (*,*) '        (networkfile and parameterfile)'
  write (*,*) 'Second: Location and prefix of output files'
  write (*,*) ' '
  write (*,*) 'eg: $ <program-name> ./input/Salmon_0.50 ./output/Salmon_Test'
  write (*,*) ' '
  stop
end if
call getarg ( 1, inPrefix )
call getarg ( 2, outPrefix )
!
! Identify input/output files
!
! Input files
!
net_file      = TRIM(inPrefix)//'_Network'
open(90,file=net_file,status='old')
!
param_file    = TRIM(inPrefix)//'_Parameters'
open(30,file=param_file,status='old')
!
chloride_file = TRIM(inPrefix)//'_Cl_PointSource'
open(40,file=chloride_file,status='old')
!
! Chloride background and headwaters files
!
cl_background = TRIM(inPrefix)//'_Cl_background'
open(45,file=cl_background,status='old')
!
! Thermal file
!
thermal_file  = TRIM(inPrefix)//'_T_PointSource'
open(50,file=thermal_file,status='old')
!
! Output files
!
cl_out_file = TRIM(outPrefix)//'.Chloride'
open(20,file=cl_out_file,status='unknown')
!
spatial_file  = TRIM(outPrefix)//'.Spat'
open(22,file=spatial_file,status='unknown')
!
temp_out_file     = TRIM(outPrefix)//'.Temp'
open(25,file=temp_out_file,status='unknown')
!
! Output files
!
write(*,*) 'Spatial file: ',spatial_file         
write(*,*) 'Network file    : ',net_file
write(*,*) 'Parameter file  : ',param_file
!
write(*,*) 'Chloride file: ',cl_out_file
write(*,*) 'Temperature file: ',temp_out_file
!
OPEN(UNIT=90,FILE=TRIM(net_file),STATUS='OLD')
!
!     Read header information from control file
!
read(90,*)
read(90,'(A)') flow_file
!
!     Open file with hydrologic data
!
open(unit=35,FILE=TRIM(flow_file) ,FORM='FORMATTED',ACCESS='DIRECT' ,RECL=60,STATUS='old')
!
!
read(90,'(A)') heat_file
!
!     Open file with meteorologic data
!     
open(unit=36,FILE=TRIM(heat_file) ,FORM='FORMATTED',ACCESS='DIRECT' ,RECL=50,STATUS='old')
!
!     Call systems programs to get started
!
!     SUBROUTINE BEGIN reads control file, sets up topology and
!     important properties of reaches
!
write(*,*) 'Calling BEGIN'
!
!     SUBROUTINE BEGIN reads in river system information from the NETWORK file
!
CALL BEGIN(param_file, spatial_file)
!
!     SUBROUTINE SYSTMM performs the simulations
!
CALL SYSTMM 
!
!     Close files after simulation is complete
!
write(*,*) ' Closing files after simulation'

CLOSE(35)
CLOSE(36)
CLOSE(90)
STOP
END PROGRAM RBM10_VIC
