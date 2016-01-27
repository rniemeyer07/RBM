program two_layer_diffusion
!
!------------------------------------------------------------------------- 
!           Practice 2-layer Sream Temperature  DIFFUSION model
!                 Ryan Niemeyer, January 2016
!     based on Strzepek et al. (2015) equations 6 & 7, which is based
!      off of Chapra 1997
!
!-------------------------------------------------------------------------

implicit none

!-------------------------------------------------------------------------
!    define variables
!-------------------------------------------------------------------------

! 1-dim. real array for 10 years
real, dimension(365*10) :: flow_in,flow_out,flow_eout,flow_hout, flow_Tin
real, dimension(365*10) ::  temp_epil,temp_hypo, temp_out_tot
real, dimension(365*10) :: temp_change_ep, temp_change_hyp, energy
real, dimension(365*10) :: energy_tot, diffusion_tot, T_in_tot, T_out_tot
!
! Made hypolimnion flow = 0.0 as in Strzepek et al  JRY 1/27/2016
!
REAL, PARAMETER :: Pi = 3.1415927, prcnt_flow_epil = 1.0, prcnt_flow_hypo=0.0
real, parameter :: v_t = 1.0 !diffusion coefficient (m/day)
!    diffusion coefficient - based on Snodgrass, 1974
real, parameter :: density = 1000 !  density of water in kg / m3
real, parameter :: heat_c = 4180  !  heat capacity of water in joules/ kg * C
real  :: flow_constant
integer  :: i
real :: x1, x2, x3
!
! Defined omega, the frequency of forcing variables for consistentcy with
! standard notation
!
real  :: delta_t_sec,omega ! Frequency of forcings and time steps JRY 1/27/2016
real  :: day                       ! Day JRY 1/27/2016
!
! Define advective components for simplification JRY 1/27/2016
!
real  :: Q_epi_in,Q_epi_out,Q_hyp_in,Q_hyp_out ! Constant inflows and outflows to epilimnion and hypolimnion
real  :: rho_Cp ! Product of density and specific heat capacity of water
!
real  :: depth_total, depth_e, depth_h, width, length, volume_e_x, outflow_x
!real :: energy_x, volume_h_x, area, density, heat_c, temp_change, delta_t
real :: energy_x, volume_h_x, area, temp_change, delta_t
real  :: flow_in_hyp_x, flow_in_epi_x, flow_out_epi_x, flow_out_hyp_x
real  :: epix, hypox, dif_epi_x, dif_hyp_x, x, flow_epi_x, flow_hyp_x
real  :: residence
!
depth_total = 30
width = 200
length = 17000
area = width*length
delta_t = 1 ! time is days,  assumes all units in equations are in days
!
! Add delta_t_sec to simplify JRY 1/27/2016
!
delta_t_sec = 84600.
!
!-------------------------------------------------------------------------
!     generate flow and energey temporal variables
!-------------------------------------------------------------------------

! --------------------------- flow -------------------------

! --------- constant flow paramaeter -------
! constant  to change day of
!  flow to go "up and down" with sin wave
! with 0 and 365 being "0" point
!flow_constant = 365/(2*Pi)
!
! Replace with omega JRY 1/27/2016
!
omega = 2.*PI/365.
!
! generates flow eacy day as a sin wave with the peak flow on April 1
! at 90000 cfs, and lowest point is 30000 cfs on October 1
! days are calendar year (day = 1 = January 1)
!
! Initialize some variables JRY 1/27/2016
!
width = 200
length = 17000
area = width*length
delta_t = 1 ! time is days,  assumes all units in equations are in days
!
! Add delta_t_sec to simplify JRY 1/27/2016
!
delta_t_sec = 84600.
depth_total = 20
depth_e = 5
depth_h = 15
volume_e_x = area*depth_e
write(*,*) 'volume_e_x ',volume_e_x
volume_h_x = area*depth_h
write(*,*) 'volume_h_x ',volume_h_x
rho_Cp = density*heat_c 
!
! Make outflows a constant for this execise JRY 1/27/2016. 
! Ultimately, there should be a statement or a function here 
! to read the flows from the input file
! created by Yixin as a result of the work she is doing.
!
!
! Set epilimnion outflow to zero, as in Strzepek et al JRY 1/27/2016.
! It really isn't clear what Strzepek et al are trying to do here
! so I've made some assumptions.
!
   Q_epi_in = 360. ! Epilimnion inflow in cfs
   Q_epi_in = (Q_epi_in/35.315)   ! converts cfs to m3/sec
   residence = (volume_e_x/Q_epi_in)/86400.
   write(*,*) 'Residence ',residence
   Q_epi_out = 0. ! Epilimnion outflow in cfs
   Q_epi_out = (Q_epi_out/35.315)   ! converts cfs to m3/sec
!
   Q_hyp_in = 0.       ! Hypolimnion inlow in cfs
   Q_hyp_in = (Q_hyp_in/35.315)   ! converts cfs to m3/sec
   Q_hyp_out = Q_epi_in       ! Hypolimnion outlow in cfs
!
! Set vertical flow (Qout in Strzepek et al, I think. It's not clear) 
! Nevertheless, it's not used in the diffusion only model JRY 1/27/2016
!
!   Q_vert = Q_epi_in
!
!
!
! Initialization of variables moved to before day loop starts JRY 1/27/2016
!
temp_epil(1) = 5 ! starting epilimnion temperature at 5 C
temp_hypo(1) = 5 ! starting hypolimnion temperature at 5 C

! start at 2, because need to have an epil and hypo temperature to start out
do  i=2,365
!
! Define the day
!
  day = i
!
! Do everything in one loop and simplify JRY 1/27/2016
!
! Simplify and clarify JRY 1/27/2016
!
    flow_Tin(i) = 15.- 10.*cos(omega*day)
!
! Simpily and clarify (NOTE: This formulation needs to be changed)
!
    energy(i)= 60. - 120.*cos(omega*day)
! 
  ! gets net energy (positive downward) to vary from:
  ! " +0.5)*120":  -60 to 180 W/m2 - it will be this one JRY 1/27/2016
  ! units are  W/m2 or Joules/m2 * sec
! Convert to Joules/m2/day JRY 1/27/2016
!
    energy(i) = energy(i)*delta_t_sec 

! calculate incoming net energy (J/m2*day) to epilimnion
!
  energy_x = energy(i)*area/(volume_e_x*rho_Cp)

!
  ! calculate energy flow (using temperature as surrogate) change due to diffusion
  ! NOTE: don't need to multiply by heat capacity or density of water because
  !       energy component is divided by those two
  ! Why not do it? JRY 1/27/2016
!
  dif_epi_x  = v_t * area * (temp_hypo(i-1) - temp_epil(i-1))/volume_e_x
  dif_hyp_x  = v_t * area * (temp_epil(i-1) - temp_hypo(i-1))/volume_h_x
!
! Energy flow by transport (this is the advection part of Strzepek et al)
!
! Epilimnion
!
  flow_in_epi_x = Q_epi_in*flow_Tin(i)/volume_e_x 
!
! Hypolimnion
!
  flow_out_hyp_x = Q_hyp_out*temp_hypo(i-1)/volume_h_x

  ! calculate change in EPILIMNION  temperature (celsius)
!
! Simplify JRY 1/27/2016
!
! Epilimnion
!
  temp_epil(i) = (energy_x + flow_in_epi_x + dif_epi_x)*delta_t +temp_epil(i-1)
!
! Hypolimnion
!
  temp_hypo(i)= (-flow_out_hyp_x +dif_hyp_x)*delta_t + temp_hypo(i-1)
!
! write some stuff
!
write(10,*) day,temp_epil(i),temp_hypo(i)
write(20,*) day,energy_x,flow_in_epi_x,dif_epi_x,flow_out_hyp_x,dif_hyp_x
!
end  do
 
end program two_layer_diffusion
