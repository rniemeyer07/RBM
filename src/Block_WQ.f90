module block_wq
!
! Dimensioned and allocated water quality variables 
!
! TEMP     ! 1  Stream Temperature
! DO       ! 2  Dissolved Oxygen
! BOD      ! 3  Biochemical Oxygen Demand
! COLIF    ! 4  Coliform Bacteria
! TSS      ! 5  Total Suspended Solids
! PO4      ! 6  Orthophosphorus
! P_Org    ! 7  Organic Phosphorus
! NO2      ! 8  Nitrite
! NO3      ! 9  Nitrate
! NH4      ! 10 Ammonium
! pH       ! 11 pH
! H2CO3    ! 12 Carbonic Acid
! HCO3     ! 13 Bicarbonate
! CO3      ! 14 Carabon Trioxide
! ALK      ! 15 Alkalinity
! ALGA1    ! 16 Algae Type 1
! ALGA2    ! 17 Algae Type 2
! ZOO_1    ! 18 Zooplankton Type 1
! ZOO_2    ! 19 Zooplankton Type 2
! TDS      ! 30 Total Dissolved Solids
! 
! All water quality constituents are folded into the variable, CONST
! for purposes of the making the code more compact. The variables will
! be indexed according to the specifications, above.
!
      integer(i2b)                                   :: NO_const
      integer(i2b)                                   :: NQ_total
      integer(i2b),dimension(:),allocatable          :: CONST_table
      logical(lg)                                    :: do_TEMP = .FALSE.
!
! Rate of change of constituent
!
     real(sp), dimension(:),       allocatable       :: CONST_rate
!
! Simulated water quality constituents
!
      real(sp), dimension(:,:,:,:), allocatable      :: CONST
!
! Headwaters values of water quality constituents
!
      real(sp), dimension(:,:),     allocatable      :: CONST_head
!
! Tributare values of water quality constituents
!
      real(sp), dimension(:,:,:),   allocatable      :: CONST_trib
!
! Nonpoint source inputs of water quality constituents
!
      real(sp), dimension(:,:,:),   allocatable      :: CONST_nps
!
! Point source inputs of water quality constituents
!
      real(sp), dimension(:,:),     allocatable      :: CONST_src
! 
! Tokens for water quality constituents (see indices above)
!
      character(),dimension(20),parameter            ::               &
        WQ_Token=(/'TEMP ','DO   ','BOD  ','COLIF','TSS  ',           &
                   'PO4  ','P_Org','NO2  ','NO3  ','NH4  ',           &
                   'pH   ','H2CO3','HCO3 ','CO3  ','ALK  ',           &  
                   'ALGA1','ALGA2','ZOO1 ','ZOO_2','TDS  '/)
!
end module block_wq
