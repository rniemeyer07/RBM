SUBROUTINE WRITE(time,nd,nr,ncell,ns,T_0,Cl_0,T_head,dbt)
!
Implicit NONE
!
integer:: nd,nr,ncell,ns 
real   :: Cl_0,T_0,T_dist
real(8):: time
real   :: T_head
real   :: dbt
!
write (25,'(f12.4,4i6,3f8.2,f9.0,f8.1,f8.4)')           &                                              
            time,nd,nr,ncell,ns,T_0,Cl_0,T_head
end SUBROUTINE WRITE
