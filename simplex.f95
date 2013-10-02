program simplexsolver
use global
      implicit none
      
      condition = .TRUE.
      call io_init
      call allocate     
      call io_func
      call preForm
      call mark_s
      call init
      call init_table
      call print_table
      call solve  
stop



      
            
            
! !       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 0 = 
! 1 <
! -1 >
! 10 <=
! -10 >=
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 





end program simplexsolver

