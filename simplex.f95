program simplexsolver
use global
      implicit none
   
      call io_init
      call allocate     
      call io_func
      call preForm
      call mark_s
      call init
      call init_table
      call print_table
      
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

