module global
      implicit none

      integer(kind =4) :: var_num, lim_num
      real(kind =8),dimension(:,:),allocatable :: simplex_table
      character(len =2048), dimension(:),allocatable :: form_tab
      real(kind=8) , dimension(:),allocatable :: func_tab 
      real(kind=8), dimension(:,:),allocatable :: lim_tab
      character(len =3) :: mm
      character(len=2), dimension(:),allocatable :: sig_tab
end module global 

program simplexsolver
use global

      implicit none
      integer(kind=4) :: i 
      integer(kind=4) :: j
      character(len =1) :: sig
      
      write(*,*) 'Co robimy: '
      read(*,*) mm
      

      write(*,*) 'Liczba ograniczen: '
      read(*,*) lim_num
      
      
      
      allocate(form_tab(0 : lim_num))
      
      write(*,*) 'Podaj funkcje '
      read(*,*) form_tab(0)
      write(*,*) 'Podaj ograniczenia '
      read(*,*) (form_tab(i), i=1,lim_num)
      

      
      
      
!       allocate(simplex_table(0:30,0:30))





stop

      contains
      
            subroutine limits
            use global
                  implicit none
                  integer(kind = 4) :: i,j
                  
                  
                  
                  
                  
          
            end subroutine limits
      
!             subroutine getLimit
!                   implicit none
!                   integer(kind = 8), intent(in) limit_number
!       
!                   write(*,*) 'Liczba ograniczen: '
!                   read(*,*) limit_number
!       
!       
!             end subroutine getLimit
!       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 0 = 
! 1 <
! 2 >
! 3 <=
! 4 >=
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 


end program simplexsolver

