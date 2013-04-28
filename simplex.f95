module global
      implicit none

      integer(kind =4) :: var_num, lim_num
      real(kind =8),dimension(:,:),allocatable :: simplex_table
      real(kind =8), dimension(:,:),allocatable :: var_tab
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
      
      
      write(*,*) 'Co robimy: '
      read(*,*) mm
      write(*,*) 'Liczba zmiennych: '
      read(*,*) var_num

      write(*,*) 'Liczba ograniczen: '
      read(*,*) lim_num
      
      
      
      allocate(func_tab(0 : var_num))
      
      write(*,*) 'Podaj funkcje '
      read(*,*) (func_tab(i),i=0,var_num)
      write(*,*) 'Podaj ograniczenia '
   
      allocate(lim_tab(0 : lim_num-1, 0:var_num+2))
      i=0
      lim_loop: do
      read(*,*) (lim_tab(i,j),j=0,var_num+2)
      i=i+1
      if(i .eq. lim_num) exit
      end do lim_loop
      
      call preForm




stop

     contains
      

      
            subroutine preForm
                  implicit none
                  if(mm .eq. 'min') func_tab=-func_tab
                  i=0
                  do
                  if (lim_tab(i,var_num+2) .lt. 0) lim_tab(i,:)=-lim_tab(i,:) 
                  i=i+1
                  if(i .eq. lim_num) exit
                  end do       
            end subroutine preForm
            
            
! !       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 0 = 
! 1 <
! -1 >
! 10 <=
! -10 >=
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 





end program simplexsolver

