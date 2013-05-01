module global
      implicit none

      integer(kind =4) :: var_num, lim_num, s_num
      real(kind =8),dimension(:,:),allocatable :: simplex_table
      real(kind =8), dimension(:,:),allocatable :: var_tab
      real(kind=8) , dimension(:),allocatable :: func_tab 
      real(kind=8), dimension(:,:),allocatable :: lim_tab
      integer(kind=1) ,dimension(:), allocatable :: s_tab
      character(len =3) :: mm
      character(len=2), dimension(:),allocatable :: sig_tab
      integer(kind=4) :: i 
      integer(kind=4) :: j
      integer(kind=4) :: x_tab, y_tab

end module global 


program simplexsolver
use global

      implicit none

      s_num =0
 
      write(*,*) 'Co robimy: '
      read(*,*) mm
      write(*,*) 'Liczba zmiennych: '
      read(*,*) var_num

      write(*,*) 'Liczba ograniczen: '
      read(*,*) lim_num
      
      x_tab=4+var_num+s_num
      y_tab=lim_num      
      
      allocate(func_tab(0 : var_num))
      allocate(s_tab(0 : lim_num-1))
      
      
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
      call mark_s

      i=0
      zlicz_s: do
      if(s_tab(i) .eq. 1) s_num = s_num +1
      
      i=i+1
      if(i .eq. lim_num) exit
      end do zlicz_s
      

      allocate(simplex_table(0:y_tab,0:x_tab))
           call print_table
 
stop

     contains
      

      
            subroutine preForm
		  use global
                  implicit none
                  if(mm .eq. 'min') func_tab=-func_tab
                  i=0
                  do
                  if (lim_tab(i,var_num+2) .lt. 0) lim_tab(i,:)=-lim_tab(i,:) 
                  i=i+1
                  if(i .eq. lim_num) exit
                  end do       
            end subroutine preForm
            
            
            subroutine mark_s
                  use global
	          implicit none
	          i=0
	          
                  do
                  if (lim_tab(i,var_num+1) .gt. 0) then
                  s_tab(i)=1
                  else
                  s_tab(i)=0
                  end if
                  i=i+1
                  if(i .eq. lim_num) exit
                  end do               
            
            
            
            end subroutine mark_s
            
            subroutine print_table
                  use global
                  implicit none
       
                   do, i=0,y_tab
                         do,j=0,x_tab
                         write(*,"(E10.3)",advance="no") simplex_table(i,j)
              
                         end do
                         write(*,*) ''
             
                   end do 
                            
            end subroutine
            
            
! !       
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 0 = 
! 1 <
! -1 >
! 10 <=
! -10 >=
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 





end program simplexsolver

