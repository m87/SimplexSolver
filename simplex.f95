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
      
      allocate(base(0:lim_num))
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
      do,i=0,lim_num
      base(i) = '---'
      end do
! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
   
   
      
      allocate(func_tab(0 : var_num))
      allocate(s_tab(0 : lim_num-1))
      
      
      write(*,*) 'Podaj funkcje '
      read(*,*) (func_tab(i),i=0,var_num)
      write(*,*) 'Podaj ograniczenia '
   
      allocate(lim_tab(0 : lim_num-1, 0:var_num+2))
      
      lim_loop: do,i=0,lim_num-1
      read(*,*) (lim_tab(i,j),j=0,var_num+2)
     
      end do lim_loop
      
      call preForm
      call mark_s

      
      zlicz_s: do,i=0,lim_num-1
      if(s_tab(i) .eq. 1) s_num = s_num +1
      
      
      end do zlicz_s
      x_tab=3+var_num+s_num
      y_tab=lim_num   
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

