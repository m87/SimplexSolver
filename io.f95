subroutine io_init      
      use global
      implicit none
     ! write(*,*) 'Co robimy: '
      read(*,*) mm
     ! write(*,*) 'Liczba zmiennych: '
      read(*,*) var_num

      !write(*,*) 'Liczba ograniczen: '
      read(*,*) lim_num
end subroutine io_init

subroutine io_func
      use global
      implicit none
      !write(*,*) 'Podaj funkcje '
      !formula parser here
      read(*,*) (func_tab(i),i=0,var_num)
      !write(*,*) 'Podaj ograniczenia '
   
      
      !fph
      lim_loop: do,i=0,lim_num-1
      read(*,*) (lim_tab(i,j),j=0,var_num+1)
      end do lim_loop

      

      
end subroutine io_func
