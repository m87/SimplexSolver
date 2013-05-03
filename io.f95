subroutine io_init      
      use global
      write(*,*) 'Co robimy: '
      read(*,*) mm
      write(*,*) 'Liczba zmiennych: '
      read(*,*) var_num

      write(*,*) 'Liczba ograniczen: '
      read(*,*) lim_num
end subroutine io_init

subroutine io_func
      use global
      write(*,*) 'Podaj funkcje '
      read(*,*) (func_tab(i),i=0,var_num)
      write(*,*) 'Podaj ograniczenia '
   
      
      
      lim_loop: do,i=0,lim_num-1
      read(*,*) (lim_tab(i,j),j=0,var_num+2)
     
      end do lim_loop
      
end subroutine io_func