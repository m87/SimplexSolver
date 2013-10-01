subroutine solve
      use global
      implicit none
      Integer(kind=4) :: max_w, max_s
      do
      call check()
      if(condition .eqv. .TRUE.)  exit
      max_w = max_s()
      
      write(*,*) max_w
      end do
	
	

end subroutine solve


function max_s()
      use global
      implicit none
      Integer(kind = 4):: local_i, max_s, max_i
      real(kind = 8):: max_local
      max_local = 0.0
      do, local_i=2,x_tab-1
      if(simplex_table(y_tab,local_i) .gt. max_local) then
             max_local = simplex_table(y_tab, local_i) 
             max_i=local_i
      end if 
      end do
      max_s=max_i
end function max_s
