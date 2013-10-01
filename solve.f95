subroutine solve
      use global
      implicit none
      
      do
      call check()
      if(condition .eqv. .TRUE.)  exit
      

      end do
	
	

end subroutine solve


function max_s
      use global
      Integer(kind = 4):: local_i
      real(kind = 8):: max_local, max_s
      max_local = 0.0
      do, local_i=2,x_tab-1
      if(simplex_table(y_tab,local_i) .gt. max_local) max_local = simplex_table(y_tab, local_i)  
      end do
      max_s=max_local
end function max_s
