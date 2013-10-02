subroutine solve
      use global
      implicit none
      Integer(kind=4) :: max_w, max_s,min_d, min_w
      do
      !call check()
      !if(condition .eqv. .TRUE.)  exit
      max_w = max_s()
      min_w = min_d(max_w)
      write(*,*) max_w
      write(*,*) min_w 
    
      write(*,*) simplex_table(min_w,max_w)
 
      if(min_w .eq. -1) then
      write(*,*) "Nieograniczone rozwiązanie"
      exit
      end if
      call check()
       if(condition .eqv. .TRUE.)  exit

   
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
      RETURN
end function max_s


function min_d(x)
      use global 
      implicit none
      integer(kind = 4) :: local_i, min_d, max_i,x
      real(kind=8):: max_local, temp
      max_local = simplex_table(0,x)/simplex_table(y_tab,x)
       do, local_i=0,y_tab-1
	temp = simplex_table(local_i,x)/simplex_table(y_tab,x)
        if(temp .le. 0) continue
	if(((temp .lt. max_local).OR. (max_local .le. 0 )).AND. (temp .gt. 0)) then
              max_local = temp
              max_i=local_i
       end if 
       end do
       if(max_local .le. 0) then
       min_d = -1
       else
       min_d=max_i
       end if
       RETURN

end function min_d


