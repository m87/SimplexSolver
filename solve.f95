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
!      write(*,*) simplex_table(:,:)    
      write(*,*) simplex_table(min_w,max_w)
      write(*,*) base(:) 
      if(min_w .eq. -1) then
      write(*,*) "Nieograniczone rozwiązanie"
      exit
      end if
      call check()
       if(condition .eqv. .TRUE.)  exit

!!!!BASE REFr   
      end do
	
	

end subroutine solve


function max_s()
      use global
      implicit none
      Integer(kind = 4):: local_i, max_s, max_i
      real(kind = 8):: max_local
      max_local = 0.0
      max_i = -1
      do, local_i=2,x_tab-1
      if(simplex_table(y_tab,local_i) .ge. max_local) then
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
      max_local =  -1.0
      max_i =-1
       do, local_i=0,y_tab-1
	if (simplex_table(local_i,x) .le. 0 ) continue
	temp = simplex_table(local_i,x_tab)/simplex_table(local_i,x)
	if(((temp .le. max_local).OR. (max_local .lt. 0 )).AND. (temp .ge. 0)) then
              max_local = temp
              max_i=local_i
       end if 
       end do
       if(max_local .lt. 0) then
       min_d = -1
       else
       min_d=max_i
       end if
       RETURN

end function min_d


subroutine ch_base(x,y)
      integer(kind =4) x,y


end subroutine ch_base

