subroutine solve
      use global
      implicit none
      Integer(kind=4) ::  max_s,min_d
      do
      !call check()
      !if(condition .eqv. .TRUE.)  exit
      max_w = max_s()
   
      min_w = min_d(max_w)
      base_val = simplex_table(min_w,max_w)    
      if(min_w .eq. -1) then
      write(*,*) "Nieograniczone rozwiązanie"
      exit
      end if
      call cal
      call ch_base
      call print_table
      call check()
       if(condition .eqv. .FALSE.)  exit

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

subroutine cal()
      use global
      implicit none 
      integer(kind =4 ) :: local_i,local_j,local_k
      real(kind = 8 ) :: sqr     
      
 
 
      do, local_i=0, y_tab 
            do, local_j=3, x_tab  
		 
               simplex_table_tmp(local_i,local_j) = sqr(min_w,max_w,local_i,local_j)
            end do
            simplex_table_tmp(local_i,2) = simplex_table(local_i,2)
      end do
       do, local_k=3,x_tab 
                simplex_table_tmp(min_w,local_k) = simplex_table(min_w,local_k)/base_val
    
       end do
                                                                                                                                                                  
       simplex_table(min_w,max_w) =1.0
       simplex_table_tmp(min_w,max_w) =1.0  

       simplex_table=simplex_table_tmp

end subroutine cal


subroutine ch_base
      use global
      implicit none
      integer(kind =4) :: local_i,local_j
      character(len = 5) :: no 
      if(max_w .le. 3+var_num) then
           
           call itostr(max_w-2,no)
           base(min_w) = "x" // no
           simplex_table(min_w,2) = func_tab(max_w-3)
      else
           
           call itostr(max_w-2-var_num,no)
          
           base(min_w) = "s" // no
           
           simplex_table(min_w,2) = 0.0
      end if            


end subroutine ch_base

