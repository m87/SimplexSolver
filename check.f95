subroutine check
     use global
     implicit none
     integer(kind =4):: local_i
     condition = .TRUE.
     do, local_i=2,x_tab-1
     if(simplex_table(y_tab,local_i) .gt. 0) condition = .TRUE.
       
     
     end do
     
     
end subroutine check
