

module graph
       use global
       implicit none
       contains
        subroutine print_table
                  use global
                  implicit none
                  write(*,*) ''
                  write(*,*) ''
                  do,j=0,x_tab
                         write(*,"(a)",advance="no") '============'
                  end do
                   write(*,*) ''
                   write(*,"(3(a10,a))",advance="no") 'i',' |','b',' |','c',' |'
                   do,i=1,var_num
                   
                    write(*,"(a8,i2,a)",advance="no") 'x',i,' |'
                   end do
                   do,i=1,s_num
                   
                    write(*,"(a8,i2,a)",advance="no") 's',i,' |'
                   end do
                   
                    write(*,"(a10,a)") 'RHS',' |'
                    
                         do,j=0,x_tab
                         write(*,"(a)",advance="no") '-----------+'
                         end do
                    write(*,*) ''
                   
                   do, i=0,y_tab
                         write(*,"(i10, a)",advance="no") i+1, ' |'
                         write(*,"(a10, a)",advance="no") base(i), ' |'
                         do,j=2,x_tab
                         write(*,"(E10.3, a)",advance="no") simplex_table(i,j), ' |'
              
                         end do
                         write(*,*) ''
                         do,j=0,x_tab
                         write(*,"(a)",advance="no") '-----------+'
                         end do
                         write(*,*) ''
                      
                   end do 
                            
            end subroutine print_table





end module graph