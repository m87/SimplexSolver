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
                    ! call ftostr(simplex_table(3,0),base(0))
                     write(*,"(F10.3)",advance="no") 
                   end do
                    do,i=1,a_num
                                       
                    write(*,"(a8,i2,a)",advance="no") 'a',i,' |'
                   
                     write(*,"(F10.3)",advance="no") 
                    end do
                      
                     write(*,"(a10,a)") 'RHS',' |'
                    
                         do,j=0,x_tab
                         write(*,"(a)",advance="no") '-----------+'
                         end do
                    write(*,*) ''
                   
                   do, i=0,y_tab
                         write(*,"(i10, a)",advance="no") i+1, ' |'
                         write(*,"(a10, a)",advance="no") base(i) , ' |'
                         do,j=2,x_tab
                         if ((j .eq. 2) .and. (i .eq. y_tab) ) then
							write(*,"(a10, a)",advance="no") '---', ' |'
						else
                        if(simplex_table_m(i,j) .eq. 1.0) then 
                        write(*,"(F4.1,a,F4.1,a)",advance="no") simplex_table_m(i,j),'M +',simplex_table_m_r(i,j), '|' 
                       else
                         write(*,"(F10.3, a)",advance="no") simplex_table(i,j), ' |'
                        end if

						end if
                         end do
                         write(*,*) ''
                         do,j=0,x_tab
                         write(*,"(a)",advance="no") '-----------+'
                         end do
                         write(*,*) ''
                      
                   end do 
                            
end subroutine print_table

