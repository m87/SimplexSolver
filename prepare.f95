            subroutine preForm
		  use global
                  implicit none
                  if(mm .eq. 'min') func_tab=-func_tab
                  
                  do, i=0,lim_num
                  if (lim_tab(i,var_num+2) .lt. 0) lim_tab(i,:)=-lim_tab(i,:) 
            
                  end do       
            end subroutine preForm
            
            
            subroutine mark_s
                  use global
	          implicit none
	        
	          
                  do,i=0,lim_num
                  if (lim_tab(i,var_num+1) .gt. 0) then
                  s_tab(i)=1
                  else
                  s_tab(i)=0
                  end if
                
                  end do               
            
            
            
            end subroutine mark_s
            

            
            
            subroutine init_table
                 use global
                 implicit none
                 allocate(simplex_table(0:y_tab,2:x_tab))
                 do,i=0,y_tab
                       do, j=0,x_tab
                       simplex_table(i,j) = 0.0
                       end do
                 end do
            
            end subroutine init_table