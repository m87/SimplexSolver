            subroutine preForm
		  use global
                  implicit none
                  if(mm .eqv. .FALSE.) func_tab=-func_tab
                  
                  do, i=0,lim_num
                  if (lim_tab(i,var_num+1) .lt. 0) lim_tab(i,:)=-lim_tab(i,:) 
            
                  end do       
            end subroutine preForm
            
            
            subroutine mark_s
                  use global
	          implicit none
	          
                  do,i=0,lim_num
                  if (lim_tab(i,var_num) .gt. 0) then
                  s_tab(i)=1
                  a_tab(i)=0
                  else
                  s_tab(i)=0
                  a_tab(i)=1
                  end if
                
                  end do               
            end subroutine mark_s
            
            
            subroutine init_table
                 use global
                 implicit none
                 allocate(simplex_table(0:y_tab,2:x_tab))
                 allocate(simplex_table_m(0:y_tab,2:x_tab))
                 allocate(simplex_table_tmp(0:y_tab,2:x_tab))
                 allocate(simplex_table_m_r(0:y_tab,2:x_tab))
                do,i=0,y_tab
                       do, j=0,x_tab
                       simplex_table(i,j) = 0.0
                       simplex_table_tmp(i,j) = 0.0
                       simplex_table_m(i,j) =0.0
                       simplex_table_m_r(i,j) = 0.0
                       end do
                 end do
                 do,i=0,lim_num-1
					do,j=0, var_num-1
					simplex_table(i,j+3) = lim_tab(i,j)
                                       
					end do
			
		
					simplex_table(i,x_tab) = lim_tab(i,var_num+1)
                                if(s_tab(i) .eq. 1) then
                                base(i)='s'
 
					simplex_table(i,var_num+3+i-a_num)=1
                                end if
                                 if(a_tab(i) .eq. 1) then 
                                  base(i)='a' 
                                simplex_table_m(i,2)=1.0
                                simplex_table(i,var_num+3+i+s_num)=1
				simplex_table_m(y_tab,var_num+3+i+s_num) = 1.0
                                 end if
				 end do
			
				 do,i=0,var_num-1
					simplex_table(lim_num,i+3) = func_tab(i)
					
				end do
					
            end subroutine init_table
