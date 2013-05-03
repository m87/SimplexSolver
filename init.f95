subroutine init
      use global
      s_num =0
      ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
      do,i=0,lim_num
      base(i) = '---'
      end do
      ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
      zlicz_s: do,i=0,lim_num-1
      if(s_tab(i) .eq. 1) s_num = s_num +1
      
      
      end do zlicz_s
      x_tab=3+var_num+s_num
      y_tab=lim_num   




end subroutine init