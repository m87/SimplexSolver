subroutine init
      use global
      implicit none
      s_num =0
      a_num=0
      ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
      do,i=0,lim_num
      base(i) = '---'
      end do
      ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
      zlicz_s: do,i=0,lim_num-1
      if(s_tab(i) .eq. 1) s_num = s_num +1
      if(a_tab(i) .eq. 1) a_num = a_num +1
      
      end do zlicz_s
      x_tab=3+var_num+s_num+a_num
      y_tab=lim_num   




end subroutine init
