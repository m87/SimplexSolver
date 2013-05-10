subroutine allocate    
      use global
      implicit none
      allocate(base(0:lim_num))
      allocate(func_tab(0 : var_num))
      allocate(s_tab(0 : lim_num-1))
      allocate(lim_tab(0 : lim_num-1, 0:var_num+3))
end subroutine allocate
