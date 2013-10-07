module global
      implicit none

      integer(kind =4) :: var_num, lim_num, s_num,a_num
      real(kind =8),dimension(:,:),allocatable :: simplex_table
      real(kind =8), dimension(:,:), allocatable :: simplex_table_m
      real(kind =8),dimension (:,:), allocatable :: simplex_table_m_r
      real(kind =8),dimension(:,:),allocatable :: simplex_table_tmp
      real(kind =8), dimension(:,:),allocatable :: var_tab
      real(kind=8) , dimension(:),allocatable :: func_tab 
      real(kind=8), dimension(:,:),allocatable :: lim_tab
      integer(kind=1) ,dimension(:), allocatable :: s_tab
      integer(kind=1) , dimension(:), allocatable :: a_tab
      logical :: mm
      character(len=6), dimension(:),allocatable :: base
      integer(kind=4) :: i 
      integer(kind=4) :: j
      integer(kind=4) :: x_tab, y_tab, min_w, max_w
      real(kind =8) :: a_ij,a_ik,a_lj,sqr_result, base_val
      logical:: condition;
      
      
end module global 
