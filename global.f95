module global
      implicit none

      integer(kind =4) :: var_num, lim_num, s_num
      real(kind =8),dimension(:,:),allocatable :: simplex_table
      real(kind =8), dimension(:,:),allocatable :: var_tab
      real(kind=8) , dimension(:),allocatable :: func_tab 
      real(kind=8), dimension(:,:),allocatable :: lim_tab
      integer(kind=1) ,dimension(:), allocatable :: s_tab
      character(len =3) :: mm
      character(len=5), dimension(:),allocatable :: base
      integer(kind=4) :: i 
      integer(kind=4) :: j
      integer(kind=4) :: x_tab, y_tab

end module global 
