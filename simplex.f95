program simplexsolver
      implicit none
      !real(kind =8),public,dimension(),allocatable
      integer(kind = 8), public :: limit_number
      
      





stop
      contains

      subroutine getFormula
      implicit none
      
          
      end subroutine getFormula
      
      subroutine getLimit
      implicit none
      integer(kind = 8), intent(in) limit_number
      
      write(*,*) 'Liczba ograniczen: '
      read(*,*) limit_number
      
      
      end subroutine getLimit
      
      
end program simplexsolver

