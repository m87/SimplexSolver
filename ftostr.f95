subroutine ftostr(num, output)
      use global
      implicit none
      real(kind = 8) :: num
      character(len = 5) :: TMP
      character(len = 5) :: output
      write(TMP,"(F5.3)") num 
      output = TMP
	
end subroutine ftostr
