subroutine itostr(num, output)                                 
         use global
          implicit none
          integer(kind = 4) :: num
          character(len = 5) :: TMP
          character(len = 5) :: output
          write(TMP,"(i5)") num 
          output = TMP
end subroutine itostr
                                                        
                                 
