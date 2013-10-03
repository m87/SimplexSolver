
function sqr(base_y, base_x, target_y, target_x)
     use global
     implicit none
     integer(kind = 4) base_y,base_x,target_y,target_x
     real(kind = 8) sqr, temp
    ! if((base_y .eq. target_y) .AND. (base_x .eq. target_x)) then
    !       simplex_table_tmp(base_y,base_x) = 1.0
            
    ! else
     temp=(simplex_table(base_y,target_x)/base_val)
     sqr= simplex_table(target_y,target_x) - temp*simplex_table(target_y,base_x)
    ! end if

end function sqr
