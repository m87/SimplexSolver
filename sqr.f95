
function sqr(base_y, base_x, target_y, target_x)
     use global
     implicit none
     integer(kind = 4) base_y,base_x,target_y,target_x
     real(kind = 8) sqr, temp
     temp=(simplex_table(base_y,target_x)/simplex_table(base_y,base_x))
     sqr= simplex_table(target_y,target_x) - temp*simplex_table(target_y,base_x)


end function sqr
