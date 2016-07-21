   
   s []  = True
   s str = b [head str] (tail str) || c [head str] (tail str)
   
   b "0" "11" = True
   b _   []   = False
   b "0" w    = b [head w] (tail w)
   b "1" w    = b [head w] (tail w)
   b _   _    = False
   
   c "0" [] = True
   c "1" [] = True
   c _   [] = False
   c "0" w  = d [head w] (tail w)
   c "1" w  = c [head w] (tail w)
   c _   _  = False
   
   d "0" [] = True
   d _   [] = False
   d "0" w  = c [head w] (tail w)
   d "1" w  = d [head w] (tail w)
   d _   _  = False 
   
   test =   s "011"  && s ""  && s "111011110" 
         && not (s "11101111") &&  s "101010101010101011" 
         && s "0101010101010" && not (s "01")