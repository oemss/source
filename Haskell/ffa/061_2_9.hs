   --{S->aS|aA,A->bAa|ba}
   s "" = False
   s ('a':w)      = s w 
   s ('b':w)      = s2 (init ('b':w)) [last w]
   s _            = False
   s2 "b" "a"      = True 
   s2 ('b':w) "a" = s2 (init w) [last w]
   s2 _ _         = False
   
   test = s "aabbbbaaaa" && not(s "") && s "aba" && 
          not(s "a") && not (s "ab") && 
		  s "aaaaaaaaaaaaaaaabbbaaa"