   s ""      = False
   s ('a':w) = a w
   s _       = False
   a ""      = False
   a ('a':w) = a w
   a ('b':w) = b w
   a _       = False
   b ""      = True
   b ('b':w) = b w
   b _       = False
   
   
   test = s "aaaaabbbbbbbb" && s "aaabb" && s "ab" && not (s "a" || s "b")   