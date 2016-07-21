   import List
   soglas = "qwrtpsdfghjklzxcvbnmy"
   glas   = "eioa"
   u      = "u"
   y      = "y"
   pig str = pig1 str ""
   pig1 str newstr
        | not (((nub str) \\ (soglas++glas++u++y)) == []) 
                                        = "["++(intersperse ' ' 
                                          ((nub str) \\ (soglas++glas++u++y)))
                                          ++"]" ++ " not supported"
        | length str < 1                = "String length is less than 1"
        | length str == 1               = str
        | elem (head str) y && 
          elem (head (tail str)) glas = str++"yay"
        | elem (head str) soglas        = sog (tail str) (newstr++[head str])
        | elem (head str) (glas++u)     = str++(newstr++"yay")
        
   
   sog  str newstr  
        | elem (head str) soglas = sog (tail str) (newstr++[head str])
        | elem (head str) glas   = str++(newstr++"ay")
        | elem (head str) u && last newstr == 'q' 
                                 = sog (tail str) (newstr++[head str])
	| elem (head str) u      = str++(newstr++"ay")
   test = pig "pig" == "igpay" && pig "qweqweqiyit" == "eqweqiyitqway"
          && pig "ertqwr" == "ertqwryay" && pig "yat" == "yatyay" &&
          pig "ququip" == "ipququay" && pig "a" == "a"

