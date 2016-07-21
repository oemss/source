    g "" = ""
    g str = s (init str) [last str] ""
    
    s "" _ _    = "Error"
    s w "a" str = s (init w) [last w] (str++"k")
    s w "b" str = a (init w) [last w] ("m"++str)
    s _ _ str   = "(Symbol not found)"++str
    
    a "" "a" str = "n"++str
    a w "a" str  = a (init w) [last w] ("k"++str)
    a _ _ str    = "(Symbol not found)"++str
    
    test = g "abaaaaaa" == "nmkkkkkk" && g "abaa" == "nmkk" && g "aaaaaabaaaa" == "nkkkkkmkkkk"