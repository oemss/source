    --{S->Aa,An;S->Bb,Bk;S->c,c;A->aS,nS;B->bS,mS}
    
    g "" = "Empty"
    g str = s (init str) [last str] "" "" str
    
    s w "a" str1 str2 str = a [head w] (tail w) ("n"++str1) str2 str
    s w "b" str1 str2 str = b [head w] (tail w) ("k"++str1) str2 str
    s _ "c" str1 str2 str | length (str2++"c"++str1) == 
                            length str = str2++"c"++str1
                          | True       = "Warning length not equal : "
                                         ++str2++"c"++str1
    s _ _   str1 str2 _   = str2++"(Symbol not found)"++str1 
    
    a "a" w str1 str2 str = s (init w) [last w] str1 (str2++"n") str
    a _   w str1 str2 str = s (init w) [last w] str1 (str2++
                                       "(Symbol not found)") str
    
    b "b" w str1 str2 str = s (init w) [last w] str1 (str2++"m") str
    b _   w str1 str2 str = s (init w) [last w] str1 (str2++
                                       "(Symbol not found)") str
    
    
    test = g "aabbcbbaa" == "nnmmckknn" && g "aca" == "ncn" && g "c" == "c"
           && g "aabbcbaa" == "Warning length not equal : nnmcknn"