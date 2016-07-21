   import List
   -- Запуск функции
   monkey str = check str
   -- Алфавит
   alphabet = "abd#"
   -- Функция проверят из каких букв состоит слово
   check str | not (((nub str) \\ alphabet) == []) = False
             | True = myfold (map (\x -> word x) (test str [[]] []))
   -- Список всех слогов
   allwords = ["ba","da","bab","dab","bad","dad","aba","ada","ab","ad"]
   -- Функция возвращает список всех слов
   test :: [Char]->[[Char]]->[Char]->[[Char]]
   test x new new2 | x == [] = init (new2:new)
                   | head x == '#' = test (tail x) (new2:new) [] 
                   | True = test (tail x) new (new2++[head x])
   -- Функция возвращает возможно ли составить слово(str) из слогов(allwords)               
   word str | inlist str = True
            | length str <=5 = False
            | domyjob str 2 1 2                          = True
            | domyjob str 2 4 3                          = True
            | domyjob str 3 2 2                          = True
            | domyjob str 3 3 3                          = True
            | True                                       = False
   -- Функция возвращает два/три первых элемента списка  
   substr str 2 = [head str]++[head (tail str)]
   substr str _ = [head str]++[head (tail str)]++[head (tail (tail str))]
   -- Функция возвращает два/три последних элемента списка
   endsubstr str 2 = [last (init str)]++[last str] 
   endsubstr str _ = [last (init (init str))]++[last (init str)]++[last str]
   -- Функция возвращает список без первых двух/трех и последних двух/трех элементов списка                
   cut str 1 = (tail (tail (init (init str)))) 
   cut str 2 = (tail (tail (tail (init (init str)))))
   cut str 3 = (tail (tail (tail (init (init (init str))))))
   cut str _ = (tail (tail (init (init (init str))))) 
   -- Функция определят входит ли слог(str) в список слогов(allwords)
   inlist el = elem el allwords
   -- Вспомогательная функции для разделения слова 
   -- str - строка
   -- ind1 - количество отрезаемых символов начала списка
   -- ind2 - переменная для определения длины "середины"
   -- ind3 - количество отрезаемых символов конца списка  
   domyjob :: [Char]->Integer->Integer->Integer->Bool
   domyjob str ind1 ind2 ind3 =  word (substr str ind1) 
                          && word (endsubstr str ind3) && word (cut str ind2)
   -- Функция возвращает результат конъюнкции элементов списка типа Bool                       
   myfold [True]   = True
   myfold [False]  = False
   myfold (x:xs) = x && myfold xs
   
   -- Слова обезьян
   monk = [monkey "ba#ababadada#bad#dabbada",monkey "abdabaadab#ada",monkey "dad#ad#abaadad#badadbaad"]