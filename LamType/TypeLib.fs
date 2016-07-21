namespace TypeLib2
open System
    module Term2 = 
         type Term = Var of string
                   | App of Term * Term
                   | Lam of string * Term
         type Type = Sym of string
                   | Arrow of Type * Type
                   | All of string * Type 
         type Tree = Null
                   | Branch of string * string * Tree * Tree
         type Kinds = Star
                    | Box
                    | S of String
         type Type_w = Sym_w of Kinds
                       | Arrow_w of Kinds * Kinds
         type Expr = Varw of String
                   | Appw of Expr * Expr
                   | Lamw of String * Expr * Expr
                   | Pi of String * Expr * Expr
                   | Kind of Kinds
         // Функция, возвращающая по типу tp подтип до операции "->",
         // имеющей наивысший приоритет
         //let pref tp = if List.head <> '(' then List.takeWhile (fun x -> x <> '-') tp
         //             elif  
         //-------------------------------------------------------------------------------
         // Проверка на пустоту 
         let nullc x = List.length x = 0
         let snullc x = x = ""

         //-------------------------------------------------------------------------------
         // [char] -> string
         let rec c2s s = 
            match s with
            | [] -> ""
            | _  -> String.Concat(List.head) + c2s (List.tail s)
         
         // Функция возвращает список без последнего элемента
         let rec linit str = 
             match str with
             | []       -> []
             | hd :: [] -> []
             | hd :: t1 -> hd :: linit (List.tail str)
         let lhead t = if List.length(t) = 0 then (Sym "a",Sym "a")
                       else List.head(t)
         let minit (str:string) = str.[0..(String.length str - 2)]
         let stail (str:string) = if String.length str = 1 then ""
                                  elif String.length str = 2 then String.Concat(str.[1])
                                  else str.[1..(String.length str - 1)]
         let shead (str:string) = str.[0]
         let sinit (str:string) = str.[0..(String.length str - 2)]
         let sinit2 (str:string) = if String.length str = 2 then String.Concat(shead str)
                                   else sinit str
         let slast (str:string) = str.[String.length str - 1]
         let sslast (str:string) = String.Concat(slast str)
         let rec str2list (str:string) = 
            match str with
            | "" -> []
            | _  -> str.[0] :: str2list (stail str)
         let rec fl s =
            match s with 
            | "" -> ""
            | _ -> match (shead s) with
                   | '.' -> "."
                   | _ -> String.Concat(shead s) + fl (stail s)
         let rec aft s = 
             match s with 
             | "" -> ""
             | _ -> match (shead s) with
                    | '.' -> stail s
                    | _ -> aft (stail s)
         let rec rev (str:string) = 
                 match str with
                 | "" -> ""
                 | s -> match (shead str) with
                        | '\\' -> fl str + rev (aft str)
                        | _    -> String.Concat(slast str) + rev (sinit2 str)
                 
         //-------------------------------------------------------------------------------
         // String -> Term  
         let rec finddl str = 
            match (shead str) with 
            | '.' -> ""
            | '\\' -> ""
            | ' ' -> ""
            | s -> String.Concat(s) + finddl (stail str)

         let rec retdl str =
            match (shead str) with 
            | '.' -> stail str
            | '\\' -> stail str
            | ' ' -> stail str
            | _ -> retdl (stail str)
         let rec check1 str i =
            match (i,shead str) with
            | (1,')') -> if String.length(str) = 1 then true
                         else false
            | (t,'(') -> check1 (stail str) (i+1)
            | (t,')') -> check1 (stail str) (i-1)
            | (_,s)   -> check1 (stail str) i   
         let check str = check1 str 0
         // Определение наличия аппликации
         let rec checkstr str = 
                 match str with 
                 | "" -> true
                 | _ -> if (shead str) = '(' then check str
                        else match (shead str) with 
                             | ' ' -> if (String.length str = 1) then true
                                      else false
                             | _ -> checkstr (stail str)
         // Удаляет внешние скобки
         let rec pts1 str i = 
            match (i,(shead str)) with
            | (1,')') -> ""
            | (t,'(') -> "(" + pts1 (stail str) (i+1)
            | (t,')') -> ")" + pts1 (stail str) (i-1)
            | (_,s)   -> String.Concat(s) + pts1 (stail str) i  
         let pts str = pts1 str 1   
         
         let rec pts2 str = 
            match (shead str) with
            | '(' -> pts1 (stail str) 1
            | _   -> pts2 (stail str)
         let rec fts1 str i =
            match (i,shead str) with
            | (1,')') -> str
            | (t,'(') -> fts1 (stail str) (i+1)
            | (t,')') -> fts1 (stail str) (i-1)
            | (_,s)   -> fts1 (stail str) i   
         let fts str = fts1 str 1

         let rec prefterm str = if str = "" then ""
                                else
                                    match (shead str) with
                                    | ' ' -> ""
                                    | s -> String.Concat(s) + prefterm (stail str)
         let rec postterm str = 
            match (shead str) with
            | ' ' -> stail str
            | _   -> postterm (stail str)
         let rec retskob str k = 
                 match (slast str) with
                 | ')' -> retskob (sinit str) (k+1) + ")"
                 | '(' ->
                          match k with 
                          | 1 -> ""
                          | _ -> retskob (sinit str) (k-1) + "(" 
                 | _   -> retskob (sinit str) k +  String.Concat(slast str) 
         let rec retskob0 str = retskob (sinit str) 1
         let rec lastt2 str = 
                 match (slast str) with 
                 | ' ' -> ""
                 | _ -> lastt2 (sinit str) + String.Concat(slast str) 
         let lastt str = 
             match (slast str) with 
             | ')' -> retskob0 str 
             | _ -> lastt2 str
         let rec retskob1 str k = 
                 match (slast str) with
                 | ')' -> retskob1 (sinit str) (k+1) 
                 | '(' ->
                          match k with 
                          | 1 -> sinit str
                          | _ -> retskob1 (sinit str) (k-1)
                 | _   -> retskob1 (sinit str) k 
         let rec retskob2 str = sinit (retskob1 (sinit str) 1)
         let rec plastt2 str = 
                 match (slast str) with 
                 | ' ' -> sinit str
                 | _ -> plastt2 (sinit str)
         let plastt str = 
             match (slast str) with 
             | ')' -> retskob2 str 
             | _ -> plastt2 str
         //Транслятор из строковой записи в запись в виде конструкторов
         //String->Term
         let rec sterm str =  
            match (shead str) with
            | '\\' -> Lam (finddl (stail str),sterm (retdl (stail str)))
            | '.'  -> sterm (stail str)
            | '(' -> if checkstr str then sterm (stail (sinit str))
                     else App (sterm (plastt str),sterm (lastt str))            
            | _ -> if checkstr str then Var (prefterm str)
                   else App (sterm (plastt str),sterm (lastt str)) 

         let rec preft str = 
            match str with 
            | "" -> ""
            | _ ->
                   match (shead str) with 
                   | ' ' -> ""
                   | '(' -> ""
                   | ')' -> ""
                   | s   -> String.Concat(s) + preft (stail str) 
         let rec pos str = 
            match (shead str) with
            | ' ' -> stail str
            | '(' -> stail str
            | _   -> pos (stail str)
         let rec s2term (str:string) = if str.[0..2] = "Lam" then Lam (preft str.[4..(String.length str - 1)],s2term (pts2 (pos str.[4..(String.length str - 1)])))
                                       elif str.[0..2] = "App" then App (s2term (pts str.[5..(String.length str - 1)]),s2term (pts2 (fts str.[5..(String.length str - 1)])))
                                       else Var (preft str.[4..(String.length str - 1)])

         //-------------------------------------------------------------------------------
         
         // Предикат, определяющий, имеется ли вхождение
         // строки s1 в строку s2
         let rec findstr s1 s2 = if String.length s1 > String.length s2 then false
                                 elif s1 = s2.[0..(String.length s1 - 1)] then true
                                 else findstr s1 (stail s2)


         let veryf (tp:string) = List.length (List.filter (fun x -> x = '(') (List.takeWhile (fun x -> x <> ')') (str2list tp))) = List.length (List.filter (fun x -> x = ')') (List.takeWhile (fun x -> x <> '(') (List.skipWhile (fun x -> x <> ')') (str2list tp))))
         let rec pref' tp1 tp2 n =
             let op = String.Concat(List.takeWhile (fun x -> x <> '(') (str2list tp1))
             let cl = String.Concat(List.takeWhile (fun x -> x <> ')') (str2list tp1)) 
             let nx ch = stail (String.Concat(List.skipWhile (fun x -> x <> ch) (str2list tp1)))
             match n with
             | 0 -> tp2
             | _ -> if findstr "(" tp1 && snullc op then pref' (nx '(') (tp2 + "(") (n+1)
                    elif findstr "(" tp1 && snullc cl then pref' (nx ')') (tp2 + ")") (n-1)
                    elif String.length op < String.length cl then pref' (nx '(') (tp2 + op + "(") (n+1)
                    else pref' (nx ')') (tp2 + cl + ")") (n-1)
         (*let rec pref tp =
             match tp with 
             | x when shead tp <> '(' -> String.Concat(List.takeWhile (fun x -> x <> '-') (str2list tp))
             | x when veryf (stail (sinit tp)) -> pref (stail (sinit tp))
             | _ -> pref' (stail tp) "(" 1*)
         let rec pref tp = if shead tp <> '(' then String.Concat(List.takeWhile (fun x -> x <> '-') (str2list tp))
                           elif veryf (stail (sinit tp)) then pref (stail (sinit tp))
                           else pref' (stail tp) "(" 1
         //let chectrue tp1 tp2 = 
            
         let postf tp = 
            let prTp = pref tp
            let poTp (t:string) = t.[(String.length prTp + 1)..(String.length t - 1)]
            if prTp = tp then poTp tp
            else poTp (stail tp)

        
         let rec s2type s1 = if not (findstr ">" s1) && not (findstr "forall" s1) then Sym s1
                             elif String.length s1 > 5 && s1.[0..5] = "forall" then All ((String.Concat(List.takeWhile (fun x -> x <> '.') (str2list s1.[7..(String.length s1 - 1)]))),(s2type (stail (stail (String.Concat(List.skipWhile (fun x -> x <> '.') (str2list s1)))))))
                             elif shead s1 = '(' && not (findstr ")->" s1) then s2type (sinit (stail s1))
                             else Arrow (s2type (pref s1),s2type (postf s1))
         // Перевод из строки в число
         let rec hstr2num (str:string) s = 
             match str with 
             | "" -> 0
             | _ ->
                    match str.[String.length str - 1] with
                    | '1' -> 1*s + hstr2num (minit str) (s*10)
                    | '2' -> 2*s + hstr2num (minit str) (s*10)
                    | '3' -> 3*s + hstr2num (minit str) (s*10)
                    | '4' -> 4*s + hstr2num (minit str) (s*10)
                    | '5' -> 5*s + hstr2num (minit str) (s*10)
                    | '6' -> 6*s + hstr2num (minit str) (s*10)
                    | '7' -> 7*s + hstr2num (minit str) (s*10)
                    | '8' -> 8*s + hstr2num (minit str) (s*10)
                    | '9' -> 9*s + hstr2num (minit str) (s*10)
                    | '0' -> 0*s + hstr2num (minit str) (s*10)
                    | _   -> 0  
                               
         let str2num str = hstr2num str 1
         //-------------------------------------------------------------------------------

         // Перевод из числа в строку 
         let rec num2str num = 
             match num with
             | 0 -> ""
             | _ -> match (num%10) with
                    | 1 -> num2str ((num - num%10)/10) + "1" 
                    | 2 -> num2str ((num - num%10)/10) + "2" 
                    | 3 -> num2str ((num - num%10)/10) + "3" 
                    | 4 -> num2str ((num - num%10)/10) + "4" 
                    | 5 -> num2str ((num - num%10)/10) + "5" 
                    | 6 -> num2str ((num - num%10)/10) + "6" 
                    | 7 -> num2str ((num - num%10)/10) + "7" 
                    | 8 -> num2str ((num - num%10)/10) + "8" 
                    | 9 -> num2str ((num - num%10)/10) + "9"
                    | 0 -> num2str ((num - num%10)/10) + "0"
         //-------------------------------------------------------------------------------
         let rec helprec lst s = 
             match lst with 
             | [] -> false
             | _  -> if s = List.head lst then true
                     else helprec (List.tail lst) s
         //-------------------------------------------------------------------------------
         //let (explode1:char list) =
         //        [for (c in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" -> c]
         let explode1 s =
            match s with 
            | "a" -> "b"
            | "b" -> "c"
            | "c" -> "d"
            | "d" -> "e"
            | "e" -> "f"
            | "f" -> "g"
            | "g" -> "h"
            | "h" -> "i"
            | "i" -> "j"
            | "j" -> "k"
            | "k" -> "l"
            | "l" -> "m"
            | "m" -> "n"
            | "n" -> "o"
            | "o" -> "p"
            | "p" -> "q"
            | "q" -> "r"
            | "r" -> "s"
            | "s" -> "t"
            | "t" -> "u"
            | "u" -> "v"
            | "v" -> "w"
            | "w" -> "x"
            | "x" -> "y"
            | "y" -> "z"
            | "z" -> "a1" 
            | _   -> "a"  
    
         // Следующая буква 
         //let rec hchrord lst s =
         //    match lst with
         //   | [] -> 'a'
         //    | _  -> if List.head lst = s then List.head (List.tail lst)
         //            else hchrord (List.tail lst) s 
         //----------------------------------------------------------------------
         (*let nextTp s = 
            match (String.length s) with
            | 1 -> explode1 s 
            | 0 -> "Error"
            | _ -> String.Concat(s.[0]) + num2str (str2num (String.Concat(s.[1..String.length s])) *)
         let Str s = List.ofSeq s
         let nextTp s = if String.length s = 1 then explode1 s 
                        else String.Concat(s.[0]) + num2str (str2num (s.[1..(String.length s - 1)]) + 1)

         //-------------------------------------------------------------------------------
         
         //Функция, выводящая по терму trm уравнения (ограничения) его подтипов 
         //Term -> (string * string) list
         let rec equat1 lstTp term tp symb = 
             match term with
             | Var p -> if nullc (List.filter (fun x -> p = x) (fst (List.unzip lstTp))) then failwith "Error"
                        else [(tp,snd (List.head (List.filter (fun (x,y) -> x = p) lstTp)))]
             | App (t1,t2) -> equat1 lstTp t1 (nextTp symb + "->" + tp) (nextTp symb) @ 
                              equat1 lstTp t2 (nextTp symb) ((nextTp symb) + "1")
             | Lam (p,t)   -> equat1 ((p,nextTp symb) :: lstTp) t (nextTp (nextTp symb)) (nextTp (nextTp symb)) @ 
                              [(nextTp symb + "->" + (nextTp (nextTp symb)),tp)]
         let equat trm = equat1 [] trm "a" "a"
         //-------------------------------------------------------------------------------

         // Предикат, определяющий, представляет ли запись типа символ
         let sym t = 
            match t with
            | Sym _ -> true
            | _     -> false
         //-------------------------------------------------------------------------------

         // Предикат, определяющий вхождение типа t1 в заданный тип
         let rec findTp t1 t2 = if t1 = t2 then true
                                else match t2 with
                                     | All (s1,t3) -> findTp t1 t3
                                     | Arrow (t3,t4) -> findTp t1 t3 || findTp t1 t4
                                     | _ -> false
         //-------------------------------------------------------------------------------
         // Функция, возвращающая результат подстановки вместо типа
         // t1 типа t в заданном типе t2
         //Type->Type->Type->Type
         let rec repl t1 t t2 = if t1 = t2 then t
                                else match t2 with
                                     | All (s2,t3) -> All (s2,repl t1 t t3)
                                     | Arrow (t3,t22) -> Arrow (repl t1 t t3,repl t1 t t22)
                                     | _             -> t2
         //-------------------------------------------------------------------------------

         // Функция, возвращающая результат решения уравнения на типах:
         // a = t, где тип a входит в запись типа t
         let quantif tp1 tp2 =
            match tp1 with
            | Sym a -> All (a + "'",repl (Sym a) (Sym (a + "'")) tp2) 
         //-------------------------------------------------------------------------------

         // Функция, возвращающая результат композиции подстановок, применённой к заданному типу 
         //(Type * Type) list->Type->Type
         let rec substr lstP tp = if nullc lstP then tp 
                                  else
                                       let t = snd (List.last lstP)
                                       let p = fst (List.last lstP)
                                       substr (linit lstP) (repl p t tp)
         //-------------------------------------------------------------------------------

         // Унификация двух типов с обработкой равенства типов,
         // содержащего рекурсию
         let rec unif tp1 tp2 =
            match (tp1,tp2) with
            | (Arrow (s1,s2),Arrow (t1,t2)) -> unif (substr (unif s2 t2) s1) (substr (unif s2 t2) t1) @ (unif s2 t2)
            | (Arrow (s1,s2),Sym t2) -> unif (Sym t2) (Arrow (s1,s2))
            | (All (s1,t1),All (s2,t2)) -> if s1<>s2 then [(All (s1,t1),All ((s2 + "Error!"),t2))]
                                           else unif t1 t2
            | (All (s,t),Sym t2) -> unif (Sym t2) (All (s,t))
            | (tp1,tp2) -> if tp1 = tp2 then []
                           elif sym tp1 && findTp tp1 tp2 then [(tp1,quantif tp1 tp2)]
                           else [(tp1,tp2)]
            | (_,_) -> []
        
         //-------------------------------------------------------------------------------
         let rec ar lstT = if List.length(lstT) = 1 then List.head(lstT)
                           elif List.length(lstT) = 2 then Arrow (List.head(lstT),List.last(lstT))
                           else Arrow (List.head(lstT),ar (List.tail(lstT)))
         let rec srt1 syst syst1 = 
            let tailS = List.tail(syst) 
            let aftType = List.skipWhile (fun (x,y) -> x <> fst (List.head(syst))) tailS
            let u = unif (snd (lhead(syst))) (snd (lhead(aftType)))
            let newS = List.map (fun (x,y) -> (substr u x,substr u y)) (tailS)
            if List.length(syst) = 0 then syst1
            elif List.length(syst) = 1 then syst1
            elif List.length(aftType) = 0 then srt1 tailS syst1
            else srt1 newS newS
         let srt syst = 
            let p_fst,p_snd = List.unzip(srt1 syst syst)
            substr (unif (ar p_fst) (ar p_snd)) (fst (List.last(syst)))
         let urav lstTp = 
            let rec f (lst: (string *string) list) = 
                match lst with 
                | hd :: [] -> if findstr "->" (fst hd) then "(" + fst hd + ")"
                              else fst hd
                | hd :: hdd -> if findstr "->" (fst hd) then "(" + fst hd + ")" + "->" + f hdd
                               else fst hd + "->" + f hdd
            let rec s (lst: (string *string) list) =
                match lst with
                | hd :: [] -> if findstr "->" (snd hd) then "(" + snd hd + ")"
                              else snd hd
                | hd :: hdd -> if findstr "->" (snd hd) then "(" + snd hd + ")" + "->" + s hdd
                               else snd hd + "->" + s hdd
            (f lstTp,s lstTp)
         //-------------------------------------------------------------------------------    
           
         let rec show term = 
             match term with 
             | Var x       -> x
             | Lam (x,t)   -> "\\" + x + " -> " + show t
             | App (t1,t2) ->  match (t1,t2) with
                               | (Lam (_,_),Lam (_,_)) -> "(" + show t1 + ") "  + "(" + show t2 + ")"
                               | (Lam (_,_),App (_,_)) -> "(" + show t1 + ") " + "(" + show t2 + ")"
                               | (_,App (_,_)) -> show t1 + " (" + show t2 + ")"
                               | (Lam(_,_),_) -> "(" + show t1 + ") "  + show t2
                               | (_,Lam (_,_)) -> show t1 + " " + "(" + show t2 + ")"
                               | (_,_) -> show t1 + " " + show t2
         //∀
         let rec showtype tp = 
            match tp with
            | Sym x -> x
            | All (s,t) -> "forall " + s + ". " + showtype t 
            | Arrow (Sym t1,Sym t2) -> t1 + "->" + t2
            | Arrow (t1,t2) -> "(" + showtype t1 + ")" + "->" + showtype t2
         let rec masshowtype t = 
            match t with
            | [] -> ""
            | _ ->
                  let he = List.head(t)
                  let li = List.tail(t)
                  "[" + showtype (fst he) + "," + showtype (snd he) + "]" + masshowtype li

         // Вывод [(string,string)]
         let rec shows mas =
             match mas with 
             | [] -> ""
             |  _ -> match (List.head mas) with
                     | (x,y) -> "[" + x + "," + y + "]" + shows (List.tail mas) 

         let rec showt lst =
            match lst with 
            | [] -> ""
            | _  -> match (List.head lst) with
                    | (x,y) -> "(" + (showtype x) + "," + (showtype y) + ")," + showt (List.tail lst)  
         //-------------------------------------------------------------------------------    
         //
         let rec false_equat t = 
            match t with
            | [] -> []
            | _ -> 
                   match (List.head t) with
                   | (s1,s2) -> if (findstr "->" s1) || (findstr "->" s2) then false_equat (List.tail t)
                                else [(s1,s2)] @ false_equat (List.tail t) 
         let rec true_equat t =
            match t with
            | [] -> []
            | _ -> 
                   match (List.head t) with
                   | (s1,s2) -> if (findstr "->" s1) || (findstr "->" s2) then [(s1,s2)] @ true_equat (List.tail t)
                                else true_equat (List.tail t)
         let rec c_str (s1:string) (s2:string) s3 = if String.length s1 > String.length s2 then s2
                                                    elif s1 = s2.[0..(String.length s1 - 1)] then s3 + c_str s1 (s2.[(String.length s1)..(String.length s2 - 1)]) s3 
                                                    else String.Concat(s2.[0]) + c_str s1 (stail s2) s3
         let rec change lst w o =
            match lst with 
            | [] -> []
            | _ -> 
                   match (List.head lst) with
                   | (s1,s2) -> [(c_str w s1 o,c_str w s2 o)] @ change (List.tail lst) w o
            
         let rec let_true_equat t1 t2 = 
            match t2 with 
            | [] -> t1
            | _ -> 
                    match (List.head t2) with
                    | (s1,s2) -> let_true_equat (change t1 s1 s2) (change (List.tail t2) s1 s2)
         let rec l_s2type t =
            match t with 
            | [] -> []
            | _ -> 
                    match (List.head(t)) with
                    | (s1,s2) -> [(s2type s1,s2type s2)] @ l_s2type (List.tail(t))
         let rec rev2 t = 
            match t with
            | [] -> []
            | _ ->
                    match (List.head t) with 
                    | (s1,s2) -> [(s2,s1)] @ rev2 (List.tail t)
         

         //-- Предикат, определяющий наличие свободного
         //-- вхождения слова x в терм t
         //-----------------------------
         let rec free x arg =
            match arg with 
            | Varw p -> x = p
            | Lamw (p,tp,t) -> x<>p && free x t
            | Appw (t1,t2) -> free x t1 || free x t2
            | Pi (p,t1,t2) -> free x t1 || free x t2
            | Kind t -> false
         //-- *********************************************************
         //-- Предикат, определяющий наличие вхождения слова x в терм t
         //------------------------------------------------------------
         let rec memb x arg =
            match arg with
            | Varw p -> x = p
            | Lamw (p,pt,t) -> x = p || memb x t
            | Appw (t1,t2) -> memb x t1 || memb x t2
            | Pi (p,t1,t2) -> x = p || memb x t1 || memb x t2
            | Kind t -> false

         //-- Функция, применяющая правило "этта-редукция"
         //-- к лямбда-терму
         //--------------------------------
         let rec etta arg = 
            match arg with 
            | Lamw (p,pt,Appw (t,Varw p1)) -> if p = p1 && (free p t) = false then t
                                              else Lamw (p,pt,(Appw (t,Varw p1)))
            | Lamw (p,pt,Appw (t1,t2)) -> Lamw (p,pt,Appw (etta t1,etta t2))
            | Appw (t1,t2) -> Appw (etta t1,etta t2)
            | _ -> arg
         //-- Функция, возвращающая результат применения правила
         //-- "beta-редукция"
         //-- (реализация стратегии вычислений "нормальный порядок")
         //---------------------------------------------------------
         let rec beta arg =
            let rec alpha arg2 =
                match arg2 with
                | Lamw (p,pt,t) -> if (memb (p+"1") t) then alpha (Lamw (p+"1",pt,t))
                                   else Lamw (p+"1",pt,beta (Appw (Lamw (p,pt,t),Varw (p+"1"))))
                | _ -> arg
            match arg with
            | Appw (Lamw (p,tp,Varw t1),t2) -> if (p = t1) then t2
                                               else Varw t1
            | Appw (Lamw (p,tp,Kind t1),t2) -> Kind t1
            | Appw (Lamw (p,tp,(Pi (x,a,b))),Varw s) ->
                   if (not (memb s (Pi (x,a,b)))) then Pi (s,beta (Appw (Lamw (p,tp,a),Varw s)),beta (Appw (Lamw (p,tp,b),Varw s)))
                   else Pi (x,a,b)
            | Appw (Lamw (p,tp,Pi (x,a,b)),t2) -> Pi (x,a,b)
            | Appw (Lamw (p,tp,Lamw (p1,tp1,t1)),t2) -> if (p = p1) then Lamw (p1,tp1,t1)
                                                        elif free p1 t2 = false then Lamw (p1,tp1,beta (Appw (Lamw (p,tp,t1),t2)))
                                                        else beta (Appw (Lamw (p,tp,(alpha (Lamw (p1,tp1,t1)))),t2))
            | Appw (Lamw (p,tp,Appw (t1,t1')),t2) -> Appw (beta (Appw (etta (Lamw (p,tp,t1)),t2)),
                                                           beta (Appw (etta (Lamw (p,tp,t1')),t2)))
            | Appw (t1,t2) -> Appw (beta t1,beta t2)
            | Lamw (p,tp,t) -> Lamw (p,tp,beta t)
            | _ -> arg
         //-- ********************************************
         //-- Предикат, проверяющий допустимость контекста
         //-----------------------------------------------
         //-- Функция, применяющая beta-редукцию к лямбда-терму до по-
         //-- лучения лямбда-терма, обладающего нормальной формой 
         //------------------------------------------------------
         //btred term | b==term = b
         //           | True    = btred b
         //   where b = beta term
         //-- *****************************************
         let rec btred trm = if beta trm = trm then beta trm
                             else btred (beta trm)          
         //-- Функция поиска предметной переменной
         //-- в заданном контексте
         //--------------------------------------------
         let findX g x = List.skipWhile (fun (t,typ) -> t <> x) g
         //-- ******************************************
         //-- Функция, возвращающая тип псевдовыражения
         //-- в заданном контексте
         //----------------------------------------------------
         let rec ty g term =
            let ok arg = 
                match arg with
                | [] -> true
                | _ -> let typ = ty (linit(arg)) (snd(List.last(arg)))
                       if(typ = Kind Box || typ = Kind Star) then true
                       else false
            match term with
            | Varw x -> 
                        let p = findX g (Varw x)
                        match (ok g && (not (nullc p))) with
                        | true -> snd (List.head p)
                        | _ -> failwith "Error!"
            | Kind Star ->
                           match ok g with
                           | true -> Kind Box
                           | _ -> failwith "Error!"
            | Appw (m,n) -> 
                           let c = ty g m
                           let d = ty g n
                           let rezRed = btred c
                           let var (Pi (x,a,b)) = x
                           let pref arg = 
                                match arg with
                                | (Pi (x,a,b)) -> a
                                | _ -> failwith "Error!"
                           let post arg = 
                                match arg with
                                | (Pi (x,a,b)) -> b
                                | _ -> failwith "Error!"
                           if (btred (pref rezRed) = d) then beta (Appw (Lamw (var rezRed,Kind Star,post rezRed),n))
                           else failwith "Error!"

            | Lamw (x,a,m) -> 
                             let b = ty (g @ [(Varw x,a)]) m
                             let typ = ty g (Pi (x,a,b))
                             if (typ = Kind Star || typ = Kind Box) then Pi (x,a,b) 
                             else failwith "Error!"  
            | Pi (x,a,b) ->
                           let typ = ty (g @ [(Varw x,a)]) b
                           if ((ty g a = Kind Star && typ = Kind Star) ||  (ty g a = Kind Box && typ = Kind Box)) then typ
                           else failwith "Error!"   
         
         let rec ty_p g term =
            let ok arg = 
                match arg with
                | [] -> true
                | _ -> let typ = ty_p (linit(arg)) (snd(List.last(arg)))
                       if(typ = Kind Box || typ = Kind Star) then true
                       else false
            match term with
            | Varw x -> 
                        let p = findX g (Varw x)
                        match (ok g && (not (nullc p))) with
                        | true -> snd (List.head p)
                        | _ -> failwith "Error!"
            | Kind Star ->
                           match ok g with
                           | true -> Kind Box
                           | _ -> failwith "Error!"
            | Appw (m,n) -> 
                           let c = ty_p g m
                           let d = ty_p g n
                           let rezRed = btred c
                           let var (Pi (x,a,b)) = x
                           let pref arg = 
                                match arg with
                                | (Pi (x,a,b)) -> a
                                | _ -> failwith "Error!"
                           let post arg = 
                                match arg with
                                | (Pi (x,a,b)) -> b
                                | _ -> failwith "Error!"
                           if (btred (pref rezRed) = d) then beta (Appw (Lamw (var rezRed,Kind Star,post rezRed),n))
                           else failwith "Error!"

            | Lamw (x,a,m) -> 
                             let b = ty_p (g @ [(Varw x,a)]) m
                             let typ = ty_p g (Pi (x,a,b))
                             if (typ = Kind Star || typ = Kind Box) then Pi (x,a,b) 
                             else failwith "Error!"  
            | Pi (x,a,b) -> if (ty_p g a = Kind Star) then ty_p (g @ [(Varw x,a)]) b
                            else failwith "Error!"
         let _ok arg = 
                match arg with
                | [] -> true
                | _ -> let typ = ty (linit(arg)) (snd(List.last(arg)))
                       if(typ = Kind Box || typ = Kind Star) then true
                       else false
         // Транслятор \w
         let rec pi str sym= 
             if (findstr "->" str) then Pi (nextTp sym,pi (pref str) (nextTp sym),pi (postf str) (nextTp sym + "1"))
             else if (str = "*") then Kind Star
                  else Varw str
         
         let rec s2type_w s1 s2 = if not (findstr ">" s2) then if (s2 = "*") then Kind Star
                                                               elif (s2 = "#") then Kind Box
                                                               else Varw s2
                                  elif shead s2 = '(' && not (findstr ")->" s1) then s2type_w s1 (sinit (stail s2))
                                  else Pi (s1+"1",s2type_w s1 (pref s2),s2type_w s1 (postf s2))
         
         let rec aft_lam1 str i = 
            match str with 
            | "" -> ""
            | _ -> match (shead str,i) with
                   | ('.',0) -> stail str
                   | ('\\',x) -> aft_lam1 (stail str) (x+1)
                   | ('.',x) -> aft_lam1 (stail str) (x-1)
                   | (s,x) -> aft_lam1 (stail str) x
         let rec aft_lam str = aft_lam1 str 0
         let rec count_lam1 str i = 
            match str with 
            | "" -> ""
            | _ -> match (shead str,i) with
                   | ('.',0) -> ""
                   | ('\\',x) -> "\\" + count_lam1 (stail str) (x+1)
                   | ('.',x) -> "." + count_lam1 (stail str) (x-1)
                   | (s,x) -> String.Concat(s) + count_lam1 (stail str) x
         let rec count_lam str = count_lam1 str 0
         let rec div1 str lst = 
             match str with 
             | "" -> ("","")
             | _ ->
                    match (shead str) with 
                    | '|' -> (lst + ",", stail (stail str))
                    | _ -> div1 (stail str) (lst + String.Concat(shead str))
         let div str = div1 str ""
         let rec comma1 str lst = 
            match str with
            | "" -> ("","")
            | _ -> 
                   match (shead str) with
                   | ',' -> (lst,stail str)
                   | _ -> comma1 (stail str) (lst + String.Concat(shead str))
         let comma str = comma1 str ""
         let rec colon1 str lst = 
            match str with
            | "" -> ("","")
            | _ ->
                   match (shead str) with
                   | ':' -> (lst,stail str)
                   | _ -> colon1 (stail str) (lst + String.Concat(shead str))
         let colon str = colon1 str ""
         let rec lst_cont str = 
            match str with
            | "" -> []
            | _ -> 
                  let comm = fst (comma str)
                  let _str = snd (comma str)
                  let _col = fst (colon comm)
                  let col_ = snd (colon comm)
                  [(Varw _col,s2type_w _col col_)] @ lst_cont _str
         //Транслятор из строковой записи в запись в виде конструкторов
         //String->Term
         let rec sterm_w str =  
            match (shead str) with
            | '\\' -> 
                      let lam = colon (count_lam (stail str))
                      Lamw (fst lam,sterm_w (snd lam),sterm_w (aft_lam (stail str)))
            | '.'  -> sterm_w (stail str)
            | '(' -> if checkstr str then sterm_w (stail (sinit str))
                     else Appw (sterm_w (plastt str),sterm_w (lastt str))            
            | _ -> if checkstr str then if (findstr "->" str) && (not (findstr ":" str)) then pi str ("1")
                                        else if (prefterm str = "*") then Kind Star
                                             else Varw (prefterm str)
                   else Appw (sterm_w (plastt str),sterm_w (lastt str))  
         let rec sterm_p str =  
            let rec pi_p str sym= 
                Pi (nextTp sym,sterm_p (pref str),sterm_p (postf str))
             
            match (shead str) with
            | '\\' -> 
                      let lam = colon (count_lam (stail str))
                      Lamw (fst lam,sterm_p (snd lam),sterm_p (aft_lam (stail str)))
            | 'П' -> let lam = colon (count_lam (stail str))
                     Pi (fst lam,sterm_p (snd lam),sterm_p (aft_lam (stail str)))
            | '.'  -> sterm_p (stail str)
            | '(' -> if checkstr str then sterm_p (stail (sinit str))
                     elif (findstr "->" str) then pi_p str ("a1")
                     else Appw (sterm_p (plastt str),sterm_p (lastt str))         
            | _ -> if checkstr str then if (findstr "->" str) then pi_p str ("1")
                                        else if (prefterm str = "*") then Kind Star
                                             elif (prefterm str = "#") then Kind Box
                                             else Varw (prefterm str)
                   else Appw (sterm_p (plastt str),sterm_p (lastt str))  
         let translate str = 
            let cont = fst (div str)
            //printf "%s" cont
            let term = snd (div str)
            match (cont,term) with
            | (",",_) -> ([],sterm_w term)
            | (_,_) -> (lst_cont cont,sterm_w term)
         let translate_p str = 
            let cont = fst (div str)
            //printf "%s" cont
            let term = snd (div str)
            match (cont,term) with
            | (",",_) -> ([],sterm_p term)
            | (_,_) -> (lst_cont cont,sterm_p term)
         let rec show_w str = 
            match str with 
            | Varw x -> x
            | Appw (x,Varw y) -> "(" + show_w x + ")" + y
            | Appw (Varw x,y) ->  x + "(" + show_w y + ")" 
            | Appw (x,y) -> "(" + show_w x + ")" + "(" + show_w y + ")"
            | Lamw (x,y,z) -> "(" + x + ":" + show_w y + "." + show_w z + ")"
            | Pi (x,Varw y,Varw z) -> y + "->" + z
            | Pi (x,Kind Star,Varw z) -> "*" + "->" + z
            | Pi (x,Varw y,Kind Star) -> y + "->" + "*"
            | Pi (x,Kind Star,z) -> "*" + "->" + "(" + show_w z + ")"
            | Pi (x,y,Kind Star) -> "(" + show_w y + ")" + "->" + "*"
            | Pi (x,y,z) -> "(" + show_w y + ")" + "->" + "(" + show_w z + ")"
            | Kind Star -> "*"
            | Kind Box -> "#"
         let rec showmas_w t = 
            match t with
            | [] -> ""
            | _ ->
                  let he = List.head(t)
                  let li = List.tail(t)
                  if (List.length t = 1) then show_w (fst he) + ":" + show_w (snd he)
                  else show_w (fst he) + ":" + show_w (snd he) + "," + showmas_w li
         let rec postr trm way = 
            match trm with
            | Lam (s1,Var s2) -> [(way + "s",s1)] @ [(way + "0d",s2)]
            | Lam (s,tr) -> [(way + "s",s)] @ [(way + "0",show tr)] @ postr tr (way + "0") 
            | App (Var s1,Var s2) -> [(way + "1d",s1)] @ [(way + "0d",s2)]
            | App (Var s1,tr2) -> [(way + "1d",s1)] @ [(way + "0",show tr2)] @ postr tr2 (way + "0") 
            | App (tr1,Var s2) -> [(way + "1",show tr1)] @ [(way + "0d",s2)] @ postr tr1 (way + "1") 
            | App (tr1,tr2) -> [(way + "1",show tr1)] @ [(way + "0",show tr2)] @ 
                               postr tr1 (way + "1") @ postr tr2 (way + "0")      
            | Var x -> [way + "d",x]
         let post trm = [("0",show trm)] @ postr trm "0"

         let rec outtype' trm tp symb way = 
            match trm with
            | Var p -> Branch (way,tp,Null,Null)
            | App (t1,t2) -> Branch (way,tp,outtype' t1 (nextTp symb + "->" + tp) (nextTp symb) (way + "1"),outtype' t2 (nextTp symb) (nextTp symb + "1") (way + "0"))
            | Lam (p,t) -> Branch (way,tp + "=" + nextTp symb + "->" + (nextTp (nextTp symb)),
                                     outtype' (Var p) (nextTp symb) symb (way + "1"),outtype' t (nextTp (nextTp symb)) (nextTp (nextTp symb)) (way + "0"))
         let outtype trm = outtype' trm "a" "a" "0"

         let rec showout tp = 
            match tp with
            | Null -> ""
            | Branch (x,w,l,r) -> "[" + x + "," + w + "]" + showout l + showout r
         //-------------------------------------------------------------------------------  
         let strue t = 
            match t with 
            | true -> "true"
            | false -> "false"

         let deftype_l t = showtype (substr (unif (s2type (fst (urav (equat (sterm t))))) (s2type (snd (urav (equat (sterm t)))))) (s2type (snd (List.last(equat (sterm t))))))
         let deftype t = if (findstr "forall" (showtype (srt (l_s2type (rev2 (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t))))))))) then showtype (srt (l_s2type (rev2 (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t)))))))
                         else deftype_l t
         let deftype_lw t = show_w (ty (fst (translate t)) (snd (translate t)))
         let deftype_lp t = show_w (ty_p (fst (translate_p t)) (snd (translate_p t)))
         //let deftype_lp t = show_w (sterm_p t)
         let treelam t = shows (post (sterm t)) + "!"
         let out_equat t = shows (equat (sterm t)) + "!"
         let out_rule t = showout (outtype (sterm t)) + "!"
         let ttr10 t = "equat = " + shows (equat (sterm t))
         let ttr11 t = "type = " + showtype (s2type (snd (List.last(let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t)))))))
         let ttr12 t = "let_equat = " + shows (rev2 (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t)))))
         let ttr13 t = "unif = " + masshowtype (unif (s2type (fst (urav (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t))))))) (s2type (snd (urav (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t))))))))
         let ttr14 t = showtype (substr (unif (s2type (fst (urav (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t))))))) (s2type (snd (urav (let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t)))))))) (s2type (snd (List.last(let_true_equat (true_equat(equat (sterm t))) (false_equat(equat (sterm t))))))))
         let ttr15 t = showtype (substr (unif (s2type (fst (urav (equat (sterm t))))) (s2type (snd (urav (equat (sterm t)))))) (s2type (snd (List.last(equat (sterm t))))))
         
