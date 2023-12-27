first (e,_,_) = e
second (_,e,_) = e
third (_,_,e) = e

strStart s pref = let prefLen = length pref in let s2 = take prefLen s in s2 == pref
stackPush a s = a : s
stackPop s = tail s
stackTop s = head s
stackTop2nd s = stackTop (tail s)
stackSize s = length s
printElem e = if first(e) == 0 then print(second(e)) else print(third(e))
elemType e = if first(e) == 0 then "Bool" else "Int"


loop s = do 
 str <- getLine
 if str == "True" || str == "true"
 then do let s2 = stackPush (1,1,True) s in loop s2
 else if str == "False" || str == "false"
 then do let s2 = stackPush (1,0,False) s in loop s2
 else if strStart str "push-" || strStart str "Push-"
 then do
  let tmp = drop 5 str in let tmpInt = read tmp :: Integer in let s2 = stackPush (0,tmpInt,False) s in loop s2

 else if str == "pop" || str == "Pop"
 then if stackSize s > 0 then let s2 = stackPop s in loop s2 else return()

 else if str == "top" || str == "Top"
 then if stackSize s > 0 then let out = stackTop s in printElem out else return ()
 
 else if str == "add" || str == "Add"
 then 
  if (stackSize s > 1 && first(stackTop s) == 0 && first(stackTop2nd s) == 0) 
  then let e1 = stackTop s in let e2 = stackTop2nd s in let s2 = stackPop s in let s3 = stackPop s2 in let s4 = stackPush (0,second(e1)+second(e2),False) s3 in loop s4
  else return()
  
 else if str == "sub" || str == "Sub"
 then if (stackSize s > 1 && first(stackTop s) == 0 && first(stackTop2nd s) == 0) 
  then let e1 = stackTop s in let e2 = stackTop2nd s in let s2 = stackPop s in let s3 = stackPop s2 in let s4 = stackPush (0,second(e1)-second(e2),False) s3 in loop s4
  else return()
  
 else if str == "mul" || str == "Mul"
 then if (stackSize s > 1 && first(stackTop s) == 0 && first(stackTop2nd s) == 0) 
  then let e1 = stackTop s in let e2 = stackTop2nd s in let s2 = stackPop s in let s3 = stackPop s2 in let s4 = stackPush (0,second(e1)*second(e2),False) s3 in loop s4
  else return()
  
 else if str == "and" || str == "And"
 then if (stackSize s > 1 && first(stackTop s) == 1 && first(stackTop2nd s) == 1) 
  then let e1 = stackTop s in let e2 = stackTop2nd s in let s2 = stackPop s in let s3 = stackPop s2 in let s4 = stackPush (1,0,third(e1) && third(e2)) s3 in loop s4
  else return()
 
 else return()
 
 if str=="exit" || str=="Exit"
 then return()
 else let s2 = s in loop s2
 

main = loop []
