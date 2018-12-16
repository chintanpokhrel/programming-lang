fun same_string(s1 : string, s2 : string) =
    s1 = s2



fun all_except_option1(x2, x) =
   case x2 of
       [] => []
     | a::b => let val e = all_except_option1(b,x)
              in  if same_string(x,a)
                then e 
                else a::e 
                
              end 
     
 
fun all_except_option(string : string, lisete : string list)=

         if length(all_except_option1(lisete,string)) = length(lisete)
         then NONE
         else SOME (all_except_option1(lisete,string))


fun get_substitutions1 (x : string list list, s : string) =
    case x of
	[] => []
   | (x::xs) => case all_except_option(s,x) of
		       SOME b => b @ get_substitutions1(xs, s) 
		     | NONE => get_substitutions1(xs, s)


fun get_substitutions2 (x : string list list, s : string) =
   let fun aux(x : string list list, s : string, acc) = 
    case x of
    	[] => acc
    | (x::xs) => case all_except_option(s,x) of
		       SOME b => aux(xs, s, acc@b) 
		     | NONE => aux(xs, s, acc)
    in aux(x,s,[])
    end
type full_name = { first  : string, 
                   middle : string, 
                   last   : string }

fun map_names ({first=f, middle=m, last=l}, x : string list) =
    case x of
	[] => []
	   | head::tail => {first=head, middle=m, last=l}::map_names({first=f, middle=m, last=l}, tail)

fun similar_names (c : string list list, fname : full_name) =

   let
       val {first=f, middle=m, last=l} = fname
   in 
       fname::(map_names (fname,get_substitutions1(c, f))) 
   end



datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 
exception IllegalMove
fun card_color (x,_)=
case x of 
  Spades => Black
  |Clubs => Black
  |Hearts => Red
 | Diamonds=> Red




fun card_value(_,s)=
  case s of 
  Jack => 10
  | Queen => 10
  | King => 10 
  | Ace => 11
  | Num a => a 
fun remove_card(cs : card list, c : card, e : exn)=
  case cs of 
    [] => raise e 
  |a::b => if a = c 
          then b 
          else a::remove_card(b,c,e)




fun all_same_color(cl : card list)=
case cl of 
  [] => true 
  | a::[] => true 
  | a::(b::c) => (card_color(a) = card_color(b)) andalso all_same_color(b::c) 
fun sum_cards(ls : card list) = 
  let fun f(ls, acc) = 
    case ls of 
      [] => acc
      | a::b => f(b,card_value(a)+acc)
    in f(ls,0)
  end 
fun score (cl,goal)= 
 if all_same_color(cl :card list)
 then  
  if sum_cards(cl)>goal 
  then 2 div (3*(sum_cards(cl)-goal))
  else 2 div (goal-sum_cards(cl))
else 
  if sum_cards(cl)>goal 
  then 3*(sum_cards(cl)-goal)
  else goal-sum_cards(cl)




fun officiate (cards : card list, moves : move list, goal : int) = 
    let
	fun aux (cards : card list, held : card list, moves : move list, goal : int) =
	    case (cards, held, moves, goal) of
		(_,     _, [],    _) => score (held, goal)
	      | ([],    _, _,     _) => score (held, goal)
	      | (c::cs, _, m::ms, _) => case m of
				      Discard d => aux (c::cs, remove_card (held, d, IllegalMove), ms, goal)
				    | Draw => case c::cs of
						  [] => score (held, goal)
						  | _ => 
						    let
							val held' = c::held
							val held_sum = sum_cards (held')
						    in
							if (held_sum > goal)
							then score (held', goal)
							else aux (cs, held', ms, goal)
						    end
    in
	aux (cards, [] , moves, goal)
    end