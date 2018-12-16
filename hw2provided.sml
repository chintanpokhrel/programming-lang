(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, sl) =
    let fun find(sl) = 
	    case sl of
		[] => false
	      | x::xs => if same_string(s, x) then true else find(xs)
        fun except(sl) =
	    case sl of
		[] => []
	      | x::xs => if same_string(s, x) then except(xs) else x::except(xs)
    in
	case find(sl) of
 	    false => NONE
	 | true  => SOME (except sl) 
    end


fun get_substitutions1 (sll, s) =
    case sll of
	[] => []
      | sl::sll' => case all_except_option(s, sl) of
			 NONE => get_substitutions1(sll', s)
		       | SOME ls => ls @ get_substitutions1(sll', s)


fun get_substitutions2(sll, s) =
    let fun getsub_helper(sll, aux) =
	    case sll of
		[] => aux
	     |  sl:: sll' => case all_except_option(s, sl) of
				 NONE => getsub_helper(sll', aux)
			       | SOME ls => getsub_helper(sll', aux @ ls)
    in
	getsub_helper(sll, [])
    end

fun similar_names(sublist, {first,middle,last}) =
    let fun make_names(subs) =
	    case subs of
		[] => []
	      | sub :: subs' => [{ first=sub, middle=middle, last=last }] @ make_names(subs') 
    in
	    [{ first=first, middle=middle, last=last }] @ make_names(get_substitutions2(sublist, first))
    end
	
							   
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c) =
    case c of
	(Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black 

fun card_value(c) =
    case c of
	(_, Num x) => x
      | (_, Ace) => 11
      | _ => 10
		 
fun remove_card(cs, c, e) =
    case cs of
	[] => raise e
      | c'::cs' => if c=c' then cs' else c'::remove_card(cs', c, e)
							
fun all_same_color(cs) =
    case cs of
	[] => true
      | c::c'::cs' => ((card_color(c) = card_color(c')) andalso all_same_color(c'::cs'))
      | _ => true
								  
fun sum_cards(cs) =
    let fun sum_helper(cs, sum) =
	    case cs of
		[] => sum
	      | c::cs' => sum_helper(cs', sum + card_value(c))
    in
	sum_helper(cs, 0)
    end
	
fun score(cs, goal) =
    let val sum=sum_cards(cs)
	val same_color=all_same_color(cs)
	fun prelim_score() =
	    if sum > goal then 3 * (sum - goal)
	    else goal - sum
    in
	if same_color then (prelim_score ()) div 2
	else prelim_score()
    end
	

fun officiate(cs, ml, goal) =
    let val e  = IllegalMove
	fun helper(cs, ml, hcs) =
	    case ml of
		[] => score(hcs, goal)
	      | m::ml' => case m of
			      Discard c => helper(cs, ml', remove_card(hcs, c, e)) 
			    | Draw => case cs of
					  [] => score(hcs, goal)
					| c::cs' => if sum_cards(hcs) > goal then score(hcs, goal)
						   else helper(cs', ml', c::hcs)
    in
	helper(cs, ml, [])
    end

    
    
		  
