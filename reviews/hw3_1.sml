(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
val only_capitals =	List.filter (fn s => Char.isUpper (String.sub(s, 0)))

(* 2 *)
val longest_string1 = foldl (fn (s1, s2) => if String.size s1 > String.size s2 then s1 else s2) ""

(* 3 *)
val longest_string2 = foldl (fn (s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) ""

(* 4 *)
fun longest_string_helper g =
	foldl (fn (s1, s2) => if g (String.size s1, String.size s2) then s1 else s2) ""

val longest_string3 = longest_string_helper (fn (i1, i2) => i1 > i2)
val longest_string4 = longest_string_helper (fn (i1, i2) => i1 >= i2)

(* 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer g xs =
	case xs of
		[] => raise NoAnswer
	  | x::xs' => case g x of
					NONE => first_answer g xs'
				  | SOME i => i

(* 8 *)
fun all_answers g xs =
	let fun aux (ys, acc) =
			case ys of
				[] => SOME acc
			  | y::ys' => case g y of
							  NONE => NONE
							| SOME j => aux (ys', acc @ j)
	in aux (xs, []) end

(* 9 *)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =	g (fn () => 1) String.size p

fun count_some_var (s, p) =	g (fn () => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat p =
	let
		fun helper1 p =
			case p of
				Variable x => [x]
			  |	TupleP ps => foldl (fn (p, acc) => acc @ (helper1 p)) [] ps
			  | ConstructorP(_, p) => helper1 p
			  | _ => []
		fun helper2 xs =
			case xs of
				[] => true
			  | x::xs' => (not (List.exists (fn y => y = x) xs')) andalso helper2 xs'
	in helper2 (helper1 p)
	end

(* 11 *)
fun match x =
	case x of
		(_, Wildcard) => SOME []
	  | (v, Variable s) => SOME [(s, v)]
	  | (Unit, UnitP) => SOME []
	  | (Const i, ConstP j) => if i = j then SOME [] else NONE
	  | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
	  							 then all_answers match (ListPair.zip (vs, ps))
								 else NONE	
	  | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 then match (v, p) else NONE
	  | _ => NONE	 

(* 12 *)
fun first_match v ps =
	SOME (first_answer (fn p => match (v, p)) ps)
  	handle NoAnswer => NONE

(* Challenge Problem *)
(* fun typecheck_patterns  *)