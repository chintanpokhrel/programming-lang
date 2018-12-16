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

fun only_capitals(sl) =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) sl  

fun longest_string1 sl =
    List.foldl (fn (x,init) => (if (String.size x > String.size(init)) then x else init)) "" sl

fun longest_string2 sl =
    List.foldl (fn (x,init) => (if (String.size x >= String.size init) then x else init)) "" sl

fun longest_string_helper f sl =
    List.foldl (fn (x, init) => if (f(String.size x, String.size init)) then x else init) "" sl

val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

val longest_capitalized = longest_string3 o only_capitals
	
val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME v => v
		    | NONE => first_answer f xs'


fun all_answers f xs =
    let fun loop(acc,xs) =
	    case xs of
		[] => acc
	     |  x::xs => case (f x, acc) of
			     ((SOME v), SOME a) => loop(SOME (v@a), xs)
			   | _ => NONE
    in
	loop((SOME []), xs)
    end
	
	
val count_wildcards = g (fn ()=>1) (fn x=>0)

val count_wild_and_variable_lengths = g (fn f=>1) String.size

fun count_some_var (x,p) =
    g (fn () => 0) (fn y => if x=y then 1 else 0) p

fun check_pat p =
    let fun get_vars (p,acc) =
	    case p of
		Variable x => acc@[x]
	      | TupleP ps => List.foldl (get_vars) acc ps
	      | ConstructorP(_,p') => get_vars(p',acc)
	      | _ => acc
	fun repeats xs =
	    case xs of
		[] => true
	      | x::xs' => if List.exists (fn y => x=y) xs' then false else repeats xs'
    in
	repeats (get_vars(p, []))
    end
	

fun match (v, p) =
    case (v,p) of
	(_,Wildcard) => SOME []
     | (v, Variable s) => SOME [(s,v)]
     | (Unit, UnitP) =>  SOME []
     | (Const x, ConstP y) => if x=y then SOME [] else NONE
     | (Constructor (s1, v), ConstructorP (s2, p)) => if s1 = s2 then match (v, p) else NONE
     | (Tuple vs, TupleP ps) => if List.length ps = List.length vs
				then all_answers match (ListPair.zip (vs, ps))
				else NONE
     | _ => NONE


fun first_match v ps =
    let val vps = map (fn x => (v, x)) ps
    in
	SOME (first_answer match vps) handle NoAnswer => NONE
    end
	
												  
														       
