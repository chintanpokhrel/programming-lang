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
(*problem 1*)
fun only_capitals str_list =
    List.filter (fn x => Char.isUpper (String.sub (x,0))) str_list;

(*problem 2*)
fun longest_string1 str_list =
    List.foldl (fn (x, y) => if String.size x > String.size y then x else y) "" str_list;

(*problem 3*)
fun longest_string2 str_list =
    List.foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" str_list;

(*problem 4*)
fun longest_string_helper f str_list =
    case str_list of
	[] => ""
      | x::[] => x
      | x::y::xs => if f (String.size y, String.size x) then longest_string_helper f (y::xs) else longest_string_helper f (x::xs);

val longest_string3 = longest_string_helper (fn (x, y) => x > y);
val longest_string4 = longest_string_helper (fn (x, y) => x >= y);
	     

(*problem 5*)
val longest_capitalized  = longest_string1 o only_capitals

(*problem 6*)
val rev  =
    String.implode o List.rev o String.explode;
fun rev_string str = rev str

(*problem 7*)
fun first_answer f list =
    case list of
	[] => raise NoAnswer
      | x::xs => case f x of
		     NONE => first_answer f xs
		   | SOME y => y 
					   
(*problem 8*)
fun all_answers f list =
    let fun helper (f, acc, lis) =
	    case lis of
		[] => SOME acc
	      | x::xs => case f x of
			     NONE => NONE
			   | SOME y => helper (f, acc@y, xs)
    in
	helper (f, [], list)
    end

(*problem 9a*)
val help1 = g (fn _ => 1) (fn x => 0)
fun count_wildcards p = help1 p

(*problem 9b*)
val help2 = g (fn _ => 1) (fn x => String.size x)
fun count_wild_and_variable_lengths p = help2 p

(*problem 9c*)
fun count_some_var (str, p) =
    let val help3 =
	    g (fn _ => 0) (fn x => if x=str then 1 else 0)
    in
	help3 p
    end
    
(*problem 10*)
fun check_pat p =
    let fun helper1 p =
	    case p of
		Variable s => [s]
	      | TupleP ps => List.foldl (fn (x, y) => (helper1 x)@y) [] ps
	      | ConstructorP (_, p') => helper1 p'
	      | _ => []
	fun helper2 (str, str_list) =
	    List.exists (fn x => if x=str then true else false) str_list
	fun helper3 str_list =
	    case str_list of
		[] => true
	      | x::xs => not(helper2 (x, xs)) andalso helper3 xs 
    in
	let val lis = helper1 p
	in
	    helper3 lis
	end
    end

(*problem 11*)
fun match (v, p) =
    case p of
	Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v of
		     Unit => SOME []
		   | _ => NONE)
      | ConstP x => (case v of
			Const y => if x=y then SOME [] else NONE
		      | _  => NONE) 
      | TupleP ps => (case v of
 			 Tuple vs => if List.length ps <> List.length vs then NONE else all_answers match (ListPair.zip (vs, ps))
			| _ => NONE)
      | ConstructorP (s1, p')  => (case v of
				       Constructor (s2, v') => if s1=s2 then match(v', p') else NONE
				     | _ => NONE) 

(*problem 12*)
fun first_match	v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE		 



