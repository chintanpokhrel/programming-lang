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
fun only_capitals xs =
    List.filter (fn s => Char.isUpper (String.sub(s,0))) xs

		
(* 2 *)
fun longest_string1 xs =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" xs


(* 3 *)
fun longest_string2 xs =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" xs


(* 4 *)
fun longest_string_helper f xs =
    foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper(fn (x,y) => x >= y)
					    
					    
(* 5 *)				      
val longest_capitalized = longest_string1 o only_capitals

						   
(* 6 *)
fun rev_string s = (String.implode o List.rev o String.explode) s


(* 7 *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      SOME i => i
		    | NONE => first_answer f xs'


(* 8 *)					   
fun all_answers f xs =
    let fun isNone xs = case xs of
			    [] => false
			  | x::xs' => case x of
					  NONE => true
					| SOME _ => isNone xs'
	fun aux(x, acc) = case x of
			      SOME lst => lst @ acc
			    | _ => acc
						    
    in
	if isNone (map f xs)
	then NONE
	else SOME (foldl aux [] (map f xs))
    end

(* as per solution...
fun all_answers f xs =
    let fun loop(xs, acc) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of
			      NONE => NONE
			    | SOME lst => loop(xs', lst@acc)
    in loop(xs,[])
    end *)

	
(* 9 (a) *)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

(* 9 (b) *)
fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p
(* as per solution...
val count_wild_and_variable_lengths = g (fn () => 1) String.size *)
					  
      
(* 9 (c) *)
fun count_some_var (s,p) =
    g (fn () => 0) (fn x => if x = s then 1 else 0) p


(* 10 *)
fun check_pat p =
    let
	fun aux(p,acc) =
	    case p of
		Variable x => [x] @ acc
	      | TupleP ps => (List.foldl aux [] ps) @ acc
	      | ConstructorP (_,p1) => aux(p1,acc)
	      | _ => acc
	fun isRepeat xs =
	    case xs of
		[] => false
	      | s::xs' => (List.exists (fn x => x = s) xs') orelse
			  isRepeat xs'
    in
	not (isRepeat (aux(p,[])))
    end

	
(* 11 *)
fun match (v,p) =
    case (v,p) of
	(_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s,v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length ps = List.length vs
				 then all_answers match (ListPair.zip(vs, ps))
				 else NONE
      | (Constructor (s2,v2), ConstructorP (s1,p1)) => if s1 = s2
						       then match(v2,p1)
						       else NONE
      | _ => NONE

		 
(* 12 *)
fun first_match v ps =
    let
	fun curry f x y = f(x,y)
    in
	SOME (first_answer ((curry match) v) ps) handle NoAnswer => NONE
    end

	
(* Challenge Problem *)
	
fun pattern_to_type ys p =
    case p of
	Wildcard => Anything
      | Variable s => Anything
      | UnitP => UnitT
      | ConstP i => IntT
      | TupleP ps => TupleT (map (pattern_to_type ys) ps)
      | ConstructorP (s1,p1) =>
	(case ys of
	    [] => raise NoAnswer
	  | (s2,s2',t1)::ys' => if s2=s1 andalso (pattern_to_type ys p1)=t1
				then Datatype s2'
				else pattern_to_type ys' p
	)

fun compare (x,acc) =
    case (x,acc) of
	(Anything, acc) => acc
      | (x, Anything) => x
      | (UnitT, UnitT) => UnitT
      | (IntT, IntT) => IntT
      | (TupleT ts1, TupleT ts2) => TupleT (map compare (ListPair.zip(ts1,ts2)))
      | (Datatype s1, Datatype s2) => if s1=s2
				      then Datatype s1
				      else raise NoAnswer
      | (_,_) => raise NoAnswer    

fun typecheck_patterns (xs, ps)  =
    let	
	fun loop (acc, xs) =
	    case xs of
		[] => acc
	      | x::xs' => loop(compare(x,acc), xs')
    in
	SOME (loop(Anything, (map (pattern_to_type xs) ps))) handle NoAnswer => NONE
    end


(*
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string				
*)
