(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* hlper function is really not needed here... my mistake*)
fun all_except_option(s: string, ls: string list) =
  let fun f(remain, output, has_s) =
    case (remain, has_s) of
       ([], false) => NONE
       | ([], true) => SOME output
       | (s'::remain', _) => if same_string(s, s') 
            then f(remain', output, true)
            else f(remain', output@[s'], has_s)
  in
    f(ls, [], false)
  end

fun all_except_options_guide(s: string, xs: string list) =
  case xs of
       [] => NONE
     | x::xs' =>
         if same_string(s, x)
         then SOME xs'
         else case all_except_options_guide(s, xs') of
                   NONE => NONE
                 | SOME y => SOME(x::y)


fun get_substitutions1(sub: string list list, s: string) =
  case sub of
      [] => []
    | sl::sub' => 
           case all_except_option(s, sl) of
                 SOME all' => all'@get_substitutions1(sub', s)
               | NONE => get_substitutions1(sub', s)

fun get_substitutions1_guide(substitutions, str) = 
  case substitutions of
       [] => []
     | x::xs => let val foo = all_except_options_guide(str, x)
                in case foo of
                        NONE => get_substitutions1_guide(xs, str)
                      | SOME y => y @ get_substitutions1_guide(xs, str)
                end

fun get_substitutions2(substitutions: string list list, s: string) =
  let fun get_sub(remain, output) = 
    case remain of
        [] => output
      | sl::remain' => 
             case all_except_option(s, sl) of
                 SOME all => get_sub(remain', output@all)
               | NONE => get_sub(remain', output)
  in
    get_sub(substitutions, [])
  end


fun similar_names(sll: string list list, nr: {first: string, middle: string, last:string}) =
  let fun sub_nr(names, output) = 
    case names of
        ([]) => output
      | (name::names') => sub_nr(names', output@[{first=name, middle=(#middle nr), last=(#last nr)}])
  in
    sub_nr(get_substitutions2(sll, (#first nr)), [nr])
  end

fun similar_names_gd(substitutions, name) =
  let
    val {first=f, middle=m, last=l} = name
    fun make_names xs =
      case xs of
           [] => []
         | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
  in
    name::make_names(get_substitutions2(substitutions, f))
  end

fun card_color(cd: card) =
  case (#1 cd) of
       (Spades | Clubs) => Black
     | (_) => Red

fun card_color_gd(card) =
  let val (s, v) = card
  in case s of
          Clubs => Black
        | Diamonds => Red
        | Hearts => Red
        | Spades => Black
  end

fun card_value(cd: card) =
  case (#2 cd) of
       Num v => v
     | Ace => 11
     | _ => 10

fun remove_card(cds: card list, cd: card, ex) =
  let fun except(cds_lst, output, has_cd) =
    case (cds_lst, has_cd) of
         ([], false) => raise ex
       | ([], true) => output
       | (cd'::cds_lst', _) => 
           case (has_cd, cd' = cd) of
                (true, _) => except(cds_lst', output@[cd'], true)
              | (false, true) => except(cds_lst', output, true)
              | (false, false) => except(cds_lst', output@[cd'], false)
  in
    except(cds, [], false)
  end

fun remove_card1(cs, c, e) =
  case cs of
       [] => raise e
     | x::cs' => if x = c then cs' else x :: remove_card(cs', c, e)

fun remove_card2(cs, c, e) =
  let fun f cs =
    case cs of
        [] => raise e
      | x::cs' => if x = c then cs' else x :: f cs'
  in
    f cs
  end


fun all_same_color(cds: card list) =
  let fun eq(cds_remain, first) =
    case cds_remain of
        [] => true
      | c::cds_remain' => 
          if card_color(c) = card_color(first) 
          then eq(cds_remain',first)
          else false
  in
    case cds of
         [] => true
       | c::cds' => eq(cds', c)
  end

fun all_same_color1(cs) =
  case cs of
       [] => true
     | [_] => true
     | head::neck::tail =>
         card_color(head) = card_color(neck) andalso all_same_color(neck::tail)

fun all_same_color2(cs) =
  case cs of
       head::neck::tail => 
         card_color(head) = card_color(neck) andalso all_same_color(neck::tail)
     | _ => true 


fun sum_cards(cds: card list) =
  let fun sum(cds_remain: card list, result) =
    case cds_remain of
         [] => result
       | cd'::cds_remain' => sum(cds_remain', card_value(cd') + result)
  in
    sum(cds, 0)
  end

fun sum_cards1(cs) =
  let fun loop(acc, cs) =
    case cs of
         [] => acc
       | c::cs' => loop (acc + card_value(c), cs')
  in
    loop(0, cs)
  end


fun score(cds: card list, goal: int) =
  let fun drawing(cds_remain, held) =
    let 
      val sum_of_score = sum_cards(held)
      val same_color = all_same_color(held)
    in case (cds_remain, sum_of_score >= goal) of
           (_, true) => 
            if same_color
            then ((sum_of_score - goal) * 3) div 2
            else (sum_of_score - goal) * 3
         | ([], _) => 
             if same_color
             then (goal - sum_of_score)  div 2
             else goal - sum_of_score
         | (cd'::cds_remain', _) => drawing(cds_remain', held@[cd'])
   end
  in
    drawing(cds, [])
  end

fun score1(cs, goal) = 
  let
    val sum = sum_cards(cs)
  in
    (if sum >= goal then 3 * (sum - goal) else goal - sum)
    div (if all_same_color cs then 2 else 1)
  end

fun officiate(cds: card list, mvs: move list, goal: int) =
  let fun f(cds_remain, held, mvs_remain) =
    case mvs_remain of
         [] => score(held,goal)
       | mv'::mvs' =>
           case mv' of 
                Discard discard =>
                  f(cds_remain, remove_card(held, discard, IllegalMove), mvs')
              | Draw =>
                  case (cds_remain, sum_cards(held) > goal) of
                       ([], _) => score(held, goal)
                     | (_, true) => score(held, goal)
                     | (cd'::cds_remain', _) => f(cds_remain', held@[cd'], mvs')
  in
    f(cds, [], mvs)
  end

fun officiate2(cards, plays, goal) =
  let
    fun loop(current_cards, cards_left, plays_left) =
      case plays_left of
           [] => score(current_cards, goal)
         | (Discard c)::tail =>
             loop(remove_card(current_cards, c, IllegalMove), cards_left, tail)
         | Draw::tail =>
             case cards_left of
                  [] => score(current_cards, goal)
                | c::rest =>
                    if sum_cards (c::current_cards) > goal
                    then score(c::current_cards, goal)
                    else loop(c::current_cards, rest, tail)
  in
    loop([], cards, plays)
  end

