(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(str, str_list) = 
    case str_list of
        [] => NONE
        | x :: xs => case (same_string(x, str), all_except_option(str, xs)) of
                    (true, _) => SOME(xs)
                    | (false, NONE) => NONE
                    | (false, SOME(y)) => SOME(x :: y)


fun get_substitutions1(str_list_list, str) =
    case str_list_list of
        [] => []
        | list :: lists => case all_except_option(str, list) of 
                                SOME(x) => x @ get_substitutions1(lists, str) 
                                | NONE => get_substitutions1(lists, str) 

fun get_substitutions2(str_list_list, str) = 
    let 
        fun helper(str_list_list, acc) =
            case str_list_list of
                [] => acc
                | list :: lists => case all_except_option(str, list) of 
                                        SOME(x) => helper(lists, acc @ x)
                                        | NONE => helper(lists, acc) 
    in 
        helper(str_list_list, [])
    end

fun similar_names(str_list_list, {first, middle, last}) = 
    let 
        val list = get_substitutions2(str_list_list, first)
        fun helper(list, acc) = 
            case list of
                [] => acc
                | sub :: subs => helper(subs, {first = sub, middle = middle, last = last} :: acc)
    in
        helper(list, [{first = first, middle = middle, last = last}])
    end
    
val test1 = all_except_option("abc", ["sdf", "prd", "abc", "sss", "qw"]) = SOME(["sdf", "prd", "sss", "qw"])
val test2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test3 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],{first="Fred", middle="W", last="Smith"}) 
= [{first="F",last="Smith",middle="W"}, {first="Freddie",last="Smith",middle="W"}, {first="Fredrick",last="Smith",middle="W"}, {first="Fred",last="Smith",middle="W"}]

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove
(* put your solutions for problem 2 here *)

fun card_color(card) = 
    case card of 
        (Clubs, _) => Black
        | (Spades, _) => Black
        | (Diamonds, _) => Red
        | (Hearts, _) => Red

fun card_value(card) =
    case card of
        (_, Num num) => num
        | (_, Ace) => 11
        | (_, _) => 10

fun remove_card(cs, c, e) = 
    let fun helper(cs, acc) = 
        case cs of 
            [] => raise e
            | card :: cards => if card = c then acc @ cards
                                else helper(cards, card :: acc)
    in 
        helper(cs, [])
    end

fun all_same_color(cs) = 
    case cs of 
        [] => true
        | _ :: [] => true
        | head :: (neck :: rest) => (card_color(head) = card_color(neck) andalso all_same_color(neck :: rest))

fun sum_cards(cs) = 
    let fun helper(cs, acc) = 
        case cs of 
            [] => acc
            | x :: xs => helper(xs, acc + card_value(x))
    in 
        helper(cs, 0)
    end

fun score(cs, goal) = 
    let 
        val sc = sum_cards(cs) - goal
    in 
        if sc >= 0 then ~3 * sc
        else if (all_same_color(cs)) then sc div 2
        else sc
    end

fun officiate(cs,moves,goal) =
  let fun run(cs,moves,held) =
          case (cs,moves) of
            (_,[]) => score(held,goal)
          | ([],Draw::ms) => score(held,goal)
          | (c::cs',Draw::ms) => if sum_cards(c::held) > goal
                                 then score(c::held,goal)
                                 else run(cs',ms,c::held)
          | (c::cs',(Discard card)::ms) => let val held_after_remove = remove_card(held,card,IllegalMove)
                                           in run(cs,ms,held_after_remove)
                                           end
    in
      run(cs,moves,[])
    end

val test5 = card_color(Clubs, Num 10) = Black
val test6 = card_value(Hearts, Num 9) = 9
val test7 = remove_card([(Clubs, Num 10), (Hearts, Num 9),(Clubs, Num 10)], (Clubs, Num 10), IllegalMove) = [(Hearts,Num 9),(Clubs,Num 10)]
val test8 = all_same_color([(Clubs, Num 10), (Spades, Num 9),(Clubs, Num 10)])
val test9 = sum_cards([(Clubs, Num 10), (Spades, Num 9),(Clubs, Num 10)])