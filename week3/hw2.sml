(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines.*)   
fun all_except_option (text: string,text2:string list)=
    case text2 of
	[]=>NONE
       |x::text2' =>
	if same_string(x,text) then
	    SOME text2'
	else
	    case all_except_option(text,text2') of
		SOME y=> SOME (x::y)
		      | NONE  =>  all_except_option(text,text2')		      
(* (b)  Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result.*)	     
	 	 	       
fun get_substitutions1 (strings: (string list) list,s: string )=
    let fun iterate(strings1: (string list) list,s1:string)=
       case strings1 of
       []=>[]
       |x::strings1' =>
          case  all_except_option(s1,x)of
	     NONE => []
	     |SOME lis  => lis 		 
     in
    case strings of
	[]=>[]
     | x::strings' => iterate(strings,s)@iterate(strings',s)
    end
(*(c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun get_substitutions2 (strings: (string list) list,s: string )=
    
    let fun iterate(strings1: (string list) list,s1:string,acc)=
       case strings1 of
       []=>acc
       |x::strings1' =>
          case  all_except_option(s1,x)of
	     NONE => iterate(strings1',s1,acc)
	     |SOME lis  =>iterate(strings1',s1,acc@lis )
      in
       iterate(strings,s,[])
   
    end
	
(* (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names).
Do not eliminate duplicates from the answer. Hint: Use a local helper function. *)

fun similar_names (strings: (string list)list,r:{first:string,middle:string,last:string} )=
    let fun helper (strings: string list,r:{first:string,middle:string,last:string} ,acc:{first:string,middle:string,last:string} list   )=
	    case (strings,r,acc) of
		 ([],r,acc)=> acc
	        |(strings,r,[])=> helper(strings,r,acc@[{first=(#first r),middle=(#middle r),last=(#last r)}])
	        |(x::strings',r,acc)  => helper(strings',r,acc@[{first=x,middle=(#middle r),last=(#last r)}])
  	    
	    
    in
	helper((get_substitutions2(strings,#first r)),r,[])
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
 (* (a) Write a function card_color, which takes a card and returns its color
 (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough.*)

fun  card_color(suit,rank)=
	       case suit of
		 Spades =>Black
		|  Clubs=>Black
	        | Diamonds  => Red
		| Hearts   => Red
				  
(*(b) Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough.*)

fun card_value(suit,rank )=
    case rank of
	Ace=>11
     |  Jack=>	10
     | Queen =>10
     |King  =>10 
     |Num i  =>i 

(*(c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =.*)
fun remove_card (cs: card list,c:card,e)=
    case cs of
     []=>raise e
     | x::cs'=> case x=c of
		   true => cs'
		  |false => x:: remove_card(cs',c,e) 
				
    
(*d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
pattern-matching in the lectures.*)
fun all_same_color (cs: card list)=
    let fun helper(cs:card list,c:card)=
	    case cs of
                []=>true			
	     |  x::cs'=> if card_color(x)=card_color(c)
			 then helper(cs',x)
			 else false	    
	    in
	case cs of
	[]=>true	    
       | x::cs'=> helper(cs',x)
    end
	val test8 = all_same_color [(Hearts, Ace)] = true


(* e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
requirement for this problem.)*)


fun sum_cards (cs: card list)=
    let fun helper(cs:card list,acc:int)=
	    case cs of
		[]=>acc
	        |x::cs'  => helper(cs',acc+card_value(x)) 
    in
    helper(cs,0)
    end
	
(*
(f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described:
Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal),
else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML’s div operator).*)
fun score (cs: card list,n:int)=
       
    let val sum=sum_cards(cs)
	val goal=n
        val sumBiggerScore=3*(sum-goal)
	val goalBiggerScore=goal-sum
    in
	case all_same_color(cs) of
	    false=> if sum>goal then sumBiggerScore
                   else
	              goalBiggerScore
	    
	  | true => if sum>goal then sumBiggerScore div 2
		     else 
		     goalBiggerScore div 2	 
     end

(*(g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described:
A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
makes a move by either drawing, which means removing the first card in the card-list from the card-list and
adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
is greater than the goal.

The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list. *)
(*	NESTED PATTERNS: POPRAWIC TO I NAPISAC SIMILAR NAMES *)
fun officiate (cs:card list,movl:move list,goal: int )=
     
    let fun helper(cs:card list,movl:move list,goal:int,heldcards: card list)=
if (sum_cards(heldcards))<goal
then
    case (cs,movl,goal,heldcards) of
     
     (x::cs',[],goal,heldcards)=>score(heldcards,goal)
     |([],mov::movl',goal,heldcards)=>score(heldcards,goal)
     |(x::cs',mov::movl',goal,heldcards)  =>( case mov of
						 Draw => helper(remove_card(cs,x,IllegalMove),movl',goal,x::heldcards)
						 |Discard x  => helper(cs,movl',goal,remove_card(heldcards,x,IllegalMove)) )	       
     | (_,_,_,_) =>  score(heldcards,goal) 
  
else
   score(heldcards,goal)
    in
   helper(cs,movl,goal,[])
	
    end


(* tests: *)	
val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15);
									   
val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42);
	 val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
				false)		   
              handle IllegalMove => true)
             
