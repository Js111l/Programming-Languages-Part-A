(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer



datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

      (**** you can put all your code here ****)

(*
1. Write a function only_capitals that takes a string list and returns a string list that has only
the strings in the argument that start with an uppercase letter. Assume all strings have at least 1
character. Use List.filter, Char.isUpper, and String.sub to make a 1-2 line solution.*)
			       
fun only_capitals(strings: string list)=
    List.filter (fn x =>Char.isUpper(String.sub(x,0)) ) (strings)
			      
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

(*2. Write a function longest_string1 that takes a string list and returns the longest string in the
list. If the list is empty, return "". In the case of a tie, return the string closest to the beginning of the
list. Use foldl, String.size, and no recursion (other than the implementation of foldl is recursive).*)

fun longest_string1 (xs:string list)=
    case xs of
	[]=>""
       |	x::xs'=>List.foldl (fn (x,y) => if (String.size x=String.size y) orelse(String.size x > String.size y) then x
						else y ) "" xs
											 
					      
val test2 = longest_string1 ["A","bc","C"] = "bc"

						 
(*
3. Write a function longest_string2 that is exactly like longest_string1 except in the case of ties
it returns the string closest to the end of the list. Your solution should be almost an exact copy of
longest_string1. Still use foldl and String.size.
*)

fun longest_string2 (xs:string list)=
    case xs of
	[]=>""
      |  x::xs'=>List.foldl (fn (x,y) => if (String.size x=String.size y) then y
						else if(String.size x > String.size y) then x
						else y ) "" xs

val test3 = longest_string2 ["A","bc","C"] = "bc"


						 
 (*
4. Write functions longest_string_helper, longest_string3, and longest_string4 such that:
• longest_string3 has the same behavior as longest_string1 and longest_string4 has the
same behavior as longest_string2.
• longest_string_helper has type (int * int -> bool) -> string list -> string
(notice the currying). This function will look a lot like longest_string1 and longest_string2
but is more general because it takes a function as an argument.
• If longest_string_helper is passed a function that behaves like > (so it returns true exactly
when its first argument is stricly greater than its second), then the function returned has the same
behavior as longest_string1.

*)



			    
fun incList [] = []
  | incList (x::xs) = (x + 1) :: incList xs


fun longest_string_helper  f  =
    List.foldl(fn (x,y)=> if f ((String.size x),(String.size y)) then x 
     else y) "" 

			    
					   
val longest_string3 =longest_string_helper(fn(x,y)=> if x>y orelse x=y then true else false)
				  
val longest_string4=longest_string_helper(fn(x,y)=> if x>y then true else false)

					 
val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

						 
(*						 
5. Write a function longest_capitalized that takes a string list and returns the longest string in
the list that begins with an uppercase letter, or "" if there are no such strings. Assume all strings
have at least 1 character. Use a val-binding and the ML library’s o operator for composing functions.
Resolve ties like in problem 2.*)

					 
val fil = Char.isUpper o hd o explode 		  
					  

fun longest_capitalized (xs: string list)=
     longest_string4 (List.filter(fn x=>fil x) xs)
						     

		     
val test5 = longest_capitalized ["A","bc","C"] = "A"



(*
6. Write a function rev_string that takes a string and returns the string that is the same characters in
reverse order. Use ML’s o operator, the library function rev for reversing lists, and two library functions
in the String module. (Browse the module documentation to find the most useful functions.)
*)

val rev = implode o List.rev o explode   


				   
fun rev_string (x:string)=
    rev x

	
val test6 = rev_string "abc" = "cba"


				   

				   (*
7. Write a function first_answer of type (’a -> ’b option) -> ’a list -> ’b (notice the 2 arguments are curried).
The first argument should be applied to elements of the second argument in order
until the first time it returns SOME v for some v and then v is the result of the call to first_answer.
If the first argument returns NONE for all list elements, then first_answer should raise the exception
NoAnswer. Hints: Sample solution is 5 lines and does nothing fancy.*)


fun first_answer f xs =
    case xs of
	[]=> raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		   | SOME x => x



val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4




										    (*
8. Write a function all_answers of type (’a -> ’b list option) -> ’a list -> ’b list option
(notice the 2 arguments are curried). The first argument should be applied to elements of the second
argument. If it returns NONE for any element, then the result for all_answers is NONE. Else the
calls to the first argument will have produced SOME lst1, SOME lst2, ... SOME lstn and the result of
all_answers is SOME lst where lst is lst1, lst2, ..., lstn appended together (order doesn’t matter).
Hints: The sample solution is 8 lines. It uses a helper function with an accumulator and uses @. Note
all_answers f [] should evaluate to SOME [].*)


										    


fun all_answers f xs =
    let
	fun helper f xs acc=
	    case xs of
		[]=> SOME acc
	     |  x::xs' => case f x of
			      NONE => NONE
			      | SOME [x]  => helper f xs' (acc@[x])

    in  
    helper f xs []	
    end



val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern


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
						

(*
9. (This problem uses the pattern datatype but is not really about pattern-matching.) A function g has
been provided to you.
(a) Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains.
(b) Use g to define a function count_wild_and_variable_lengths that takes a pattern and returns
the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables
in the variable patterns it contains. (Use String.size. We care only about variable names; the
constructor names are not relevant.)
(c) Use g to define a function count_some_var that takes a string and a pattern (as a pair) and
returns the number of times the string appears as a variable in the pattern. We care only about
variable names; the constructor names are not relevant.*)
(*

fun count_wildcards (x: pattern)=
    g fn x=> *)


	
val f= fn () => int
