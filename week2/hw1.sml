(*
1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.)*)
fun is_older (dt1: int*int*int,dt2: int*int*int)=
    if #1 dt1 < #1 dt2
    then true
    else
	if #1 dt1 <= #1 dt2 andalso #2 dt1 < #2 dt2
	then true
	else
	    if #1 dt1 <= #1 dt2 andalso #2 dt1 <= #2 dt2 andalso #3 dt1 < #3dt2
	    then true
	    else
		false
		    
 (*2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month.*)   

		    
fun number_in_month (dates:( int*int*int)list, month: int)=
    if null dates then 0
    else
	if #2 (hd dates)=month
	then 1+number_in_month(tl dates,month)
	else  0+ number_in_month(tl dates,month)

(*3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem.*)

					       
fun number_in_months (dates: (int*int*int) list,months: int list )=
    if null months then 0
    else if null (tl months) then number_in_month(dates,hd months)
    else number_in_month(dates,hd months)+number_in_months(dates, tl months)
							  
							  
(*4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given.*)

							  

fun dates_in_month(dates: (int*int*int)list,month: int)=
    if #2(hd dates)=month
    then (hd dates)::dates_in_month(tl dates,month)
     else dates_in_month(tl dates, month)




(*5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@).*)

			
fun dates_in_months(dates: (int*int*int)list,months: int list)=
    if null (tl months) then dates_in_month(dates,hd months)
    else dates_in_month(dates, hd months) @ dates_in_months(dates,tl months)


(*6. Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay.*)
							 
fun get_nth(xs: string list,n: int)=
       if n=1 then hd xs
       else  get_nth(tl xs,n-1)

(*7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December.*)

		    
fun date_to_string(date: (int*int*int))=
    get_nth(["January","February","March","April",
	     "May","June","July","August","September",
	     "October","November","December"],#2 date)
		^ " "^Int.toString(#3 date)^","^Int.toString(#1 date)


(*8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case.*)

							    
fun number_before_reaching_sum(sum: int,lista: int list)=
    if null lista then 0
    else if hd lista>=sum then 0
    else 1+ number_before_reaching_sum(sum- hd lista, tl lista)

(*9. Write a function what_month that takes a day of year (i.e., an int between 1 and 365) and returns
what month that day is in (1 for January, 2 for February, etc.). Use a list holding 12 integers and your
answer to the previous problem.*)


fun what_month(day: int)=
     number_before_reaching_sum(day,[31,28,31,30,31,30,31,30,31,30,31,30])+1



(*10. Write a function month_range that takes two days of the year day1 and day2 and returns an int list
[m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month
of day day2. Note the result will have length day2 - day1 + 1 or length 0 if day1>day2.*)
									       
fun month_range(day1:int,day2:int)=
    if day1<=day2
    then (what_month day1)::month_range (day1+1,day2)
    else
    []

(*11. Write a function oldest that takes a list of dates and evaluates to an (int*int*int) option. It
evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list.*)

fun oldest(dates: (int*int*int) list)=
    if null dates then NONE
    else
	let fun helper(dates: (int*int*int)list)=
	    if null( tl dates)
	    then hd dates
	    else
		let val last=helper(tl dates)
	           in
	           if is_older(hd dates,last)
	           then hd dates
	           else last
		end
           in SOME(helper dates)
	end