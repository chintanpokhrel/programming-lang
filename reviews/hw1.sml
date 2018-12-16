(* Test if date2 is older than date1 *)
fun is_older (date1 : int*int*int, date2 : int*int*int) =
    (#1 date1 < #1 date2) orelse
    (#1 date1 = #1 date2 andalso #2 date1 < #2 date2) orelse
    (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(* Return number of dates that contain given month  *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
	let val headCount = 
		 if (#2 (hd dates)) = month 
		 then 1
		 else 0
	in
	    headCount + number_in_month(tl dates, month)
	end

(* Return number of dates which months are in list of months  *)
fun number_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))

(* Return dates that contain given month  *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
	if #2 (hd dates) = month
	then hd dates :: dates_in_month (tl dates, month)
	else dates_in_month (tl dates, month)

(* Return dates which months are in list of months *)
fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* Return the nth element from list *)
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)
    
(* Convert date tupple to string format *)
fun date_to_string (date : int*int*int) =
    let val month = get_nth (["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"], #2 date)
	val day = Int.toString (#3 date)
	val year = Int.toString (#1 date)
    in
	month ^ " " ^ day ^ ", " ^ year
    end

(* Return the number of elements that its sum is less than the given number *)
fun number_before_reaching_sum (sum : int, numbers : int list) =
    let fun sum_numbers (count : int, total : int, numbers : int list) =
	    if total >= sum
	    then count
	    else sum_numbers(count + 1, total + (hd numbers), tl numbers)
    in
	sum_numbers(0, hd numbers, tl numbers)
    end

(* Return corrensponding  month of the year day *)
fun what_month (day : int) =
    number_before_reaching_sum (day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1

(* Return months of range of days *)
fun month_range (days : int*int) =
    let val day2 = #2 days
	fun days_months (day1 : int) =
	    if day1 > day2
	    then []
	    else (what_month day1) :: days_months (day1 + 1)
    in
	days_months (#1 days)
    end

(* Return the oldest date in the list *)
fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let fun get_max_date (maxDate : int*int*int, dates : (int*int*int) list) =
		 if null dates
		 then SOME maxDate
		 else let val headDate = hd dates
		      in
			  if is_older(maxDate, headDate)
			  then get_max_date(maxDate, tl dates)
			  else get_max_date(headDate, tl dates)
		      end
	 in
	     get_max_date(hd dates, tl dates)
	 end

(* remove duplicate elements from list *)
fun remove_duplicates (xs : int list) =
    if null xs
    then []
    else let fun remove_duplicate (x : int, xs : int list) =
		 if null xs
		 then []
		 else if (hd xs) = x
		 then remove_duplicate(x, tl xs)
		 else (hd xs) :: remove_duplicate(x, tl xs)
	 in
	     hd xs :: remove_duplicate(hd xs, remove_duplicates(tl xs))
	 end
	    
fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

(* Test if date value is a reasonable date *)
fun reasonable_date (date : int*int*int) =
    let val year = #1 date
	val month = #2 date
	val day = #3 date
    in
	if year < 1 orelse
	   month < 1 orelse month > 12 orelse
	   day < 1
	then false
	else let val isLeapYear = if (year mod 4) = 0 andalso
				     ((year mod 100) <> 0 orelse (year mod 400) = 0)
				  then true
				  else false
		 val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
		 fun days_in_month (month : int, dayList : int list) =
		     if month = 1
		     then hd dayList
		     else days_in_month(month - 1, tl dayList)		      
	     in
		 if isLeapYear andalso month = 2 andalso day <= 29
		 then true
		 else day <= days_in_month(month, monthDays)
	     end
    end
		
		 
	     
    
    
 			       
