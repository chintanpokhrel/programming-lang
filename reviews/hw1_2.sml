fun is_older (date1 : int*int*int, date2 : int*int*int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) < (#3 date2)
    then true
    else false

fun number_in_month (date_list : (int*int*int) list, month : int) =
    if null date_list
    then 0
    else if (#2 (hd date_list)) = month
    then 1 + number_in_month(tl date_list, month)
    else number_in_month(tl date_list, month)
									
fun number_in_months (date_list : (int*int*int) list, months : int list) =
    if null date_list orelse null months
    then 0
    else number_in_month(date_list, hd months) + number_in_months(date_list, tl months)
							 
fun dates_in_month (date_list : (int*int*int) list, month : int) =
    if null date_list
    then []
    else if  (#2 (hd date_list)) = month
    then hd date_list :: dates_in_month(tl date_list, month)
    else dates_in_month(tl date_list, month)
				       
fun dates_in_months (date_list : (int*int*int) list, months : int list) =
    if null date_list orelse null months
    then []				  
    else dates_in_month(date_list, hd months) @ dates_in_months(date_list, tl months)	     

fun get_nth (string_list : string list, index : int) =
    let
        val count = 1
	fun get_nth_helper (string_list : string list, index : int, count : int) =
	    if count = index
            then hd string_list
            else get_nth_helper (tl string_list, index, count + 1)		   
    in
	get_nth_helper(string_list, index, count)
    end	      

fun date_to_string (date : int*int*int) =
    let
	val month_list = ["January", "February", "March", "April",
			  "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(month_list, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ "," ^ " " ^ Int.toString(#1 date)	
    end

fun number_before_reaching_sum (sum : int, numbers : int list) =
    let
        val count = 0
        val sum_temp = 0
	fun helper (sum : int, numbers : int list, count : int, sum_temp : int) =
	    if (sum_temp + hd numbers) >= sum
	    then count
	    else helper (sum, tl numbers, count + 1, sum_temp + hd numbers)
    in
	helper(sum, numbers, count, sum_temp)
    end

fun what_month (day : int) =
    let
	val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in
        1 + number_before_reaching_sum(day, days_of_months)
    end   
    

fun month_range (day1 : int, day2 : int) =
    let
	 val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
         if (day1 > day2)
         then []
         else if day1 = day2
         then 1 + number_before_reaching_sum(day1, days_of_months) :: []
         else 1 + number_before_reaching_sum(day1, days_of_months) :: month_range(day1 + 1, day2)	
    end 
    

fun oldest (date_list : (int*int*int) list) =	
    if null date_list
    then NONE
    else if null (tl date_list)
    then SOME (hd date_list)
    else
	 let val max_of_rest = oldest(tl date_list)
	 in
	     if is_older(hd date_list, valOf max_of_rest)
	     then SOME (hd date_list)
	     else max_of_rest
	 end
  	 

