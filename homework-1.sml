fun is_older(d1: int*int*int, d2: int*int*int) =
    if #1 d1 < #1 d2 then true
    else if #1 d2 < #1 d1 then false
    else if #2 d1 < #2 d2 then true
    else if #2 d2 < #2 d1 then false
    else if #3 d1 < #3 d2 then true
    else false

fun number_in_month(dates: (int*int*int) list, mon: int) =
    if null dates then 0
    else if #2 (hd dates) = mon then 1 + number_in_month(tl dates, mon)
    else number_in_month(tl dates, mon)

fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates then []
    else if #2 (hd dates) = month then (hd dates) :: (dates_in_month(tl dates, month))
    else dates_in_month(tl dates, month)

fun dates_in_months(dates: (int*int*int) list, months: int list) =
    let
	  fun append(xs : (int*int*int) list, ys: (int*int*int) list) =
	    if null xs then ys
	    else hd xs :: append(tl xs, ys)
    in
	  if null months then []
	  else append(dates_in_month(dates, hd months), dates_in_months(dates, tl months))
    end       

fun get_nth(xs: string list, n: int) =
    if n = 1 then hd xs
    else get_nth(tl xs, n-1)
	
fun date_to_string(date: int*int*int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	  get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum: int, xs: int list) =
    if sum <= hd xs then 0
    else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)
				      
fun what_month(doy: int) = 
    let val mons = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
      1 + number_before_reaching_sum(doy, mons)
    end


fun month_range(doy1: int, doy2: int) =
    let fun month_range_i(doy1, l:int list) =
	    if doy1 <= doy2 then what_month(doy1) ::month_range_i(doy1+1, l)
	    else []
    in month_range_i(doy1, []) end
    
fun oldest(dlist: (int*int*int) list) =
  if null dlist then NONE
  else let fun oldest_i(dlist: (int*int*int) list, cur_oldest: int*int*int) = 
            if null dlist then cur_oldest
            else if is_older(hd dlist, cur_oldest) then oldest_i(tl dlist, hd dlist)
            else oldest_i(tl dlist, cur_oldest)
       in 
       SOME (oldest_i(dlist, (10000, 1, 1)))
       end


    
