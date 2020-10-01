fun is_older(x: int * int * int, y: int * int * int) = 
    if #1 x <> #1 y then #1 x < #1 y
    else if #2 x <> #2 y then #2 x < #2 y
    else #3 x < #3 y

fun number_in_month(xs: (int * int * int) list, month: int ) =
    if null xs then 0
    else let 
            val next = number_in_month(tl xs, month)
        in
            if #2(hd xs) = month then 1 + next else next
        end

fun number_in_months(xs: (int * int * int) list, months: int list ) =
    if null months then 0
    else number_in_month(xs, hd months) + number_in_months(xs, tl months)

fun dates_in_month(xs: (int * int * int) list, month: int) =
    if null xs then []
    else let 
            val next = dates_in_month(tl xs, month) 
        in 
            if #2(hd xs) = month then hd xs :: next
            else next 
        end

fun dates_in_months(xs: (int * int * int) list, months: int list) =
    if null months then []
    else dates_in_month(xs, hd months) @ dates_in_months(xs, tl months)

fun get_nth(xs: string list, n: int) =
    if n = 1 then hd xs
    else get_nth(tl xs, n - 1)

fun date_to_string(x: (int * int * int)) = 
    let 
        val month_list = ["January", "February", "March", "April", "May", 
        "June", "July", "August", "September", "October", "November", "December"]
    in 
        get_nth(month_list, #2 x) ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
    end

fun number_before_reaching_sum(sum: int, xs: int list) =  
    if sum <= 0 then ~1
    else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month(day: int) = 
    let
        val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days) + 1
    end

fun month_range(day1: int, day2: int) = 
    if day1 > day2 then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(xs: (int * int * int) list) =
    if null xs then NONE
    else let
            val later = oldest(tl xs)
        in
            if isSome later andalso is_older(valOf later, hd xs)
            then later
            else SOME(hd xs)
        end


val test1 = is_older ((1,2,3),(2,3,4)) = true 
val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test9 = what_month 70 = 3
val test10 = month_range (31, 34) = [1,2,2,2]
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
