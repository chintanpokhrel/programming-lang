fun factorial(x: int) = 
  if x = 1 then 1
  else x * factorial(x-1)

fun is_even(x: int) = 
  if x mod 2 = 0 then true
  else false

fun is_odd(x: int) =
  if x mod 2 >0 then true
  else false
