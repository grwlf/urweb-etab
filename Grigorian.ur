
(* Is this year a leap year according to the proleptic Gregorian calendar? *)
fun isLeapYear (y : int) : bool = 
  (mod y 4 = 0) && ((mod y 400 = 0) || not (mod y 100 = 0))

datatype month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec

val months = Jan :: Feb :: Mar :: Apr :: May :: Jun :: Jul :: Aug :: Sep :: Oct :: Nov :: Dec :: []

fun numOfMonth m =
  case m of
    Jan => 1 | Feb=> 2  | Mar=> 3  | Apr=> 4  | May=> 5  | Jun=> 6  | Jul=> 7  |
    Aug=> 8  | Sep=> 9  | Oct=> 10  | Nov=> 11  | Dec => 12

fun monthLength isleap m = 
  case m of
    Jan => 31 | Feb=> if isleap then 29 else 28  | Mar=> 31  | Apr=> 30  | May=>
    31| Jun=> 30  | Jul=> 31  | Aug=> 31  | Sep=> 30  | Oct=> 31  | Nov=> 30  | Dec => 31

datatype daysOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat


