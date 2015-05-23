open Datetime

(* Is this year a leap year according to the proleptic Gregorian calendar? *)
fun isLeapYear (y : int) : bool = 
  (mod y 4 = 0) && ((mod y 400 = 0) || not (mod y 100 = 0))

val months = January :: February :: March :: April :: May :: June :: July ::
         August :: September :: October :: November :: December :: []

fun monthLength isleap m = 
  case m of
    January => 31 | February => if isleap then 29 else 28  | March=> 31  |
    April=> 30  | May=> 31| June=> 30  | July=> 31  | August=> 31  |
    September=> 30  | October=> 31 | November=> 30  | December => 31

val ord_month = mkOrd { Lt = fn a b => monthToInt a < monthToInt b,
                           Le = fn a b => monthToInt a <= monthToInt b }
