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

fun isWeekend d : bool =
  case intToDayOfWeek (datetimeDayOfWeek d) of
    |Sunday => True
    |Saturday => True
    |_ => False

fun monthName m =
  case m of
    January => "Январь" | February=> "Февраль"  | March=> "Март"  | April=> "Апрель"  |
    May=> "Май" | June=> "Июнь"  | July=> "Июль"  | August=> "Август"  | September=> "Сентябрь" |
    October=> "Октябрь"  | November=> "Ноябрь"  | December => "Декабрь"

fun toMonth t = (fromTime t).Month

fun daysDiff t1 t2 = (((toSeconds t2) - (toSeconds t1)) / (60 * 60 * 24)) + 1

fun sameDay (t:time) (n:time) : bool =
  ((datetimeYear t) = (datetimeYear n) &&
  (datetimeMonth t) = (datetimeMonth n) &&
  (datetimeDay t) = (datetimeDay n))

fun monthGE (t:time) (n:time) : bool =
  if (datetimeYear t) = (datetimeYear n) then
    (datetimeMonth t) >= (datetimeMonth n)
  else
    if (datetimeYear t) > (datetimeYear n) then True else False

