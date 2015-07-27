(*
 ____        _
|  _ \  __ _| |_ __ _
| | | |/ _` | __/ _` |
| |_| | (_| | || (_| |
|____/ \__,_|\__\__,_|

*)

open Gregorian

datatype lang = Ru

datatype age = Youth | Adult | All

datatype eventkind = StateTournament of age | StateCup | StateCompetition | ZoneCompetition
                     | LocalCompetition | LocalTournament of age

datatype sport = A3D | Target | Field

fun sportName_ru s : string =
  case s of
    |A3D => "3D"
    |Target => "Таргет"
    |Field  => "Филд"

fun kindName_ru (e:eventkind) : (string*string) =
  case e of
    |StateTournament Youth => ("ПР", "Первенство России")
    |StateTournament Adult => ("ЧР", "Чемпионат России")
    |StateTournament All => ("ЧР", "Чемпионат России")
    |StateCup =>        ("КР", "Кубок России")
    |StateCompetition => ("ВС", "Всероссийские соревнования")
    |ZoneCompetition => ("ЗС", "Зональные соревнования")
    |LocalCompetition => ("ЛС", "Локальные соревнования")
    |LocalTournament Adult => ("ЛЧ", "Локальный Чемпионат")
    |LocalTournament Youth => ("ЛП", "Локальное Первенство")
    |LocalTournament All => ("ЛЧ", "Локальный Чемпионат")


datatype country = Russia | Bulgaia | OtherCountry of string

datatype city = Moscow | Birsk | UlanUde | Kugesi | Unknown | Ryazan | Rybinsk |
                Ekaterinburg | Beloretsk | SPB | VelikieLuki | Chita | Taganrog |
                Cheboxary | OtherCiry of string | Vladimir | Oblast of city | Cheliabinsk

fun cityName_ru c : string =
  case c of
    |Moscow => "Москва"
    |Birsk => "Бирск"
    |UlanUde => "Улан-Удэ"
    |Kugesi => "Кугести"
    |Ryazan => "Рязань"
    |Rybinsk => "Рыбинск"
    |Ekaterinburg => "Екатеринбург"
    |Beloretsk => "Белорецк"
    |SPB => "Санкт Петербург"
    |VelikieLuki => "Великие Луки"
    |Chita => "Чита"
    |Taganrog => "Таганрог"
    |Cheboxary => "Чебоксары"
    |Unknown => "По назначению"
    |Vladimir => "Владимир"
    |Oblast c => cityName_ru (case c of |Oblast x => x |y => y) ^ " (область)"
    |Cheliabinsk => "Челябинск"
    |OtherCiry x => x

con event_details = [
    Start = time
  , Stop = time
  , Caption = string
  , Country = serialized country
  , City = serialized city
  , Kind = serialized eventkind
  , Description  = string
  , Sport = serialized sport
  ]

fun eventCity [t] [t~[City]] (e : record (t ++ [City = serialized city])) : city = deserialize e.City
fun eventKind [t] [t~[Kind]] (e : record (t ++ [Kind = serialized eventkind])) : eventkind = deserialize e.Kind
fun eventCountry [t] [t~[Country]] (e : record (t ++ [Country = serialized country])) : country = deserialize e.Country
fun eventLength [t] [t~[Start,Stop]] (e : record (t ++ [Start = time, Stop = time])) : int = daysDiff e.Start e.Stop
fun eventSport [t] [t~[Sport]] (e : record (t ++ [Sport = serialized sport])) : sport = deserialize e.Sport

con _event = [ Id = int ] ++ event_details
con event = record _event

fun filterMonth (m:month)  (l:list event) : list event =
    List.filter (fn e => (m >= (toMonth e.Start)) && (toMonth e.Stop) >= m) l

fun splitMonths (l:list event) : list (list event) =
  List.mp (fn m => filterMonth m l) months

