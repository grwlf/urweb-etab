(*
 _     _ _
| |   (_) |__
| |   | | '_ \
| |___| | |_) |
|_____|_|_.__/

*)


(* Iterate throw [fst..lst), assume that fst < lst *)
fun ifor [s:::Type] (f: int -> s -> s) (fst:int) (lst:int) (s:s) : s =
  if (fst < lst) then
    ifor f (fst+1) lst (f fst s)
  else
    s

fun iwhile [s:::Type] (f: s -> (s * bool)) (s:s) : s =
  let 
    val (s', ex) = f s
  in
    if ex then s' else iwhile f s'
  end

(*
 _____                    _       _
|_   _|__ _ __ ___  _ __ | | __ _| |_ ___
  | |/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \
  | |  __/ | | | | | |_) | | (_| | ||  __/
  |_|\___|_| |_| |_| .__/|_|\__,_|\__\___|
                   |_|
*)


val srcprj = bless "https://github.com/grwlf/urweb-etab"

fun template mb : transaction page =
  let
  Uru.run (
  JQuery.add (
  Bootstrap.add (
  Soup.narrow (fn nar =>
  Uru.withHeader
  <xml>
    <title>Event table</title>
    (* <link rel="icon" type="image/x-icon" href={Favicon_ico.geturl}/> *)
  </xml> (
  Uru.withBody (fn _ =>
    b <- XMLW.run mb;
    return
    <xml>
      {nar.Container
      <xml>
        {Soup.forkme_ribbon srcprj}
        <div style="text-align:center">
          {b}
        </div>
      </xml>}

      {nar.Footer
      <xml>
        <hr/>
        <p class={Bootstrap.text_muted}>
          The site is written in <a href={bless "http://impredicative.com/ur/"}>Ur/Web</a>,
          the general-purpose typed functional language.
        </p>
        <p class={Bootstrap.text_muted}>
        <ul style="padding-left: 0px; margin-top: 20px; color: #999;">
          {Soup.footer_doc_links (
          <xml><a href={srcprj}>Sources</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/cake3"}>Cake3</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/uru3"}>Uru3</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/urweb-monad-pack"}>MonadPack</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/urweb-xmlw"}>XMLW</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/urweb-soup"}>Soup</a></xml> ::
          []
          )}
        </ul>
        </p>
      </xml>}

    </xml>

    ))))))
  where
  end

(*
 ____        _
|  _ \  __ _| |_ __ _
| | | |/ _` | __/ _` |
| |_| | (_| | || (_| |
|____/ \__,_|\__\__,_|

*)

datatype eventkind = StateTournament | StateCup | StateCompetition | ZoneCompetition

fun kindStyle (e:eventkind) : css_style =
  case e of
    |StateTournament => STYLE "background:#aa0000"
    |StateCup => STYLE "background:#bb0000"
    |StateCompetition => STYLE "background:#cc0000"
    |ZoneCompetition => STYLE "background:#dd0000"

datatype country = Russia | Bulgaia | OtherCountry of string

datatype city = Moscow | Birsk | UlanUde | Kugesi | Unknown | Ryazan | Rybinsk |
                Ekaterinburg | Beloretsk | OtherCiry of string


con event_details = [
    Start = time
  , Stop = time
  , Caption = string
  , Country = serialized country
  , City = serialized city
  , Kind = serialized eventkind
  ]

con event = [ Id = int ] ++ event_details

table events : (event)
  PRIMARY KEY Id

con event = record event

sequence events_gen

fun event_insert (e : record event_details) : transaction int =
  i <- nextval events_gen;
  dml(INSERT INTO events(Id, Start, Stop, Caption, Country, City, Kind)
      VALUES({[i]}, {[e.Start]}, {[e.Stop]}, {[e.Caption]}, {[e.Country]},
             {[e.City]}, {[e.Kind]}));
  return i

fun state_cup e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize StateCup})

fun state_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize StateCompetition})

fun zone_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize ZoneCompetition})

fun mkDate d m y = fromDatetime y (m-1) d 12 0 0
fun mkDate' d m y = fromDatetime y (Datetime.monthToInt m) d 12 0 0
fun mkDate15 d m = mkDate d m 2015

task initialize = fn _ =>
  (* Cleanup *)
  dml(DELETE FROM events WHERE Id > 0);
  setval events_gen 1;

  (* State competitions *)
  _ <- state_competition {
           Start = mkDate15 22 07
         , Stop =  mkDate15 26 07
         , Caption = "Стрелы Байкала"
         , City = serialize UlanUde };

  _ <- state_competition {
           Start = mkDate15 21 08
         , Stop =  mkDate15 24 08
         , Caption = "Турнир Палагина"
         , City = serialize Kugesi };

  _ <- state_competition {
           Start = mkDate15 20 11
         , Stop =  mkDate15 25 11
         , Caption = "Надежды России"
         , City = serialize Unknown };

  _ <- state_competition {
           Start = mkDate15 03 12
         , Stop =  mkDate15 07 12
         , Caption = "Турнир на приз Малахова"
         , City = serialize Ryazan };

  _ <- state_competition {
           Start = mkDate15 23 12
         , Stop =  mkDate15 27 12
         , Caption = "Памяти ЗТР Устинова-Иванова"
         , City = serialize Rybinsk };

  (* Zone *)
  _ <- zone_competition {
           Start = mkDate15 20 02
         , Stop =  mkDate15 22 02
         , Caption = "Уральский ФО, Свердловская область"
         , City = serialize Ekaterinburg };

  _ <- zone_competition {
           Start = mkDate15 03 07
         , Stop =  mkDate15 05 07
         , Caption = "Уральский ФО, Свердловская область"
         , City = serialize Ekaterinburg };

  _ <- zone_competition {
           Start = mkDate15 15 07
         , Stop =  mkDate15 20 07
         , Caption = "Центральный ФО"
         , City = serialize Ryazan };

  _ <- zone_competition {
           Start = mkDate15 12 12
         , Stop =  mkDate15 15 12
         , Caption = "Сибирский ФО, Дальневосточный ФО"
         , City = serialize Unknown };

  (* Cup *)

  _ <- state_cup {
           Start = mkDate15 24 09
         , Stop =  mkDate15 29 09
         , Caption = "Кубок России"
         , City = serialize Unknown };

  _ <- state_cup {
           Start = mkDate15 03 12
         , Stop =  mkDate15 07 12
         , Caption = "Этап 1, сезон 2015-2016"
         , City = serialize Unknown };

  _ <- state_cup {
           Start = mkDate15 17 12
         , Stop =  mkDate15 21 12
         , Caption = "Этап 2, сезон 2015-2016"
         , City = serialize Beloretsk };

  _ <- state_cup {
           Start = mkDate15 01 03
         , Stop =  mkDate15 20 04
         , Caption = "TESTEVENT1"
         , City = serialize Beloretsk };
  return {}


(*
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

*)

fun daysDiff t1 t2 = (((toSeconds t2) - (toSeconds t1)) / (60 * 60 * 24)) + 1

val pb = @@XMLW.push_back_xml
(* fun ns x m = XMLW.push_back (XMLW.nest x m) *)
fun xt m =
  XMLW.push_back (XMLW.nest (fn x=><xml><table class={
    CSS.list (Bootstrap.bs3_table :: Bootstrap.table_striped :: [])}>{x}</table></xml>) m)
fun xtrow m = XMLW.push_back (XMLW.nest (fn x=><xml><tr>{x}</tr></xml>) m)

open Prelude
open Gregorian
open List

fun monthName m =
  case m of
    January => "Январь" | February=> "Февраль"  | March=> "Март"  | April=> "Апрель"  |
    May=> "Май" | June=> "Июнь"  | July=> "Июль"  | August=> "Август"  | September=> "Сентябрь" |
    October=> "Октябрь"  | November=> "Ноябрь"  | December => "Декабрь"


(* monthEvents (es : list event) : list (list event) *)


(* fun mapEvents [s ::: Type] (f : int -> event -> s -> s) (s:s) (n:int) (l:list event) : s = *)

fun toMonth t = (fromTime t).Month

fun filterMonth (m:month)  (l:list event) : list event =
    filter (fn e => (m >= (toMonth e.Start)) && (toMonth e.Stop) >= m) l

fun splitMonths (l:list event) : list (list event) =
  mp (fn m => filterMonth m l) months

structure LMap = AATreeMap.MkAATreeMap (struct
  type key = int
  type item = list event
end)

type lmap = LMap.t int (list event)

fun splitLayers (l: list event) : lmap =
  LMap.mapValues (fn x => List.rev x)
    (List.foldl (fn e m =>
      (iwhile (fn (i,m) =>
        (case (LMap.lookup i m) of
          |None => ((i,LMap.insert i (e::[]) m), True)
          |Some [] =>
            ((i,LMap.insert i (e::[]) m), True)
          |Some (e'::es') =>
            (if e'.Stop < e.Start then
                ((i,LMap.insert i (e::e'::es') m), True)
              else
                ((i+1,m), False)
            )
        )
      ) (0,m)).2
    ) LMap.empty l)

fun getLayer i (m:lmap) : list event =
  case (LMap.lookup i m) of |None => [] | Some x => x
  

fun main {} : transaction page =
  template (
    q <- XMLW.lift (queryL1 (SELECT * FROM events AS E ORDER BY E.Start DESC));

    xt (
      let
        val ls = splitLayers q
        val nl = LMap.size ls
        val year = 2015
      in
        forM_ months (fn m =>
          let
            val ndays = monthLength (isLeapYear year) m
            val days = sequence_ 1 ndays
          in
            pb <xml>
              <tr><td colspan={31}><h3>{cdata (monthName m)}</h3>
              {mapX (fn e => <xml>{[e.Id]}:{[e.Caption]}<br/></xml>) (filterMonth m q)}
              </td></tr>
            </xml>;

            xtrow (
              forM_ (sequence_ 1 31) (fn i =>
                if i <= ndays then
                  pb <xml><td>{[i]}</td></xml>
                else
                  pb <xml><td>x</td></xml>
              )
            );

            forM_ (sequence_ 0 (nl-1)) (fn i =>
              let
                val l = filterMonth m (getLayer i ls)
              in
                xtrow (
                  foldlM_ (fn day es =>
                    let
                      val d = mkDate' day m year
                      val eom = mkDate' ndays m year
                      val som = mkDate' 1 m year
                    in
                    if day <= ndays then
                      case es of
                        |e::es => (
                          if d >= e.Start then
                            (if d = e.Start || d = som then
                              pb
                              <xml>
                                <td colspan={min (daysDiff d eom) (daysDiff d e.Stop)}
                                    style={kindStyle (deserialize e.Kind)}>
                                  {[e.Id]}
                                </td>
                              </xml>
                            else
                              return {});
                            (if d = e.Stop then
                              return es
                            else
                              return (e::es))
                          else
                            pb <xml><td>.</td></xml>;
                            return (e::es)
                          )
                        |[]=> (
                          pb <xml><td>:</td></xml>;
                          return [])
                    else
                      pb <xml><td>x</td></xml> ;
                      return []
                  end
                  ) l (sequence_ 1 31)
                )
              end
            )
          end
        )
      end
    );


    (* forM x (fn x => *)
    (*   return {} *)
    (* ); *)

    return {}
  )

