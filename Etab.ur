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

table events : ([ Id = int ] ++ event_details)
  PRIMARY KEY Id

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

fun event15 d m = fromDatetime 2015 (m-1) d 12 0 0

task initialize = fn _ =>
  (* Cleanup *)
  dml(DELETE FROM events WHERE Id > 0);

  (* State competitions *)
  _ <- state_competition {
           Start = event15 22 07
         , Stop =  event15 26 07
         , Caption = "Стрелы Байкала"
         , City = serialize UlanUde };

  _ <- state_competition {
           Start = event15 21 08
         , Stop =  event15 24 08
         , Caption = "Турнир Палагина"
         , City = serialize Kugesi };

  _ <- state_competition {
           Start = event15 20 11
         , Stop =  event15 25 11
         , Caption = "Надежды России"
         , City = serialize Unknown };

  _ <- state_competition {
           Start = event15 03 12
         , Stop =  event15 07 12
         , Caption = "Турнир на приз Малахова"
         , City = serialize Ryazan };

  _ <- state_competition {
           Start = event15 23 12
         , Stop =  event15 27 12
         , Caption = "Памяти ЗТР Устинова-Иванова"
         , City = serialize Rybinsk };

  (* Zone *)
  _ <- zone_competition {
           Start = event15 20 02
         , Stop =  event15 22 02
         , Caption = "Уральский ФО, Свердловская область"
         , City = serialize Ekaterinburg };

  _ <- zone_competition {
           Start = event15 03 07
         , Stop =  event15 05 07
         , Caption = "Уральский ФО, Свердловская область"
         , City = serialize Ekaterinburg };

  _ <- zone_competition {
           Start = event15 15 07
         , Stop =  event15 20 07
         , Caption = "Центральный ФО"
         , City = serialize Ryazan };

  _ <- zone_competition {
           Start = event15 12 12
         , Stop =  event15 15 12
         , Caption = "Сибирский ФО, Дальневосточный ФО"
         , City = serialize Unknown };

  (* Cup *)

  _ <- state_cup {
           Start = event15 24 09
         , Stop =  event15 29 09
         , Caption = "Кубок России"
         , City = serialize Unknown };

  _ <- state_cup {
           Start = event15 03 12
         , Stop =  event15 07 12
         , Caption = "Этап 1, сезон 2015-2016"
         , City = serialize Unknown };

  _ <- state_cup {
           Start = event15 17 12
         , Stop =  event15 21 12
         , Caption = "Этап 2, сезон 2015-2016"
         , City = serialize Beloretsk };
  return {}


(*
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

*)

val pb = @@XMLW.push_back_xml
(* fun ns x m = XMLW.push_back (XMLW.nest x m) *)
fun xt m =
  XMLW.push_back (XMLW.nest (fn x=><xml><table class={
    CSS.list (Bootstrap.bs3_table :: Bootstrap.table_striped :: [])}>{x}</table></xml>) m)
fun xtrow m = XMLW.push_back (XMLW.nest (fn x=><xml><tr>{x}</tr></xml>) m)

open Prelude
open Grigorian

fun monthName m =
  case m of
    January => "Январь" | February=> "Февраль"  | March=> "Март"  | April=> "Апрель"  | May=>
    "Май" | June=> "Июнь"  | July=> "Июль"  | August=> "Август"  | September=> "Сентябрь"
    | October=> "Октябрь"  | November=> "Ноябрь"  | December => "Декабрь"

fun main {} : transaction page =
  template (
    x <- XMLW.lift (queryL (SELECT * FROM events));

    xt (
      let 
        val year = 2015
      in
        forM_ months (fn m =>
          let
            val ndays = monthLength (isLeapYear year) m 
            val days = sequence_ 1 ndays
          in
            pb <xml><tr><td colspan={31}><h3>{cdata (monthName m)}</h3></td></tr></xml>;
            xtrow (
              forM_ days (fn d =>
                pb <xml><td>{[d]}</td></xml>
              );
              forM_ (sequence_ ndays 31) (fn i =>
                pb <xml><td>.</td></xml>
              )
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

