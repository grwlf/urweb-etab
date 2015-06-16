(*
 _     _ _
| |   (_) |__
| |   | | '_ \
| |___| | |_) |
|_____|_|_.__/

*)

open Prelude
open Gregorian
open HTML5Tags
open BootstrapStyles
structure E = Error.Trans(struct con m = Basis.transaction end)

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

fun daysDiff t1 t2 = (((toSeconds t2) - (toSeconds t1)) / (60 * 60 * 24)) + 1

(*
 _____                    _       _
|_   _|__ _ __ ___  _ __ | | __ _| |_ ___
  | |/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \
  | |  __/ | | | | | |_) | | (_| | ||  __/
  |_|\___|_| |_| |_| .__/|_|\__,_|\__\___|
                   |_|
*)

val srcprj = bless "https://github.com/grwlf/urweb-etab"

fun template_ w links mb : transaction page =
  let
  Uru.run (
  JQuery.add (
  Bootstrap.add (
  Soup.layout {Width=w} (fn nar =>
  Uru.withStylesheet (Etab_css.url) (
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
        {b}
      </xml>}

      {nar.Footer
      <xml>
        <hr/>
        <p class="text-muted">
          The site is written in <a href={bless "http://impredicative.com/ur/"}>Ur/Web</a>,
          the general-purpose typed functional language.
        </p>
        <p class="text-muted">
        <ul style="padding-left: 0px; margin-top: 20px; color: #999;">
          {Soup.footer_doc_links (
          <xml><a href={srcprj}>Sources</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/cake3"}>Cake3</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/uru3"}>Uru3</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/urweb-monad-pack"}>MonadPack</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/urweb-xmlw"}>XMLW</a></xml> ::
          <xml><a href={bless "http://github.com/grwlf/urweb-soup"}>Soup</a></xml> ::
          links
          )}
        </ul>
        </p>
      </xml>}

    </xml>

    )))))))
  where
  end

val template = template_ 1200

val template_narrow = template_ 768

(*
 ____        _
|  _ \  __ _| |_ __ _
| | | |/ _` | __/ _` |
| |_| | (_| | || (_| |
|____/ \__,_|\__\__,_|

*)

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

con event = [ Id = int ] ++ event_details

table events : (event)
  PRIMARY KEY Id

con event = record event

sequence events_gen

fun event_insert_ s e' : transaction int =
  let 
    val e : record event_details = e' ++ { Sport = serialize s }
  in
  i <- nextval events_gen;
  dml(INSERT INTO events(Id, Start, Stop, Caption, Country, City, Kind, Description, Sport)
      VALUES({[i]}, {[e.Start]}, {[e.Stop]}, {[e.Caption]}, {[e.Country]},
             {[e.City]}, {[e.Kind]}, {[e.Description]}, {[e.Sport]}));
  return i
  end

fun event_insert e' : transaction int = event_insert_ Target e'
fun event_insert_3D e' : transaction int = event_insert_ A3D e'

val archery_su = show (bless "http://www.archery.su/%D0%BA%D0%B0%D0%BB%D0%B5%D0%BD%D0%B4%D0%B0%D1%80%D1%8C.html")

fun state_cup e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize StateCup})

fun state_tournament_adult e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize (StateTournament Adult)})

fun state_tournament_youth e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize (StateTournament Youth)})

fun state_competition e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize StateCompetition})

fun zone_competition e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize ZoneCompetition})

fun local_competition e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize LocalCompetition})

fun local_tournament e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize (LocalTournament Adult)})

fun local_tournament_3D e : transaction int =
  event_insert_3D (e ++ {
    Country = serialize Russia,
    Kind = serialize (LocalTournament All)})

fun mkDate d m y = fromDatetime y (m-1) d 12 0 0
fun mkDate' d m y = fromDatetime y (Datetime.monthToInt m) d 12 0 0
fun mkDate15 d m = mkDate d m 2015

task initialize = fn _ =>
  (* Cleanup *)
  dml(DELETE FROM events WHERE Id > 0);
  setval events_gen 1;

  (* State tournaments (Adult) *)
  _ <- state_tournament_adult {
           Start = mkDate15 01 02
         , Stop =  mkDate15 06 02
         , Caption = ""
         , City = serialize VelikieLuki };

  _ <- state_tournament_adult {
           Start = mkDate15 25 06
         , Stop =  mkDate15 30 06
         , Caption = ""
         , City = serialize Unknown };

  _ <- state_tournament_adult {
           Start = mkDate15 18 08
         , Stop =  mkDate15 24 08
         , Caption = ""
         , City = serialize Chita };

  (* State tournaments (Youth) *)

  _ <- state_tournament_youth {
           Start = mkDate15 06 02
         , Stop =  mkDate15 11 02
         , Caption = ""
         , City = serialize VelikieLuki };

  _ <- state_tournament_youth {
           Start = mkDate15 25 04
         , Stop =  mkDate15 30 04
         , Caption = ""
         , City = serialize Unknown };

  _ <- state_tournament_youth {
           Start = mkDate15 10 08
         , Stop =  mkDate15 16 08
         , Caption = ""
         , City = serialize Cheboxary };

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
           Start = mkDate15 27 01
         , Stop =  mkDate15 31 01
         , Caption = ""
         , City = serialize VelikieLuki };

  _ <- state_cup {
           Start = mkDate15 19 04
         , Stop =  mkDate15 24 04
         , Caption = ""
         , City = serialize Taganrog };

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

  (* Local *)
  _ <- local_tournament
         { Start = mkDate15 31 05
         , Stop =  mkDate15 31 05
         , Caption = "Чемпионат Московской области"
         , City = serialize Moscow };

  (* 3D *)
  _ <- local_tournament_3D
         { Start = mkDate15 13 06
         , Stop =  mkDate15 13 06
         , Caption = "Четыре сезона - июньский рубеж"
         , City = serialize (Oblast Moscow)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=10788.0"};

  _ <- local_tournament_3D
         { Start = mkDate15 11 07
         , Stop =  mkDate15 12 07
         , Caption = "Gorbatka Open"
         , City = serialize (Oblast Vladimir)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=10105.0" };

  _ <- local_tournament_3D
         { Start = mkDate15 04 07
         , Stop =  mkDate15 04 07
         , Caption = "Чемпионат Москвы"
         , City = serialize Moscow
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11016.0" };

  _ <- local_tournament_3D
         { Start = mkDate15 27 06
         , Stop =  mkDate15 28 06
         , Caption = "Гран-при Евразии"
         , City = serialize Ekaterinburg
         , Description = "http://www.bowmania.ru/forum/index.php?topic=9850.0" };

  _ <- local_tournament_3D
         { Start = mkDate15 06 07
         , Stop =  mkDate15 07 07
         , Caption = "Кубок России, 2й этап"
         , City = serialize Ekaterinburg
         , Description = "http://www.bowmania.ru/forum/index.php?topic=10650.0" };

  _ <- local_tournament_3D
         { Start = mkDate15 12 07
         , Stop =  mkDate15 12 07
         , Caption = "Уральская заимка, 2й этап"
         , City = serialize (Oblast Cheliabinsk)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11115.0" };

  _ <- local_tournament_3D
         { Start = mkDate15 13 09
         , Stop =  mkDate15 13 09
         , Caption = "Уральская заимка, 3й этап"
         , City = serialize (Oblast Cheliabinsk)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11115.0" };

  _ <- local_tournament_3D
         { Start = mkDate15 11 10
         , Stop =  mkDate15 11 10
         , Caption = "Уральская заимка, 4й этап"
         , City = serialize (Oblast Cheliabinsk)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11115.0" };

  return {}


(*
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

*)

val pb = @@XMLW.push_back_xml
fun xt m = XMLW.push_back (XMLW.nest (fn x=><xml><table class="bs3-table table-striped">{x}</table></xml>) m)
fun xtrow m = XMLW.push_back (XMLW.nest (fn x=><xml><tr>{x}</tr></xml>) m)

fun monthName m =
  case m of
    January => "Январь" | February=> "Февраль"  | March=> "Март"  | April=> "Апрель"  |
    May=> "Май" | June=> "Июнь"  | July=> "Июль"  | August=> "Август"  | September=> "Сентябрь" |
    October=> "Октябрь"  | November=> "Ноябрь"  | December => "Декабрь"

fun toMonth t = (fromTime t).Month

fun filterMonth (m:month)  (l:list event) : list event =
    List.filter (fn e => (m >= (toMonth e.Start)) && (toMonth e.Stop) >= m) l

fun splitMonths (l:list event) : list (list event) =
  List.mp (fn m => filterMonth m l) months

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

fun caption e : (string * string) = 
  let
    val (abbr,k) = (kindName_ru (eventKind e))
    val s = eventSport e
    val a3d = sportName_ru A3D
  in
    case s of
      |A3D => (a3d, if (strlen e.Caption) > 0 then e.Caption else sportName_ru s)
      |_=> ("КЛ/БЛ", if (strlen e.Caption) > 0 then e.Caption ^ " (" ^ k ^ ")" else k)
  end

fun unwords_url [ctx] [[Body] ~ ctx] (s : string) : xml ([Body] ++ ctx) [] [] =
  let
    val (s1,s2) = (case String.split s #" " of |None => (s,"") |Some (s1, s2) => (s1,s2))
  in
      let
        val (p:bool) = (String.isPrefix {Full=s1, Prefix="http://" }) ||
                       (String.isPrefix {Full=s1, Prefix="https://"})
      in
        if s1 = "" then  <xml/> else
          let
            val next = <xml>{unwords_url s2}</xml>
          in
          case p of
            |True => (case checkUrl s1 of
              |Some u => <xml><a href={u}>{[s1]}</a> {next}</xml>
              |None => <xml>{[s1]} (ссылка) {next}</xml>)
            |False => <xml>{[s1]} {next}</xml>
          end
      end
  end

fun tooltip e : xbody =
  let
  in
  <xml>
  <strong>{[(caption e).2]}</strong> <br/>
  <strong>Город:</strong> {[cityName_ru (eventCity e)]}<br/>
  </xml>
  end

fun details e : transaction xbody =
  let
    val (abbr,name) = caption e
    val city = cityName_ru (eventCity e)
  in
  i <- fresh ; 
  Soup.modal {
    Title=<xml><h3>{[name]}</h3></xml>
      , Body = <xml>{unwords_url e.Description}</xml>
      , Footer = <xml></xml>
      , Placeholder = 
      <xml>
        <div
          data-html="true"
          id={i}
          onmouseover={fn _ => Bootstrap.tooltip_xshow i (tooltip e)}
          style="text-align:center"
        >
        {case eventLength e of
          |1=>(cdata abbr) |2=> (cdata abbr) |_=> (cdata (abbr^": "^city))
        }
        </div>
      </xml>
  }
  end

fun links {} =
  <xml><a href={url (register_user {})}>Зарегистрироваться</a></xml> ::
  []

and register_user {} = 
  let
    register_user_ "" {UName="", Email="", Password="", Password2="", Capcheck=""}
  where

    fun register_handler (cid:int) (f:{UName: string, Email:string, Password:string,
                             Password2:string, Capcheck:string}) : transaction page =
      capt_ok <- Captcha.check_free f.Capcheck cid;
      e <- E.run (
        Prelude.when (f.Password <> f.Password2) (E.fail "Пароли не совпадают");
        Prelude.when (not capt_ok) (E.fail "Неверный текст");
        return {}
      );
      case e of
        | Error.ERight {} => redirect (url (register_user {}))
        | Error.ELeft e => register_user_ e f

    and register_user_ err f : transaction page =
      let
      in
      template_narrow (links {}) (
        psw <- XMLW.lift fresh;
        eml <- XMLW.lift fresh;
        cid <- XMLW.lift (Captcha.allocate {});
        pb
        <xml>
          <h1>Регистрация нового пользователя</h1>
          {if err <> "" then
          <xml><div class="alert alert-danger">
            <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
            <span class="sr-only">Ошибка:</span>
            {[err]}
          </div></xml> else <xml/>}
          <form class={form_horizontal} style="text-align:left">

            <div class="form-group">
              <label for={eml} class="col-sm-2 control-label">Имя пользователя</label>
              <div class="col-sm-10">
                <textbox{#UName} class="form-control" placeholder="Имя пользователя" value={f.UName}/>
              </div>
            </div>

            <div class="form-group">
              <label for={eml} class="col-sm-2 control-label">Эл. почта</label>
              <div class="col-sm-10">
                <email{#Email} class="form-control" id={eml} placeholder="Email" value={f.Email}/>
              </div>
            </div>

            <div class="form-group">
              <label for={psw} class="col-sm-2 control-label">Пароль</label>
              <div class="col-sm-10">
                <password{#Password} class="form-control" id={psw} placeholder="Пароль" value={f.Password}/>
              </div>
            </div>

            <div class="form-group">
              <label class="col-sm-2 control-label">Пароль (повтор)</label>
              <div class="col-sm-10">
                <password{#Password2} class="form-control" placeholder="Пароль (повтор)" value={f.Password2}/>
              </div>
            </div>

            <div class="form-group">
              <label class="col-sm-2 control-label">Введите текст с картинки</label>
              <div class="col-sm-4">
                <textbox{#Capcheck} class="form-control" placeholder="Строчные латинские буквы"/><br/>
                <img src={url (Captcha.blob cid)} alt="captcha"/>
              </div>
              <div class="col-sm-6"/>
            </div>

            <div class="form-group">
              <div class="col-sm-2"/>
              <div class="col-sm-10">
              <submit action={register_handler cid} class="btn btn-default" value="Отправить"/>
              </div>
            </div>

          </form>
        </xml>;
        return {}
      )
      end
  end

and main {} : transaction page =
  template (links {}) (
    q <- XMLW.lift (queryL1 (SELECT * FROM events AS E ORDER BY E.Start DESC));
    let
      val year = 2015
      val ls = splitLayers q
      val nl = LMap.size ls
    in

    pb <xml>
    <h1>
      Календарный план соревнований по стрельбе из лука на {[year]} год</h1>
    </xml>;

    xt (
      forM_ months (fn m =>
        let
          val ndays = monthLength (isLeapYear year) m
          val days = sequence_ 1 ndays
          val border = STYLE "border:1px solid #ddd"
          val border_we = STYLE "border:1px solid #ddd; background:#ddd"
        in
          pb <xml>
            <tr><td colspan={31}><h3>{cdata (monthName m)}</h3>
            </td></tr>
          </xml>;

          xtrow (
            forM_ (sequence_ 1 31) (fn i =>
              if i <= ndays then
                if isWeekend (mkDate' i m year) then
                  pb <xml><td class="text-muted" style={border_we}>{[i]}</td></xml>
                else
                  pb <xml><td class="text-muted" style={border}>{[i]}</td></xml>
              else
                pb <xml><td style={border}> </td></xml>
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
                            let
                            in
                              m <- XMLW.lift (details e);
                              pb
                              <xml>
                                <td colspan={min (daysDiff d eom) (daysDiff d e.Stop)}
                                    style={
                                      case (deserialize e.Kind, deserialize e.Sport) of
                                        |(_,A3D) =>STYLE "background:#33CC33"
                                        |(StateTournament _,_) => STYLE "background:#FFEB99"
                                        |(StateCup,_) => STYLE "background:#FF9900"
                                        |(StateCompetition,_) => STYLE "background:#FF9900"
                                        |(ZoneCompetition,_) => STYLE "background:#FF9900"
                                        |(LocalCompetition,_) => STYLE "background:#FFFFA3"
                                        |(LocalTournament _,_) => STYLE "background:#FFFFA3"
                                    }
                                    data-container="body"
                                >
                                  {m}
                                </td>
                              </xml>
                            end
                          else
                            return {});
                          (if d = e.Stop then
                            return es
                          else
                            return (e::es))
                        else
                          pb <xml><td style={border}>&nbsp;</td></xml>;
                          return (e::es)
                        )
                      |[]=> (
                        pb <xml><td style={border}>&nbsp;</td></xml>;
                        return [])
                  else
                    pb <xml><td style={border}>&nbsp;</td></xml> ;
                    return []
                end
                ) l (sequence_ 1 31)
              )
            end
          )
        end
      );

      pb <xml><tr><td colspan={31}/></tr></xml>

    )
    end;

    return {}
  )

