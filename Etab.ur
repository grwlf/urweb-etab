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
        {Soup.forkme_ribbon srcprj}
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

datatype age = Youth | Adult

datatype eventkind = StateTournament of age | StateCup | StateCompetition | ZoneCompetition
                     | LocalCompetition | LocalTournament of age

fun kindStyle (e:eventkind) : css_style =
  case e of
    |StateTournament _ => STYLE "background:#FFEB99"
    |StateCup => STYLE "background:#FF9900"
    |StateCompetition => STYLE "background:#FF9900"
    |ZoneCompetition => STYLE "background:#FF9900"
    |LocalCompetition => STYLE "background:#FFFFA3"
    |LocalTournament _ => STYLE "background:#FFFFA3"

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
    |StateCup =>        ("КР", "Кубок России")
    |StateCompetition => ("ВС", "Всероссийские соревнования")
    |ZoneCompetition => ("ЗС", "Зональные соревнования")
    |LocalCompetition => ("ЛС", "Локальные соревнования")
    |LocalTournament Adult => ("ЛЧ", "Локальный Чемпионат")
    |LocalTournament Youth => ("ЛП", "Локальное Первенство")


datatype country = Russia | Bulgaia | OtherCountry of string

datatype city = Moscow | Birsk | UlanUde | Kugesi | Unknown | Ryazan | Rybinsk |
                Ekaterinburg | Beloretsk | SPB | VelikieLuki | Chita | Taganrog |
                Cheboxary | OtherCiry of string

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

fun event_insert e' : transaction int =
  let 
    val e : record event_details = e' ++ { Description = "Description", Sport = serialize Target }
  in
  i <- nextval events_gen;
  dml(INSERT INTO events(Id, Start, Stop, Caption, Country, City, Kind, Description, Sport)
      VALUES({[i]}, {[e.Start]}, {[e.Stop]}, {[e.Caption]}, {[e.Country]},
             {[e.City]}, {[e.Kind]}, {[e.Description]}, {[e.Sport]}));
  return i
  end

fun state_cup e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize StateCup})

fun state_tournament_adult e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize (StateTournament Adult)})

fun state_tournament_youth e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize (StateTournament Youth)})

fun state_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize StateCompetition})

fun zone_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize ZoneCompetition})

fun local_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize LocalCompetition})

fun local_tournament e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize (LocalTournament Adult)})

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

fun caption e : string = 
  let
    val k = (kindName_ru (eventKind e)).2
  in
    if (strlen e.Caption) > 0 then e.Caption ^ " (" ^ k ^ ")" else k
  end

fun tooltip e : xbody =
  let
  in
  <xml>
  <strong>{[caption e]}</strong> <br/>
  <strong>Город:</strong> {[cityName_ru (eventCity e)]}<br/>
  <br/>
  {[e.Description]}
  </xml>
  end

fun details e : transaction xbody =
  let
    val (abbr,name) = kindName_ru (eventKind e)
    val city = cityName_ru (eventCity e)
  in
  i <- fresh ; 
  Soup.modal {
    Title=<xml><h3>{[caption e]}</h3></xml>
      , Body = <xml>{cdata e.Description}</xml>
      , Footer = <xml></xml>
      , Placeholder = 
      <xml>
        <div
          data-html="true"
          id={i}
          onmouseover={fn _ => Bootstrap.tooltip_xshow i (tooltip e)}
        >
        {if eventLength e <= 3 then
          (cdata abbr)
         else
          (cdata (abbr^":"^city))
        }
        </div>
      </xml>
  }
  end

fun links {} =
  <xml><a href={url (register_user {})}>Зарегистрироваться</a></xml> ::
  []

and register_user {} = register_user_ "" {UName="", Email="", Password="", Password2="", Capcheck=""}

and register_user_ err f : transaction page =
  let
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
            <textbox{#Capcheck} class="form-control"/><br/>
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

and main {} : transaction page =
  template (links {}) (
    q <- XMLW.lift (queryL1 (SELECT * FROM events AS E ORDER BY E.Start DESC));
    let
      val year = 2015
      val ls = splitLayers q
      val nl = LMap.size ls
    in

    i <- XMLW.lift fresh;

    pb <xml>
    <h1 data-html="true" id={i} onmouseover={fn _ => Bootstrap.tooltip_xshow i <xml>Haha<b>hah</b>aha</xml>}>
      Календарный план соревнований по стрельбе из лука на {[year]} год</h1>
    </xml>;

    xt (
      forM_ months (fn m =>
        let
          val ndays = monthLength (isLeapYear year) m
          val days = sequence_ 1 ndays
          val border = STYLE "border:1px solid #ddd"
        in
          pb <xml>
            <tr><td colspan={31}><h3>{cdata (monthName m)}</h3>
            (* {List.mapX (fn e => <xml>{[e.Id]}:{[e.Caption]}<br/></xml>) (filterMonth m q)} *)
            </td></tr>
          </xml>;

          xtrow (
            forM_ (sequence_ 1 31) (fn i =>
              if i <= ndays then
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
                                    style={kindStyle (deserialize e.Kind)}
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

    (* forM x (fn x => *)
    (*   return {} *)
    (* ); *)

    return {}
  )

