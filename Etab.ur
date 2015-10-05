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
open Types

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

(*
 _____                    _       _
|_   _|__ _ __ ___  _ __ | | __ _| |_ ___
  | |/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \
  | |  __/ | | | | | |_) | | (_| | ||  __/
  |_|\___|_| |_| |_| .__/|_|\__,_|\__\___|
                   |_|
*)

style bg_3d
style bg_tournament
style bg_open

style icon_bar

style wide
style brand

style addbutton
style addlabel
style strike

val srcprj = bless "https://github.com/grwlf/urweb-etab"

val donate = Unsafe.s2xbody
  "<iframe frameborder='0' allowtransparency='true' scrolling='no' src='https://money.yandex.ru/embed/donate.xml?account=41001443664241&quickpay=donate&payment-type-choice=off&default-sum=100&targets=%D0%9F%D0%BE%D0%B4%D0%B4%D0%B5%D1%80%D0%B6%D0%BA%D0%B0+%D0%BF%D1%80%D0%BE%D0%B5%D0%BA%D1%82%D0%B0&target-visibility=on&project-name=ArcheryDays.ru&project-site=ArcheryDays.ru&button-text=05&successURL=ArcheryDays.ru' width='512' height='132'></iframe>"

(* To avoid clashes with 'nav' css_class *)
val nav_ = @@NavTag.nav
(* Re-introducing Basis.hidden from tag *)
val hidden = @@Basis.hidden

fun template_ w links mb : transaction page =
  let
  Uru.run (
  JQuery.add (
  Bootstrap.add (
  Soup.layout {Width=w} (fn nar =>
  Uru.withStylesheet (Etab_css.url) (
  Uru.withHeader
  <xml>
    <title>Календарный план соревнований</title>
    <link rel="icon" type="image/x-icon" href={Etab_ico.geturl}/>
    <link href="http://fonts.googleapis.com/css?family=Lobster&subset=latin,cyrillic" rel="stylesheet" type="text/css"/>
    {Analytics.insert "UA-55678474-3"}
  </xml> (
  Uru.withBody (fn _ =>
    b <- XMLW.run mb;
    x <- fresh;
    n <- fresh;
    return
    <xml>

      (*
      <nav_ class="navbar navbar-default navbar-fixed-top">
      <div class="container" style={oneProperty (STYLE "") (value (property "max-width") (atom ((show w) ^ "px")))}>
        (* <div class="navbar-header"> *)
        (*   <a class="brand navbar-brand" href={links.Main}>AD.RU</a> *)
        (* </div> *)
        <div id={n} class="navbar-collapse collapse">
          <ul class="nav navbar-nav">
            (* <li><a href={links.Report}>Сообщить о соревновании</a></li> *)
            (* <li class="dropdown"> *)
            (*   <a href={links.Main} class="dropdown-toggle" data-toggle="dropdown" role="button" aria-haspopup="true" aria-expanded="false"> *)
            (*     Действия<span class="caret"></span> *)
            (*   </a> *)
            (*   <ul class="dropdown-menu"> *)
            (*     <li><a href={links.Main}>Action</a></li> *)
            (*     <li><a href={links.Main}>Another action</a></li> *)
            (*     <li><a href={links.Main}>Something else here</a></li> *)
            (*     <li role="separator" class="divider"></li> *)
            (*     <li class="dropdown-header">Nav header</li> *)
            (*     <li><a href={links.Main}>Separated link</a></li> *)
            (*     <li><a href={links.Main}>One more separated link</a></li> *)
            (*   </ul> *)
            (* </li> *)
          </ul>
          <ul class="nav navbar-nav navbar-right">
            (* <li><a href={links.Main}>О проекте</a></li> *)
            (* <li><a href={links.Main}>Обратная связь</a></li> *)
          </ul>
        </div><!--/.nav-collapse -->
      </div>
      </nav_>
      *)

      {nar.Header wide
      <xml>
      </xml>}

      {nar.Container
      <xml>
        {b}
      </xml>}

      {nar.Footer
      <xml>
        <hr/>
        (* <p class="text-muted"> *)
        (*   Сайт разработан с помощью <a href={bless "http://impredicative.com/ur/"}>Ur/Web</a>. *)
        (* </p> *)
        <p class="text-muted">
        <ul style="padding-left: 0px; margin-top: 20px; color: #999;">
          {Soup.footer_doc_links ( links.Bottom  )}
        </ul>
        (* {donate} *)
        </p>
      </xml>}

    </xml>

    )))))))
  where
  end

val template = template_ 1200

val template_narrow = template_ 768


table events : _event
  PRIMARY KEY Id

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

fun city_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize CityCompetition})

fun club_competition e : transaction int =
  event_insert (e ++ {
    Country = serialize Russia,
    Kind = serialize ClubCompetition})

fun city_tournament e : transaction int =
  event_insert (e ++ {
    Description = archery_su,
    Country = serialize Russia,
    Kind = serialize (CityTournament Adult)})

fun city_tournament_3D e : transaction int =
  event_insert_3D (e ++ {
    Country = serialize Russia,
    Kind = serialize (CityTournament All)})

task initialize = fn _ =>
  (* Check env *)
  _ <- Email.env {};

  (* Cleanup *)
  dml(DELETE FROM events WHERE Id > 0);
  setval events_gen 1;

  (*{{{ Initial data *)
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
           Start = mkDate15 03 10
         , Stop =  mkDate15 08 10
         , Caption = "Кубок России"
         , City = serialize Taganrog };

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

  (* City *)
  _ <- city_tournament
         { Start = mkDate15 31 05
         , Stop =  mkDate15 31 05
         , Caption = "Чемпионат Московской области"
         , City = serialize Moscow };

  _ <- city_competition
         { Start = mkDate15 30 08
         , Stop =  mkDate15 30 08
         , Caption = "Чемпионат Воронежа"
         , City = serialize Voronezh
         , Description = ""};

  _ <- city_competition
         { Start = mkDate15 10 12
         , Stop =  mkDate15 13 12
         , Caption = "Кубок Москвы"
         , City = serialize Moscow
         , Description = "18  метров"};

  (* Club *)

  _ <- club_competition
         { Start = mkDate15 26 09
         , Stop =  mkDate15 26 09
         , Caption = "Соревнования в Хантере"
         , City = serialize Moscow
         , Description = "Соревнования в Хантере"};

  _ <- club_competition
         { Start = mkDate15 10 10
         , Stop =  mkDate15 10 10
         , Caption = "Варяг"
         , City = serialize Moscow
         , Description = "Спорт в Варяе"};

  _ <- club_competition
         { Start = mkDate15 18 10
         , Stop =  mkDate15 18 10
         , Caption = "Варяг"
         , City = serialize Moscow
         , Description = "Традиция в Варяе"};

  _ <- club_competition
         { Start = mkDate15 24 10
         , Stop =  mkDate15 24 10
         , Caption = "Бункер"
         , City = serialize Moscow
         , Description = "Бункер"};


  _ <- club_competition
         { Start = mkDate15 10 10
         , Stop =  mkDate15 10 10
         , Caption = "Тверской"
         , City = serialize Moscow
         , Description = ""};

  _ <- club_competition
         { Start = mkDate15 07 11
         , Stop =  mkDate15 07 11
         , Caption = "Бункер 1"
         , City = serialize Moscow
         , Description = ""};

  _ <- club_competition
         { Start = mkDate15 28 11
         , Stop =  mkDate15 28 11
         , Caption = "Тверской 2"
         , City = serialize Moscow
         , Description = ""};

  _ <- club_competition
         { Start = mkDate15 05 12
         , Stop =  mkDate15 05 12
         , Caption = "Бункер 2"
         , City = serialize Moscow
         , Description = ""};

  (* 3D *)
  _ <- city_tournament_3D
         { Start = mkDate15 13 06
         , Stop =  mkDate15 13 06
         , Caption = "Четыре сезона - июньский рубеж"
         , City = serialize (Oblast Moscow)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=10788.0"};

  _ <- city_tournament_3D
         { Start = mkDate15 11 07
         , Stop =  mkDate15 12 07
         , Caption = "Gorbatka Open"
         , City = serialize (Oblast Vladimir)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=10105.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 04 07
         , Stop =  mkDate15 04 07
         , Caption = "Чемпионат Москвы"
         , City = serialize Moscow
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11016.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 27 06
         , Stop =  mkDate15 28 06
         , Caption = "Гран-при Евразии"
         , City = serialize Ekaterinburg
         , Description = "http://www.bowmania.ru/forum/index.php?topic=9850.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 06 07
         , Stop =  mkDate15 07 07
         , Caption = "Кубок России, 2й этап"
         , City = serialize Ekaterinburg
         , Description = "http://www.bowmania.ru/forum/index.php?topic=10650.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 12 07
         , Stop =  mkDate15 12 07
         , Caption = "Уральская заимка, 2й этап"
         , City = serialize (Oblast Cheliabinsk)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11115.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 13 09
         , Stop =  mkDate15 13 09
         , Caption = "Уральская заимка, 3й этап"
         , City = serialize (Oblast Cheliabinsk)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11115.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 11 10
         , Stop =  mkDate15 11 10
         , Caption = "Уральская заимка, 4й этап"
         , City = serialize (Oblast Cheliabinsk)
         , Description = "http://www.bowmania.ru/forum/index.php?topic=11115.0" };

  _ <- city_tournament_3D
         { Start = mkDate15 17 10
         , Stop =  mkDate15 17 10
         , Caption = "4 Сезона"
         , City = serialize Moscow
         , Description = "" };


  (*
   ____   ___  _  __
  |___ \ / _ \/ |/ /_
    __) | | | | | '_ \
   / __/| |_| | | (_) |
  |_____|\___/|_|\___/

  *)

  (* Club *)


  (*}}}*)

  return {}


(*
 __  __       _
|  \/  | __ _(_)_ __
| |\/| |/ _` | | '_ \
| |  | | (_| | | | | |
|_|  |_|\__,_|_|_| |_|

*)

(*{{{ Utils *)
val pb = @@XMLW.push_back_xml
fun xt m = XMLW.push_back (XMLW.nest (fn x=><xml><table class="bs3-table table-striped">{x}</table></xml>) m)
fun xtrow m = XMLW.push_back (XMLW.nest (fn x=><xml><tr>{x}</tr></xml>) m)

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
            (if (addDays e'.Stop 3) < e.Start then
                (* add the event to the current layer *)
                ((i,LMap.insert i (e::e'::es') m), True)
              else
                (if (e'.Stop < e.Start) && (daysDiff e'.Start e'.Stop) > 3 then
                  (* add the event to the current layer *)
                  ((i,LMap.insert i (e::e'::es') m), True)
                 else
                  (* move to the next layer *)
                  ((i+1,m), False)
                )
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
      |_=> ("К/Б", if (strlen e.Caption) > 0 then e.Caption ^ " (" ^ k ^ ")" else k)
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

fun capform [x:::{Type}] [x ~ [Capcheck=string,CapId=string]] {} : transaction (
  xml ([Body = (), Dyn = (), Form = ()]) x ([Capcheck = string, CapId = string])) =
  cid <- Captcha.allocate {};
  return
  <xml>
    <div class="form-group">
      <div class="col-sm-2"/>
      <div class="col-sm-4">
        <img src={url (Captcha.blob cid)} alt="captcha"/>
      </div>
      <div class="col-sm-6"/>
    </div>

    <div class="form-group">
      <label class="col-sm-2 control-label">Слово с картинки</label>
      <div class="col-sm-4">
        <textbox{#Capcheck} class="form-control" placeholder="Строчные латинские буквы"/><br/>
      </div>
      <div class="col-sm-6"/>
    </div>
    <hidden{#CapId} value={show cid}/>

  </xml>

(*}}}*)

(*{{{ Links*)
fun links {} = {
      Report = url (report_comp {}),
      Main = url (main {}),
      Bottom =
        (* <xml><a href={url (register_user {})}>Зарегистрироваться</a></xml> :: *)
        <xml><a href={url (contact_us {})}>Обратная связь</a></xml> ::
        []
    }
(*}}}*)

(*{{{ Report competition *)
and report_comp {} =
  let
    report_comp_ "" {Email="", Capcheck="", CapId="", Message="", Message2=""}
  where

    fun report_handler f : transaction page =
      capt_ok <- Captcha.check_free f.Capcheck (readError f.CapId);
      e <- E.run (
        Prelude.when (f.Message = "") (E.fail "Сообщение не должно быть пустым");
        Prelude.when (f.Message2 = "") (E.fail "Сообщение не должно быть пустым");
        Prelude.when ((strlen f.Message) > 1024) (E.fail "Сообщение слишком длинное");
        Prelude.when (f.Email = "") (E.fail "Укажите адрес Вашей электронной почты");
        Prelude.when (not capt_ok) (E.fail "Текст не совпадает с картинкой");
        return {}
      );
      case e of
        | Error.ERight {} =>
            b <- Email.send "ierton@gmail.com" f.Email ("Etab message from " ^ f.Email) f.Message;
            sent_message_report b
        | Error.ELeft e => report_comp_ e f

    and report_comp_ err f : transaction page =

      template_narrow (links {}) (

        (* cap <- XMLW.lift (capform {}); *)

        let
          fun sect cls x = <xml><div class="row"><div class="col-sm-12"><div class={cls}>{x}</div></div></div></xml>
        in

        pb <xml>
        <div class="row">
          <div class="col-sm-12">
          <h1>Сообщить о соревновании</h1>
         </div>
        </div>
        <div class="row" style="text-align:center">
          <div class="col-sm-4">
            {sect addbutton <xml>
            <a class="btn btn-lg btn-primary btn-block" link={main {}}>Указать ссылку</a>
            </xml>}
            {sect addlabel <xml>
              <p class="text-muted">Указать ссылку на тему форума или электронный документ с "Вызовом"</p>
            </xml>}
          </div>
          <div class="col-sm-4">
            {sect addbutton <xml>
              <a class="btn btn-lg btn-primary btn-block" link={main {}}>Загрузить документ</a>
            </xml>}
            {sect addlabel <xml>
              <span class="text-muted">Загрузить электронный документ (Word, PDF), содержащий "Вызов"</span>
            </xml>}
          </div>
          <div class="col-sm-4">
            {sect addbutton <xml>
              <a class="btn btn-lg btn-primary btn-block" link={main {}}>Заполнить форму</a>
            </xml>}
            {sect addlabel <xml>
              <span class="text-muted">Заполнить форму, указав дату начала и окончания соревнования, текст вызова</span>
            </xml>}
          </div>
        </div>

        (* xxxxxx *)
        (* <div class="strike"> *)
        (*    <span>Kringle</span> *)
        (* </div> *)
        (* aaaaaaa *)

        </xml>

        end;

        (* pb <xml> *)
        (*   <h1>Сообщить о соревновании</h1> *)
        (*   {if err <> "" then *)
        (*   <xml><div class="alert alert-danger"> *)
        (*     <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span> *)
        (*     <span class="sr-only">Ошибка:</span> *)
        (*     {[err]} *)
        (*   </div></xml> else <xml/>} *)

        (*   <form class={form_horizontal} style="text-align:left"> *)
        (*     <div class="form-group"> *)
        (*       <label class="col-sm-2 control-label">Эл.почта отправителя</label> *)
        (*       <div class="col-sm-10"> *)
        (*         <email{#Email} class="form-control" placeholder="Email" value={f.Email}/> *)
        (*       </div> *)
        (*     </div> *)

        (*     <div class="form-group"> *)
        (*       <label class="col-sm-2 control-label">Текст сообщения</label> *)
        (*       <div class="col-sm-10"> *)
        (*         <textarea{#Message} class="form-control">{cdata f.Message}</textarea> *)
        (*       </div> *)
        (*     </div> *)

        (*     <div class="form-group"> *)
        (*       <label class="col-sm-2 control-label">Текст сообщения</label> *)
        (*       <div class="col-sm-10"> *)
        (*         <textarea{#Message2} class="form-control">{cdata f.Message2}</textarea> *)
        (*       </div> *)
        (*     </div> *)

        (*     {cap} *)

        (*     <div class="form-group"> *)
        (*       <div class="col-sm-2"/> *)
        (*       <div class="col-sm-10"> *)
        (*       <submit action={report_handler} class="btn btn-default" value="Отправить"/> *)
        (*       </div> *)
        (*     </div> *)

        (*   </form> *)

        (* </xml>; *)

        return {}
      )

  end
(*}}}*)

and sent_message_report (b:xbody) =
  template_narrow (links {}) (
    pb <xml>
      <div style="text-align:center">
        <p>{b}</p>
        <p><a link={main {}}>На главную</a></p>
      </div>
    </xml>;
    return {}
  )

(*{{{ Contact us *)
and contact_us {} = 
  let
    contact_us_ "" {Email="", Capcheck="", CapId="", Message=""}
  where

    fun contact_handler (f:{Email:string, Capcheck:string, CapId:string, Message:string}) : transaction page =
      capt_ok <- Captcha.check_free f.Capcheck (readError f.CapId);
      e <- E.run (
        Prelude.when (f.Message = "") (E.fail "Сообщение не должно быть пустым");
        Prelude.when ((strlen f.Message) > 1024) (E.fail "Сообщение слишком длинное");
        Prelude.when (f.Email = "") (E.fail "Укажите адрес Вашей электронной почты");
        Prelude.when (not capt_ok) (E.fail "Текст не совпадает с картинкой");
        return {}
      );

      case e of
        | Error.ERight {} =>
            b <- Email.send "ierton@gmail.com" f.Email ("Etab message from " ^ f.Email) f.Message;
            sent_message_report b
        | Error.ELeft e => contact_us_ e f

    and contact_us_ err f : transaction page =

      template_narrow (links {}) (

        cap <- XMLW.lift (capform {});

        pb <xml>
          <h1>Обратная связь</h1>
          {if err <> "" then
          <xml><div class="alert alert-danger">
            <span class="glyphicon glyphicon-exclamation-sign" aria-hidden="true"></span>
            <span class="sr-only">Ошибка:</span>
            {[err]}
          </div></xml> else <xml/>}

          <form class={form_horizontal} style="text-align:left">
            <div class="form-group">
              <label class="col-sm-2 control-label">Эл.почта отправителя</label>
              <div class="col-sm-10">
                <email{#Email} class="form-control" placeholder="Email" value={f.Email}/>
              </div>
            </div>

            <div class="form-group">
              <label class="col-sm-2 control-label">Текст сообщения</label>
              <div class="col-sm-10">
                <textarea{#Message} class="form-control">{cdata f.Message}</textarea>
              </div>
            </div>

            {cap}

            <div class="form-group">
              <div class="col-sm-2"/>
              <div class="col-sm-10">
              <submit action={contact_handler} class="btn btn-default" value="Отправить"/>
              </div>
            </div>

          </form>

        </xml>;

        return {}
      )

  end
(*}}}*)

(*
(*{{{ Register user *)
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
  (*}}}*)
*)

(*{{{ Main *)
and main_ o : transaction page =
  let
    val year = o.Year
  in
  template (links {}) (
    now <- XMLW.lift Basis.now;
    q <- XMLW.lift (queryL1 (SELECT * FROM events AS E WHERE
      E.Start >= {[mkDate 1 1 year]} AND E.Stop < {[mkDate 1 1 (year+1)]} ORDER BY E.Start DESC));
    let
      val ls = splitLayers q
      val nl = LMap.size ls
    in
    pb (
    <xml>
    </xml>
    );

    pb <xml>
      <a href={url (main_ (o -- #Year ++ {Year=2015}))}                         >2015</a>
      <a href={url (main_ (o -- #Year ++ {Year=2016}))} style="margin-left:30px">2016</a>
      <h1>Календарный план соревнований по стрельбе из лука на {[year]} год</h1>
    </xml>;

    (*
    pb (
    <xml>
      <a href={url (report_comp {})} style="margin-left:30px">Сообщить о соревновании</a>
    </xml>
    );
    *)

    pb (
      if o.ShowPastMonths then
        <xml>
          <a href={url (main_ {ShowPastMonths=False, Year=o.Year})}>Скрыть прошедшие месяцы</a>
        </xml>
      else
        <xml>
          <a href={url (main_ {ShowPastMonths=True, Year=o.Year})}>Показать прошедшие месяцы</a>
        </xml>
    );

    xt (
      forM_ months (fn m =>
        let
          val ndays = monthLength (isLeapYear year) m
          val days = sequence_ 1 ndays
          val border = STYLE "border:1px solid #ddd"
          val border_we = STYLE "border:1px solid #ddd; background:#ddd"
          val border_now = STYLE "border:3px solid #f88; background:#fdd"
        in
          if (not o.ShowPastMonths) && (not (monthGE (mkDate' 1 m year) now)) then return {} else
          pb <xml>
            <tr><td style="border-top:0px" colspan={31}><h3>{cdata (monthName m)}</h3>
            </td></tr>
          </xml>;

          (* Day-of-month header, showing weekands and the current day *)
          xtrow (
            forM_ (sequence_ 1 31) (fn i =>
            let
              val d = mkDate' i m year
            in
              if i <= ndays then
                (if sameDay now d then
                  pb <xml><td class="text-muted" style={border_now}>{[i]}</td></xml>
                else
                  (if isWeekend d then
                    pb <xml><td class="text-muted" style={border_we}>{[i]}</td></xml>
                  else
                    pb <xml><td class="text-muted" style={border}>{[i]}</td></xml>))
              else
                pb <xml><td style={border}> </td></xml>
            end
            )
          );

          (* Render each layer of events one by one *)
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
                              (* render the event cells *)
                              pb
                              <xml>
                                <td colspan={min (daysDiff d eom) (daysDiff d e.Stop)}
                                    class={
                                      case (deserialize e.Kind, deserialize e.Sport) of
                                        |(_,A3D) =>bg_3d
                                        |(StateTournament _,_) => bg_tournament
                                        |(StateCup,_) => bg_open
                                        |(StateCompetition,_) => bg_open
                                        |(ZoneCompetition,_) => bg_open
                                        |(CityCompetition,_) => bg_open
                                        |(CityTournament _,_) => bg_tournament
                                        |(ClubCompetition,_) => bg_open
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
                            (* last event cell *)
                            return es
                          else
                            (* first event cell *)
                            return (e::es))
                        else
                          (* pre-start for some event *)
                          pb <xml><td style={border}>&nbsp;</td></xml>;
                          return (e::es)
                        )
                      |[]=> (
                        (* no events to render *)
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
  end

and main {} = main_ {ShowPastMonths = False, Year = 2015}

(*}}}*)

