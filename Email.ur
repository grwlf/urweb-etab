structure C = CallbackNotify.Default

fun createJob exe args =
  _ <- C.abortMore 10;
  C.create (C.absCommand exe args)

fun getenv_ (v:string) : transaction string =
  o <- getenv (blessEnvVar v);
  case o of
    |Some v => return v
    |None => 
      debug ("Env var '" ^ v ^ "' is not set");
      error <xml>Inconsistent environment</xml>
  
fun env {} =
  swaks <- getenv_ "SWAKS";
  pass <- getenv_ "PASS";
  return (swaks, pass)

fun send (to:string) (from:string) (subj:string) (body:string) : transaction xbody =
  (swaks,pass) <- env {};
  jr <- createJob  swaks (
       "-s" :: "smtp.gmail.com" :: "-p" :: "587"
    :: "-t" :: to :: "-f" :: from
    :: "--header" :: ("Subject: " ^ subj)
    :: "-S" :: "--protocol" :: "ESMTP" :: "-tls"
    :: "-a" :: "-au" :: "ierton@gmail.com" :: "-ap" :: pass
    :: "--body" :: body
    :: "--timeout" :: "5"
    :: [] );
  C.monitorX jr (fn jr => 
    case jr.ExitCode of
      |None => <xml>Идет отправка сообщения</xml>
      |Some 0 => <xml>Сообщение успешно отправлено</xml>
      |_ => <xml>Не удалось отправить сообщение, код {[naughtyDebug (jr.Stdout ^ "\n" ^ jr.Stderr)+1]}</xml>)

