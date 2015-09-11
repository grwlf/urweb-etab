module Cake_Etab where

import Development.Cake3
import Development.Cake3.Ext.UrWeb as UW
import qualified Cake_Bootstrap as Bootstrap hiding(main)
import qualified Cake_Prelude as Prelude hiding(main)
import qualified Cake_MonadPack as MonadPack hiding(main)
import qualified Cake_Soup as Soup hiding(main)
import qualified Cake_XMLW as XMLW hiding(main)
import qualified Cake_Captcha as Captcha hiding(main)
import qualified Cake_Callback as Callback hiding(main)
import Cake_Etab_P

aatree_urp = file "lib/urweb-aatree/lib_aatree.urp"
bits_c = file "lib/urweb-aatree/lib/lib_bits/src/c/Bits.c"
bits_o = bits_c .= ".o"

check_aatree_urp = rule $ do
  shell [cmd| git -C $(cwd) submodule update --init lib/urweb-aatree |]
  shell [cmd| git -C $(cwd)/lib/urweb-aatree checkout -f |]
  shell [cmd| touch -c @(aatree_urp)|]

check_bits_o = rule $ do
  depend check_aatree_urp
  shell [cmd|echo '*.o' > $(cwd)/lib/urweb-aatree/.gitignore |]
  shell [cmd|echo '.*' >> $(cwd)/lib/urweb-aatree/.gitignore |]
  shell [cmd|C_INCLUDE_PATH=$(uwincludedir) $(uwcc) -c -o @(bits_o) $(bits_c)|]

(app,db) = uwapp_postgres (file "Etab.urp") $ do
  depend check_bits_o
  allow mime "text/javascript";
  allow mime "text/css";
  allow mime "image/jpeg";
  allow mime "image/png";
  allow mime "image/gif";
  allow mime "application/octet-stream";
  allow url "https://github.com/grwlf/urweb-etab*"
  allow url "http://www.bowmania.ru/forum/*"
  allow url "http://www.archery.su/*"
  allow url "http://fonts.googleapis.com/*"
  allow env "SWAKS"
  allow env "PASS"
  library MonadPack.lib
  library Prelude.lib
  library Bootstrap.lib
  library Soup.lib
  library XMLW.lib
  library Captcha.lib
  library Callback.lib
  library (file "lib/urweb-aatree/lib_aatree.urp")
  safeGet "Etab/register_user"
  safeGet "Etab/report_comp"
  safeGet "Etab/contact_us"
  embed (file "Etab.css")
  embed (file "Etab.ico")
  static "/Image_jpg/contents" (file "Image.jpg")
  ur (sys "list")
  ur (sys "option")
  ur (sys "string")
  ur (sys "datetime")
  ur (sys "char")
  ur (file "Gregorian.ur")
  ur (file "Email.ur")
  ur (file "Types.ur")
  ur (file "Etab.ur")

main = writeDefaultMakefiles $ do

  rule $ do
    phony "dropdb"
    depend db

  rule $ do
    phony "run"
    shell [cmd|$(app)|]

  rule $ do
    phony "all"
    depend app

