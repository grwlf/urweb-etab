# This Makefile was generated by the Cake3
# https://github.com/grwlf/cake3

GUARD = .cake3/GUARD_$(1)_$(shell echo $($(1)) | md5sum | cut -d ' ' -f 1)

ifeq ($(MAIN),1)
unexport MAIN

# Main section

URWEB = urweb
UWCC = $(shell $(shell $(URWEB) -print-ccompiler) -print-prog-name=gcc)
UWCPP = $(shell $(shell $(URWEB) -print-ccompiler) -print-prog-name=g++)
UWFLAGS = 
UWINCLUDE = $(shell $(URWEB) -print-cinclude)
UWINCLUDEDIR = $(shell $(URWEB) -print-cinclude)/..
UWVER = $(shell $(URWEB) -version)
.PHONY: ./all
./all: ./Etab.exe ./Etab.sql ./Makefile
./.cake3/tmp___Etab_in_2: ./Etab.ur ./Gregorian.ur ./Makefile ./autogen/Etab_css.ur ./autogen/Etab_css_c.h ./autogen/Etab_css_c.o ./lib/urweb-aatree/lib_aatree.urp ./lib/uru3/Bootstrap/lib.urp ./lib/urweb-captcha/lib.urp ./lib/urweb-monad-pack/lib.urp ./lib/urweb-prelude/lib.urp ./lib/urweb-soup/lib.urp ./lib/urweb-xmlw/lib.urp
	( \
	echo   ;\
	echo \.\/autogen\/Etab\_css  ;\
	echo $$\/list  ;\
	echo $$\/option  ;\
	echo $$\/string  ;\
	echo $$\/datetime  ;\
	echo $$\/char  ;\
	echo \.\/Gregorian  ;\
	echo \.\/Etab  ;\
	) > ./.cake3/tmp___Etab_in_2
./.cake3/tmp___Etab_in_1: ./Etab.ur ./Gregorian.ur ./Makefile ./autogen/Etab_css.ur ./autogen/Etab_css_c.h ./autogen/Etab_css_c.o ./lib/urweb-aatree/lib_aatree.urp ./lib/uru3/Bootstrap/lib.urp ./lib/urweb-captcha/lib.urp ./lib/urweb-monad-pack/lib.urp ./lib/urweb-prelude/lib.urp ./lib/urweb-soup/lib.urp ./lib/urweb-xmlw/lib.urp
	( \
	echo database\ dbname\=Etab  ;\
	echo sql\ \.\/Etab\.sql  ;\
	echo allow\ mime\ text\/javascript  ;\
	echo allow\ mime\ text\/css  ;\
	echo allow\ mime\ image\/jpeg  ;\
	echo allow\ mime\ image\/png  ;\
	echo allow\ mime\ image\/gif  ;\
	echo allow\ mime\ application\/octet\-stream  ;\
	echo allow\ url\ https\:\/\/github\.com\/grwlf\/urweb\-etab\*  ;\
	echo allow\ url\ http\:\/\/www\.bowmania\.ru\/forum\/\*  ;\
	echo allow\ url\ http\:\/\/www\.archery\.su\/\*  ;\
	echo library\ \.\/lib\/urweb\-monad\-pack\/  ;\
	echo library\ \.\/lib\/urweb\-prelude\/  ;\
	echo library\ \.\/lib\/uru3\/Bootstrap\/  ;\
	echo library\ \.\/lib\/urweb\-soup\/  ;\
	echo library\ \.\/lib\/urweb\-xmlw\/  ;\
	echo library\ \.\/lib\/urweb\-captcha\/  ;\
	echo library\ \.\/lib\/urweb\-aatree\/lib\_aatree  ;\
	echo safeGet\ Etab\/register\_user  ;\
	echo ffi\ \.\/autogen\/Etab\_css\_c  ;\
	echo include\ \.\/autogen\/Etab\_css\_c\.h  ;\
	echo link\ \.\/autogen\/Etab\_css\_c\.o  ;\
	) > ./.cake3/tmp___Etab_in_1
./.cake3/tmp___lib_urweb_captcha_lib_in_2: ./Makefile ./lib/urweb-captcha/Captcha.ur ./lib/urweb-captcha/Captcha_ffi.h ./lib/urweb-captcha/Captcha_ffi.o ./lib/urweb-captcha/lib/captcha/libcaptcha.a
	( \
	echo   ;\
	echo \.\/Captcha  ;\
	) > ./.cake3/tmp___lib_urweb_captcha_lib_in_2
./.cake3/tmp___lib_urweb_captcha_lib_in_1: ./Makefile ./lib/urweb-captcha/Captcha.ur ./lib/urweb-captcha/Captcha_ffi.h ./lib/urweb-captcha/Captcha_ffi.o ./lib/urweb-captcha/lib/captcha/libcaptcha.a
	( \
	echo ffi\ \.\/Captcha\_ffi  ;\
	echo include\ \.\/Captcha\_ffi\.h  ;\
	echo link\ \.\/Captcha\_ffi\.o  ;\
	echo link\ \.\/lib\/captcha\/libcaptcha\.a  ;\
	) > ./.cake3/tmp___lib_urweb_captcha_lib_in_1
./lib/urweb-captcha/lib/captcha/libcaptcha.a: ./Makefile ./lib/urweb-captcha/lib/captcha/Makefile
	make -C lib/urweb-captcha/lib/captcha
	touch -c ./lib/urweb-captcha/lib/captcha/libcaptcha.a
./lib/urweb-captcha/lib/captcha/Makefile: ./Makefile
	git -C lib/urweb-captcha submodule update --init
	git -C lib/urweb-captcha/lib/captcha checkout -f
	touch -c ./lib/urweb-captcha/lib/captcha/Makefile
./.cake3/tmp___lib_urweb_xmlw_lib_in_2: ./Makefile ./lib/urweb-monad-pack/lib.urp ./lib/urweb-xmlw/XMLW.ur
	( \
	echo   ;\
	echo \.\/XMLW  ;\
	) > ./.cake3/tmp___lib_urweb_xmlw_lib_in_2
./.cake3/tmp___lib_urweb_xmlw_lib_in_1: ./Makefile ./lib/urweb-monad-pack/lib.urp ./lib/urweb-xmlw/XMLW.ur
	( \
	echo library\ \.\.\/\.\.\/lib\/urweb\-monad\-pack\/  ;\
	) > ./.cake3/tmp___lib_urweb_xmlw_lib_in_1
./.cake3/tmp___lib_urweb_monad_pack_lib_in_2: ./Makefile ./lib/urweb-monad-pack/error.ur ./lib/urweb-monad-pack/identity.ur ./lib/urweb-monad-pack/pure.ur ./lib/urweb-monad-pack/state.ur
	( \
	echo   ;\
	echo \.\/error  ;\
	echo \.\/state  ;\
	echo \.\/identity  ;\
	echo \.\/pure  ;\
	) > ./.cake3/tmp___lib_urweb_monad_pack_lib_in_2
./.cake3/tmp___lib_urweb_monad_pack_lib_in_1: ./Makefile ./lib/urweb-monad-pack/error.ur ./lib/urweb-monad-pack/identity.ur ./lib/urweb-monad-pack/pure.ur ./lib/urweb-monad-pack/state.ur
	echo -n > ./.cake3/tmp___lib_urweb_monad_pack_lib_in_1
./.cake3/tmp___lib_urweb_soup_lib_in_2: ./Makefile ./lib/uru3/Bootstrap/lib.urp ./lib/uru3/Uru/lib.urp ./lib/urweb-prelude/lib.urp ./lib/urweb-soup/Analytics.h ./lib/urweb-soup/Analytics.o ./lib/urweb-soup/Soup.ur ./lib/urweb-soup/autogen/Soup_css.ur ./lib/urweb-soup/autogen/Soup_css_c.h ./lib/urweb-soup/autogen/Soup_css_c.o
	( \
	echo   ;\
	echo \.\/autogen\/Soup\_css  ;\
	echo \.\/Soup  ;\
	) > ./.cake3/tmp___lib_urweb_soup_lib_in_2
./.cake3/tmp___lib_urweb_soup_lib_in_1: ./Makefile ./lib/uru3/Bootstrap/lib.urp ./lib/uru3/Uru/lib.urp ./lib/urweb-prelude/lib.urp ./lib/urweb-soup/Analytics.h ./lib/urweb-soup/Analytics.o ./lib/urweb-soup/Soup.ur ./lib/urweb-soup/autogen/Soup_css.ur ./lib/urweb-soup/autogen/Soup_css_c.h ./lib/urweb-soup/autogen/Soup_css_c.o
	( \
	echo library\ \.\.\/\.\.\/lib\/uru3\/Bootstrap\/  ;\
	echo library\ \.\.\/\.\.\/lib\/uru3\/Uru\/  ;\
	echo library\ \.\.\/\.\.\/lib\/urweb\-prelude\/  ;\
	echo allow\ url\ http\:\/\/github\.com\*  ;\
	echo allow\ url\ http\:\/\/impredicative\.com\*  ;\
	echo allow\ url\ https\:\/\/camo\.githubusercontent\.com\*  ;\
	echo ffi\ \.\/autogen\/Soup\_css\_c  ;\
	echo include\ \.\/autogen\/Soup\_css\_c\.h  ;\
	echo link\ \.\/autogen\/Soup\_css\_c\.o  ;\
	echo link\ \.\/Analytics\.o  ;\
	echo link\ \-lstdc\+\+  ;\
	echo ffi\ \.\/Analytics  ;\
	echo ffi\ \.\/HTML5Tags  ;\
	echo include\ \.\/Analytics\.h  ;\
	) > ./.cake3/tmp___lib_urweb_soup_lib_in_1
./.cake3/tmp___lib_urweb_prelude_lib_in_2: ./Makefile ./lib/urweb-prelude/src/Prelude.ur
	( \
	echo   ;\
	echo $$\/list  ;\
	echo \.\/src\/Prelude  ;\
	) > ./.cake3/tmp___lib_urweb_prelude_lib_in_2
./.cake3/tmp___lib_urweb_prelude_lib_in_1: ./Makefile ./lib/urweb-prelude/src/Prelude.ur
	echo -n > ./.cake3/tmp___lib_urweb_prelude_lib_in_1
./.cake3/tmp___lib_uru3_Uru_lib_in_2: ./Makefile ./lib/uru3/Uru/CSS.ur ./lib/uru3/Uru/CSS.urs ./lib/uru3/Uru/Script.h ./lib/uru3/Uru/Script.o ./lib/uru3/Uru/Uru.ur ./lib/uru3/Uru/Uru.urs
	( \
	echo   ;\
	echo $$\/list  ;\
	echo \.\/CSS  ;\
	echo \.\/Uru  ;\
	) > ./.cake3/tmp___lib_uru3_Uru_lib_in_2
./.cake3/tmp___lib_uru3_Uru_lib_in_1: ./Makefile ./lib/uru3/Uru/CSS.ur ./lib/uru3/Uru/CSS.urs ./lib/uru3/Uru/Script.h ./lib/uru3/Uru/Script.o ./lib/uru3/Uru/Uru.ur ./lib/uru3/Uru/Uru.urs
	( \
	echo ffi\ \.\/Script  ;\
	echo include\ \.\/Script\.h  ;\
	echo link\ \.\/Script\.o  ;\
	) > ./.cake3/tmp___lib_uru3_Uru_lib_in_1
./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2: ./Makefile ./lib/uru3/Bootstrap/Bootstrap.ur ./lib/uru3/Bootstrap/BootstrapStyles.ur ./lib/uru3/Bootstrap/FormSignin.ur ./lib/uru3/Bootstrap/autogen/FormSignin_css.ur ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.h ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o ./lib/uru3/Bootstrap/autogen/Tooltip_js.ur ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.h ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_css.ur ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.h ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_min_js.ur ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.h ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css.ur ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.h ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o ./lib/uru3/JQuery/lib.urp ./lib/uru3/Uru/lib.urp
	( \
	echo   ;\
	echo \.\/autogen\/Tooltip\_js  ;\
	echo \.\/autogen\/glyphicons\_halflings\_regular\_eot  ;\
	echo \.\/autogen\/glyphicons\_halflings\_regular\_svg  ;\
	echo \.\/autogen\/glyphicons\_halflings\_regular\_ttf  ;\
	echo \.\/autogen\/glyphicons\_halflings\_regular\_woff  ;\
	echo \.\/autogen\/bootstrap\_css  ;\
	echo \.\/autogen\/bootstrap\_theme\_css  ;\
	echo \.\/autogen\/bootstrap\_min\_js  ;\
	echo \.\/autogen\/FormSignin\_css  ;\
	echo \.\/BootstrapStyles  ;\
	echo \.\/Bootstrap  ;\
	echo \.\/FormSignin  ;\
	) > ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2
./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1: ./Makefile ./lib/uru3/Bootstrap/Bootstrap.ur ./lib/uru3/Bootstrap/BootstrapStyles.ur ./lib/uru3/Bootstrap/FormSignin.ur ./lib/uru3/Bootstrap/autogen/FormSignin_css.ur ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.h ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o ./lib/uru3/Bootstrap/autogen/Tooltip_js.ur ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.h ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_css.ur ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.h ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_min_js.ur ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.h ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css.ur ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.h ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff.ur ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.h ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o ./lib/uru3/JQuery/lib.urp ./lib/uru3/Uru/lib.urp
	( \
	echo rewrite\ style\ BootstrapStyles\/bs3\_table\ table  ;\
	echo rewrite\ style\ BootstrapStyles\/\*\ \[\-\]  ;\
	echo ffi\ \.\/NavTag  ;\
	echo ffi\ \.\/autogen\/Tooltip\_js\_js  ;\
	echo ffi\ \.\/autogen\/Tooltip\_js\_c  ;\
	echo include\ \.\/autogen\/Tooltip\_js\_c\.h  ;\
	echo link\ \.\/autogen\/Tooltip\_js\_c\.o  ;\
	echo library\ \.\.\/\.\.\/\.\.\/lib\/uru3\/JQuery\/  ;\
	echo library\ \.\.\/\.\.\/\.\.\/lib\/uru3\/Uru\/  ;\
	echo ffi\ \.\/autogen\/glyphicons\_halflings\_regular\_eot\_c  ;\
	echo include\ \.\/autogen\/glyphicons\_halflings\_regular\_eot\_c\.h  ;\
	echo link\ \.\/autogen\/glyphicons\_halflings\_regular\_eot\_c\.o  ;\
	echo ffi\ \.\/autogen\/glyphicons\_halflings\_regular\_svg\_c  ;\
	echo include\ \.\/autogen\/glyphicons\_halflings\_regular\_svg\_c\.h  ;\
	echo link\ \.\/autogen\/glyphicons\_halflings\_regular\_svg\_c\.o  ;\
	echo ffi\ \.\/autogen\/glyphicons\_halflings\_regular\_ttf\_c  ;\
	echo include\ \.\/autogen\/glyphicons\_halflings\_regular\_ttf\_c\.h  ;\
	echo link\ \.\/autogen\/glyphicons\_halflings\_regular\_ttf\_c\.o  ;\
	echo ffi\ \.\/autogen\/glyphicons\_halflings\_regular\_woff\_c  ;\
	echo include\ \.\/autogen\/glyphicons\_halflings\_regular\_woff\_c\.h  ;\
	echo link\ \.\/autogen\/glyphicons\_halflings\_regular\_woff\_c\.o  ;\
	echo ffi\ \.\/autogen\/bootstrap\_css\_c  ;\
	echo include\ \.\/autogen\/bootstrap\_css\_c\.h  ;\
	echo link\ \.\/autogen\/bootstrap\_css\_c\.o  ;\
	echo ffi\ \.\/autogen\/bootstrap\_theme\_css\_c  ;\
	echo include\ \.\/autogen\/bootstrap\_theme\_css\_c\.h  ;\
	echo link\ \.\/autogen\/bootstrap\_theme\_css\_c\.o  ;\
	echo ffi\ \.\/autogen\/bootstrap\_min\_js\_c  ;\
	echo include\ \.\/autogen\/bootstrap\_min\_js\_c\.h  ;\
	echo link\ \.\/autogen\/bootstrap\_min\_js\_c\.o  ;\
	echo ffi\ \.\/autogen\/FormSignin\_css\_c  ;\
	echo include\ \.\/autogen\/FormSignin\_css\_c\.h  ;\
	echo link\ \.\/autogen\/FormSignin\_css\_c\.o  ;\
	echo safeGet\ Tooltip\_js\/enable\_tooltips  ;\
	) > ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1
./.cake3/tmp___lib_uru3_JQuery_lib_in_2: ./Makefile ./lib/uru3/JQuery/JQuery.ur ./lib/uru3/JQuery/JQuery.urs ./lib/uru3/JQuery/autogen/jquery_1_9_1_js.ur ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.h ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o ./lib/uru3/Uru/lib.urp
	( \
	echo   ;\
	echo \.\/autogen\/jquery\_1\_9\_1\_js  ;\
	echo \.\/JQuery  ;\
	) > ./.cake3/tmp___lib_uru3_JQuery_lib_in_2
./.cake3/tmp___lib_uru3_JQuery_lib_in_1: ./Makefile ./lib/uru3/JQuery/JQuery.ur ./lib/uru3/JQuery/JQuery.urs ./lib/uru3/JQuery/autogen/jquery_1_9_1_js.ur ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.h ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o ./lib/uru3/Uru/lib.urp
	( \
	echo library\ \.\.\/\.\.\/\.\.\/lib\/uru3\/Uru\/  ;\
	echo ffi\ \.\/autogen\/jquery\_1\_9\_1\_js\_c  ;\
	echo include\ \.\/autogen\/jquery\_1\_9\_1\_js\_c\.h  ;\
	echo link\ \.\/autogen\/jquery\_1\_9\_1\_js\_c\.o  ;\
	) > ./.cake3/tmp___lib_uru3_JQuery_lib_in_1
./lib/urweb-aatree/lib/lib_bits/src/c/Bits.o: ./Makefile ./lib/urweb-aatree/lib/lib_bits/src/c/Bits.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDEDIR)
	C_INCLUDE_PATH=$(UWINCLUDEDIR) $(UWCC) -c -o ./lib/urweb-aatree/lib/lib_bits/src/c/Bits.o ./lib/urweb-aatree/lib/lib_bits/src/c/Bits.c
.PHONY: ./run
./run: ./Etab.exe ./Etab.sql ./Makefile
	./Etab.exe
.PHONY: ./dropdb
./dropdb: ./Etab.db ./Makefile
./Etab.db: ./Etab.exe ./Etab.sql ./Makefile
	dropdb --if-exists Etab
	createdb Etab
	psql -f ./Etab.sql Etab
	touch ./Etab.db
.INTERMEDIATE: ./.fix-multy1
./.fix-multy1: ./Etab.urp ./Makefile $(call GUARD,URWEB) $(call GUARD,UWFLAGS) $(call GUARD,UWINCLUDEDIR) $(call GUARD,UWVER)
	C_INCLUDE_PATH=$(UWINCLUDEDIR) $(URWEB) -dbms postgres $(UWFLAGS) ./Etab
./Etab.exe: ./.fix-multy1
./Etab.sql: ./.fix-multy1
./Etab.urp: ./.cake3/tmp___Etab_in_1 ./.cake3/tmp___Etab_in_2 ./Makefile ./autogen/Etab_css.urp.in ./lib/urweb-aatree/lib/lib_bits/src/c/Bits.o
	cat ./.cake3/tmp___Etab_in_1 > ./Etab.urp
	cat ./autogen/Etab_css.urp.in >> ./Etab.urp
	cat ./.cake3/tmp___Etab_in_2 >> ./Etab.urp
./autogen/Etab_css_c.o: ./Makefile ./autogen/Etab_css_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./autogen/Etab_css_c.o ./autogen/Etab_css_c.c
./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.c
./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.c
./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.c
./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.c
./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.c
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.c
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.c
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.c
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o: ./Makefile ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.c
./lib/uru3/Bootstrap/lib.urp: ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1 ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2 ./Makefile ./lib/uru3/Bootstrap/autogen/FormSignin_css.urp.in ./lib/uru3/Bootstrap/autogen/Tooltip_js.urp.in ./lib/uru3/Bootstrap/autogen/bootstrap_css.urp.in ./lib/uru3/Bootstrap/autogen/bootstrap_min_js.urp.in ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff.urp.in
	cat ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1 > ./lib/uru3/Bootstrap/lib.urp
	cat ./lib/uru3/Bootstrap/autogen/FormSignin_css.urp.in ./lib/uru3/Bootstrap/autogen/bootstrap_min_js.urp.in ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css.urp.in ./lib/uru3/Bootstrap/autogen/bootstrap_css.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg.urp.in ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot.urp.in ./lib/uru3/Bootstrap/autogen/Tooltip_js.urp.in >> ./lib/uru3/Bootstrap/lib.urp
	cat ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2 >> ./lib/uru3/Bootstrap/lib.urp
./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o: ./Makefile ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.c
./lib/uru3/JQuery/lib.urp: ./.cake3/tmp___lib_uru3_JQuery_lib_in_1 ./.cake3/tmp___lib_uru3_JQuery_lib_in_2 ./Makefile ./lib/uru3/JQuery/autogen/jquery_1_9_1_js.urp.in
	cat ./.cake3/tmp___lib_uru3_JQuery_lib_in_1 > ./lib/uru3/JQuery/lib.urp
	cat ./lib/uru3/JQuery/autogen/jquery_1_9_1_js.urp.in >> ./lib/uru3/JQuery/lib.urp
	cat ./.cake3/tmp___lib_uru3_JQuery_lib_in_2 >> ./lib/uru3/JQuery/lib.urp
./lib/uru3/Uru/Script.o: ./Makefile ./lib/uru3/Uru/Script.c $(call GUARD,UWCC) $(call GUARD,UWCFLAGS) $(call GUARD,UWINCLUDE) $(call GUARD,UWINCLUDEDIR)
	$(UWCC) -c -I$(UWINCLUDEDIR) -I$(UWINCLUDE) $(UWCFLAGS)  -o ./lib/uru3/Uru/Script.o ./lib/uru3/Uru/Script.c
./lib/uru3/Uru/lib.urp: ./.cake3/tmp___lib_uru3_Uru_lib_in_1 ./.cake3/tmp___lib_uru3_Uru_lib_in_2 ./Makefile
	cat ./.cake3/tmp___lib_uru3_Uru_lib_in_1 > ./lib/uru3/Uru/lib.urp
	cat ./.cake3/tmp___lib_uru3_Uru_lib_in_2 >> ./lib/uru3/Uru/lib.urp
./lib/urweb-captcha/Captcha_ffi.o: ./Makefile ./lib/urweb-captcha/Captcha_ffi.c $(call GUARD,UWCC) $(call GUARD,UWCFLAGS) $(call GUARD,UWINCLUDE) $(call GUARD,UWINCLUDEDIR)
	$(UWCC) -c -I$(UWINCLUDEDIR) -I$(UWINCLUDE) $(UWCFLAGS)  -o ./lib/urweb-captcha/Captcha_ffi.o ./lib/urweb-captcha/Captcha_ffi.c
./lib/urweb-captcha/lib.urp: ./.cake3/tmp___lib_urweb_captcha_lib_in_1 ./.cake3/tmp___lib_urweb_captcha_lib_in_2 ./Makefile
	cat ./.cake3/tmp___lib_urweb_captcha_lib_in_1 > ./lib/urweb-captcha/lib.urp
	cat ./.cake3/tmp___lib_urweb_captcha_lib_in_2 >> ./lib/urweb-captcha/lib.urp
./lib/urweb-monad-pack/lib.urp: ./.cake3/tmp___lib_urweb_monad_pack_lib_in_1 ./.cake3/tmp___lib_urweb_monad_pack_lib_in_2 ./Makefile
	cat ./.cake3/tmp___lib_urweb_monad_pack_lib_in_1 > ./lib/urweb-monad-pack/lib.urp
	cat ./.cake3/tmp___lib_urweb_monad_pack_lib_in_2 >> ./lib/urweb-monad-pack/lib.urp
./lib/urweb-prelude/lib.urp: ./.cake3/tmp___lib_urweb_prelude_lib_in_1 ./.cake3/tmp___lib_urweb_prelude_lib_in_2 ./Makefile
	cat ./.cake3/tmp___lib_urweb_prelude_lib_in_1 > ./lib/urweb-prelude/lib.urp
	cat ./.cake3/tmp___lib_urweb_prelude_lib_in_2 >> ./lib/urweb-prelude/lib.urp
./lib/urweb-soup/Analytics.o: ./Makefile ./lib/urweb-soup/Analytics.cpp $(call GUARD,UWCFLAGS) $(call GUARD,UWCPP) $(call GUARD,UWINCLUDE) $(call GUARD,UWINCLUDEDIR)
	$(UWCPP) -c $(UWCFLAGS) -I$(UWINCLUDEDIR) -I$(UWINCLUDE) -std=c++11 -o ./lib/urweb-soup/Analytics.o ./lib/urweb-soup/Analytics.cpp
./lib/urweb-soup/autogen/Soup_css_c.o: ./Makefile ./lib/urweb-soup/autogen/Soup_css_c.c $(call GUARD,UWCC) $(call GUARD,UWINCLUDE)
	$(UWCC) -c -I$(UWINCLUDE) -o ./lib/urweb-soup/autogen/Soup_css_c.o ./lib/urweb-soup/autogen/Soup_css_c.c
./lib/urweb-soup/lib.urp: ./.cake3/tmp___lib_urweb_soup_lib_in_1 ./.cake3/tmp___lib_urweb_soup_lib_in_2 ./Makefile ./lib/urweb-soup/autogen/Soup_css.urp.in
	cat ./.cake3/tmp___lib_urweb_soup_lib_in_1 > ./lib/urweb-soup/lib.urp
	cat ./lib/urweb-soup/autogen/Soup_css.urp.in >> ./lib/urweb-soup/lib.urp
	cat ./.cake3/tmp___lib_urweb_soup_lib_in_2 >> ./lib/urweb-soup/lib.urp
./lib/urweb-xmlw/lib.urp: ./.cake3/tmp___lib_urweb_xmlw_lib_in_1 ./.cake3/tmp___lib_urweb_xmlw_lib_in_2 ./Makefile
	cat ./.cake3/tmp___lib_urweb_xmlw_lib_in_1 > ./lib/urweb-xmlw/lib.urp
	cat ./.cake3/tmp___lib_urweb_xmlw_lib_in_2 >> ./lib/urweb-xmlw/lib.urp
$(call GUARD,URWEB):
	rm -f .cake3/GUARD_URWEB_*
	touch $@
$(call GUARD,UWCC):
	rm -f .cake3/GUARD_UWCC_*
	touch $@
$(call GUARD,UWCFLAGS):
	rm -f .cake3/GUARD_UWCFLAGS_*
	touch $@
$(call GUARD,UWCPP):
	rm -f .cake3/GUARD_UWCPP_*
	touch $@
$(call GUARD,UWFLAGS):
	rm -f .cake3/GUARD_UWFLAGS_*
	touch $@
$(call GUARD,UWINCLUDE):
	rm -f .cake3/GUARD_UWINCLUDE_*
	touch $@
$(call GUARD,UWINCLUDEDIR):
	rm -f .cake3/GUARD_UWINCLUDEDIR_*
	touch $@
$(call GUARD,UWVER):
	rm -f .cake3/GUARD_UWVER_*
	touch $@

else

# Prebuild/postbuild section

ifneq ($(MAKECMDGOALS),clean)

.PHONY: ./all
./all: ./.fix-multy1
.PHONY: ./.cake3/tmp___Etab_in_2
./.cake3/tmp___Etab_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___Etab_in_1
./.cake3/tmp___Etab_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_captcha_lib_in_2
./.cake3/tmp___lib_urweb_captcha_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_captcha_lib_in_1
./.cake3/tmp___lib_urweb_captcha_lib_in_1: ./.fix-multy1
.PHONY: ./lib/urweb-captcha/lib/captcha/libcaptcha.a
./lib/urweb-captcha/lib/captcha/libcaptcha.a: ./.fix-multy1
.PHONY: ./lib/urweb-captcha/lib/captcha/Makefile
./lib/urweb-captcha/lib/captcha/Makefile: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_xmlw_lib_in_2
./.cake3/tmp___lib_urweb_xmlw_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_xmlw_lib_in_1
./.cake3/tmp___lib_urweb_xmlw_lib_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_monad_pack_lib_in_2
./.cake3/tmp___lib_urweb_monad_pack_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_monad_pack_lib_in_1
./.cake3/tmp___lib_urweb_monad_pack_lib_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_soup_lib_in_2
./.cake3/tmp___lib_urweb_soup_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_soup_lib_in_1
./.cake3/tmp___lib_urweb_soup_lib_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_prelude_lib_in_2
./.cake3/tmp___lib_urweb_prelude_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_urweb_prelude_lib_in_1
./.cake3/tmp___lib_urweb_prelude_lib_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_uru3_Uru_lib_in_2
./.cake3/tmp___lib_uru3_Uru_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_uru3_Uru_lib_in_1
./.cake3/tmp___lib_uru3_Uru_lib_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2
./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1
./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_uru3_JQuery_lib_in_2
./.cake3/tmp___lib_uru3_JQuery_lib_in_2: ./.fix-multy1
.PHONY: ./.cake3/tmp___lib_uru3_JQuery_lib_in_1
./.cake3/tmp___lib_uru3_JQuery_lib_in_1: ./.fix-multy1
.PHONY: ./lib/urweb-aatree/lib/lib_bits/src/c/Bits.o
./lib/urweb-aatree/lib/lib_bits/src/c/Bits.o: ./.fix-multy1
.PHONY: ./run
./run: ./.fix-multy1
.PHONY: ./dropdb
./dropdb: ./.fix-multy1
.PHONY: ./Etab.db
./Etab.db: ./.fix-multy1
.INTERMEDIATE: ./.fix-multy1
./.fix-multy1:
	-mkdir .cake3
	MAIN=1 $(MAKE) -f ./Makefile $(MAKECMDGOALS)
.PHONY: ./Etab.exe
./Etab.exe: ./.fix-multy1
.PHONY: ./Etab.sql
./Etab.sql: ./.fix-multy1
.PHONY: ./Etab.urp
./Etab.urp: ./.fix-multy1
.PHONY: ./autogen/Etab_css_c.o
./autogen/Etab_css_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o
./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o
./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o
./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o
./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o
./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o
./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/Bootstrap/lib.urp
./lib/uru3/Bootstrap/lib.urp: ./.fix-multy1
.PHONY: ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o
./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o: ./.fix-multy1
.PHONY: ./lib/uru3/JQuery/lib.urp
./lib/uru3/JQuery/lib.urp: ./.fix-multy1
.PHONY: ./lib/uru3/Uru/Script.o
./lib/uru3/Uru/Script.o: ./.fix-multy1
.PHONY: ./lib/uru3/Uru/lib.urp
./lib/uru3/Uru/lib.urp: ./.fix-multy1
.PHONY: ./lib/urweb-captcha/Captcha_ffi.o
./lib/urweb-captcha/Captcha_ffi.o: ./.fix-multy1
.PHONY: ./lib/urweb-captcha/lib.urp
./lib/urweb-captcha/lib.urp: ./.fix-multy1
.PHONY: ./lib/urweb-monad-pack/lib.urp
./lib/urweb-monad-pack/lib.urp: ./.fix-multy1
.PHONY: ./lib/urweb-prelude/lib.urp
./lib/urweb-prelude/lib.urp: ./.fix-multy1
.PHONY: ./lib/urweb-soup/Analytics.o
./lib/urweb-soup/Analytics.o: ./.fix-multy1
.PHONY: ./lib/urweb-soup/autogen/Soup_css_c.o
./lib/urweb-soup/autogen/Soup_css_c.o: ./.fix-multy1
.PHONY: ./lib/urweb-soup/lib.urp
./lib/urweb-soup/lib.urp: ./.fix-multy1
.PHONY: ./lib/urweb-xmlw/lib.urp
./lib/urweb-xmlw/lib.urp: ./.fix-multy1

endif
.PHONY: ./clean
./clean:
	-rm ./.cake3/tmp___Etab_in_1 ./.cake3/tmp___Etab_in_2 ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_1 ./.cake3/tmp___lib_uru3_Bootstrap_lib_in_2 ./.cake3/tmp___lib_uru3_JQuery_lib_in_1 ./.cake3/tmp___lib_uru3_JQuery_lib_in_2 ./.cake3/tmp___lib_uru3_Uru_lib_in_1 ./.cake3/tmp___lib_uru3_Uru_lib_in_2 ./.cake3/tmp___lib_urweb_captcha_lib_in_1 ./.cake3/tmp___lib_urweb_captcha_lib_in_2 ./.cake3/tmp___lib_urweb_monad_pack_lib_in_1 ./.cake3/tmp___lib_urweb_monad_pack_lib_in_2 ./.cake3/tmp___lib_urweb_prelude_lib_in_1 ./.cake3/tmp___lib_urweb_prelude_lib_in_2 ./.cake3/tmp___lib_urweb_soup_lib_in_1 ./.cake3/tmp___lib_urweb_soup_lib_in_2 ./.cake3/tmp___lib_urweb_xmlw_lib_in_1 ./.cake3/tmp___lib_urweb_xmlw_lib_in_2 ./Etab.db ./Etab.exe ./Etab.sql ./Etab.urp ./autogen/Etab_css_c.o ./lib/urweb-aatree/lib/lib_bits/src/c/Bits.o ./lib/uru3/Bootstrap/autogen/FormSignin_css_c.o ./lib/uru3/Bootstrap/autogen/Tooltip_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_css_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_min_js_c.o ./lib/uru3/Bootstrap/autogen/bootstrap_theme_css_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_eot_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_svg_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_ttf_c.o ./lib/uru3/Bootstrap/autogen/glyphicons_halflings_regular_woff_c.o ./lib/uru3/Bootstrap/lib.urp ./lib/uru3/JQuery/autogen/jquery_1_9_1_js_c.o ./lib/uru3/JQuery/lib.urp ./lib/uru3/Uru/Script.o ./lib/uru3/Uru/lib.urp ./lib/urweb-captcha/Captcha_ffi.o ./lib/urweb-captcha/lib.urp ./lib/urweb-captcha/lib/captcha/Makefile ./lib/urweb-captcha/lib/captcha/libcaptcha.a ./lib/urweb-monad-pack/lib.urp ./lib/urweb-prelude/lib.urp ./lib/urweb-soup/Analytics.o ./lib/urweb-soup/autogen/Soup_css_c.o ./lib/urweb-soup/lib.urp ./lib/urweb-xmlw/lib.urp
	-rm -rf .cake3

endif
