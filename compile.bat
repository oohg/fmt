@echo off
rem
rem $Id: compile.bat $
rem

:COMPILE_FMT

   pushd "%~dp0"
   set HG_START_DP_FMT_COMPILE_BAT=%CD%
   popd

   if /I not "%1%" == "/NOCLS" cls
   if /I "%1%" == "/NOCLS" shift

   if not exist fmtcls.prg goto ERROR1

   if /I not "%1" == "/C" goto ROOT
   shift
   set HG_ROOT=
   set HG_HRB=
   set HG_BCC=
   set LIB_GUI=
   set LIB_HRB=
   set BIN_HRB=

:ROOT

   if not "%HG_ROOT%" == "" goto CONTINUE
   pushd "%HG_START_DP_FMT_COMPILE_BAT%\.."
   set HG_ROOT=%CD%
   popd

:CONTINUE

   if "%HG_HRB%"==""   set HG_HRB=%HG_ROOT%\xhbcc
   if "%HG_BCC%"==""   set HG_BCC=c:\Borland\BCC55
   if "%LIB_GUI%"==""  set LIB_GUI=lib\xhb\bcc
   if "%LIB_HRB%"==""  set LIB_HRB=lib
   if "%BIN_HRB%"==""  set BIN_HRB=bin

:CLEAN_EXE

   if exist ofmt.exe del ofmt.exe
   if exist ofmt.exe goto ERROR2

:COMPILE

   set HG_DEFXHB=
   if /I not "%1"=="/H" set HG_DEFXHB=-D__XHARBOUR__
   echo Harbour: Compiling sources...
   "%HG_HRB%\%BIN_HRB%\harbour.exe" fmt    -i%HG_HRB%\include;%HG_ROOT%\include -n1 -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" fmtcls -i%HG_HRB%\include;%HG_ROOT%\include -n1 -w3 -gc0 -es2 -q0
   echo BCC32: Compiling...
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% fmt.c    > nul
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; %HG_DEFXHB% fmtcls.c > nul

:LINK

   echo ILINK32: Linking... ofmt.exe
   echo c0w32.obj + > b32.bc
   echo fmt.obj fmtcls.obj, + >> b32.bc
   echo ofmt.exe, + >> b32.bc
   echo ofmt.map, + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\oohg.lib + >> b32.bc
   for %%a in ( rtl vmmt gtgui lang codepage macro rdd dbfntx dbfcdx dbffpt common debug pp ct dbfdbt hbsix tip hsx )        do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   for %%a in ( hbrtl hbvm hblang hbcpage hbmacro hbrdd rddntx rddcdx rddfpt hbcommon hbdebug hbpp hbct hbwin pcrepos zlib ) do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   if exist "%HG_HRB%\%LIB_HRB%\libmisc.lib"    echo %HG_HRB%\%LIB_HRB%\libmisc.lib + >> b32.bc
   if exist "%HG_HRB%\%LIB_HRB%\hboleaut.lib"   echo %HG_HRB%\%LIB_HRB%\hboleaut.lib + >> b32.bc
   if exist "%HG_HRB%\%LIB_HRB%\dll.lib"        echo %HG_HRB%\%LIB_HRB%\dll.lib + >> b32.bc
   if exist "%HG_HRB%\%LIB_HRB%\socket.lib"     echo %HG_HRB%\%LIB_HRB%\socket.lib + >> b32.bc
   if exist "%HG_ROOT%\%LIB_GUI%\socket.lib"    echo %HG_ROOT%\%LIB_GUI%\socket.lib + >> b32.bc
   if exist "%HG_ROOT%\%LIB_GUI%\bostaurus.lib" echo %HG_ROOT%\%LIB_GUI%\bostaurus.lib + >> b32.bc
   if exist "%HG_ROOT%\%LIB_GUI%\hbprinter.lib" echo %HG_ROOT%\%LIB_GUI%\hbprinter.lib + >> b32.bc
   if exist "%HG_ROOT%\%LIB_GUI%\miniprint.lib" echo %HG_ROOT%\%LIB_GUI%\miniprint.lib + >> b32.bc
   echo cw32mt.lib + >> b32.bc
   echo msimg32.lib + >> b32.bc
   echo import32.lib, , + >> b32.bc
   echo %HG_ROOT%\resources\oohg.res + >> b32.bc
   "%HG_BCC%\bin\ilink32.exe" -Gn -Tpe -aa -L%HG_BCC%\lib;%HG_BCC%\lib\psdk; @b32.bc > nul
   if exist ofmt.exe goto OK
   echo Build finished with ERROR !!!
   goto CLEAN

:ERROR1

   echo This file must be executed from FMT folder !!!
   goto END

:ERROR2

   echo COMPILE ERROR: Is ofmt.exe running ?
   goto END

:OK

   echo Build finished OK !!!

:CLEAN

   for %%a in (*.tds)  do del %%a
   for %%a in (*.c)    do del %%a
   for %%a in (*.map)  do del %%a
   for %%a in (*.obj)  do del %%a
   for %%a in (b32.bc) do del %%a
   for %%a in (*.res)  do del %%a
   set HG_DEFXHB=

:END

   set HG_START_DP_FMT_COMPILE_BAT=
   echo.
