@echo off
rem
rem $Id: build.bat $
rem

:MAIN

   cls
   if /I "%1"=="/C" goto CLEAN_PATH
   if "%HG_ROOT%"=="" set HG_ROOT=c:\oohg
   set HG_CLEAN=
   goto PARAMS

:CLEAN_PATH

   set HG_ROOT=c:\oohg
   set HG_CLEAN=/C
   shift

:PARAMS

   if /I "%1"=="HB30" goto CHECK30
   if /I "%1"=="HB32" goto CHECK32

:NOVERSION

   if exist %HG_ROOT%\buildapp30.bat goto CONTINUE
   if exist %HG_ROOT%\buildapp32.bat goto HB32
   echo File  %HG_ROOT%\buildapp30.bat not found !!!
   echo File  %HG_ROOT%\buildapp32.bat not found !!!
   echo.
   goto END

:CONTINUE

   if not exist %HG_ROOT%\buildapp32.bat goto HB30
   echo Syntax:
   echo    To build with Harbour 3.0
   echo       build [/C] HB30 file [options]
   echo   To build with Harbour 3.2
   echo       build [/C] HB32 file [options]
   echo.
   goto END

:CHECK30

   shift
   if exist %HG_ROOT%\buildapp30.bat goto HB30
   echo File  %HG_ROOT%\buildapp30.bat not found !!!
   echo.
   goto END

:CHECK32

   shift
   if exist %HG_ROOT%\buildapp32.bat goto HB32
   echo File  %HG_ROOT%\buildapp32.bat not found !!!
   echo.
   goto END

:HB30

   set HG_VERSION=HB30
   goto SETPATH

:HB32

   set HG_VERSION=HB32

:SETPATH

   if "%HG_CLEAN%"=="/C" goto CLEAN_PATH
   if "%HG_HRB%"==""     set HG_HRB=c:\oohg\%HG_VERSION%
   if "%HG_MINGW%"==""   set HG_MINGW=c:\oohg\%HG_VERSION%\comp\mingw
   if "%LIB_GUI%"==""    set LIB_GUI=lib\hb\mingw
   if "%LIB_HRB%"==""    set LIB_HRB=lib\win\mingw
   if "%BIN_HRB%"==""    set BIN_HRB=bin
   goto COMPILE

:CLEAN_PATH

   set HG_HRB=c:\oohg\%HG_VERSION%
   set HG_MINGW=c:\oohg\%HG_VERSION%\comp\mingw
   set LIB_GUI=lib\hb\mingw
   set LIB_HRB=lib\win\mingw
   set BIN_HRB=bin

:COMPILE

   set HG_VERSION=
   set HG_CCOMP=%HG_MINGW%
   set TPATH=%PATH%
   set PATH=%HG_CCOMP%\bin;%HG_HRB%\%BIN_HRB%

   if exist ofmt.exe   del ofmt.exe
   if exist output.log del output.log

   echo #define oohgpath %HG_ROOT%\RESOURCES > _oohg_resconfig.h
   copy /b %HG_ROOT%\resources\oohg.rc _temp.rc > nul
   windres -i _temp.rc -o _temp.o

   hbmk2 ofmt.hbp %1 %2 %3 %4 %5 %6 %7 %8 %9 >> output.log 2>&1
   if exist output.log type output.log

   if exist _oohg_resconfig.h del _oohg_resconfig.h
   if exist _temp.* del _temp.*
   set PATH=%TPATH%
   set TPATH=

:END

   set HG_CLEAN=
