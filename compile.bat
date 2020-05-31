@echo off
rem
rem $Id: compile.bat $
rem

setlocal

:COMPILE_FMT

   pushd "%~dp0"
   set HG_START_DP_FMT_COMPILE_BAT=%CD%
   popd

   if /I not "%1%" == "/NOCLS" cls
   if /I "%1%" == "/NOCLS" shift

   if not exist fmtcls.prg goto ERROR1

   if /I not "%1" == "/C" goto VERBOSE
   shift
   set HG_ROOT=
   set HG_HRB=
   set HG_BCC=
   set HG_CCOMP=
   set LIB_GUI=
   set LIB_HRB=
   set BIN_HRB=

:VERBOSE

   set HG_SILENT=^> nul
   if /I not "%1" == "/V" goto ROOT
   shift
   set HG_SILENT=

:ROOT

   if not "%HG_ROOT%" == "" goto TEST
   pushd "%HG_START_DP_FMT_COMPILE_BAT%\.."
   set HG_ROOT=%CD%
   popd

:TEST

   if /I "%1"=="XB55" ( shift & goto COMPILE_XB )
   if /I "%1"=="XB58" ( shift & goto COMPILE_XB )
   if /I "%1"=="XM"   ( shift & goto COMPILE_XM )

:DETECT_XB

   if not exist "%HG_ROOT%\compileXB.bat" goto DETECT_XM
   if exist "%HG_ROOT%\compileXM.bat" goto SYNTAX
   goto COMPILE_XB

:DETECT_XM

   if exist "%HG_ROOT%\compileXM.bat" goto COMPILE_XM

:SYNTAX

   echo Syntax:
   echo   To build with xHarbour and BCC 5.5.1
   echo       compile [/C] XB55 file [options]
   echo   To build with xHarbour and BCC 5.8.2
   echo       compile [/C] XB58 file [options]
   echo   To build with xHarbour and MinGW
   echo       compile [/C] XM file [options]
   echo.
   goto END

:COMPILE_XB

   if "%HG_HRB%"   == "" set HG_HRB=%HG_ROOT%\xhbcc
   if "%HG_BCC%"   == "" set HG_BCC=%HG_CCOMP%
   if "%HG_BCC%"   == "" if /I "%1" == "XB58" set HG_BCC=c:\Borland\BCC58
   if "%HG_BCC%"   == "" set HG_BCC=c:\Borland\BCC55
   set HG_CCOMP=%HG_BCC%
   if "%LIB_GUI%"  == "" set LIB_GUI=lib\xhb\bcc
   if "%LIB_HRB%"  == "" set LIB_HRB=lib
   if "%BIN_HRB%"  == "" set BIN_HRB=bin

   if exist ofmt.exe del ofmt.exe
   if exist ofmt.exe goto ERROR2

   echo xHarbour: Compiling sources...
   "%HG_HRB%\%BIN_HRB%\harbour.exe" fmt    -i%HG_HRB%\include;%HG_ROOT%\include -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3
   "%HG_HRB%\%BIN_HRB%\harbour.exe" fmtcls -i%HG_HRB%\include;%HG_ROOT%\include -n -w3 -gc0 -es2 -q0
   if errorlevel 1 goto ERROR3

   echo BCC32: Compiling sources...
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; -D__XHARBOUR__ fmt.c    %HG_SILENT%
   if errorlevel 1 goto ERROR3
   "%HG_BCC%\bin\bcc32.exe" -c -O2 -tW -tWM -M -d -a8 -OS -5 -6 -w -I%HG_HRB%\include;%HG_BCC%\include;%HG_ROOT%\include; -L%HG_HRB%\%LIB_HRB%;%HG_BCC%\lib; -D__XHARBOUR__ fmtcls.c %HG_SILENT%
   if errorlevel 1 goto ERROR3

   echo BRC32: Compiling resources...
   copy /b %HG_ROOT%\resources\oohg_bcc.rc + ofmt.rc _temp.rc /y %HG_SILENT%
   %HG_BCC%\bin\brc32.exe -r -i%HG_ROOT%\resources _temp.rc -I%HG_ROOT%\include %HG_SILENT%
   if errorlevel 1 goto ERROR3

   echo ILINK32: Linking...
   echo c0w32.obj + > b32.bc
   echo fmt.obj fmtcls.obj, + >> b32.bc
   echo ofmt.exe, + >> b32.bc
   echo ofmt.map, + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\oohg.lib + >> b32.bc
   for %%a in ( gtgui rtl vmmt rdd common hbsix dbffpt dbfntx macro ct lang pcrepos ) do if exist %HG_HRB%\%LIB_HRB%\%%a.lib echo %HG_HRB%\%LIB_HRB%\%%a.lib + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\bostaurus.lib + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\hbprinter.lib + >> b32.bc
   echo %HG_ROOT%\%LIB_GUI%\miniprint.lib + >> b32.bc
   for %%a in ( cw32mt import32 user32 winspool gdi32 comctl32 comdlg32 shell32 ole32 oleaut32 uuid mpr wsock32 ws2_32 mapi32 winmm vfw32 msimg32 iphlpapi ) do echo %%a.lib + >> b32.bc
   echo , , + >> b32.bc
   echo _temp.res + >> b32.bc
   "%HG_BCC%\bin\ilink32.exe" -Gn -Tpe -aa -L%HG_BCC%\lib;%HG_BCC%\lib\psdk; @b32.bc %HG_SILENT%

   if exist ofmt.exe goto OK_XB
   echo Build finished with ERROR !!!
   goto CLEAN_XB

:ERROR1

   echo This file must be executed from FMT folder !!!
   goto END

:ERROR2

   echo COMPILE ERROR: Is ofmt.exe running ?
   goto END

:ERROR3
   echo Build finished with ERROR !!!
   goto END

:OK_XB

   echo Build finished OK !!!

:CLEAN_XB

   for %%a in ( *.tds )   do del %%a %HG_SILENT%
   for %%a in ( *.c )     do del %%a %HG_SILENT%
   for %%a in ( *.map )   do del %%a %HG_SILENT%
   for %%a in ( *.obj )   do del %%a %HG_SILENT%
   for %%a in ( b32.bc )  do del %%a %HG_SILENT%
   for %%a in ( _temp.* ) do del %%a %HG_SILENT%
   goto END

:COMPILE_XM

   if "%HG_HRB%"   == "" set HG_HRB=%HG_ROOT%\xhmingw
   if "%HG_MINGW%" == "" set HG_MINGW=%HG_CCOMP%
   if "%HG_MINGW%" == "" set HG_MINGW=%HG_HRB%\comp\mingw
   set HG_CCOMP=%HG_MINGW%
   if "%LIB_GUI%"  == "" set LIB_GUI=lib\xhb\mingw
   if "%LIB_HRB%"  == "" set LIB_HRB=lib
   if "%BIN_HRB%"  == "" set BIN_HRB=bin

   if exist ofmt.exe del ofmt.exe
   if exist ofmt.exe goto ERROR2

   set "HG_PATH=%PATH%"
   set "PATH=%HG_MINGW%\bin;%HG_HRB%\%BIN_HRB%"

   echo xHarbour: Compiling sources...
   "%HG_HRB%\%BIN_HRB%\harbour.exe" fmt    -i%HG_HRB%\include;%HG_ROOT%\include -n -w3 -gc0 -es2 -q0
   "%HG_HRB%\%BIN_HRB%\harbour.exe" fmtcls -i%HG_HRB%\include;%HG_ROOT%\include -n -w3 -gc0 -es2 -q0

   echo GCC: Compiling...
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c fmt.c    -o fmt.o
   gcc -I. -I%HG_HRB%\include -I%HG_ROOT%\include -Wall -c fmtcls.c -o fmtcls.o

   echo WindRes: Compiling resource file...
   echo #define oohgpath %HG_ROOT%\RESOURCES > _oohg_resconfig.h
   echo #include "%HG_ROOT%\INCLUDE\oohgversion.h" >> _oohg_resconfig.h
   copy /b %HG_ROOT%\resources\ooHG.rc + ofmt.rc _temp.rc %HG_SILENT%
   windres -i _temp.rc -o _temp.o

   echo GCC: Linking...
   set HG_OBJS=%HG_HRB%\%LIB_HRB%\mainwin.o fmt.o fmtcls.o _temp.o
   set HG_LIBS=-lbostaurus -lhbprinter -lminiprint -looHG
   set HG_CFLAGS=-Wall -mwindows -static -static-libgcc
   set HG_XLIBS=-lrtl -lvmmt -lrdd -lcodepage -lcommon -lct -ldbfcdx -ldbffpt -ldbfntx -ldebug -lgtgui -lgtwin -lhbsix -lhsx -llang -lmacro -lpcrepos -lpp
   set HG_WLIBS=-luser32 -lwinspool -lgdi32 -lcomctl32 -lcomdlg32 -lole32 -loleaut32 -luuid -lmpr -lwsock32 -lws2_32 -lmapi32 -lwinmm -lvfw32 -lmsimg32 -liphlpapi
   set HG_SEARCH=-L. -L%HG_MINGW%\lib -L%HG_HRB%\%LIB_HRB% -L%HG_ROOT%\%LIB_GUI%
   gcc -o ofmt.exe %HG_OBJS% %HG_CFLAGS% %HG_SEARCH% -Wl,--start-group %HG_XLIBS% %HG_LIBS% %HG_WLIBS% -Wl,--end-group

   if exist ofmt.exe goto OK_XM
   echo Build finished with ERROR !!!
   goto CLEAN_XM

:OK_XM

   echo Build finished OK !!!

:CLEAN_XM

   if exist _temp.o del _temp.o %HG_SILENT%
   if exist _temp.rc del _temp.rc %HG_SILENT%
   if exist _oohg_resconfig.h del _oohg_resconfig.h %HG_SILENT%
   del fmt.o %HG_SILENT%
   del fmt.c %HG_SILENT%
   del fmtcls.o %HG_SILENT%
   del fmtcls.c %HG_SILENT%
   set "PATH=%HG_PATH%"
   set HG_OBJS=
   set HG_LIBS=
   set HG_CFLAGS=
   set HG_XLIBS=
   set HG_WLIBS=
   set HG_SEARCH=
   set HG_PATH=
   goto END

:END

   set HG_START_DP_FMT_COMPILE_BAT=
   echo.
