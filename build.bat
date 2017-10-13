@echo off
rem
rem $Id: build.bat,v 1.22 2015/03/18 01:22:29 fyurisich Exp $
rem
cls

if /I "%1"=="/C" goto SET_ROOT
if "%HG_ROOT%"=="" goto SET_ROOT
goto BUILD

:SET_ROOT
set HG_ROOT=c:\oohg

:BUILD
copy hbformat.ini %HG_ROOT%\
set TPATH=%PATH%
set PATH=C:\Herramientas\Harbour\hb32\bin;C:\Herramientas\Harbour\hb32\comp\mingw
if /I "%1"=="/C" shift
HBMK2 ofmt.hbp %1 %2 %3 %4 %5 %6 %7 %8 %9
IF ERRORLEVEL 1 PAUSE

:CLEAN
set PATH=%TPATH%
set TPATH=
