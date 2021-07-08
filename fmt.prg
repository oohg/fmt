/*
 * $Id: fmt.prg $
 */
/*
 * ooHG source code:
 * Formatter for OOHG based source code
 *
 * Copyright 2014-2021 Fernando Yurisich <fyurisich@oohg.org> and contributors of
 * the Object Oriented (x)Harbour GUI (aka OOHG) Project, https://oohg.github.io
 *
 * Adapted from the Harbour and xHarbour source code formatters
 * Copyright 2009 Alexander S.Kresin
 * <alex@belacy.belgorod.su>
 *
 * Portions of this project are based upon:
 *    "Harbour MiniGUI Extended Edition Library"
 *       Copyright 2005-2021 MiniGUI Team, http://hmgextended.com
 *    "Harbour GUI framework for Win32"
 *       Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
 *       Copyright 2001 Antonio Linares <alinares@fivetech.com>
 *    "Harbour MiniGUI"
 *       Copyright 2002-2016 Roberto Lopez <mail.box.hmg@gmail.com>
 *    "Harbour Project"
 *       Copyright 1999-2021 Contributors, https://harbour.github.io/
 */
/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file LICENSE.txt. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1335, USA (or download from http://www.gnu.org/licenses/).
 *
 * As a special exception, the ooHG Project gives permission for
 * additional uses of the text contained in its release of ooHG.
 *
 * The exception is that, if you link the ooHG libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the ooHG library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the ooHG
 * Project under the name ooHG. If you copy code from other
 * ooHG Project or Free Software Foundation releases into a copy of
 * ooHG, as the General Public License permits, the exception does
 * not apply to the code that you add in this way. To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for ooHG, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 */

#include "oohg.ch"
#include "directry.ch"

PROCEDURE Main( ... )

   LOCAL oRef, aParams, cFileName, cInitDir, i, cParam, lRecursive := .F., oMain, bMainBlock

   aParams := hb_AParams()

   IF Empty( aParams ) .OR. Left( cFileName := ATail( aParams ), 1 ) $ "@/-"
      About()
      RETURN
   ENDIF

   FOR EACH cParam IN aParams
      IF Left( cParam, 1 ) $ "-/"
         IF SubStr( cParam, 2 ) == "r"
            lRecursive := .T.
            cParam := "#"
            EXIT
         ENDIF
      ENDIF
   NEXT

   oRef := TFormatCode():New( aParams, hb_FNameMerge( hb_DirBase(), "ofmt.ini" ) )
   IF oRef:nErr > 0
      MsgStop( "Initialization error " + hb_ntos( oRef:nErr ) + iif( oRef:nLineErr == 0, " in parameter", " on line " + hb_ntos( oRef:nLineErr ) ) + ":" + oRef:cLineErr )
      _OOHG_ErrorLevel := 1
      RETURN
   ENDIF

   DEFINE WINDOW frm_Main OBJ oMain ;
      AT 0, 0 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      CLIENTAREA ;
      TITLE "OOHG Code Formatter" ;
      MAIN ;
      ICON "APPICO" ;
      NOMAXIMIZE ;
      NOSIZE ;
      ON INIT ( oMain:Closable := .F., Eval( bMainBlock ) )

      @ 10, 10 EDITBOX edt_Status ;
         WIDTH frm_Main.ClientWidth - 20 ;
         HEIGHT frm_Main.ClientHeight - 60 ;
         READONLY

      @ frm_Main.ClientHeight - 37, 10 PROGRESSBAR pgb_Progress ;
         WIDTH frm_Main.ClientWidth - 20 ;
         HEIGHT 24 ;
         SMOOTH

      ON KEY ESCAPE ACTION iif( oMain:Closable, oMain:Release(), NIL )
   END WINDOW

   IF "*" $ cFileName
      IF ( i := RAt( ".", cFileName ) ) == 0 .OR. SubStr( cFileName, i + 1, 1 ) < "A"
         MsgStop( "Wrong mask" )
         _OOHG_ErrorLevel := 2
         RETURN
      ENDIF

      cInitDir := iif( ( i := RAt( '\', cFileName ) ) == 0, '.\', Left( cFileName, i ) )
      cFileName := iif( i == 0, cFileName, SubStr( cFileName, i + 1 ) )
      bMainBlock := { || DirEval( cInitDir, cFileName, lRecursive, {| name | Reformat( oRef, name ) } ), oMain:Closable := .T. }
   ELSE
      bMainBlock := { || Reformat( oRef, cFileName ), oMain:Closable := .T. }
   ENDIF

   CENTER WINDOW frm_Main
   ACTIVATE WINDOW frm_Main

   RETURN

STATIC PROCEDURE ShowProgress( nItem, nTotal )

   frm_Main.pgb_Progress.Value := Int( nItem / nTotal * 100 )
   ProcessMessages()

   RETURN

STATIC PROCEDURE Reformat( oRef, cFileName )

   LOCAL aFile

   frm_Main.pgb_Progress.Value := 0

   IF Empty( aFile := oRef:File2Array( cFileName ) )
      frm_Main.edt_Status.Value += "File " + cFileName + " not found !" + Chr( 13 ) + Chr( 10 )
   ELSE
      oRef:bCallBack := {| nItem, nTotal | ShowProgress( nItem, nTotal ) }

      frm_Main.edt_Status.Value += "File " + cFileName

      IF oRef:Reformat( aFile )
         oRef:Array2File( cFileName, aFile )
         frm_Main.edt_Status.Value += ", " + hb_ntos( Len( aFile ) ) + " lines reformatted." + Chr( 13 ) + Chr( 10 )
      ELSE
         frm_Main.edt_Status.Value += ", error " + hb_ntos( oRef:nErr ) + " on line " + hb_ntos( oRef:nLineErr ) + ": " + oRef:cLineErr + Chr( 13 ) + Chr( 10 )
      ENDIF
   ENDIF

   frm_Main.pgb_Progress.Value := 100
   ProcessMessages()

   RETURN

STATIC PROCEDURE DirEval( cInitDir, cMask, lRecur, bCode )

   LOCAL aFile

   cInitDir := hb_DirSepAdd( cInitDir )
   cMask := iif( cMask == NIL, hb_osFileMask(), Upper( cMask ) )

   FOR EACH aFile IN Directory( cInitDir + cMask, "HSD" )
      IF "D" $ aFile[ F_ATTR ]
         IF ! ( "." == aFile[ F_NAME ] ) .AND. ;
               ! ( ".." == aFile[ F_NAME ] ) .AND. lRecur
            DirEval( cInitDir + aFile[ F_NAME ], cMask, lRecur, bCode )
         ENDIF
      ELSE
         IF bCode != NIL
            Eval( bCode, cInitDir + aFile[ F_NAME ] )
         ENDIF
      ENDIF
   NEXT

   RETURN

STATIC PROCEDURE About()

   MsgInfo( ;
      "OOHG Source Formatter" + Chr( 13 ) + Chr( 10 ) + ;
      "based on Harbour Source Formatter" + Chr( 13 ) + Chr( 10 ) + ;
      "Copyright (c) 2017-2018, OOHG Project, https://oohg.github.io/" + Chr( 13 ) + Chr( 10 ) + ;
      "Copyright (c) 2010-2018, Harbour Project, https://harbour.github.io/" + Chr( 13 ) + Chr( 10 ) + ;
      "Copyright (c) 2009, Alexander S.Kresin" + Chr( 13 ) + Chr( 10 ) + ;
      Chr( 13 ) + Chr( 10 ) + ;
      "Syntax:  ofmt [options] [@config] <file[s]>" + Chr( 13 ) + Chr( 10 ) )

   RETURN

#ifdef __XHARBOUR__

FUNCTION hb_DirSepAdd( cDir )

   LOCAL cSep := '\'

   IF ! ( Right( cDir, 1 ) == cSep )
      cDir += cSep
   ENDIF

   RETURN cDir

FUNCTION hb_DirBase()

   LOCAL cDirBase

   hb_FNameSplit( hb_argv( 0 ), @cDirBase )

   RETURN cDirBase

#endif
