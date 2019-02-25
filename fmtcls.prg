/*
 * $Id: hbfmtcls.prg $
 */
/*
 * ooHG source code:
 * Formatter for OOHG based source code
 *
 * Copyright 2014-2019 Fernando Yurisich <fyurisich@oohg.org> and contributors of
 * the Object Oriented (x)Harbour GUI (aka OOHG) Project, https://oohg.github.io
 *
 * Adapted from the Harbour and xHarbour source code formatters
 * Copyright 2009 Alexander S.Kresin
 * <alex@belacy.belgorod.su>
 *
 * Portions of this project are based upon:
 *    "Harbour MiniGUI Extended Edition Library"
 *       Copyright 2005-2019 MiniGUI Team, http://hmgextended.com
 *    "Harbour GUI framework for Win32"
 *       Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
 *       Copyright 2001 Antonio Linares <alinares@fivetech.com>
 *    "Harbour MiniGUI"
 *       Copyright 2002-2016 Roberto Lopez <mail.box.hmg@gmail.com>
 *    "Harbour Project"
 *       Copyright 1999-2019 Contributors, https://harbour.github.io/
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
 * Boston, MA 02110-1335,USA (or download from http://www.gnu.org/licenses/).
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

#include "hbclass.ch"
#include "fileio.ch"
#include "common.ch"

#xtranslate hb_LeftEq( <var>, <char> )            => ( Left( <var>, Len( <char> ) ) == <char> )

#ifdef __XHARBOUR__
   #pragma gc0

   #xtranslate hb_ps()                            =>   hb_OsPathSeparator()
   #xtranslate hb_StrShrink( <char>, <nLen> )     =>   Left( <char>, Len( <char> ) - <nLen> )
   #xtranslate hb_StrShrink( <char> )             =>   Left( <char>, Len( <char> ) - 1 )
   #xtranslate hb_AIns( <arr>, <nItem>, <value> ) =>   AIns( <arr>, <nItem> ) ;; <arr>\[ <nItem> \] := <value>
   #xtranslate hb_At( <subs>, <stroka>, <nPos2> ) =>   At( <subs>, <stroka>, <nPos2> )
   #xtranslate hb_DirScan( <cDir>, <cAtt> )       =>   DirectoryRecurse( <cDir> + <cAtt> )
   #xtranslate hb_MemoRead( <x> )                 =>   MemoRead( <x> )
#endif

#define RF_STATE_FUNC   1
#define RF_STATE_VAR    2
#define RF_STATE_CODE   3
#define RF_STATE_RET    4

CREATE CLASS TFormatCode

   VAR bCallback
   VAR cEol

   VAR nLineErr, nErr, cLineErr
   /* ::nErr posible values:
      0 = No error.
      1 = Parameter's format is not valid, expected: <option> = <value>.
      2 = Parameter's option is not a valid one ( see next group of vars ).
      3 = Parameter's value is not valid ( expected empty, number, ON, OFF, YES, NO ).
      4 = Parameter's value is not of the expected type.
      5 = A "FUNCTION", "PROCEDURE", "CLASS" or "METHOD" is defined inside another.
      6 = A control structure ( see ::aContStruc ) not properly closed.
    */

   // This vars may be changed by ::ReadIni
   VAR nEol                  INIT 1        // Eol: -1 = no change, 0 = OS default, 1 = DOS, 2 = UNIX.
   VAR lFCaseLow             INIT .F.      // If true, convert file name to lower case.
   VAR lNoTabs               INIT .T.      // If true, converts all tabs to spaces. Note that it's forced to true when lIndent if true.
   VAR lIndent               INIT .T.      // If true, indent code. Note that true forces lNoTabs to true.
   VAR lCase                 INIT .T.      // If true, make case conversion.
   VAR lSpaces               INIT .T.      // If true, reformat spaces.
   VAR lIndFunc              INIT .F.      // If true, indent "FUNCTION", "PROCEDURE", "CLASS", "METHOD".
   VAR lIndVar               INIT .T.      // If true, indent "LOCAL", "PRIVATE", etc. at the function beginning.
   VAR lIndDrt               INIT .F.      // If true, indent directives.
   VAR lIndRet               INIT .T.      // If true, indent "RETURN".
   VAR lIncOOHG              INIT .T.      // If true, OOHG structures included in aOtherStruc are added to aContStruc before processing.
   VAR nIndLeft              INIT 3        // Leftmost indent - amount of spaces.
   VAR nIndNext              INIT 3        // Indent - amount of spaces.
   VAR nIndCont              INIT 3        // Indent for continuation ( after ';' ) lines - amount of spaces.
   VAR lCnvAst               INIT .T.      // If true, convert '*' to '//'.
   VAR lCnvAmp               INIT .T.      // If true, convert '&&' to '//'.
   VAR nSpaceComment         INIT 1        // Number of spaces after '*', '//' and '/*' comments ( -1 = no change ).
   VAR lCnvNot               INIT .T.      // If true, convert .NOT. to !.
   VAR nCaseCmd              INIT 1        // Case of commands ( -1 = no change, 1 = upper, 2 = lower, 3 = title ).
   VAR nCaseBoo              INIT 1        // Case of boolean operators ( -1 = no change, 1 = upper, 2 = lower, 3 = title ).
   VAR nCaseFnc              INIT 4        // Case of functions ( -1 = no change, 1 = upper, 2 = lower, 3 = title, 4 = as in pattern ).
   VAR nCaseUnk              INIT -1       // Case of unknown functions ( -1 = no change, 1 = upper, 2 = lower, 3 = title ).
   VAR nCaseDrt              INIT 2        // Case of directives ( -1 = no change, 1 = upper, 2 = lower, 3 = title ).
   VAR nSpaceDrt             INIT 0        // Number of spaces after # in directives ( -1 = no change ).
   VAR nLineIf               INIT 1        // -1 = no change, 1 = insert empty line before #ifdef, #ifndef, #else, #elif and #endif directives, 2 = remove it.
   VAR nLineDump             INIT 1        // -1 = no change, 1 = insert empty line before #pragma begindump and #pragma enddump directives, 2 = remove it.
   VAR nLineFnc              INIT 1        // -1 = no change, 1 = insert empty line before "FUNCTION", "PROCEDURE", "CLASS" or "METHOD", 2 = remove it.
   VAR nLineRet              INIT 1        // -1 = no change, 1 = insert empty line before "RETURN", 2 = remove it.
   VAR nLineVar              INIT 1        // -1 = no change, 1 = insert empty line before variables declaration, 2 = remove it.
   VAR nLineCode             INIT 1        // -1 = no change, 1 = insert empty line before code in function, 2 = remove it.
   VAR nBr4Comma             INIT 3        // Max level of brackets nesting, for which a space is added after a comma.
   VAR nBr4Brac              INIT 3        // Max level of nesting in brackets, while space is added after/before a bracket.
   VAR cExtSave              INIT ""       // Extension of the formatted file ( "" = use original ).
   VAR cExtBack              INIT ".bak"   // Extension of the backup file.
   VAR cHBXList              INIT ""       // List of space separated .hbx files for specifying proper casing of function names.
   VAR lSilent               INIT .F.      // If true, run silently.
   VAR lNoSepBC              INIT .F.      // If true, converts '( ,' to '(,' and ', ,' to ',,'.
   VAR lOpAsPrfx             INIT .T.      // If true, converts '+ <number>' and '- <number>' to '+<number>' and '-<number>' after the words in cOpAsPrfx.

   VAR cCommands             INIT ""
   VAR cClauses              INIT ""
   VAR cFunctions            INIT ""
   VAR cOpAsPrfx             INIT ""
   VAR aContStruc            INIT { { "if",     "",              { "else", "elseif" },               { "endif", "end" },          { NIL, NIL }      }, ;
                                    { "do",     "while",         { "" },                             { "enddo", "end" },          { NIL, NIL }      }, ;
                                    { "while",  "",              { "" },                             { "enddo", "end" },          { "", NIL }       }, ;
                                    { "for",    "",              { "" },                             { "next", "endfor", "end" }, { NIL, NIL, NIL } }, ;
                                    { "do",     "case",          { "case", "otherwise" },            { "endcase", "end" },        { NIL, NIL }      }, ;
                                    { "with",   "object",        { "" },                             { "end" },                   { NIL }           }, ;
                                    { "begin",  "sequence",      { "recover", "always" },            { "end" },                   { NIL }           }, ;
                                    { "try",    "",              { "catch", "finally" },             { "end" },                   { NIL }           }, ;
                                    { "switch", "",              { "case", "otherwise", "default" }, { "endswitch", "end" },      { "", NIL }       } }
   /*
   TODO:
   Add support for OOHG specific structures.

   Some of these pairs may be separated (and also nested) by aContStruc:
      IF xxx                DEFINE WINDOW       DEFINE WINDOW
         DEFINE WINDOW      IF xxx              IF xxx
      ELSE                     ....                ....
         DEFINE WINDOW         END WINDOW          END WINDOW
      ENDIF                    ....                ....
      ....                   ENDIF              ELSE
      END WINDOW             ....                  ....
                             END WINDOW            END WINDOW
                                                   ....
                                                ENDIF

      DO CASE / CASE / OTHERWISE / ENDCASE
      SWITCH / CASE / OTHERWISE / DEFAULT / ENDSWITCH
      WITH OBJECT / END

   And others not:
      DO WHILE
         .... 
         DEFINE WINDOW          
            ....                
      ENDDO
      ...
      END WINDOW

      WHILE / ENDDO
      FOR / NEXT / ENDFOR
      BEGIN SEQUENCE / RECOVER / END SEQUENCE
      TRY / CATCH / FINALLY / END

      Use array at item 5 to list compatibilities or incompatibilities:
   */
   VAR aOtherStruc           INIT { { "begin",  "ini",           { "" },                             { "end" },            { "ini" }                }, ;
                                    { "define", "activex",       { "" },                             { "end" },            { "activex" }            }, ;
                                    { "define", "anigif",        { "" },                             { "end" },            { "anigif" }             }, ;
                                    { "define", "animatebox",    { "" },                             { "end" },            { "animatebox" }         }, ;
                                    { "define", "browse",        { "" },                             { "end" },            { "browse" }             }, ;
                                    { "define", "button",        { "" },                             { "end" },            { "button" }             }, ;
                                    { "define", "checkbox",      { "" },                             { "end" },            { "checkbox" }           }, ;
                                    { "define", "checkbutton",   { "" },                             { "end" },            { "checkbutton" }        }, ;
                                    { "define", "checklist",     { "" },                             { "end" },            { "checklist" }          }, ;
                                    { "define", "combobox",      { "" },                             { "end" },            { "combobox" }           }, ;
                                    { "define", "context",       { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "contextmenu",   { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "dropdown",      { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "datepicker",    { "" },                             { "end" },            { "datepicker" }         }, ;
                                    { "define", "editbox",       { "" },                             { "end" },            { "editbox" }            }, ;
                                    { "define", "frame",         { "" },                             { "end" },            { "frame" }              }, ;
                                    { "define", "grid",          { "" },                             { "end" },            { "grid" }               }, ;
                                    { "define", "hotkeybox",     { "" },                             { "end" },            { "hotkeybox" }          }, ;
                                    { "define", "hyperlink",     { "" },                             { "end" },            { "hyperlink" }          }, ;
                                    { "define", "image",         { "" },                             { "end" },            { "image" }              }, ;
                                    { "insert", "popup",         { "" },                             { "end" },            { "popup" }              }, ;
                                    { "define", "internal",      { "" },                             { "end" },            { "internal" }           }, ;
                                    { "define", "ipaddress",     { "" },                             { "end" },            { "ipaddress" }          }, ;
                                    { "define", "label",         { "" },                             { "end" },            { "label" }              }, ;
                                    { "define", "listbox",       { "" },                             { "end" },            { "listbox" }            }, ;
                                    { "define", "main",          { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "mainmenu",      { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "menu",          { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "monthcalendar", { "" },                             { "end" },            { "monthcalendar" }      }, ;
                                    { "define", "node",          { "" },                             { "end" },            { "node" }               }, ;
                                    { "define", "notify",        { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "notifymenu",    { "" },                             { "end" },            { "menu" }               }, ;
                                    { "define", "page",          { "" },                             { "end" },            { "page" }               }, ;
                                    { "define", "picture",       { "" },                             { "end" },            { "picture" }            }, ;
                                    { "define", "player",        { "" },                             { "end" },            { "player" }             }, ;
                                    { "define", "popup",         { "" },                             { "end" },            { "popup" }              }, ;
                                    { "define", "progressbar",   { "" },                             { "end" },            { "progressbar" }        }, ;
                                    { "define", "progressmeter", { "" },                             { "end" },            { "progressmeter" }      }, ;
                                    { "define", "radiogroup",    { "" },                             { "end" },            { "radiogroup" }         }, ;
                                    { "define", "richeditbox",   { "" },                             { "end" },            { "richeditbox" }        }, ;
                                    { "define", "scrollbar",     { "" },                             { "end" },            { "scrollbar" }          }, ;
                                    { "define", "slider",        { "" },                             { "end" },            { "slider" }             }, ;
                                    { "define", "spinner",       { "" },                             { "end" },            { "spinner" }            }, ;
                                    { "define", "splitbox",      { "" },                             { "end" },            { "splitbox" }           }, ;
                                    { "define", "statusbar",     { "" },                             { "end" },            { "statusbar" }          }, ;
                                    { "define", "tab",           { "" },                             { "end" },            { "tab" }                }, ;
                                    { "define", "tab page",      { "" },                             { "end" },            { "page" }               }, ;
                                    { "define", "textarray",     { "" },                             { "end" },            { "textarray" }          }, ;
                                    { "define", "textbox",       { "" },                             { "end" },            { "textbox" }            }, ;
                                    { "define", "timepicker",    { "" },                             { "end" },            { "timepicker" }         }, ;
                                    { "define", "toolbar",       { "" },                             { "end" },            { "toolbar" }            }, ;
                                    { "define", "tree",          { "" },                             { "end" },            { "tree" }               }, ;
                                    { "define", "window",        { "" },                             { "end" },            { "window" }             }, ;
                                    { "define", "xbrowse",       { "" },                             { "end" },            { "xbrowse" }            } }

   METHOD New( aParams, cIniName )
   METHOD SetOption( cLine, i, aIni )
   METHOD ReadIni( cIniName )
   METHOD Reformat( aFile )
   METHOD FormatLine( cLine, lIsContinuation )
   METHOD ConvertCmd( cLine, nBegin, nEnd, lOnlyCommands )
   METHOD ConvertFnc( cLine, nBegin, nEnd )
   METHOD ConvertBool( cLine, nBegin, nEnd )
   METHOD Source2Array( cSource )
   METHOD Array2Source( aSource )
   METHOD File2Array( cFileName )
   METHOD Array2File( cFileName, aSource )

   ENDCLASS

METHOD New( aParams, cIniName ) CLASS TFormatCode

   LOCAL cParam, cOOHGCmds, cOOHGClss, cOOHGFuns, cOOHGOaPs, aExt

   ::nErr := 0

   IF HB_ISSTRING( cIniName )
      IF ! ::ReadIni( cIniName )
         RETURN Self
      ENDIF
      FOR EACH cParam IN aParams
         IF hb_LeftEq( cParam, "@" )
            IF ! ::ReadIni( SubStr( cParam, 2 ) )
               RETURN Self
            ENDIF
         ELSEIF Left( cParam, 1 ) $ "-/"
            IF ! ::SetOption( SubStr( cParam, 2 ), 0 )
               RETURN Self
            ENDIF
         ENDIF
      NEXT
   ENDIF

   /* OOHG extensions */
   cOOHGCmds := "ACTIVATE,CENTER,DEFINE,EDITBOX,PROGRESSBAR"
   cOOHGClss := "ACTION,AT,CAPTION,CLIENTAREA,HEIGHT,ICON,INPUTMASK,MAIN,NOMAXIMIZE,NOSIZE,NUMERIC,OBJ,PROGID,READONLY,SMOOTH,TITLE,VALUE,WIDTH,WINDOW,"
   cOOHGFuns := ""
   cOOHGOaPs := "VALUE,DEFAULT,"
   IF ::lIncOOHG
      FOR EACH aExt IN ::aOtherStruc
         AAdd( ::aContStruc, aExt )
      NEXT
   ENDIF

   ::cCommands := "," + ;
      "IF,ELSE,ELSEIF,END,ENDIF,DO,WHILE,ENDDO,WITH,CASE,OTHERWISE,ENDCASE,BEGIN,ANNOUNCE,REQUEST,THREAD,DYNAMIC,EXTERNAL," + ;
      "FUNCTION,PROCEDURE,RETURN,CLASS,ENDCLASS,METHOD,DATA,LOCAL,PRIVATE,PUBLIC,STATIC,FIELD,MEMVAR,PARAMETERS,DECLARE," + ;
      "ACCEPT,APPEND,AVERAGE,CLEAR,CLOSE,COMMIT,CONTINUE,COPY,COUNT,CREATE,DEFAULT," + ;
      "DELETE,DISPLAY,EJECT,ERASE,EXIT,FOR,GO,GOTO,INDEX,INIT,INPUT,JOIN,KEYBOARD,LABEL,LIST,LOCATE," + ;
      "LOOP,MENU,NEXT,PACK,PRINT,QUIT,READ,RECALL,REINDEX,RELEASE,RENAME,REQUEST,REPLACE,RESTORE," + ;
      "RUN,SAVE,SEEK,SELECT,SET,SKIP,SORT,STORE,SUM,TEXT,TOTAL,UNLOCK,USE,VAR,WAIT,ZAP," + cOOHGCmds
   IF ! Right( ::cCommands, 1 ) == ","
      ::cCommands += ","
   ENDIF

   ::cClauses := "," + ;
      "ADDITIVE,ALIAS,ALL,BLANK,BOTTOM,BOX,COLOR,DATE,DELETED,EACH,EXTENDED,EXCLUSIVE,FROM,GET,IN," + ;
      "RANGE,READONLY,REST,SAY,SCREEN,ALTERNATE,BELL,CENTURY,CONFIRM,CONSOLE,CURSOR,DECIMALS,DELIMITERS,DEVICE,EPOCH,ESCAPE," + ;
      "EXACT,EXCLUSIVE,FILTER,FIXED,FORMAT,INHERIT,INTENSITY,KEY,LIKE,MARGIN,MESSAGE,NEW,NIL,OFF,ON,ORDER,PATH,PICTURE,PRINTER,PROMPT," + ;
      "PROTECTED,RELATION,SCOREBOARD,SEQUENCE,SOFTSEEK,STEP,STRUCTURE,TYPEAHEAD,UNIQUE,WRAP,TAG,TO,TOP,VALID,WHEN," + cOOHGClss
   IF ! Right( ::cClauses, 1 ) == ","
      ::cClauses += ","
   ENDIF

   ::cFunctions := "," + ;
      "iif,ISNIL,ISARRAY,ISBLOCK,ISCHARACTER,ISDATE,ISLOGICAL,ISMEMO,ISNUMBER,ISOBJECT,Main," + cOOHGFuns
   AddFunctionsFromFiles( @::cFunctions, ::cHBXList )
   IF ! Right( ::cFunctions, 1 ) == ","
      ::cFunctions += ","
   ENDIF

   ::cOpAsPrfx := "," + ;
      "IF,ELSEIF,WHILE,STEP,CASE,INIT,TO,SKIP,SAY,RETURN," + cOOHGOaPs
   IF ! Right( ::cOpAsPrfx, 1 ) == ","
      ::cOpAsPrfx += ","
   ENDIF

   DO CASE
   CASE ::nEol == 2
      ::cEol := Chr( 10 )
   CASE ::nEol == 1
      ::cEol := Chr( 13 ) + Chr( 10 )
   CASE ::nEol == 0
      ::cEol := Chr( 13 ) + Chr( 10 )
   ENDCASE
   IF ::lIndent
      ::lNoTabs := .T.
   ENDIF

   RETURN Self

STATIC FUNCTION BuiltInFunctionList()

   #ifndef __XHARBOUR__
      #define STREAMINCLUDE #pragma __streaminclude "harbour.hbx" | RETURN %s
   #else
      #define STREAMINCLUDE #pragma __streaminclude "hbextern.ch" | RETURN %s
   #endif

   STREAMINCLUDE

METHOD Reformat( aFile ) CLASS TFormatCode

   LOCAL i, iDelta := 0, nLen := Len( aFile ), cToken1, cToken2, nLenToken, nPos, nCol
   LOCAL nPosSep, cLine, cLineAll, nLineSegment, lAddRem
   LOCAL nContrState, nIndent, nDeep := 0, aDeep := {}
   LOCAL lPragmaDump := .F., lClass := .F., lComment := .F., nPosComment, lContinue := .F.
   LOCAL nStatePrev, nState := 0

   ::nErr := 0

   FOR i := 1 TO nLen
      // See if we are done
      IF aFile[ i ] == NIL
         EXIT
      ENDIF
      // Show progress
      IF ::bCallBack != NIL
         Eval( ::bCallBack, i, nLen )
      ENDIF
      // Expand tabs
      IF ::lNoTabs .OR. ::lIndent
         aFile[ i ] := StrTran( aFile[ i ], Chr( 9 ), " " )
      ENDIF
      // Remove trailing spaces
      aFile[ i ] := RTrim( aFile[ i ] )
      // Ignore empty line
      IF Empty( aFile[ i ] )
         aFile[ i ] := ""
         LOOP
      ENDIF

      IF lComment
         // If there's something after comment's end then move the rest of the line to a new line and loop
         IF ( nPos := FindNotQuoted( "*/", aFile[ i ] ) ) > 0
            lComment := .F.
            IF ! Empty( cToken1 := SubStr( aFile[ i ], nPos + 2 ) )
               aFile[ i ] := Left( aFile[ i ], nPos + 1 )
               nLen := rf_AINS( aFile, i + 1, cToken1 )
               iDelta++
            ENDIF
         ENDIF
         LOOP
      ENDIF

      cLineAll := LTrim( aFile[ i ] )

      // Process directives
      IF hb_LeftEq( cLineAll, "#" )
         cToken1 := Lower( hb_tokenGet( cLineAll, 1 ) )
         cToken2 := Lower( hb_tokenGet( cLineAll, 2 ) )
         IF Len( cToken1 ) == 1
            cToken1 += cToken2
            cToken2 := Lower( hb_tokenGet( cLineAll, 3 ) )
         ENDIF
         lAddRem := .F.
         IF cToken1 == "#pragma"
            IF cToken2 == "begindump"
               lPragmaDump := .T.
            ELSEIF cToken2 == "enddump"
               lPragmaDump := .F.
            ENDIF
            // Add or remove lines around #pragma begindump and #pragma enddump
            IF ::nLineDump > 0 .AND. i > 1 .AND. ( cToken2 == "begindump" .OR. cToken2 == "enddump" )
               lAddRem := .T.
            ENDIF
         ELSEIF cToken1 == "#ifdef" .OR. cToken1 == "#ifndef" .OR. cToken1 == "#else" .OR. cToken1 == "#elif"  .OR. cToken1 == "#endif"
            // Add or remove lines around #ifdef, #ifndef, #else", #elif and #endif"
            IF ::nLineIf > 0 .AND. i > 1
               lAddRem := .T.
            ENDIF
         ENDIF
         IF lAddRem
            nPos := i - 1
            IF ::nLineDump == 1
               IF ! Empty( aFile[ nPos ] )
                  nLen := rf_AINS( aFile, nPos + 1, "" )
                  iDelta++
                  i++
               ELSE
                  nPos--
               ENDIF
            ENDIF
            DO WHILE nPos > 1 .AND. Empty( aFile[ nPos ] )
               rf_ADEL( aFile, nPos )
               iDelta--
               i--
               nPos--
            ENDDO
         ENDIF
      ENDIF

      // Process *, // and && comments
      nPosComment := 0
      IF hb_LeftEq( cLineAll, "*" )
         nPosComment := 1
         IF ::lCnvAst
            cLineAll := "//" + SubStr( cLineAll, 2 )
         ENDIF
      ELSEIF ( nPos := FindNotQuoted( "//", cLineAll ) ) > 0
         nPosComment := nPos
      ELSEIF ( nPos := FindNotQuoted( "&&", cLineAll ) ) > 0
         nPosComment := nPos
         IF ::lCnvAmp
            cLineAll := Left( cLineAll, nPos - 1 ) + "//" + SubStr( cLineAll, nPos + 2 )
         ENDIF
      ENDIF
      // Change number of spaces after comment's start
      IF nPosComment > 0
         nPos := nPosComment + iif( SubStr( cLineAll, nPosComment, 1 ) == "*", 1, 2 )
         IF ::nSpaceComment >= 0
            cLineAll := Left( cLineAll, nPos - 1 ) + Space( ::nSpaceComment ) + LTrim( SubStr( cLineAll, nPos ) )
         ENDIF
      ENDIF
      // Process /* ... */ comments
      IF ( nPos := FindNotQuoted( "/*", cLineAll ) ) > 0 .AND. ( nPosComment == 0 .OR. nPosComment > nPos )
         nPosComment := nPos
         IF hb_At( "*/", cLineAll, nPos + 2 ) == 0
            lComment := .T.
         ENDIF
      ENDIF
      IF nPosComment == 1 .AND. nDeep == 0 .AND. nState == RF_STATE_RET
         nState := 0
      ENDIF

      IF lComment .OR. lPragmaDump
         // Do nothing and loop
      ELSEIF ! ::lIndent
         aFile[ i ] := ::FormatLine( aFile[ i ] )
      ELSE
         aFile[ i ] := cLineAll
         IF lContinue
            // This line is a continuation of the previous one
            aFile[ i ] := Space( ::nIndLeft + ::nIndNext * nDeep + ::nIndCont ) + ::FormatLine( aFile[ i ], .T. )
         ELSE
            nPosSep := 1
            nLineSegment := 1
            DO WHILE .T.
               nPos := nPosSep
               IF ! hb_LeftEq( aFile[ i ], "#" ) .AND. ;
                     ( nPosSep := FindNotQuoted( ";", aFile[ i ], nPosSep ) ) > 0 .AND. ;
                     nPosSep < Len( aFile[ i ] ) .AND. ( nPosComment == 0 .OR. nPosSep < nPosComment )
                  cLine := SubStr( aFile[ i ], nPos, nPosSep - nPos + 1 )
               ELSE
                  nPosSep := 0
                  cLine := SubStr( aFile[ i ], nPos, Len( aFile[ i ] ) - nPos + 1 )
               ENDIF

               nContrState := 0
               nStatePrev := nState
               cToken1 := Lower( hb_tokenGet( cLine, 1 ) )
               nLenToken := Len( cToken1 )
               nPos := 2
               DO WHILE nPos <= nLenToken .AND. SubStr( cToken1, nPos, 1 ) >= "_"
                  nPos++
               ENDDO
               IF nPos <= nLenToken
                  nLenToken := nPos - 1
                  cToken1 := Left( cToken1, nLenToken )
               ENDIF
               cToken2 := Lower( hb_tokenGet( cLine, 2 ) )
               IF hb_LeftEq( cToken1, "#" )
                  // Ignore directives
               ELSEIF nLenToken >= 4 .AND. ( ;
                     ( hb_LeftEq( "static", cToken1 ) .AND. ;
                     ( hb_LeftEq( "function", cToken2 ) .OR. hb_LeftEq( "procedure", cToken2 ) ) ) .OR. ;
                     ( Len( cToken2 ) >= 4 .AND. hb_LeftEq( "procedure", cToken2 ) .AND. ( "init" == cToken1 .OR. "exit" == cToken1 ) ) .OR. ;
                     hb_LeftEq( "function", cToken1 ) .OR. ;
                     hb_LeftEq( "procedure", cToken1 ) .OR. ;
                     ( "method" == cToken1 .AND. ! lClass ) .OR. ;
                     ( "class" == cToken1 .AND. ! lClass ) .OR. ;
                     ( "create" == cToken1 .AND. "class" == cToken2 .AND. ! lClass ) )
                  IF nDeep == 0
                     nState := RF_STATE_FUNC
                     IF "class" == cToken1 .OR. ( "create" == cToken1 .AND. "class" == cToken2 )
                        lClass := .T.
                     ENDIF
                  ELSE
                     ::nLineErr := i - iDelta
                     ::nErr := 5
                     ::cLineErr := cLine
                     RETURN .F.
                  ENDIF
               ELSEIF nLenToken >= 4 .AND. ( ;
                     hb_LeftEq( "request", cToken1 ) .OR. ;
                     hb_LeftEq( "announce", cToken1 ) .OR. ;
                     hb_LeftEq( "dynamic", cToken1 ) .OR. ;
                     hb_LeftEq( "external", cToken1 ) .OR. ;
                     hb_LeftEq( "thread", cToken1 ) )
                  nState := 0
               ELSEIF nLenToken >= 4 .AND. ( ;
                     hb_LeftEq( "local", cToken1 ) .OR. ;
                     hb_LeftEq( "private", cToken1 ) .OR. ;
                     hb_LeftEq( "public", cToken1 ) .OR. ;
                     hb_LeftEq( "field", cToken1 ) .OR. ;
                     hb_LeftEq( "static", cToken1 ) .OR. ;
                     hb_LeftEq( "memvar", cToken1 ) .OR. ;
                     hb_LeftEq( "parameters", cToken1 ) .OR. ;
                     hb_LeftEq( "declare", cToken1 ) )
                  IF nStatePrev == RF_STATE_FUNC
                     nState := RF_STATE_VAR
                  ENDIF
               ELSEIF cToken1 == "return" .OR. cToken1 == "endclass" .OR. ;
                     ( cToken1 == "end" .AND. cToken2 == "class" )
                  IF nDeep == 0
                     nState := RF_STATE_RET
                  ENDIF
               ELSE
                  IF nState > 0
                     nState := RF_STATE_CODE
                  ENDIF
                  IF ( nContrState := AScan( ::aContStruc, {| a | a[ 1 ] == cToken1 .AND. ( Empty( a[ 2 ] ) .OR. a[ 2 ] == cToken2 ) } ) ) > 0
                     IF Len( aDeep ) < ++nDeep
                        AAdd( aDeep, NIL )
                     ENDIF
                     aDeep[ nDeep ] := nContrState
                  ELSEIF ( nContrState := AScan( ::aContStruc, {| a | AScan( a[ 3 ], {| e | e == cToken1 } ) > 0 } ) ) > 0
                     // do nothing
                  ELSEIF nDeep > 0 .AND. ( nPos := AScan( ::aContStruc, {| a | ( nCol := AScan( a[ 4 ], {| e | e == cToken1 } ) ) > 0 } ) ) > 0
                     // lines are indented and cToken1 is a closing token
                     IF aDeep[ nDeep ] != nPos
                        // search until nPos corresponds to the currently opened structure
                        DO WHILE ( nPos := AScan( ::aContStruc, {| a | ( nCol := AScan( a[ 4 ], {| e | e == cToken1 } ) ) > 0 }, nPos + 1 ) ) > 0 .AND. aDeep[ nDeep ] != nPos
                        ENDDO
                     ENDIF
                     IF aDeep[ nDeep ] == nPos .AND. ( ::aContStruc[ nPos, 5, nCol ] == NIL .OR. ::aContStruc[ nPos, 5, nCol ] == cToken2 )
                        // ok
                        nDeep--
                     ELSE
                        ::nLineErr := i - iDelta
                        ::nErr := 6
                        ::cLineErr := cLine
                        RETURN .F.
                     ENDIF
                  ENDIF
               ENDIF
               IF nLineSegment == 1
                  IF nState == 0
                     nIndent := 0
                  ELSEIF nState == RF_STATE_FUNC
                     nIndent := iif( ::lIndFunc, ::nIndLeft, 0 )
                  ELSEIF nState == RF_STATE_VAR
                     nIndent := iif( ::lIndVar, ::nIndLeft, 0 )
                  ELSEIF nState == RF_STATE_RET
                     nIndent := iif( ! lClass .AND. ::lIndRet, ::nIndLeft, 0 )
                  ELSE
                     nIndent := ::nIndLeft + ::nIndNext * iif( nContrState == 0, nDeep, nDeep - 1 )
                  ENDIF
                  IF hb_LeftEq( cLine, "#" ) .AND. ! ::lIndDrt
                     nIndent := 0
                  ENDIF
                  cLineAll := Space( nIndent ) + ::FormatLine( cLine )
                  // Add or remove lines around return, function, vars and code
                  IF i > 1 .AND. ( ( nState == RF_STATE_RET .AND. ::nLineRet > 0 .AND. nStatePrev != RF_STATE_FUNC ) .OR. ;
                                   ( nState == RF_STATE_FUNC .AND. ::nLineFnc > 0 /* .AND. nStatePrev > 0 */ ) .OR. ;
                                   ( nState == RF_STATE_VAR .AND. nStatePrev != nState .AND. ::nLineVar > 0 ) .OR. ;
                                   ( nState == RF_STATE_CODE .AND. nStatePrev != nState .AND. ::nLineCode > 0 ) )
                     nPos := i - 1
                     IF ( nState == RF_STATE_RET  .AND. ::nLineRet == 1 ) .OR. ;
                           ( nState == RF_STATE_FUNC .AND. ::nLineFnc == 1 ) .OR. ;
                           ( nState == RF_STATE_VAR  .AND. ::nLineVar == 1 ) .OR. ;
                           ( nState == RF_STATE_CODE .AND. ::nLineCode == 1 )
                        IF nState == RF_STATE_FUNC .AND. SubStr( aFile[ nPos ], 1, 2 ) == "/*"
                           nPos --
                        ENDIF
                        IF nPos > 1
                           IF Empty( aFile[ nPos ] )
                              nPos--
                           ELSE
                              nLen := rf_AINS( aFile, nPos + 1, "" )
                              iDelta++
                              i++
                           ENDIF
                        ENDIF
                     ENDIF
                     DO WHILE nPos >= 1 .AND. Empty( aFile[ nPos ] )
                        rf_ADEL( aFile, nPos )
                        iDelta--
                        i--
                        nPos--
                     ENDDO
                  ENDIF
               ELSE
                  cLineAll += ::FormatLine( cLine )
               ENDIF
               IF nState == RF_STATE_RET
                  IF lClass
                     lClass := .F.
                  ENDIF
                  nState := 0
               ENDIF
               IF nPosSep == 0 .OR. nPosSep == Len( aFile[ i ] )
                  EXIT
               ENDIF
               nPosSep++
               nLineSegment++
            ENDDO
            aFile[ i ] := cLineAll
         ENDIF
         IF ( nPosComment > 0 .AND. Right( RTrim( Left( aFile[ i ], nPosComment - 1 ) ), 1 ) == ';' ) .OR. ;
               ( nPosComment == 0 .AND. Right( aFile[ i ], 1 ) == ';' )
            lContinue := .T.
         ELSE
            lContinue := .F.
         ENDIF
      ENDIF
   NEXT

   RETURN .T.

#define FL_STATE_DIGIT   1
#define FL_STATE_ANY     2
#define FL_STATE_OP      3
#define FL_STATE_STRING  4
#define FL_STATE_QUOTED 11
#define FL_STATE_SQBR   12

METHOD FormatLine( cLine, lIsContinuation ) CLASS TFormatCode

   LOCAL i, nLen, c, nState := 0, cSymb, cToken, nPos := 1, cNext
   LOCAL lFirst, nBegin, nEnd, nB := 0, nA := 0, aBrackets := {0, 0}
   LOCAL cOperators := "+-*/%#=^<>$!", lMacro := .F.

   IF ! ::lCase .AND. ! ::lSpaces
      RETURN cLine
   ENDIF

   IF ! HB_ISLOGICAL( lIsContinuation )
      lIsContinuation := .F.
   ENDIF
   lFirst := ! lIsContinuation

   nLen := Len( cLine )

   DO WHILE SubStr( cLine, nPos, 1 ) == " "
      nPos++
   ENDDO

   IF lFirst .AND. hb_LeftEq( cLine, "#" )
      IF ::lSpaces .AND. ::nSpaceDrt != -1
         cLine := Left( cLine, nPos ) + Space( ::nSpaceDrt ) + LTrim( SubStr( cLine, nPos + 1 ) )
      ENDIF
      nLen := Len( cLine )
      IF ::lCase .AND. ::nCaseDrt != -1
         nPos++
         DO WHILE SubStr( cLine, nPos, 1 ) == " "
            nPos++
         ENDDO
         i := nPos
         DO WHILE nPos <= nLen .AND. SubStr( cLine, nPos, 1 ) >= "A"
            nPos++
         ENDDO
         IF SubStr( cLine, nPos, 1 ) >= "A"
            nPos++
         ENDIF
         cToken := SubStr( cLine, i, nPos - i )
         cToken := iif( ::nCaseDrt == 1, Upper( cToken ), iif( ::nCaseDrt == 2, Lower( cToken ), Upper( Left( cToken, 1 ) ) + Lower( SubStr( cToken, 2 ) ) ) )
         cLine := Left( cLine, i - 1 ) + cToken + iif( nPos > nLen, "", SubStr( cLine, nPos ) )
      ENDIF

      RETURN cLine
   ENDIF

   FOR i := nPos TO nLen
      c := SubStr( cLine, i, 1 )

      IF ( nState == FL_STATE_QUOTED .AND. c == cSymb ) .OR. ( nState == FL_STATE_SQBR .AND. c == "]" )
         nState := FL_STATE_ANY
      ELSEIF nState <= FL_STATE_STRING
         IF ( c >= "0" .AND. c <= "9" ) .OR. ( c >= "A" .AND. c <= "Z" ) .OR. ( c >= "a" .AND. c <= "z" ) .OR. c == "_"
            IF nState < FL_STATE_STRING .OR. ( nState == FL_STATE_STRING .AND. nEnd > nBegin )
               IF nState == FL_STATE_STRING
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
               IF c >= "A"
                  nState := FL_STATE_STRING
                  nBegin := nEnd := i
               ELSE
                  nState := FL_STATE_DIGIT
               ENDIF
            ENDIF
         ELSEIF  c == "'" .OR. c == '"'
            IF lMacro
               lMacro := .F.
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
            ENDIF
            cSymb := c
            nState := FL_STATE_QUOTED
         ELSEIF c == "["
            IF lMacro
               lMacro := .F.
               nA := i
               nState := FL_STATE_ANY
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
               nA := i
               nState := FL_STATE_ANY
            ELSEIF  nState == FL_STATE_ANY
               nA := i
            ELSE
               nState := FL_STATE_SQBR
            ENDIF
         ELSEIF c == "/" .AND. SubStr( cLine, i + 1, 1 ) == "/"
            IF ! lMacro .AND. nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
            ENDIF
            EXIT
         ELSEIF c == "/" .AND. SubStr( cLine, i + 1, 1 ) == "*"
            IF lMacro
               lMacro := .F.
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
            ENDIF
            nState := FL_STATE_ANY
            DO WHILE i < nLen .AND. SubStr( cLine, i, 2 ) # "*/"   // NOTE: this must come before ELSEIF c $ cOperators
               i ++
            ENDDO
            IF i == nLen
               // Comment is not closed
               EXIT
            ENDIF
            i ++
         ELSEIF c == "(" .OR. c == "{"
            aBrackets[ iif( c == "(", 1, 2 ) ]++
            IF lMacro
               lMacro := .F.
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               IF ( ! lFirst .OR. ! ::ConvertCmd( @cLine, nBegin, nEnd, .T. ) ) .AND. c == "("
                  ::ConvertFnc( @cLine, nBegin, nEnd )
               ENDIF
            ENDIF
            IF aBrackets[ iif( c == "(", 1, 2 ) ] <= ::nBr4Brac .AND. i < nLen .AND. !( SubStr( cLine, i + 1, 1 ) $ iif( c == "(", " )", " |}" ) )
               nA := i
            ENDIF
            nState := FL_STATE_ANY
         ELSEIF c == "&"
            lMacro := .T.
            IF nState == FL_STATE_STRING
               IF i > 1 .AND. SubStr( cLine, i - 1, 1 ) == " "
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
            ENDIF
            nState := FL_STATE_ANY
         ELSEIF c == "."
            IF nState == FL_STATE_STRING
               IF nBegin > 1 .AND. SubStr( cLine, nBegin - 1, 1 ) == "." .AND. nEnd == nBegin
                  IF ::ConvertBool( @cLine, nBegin, i )
                     IF Len( cLine ) != nLen
                        /* .NOT. was converted to ! */
                        i -= ( nLen - Len( cLine ) )
                        nLen := Len( cLine )
                     ELSE
                        nB := nBegin - 1
                        nA := i
                     ENDIF
                  ENDIF
               ELSE
                  IF ! lMacro
                     IF nEnd == nBegin
                        nEnd := i
                     ENDIF
                     ::ConvertCmd( @cLine, nBegin, nEnd )
                  ENDIF
               ENDIF
            ENDIF
            nState := FL_STATE_ANY
         ELSEIF c == ","
            IF lMacro
               lMacro := .F.
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
            ENDIF
            IF aBrackets[ 1 ] <= ::nBr4Comma .AND. aBrackets[ 2 ] <= ::nBr4Comma
               nA := i
            ENDIF
            nState := FL_STATE_ANY
         ELSEIF c == "@" .AND. ::lSpaces
            IF i == 1
               IF  i < nLen
                  i++
                  DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                     i++
                  ENDDO
                  i--
                  cLine := "@ " + SubStr( cLine, i + 1 )
                  nLen  := Len( cLine )
                  i := 3
               ENDIF
            ELSE
               nEnd := i
               i++
               DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                  i++
               ENDDO
               i--
               cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )
               nLen  := Len( cLine )
               i := nEnd + 1
            ENDIF
         ELSEIF c == " "
            IF ::lSpaces
               nEnd := i
               i++
               DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                  i++
               ENDDO
               i--
               IF i > nEnd
                  // Replace multiples spaces with only one.
                  cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )
                  nLen  := Len( cLine )
                  i := nEnd
               ENDIF
               /*
                  Remove spaces before a comma but ignore '( ,' and ', ,' if ::lNoSepBC is false.
                  Remove spaces between this pairs '{ }', ´( )', '] [', '{ |', '| |'
                  Remove spaces between 'identificator' and '['.
                  Remove spaces between 'identificator' and '++', '--' or '->'.
                */
               IF ( SubStr( cLine, i + 1, 1 ) == "," .AND. ( ! SubStr( cLine, nEnd - 1, 1 ) $ "(," .OR. ::lNoSepBC ) ) .OR. ;
                     ( "*" + SubStr( cLine, nEnd - 1, 1 ) + SubStr( cLine, i + 1, 1 ) + "*" $ "*{}*()*][*{|*||*" ) .OR. ;
                     ( nState == FL_STATE_STRING .AND. SubStr( cLine, i + 1, 1 ) == "[" ) .OR. ;
                     ( nState == FL_STATE_STRING .AND. "*" + SubStr( cLine, i + 1, 2 ) + "*" $ "*--*++*" ) .OR. ;
                     ( SubStr( cLine, nEnd - 1, 1 ) == ")" .AND. SubStr( cLine, i + 1, 2 ) == "->" )
                  cLine := Left( cLine, nEnd - 1 ) + SubStr( cLine, i + 1 )
                  nLen  := Len( cLine )
                  i := nEnd - 1
               ENDIF
            ENDIF
         ELSEIF c $ cOperators .OR. ( c == ":" .AND. SubStr( cLine, i + 1, 1 ) == "=" )   // cOperators := "+-*/%#=^<>$!"
            IF !( ( cNext := SubStr( cLine, i + 1, 1 ) ) $ cOperators )
               cNext := ""
            ENDIF
            DO CASE
            CASE Empty( cNext )
               IF c $ "+-"
                  /*
                     For + - operators:
                     1- Always add a space before them.
                     2- If preceded by an identificator then add a space after them,                  'a - b'
                     3- unless ::lOpAsPrfx is .T. and the identificator is in ::cOpAsPrfx
                        list (for TO, STEP and the like) in which case all spaces must be removed.    'STEP -b'
                     4- If preceded by a number or ) or ] or " or ' then add a space after them.      '9 - b'     ') - b'    '] - b'   '" - '   "' - "
                     5- If followed by " or ' then add a space after them.                            '- "'       "- '"
                     6- Otherwise remove all spaces between them and the following token.             'x := -2'   'a * -b'   ', -b'    '( -b'   '| -b'   '+ -b'
                  */
                  nB := i                                                          // 1
                  IF lMacro .OR. nState == FL_STATE_STRING
                     IF ! lMacro .AND. nState == FL_STATE_STRING
                        IF nEnd == nBegin
                           nEnd := i
                        ENDIF
                        ::ConvertCmd( @cLine, nBegin, nEnd )
                     ENDIF
                     IF ::lOpAsPrfx .AND. "," + Upper( SubStr( cLine, nBegin, nEnd - nBegin ) ) + "," $ ::cOpAsPrfx
                        nEnd := i
                        i++
                        DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                           i++
                        ENDDO
                        i--
                        IF ::lSpaces
                           cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )   // 3
                           nLen  := Len( cLine )
                           i := nEnd
                        ENDIF
                     ELSE
                        nA := i                                                    // 2
                     ENDIF
                  ELSEIF nState == FL_STATE_DIGIT .OR. PrevNonSpaceChar( cLine, i ) $ ")]'" + '."'
                     nA := i                                                       // 4
                  ELSEIF PostNonSpaceChar( cLine, i ) $ "'" + '"'
                     nA := i                                                       // 5
                  ELSE
                     nEnd := i
                     i++
                     DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                        i++
                     ENDDO
                     i--
                     IF ::lSpaces
                        cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )      // 6
                        nLen  := Len( cLine )
                        i := nEnd
                     ENDIF
                  ENDIF
               ELSEIF c == "!"
                  /*
                     For ! operator:
                     1- Add a space before it.
                     2- Remove all spaces after it if followed by (.
                     3- Add a space after if not followed by (.
                  */
                  nB := i                                                          // 1
                  nEnd := i
                  i++
                  DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                     i++
                  ENDDO
                  i--
                  IF ::lSpaces
                     cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )         // 2
                     nLen  := Len( cLine )
                     i := nEnd
                  ENDIF
                  IF ! SubStr( cLine, i + 1, 1 ) == "("
                     nA := i                                                       // 3
                  ENDIF
               ELSE
                  /*
                     For * / % # = ^ < > $ operators:
                     1- Add a space before and after them.
                  */
                  nB := nA := i   // 1
               ENDIF
               nState := FL_STATE_OP
            CASE "|" + c + cNext + "|" $ "|+=|-=|**|*=|/=|%=|==|^=|<=|<>|>=|!=|:=|"
               /*
                  For += -= ** *= /= %= == ^= <= <> >= != := operators:
                  1- Add a space before them.
                  2- Add a space after them.
               */
               nB := i                                                             // 1
               IF ! lMacro .AND. nState == FL_STATE_STRING
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
               i ++
               nA := i                                                             // 2
               nState := FL_STATE_OP
            CASE "|" + c + cNext + "|" $ "|++|--|"
               /*
                  For ++ -- operators:
                  1- Do not separate id++, id--, id->id. See previous ELSEIF c == " ".
                  2- Add a space after id++, id--.
                  3- Add a space before ++id, --id.
                  4- Do not separate ++id, --id.
                */
               IF lMacro
                  nB := 0                                                          // 1
                  i ++
                  nA := i                                                          // 2
               ELSEIF nState == FL_STATE_STRING
                  nB := 0                                                          // 1
                  IF nState == FL_STATE_STRING
                     IF nEnd == nBegin
                        nEnd := i
                     ENDIF
                     ::ConvertCmd( @cLine, nBegin, nEnd )
                  ENDIF
                  i ++
                  nA := i                                                          // 2
               ELSE
                  nB := i                                                          // 3
                  i++
                  nEnd := i
                  i++
                  DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                     i++
                  ENDDO
                  i--
                  IF ::lSpaces
                     cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )         // 4
                     nLen  := Len( cLine )
                     i := nEnd
                  ENDIF
                  nA := 0
               ENDIF
               nState := FL_STATE_OP
            CASE c + cNext == "->"
               /*
                  For -> operators:
                  1- Do not separate from previous token. See previous ELSEIF c == " ".
                  2- Do not separate from following token. See previous ELSEIF c == " ".
               */
               nB := 0                                                             // 1
               IF ! lMacro .AND. nState == FL_STATE_STRING
                  IF nEnd == nBegin
                     nEnd := i
                  ENDIF
                  ::ConvertCmd( @cLine, nBegin, nEnd )
               ENDIF
               i++
               nEnd := i
               i++
               DO WHILE i <= nLen .AND. SubStr( cLine, i, 1 ) == " "
                  i++
               ENDDO
               i--
               IF ::lSpaces
                  cLine := Left( cLine, nEnd ) + SubStr( cLine, i + 1 )            // 2
                  nLen  := Len( cLine )
                  i := nEnd
               ENDIF
               nA := 0
               nState := FL_STATE_OP
            CASE cNext $ cOperators
               /*
                  1- cNext is a not a valid compound operator so add spaces before and after c and continue processing.
               */
               nB := nA := i                                                       // 1
               nState := FL_STATE_OP
            ENDCASE
            lMacro := .F.
         ELSEIF c == "|"
            IF lMacro
               nB := i
               lMacro := .F.
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
               nB := i
            ELSEIF i >= 3 .AND. SubStr( cLine, i - 2, 2 ) == "| "
               IF ::lSpaces
                  cLine := Left( cLine, i - 2 ) + SubStr( cLine, i )
                  nLen  := Len( cLine )
                  i--
               ENDIF
            ENDIF
            nA := i
            nState := FL_STATE_ANY
         ELSEIF c == ")" .OR. c == "}" .OR. c == "]"
            lMacro := .F.
            IF ::lSpaces
               nEnd := i
               i--
               DO WHILE i >= 1 .AND. SubStr( cLine, i, 1 ) == " "
                  i--
               ENDDO
               i++
               cLine := Left( cLine, i - 1 ) + SubStr( cLine, nEnd )
               nLen  := Len( cLine )
            ENDIF
            IF aBrackets[ iif( c == "(", 1, 2 ) ] <= ::nBr4Brac .AND. i > 1 .AND. !( SubStr( cLine, i - 1, 1 ) $ " ({" )
               nB := i
            ENDIF
            aBrackets[ iif( c == ")", 1, 2 ) ]--
            nState := FL_STATE_ANY
         ELSEIF c == ";"
            IF lMacro
               lMacro := .F.
            ELSEIF nState == FL_STATE_STRING
               IF nEnd == nBegin
                  nEnd := i
               ENDIF
               ::ConvertCmd( @cLine, nBegin, nEnd )
            ENDIF
            IF i > 1 .AND. SubStr( cLine, i - 1, 1 ) != " "
               nB := i
            ENDIF
            nState := FL_STATE_ANY
         ENDIF

         IF lFirst .AND. nState != FL_STATE_STRING
            lFirst := .F.
         ENDIF

         // Add space after char nA
         IF nA != 0 .AND. ::lSpaces .AND. nA < nLen .AND. !( SubStr( cLine, nA + 1, 1 ) $ " ," )
            cLine := Left( cLine, nA ) + " " + SubStr( cLine, nA + 1 )
            nLen++
            i++
         ENDIF
         // Add space before char nB
         IF nB > 1 .AND. ::lSpaces .AND. SubStr( cLine, nB - 1, 1 ) != " "
            cLine := Left( cLine, nB - 1 ) + " " + SubStr( cLine, nB )
            nLen++
            i++
         ENDIF
         nA := nB := 0
      ENDIF

      IF i == nLen .AND. nState == FL_STATE_STRING
         i++
         ::ConvertCmd( @cLine, nBegin, i )
      ENDIF
   NEXT

   RETURN cLine

STATIC FUNCTION PrevNonSpaceChar( cLine, i )

   i --
   DO WHILE i > 0 .AND. SubStr( cLine, i, 1 ) == " "
      i --
   ENDDO

   RETURN iif( i > 0, SubStr( cLine, i, 1 ), "" )

STATIC FUNCTION PostNonSpaceChar( cLine, i )

   i ++
   DO WHILE i <= Len( cLine ) .AND. SubStr( cLine, i, 1 ) == " "
      i ++
   ENDDO

   RETURN iif( i <= Len( cLine ), SubStr( cLine, i, 1 ), "" )

METHOD ConvertCmd( cLine, nBegin, nEnd, lOnlyCommands ) CLASS TFormatCode

   LOCAL nPos, cToken

   IF ::lCase
      IF SubStr( cLine, nEnd, 1 ) == "("
         RETURN .F.
      ENDIF

      IF ! HB_ISLOGICAL( lOnlyCommands )
         lOnlyCommands := .F.
      ENDIF

      cToken := Upper( SubStr( cLine, nBegin, nEnd - nBegin ) )

      IF ( ( nPos := At( "," + cToken, ::cCommands ) ) > 0 .AND. ( Len( cToken ) >= 4 .OR. SubStr( ::cCommands, nPos + Len( cToken ) + 1, 1 ) == "," ) ) .OR. ;
            ( ! lOnlyCommands .AND. ( nPos := At( "," + cToken, ::cClauses ) ) > 0 .AND. ( Len( cToken ) >= 4 .OR. SubStr( ::cClauses, nPos + Len( cToken ) + 1, 1 ) == "," ) )
         IF ::nCaseCmd > 0
            IF ::nCaseCmd > 1
               cToken := iif( ::nCaseCmd == 2, Lower( cToken ), Left( cToken, 1 ) + Lower( SubStr( cToken, 2 ) ) )
            ENDIF
            cLine := iif( nBegin == 1, cToken + SubStr( cLine, nEnd ),  Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd ) )
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

METHOD ConvertFnc( cLine, nBegin, nEnd ) CLASS TFormatCode

   LOCAL nPos, cToken

   IF ::lCase .AND. ::nCaseFnc > 0

      cToken := Upper( SubStr( cLine, nBegin, nEnd - nBegin ) )

      IF ( nPos := At( "," + cToken + ",", Upper( ::cFunctions ) ) ) > 0
         IF ::nCaseFnc > 1
            nPos++
            cToken := iif( ::nCaseFnc == 2, Lower( cToken ), iif( ::nCaseFnc == 3, ;
               Left( cToken, 1 ) + Lower( SubStr( cToken, 2 ) ), ;
               SubStr( ::cFunctions, nPos, Len( cToken ) ) ) )
         ENDIF
         cLine := iif( nBegin == 1, cToken + SubStr( cLine, nEnd ), ;
            Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd ) )
      ELSEIF ::nCaseUnk > 0
         cToken := iif( ::nCaseUnk == 2, Lower( cToken ), ;
            Left( cToken, 1 ) + Lower( SubStr( cToken, 2 ) ) )
         cLine := iif( nBegin == 1, cToken + SubStr( cLine, nEnd ), ;
            Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd ) )
      ENDIF
   ENDIF

   RETURN .T.

METHOD ConvertBool( cLine, nBegin, nEnd ) CLASS TFormatCode

   LOCAL cBool
   LOCAL nPos, cToken

   IF ::lCase

      cBool := ",NOT,AND,OR,F,T,"
      cToken := Upper( SubStr( cLine, nBegin, nEnd - nBegin ) )

      IF ( nPos := At( "," + cToken + ",", cBool ) ) > 0
         IF ::lCnvNot .AND. nPos == 1
            cLine := Left( cLine, nBegin - 2 ) + "!" + SubStr( cLine, nEnd + 1 )
         ELSE
            IF ::nCaseBoo > 0
               IF ::nCaseBoo > 1
                  cToken := iif( ::nCaseBoo == 2, Lower( cToken ), Left( cToken, 1 ) + ;
                     Lower( SubStr( cToken, 2 ) ) )
               ENDIF
               cLine := Left( cLine, nBegin - 1 ) + cToken + SubStr( cLine, nEnd )
            ENDIF
         ENDIF
      ELSE
         RETURN .F.
      ENDIF
   ENDIF

   RETURN .T.

METHOD SetOption( cLine, i, aIni ) CLASS TFormatCode

   LOCAL nPos, cToken1, cToken2, cTemp, xRes

   IF ( nPos := At( "=", cLine ) ) > 0
      cToken1 := Upper( RTrim( Left( cLine, nPos - 1 ) ) )
      cToken2 := LTrim( SubStr( cLine, nPos + 1 ) )
      IF __objHasMsg( Self, cToken1 )
         IF Empty( cToken2 )
            xRes := ""
         ELSEIF IsDigit( cToken2 ) .OR. ( hb_LeftEq( cToken2, "-" ) .AND. IsDigit( LTrim( SubStr( cToken2, 2 ) ) ) )
            xRes := Val( cToken2 )
         ELSEIF IsAlpha( cToken2 )
            cTemp := Upper( cToken2 )
            IF cTemp == "ON" .OR. cTemp == "YES"
               xRes := .T.
            ELSEIF cTemp == "OFF" .OR. cTemp == "NO"
               xRes := .F.
            ELSE
               IF Right( cToken2, 1 ) == ";" .AND. aIni != NIL
                  xRes := RTrim( hb_StrShrink( cToken2 ) )
                  DO WHILE ++i < Len( aIni )
                     IF Right( aIni[ i ], 1 ) == ";"
                        xRes += AllTrim( hb_StrShrink( aIni[ i ] ) )
                     ELSE
                        xRes += AllTrim( aIni[ i ] )
                        EXIT
                     ENDIF
                  ENDDO
               ELSE
                  xRes := cToken2
               ENDIF
            ENDIF
         ELSE
            ::nErr := 3   // Parameter's value is not valid (expected empty, number, ON, OFF, YES, NO)
         ENDIF
         IF ::nErr == 0 .AND. ValType( xRes ) != Left( cToken1, 1 )
            ::nErr := 4   // Parameter's value is not of the expected type
         ENDIF
      ELSE
         ::nErr := 2      // Parameter's option is not a valid one (see class vars)
      ENDIF
   ELSE
      ::nErr := 1         // Parameter's format is not valid, expected: <option> = <value>
   ENDIF
   IF ::nErr == 0
      __objSendMsg( Self, "_" + cToken1, xRes )
   ELSE
      ::nLineErr := i
      ::cLineErr := cLine
   ENDIF

   RETURN ::nErr == 0

METHOD ReadIni( cIniName ) CLASS TFormatCode

   LOCAL i, nLen, aIni, c

   IF File( cIniName )
      aIni := hb_ATokens( StrTran( MemoRead( cIniName ), Chr( 13 ) + Chr( 10 ), Chr( 10 ) ), Chr( 10 ) )
      nLen := Len( aIni )
      FOR i := 1 TO nLen
         IF ! HB_ISNULL( aIni[ i ] := AllTrim( aIni[ i ] ) ) .AND. ;
               ( c := Left( aIni[ i ], 1 ) ) != ";" .AND. c != "#"
            IF ! ::SetOption( aIni[ i ], @i, aIni )
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN ::nErr == 0

METHOD Source2Array( cSource ) CLASS TFormatCode

   LOCAL aTokens := hb_ATokens( StrTran( cSource, Chr( 13 ) + Chr( 10 ), Chr( 10 ) ), Chr( 10 ) )

   IF ::nEol < 0
      IF Chr( 13 ) + Chr( 10 ) $ cSource
         ::cEol := Chr( 13 ) + Chr( 10 )
      ELSE
         ::cEol := Chr( 10 )
      ENDIF
   ENDIF

   RETURN aTokens

METHOD Array2Source( aSource ) CLASS TFormatCode

   LOCAL nLen := Len( aSource ), i
   LOCAL cSource := ""

   FOR i := 1 TO nLen
      IF aSource[ i ] == NIL
         EXIT
      ENDIF
      IF i < nLen .OR. ! Empty( aSource[ i ] )
         cSource += RTrim( aSource[ i ] ) + ::cEol
      ENDIF
   NEXT

   DO WHILE Right( cSource, Len( ::cEol ) * 2 ) == Replicate( ::cEol, 2 )
      cSource := hb_StrShrink( cSource, Len( ::cEol ) )
   ENDDO

   RETURN cSource

METHOD File2Array( cFileName ) CLASS TFormatCode

   IF File( cFileName )
      RETURN ::Source2Array( MemoRead( cFileName ) )
   ENDIF

   RETURN NIL

METHOD Array2File( cFileName, aSource ) CLASS TFormatCode

   LOCAL cName, i, cBakName, cPath

   cName := iif( ( i := RAt( ".", cFileName ) ) == 0, cFileName, SubStr( cFileName, 1, i - 1 ) )
   IF HB_ISNULL( ::cExtSave )
      cBakName := cName + iif( Left( ::cExtBack, 1 ) == ".", "", "." ) + ::cExtBack
      IF __CopyFile( cFileName, cBakName ) == .F.
         RETURN .F.
      ENDIF
   ENDIF

   IF ! HB_ISNULL( ::cExtSave )
      cFileName := cName + iif( Left( ::cExtSave, 1 ) == ".", "", "." ) + ::cExtSave
   ENDIF
   IF ::lFCaseLow
      cPath := iif( ( i := RAt( '\', cFileName ) ) == 0, iif( ( i := RAt( "/", cFileName ) ) == 0, "", Left( cFileName, i ) ), Left( cFileName, i ) )
      cFileName := cPath + Lower( iif( i == 0, cFileName, SubStr( cFileName, i + 1 ) ) )
   ENDIF

   RETURN hb_MemoWrit( cFileName, ::Array2Source( aSource ) )

STATIC FUNCTION rf_AINS( arr, nItem, cItem )

   IF ATail( arr ) != NIL
      AAdd( arr, NIL )
   ENDIF
   hb_AIns( arr, nItem, cItem )

   RETURN Len( arr )

STATIC FUNCTION rf_ADEL( arr, nItem )

   ADel( arr, nItem )

   RETURN NIL

STATIC FUNCTION FindNotQuoted( subs, stroka, nPos2 )

   LOCAL nPos1, i, c, nState := 0, cSymb

   IF ! HB_ISNUMERIC( nPos2 )
      nPos2 := 1
   ENDIF

   DO WHILE .T.
      IF ( nPos1 := hb_At( subs, stroka, nPos2 ) ) == 0
         EXIT
      ENDIF
      FOR i := nPos2 TO nPos1 - 1
         c := SubStr( stroka, i, 1 )
         IF nState == 0
            IF c == "'" .OR. c == '"'
               cSymb := c
               nState := 1
            ELSEIF c == "["
               nState := 2
            ENDIF
         ELSEIF ( nState == 1 .AND. c == cSymb ) .OR. ( nState == 2 .AND. c == "]" )
            nState := 0
         ENDIF
      NEXT
      IF nState == 0
         EXIT
      ENDIF
      nPos2 := nPos1 + 1
   ENDDO

   RETURN nPos1

PROCEDURE AddFunctionsFromFiles( /* @ */ cFunctions, cFiles )

   LOCAL cName

   /* from built-in list */
   FileToFuncList( @cFunctions, BuiltInFunctionList() )

   /* from specified list of files */
   FOR EACH cName IN hb_ATokens( cFiles )
      FileToFuncList( @cFunctions, hb_MemoRead( hb_DirBase() + cName ) )
   NEXT

   RETURN

STATIC PROCEDURE FileToFuncList( /* @ */ cFunctions, cFile )

   LOCAL cLine

   FOR EACH cLine IN hb_ATokens( StrTran( cFile, Chr( 13 ) ), Chr( 10 ) )
      IF Left( cLine, Len( "DYNAMIC " ) ) == "DYNAMIC "
         cFunctions += "," + SubStr( cLine, Len( "DYNAMIC " ) + 1 )
      ELSEIF Left( cLine, Len( "EXTERNAL " ) ) == "EXTERNAL "
         cFunctions += "," + SubStr( cLine, Len( "EXTERNAL " ) + 1 )
      ENDIF
   NEXT

   RETURN
