%{
//  Lex テンプレート for Delphi 利用サンプル
//      by Osamu TAKEUCHI <osamu@big.or.jp>
//         http://www2.big.or.jp/~osamu/
//
//  TP Lex に付いてくる ExprLex.l を Delphi 用 .COD 向けに
//  書き直してみました。Expr.dpr から uses されます。
//
//  変数を使用することのできる数式処理ルーチンになります。
//
//  利用方法：
//      コマンドプロンプトから
//      lex ExprLex.L       // lex を使って ExprYacc.PAS を生成します
//      yacc ExprYacc.L     // yacc を使って ExprYacc.PAS を生成します
//      dcc32 Expr.dpr      // Delphi を使って Expr.dpr を構築します
//

unit ecma_l;

interface

uses SysUtils, LexLib_Stream,ecma_y;

%}

cpp_style_comment	"//".*
delimiter       	[ \t\f\r]
whitespace      	{delimiter}+
letter          	[a-z]|[A-Z]
line_terminator		[\n]

digit			[0-9]
hex_digit		[a-f]|[A-F]|{digit}
octal_digit		[0-7]
decimal_number		(({digit}+)|({digit}*\.{digit}+)([eE][-+]?{digit}+)?)
hex_number		("0x"|"0X"){hex_digit}+
octal_number		"0"{octal_digit}+
int_number              {digit}+
number			{int_number}|{hex_number}|{octal_number}

single_escape_char	['"\\bfnrt]
escape_character	[xu]|{single_escape_character}|{octal_digit}
non_escape_char		[^\n\rxu'"\\bfnrt0-7]
character_es		\\({single_escape_char}|{non_escape_char})
escape_sequence		{character_es}
ds_chars		[^\\"\n\r]
ss_chars		[^\\'\n\r]
double_string		\"({ds_chars}|{escape_sequence})*\"
single_string		\'({ss_chars}|{escape_sequence})*\'
string			{double_string}|{single_string}

variable_symbol		[_$]
variable        	({letter}|{variable_symbol})({letter}|{variable_symbol}|{digit})*


%start INITIAL
%start COMMENT1
%start COMMENT2
%start QUOTE_STRING1
%start QUOTE_STRING2

%%
  // ここに TLex のメンバとして .L で宣言される変数・関数を追加する

<INITIAL>"//"
  begin
    //コメント
    start(COMMENT1);
  end;

<COMMENT1>"\n"
  begin
    //コメント終り
    start(INITIAL);
  end;

<INITIAL>"/*"
  begin
    //コメント
    start(COMMENT2);
  end;

<COMMENT2>"*/"
  begin
    start(INITIAL);
  end;

<INITIAL>"?"
  begin
    return(QUERY);
  end;

<INITIAL>":"
  begin
    return(COLON);
  end;

<INITIAL>"!="
  begin
    yylval.yyChar := '!';
    return(EQOP);
  end;

<INITIAL>">="
  begin
    yylval.yyChar := ')';
    return(COMPOP);
  end;

<INITIAL>"<="
  begin
    yylval.yyChar := '(';
    return(COMPOP);
  end;

<INITIAL>"==="
  begin
    yylval.yyChar := '3';
    return(EQOP);
  end;

<INITIAL>"!=="
  begin
    yylval.yyChar := '2';
    return(EQOP);
  end;

<INITIAL>"=="
  begin
    yylval.yyChar := '=';
    return(EQOP);
  end;

<INITIAL>">"
  begin
    yylval.yyChar := '>';
    return(COMPOP);
  end;

<INITIAL>"<"
  begin
    yylval.yyChar := '<';
    return(COMPOP);
  end;

<INITIAL>"="
  begin
    return(OP_ASSIGN);
  end;

<INITIAL>"&&"
  begin
    return(OP_LOGICAL_AND);
  end;

<INITIAL>"||"
  begin
    return(OP_LOGICAL_OR);
  end;

<INITIAL>"!"
  begin
    yylval.yyChar := '!';
    return(UNOP);
  end;

<INITIAL>"&"
  begin
    return(OP_BIT_AND);
  end;

<INITIAL>"|"
  begin                
    return(OP_BIT_OR);
  end;

<INITIAL>">>>"
  begin
    yylval.yyChar := '3';
    return(SHIFTOP);
  end;

<INITIAL>"<<"
  begin
    yylval.yyChar := '<';
    return(SHIFTOP);
  end;

<INITIAL>">>"
  begin
    yylval.yyChar := '>';
    return(SHIFTOP);
  end;

<INITIAL>"~"
  begin
    yylval.yyChar := '~';
    return(UNOP);
  end;

<INITIAL>"--"
  begin
    yylval.yyChar := '-';
    return(INCDECOP);
  end;

<INITIAL>"++"
  begin
    yylval.yyChar := '+';
    return(INCDECOP);
  end;

<INITIAL>"+="
  begin
    yylval.yyChar := '+';
    return(ASSIGNOP);
  end;

<INITIAL>"-="
  begin
    yylval.yyChar := '-';
    return(ASSIGNOP);
  end;

<INITIAL>"*="
  begin
    yylval.yyChar := '*';
    return(ASSIGNOP);
  end;

<INITIAL>"/="
  begin
    yylval.yyChar := '/';
    return(ASSIGNOP);
  end;

<INITIAL>"%="
  begin
    yylval.yyChar := '%';
    return(ASSIGNOP);
  end;

<INITIAL>"&="
  begin
    yylval.yyChar := '&';
    return(ASSIGNOP);
  end;

<INITIAL>"|="
  begin
    yylval.yyChar := '|';
    return(ASSIGNOP);
  end;

<INITIAL>"^"
  begin
    return(OP_BIT_XOR);
  end;

<INITIAL>"^="
  begin
    yylval.yyChar := '^';
    return(ASSIGNOP);
  end;

<INITIAL>"<<="
  begin
    yylval.yyChar := '<';
    return(ASSIGNOP);
  end;

<INITIAL>">>="
  begin
    yylval.yyChar := '>';
    return(ASSIGNOP);
  end;

<INITIAL>">>>="
  begin
    yylval.yyChar := '3';
    return(ASSIGNOP);
  end;

<INITIAL>";"
  begin
    return(SC);
  end;

<INITIAL>"."
  begin
    return(DOT);
  end;

<INITIAL>","
  begin
    return(COMMA);
  end;

<INITIAL>"{"
  begin
    return(LB);
  end;

<INITIAL>"}"
  begin
    return(RB);
  end;

<INITIAL>"("
  begin
    return(LP);
  end;

<INITIAL>")"
  begin
    return(RP);
  end;

<INITIAL>"["
  begin
    return(LSQ);
  end;

<INITIAL>"]"
  begin
    return(RSQ);
  end;

<INITIAL>"+"
  begin
    yylval.yyChar := '+';
    return(ADDOP);
  end;

<INITIAL>"-"
  begin
    yylval.yyChar := '-';
    return(ADDOP);
  end;

<INITIAL>"/"
  begin
    yylval.yyChar := '/';
    return(MULOP);
  end;

<INITIAL>"%"
  begin
    yylval.yyChar := '%';
    return(MULOP);
  end;

<INITIAL>"*"
  begin
    yylval.yyChar := '*';
    return(MULOP);
  end;

<INITIAL>false
  begin
    return(_FALSE);
  end;

<INITIAL>true
  begin
    return(_TRUE);
  end;

<INITIAL>null
  begin
    return(_NULL);
  end;

<INITIAL>undefined
  begin
    return(_UNDEFINED);
  end;

<INITIAL>infinity
  begin
    return(_INFINITY);
  end;

<INITIAL>break
  begin
    return(_BREAK);
  end;

<INITIAL>continue
  begin
    return(_CONTINUE);
  end;

<INITIAL>delete
  begin
    return(_DELETE);
  end;

<INITIAL>else
  begin
    return(_ELSE);
  end;

<INITIAL>for
  begin
    return(_FOR);
  end;

<INITIAL>function
  begin
    return(_FUNCTION);
  end;

<INITIAL>if
  begin
    return(_IF);
  end;

<INITIAL>in
  begin
    return(_IN);
  end;

<INITIAL>new
  begin
    return(_NEW);
  end;

<INITIAL>return
  begin
    return(_RETURN);
  end;

<INITIAL>this
  begin
    return(_THIS);
  end;

<INITIAL>typeof
  begin
    return(_TYPEOF);
  end;

<INITIAL>var
  begin
    return(_VAR);
  end;

<INITIAL>void
  begin
    return(_VOID);
  end;

<INITIAL>while
  begin
    return(_WHILE);
  end;

<INITIAL>with
  begin
    return(_WITH);
  end;

<INITIAL>case
  begin
    return(_CASE);
  end;

<INITIAL>class
  begin
    return(_CLASS);
  end;

<INITIAL>do
  begin
    return(_DO);
  end;

<INITIAL>switch
  begin
    return(_SWITCH);
  end;

<INITIAL>print
  begin
    return(_PRINT);
  end;

<INITIAL>try
  begin
    return(_TRY);
  end;

<INITIAL>catch
  begin
    return(_CATCH);
  end;

<INITIAL>finally
  begin
    return(_FINALLY);
  end;

<INITIAL>throw
  begin
    return(_THROW);
  end;

<INITIAL>div
  begin
    yylval.yyChar := 'd';
    return(MULOP);
  end;


<INITIAL>{line_terminator}
  begin
    //return(LINE_TERMINATOR);
  end;

<INITIAL>{variable}
  begin
    yylval.yyPChar := PChar(yytext);
    return(VARIABLE);
  end;

<INITIAL>{string}
  begin
    yylval.yyPChar := PChar(yytext);
    return(QUOTE_STRING);
  end;

<INITIAL>{number}
  begin
    yylval.yyInteger := StrToIntDef(yytext,0);
    return(NUMBER);
  end;

<INITIAL>{decimal_number}
  begin
    try
      yylval.yyDouble := StrToFloat(yytext);
    except
      on EConvertError do
        yylval.yyDouble := 0;
    end;
    return(FLOAT_NUMBER);
  end;

<INITIAL>.
  begin
  end;

%%
    // .L で宣言した TLex メンバの実装部分をここに追加する

end.
