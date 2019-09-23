%{
//  Yacc テンプレート for Delphi 利用サンプル
//      by Osamu TAKEUCHI <osamu@big.or.jp>
//         http://www2.big.or.jp/~osamu/
//
//  TP Yacc に付いてくる ExprLex.y を Delphi 用 .COD 向けに
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

unit ecma_y;

{$DEFINE LEX}

interface

uses
{$IFDEF LEX}
   LexLib,
{$ENDIF}
  SysUtils,classes, YaccLib,ecma_expr,ecma_type,ecma_stmt;

%}

/* トークンを設定する */
%token _VAR
%token _FUNCTION
%token _PRINT
%token _IF
%token _ELSE
%token _WHILE
%token _FOR
%token _CONTINUE
%token _BREAK
%token _RETURN
%token _WITH
%token _THIS
%token _NULL
%token _TRUE
%token _FALSE
%token _NEW
%token _DELETE
%token _VOID
%token _TYPEOF
%token _TRY
%token _CATCH
%token _FINALLY
%token _THROW
%token _UNDEFINED
%token _INFINITY
%token _IN
%token _CASE
%token _CLASS
%token _CONST
%token _DEBUGGER
%token _DO
%token _ENUM
%token _EXPORT
%token _EXTENDS
%token _IMPORT
%token _SUPER
%token _SWITCH

%token <PChar> QUOTE_STRING
%token <PChar> VARIABLE
%token <Integer> NUMBER
%token <Double> FLOAT_NUMBER
%token LINE_TERMINATOR

%token LP
%token RP
%token LB
%token RB
%token LSQ
%token RSQ
%token SC
%token COMMA
%token LF

%token <Char> ADDOP SHIFTOP MULOP COMPOP EQOP ASSIGNOP
%token <Char> INCDECOP UNOP

%left  COMMA
%right OP_ASSIGN ASSIGNOP
%right QUERY COLON
%left  OP_LOGICAL_OR
%left  OP_LOGICAL_AND
%left  OP_BIT_OR
%left  OP_BIT_XOR
%left  OP_BIT_AND
%left  EQOP
%left  COMPOP
%left  SHIFTOP
%left  ADDOP
%left  MULOP
%right UNOP
%nonassoc INCDECOP
%left  DOT


%type <PJStatement> program
%type <PJStatement> source_elements
%type <PJStatement> source_element
%type <PJStatement> statement
%type <PJStatement> function_declaration
%type <PJStatement> parameter_declaration
%type <PJStatement> statement_list
%type <PJStatement> block
%type <PJStatement> empty_statement
%type <PJStatement> expression_statement
%type <PJStatement> if_statement
%type <PJStatement> while_statement
%type <PJStatement> for_statement
%type <PJStatement> forin_statement
%type <PJStatement> continue_statement
%type <PJStatement> break_statement
%type <PJStatement> return_statement
%type <PJStatement> with_statement
%type <PJStatement> print_statement
%type <PJStatement> throw_statement
%type <PJStatement> finally_statement
%type <PJStatement> catch_statement
%type <PJStatement> try_statement
%type <PJStatement> var_statement


%type <PJExpr> variable
%type <PJExpr> primary_expression
%type <PJExpr> this
%type <PJExpr> number
%type <PJExpr> string
%type <PJExpr> null
%type <PJExpr> true
%type <PJExpr> false
%type <PJExpr> member_expression
%type <PJExpr> new_expression
%type <PJExpr> call_expression
%type <PJExpr> arguments
%type <PJExpr> assignment_expression
%type <PJExpr> expression
%type <PJExpr> option_expression
%type <PJExpr> new_expression
%type <PJExpr> constant


/* BNF開始 */
%%

/* 外部変数をここで設定する */
%{
  private
    FRoot: PJStatement;
    FExpr: TJExprFactory;
    FStmt: TJStatementFactory;

  protected
    pyylval: PYYSType;
{$IFDEF LEX}
    lex : TLexBase;
{$ELSE}
    lex: TJLex;
{$ENDIF}
    // エラー処理を override
    procedure yyerror ( msg : String ); override;
  public
{$IFDEF LEX}
    constructor Create(ALex: TLexBase);
{$ELSE}
    constructor Create(ALex: TJLex);
{$ENDIF}
    destructor Destroy; override;
    procedure Clear;

    property Root: PJStatement read FRoot;

%}

program                 : source_elements       { FRoot := FStmt.MoveStart($1); }
                        ;

source_elements         : source_elements source_element
                                                { $$ := $2;
                                                  FStmt.MergeStatement($1,$2); }
                        | source_element        { $$ := $1; }
                        ;

source_element          : statement             { $$ := $1; }
                        | function_declaration  { $$ := $1; }
                        ;

statement               : block                 { $$ := $1; }
                        | empty_statement       { $$ := $1; }
                        | expression_statement  { $$ := $1; }
                        | if_statement          { $$ := $1; }
                        | while_statement       { $$ := $1; }
                        | for_statement         { $$ := $1; }
                        | forin_statement       { $$ := $1; }
                        | continue_statement    { $$ := $1; }
                        | break_statement       { $$ := $1; }
                        | return_statement      { $$ := $1; }
                        | print_statement       { $$ := $1; }
                        | try_statement         { $$ := $1; }
                        | catch_statement       { $$ := $1; }
                        | finally_statement     { $$ := $1; }
                        | throw_statement       { $$ := $1; }
                        | with_statement        { $$ := $1; }
                        | var_statement         { $$ := $1; }
                        ;


function_declaration    : _FUNCTION variable LP parameter_declaration rp block
                                                { $$ := FStmt.MakeFunctionDecl($2,$4,$6);}
                        ;

parameter_declaration   : parameter_declaration COMMA variable
                                                { $$ := FStmt.MakeParamDecl($1,$3); }
                        | variable              { $$ := FStmt.MakeParamDecl(nil,$1); }
                        |                       { $$ := FStmt.MakeParamDecl(nil,nil); }
                        ;

statement_list          : statement_list statement
                                                { $$ := $2;
                                                  FStmt.MergeStatement($1,$2); }
                        | statement             { $$ := $1; }
                        ;

block                   : LB statement_list rb  { $$ := FStmt.MakeBlockStatement($2); }
                        | LB rb                 { $$ := FStmt.MakeBlockStatement(nil); }
                        ;

empty_statement         : sc                    { $$ := FStmt.MakeEmptyStatement; }
                        ;

expression_statement    : expression sc         { $$ := FStmt.MakeExprStatement($1); }
                        ;

print_statement         : _PRINT expression sc  { $$ := FStmt.MakePrintStatement($2); }
                        | _PRINT LP expression rp sc
                                                { $$ := FStmt.MakePrintStatement($3); }
                        ;

if_statement            : _IF LP expression rp statement
                                                { $$ := FStmt.MakeIfStatement($3,$5) ;}
                        | _IF LP expression rp statement _ELSE statement
                                                { $$ := FStmt.MakeIfStatement($3,$5,$7) ;}
                        ;

while_statement         : _WHILE LP expression rp statement
                                                { $$ := FStmt.MakeWhileStatement($3,$5); }
                        ;

for_statement           : _FOR LP option_expression sc option_expression sc option_expression rp statement
                                                { $$ := FStmt.MakeForStatement($3,$5,$7,$9); }
                        ;

forin_statement         : _FOR LP variable _IN variable rp statement
                                                { $$ := FStmt.MakeForInStatement($3,$5,$7); }
                        ;

continue_statement      : _CONTINUE sc          { $$ := FStmt.MakeContinueStatement; }
                        ;

break_statement         : _BREAK sc             { $$ := FStmt.MakeBreakStatement; }
                        ;

return_statement        : _RETURN expression sc { $$ := FStmt.MakeReturnStatement($2); }
                        | _RETURN sc            { $$ := FStmt.MakeReturnStatement(nil); }
                        ;

with_statement          : _WITH LP expression rp statement
                                                { $$ := FStmt.MakeWithStatement($3,$5);}
                        ;

try_statement           : _TRY statement        { $$ := FStmt.MakeTryStatement($2); }
                        ;

catch_statement         : _CATCH LP expression rp statement
                                                { $$ := FStmt.MakeCatchStatement($3,$5); }
                        | _CATCH statement      { $$ := FStmt.MakeCatchStatement(nil,$2); }
                        ;

finally_statement       : _FINALLY statement    { $$ := FStmt.MakeFinallyStatement($2); }
                        ;

throw_statement         : _THROW LP expression rp sc
                                                { $$ := FStmt.MakeThrowStatement($3); }
                        | _THROW expression sc  { $$ := FStmt.MakeThrowStatement($2); }
                        | _THROW LP rp sc       { $$ := FStmt.MakeThrowStatement(nil);}
                        | _THROW sc             { $$ := FStmt.MakeThrowStatement(nil);}
                        ;

var_statement           : _VAR expression       { $$ := FStmt.MakeVarStatement($2); }
                        ;

option_expression       : expression            { $$ := $1; }
                        |                       { $$ := nil; }
                        ;

new_expression          : _NEW variable         { $$ := FExpr.MakeExpr2(opNew,$2,nil); }
                        | _NEW variable LP arguments rp
                                                { $$ := FExpr.MakeExpr2(opNew,$2,$4); }
                        | _NEW variable LP rp
                                                { $$ := FExpr.MakeExpr2(opNew,$2,nil); }
                        ;

assignment_expression   : primary_expression    { $$ := $1; }
                        | new_expression        { $$ := $1; }
                        | ADDOP assignment_expression %prec UNOP
                                                { case $1 of
                                                    '+': $$ := FExpr.MakeExpr1(opPlus,$2);
                                                    '-': $$ := FExpr.MakeExpr1(opMinus,$2);
                                                  end; }
                        | UNOP assignment_expression
                                                { case $1 of
                                                    '!': $$ := FExpr.MakeExpr1(opLogicalNot,$2);
                                                    '~': $$ := FExpr.MakeExpr1(opBitNot,$2);
                                                  end; }
                        | INCDECOP assignment_expression
                                                { case $1 of
                                                    '+': $$ := FExpr.MakeExpr1(opPreInc,$2);
                                                    '-': $$ := FExpr.MakeExpr1(opPreDec,$2);
                                                  end; }
                        | assignment_expression INCDECOP
                                                { case $2 of
                                                    '+': $$ := FExpr.MakeExpr1(opPostInc,$1);
                                                    '-': $$ := FExpr.MakeExpr1(opPostDec,$1);
                                                  end; }
                        | assignment_expression MULOP assignment_expression
                                                { case $2 of
                                                    '*': $$ := FExpr.MakeExpr2(opMul,$1,$3);
                                                    '/': $$ := FExpr.MakeExpr2(opDiv,$1,$3);
                                                    '%': $$ := FExpr.MakeExpr2(opMod,$1,$3);
                                                    'd': $$ := FExpr.MakeExpr2(opDivInt,$1,$3);
                                                  end; }
                        | assignment_expression ADDOP assignment_expression
                                                { case $2 of
                                                    '+': $$ := FExpr.MakeExpr2(opAdd,$1,$3);
                                                    '-': $$ := FExpr.MakeExpr2(opSub,$1,$3);
                                                  end; }
                        | assignment_expression SHIFTOP assignment_expression
                                                { case $2 of
                                                    '<': $$ := FExpr.MakeExpr2(opBitLeft,$1,$3);
                                                    '>': $$ := FExpr.MakeExpr2(opBitRight,$1,$3);
                                                    '3': $$ := FExpr.MakeExpr2(opBitRightZero,$1,$3);
                                                  end; }
                        | assignment_expression COMPOP assignment_expression
                                                { case $2 of
                                                    '(': $$ := FExpr.MakeExpr2(opLSEQ,$1,$3);
                                                    '<': $$ := FExpr.MakeExpr2(opLS,$1,$3);
                                                    ')': $$ := FExpr.MakeExpr2(opGTEQ,$1,$3);
                                                    '>': $$ := FExpr.MakeExpr2(opGT,$1,$3);
                                                  end; }
                        | assignment_expression EQOP assignment_expression
                                                { case $2 of
                                                    '=': $$ := FExpr.MakeExpr2(opEQ,$1,$3);
                                                    '!': $$ := FExpr.MakeExpr2(opNE,$1,$3);
                                                    '3': $$ := FExpr.MakeExpr2(opEQEQEQ,$1,$3);
                                                    '2': $$ := FExpr.MakeExpr2(opNEEQEQ,$1,$3);
                                                  end; }
                        | assignment_expression OP_BIT_AND assignment_expression
                                                { $$ := FExpr.MakeExpr2(opBitAnd,$1,$3); }
                        | assignment_expression OP_BIT_XOR assignment_expression
                                                { $$ := FExpr.MakeExpr2(opBitXor,$1,$3); }
                        | assignment_expression OP_BIT_OR assignment_expression
                                                { $$ := FExpr.MakeExpr2(opBitOr,$1,$3); }
                        | assignment_expression OP_LOGICAL_AND assignment_expression
                                                { $$ := FExpr.MakeExpr2(opLogicalAnd,$1,$3); }
                        | assignment_expression OP_LOGICAL_OR assignment_expression
                                                { $$ := FExpr.MakeExpr2(opLogicalOr,$1,$3); }
                        | assignment_expression QUERY assignment_expression COLON assignment_expression
                                                { $$ := FExpr.MakeExpr3(opConditional,$1,$3,$5); }
                        | assignment_expression ASSIGNOP assignment_expression
                                                { case $2 of
                                                    '+': $$ := FExpr.MakeExpr2(opAddAssign,$1,$3);
                                                    '-': $$ := FExpr.MakeExpr2(opSubAssign,$1,$3);
                                                    '*': $$ := FExpr.MakeExpr2(opMulAssign,$1,$3);
                                                    '/': $$ := FExpr.MakeExpr2(opDivAssign,$1,$3);
                                                    '%': $$ := FExpr.MakeExpr2(opModAssign,$1,$3);
                                                    '<': $$ := FExpr.MakeExpr2(opBitLeftAssign,$1,$3);
                                                    '>': $$ := FExpr.MakeExpr2(opBitRightAssign,$1,$3);
                                                    '3': $$ := FExpr.MakeExpr2(opBitRightZeroAssign,$1,$3);
                                                    '&': $$ := FExpr.MakeExpr2(opBitAndAssign,$1,$3);
                                                    '|': $$ := FExpr.MakeExpr2(opBitOrAssign,$1,$3);
                                                  end; }
                        | assignment_expression OP_ASSIGN assignment_expression
                                                { $$ := FExpr.MakeExpr2(opAssign,$1,$3); }
                        ;

expression              : assignment_expression { $$ := $1; }
                        | expression COMMA assignment_expression
                                                { $$ := FExpr.MakeExpr2(opExpr,$1,$3); }
                        ;

primary_expression      : this                  { $$ := $1; }
                        | variable              { $$ := $1; }
                        | constant              { $$ := $1; }
                        | LP expression rp      { $$ := $2; }
                        | member_expression     { $$ := $1; } 
                        | call_expression       { $$ := $1; }
                        ;

call_expression         : primary_expression LP arguments rp
                                                { $$ := FExpr.MakeExpr2(opCall,$1,$3); }
                        | primary_expression LP rp
                                                { $$ := FExpr.MakeExpr1(opCall,$1); }
                        ;

arguments               : arguments COMMA assignment_expression
                                                { $$ := FExpr.MakeArguments($1,$3); }
                        | assignment_expression
                                                { $$ := FExpr.MakeArguments(nil,$1); }
                        ;

member_expression       : primary_expression DOT variable
                                                { $$ := FExpr.MakeExpr2(opMember,$1,$3); }
                        | primary_expression LSQ assignment_expression RSQ
                                                { $$ := FExpr.MakeExpr2(opArray,$1,$3); }
                /*        | primary_expression DOT variable LP arguments rp
                                                { $$ := FExpr.MakeExpr3(opMemberCall,$1,$3,$4); } */
                        ;


this                    : _THIS                 { $$ := FExpr.MakeThis; }
                        ;

variable                : VARIABLE              { $$ := FExpr.MakeVariable(String($1)); }
                        ;

constant                : number                { $$ := $1; }
                        | string                { $$ := $1; }
                        | null                  { $$ := $1; }
                        | true                  { $$ := $1; }
                        | false                 { $$ := $1; }
                        ;

number                  : NUMBER                { $$ := FExpr.MakeNumberInt($1);}
                        | FLOAT_NUMBER          { $$ := FExpr.MakeNumberFloat($1);}
                        ;

string                  : QUOTE_STRING          { $$ := FExpr.MakeString(String($1)); }
                        ;

null                    : _NULL                 { $$ := FExpr.MakeNull; }
                        ;

true                    : _TRUE                 { $$ := FExpr.MakeBoolean(True); }
                        ;

false                   : _FALSE                { $$ := FExpr.MakeBoolean(False); }
                        ;

rp                      : RP                    { yyerrok; }
                        ;

rb                      : RB                    { yyerrok; }
                        ;

sc                      : SC                    { yyerrok; }
                        ;


%%

{$IFDEF LEX}
constructor TYacc.Create(ALex: TLexBase);
begin
  lex:= ALex;
  //型変換して入れる
  pyylval := PYYSType(ALex.pyylval);
  FRoot := nil;
  FExpr := TJExprFactory.Create;
  FStmt := TJStatementFactory.Create(FExpr);
end;
{$ELSE}
constructor TYacc.Create(ALex: TJLex);
begin
  lex:= ALex;
  //型変換して入れる
  pyylval := PYYSType(ALex.pyylval);
  FRoot := nil;
  FExpr := TJExprFactory.Create;
  FStmt := TJStatementFactory.Create(FExpr);
end;

{$ENDIF}

destructor TYacc.Destroy;
begin
  Clear;
  FExpr.Free;
  FStmt.Free;
  inherited;
end;

procedure TYacc.Clear;
var
  i: Integer;
begin
  //InitProgram(FRoot);
  FExpr.Clear;
  FStmt.Clear;
end;

// エラー処理を override
procedure TYacc.yyerror ( msg : String );
var
  i: Integer;
  s: string;
begin
  s:= lex.yytext;
  for i:= Length(s) downto 1 do
    if s[i] < ' ' then
      s := Copy(s,1,i-1)+'#'+IntToHex(Ord(s[i]),2)+Copy(s,i+1,Length(s));

  lex.yywriteln(msg+
        ' around '''+s+''''+
        ' at L'+IntToStr(lex.yylineno)+
           ':C'+IntToStr(lex.yycolno));

  //初期化する
  //InitProgram(FRoot);
  Clear;
end;


end.
