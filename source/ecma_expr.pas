unit ecma_expr;

//式の解析木関係
//2001/04/10 ~
//by Wolfy

interface

uses
  windows,sysutils,classes,ecma_type;

type
  TJExprFactory = class(TObject)
  private
    FList: TList;

    procedure FreeExpr(P: PJExpr);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    //新しいnodeを作成
    function NewExpr: PJExpr;
    //1次式を作成する
    function MakeExpr1(Code: TJOPCode;Left: PJExpr): PJExpr;
    //2次式を作成する
    function MakeExpr2(Code: TJOPCode;Left,Right: PJExpr): PJExpr;
    //3次式を作成する
    function MakeExpr3(Code: TJOPCode;Left,Right,Third: PJExpr): PJExpr;
    //定数式を作成
    function MakeConstant(Value: TJValue): PJExpr;
    //変数式を作成
    function MakeVariable(Symbol: String): PJExpr;
    //定数の整数を作成
    function MakeNumberInt(Value: Integer): PJExpr;
    function MakeNumberFloat(Value: Double): PJExpr;
    function MakeNull: PJExpr;
    function MakeNaN: PJExpr;
    function MakeBoolean(Value: Boolean): PJExpr;
    function MakeString(S: String): PJExpr;
    function MakeUndefined: PJExpr;
    function MakeInfinity(Negative: Boolean): PJExpr;
    function MakeThis: PJExpr;
    function MakeSuper(Expr: PJExpr): PJExpr;
    function MakeArguments(Prev,Next: PJExpr): PJExpr;
    function MakeObjectElement(Name,Value: PJExpr): PJExpr;
    function MakeRegExp(RE: String): PJExpr;
    function MakeFunction(FuncDecl: PJStatement): PJExpr;
  end;


implementation


constructor TJExprFactory.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TJExprFactory.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TJExprFactory.FreeExpr(P: PJExpr);
begin
  //定数を解放
  if Assigned(P^.Value) then
  begin
    system.Dispose(P^.Value);
  end;

  system.Dispose(P);
end;

procedure TJExprFactory.Clear;
//クリア
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    FreeExpr(FList[i]);
    //FList.Delete(i);
  end;
  FList.Clear;
end;

function TJExprFactory.NewExpr: PJExpr;
//新しい式を作成する
begin
{ TODO : 初期化を忘れずに }
  New(Result);
  //初期化する
  Result^.Code := opNone;
  Result^.Left := nil;
  Result^.Third := nil;
  Result^.Right := nil;
  Result^.Value := nil;
  Result^.Symbol := '';
  Result^.Statement := nil;
  FList.Add(Result);
end;


function TJExprFactory.MakeExpr1(Code: TJOPCode; Left: PJExpr): PJExpr;
//1次式を作成する
begin
  //定数の折り畳み
  if IsConstant(Left) and (Code in [opMinus,opPlus,opBitNot]) then
  begin
    Left^.Value^ := CalcValue1(Code,Left^.Value^);
    Result := Left;
  end
  else begin
    //新規作成する
    Result := NewExpr;
    Result^.Code := Code;
    Result^.Left := Left;
    Result^.Right := nil;
  end;
end;

function TJExprFactory.MakeExpr2(Code: TJOPCode; Left,Right: PJExpr): PJExpr;
//2次式を作成する
var
  v: TJValue;
begin
  //定数の折り畳み
  if IsConstant(Left) and IsConstant(Right) then
  begin
    case Code of
      opAdd,opSub,opMul,opDiv,opMod,opDivInt,opBitAnd,opBitOr,opBitXor,
      opBitLeft,opBitRight,opBitRightZero:
      begin
        //objectが入ってる可能性があるため両方消す
        v := CalcValue2(Code,Left^.Value^,Right^.Value^);
        FList.Remove(Left);
        FList.Remove(Right);
        FreeExpr(Left);
        FreeExpr(Right);
        Result := MakeConstant(v);
      end;
      opLS,opGT,opLSEQ,opGTEQ,opEQ,opNE,opEQEQEQ,opNEEQEQ,
      opLogicalOr,opLogicalOr2,opLogicalAnd,opLogicalAnd2:
      begin
        v := CompareValue(Code,Left^.Value^,Right^.Value^);
        FList.Remove(Left);
        FList.Remove(Right);
        FreeExpr(Left);
        FreeExpr(Right);
        Result := MakeConstant(v);
      end;
      else
        //新規作成する
        Result := NewExpr;
        Result^.Code := Code;
        Result^.Left := Left;
        Result^.Right := Right;
    end;
  end
  else begin
    //新規作成する
    Result := NewExpr;
    Result^.Code := Code;
    Result^.Left := Left;
    Result^.Right := Right;
  end;
end;


function TJExprFactory.MakeExpr3(Code: TJOPCode;Left,Right,Third: PJExpr): PJExpr;
//3次式を作成する
begin
  //新規作成する
  Result := NewExpr;
  Result^.Code := Code;
  Result^.Left := Left;
  Result^.Right := Right;
  Result^.Third := Third;
end;

function TJExprFactory.MakeConstant(Value: TJValue): PJExpr;
//定数式を作成
begin
  Result := NewExpr;
  Result^.Code := opConstant;
  //定数を新規作成
  New(Result^.Value);
  Result^.Value^ := Value;
end;

function TJExprFactory.MakeVariable(Symbol: String): PJExpr;
//変数作成
begin
  Result := NewExpr;
  Result^.Code := opVariable;
  Result^.Symbol := Symbol;
end;

function TJExprFactory.MakeNumberInt(Value: Integer): PJExpr;
//定数の整数を作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtInteger;
  v.vInteger := Value;
  Result := MakeConstant(v);
end;

function TJExprFactory.MakeNumberFloat(Value: Double): PJExpr;
//定数の浮動小数点を作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtDouble;
  v.vDouble := Value;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeNull: PJExpr;
//nullを作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtNull;
  v.vNull := nil;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeBoolean(Value: Boolean): PJExpr;
//boolを作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtBool;
  v.vBool := Value;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeString(S: String): PJExpr;
//文字列を作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtString;
  v.vString := S;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeUndefined: PJExpr;
//未定義を作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtUndefined;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeInfinity(Negative: Boolean): PJExpr;
//無限大を作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtInfinity;
  v.vBool := Negative;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeThis: PJExpr;
//thisを作成
begin
  Result := NewExpr;
  Result^.Code := opThis;
end;

function TJExprFactory.MakeRegExp(RE: String): PJExpr;
//正規表現を作成(仮)
var
  p: Integer;
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtRegExp;
  p := Pos(#0, RE); //#0以降にフラグが入っている
  if p > 0 then
  begin
    v.vString := Copy(RE, 1, p - 1);
    v.vRegExpOptions := Copy(RE, p + 1, MaxInt);
  end
  else
    v.vString := RE;

  Result := MakeConstant(v);
end;   

function TJExprFactory.MakeArguments(Prev,Next: PJExpr): PJExpr;
//引数を作成
begin
  Result := NewExpr;
  Result^.Code := opArg;
  Result^.Left := Prev;
  Result^.Right := Next;
end;

function TJExprFactory.MakeNaN: PJExpr;
//NaNを作成
var
  v: TJValue;
begin
  EmptyValue(v);
  v.ValueType := vtNaN;
  v.vNull := nil;
  Result := MakeConstant(v)
end;

function TJExprFactory.MakeObjectElement(Name, Value: PJExpr): PJExpr;
begin
  Result := NewExpr;
  Result^.Code := opObjectElement;
  Result^.Left := Name;
  Result^.Right := Value;
end;

function TJExprFactory.MakeSuper(Expr: PJExpr): PJExpr;
//superを作成
begin
  Result := NewExpr;
  Result^.Code := opSuper;
  Result^.Right := Expr;
end;

function TJExprFactory.MakeFunction(FuncDecl: PJStatement): PJExpr;
//function定数を作成
begin
  Result := NewExpr;
  Result^.Code := opFunction;
  Result^.Statement := FuncDecl;
end;

end.
