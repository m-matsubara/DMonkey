unit ecma_dynacall;

interface

uses
  Windows,Sysutils,Classes,ecma_type,dynamiccall,hashtable,ecma_object;

type
  TStructMemberType = (smtCharArray,
                       smtChar,
                       smtShort,smtLong,smtInt64,smtBool,
                       smtString,smtWideString,
                       smtFloat,smtDouble,
                       smtIDispatch,smtIUnknown);

  TStructMember = record
    mName: string;
    mType: TStructMemberType;
    mSize: Integer;
    mPos: Integer;
    _string: string;
    _widestring: WideString;
    //_idispatch: IDispatch;
    //_iunknown: IUnknown;
  end;

  TStructMemberArray = array of TStructMember;

  //値の一時退避用
  TTempValue = record
    case Integer of
      0: (_char: Char);
      1: (_short: SmallInt);
      2: (_long: LongInt);
      3: (_pointer: Pointer);
      4: (_float: Single);
      5: (_double: Double);
      6: (_int64: Int64);
  end;


  //dynacall
  TJDynaCall = class(TJObject)
  private
    FModules: TIntegerHashtable;
    procedure HashtableOnFreeItem(Sender: TObject; P: PHashItem);
    function SendMessageImpl(const Param: TJValueList; Post: Boolean = False): Integer;
    function DoSendMessage(Param: TJValueList): TJValue;
    function DoPostMessage(Param: TJValueList): TJValue;
    function DoMoveMemory(Param: TJValueList): TJValue;
    function DoFillMemory(Param: TJValueList): TJValue;
  protected
    function DoRegister(Param: TJValueList): TJValue;
    function AsPointer(P: PJValue; AllowString: Boolean = False): Pointer;
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil;
      RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  end;


  TJStruct = class(TJObject)
  private
    FStruct: string;
    FMembers: TStructMemberArray;
    function DoClear(Param: TJValueList): TJValue;
    function DoDefine(Param: TJValueList): TJValue;
    function DoSizeOf(Param: TJValueList): TJValue;
    function DoToString(Param: TJValueList): TJValue;
  protected
    function GetMemberIndex(const S: string; ArrayStyle: Boolean): Integer;
    function GetMemberValue(Index: Integer): TJValue;
    procedure SetMemberValue(Index: Integer; Value: TJValue);
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil;
      RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetValue(S: String; ArrayStyle: Boolean;
      Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean;
      Param: TJValueList = nil); override;
    function ToString(Value: PJValue = nil): string; override;
    procedure Clear; override;

    procedure Define(const Param: TJValueList);

    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
    class function IsArray: Boolean; override;
    class function IsMakeGlobalInstance: Boolean; override;
  published
    property length: Integer read GetCount;
  end;


function IsStructObject(P: PJValue): Boolean;

procedure RegisterDMS(Engine: TJBaseEngine);


implementation

procedure RegisterDMS(Engine: TJBaseEngine);
begin
  Engine.ImportObject('DynaCall',TJDynaCall);
  Engine.ImportObject('Struct',TJStruct);
end;

function IsStructObject(P: PJValue): Boolean;
begin
  Result := IsObject(P) and (P^.vObject is TJStruct);
end;


{ TJDynaCall }

constructor TJDynaCall.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  FModules := TIntegerHashtable.Create(13,True);
  FModules.OnFreeItem := HashtableOnFreeItem;

  RegistName('DynaCall');
end;

destructor TJDynaCall.Destroy;
begin
  FModules.Clear;
  FreeAndNil(FModules);
  inherited;
end;

function TJDynaCall.DoRegister(Param: TJValueList): TJValue;
//関数を登録する
var
  f: IJFunction;
  s1,s2,s3,lib,func: String;
  i: Integer;
  v: TJValue;
  module: HModule;
begin
  Result := BuildObject(Self);
  s1 := '';
  s2 := '';
  s3 := '';
  lib := '';
  func := '';

  if IsParam1(Param) then
  begin
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      case i of
        0: lib := AsString(@v);
        1: func := AsString(@v);
        2: s1 := LowerCase(AsString(@v));
        3: s2 := LowerCase(AsString(@v));
        4: s3 := LowerCase(AsString(@v));
      end;
    end;

    //関数名があるならば登録
    if (func <> '') and (lib <> '') then
    begin
      EmptyFunction(f);
      f.Symbol := func;
      f.FuncType := ftDynaCall;
      f.vDynaCall^ := ParseDynaDeclare([s1,s2,s3]);

      //DLLをチェック
      if FModules.HasKey(lib) then
      begin
        //存在すれば
        module := FModules[lib];
        //関数読み込み
        f.vDynaCall.ProcAddr := SearchProcAddress(module,func);
        //失敗したら例外
        if not Assigned(f.vDynaCall.ProcAddr) then
          raise EJThrow.Create(E_DLL,func);
      end
      else begin
        //DLLを読み込み
        module := LoadLibrary(PChar(lib));
        //失敗すると例外
        if module = 0 then
          raise EJThrow.Create(E_DLL,lib);

        f.vDynaCall.ProcAddr := SearchProcAddress(module,func);
        //失敗したらDLLを解放して例外
        if not Assigned(f.vDynaCall.ProcAddr) then
        begin
          FreeLibrary(module);
          raise EJThrow.Create(E_DLL,func);
        end
        else //成功したらDLLをハッシュに入れる
          FModules[lib] := module
      end;

      inherited SetValue(func,BuildFunction(f),False);
    end
    else //例外
      raise EJThrow.Create(E_DLL,'register error: ' + lib + ' ' + func);
  end
  else
    raise EJThrow.Create(E_DLL,'register error: ' + lib + ' ' + func);
end;

function TJDynaCall.SendMessageImpl(const Param: TJValueList; Post: Boolean): Integer;
// SendMessageA,PostMessageA
var
  v: TJValue;
  cnt,i: Integer;
  flg,s: string;
  hwd: HWND;
  msg: UINT;
  prm: array[2..3] of Integer;
  tmp: array[2..3] of TTempValue;
begin
  Result := 0;

  cnt := GetParamCount(Param);
  if cnt > 1 then
  begin
    v := Param[0];
    hwd := AsInteger(@v);//hWnd
    v := Param[1];
    msg := AsInteger(@v);//message
    prm[2] := 0;//wParam
    prm[3] := 0;//lParam

    if cnt > 4 then
    begin
      v := Param[4];
      flg := LowerCase(AsString(@v)) + 'll';//2文字以上にする
      cnt := 4;//4にしとく
    end
    else begin
      flg := 'll';
    end;

    //wParam,lParamに値をセット
    for i := 2 to cnt - 1 do
    begin
      v := Param[i];
      if IsUndefined(@v) or IsNull(@v) then
      begin
        //null,未定義は型指定に関わらず0
        prm[i] := 0;
      end
      else begin
        case flg[i - 1] of
          '1':
          begin
            tmp[i]._char := AsChar(@v);
            prm[i] := Integer(@tmp[i]._char);
          end;
          '2':
          begin
            tmp[i]._short := AsInteger(@v);
            prm[i] := Integer(@tmp[i]._short);
          end;
          '4':
          begin
            tmp[i]._long := AsInteger(@v);
            prm[i] := Integer(@tmp[i]._long);
          end;
          '8':
          begin
            tmp[i]._int64 := Round(AsDouble(@v));
            prm[i] := Integer(@tmp[i]._int64);
          end;
          's':
          begin
            s := AsString(@v);
            prm[i] := Integer(PChar(s));
          end;
          //'l','p','u','b','c','t':
          else
            //当てはまらない場合はすべて整数
            prm[i] := AsInteger(@v);
        end;
      end;
    end;

    //呼び出し
    if not Post then
    begin
      Result := SendMessage(hwd,msg,prm[2],prm[3]);
      //参照指定ならNumberオブジェクトに反映
      for i := 2 to cnt - 1 do
      begin
        v := Param[i];
        if IsNumberObject(@v) then
          case flg[i - 1] of
            '1': (v.vObject as TJNumberObject).int := Integer(tmp[i]._char);
            '2': (v.vObject as TJNumberObject).int := tmp[i]._short;
            '4': (v.vObject as TJNumberObject).int := tmp[i]._long;
            '8': (v.vObject as TJNumberObject).number := tmp[i]._int64;
          end;
      end;
    end
    else begin
      //PostMessageはポストするだけ
      Result := Integer(PostMessage(hwd,msg,prm[2],prm[3]));
    end;
  end;
end;

function TJDynaCall.DoSendMessage(Param: TJValueList): TJValue;
// SendMessage
begin
  Result := BuildInteger(SendMessageImpl(Param));
end;

function TJDynaCall.DoPostMessage(Param: TJValueList): TJValue;
// PostMessage
begin
  Result := BuildBool(SendMessageImpl(Param, True) <> 0);
end;

function TJDynaCall.AsPointer(P: PJValue; AllowString: Boolean = False): Pointer;
//ポインタを得る
begin
  //文字列,Structの場合はバッファのポインタ
  if IsString(P) or IsStructObject(P) then
  begin
    //値文字列不許可の場合はnil
    if not AllowString and (P^.ValueType = vtString) then
      Result := nil
    else
      Result := PChar(AsString(P));
  end
  //数値ならポインタそのものとする
  else if IsNumber(P) then
    Result := Pointer(AsInteger(P))
  else
    Result := nil;
end;

//低レベル操作
function TJDynaCall.DoMoveMemory(Param: TJValueList): TJValue;
// MoveMemory
var
  v: TJValue;
  dest,source: Pointer;
  len: DWORD;
begin
  EmptyValue(Result);

  if IsParam3(Param) then
  begin
    v := Param[0];
    dest := AsPointer(@v);
    v := Param[1];
    source := AsPointer(@v,True);
    v := Param[2];
    len := AsInteger(@v);

    if (dest <> nil) and (source <> nil) and (len > 0) then
    begin
      try
        Move(source^,dest^,len);
      except
        raise EJThrow.Create(E_DLL,'invalid parameter');//fatal
      end;
    end
    else
      raise EJThrow.Create(E_DLL,'invalid parameter');
  end;
end;

function TJDynaCall.DoFillMemory(Param: TJValueList): TJValue;
// FillMemory
var
  v: TJValue;
  dest: Pointer;
  len: DWORD;
  fill: Char;
begin
  EmptyValue(Result);

  if IsParam2(Param) then
  begin
    v := Param[0];
    dest := AsPointer(@v);
    v := Param[1];
    len := AsInteger(@v);

    if IsParam3(Param) then
    begin
      v := Param[2];
      fill := AsChar(@v);
    end
    else
      fill := #0; //省略時はZeroMemory

    if (dest <> nil) and (len > 0) then
    begin
      try
        FillChar(dest^,len,fill);
      except
        raise EJThrow.Create(E_DLL,'invalid parameter');//fatal
      end;
    end
    else
      raise EJThrow.Create(E_DLL,'invalid parameter');
  end;
end;


procedure TJDynaCall.HashtableOnFreeItem(Sender: TObject; P: PHashItem);
//DLLを解放する
begin
  FreeLibrary(P^.vInteger);
end;

procedure TJDynaCall.RegistMethods;
begin
  inherited;
  RegistMethod('register',DoRegister);
  RegistMethod('sendMessage',DoSendMessage);
  RegistMethod('postMessage',DoPostMessage);
  RegistMethod('copyMemory',DoMoveMemory);
  RegistMethod('moveMemory',DoMoveMemory);
  RegistMethod('fillMemory',DoFillMemory);
end;


{ TJStruct }

procedure TJStruct.Clear;
begin
  inherited;
  System.SetLength(FMembers,0);
  System.SetLength(FStruct,0);
end;

procedure TJStruct.Define(const Param: TJValueList);
// 構造体の定義
var
  v: TJValue;
  s: string;
  len,p,i: Integer;
begin
  if IsParam1(Param) then
  begin
    Clear;

    len := 0;
    SetLength(FMembers,Param.Count);

    for i := 0 to Param.Count - 1 do with FMembers[i] do
    begin
      v := Param[i];
      s := AsString(@v);
      p := AnsiPos(':',s);
      // : があればメンバ名を取得
      if p > 0 then
        mName := Trim(Copy(s,1,p - 1))
      else
        mName := '';
      //メンバ名を省略したときは __index を仮メンバ名とする
      if mName = '' then
        mName := '__' + IntToStr(i);

      s := LowerCase(Trim(Copy(s,p + 1,MaxInt)));
      //型を省略したときは例外
      if s = '' then
        raise EJThrow.Create(E_STRUCT,'need member type');

      mSize := StrToIntDef(s,0);
      if mSize > 0 then
        //数値に変換可能のときは文字配列
        mType := smtCharArray
      //数値に変換できないときは一文字目で判断
      else begin
        case s[1] of
          'c':
          begin
            mType := smtChar;
            mSize := SizeOf(Char);
          end;
          't':
          begin
            mType := smtShort;
            mSize := SizeOf(Smallint);
          end;
          'l','p','h','u':
          begin
            mType := smtLong;
            mSize := SizeOf(Longint);
          end;
          'b':
          begin
            mType := smtBool;
            mSize := SizeOf(Longint);
          end;
          'i':
          begin
            mType := smtInt64;
            mSize := SizeOf(Int64);
          end;
          's':
          begin
            mType := smtString;
            mSize := SizeOf(PChar);
          end;
          'w':
          begin
            mType := smtWideString;
            mSize := SizeOf(PWideChar);
          end;
          'f':
          begin
            mType := smtFloat;
            mSize := SizeOf(Single);
          end;
          'd':
          begin
            mType := smtDouble;
            mSize := SizeOf(Double);
          end;
          {'a':
          begin
            mType := smtIDispatch;
            mSize := SizeOf(IDispatch);
          end;
          'k':
          begin
            mType := smtIUnknown;
            mSize := SizeOf(IUnknown);
          end;}
        else
          //当てはまらない場合は例外
          raise EJThrow.Create(E_STRUCT,'member type error');
        end;//case
      end;//if

      mPos := len + 1; //1ベースのオフセット
      Inc(len,mSize);  //型の分だけ構造体のサイズを拡張
      //プロパティとして登録
      EmptyValue(v);
      RegistProperty(mName,v);
    end;//with(for)

    //構造体サイズ分を確保
    SetLength(FStruct,len);
    FillChar(FStruct[1],len,0); //0で初期化する
  end
  else
    raise EJThrow.Create(E_STRUCT,'need member type');
end;

constructor TJStruct.Create(AEngine: TJBaseEngine; Param: TJValueList;
  RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Struct');

  //省略時に例外を出さないようにする
  if IsParam1(Param) then
    Define(Param);
end;

destructor TJStruct.Destroy;
begin
  Clear;
  inherited;
end;

function TJStruct.DoClear(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  Clear;
end;

function TJStruct.DoDefine(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  Define(Param);
end;

function TJStruct.DoSizeOf(Param: TJValueList): TJValue;
//構造体サイズを取得
begin
  Result := BuildInteger(System.Length(FStruct));
end;

function TJStruct.DoToString(Param: TJValueList): TJValue;
//構造体の情報(仮)
var
  i: Integer;
  s,t: string;
  v: TJValue;
begin
  s := '';

  for i := 0 to System.Length(FMembers) - 1 do
  begin
    case FMembers[i].mType of
      smtCharArray:
        t := 'char[' + IntToStr(FMembers[i].mSize) + ']';
      smtChar:
        t := 'char';
      smtShort:
        t := 'short';
      smtLong:
        t := 'long';
      smtBool:
        t := 'bool';
      smtInt64:
        t := 'int64';
      smtFloat:
        t := 'float';
      smtDouble:
        t := 'double';
      smtWideString:
        t := 'widestring';
      smtString:
        if GetValueImpl(FMembers[i].mName,v) and IsStructObject(@v) then
          t := 'struct'
        else
          t := 'string';
      smtIDispatch:
        t := 'idispatch';
      smtIUnknown:
        t := 'iunknown';
      else
        t := '?';
    end;
    s := s + Format('%s : %s (%d)'#13#10,[FMembers[i].mName,t,FMembers[i].mSize]);
  end;

  s := s + StringOfChar('-',24) + #13#10 +
    Format('%dmembers / %dbytes', [System.Length(FMembers),System.Length(FStruct)]);

  Result := BuildString(s);
end;

function TJStruct.GetMemberIndex(const S: string; ArrayStyle: Boolean): Integer;
// メンバのIndexを取得
var
  len,i: Integer;
begin
  Result := -1;

  len := System.Length(FMembers);
  if ArrayStyle then
  begin
    try
      i := StrToInt(S);
      if i < 0 then
        Inc(i,len);
      if (i >= 0) and (i < len) then
        Result := i;
    except
      Result := GetMemberIndex(S,False);
    end;
  end
  else begin
    for i := 0 to len - 1 do
      if S = FMembers[i].mName then
      begin
        Result := i;
        Break;
      end;
  end;
end;

function TJStruct.GetMemberValue(Index: Integer): TJValue;
// メンバの持つ(指す)値を取得
var
  p: Pointer;
  v: TJValue;
begin
  p := @FStruct[FMembers[Index].mPos];
  case FMembers[Index].mType of
    smtCharArray:
      Result := BuildString(PChar(p));
    smtChar:
      Result := BuildInteger(Ord(PChar(p)^));
    smtShort:
      Result := BuildInteger(PSmallint(p)^);
    smtLong:
      Result := BuildInteger(PLongint(p)^);
    smtBool:
      Result := BuildBool(PLongint(p)^ <> 0);
    smtInt64:
      Result := BuildDouble(PInt64(p)^);
    smtFloat:
      Result := BuildDouble(PSingle(p)^);
    smtDouble:
      Result := BuildDouble(PDouble(p)^);
    smtWideString:
      Result := BuildString(PPWideChar(p)^);
    smtString:
      //Structオブジェクトならオブジェクトを返す
      if GetValueImpl(FMembers[Index].mName,v) and IsStructObject(@v) then
        Result := v
      else
        Result := BuildString(PPChar(p)^);
    {smtIDispatch:
      Result := BuildDispatch(FMembers[Index]._idispatch);
    smtIUnknown:
      Result := BuildDispatch(IDispatch(FMembers[Index]._iunknown));}
  else
    raise EJThrow.Create(E_STRUCT,'member type error');
  end;
end;

function TJStruct.GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList): TJValue;
var
  i: Integer;
begin
  i := GetMemberIndex(S,ArrayStyle);
  if i < 0 then
    Result := inherited GetValue(S,ArrayStyle)
  else
    Result := GetMemberValue(i);
end;

procedure TJStruct.SetMemberValue(Index: Integer; Value: TJValue);
var
  s: string;
  v: TTempValue;
begin
  with FMembers[Index] do
  begin
    case mType of
      smtCharArray:
      begin
        if IsNull(@Value) or IsUndefined(@Value) then
          s := #0
        else
          s := AsString(@Value) + #0;

        if System.Length(s) > mSize then
          s := Copy(s,1,mSize - 1) + #0;

        Move(s[1],FStruct[mPos],System.Length(s));
      end;
      smtChar:
      begin
        v._char := AsChar(@Value);
        Move(v._char,FStruct[mPos],SizeOf(v._char));
      end;
      smtShort:
      begin
        v._short := Smallint(AsInteger(@Value));
        Move(v._short,FStruct[mPos],SizeOf(v._short));
      end;
      smtLong,smtBool:
      begin
        v._long := Longint(AsInteger(@Value));
        Move(v._long,FStruct[mPos],SizeOf(v._long));
      end;
      smtInt64:
      begin
        v._int64 := Round(AsDouble(@Value));
        Move(v._int64,FStruct[mPos],SizeOf(v._int64));
      end;
      smtString:
      begin
        _string := AsString(@Value);
        if IsNull(@Value) or IsUndefined(@Value) then
          v._pointer := nil
        else
          v._pointer := PChar(_string);
        Move(v._pointer,FStruct[mPos],SizeOf(v._pointer));
      end;
      smtWideString:
      begin
        _widestring := AsString(@Value);
        if IsNull(@Value) or IsUndefined(@Value) then
          v._pointer := nil
        else
          v._pointer := PWideChar(_widestring);
        Move(v._pointer,FStruct[mPos],SizeOf(v._pointer));
      end;
      smtFloat:
      begin
        v._float := AsSingle(@Value);
        Move(v._float,FStruct[mPos],SizeOf(v._float));
      end;
      smtDouble:
      begin
        v._double := AsDouble(@Value);
        Move(v._double,FStruct[mPos],SizeOf(v._double));
      end;
      {smtIDispatch:
      begin
        _idispatch := AsDispatch(@Value);
        Move(_idispatch,FStruct[mPos],SizeOf(IDispatch));
      end;
      smtIUnknown:
      begin
        _iunknown := IUnknown(AsDispatch(@Value));
        Move(_iunknown,FStruct[mPos],SizeOf(IUnknown));
      end;}
    else
      raise EJThrow.Create(E_STRUCT,'member type error');
    end;//case

    //オブジェクトのメンバにも反映
    if not SetValueImpl(mName,Value) then
      raise EJThrow.Create(E_STRUCT,'member not found');
  end;
end;

procedure TJStruct.SetValue(S: String; Value: TJValue; ArrayStyle: Boolean;
  Param: TJValueList);
var
  i: Integer;
begin
  i := GetMemberIndex(S,ArrayStyle);
  if i < 0 then
    inherited
  else
    SetMemberValue(i,Value);
end;

function TJStruct.ToString(Value: PJValue): string;
begin
  Result := FStruct;
end;

function TJStruct.GetItem(Index: Integer): TJValue;
begin
  Result := GetMemberValue(Index);
end;

function TJStruct.GetCount: Integer;
begin
  Result := System.Length(FMembers);
end;

class function TJStruct.IsArray: Boolean;
begin
  Result := True;
end;

class function TJStruct.IsMakeGlobalInstance: Boolean;
begin
  Result := False;
end;

procedure TJStruct.RegistMethods;
begin
  inherited;
  RegistMethod('clear',DoClear);
  RegistMethod('define',DoDefine);
  RegistMethod('sizeOf',DoSizeOf);
  RegistMethod('toString',DoToString);
end;


end.
