unit ecma_object;

//基本組込みObject
//2001/04/14 ~
//by Wolfy

//Unicode対応による仕様変更
//2010/4/29
//by m.matsubara
//  文字列型でShiftJISやEUC, JISの変換を扱うメソッドは廃止しました。
//  （本質的にそれはバイナリ操作です）
//  バイナリを扱うにはBytes型を使います。
//  文字列とバイナリの相互変換にはEncoding型を使います。(ecma_extobject.pasにて定義)

interface

uses
  windows,classes,sysutils,
{$IFNDEF CONSOLE}
  dialogs,forms,ecma_promptfrm,ecma_textareafrm,
{$ENDIF}
  gsocketmisc,
  hashtable,ecma_type,Math,
  jconvert,ecma_misc,crclib,unicodelib,
  crypt3,dynamiccall,ecma_re;

type
  TOnPrineEvent = procedure(S: String) of object;

  TJArrayObject = class;
  //グローバル
  TJGlobalObject = class(TJObject)
  private
    FOnPrint: TStringEvent;
    FOnRead: TReadStringEvent;
    FArgs: TJArrayObject;
    FTerminated: Boolean;
    FFinalizer: TJArrayObject;

    function DoPrintln(Param: TJValueList): TJValue;
    function DoPrint(Param: TJValueList): TJValue;
    function DoEscape(Param: TJValueList): TJValue;
    function DoEval(Param: TJValueList): TJValue;
    function DoIsFinite(Param: TJValueList): TJValue;
    function DoIsNaN(Param: TJValueList): TJValue;
    function DoParseFloat(Param: TJValueList): TJValue;
    function DoParseInt(Param: TJValueList): TJValue;
    function DoUnescape(Param: TJValueList): TJValue;
    function DoAlert(Param: TJValueList): TJValue;
    function DoConfirm(Param: TJValueList): TJValue;
    function DoPrompt(Param: TJValueList): TJValue;
    function DoTextArea(Param: TJValueList): TJValue;
    function DoExit(Param: TJValueList): TJValue;
    function DoMsgBox(Param: TJValueList): TJValue;
    function DoRead(Param: TJValueList): TJValue;
    function DoReadln(Param: TJValueList): TJValue;
    function DoScriptEngine(Param: TJValueList): TJValue;
    function DoScriptEngineBuildVersion(Param: TJValueList): TJValue;
    function DoScriptEngineMajorVersion(Param: TJValueList): TJValue;
    function DoScriptEngineMinorVersion(Param: TJValueList): TJValue;
    function DoScriptEngineVersion(Param: TJValueList): TJValue;
    function DoIsConsole(Param: TJValueList): TJValue;
    function DoEncodeURI(Param: TJValueList): TJValue;
    function DoEncodeURIComponent(Param: TJValueList): TJValue;
    function DoDecodeURI(Param: TJValueList): TJValue;
    function DoDecodeURIComponent(Param: TJValueList): TJValue;
    function DoFormat(Param: TJValueList): TJValue;
    function DoFormatFloat(Param: TJValueList): TJValue;
    function DoEventLoop(Param: TJValueList): TJValue;
    function DoTerminate(Param: TJValueList): TJValue;
    function DoPrintfln(Param: TJValueList): TJValue;
    function DoPrintf(Param: TJValueList): TJValue;
    function DoDoEvents(Param: TJValueList): TJValue;
    function DoSleep(Param: TJValueList): TJValue;
    function DoIsUndefined(Param: TJValueList): TJValue;
    function DoIsNull(Param: TJValueList): TJValue;
    function DoNameOf(Param: TJValueList): TJValue;

    function GetApplicationHandle: THandle;
    function GetMainFormHandle: THandle;
    function GetPlatform: String;
  protected
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure Clear; override;

    procedure Print(S: String); overload;
    procedure Println(S: String); overload;
    procedure Print(V: TJValue); overload;
    function Read(Count: Integer; Line: Boolean): String;
    procedure EventLoop;
    procedure Terminate;

    property OnPrint: TStringEvent read FOnPrint write FOnPrint;
    property OnRead: TReadStringEvent read FOnRead write FOnRead;
  published
    //property args: TJArrayObject read FArgs;
    property arguments: TJArrayObject read FArgs;
    property applicationHandle: THandle read GetApplicationHandle;
    property mainFormHandle: THandle read GetMainFormHandle;
    property terminated: Boolean read FTerminated;
    property platform: String read GetPlatform;
    property finalizer: TJArrayObject read FFinalizer;
  end;

  //配列
  TJArrayObject = class(TJObject)
  private
    FFunction: IJFunction;
    FItems: TJValueList;
    FSortArgs: TJValueList;

    function ItemsSort1(Item1, Item2: Pointer): Integer;
    function ItemsSort2(Item1, Item2: Pointer): Integer;
    //登録メソッド
    function DoAppend(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoSort(Param: TJValueList): TJValue;
    function DoConCat(Param: TJValueList): TJValue;
    function DoJoin(Param: TJValueList): TJValue;
    function DoPop(Param: TJValueList): TJValue;
    function DoPush(Param: TJValueList): TJValue;
    function DoReverse(Param: TJValueList): TJValue;
    function DoShift(Param: TJValueList): TJValue;
    function DoSlice(Param: TJValueList): TJValue;
    function DoSplice(Param: TJValueList): TJValue;
    function DoUnShift(Param: TJValueList): TJValue;
    function DoAssign(Param: TJValueList): TJValue;
  protected
    procedure SetLength(const Value: Integer);
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    function ToString(Value: PJValue = nil): String; override;
    function ValueOf: TJValue; override;

    procedure Clear; override;
    function Add(Value: TJValue): Integer;
    procedure Insert(Index: Integer; Value: TJValue);
    procedure Delete(Index: Integer);

    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;

    property Items: TJValueList read FItems;
    property Count: Integer read GetCount;
  published
    property length: Integer read GetCount write SetLength;
  end;

  TJRegExpObject = class;

  TJStringObject = class(TJObject)
  private
    FText: String;

    function DoCharAt(Param: TJValueList): TJValue;
    function DoCharCodeAt(Param: TJValueList): TJValue;
    function DoConCat(Param: TJValueList): TJValue;
    function DoAnchor(Param: TJValueList): TJValue;
    function DoBig(Param: TJValueList): TJValue;
    function DoBlink(Param: TJValueList): TJValue;
    function DoBold(Param: TJValueList): TJValue;
    function DoFixed(Param: TJValueList): TJValue;
    function DoFontColor(Param: TJValueList): TJValue;
    function DoFontSize(Param: TJValueList): TJValue;
    function DoFromCharCode(Param: TJValueList): TJValue;
    function DoIndexOf(Param: TJValueList): TJValue;
    function DoItalics(Param: TJValueList): TJValue;
    function DoLastIndexOf(Param: TJValueList): TJValue;
    function DoLink(Param: TJValueList): TJValue;
    function DoMatch(Param: TJValueList): TJValue;
    function DoReplace(Param: TJValueList): TJValue;
    function DoSearch(Param: TJValueList): TJValue;
    function DoSlice(Param: TJValueList): TJValue;
    function DoSmall(Param: TJValueList): TJValue;
    function DoSplit(Param: TJValueList): TJValue;
    function DoStrike(Param: TJValueList): TJValue;
    function DoSub(Param: TJValueList): TJValue;
    function DoSubStr(Param: TJValueList): TJValue;
    function DoSubString(Param: TJValueList): TJValue;
    function DoSup(Param: TJValueList): TJValue;
    function DoToLowerCase(Param: TJValueList): TJValue;
    function DoToUpperCase(Param: TJValueList): TJValue;
{$ifdef UNICODE}
{$else}
    function DoToSJIS(Param: TJValueList): TJValue;
    function DoToJIS(Param: TJValueList): TJValue;
    function DoToEUC(Param: TJValueList): TJValue;
    function DoToWide(Param: TJValueList): TJValue;
    function DoToUtf8(Param: TJValueList): TJValue;
    function DoFromJISToSJIS(Param: TJValueList): TJValue;
    function DoFromJISToEUC(Param: TJValueList): TJValue;
    function DoFromEUCToSJIS(Param: TJValueList): TJValue;
    function DoFromEUCToJIS(Param: TJValueList): TJValue;
    function DoFromSJISToEUC(Param: TJValueList): TJValue;
    function DoFromSJISToJIS(Param: TJValueList): TJValue;
    function DoFromUtf8ToSJIS(Param: TJValueList): TJValue;
{$endif}
    //function DoOrd(Param: TJValueList): TJValue;
{$ifdef UNICODE}
{$else}
    function DoCrypt(Param: TJValueList): TJValue;
{$endif}
    function DoTrim(Param: TJValueList): TJValue;
    function DoTrimLeft(Param: TJValueList): TJValue;
    function DoTrimRight(Param: TJValueList): TJValue;
    function DoLocaleCompare(Param: TJValueList): TJValue;
    function DoReverse(Param: TJValueList): TJValue;
    function DoSizeOf(Param: TJValueList): TJValue;
    function ReplaceRange(Param: TJValueList): TJValue;
    function DoToZenkaku(Param: TJValueList): TJValue;
    function DoToHankaku(Param: TJValueList): TJValue;
    function DoToHiragana(Param: TJValueList): TJValue;
    function DoToKatakana(Param: TJValueList): TJValue;
    function DoMultiply(Param: TJValueList): TJValue;
  protected
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    function ToString(Value: PJValue = nil): String; override;
    function ToNumber: Double; override;
    function ToBool: Boolean; override;
    function ToChar: Char; override;
    function ValueOf: TJValue; override;

    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;

  published
    property length: Integer read GetCount;
    property text: String read FText write FText;
    property str: String read FText write FText;
  end;
  //数字
  TJNumberObject = class(TJObject)
  private
    function DoToExponential(Param: TJValueList): TJValue;
    function DoToFixed(Param: TJValueList): TJValue;
    function DoToPrecision(Param: TJValueList): TJValue;
    function DoToChar(Param: TJValueList): TJValue;
    function GetNumber: Double;
    procedure SetNumber(const Value: Double);
    function GetInt: Integer;
    procedure SetInt(const Value: Integer);
    function GetAsChar: String;
    procedure SetAsChar(const Value: String);
  protected
    procedure RegistProperties; override;
    procedure RegistMethods; override;
  public
    FValue: TJValue;
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
    function ToNumber: Double; override;
    function ToBool: Boolean; override;
    function ToChar: Char; override;
    function ValueOf: TJValue; override;
  published
    property number: Double read GetNumber write SetNumber;
    property int: Integer read GetInt write SetInt;
    property asChar: String read GetAsChar write SetAsChar;
  end;
  //真偽
  TJBooleanObject = class(TJObject)
  private
  protected
  public
    FBool: Boolean;
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
    function ToNumber: Double; override;
    function ToBool: Boolean; override;
    function ToChar: Char; override;
    function ValueOf: TJValue; override;
  published
    property bool: Boolean read FBool write FBool;
  end;

  //正規表現オブジェクト
  TJRegExpObject = class(TJObject)
  private
    FRegExp: TJRegExp;
    FReplaceFunc: IJFunction;
    FReplaceParam: TJValueList;

    FOnMatchEnd: TNotifyEvent;
    FOnMatchStart: TNotifyEvent;
    FOnExecInput: TRefStringEvent;

    function DoExec(Param: TJValueList): TJValue;
    function DoSplit(Param: TJValueList): TJValue;
    function DoReplace(Param: TJValueList): TJValue;
    function DoTest(Param: TJValueList): TJValue;

    function GetIgnoreCase: Boolean;
    procedure SetIgnoreCase(const Value: Boolean);
    function GetSource: String;
    procedure SetSource(const Value: String);
    function GetMultiLine: Boolean;
    procedure SetMultiLine(const Value: Boolean);
    function GetGlobal: Boolean;
    function GetIndex: Integer;
    function GetInput: String;
    function GetLastIndex: Integer;
    function GetLastMatch: String;
    function GetLastParen: String;
    function GetLeftContext: String;
    function GetRightContext: String;
    procedure SetGlobal(const Value: Boolean);

    function FuncReplace(RE: TJRegExp; Matched: String): String;

    procedure RegistSubMatch;
    procedure SetInput(const Value: String);
  protected
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ClearMatch;
    function Test(Value: TJValue): Boolean;
    procedure SetRegExpValue(var Value: TJvalue);
    procedure Assign(Source: TPersistent); override;
    function ToString(Value: PJValue = nil): String; override;
    //event
    property OnMatchStart: TNotifyEvent read FOnMatchStart write FOnMatchStart;
    property OnMatchEnd: TNotifyEvent read FOnMatchEnd write FOnMatchEnd;
    property OnExecInput: TRefStringEvent read FOnExecInput write FOnExecInput;
  published
    property ignoreCase: Boolean read GetIgnoreCase write SetIgnoreCase;
    property global: Boolean read GetGlobal write SetGlobal;
    property source: String read GetSource write SetSource;
    property multiline: Boolean read GetMultiLine write SetMultiLine;
    property input: String read GetInput write SetInput;
    property index: Integer read GetIndex;
    property lastIndex: Integer read GetLastIndex;
    property lastMatch: String read GetLastMatch;
    property lastParen: String read GetLastParen;
    property leftContext: String read GetLeftContext;
    property rightContext: String read GetRightContext;
  end;

  //数学オブジェクト
  TJMathObject = class(TJObject)
  private
    function DoExp(Param: TJValueList): TJValue;
    function DoLog(Param: TJValueList): TJValue;
    function DoSqrt(Param: TJValueList): TJValue;
    function DoAbs(Param: TJValueList): TJValue;
    function DoCeil(Param: TJValueList): TJValue;
    function DoFloor(Param: TJValueList): TJValue;
    function DoRound(Param: TJValueList): TJValue;
    function DoSin(Param: TJValueList): TJValue;
    function DoCos(Param: TJValueList): TJValue;
    function DoTan(Param: TJValueList): TJValue;
    function DoAsin(Param: TJValueList): TJValue;
    function DoAcos(Param: TJValueList): TJValue;
    function DoAtan(Param: TJValueList): TJValue;
    function DoAtan2(Param: TJValueList): TJValue;
    function DoMax(Param: TJValueList): TJValue;
    function DoMin(Param: TJValueList): TJValue;
    function DoPow(Param: TJValueList): TJValue;
    function DoRandom(Param: TJValueList): TJValue;
    function GetE: Double;
    function GetLN10: Double;
    function GetLN2: Double;
    function GetLOG10E: Double;
    function GetLOG2E: Double;
    function GetPI: Double;
    function GetSQRT1_2: Double;
    function GetSQRT2: Double;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  published
    property E: Double read GetE;
    property LN2: Double read GetLN2;
    property LN10: Double read GetLN10;
    property LOG2E: Double read GetLOG2E;
    property LOG10E: Double read GetLOG10E;
    property SQRT1_2: Double read GetSQRT1_2;
    property SQRT2: Double read GetSQRT2;
    property PI: Double read GetPI;
  end;


    //日付
  TJDateObject = class(TJObject)
  private
    FFormat: String;
    function GetUTC: TDateTime;
    procedure SetUTC(const Value: TDateTime);
    function GetLocal: TDateTime;
    procedure SetLocal(const Value: TDateTime);
  private
    FDate: TDateTime;

    function DoGetFullYear(Param: TJValueList): TJValue;
    function DoGetYear(Param: TJValueList): TJValue;
    function DoGetMonth(Param: TJValueList): TJValue;
    function DoGetDate(Param: TJValueList): TJValue;
    function DoGetDay(Param: TJValueList): TJValue;
    function DoGetHours(Param: TJValueList): TJValue;
    function DoGetMinutes(Param: TJValueList): TJValue;
    function DoGetSeconds(Param: TJValueList): TJValue;
    function DoGetMilliSeconds(Param: TJValueList): TJValue;
    function DoSetFullYear(Param: TJValueList): TJValue;
    function DoSetYear(Param: TJValueList): TJValue;
    function DoSetMonth(Param: TJValueList): TJValue;
    function DoSetDate(Param: TJValueList): TJValue;
    function DoSetHours(Param: TJValueList): TJValue;
    function DoSetMinutes(Param: TJValueList): TJValue;
    function DoSetSeconds(Param: TJValueList): TJValue;
    function DoSetMilliSeconds(Param: TJValueList): TJValue;

    function DoGetUTCFullYear(Param: TJValueList): TJValue;
    function DoGetUTCYear(Param: TJValueList): TJValue;
    function DoGetUTCMonth(Param: TJValueList): TJValue;
    function DoGetUTCDate(Param: TJValueList): TJValue;
    function DoGetUTCDay(Param: TJValueList): TJValue;
    function DoGetUTCHours(Param: TJValueList): TJValue;
    function DoGetUTCMinutes(Param: TJValueList): TJValue;
    function DoGetUTCSeconds(Param: TJValueList): TJValue;
    function DoGetUTCMilliSeconds(Param: TJValueList): TJValue;
    function DoSetUTCFullYear(Param: TJValueList): TJValue;
    function DoSetUTCYear(Param: TJValueList): TJValue;
    function DoSetUTCMonth(Param: TJValueList): TJValue;
    function DoSetUTCDate(Param: TJValueList): TJValue;
    function DoSetUTCHours(Param: TJValueList): TJValue;
    function DoSetUTCMinutes(Param: TJValueList): TJValue;
    function DoSetUTCSeconds(Param: TJValueList): TJValue;
    function DoSetUTCMilliSeconds(Param: TJValueList): TJValue;

    function DoGetTime(Param: TJValueList): TJValue;
    function DoSetTime(Param: TJValueList): TJValue;
    function DoGetTimezoneOffset(Param: TJValueList): TJValue;
    function DoToLocaleString(Param: TJValueList): TJValue;
    function DoToGMTString(Param: TJValueList): TJValue;
    function DoToUTCString(Param: TJValueList): TJValue;
    function DoUTC(Param: TJValueList): TJValue;
    function DoParse(Param: TJValueList): TJValue;

  protected
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
    function ValueOf: TJValue; override;
    function ToNumber: Double; override;

    property LocalTime: TDateTime read GetLocal write SetLocal;
    property UTC: TDateTime read GetUTC write SetUTC;
  published
    property format: String read FFormat write FFormat;
  end;

  //関数
  TJFunctionObject = class(TJObject)
  private
    FActivation: TJLocalSymbolTable;
  protected
    function GetValueImpl(S: String; var RetVal: TJValue; Param: TJValueList = nil): Boolean; override;
    function SetValueImpl(S: String; var Value: TJValue; Param: TJValueList = nil): Boolean; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function ToString(Value: PJValue = nil): String; override;
    property Activation: TJLocalSymbolTable read FActivation;
  end;



implementation

uses
  ecma_engine,ecma_parser;


{ TJGlobalObject }

constructor TJGlobalObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Global');
  FArgs := TJArrayObject.Create(FEngine,nil,False);
  FArgs.IncRef;
  FTerminated := True;
  FFinalizer := TJArrayObject.Create(FEngine,nil,False);
  FFinalizer.IncRef;
end;

destructor TJGlobalObject.Destroy;
begin
  FFinalizer.DecRef;
  FArgs.DecRef;
  inherited Destroy;
end;

function TJGlobalObject.DoEscape(Param: TJValueList): TJValue;
//urlエンコード
var
  s: String;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := Escape(AsString(@v));
    Result := BuildString(s);
  end;
end;

function TJGlobalObject.DoEval(Param: TJValueList): TJValue;
//文字列を関数式に変換して実行する
//function(){return evalコード;}
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) and Assigned(FEngine) then
  begin
    v := Param[0];
    Result := (FEngine as TJEngine).Eval(AsString(@v));
  end;
end;

function TJGlobalObject.DoIsFinite(Param: TJValueList): TJValue;
//有限な数値かどうか
var
  v: TJValue;
begin
  Result := BuildBool(False);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(ecma_type.TryAsNumber(@v));
  end;
end;

function TJGlobalObject.DoIsNaN(Param: TJValueList): TJValue;
//NaN？
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(ecma_type.IsNaN(@v));
  end;
end;

function TJGlobalObject.DoParseFloat(Param: TJValueList): TJValue;
//浮動小数点を返す
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if ecma_type.TryAsNumber(@v) then
      Result := BuildDouble(AsDouble(@v))
    else
      Result := BuildNaN;
  end;
end;

function TJGlobalObject.DoParseInt(Param: TJValueList): TJValue;
//整数を返す
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if ecma_type.TryAsNumber(@v) then
      Result := BuildInteger(AsInteger(@v))
    else
      Result := BuildNaN;
  end;
end;

function TJGlobalObject.DoPrint(Param: TJValueList): TJValue;
//標準出力
var
  i: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
    begin
      Result := Param[i];
      Print(Result);
    end;
end;

procedure TJGlobalObject.Print(S: String);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Self,S);
end;

function TJGlobalObject.DoPrintln(Param: TJValueList): TJValue;
//標準出力改行つき
var
  i: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
    begin
      Result := Param[i];
      Print(AsString(@Result) + CRLF);
    end;
end;

function TJGlobalObject.DoUnescape(Param: TJValueList): TJValue;
//urlデコード
var
  s: String;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := Unescape(AsString(@v));
    Result := BuildString(s);
  end;
end;

procedure TJGlobalObject.Print(V: TJValue);
begin
  Print(AsString(@V));
end;

function TJGlobalObject.DoAlert(Param: TJValueList): TJValue;
//ダイアログ表示
var
  s,capt: String;
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    capt := '';
    if Param.Count > 1 then
    begin
      v := Param[1];
      capt := AsString(@v);
    end;

    if capt = '' then
      capt := GetApplicationTitle;

    MsgBox(PChar(s),PChar(capt),MB_OK or MB_ICONEXCLAMATION);
  end;
end;

function TJGlobalObject.DoConfirm(Param: TJValueList): TJValue;
//ダイアログ表示
var
  s,capt: String;
  v: TJValue;
  r: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    capt := '';
    if Param.Count > 1 then
    begin
      v := Param[1];
      capt := AsString(@v);
    end;

    if capt = '' then
      capt := GetApplicationTitle;

    r := MsgBox(PChar(s),PChar(capt),MB_OKCANCEL or MB_ICONQUESTION);
    Result := BuildBool(r <> IDCANCEL);
  end;
end;

function TJGlobalObject.DoPrompt(Param: TJValueList): TJValue;
//プロンプト表示
//consoleの場合はDoReadlnへ変更
var
  v: TJValue;
  msg: String;
{$IFNDEF CONSOLE}
  frm: TfrmPrompt;
  def,capt: String;
  pas: Char;
  w,i: Integer;
{$ENDIF}
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    msg := AsString(@v);
{$IFNDEF CONSOLE}
    def := '';
    pas := #0;
    capt := '';
    w := 0;
    for i := 1 to Param.Count - 1 do
    begin
      v := Param[i];
      case i of
        1: def := AsString(@v);
        2: pas := AsChar(@v);
        3: capt := AsString(@v);
        4: w := AsInteger(@v);
      else
        Break;
      end;
    end;
  {$IFNDEF MB_NO_OWNER}
    frm := TfrmPrompt.Create(Application.MainForm);
  {$ELSE}
    frm := TfrmPrompt.Create(nil);
  {$ENDIF}
    try
      frm.lblText.Caption := msg;
      frm.edtPrompt.Text := def;
      frm.edtPrompt.PasswordChar := pas;
      if capt <> '' then
        frm.Caption := capt
      else
        frm.Caption := GetApplicationTitle;
      if w > 0 then
        frm.ClientWidth := Max(w,200);

      if frm.ShowModal = IDOK then
        Result := BuildString(frm.edtPrompt.Text)
      else
        Result := BuildNull;
    finally
      //frm.Release;
      frm.Free;
    end;
{$ELSE}
    Println(msg);
    Print('>');
    Result := DoRead(nil);
{$ENDIF}
  end;
end;

function TJGlobalObject.DoTextArea(Param: TJValueList): TJValue;
//メモ表示
//consoleの場合はreadlnへ変更
var
  msg: String;
  v: TJValue;
{$IFNDEF CONSOLE}
  frm: TfrmTextArea;
  def,capt: String;
  w,h,i: Integer;
{$ENDIF}
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    msg := AsString(@v);
{$IFNDEF CONSOLE}
    def := '';
    capt := '';
    w := 0;
    h := 0;
    for i := 1 to Param.Count - 1 do
    begin
      v := Param[i];
      case i of
        1: def := AsString(@v);
        2: capt := AsString(@v);
        3: w := AsInteger(@v);
        4: h := AsInteger(@v);
      else
        Break;
      end;
    end;
  {$IFNDEF MB_NO_OWNER}
    frm := TfrmTextArea.Create(Application.MainForm);
  {$ELSE}
    frm := TfrmTextArea.Create(nil);
  {$ENDIF}
    try
      frm.lblText.Caption := msg;
      if def <> '' then
        frm.mmText.Lines.Add(def);
      if capt <> '' then
        frm.Caption := capt
      else
        frm.Caption := GetApplicationTitle;
      if w > 0 then
        frm.ClientWidth := Max(w,200);
      if h > 0 then
        frm.ClientHeight := Max(h,200);

      if frm.ShowModal = IDOK then
        Result := BuildString(frm.mmText.Text)
      else
        Result := BuildNull;
    finally
      //frm.Release;
      frm.Free;
    end;
{$ELSE}
    Println(msg);
    Print('>');
    Result := DoReadLn(nil);
{$ENDIF}
  end;
end;

function TJGlobalObject.DoMsgBox(Param: TJValueList): TJValue;
//ダイアログ表示
var
  s,capt: String;
  v: TJValue;
  flag,i: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    flag := MB_OK;
    capt := '';
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      case i of
        0: s := AsString(@v);     // テキスト
        1: flag := AsInteger(@v); // フラグ
        2: capt := AsString(@v);  // キャプション
      else
        Break;
      end;
    end;

    if capt = '' then
      capt := GetApplicationTitle;

    Result := BuildInteger(MsgBox(PChar(s),PChar(capt),flag));
  end;
end;

function TJGlobalObject.DoExit(Param: TJValueList): TJValue;
//終了する
var
  v: TJValue;
begin
  //例外
  if IsParam1(Param) then
  begin
    v := Param[0];
    raise EJExit.Create(AsInteger(@v));
  end
  else
    raise EJExit.Create(0);
end;

function TJGlobalObject.GetApplicationHandle: THandle;
begin
{$IFNDEF CONSOLE}
  Result := Application.Handle;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function TJGlobalObject.GetMainFormHandle: THandle;
begin
  Result := 0;
{$IFNDEF CONSOLE}
  if Assigned(Application.MainForm) then
    Result := Application.MainForm.Handle;
{$ENDIF}
end;

function TJGlobalObject.DoScriptEngine(Param: TJValueList): TJValue;
begin
  Result := BuildString(DMS_ENGINE);
end;

function TJGlobalObject.DoScriptEngineBuildVersion(
  Param: TJValueList): TJValue;
begin
  Result := BuildInteger(DMS_BUILD);
end;

function TJGlobalObject.DoScriptEngineMajorVersion(
  Param: TJValueList): TJValue;
begin
  Result := BuildInteger(DMS_MAJOR);
end;

function TJGlobalObject.DoScriptEngineMinorVersion(
  Param: TJValueList): TJValue;
begin
  Result := BuildDouble(DMS_MINOR);
end;

function TJGlobalObject.DoRead(Param: TJValueList): TJValue;
var
  v: TJValue;
  size: Integer;
  s: String;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    size := AsInteger(@v);
    //指定サイズをコピー
    s := Copy(Read(size,False),1,size);
    Result := BuildString(s);
  end
  else
    Result := BuildString(Read(-1,False));
end;

function TJGlobalObject.DoReadln(Param: TJValueList): TJValue;
begin
  Result := BuildString(Read(-1,True));
end;

function TJGlobalObject.Read(Count: Integer; Line: Boolean): String;
var
  success: Boolean;
begin
  success := False;
  if Assigned(FOnRead) then
    FOnRead(Self,Result,success,Count,Line);

  if not success then
    raise EJThrow.Create(E_FILE,'read error: STDIN');
end;

function TJGlobalObject.DoScriptEngineVersion(Param: TJValueList): TJValue;
begin
  Result := BuildString(DMS_VERSION);
end;

function TJGlobalObject.DoIsConsole(Param: TJValueList): TJValue;
begin
  Result := BuildBool(IsConsole);
end;

function TJGlobalObject.DoDecodeURI(Param: TJValueList): TJValue;
//urlデコード
var
  s: String;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := DecodeURI(AsString(@v));
    Result := BuildString(s);
  end;
end;

function TJGlobalObject.DoDecodeURIComponent(Param: TJValueList): TJValue;
//urlデコード
var
  s: String;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := DecodeURIComponent(AsString(@v));
    Result := BuildString(s);
  end;
end;

function TJGlobalObject.DoEncodeURI(Param: TJValueList): TJValue;
//urlエンコード
var
  s: String;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := EncodeURI(AsString(@v));
    Result := BuildString(s);
  end;
end;

function TJGlobalObject.DoEncodeURIComponent(Param: TJValueList): TJValue;
//urlエンコード
var
  s: String;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := EncodeURIComponent(AsString(@v));
    Result := BuildString(s);
  end;
end;

procedure TJGlobalObject.Println(S: String);
begin
  Print(S + CRLF);
end;

function TJGlobalObject.DoFormat(Param: TJValueList): TJValue;
var
  rec: TVarRecArray;
  i: Integer;
  v: TJValue;
  s: String;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
    //最初を削除する
    Param.Delete(0);
    //TVarRec作成
    SetLength(rec,Param.Count);
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      rec[i] := ValueToVarRec(v);
    end;

    try try
      Result := BuildString(Format(s,rec));
    finally
      //終了処理
      DisposeVarRec(rec);
    end
    except
      on E:EConvertError do
        raise EJThrow.Create(E_CONVERT,E.Message);
    end;
  end;
end;

function TJGlobalObject.DoFormatFloat(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
  d: Double;
begin
  EmptyValue(Result);
  if IsParam2(Param) then
  begin
    v := Param[0];
    s := AsString(@v);

    v := Param[1];
    d := AsDouble(@v);
    try
      Result := BuildString(FormatFloat(s,d));
    except
      on E:EConvertError do
        raise EJThrow.Create(E_CONVERT,E.Message);
    end;
  end;
end;

function TJGlobalObject.DoEventLoop(Param: TJValueList): TJValue;
//イベントを開始
begin
  EmptyValue(Result);
  EventLoop;
end;

function TJGlobalObject.DoTerminate(Param: TJValueList): TJValue;
//イベントを終了
begin
  EmptyValue(Result);
  Terminate;
end;

procedure TJGlobalObject.EventLoop;
begin
  //falseにするだけ
  FTerminated := False;
end;

procedure TJGlobalObject.Terminate;
begin
  //trueにするだけ
  FTerminated := True;
end;

function TJGlobalObject.DoPrintf(Param: TJValueList): TJValue;
begin
  Result := DoFormat(Param);
  Print(AsString(@Result));
end;

function TJGlobalObject.DoPrintfln(Param: TJValueList): TJValue;
begin
  Result := DoFormat(Param);
  Println(AsString(@Result));
end;

function TJGlobalObject.DoDoEvents(Param: TJValueList): TJValue;
//イベント処理
var
  engine: TJEngine;
begin
  Result := BuildBool(False);
  if Assigned(FEngine) then
  begin
    engine := FEngine as TJEngine;
    Result := BuildBool(engine.DoEvents);
  end;
end;

function TJGlobalObject.DoSleep(Param: TJValueList): TJValue;
//sleep
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Sleep(AsInteger(@v));
  end
  else
    Sleep(0);
end;

function TJGlobalObject.DoIsNull(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(IsNull(@v));
  end
  else
    Result := BuildBool(False);
end;

function TJGlobalObject.DoIsUndefined(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(IsUndefined(@v));
  end
  else
    Result := BuildBool(False);
end;

function TJGlobalObject.DoNameOf(Param: TJValueList): TJValue;
//オブジェクトの名前を得る
//※エンジンに登録されたオブジェクト名とは異なる場合がある
var
  v: TJValue;
begin
  //オブジェクトで無い場合はnullを返す
  Result := BuildNull;
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsObject(@v) then
      Result := BuildString(v.vObject.Name);
  end;
end;

procedure TJGlobalObject.RegistMethods;
begin
  inherited;
  RegistMethod('escape',DoEscape);
  RegistMethod('unescape',DoUnescape);
  RegistMethod('eval',DoEval);
  RegistMethod('isFinite',DoIsFinite);
  RegistMethod('isNaN',DoIsNaN);
  RegistMethod('parseFloat',DoParseFloat);
  RegistMethod('parseInt',DoParseInt);
  RegistMethod('print',DoPrint);
  RegistMethod('println',DoPrintln);
  RegistMethod('write',DoPrint);
  RegistMethod('writeln',DoPrintln);
  RegistMethod('alert',DoAlert);
  RegistMethod('prompt',DoPrompt);
  RegistMethod('confirm',DoConfirm);
  RegistMethod('textArea',DoTextArea);
  RegistMethod('exit',DoExit);
  RegistMethod('msgBox',DoMsgBox);
  RegistMethod('scriptEngine',DoScriptEngine);
  RegistMethod('scriptEngineBuildVersion',DoScriptEngineBuildVersion);
  RegistMethod('scriptEngineMajorVersion',DoScriptEngineMajorVersion);
  RegistMethod('scriptEngineMinorVersion',DoScriptEngineMinorVersion);
  RegistMethod('read',DoRead);
  RegistMethod('readln',DoReadln);
  RegistMethod('scriptEngineVersion',DoScriptEngineVersion);
  RegistMethod('isConsole',DoIsConsole);
  RegistMethod('decodeURI',DoDecodeURI);
  RegistMethod('decodeURIComponent',DoDecodeURIComponent);
  RegistMethod('encodeURI',DoEncodeURI);
  RegistMethod('encodeURIComponent',DoEncodeURIComponent);
  RegistMethod('format',DoFormat);
  RegistMethod('formatFloat',DoFormatFloat);
  RegistMethod('sprintf',DoFormat);
  RegistMethod('eventLoop',DoEventLoop);
  RegistMethod('terminate',DoTerminate);
  RegistMethod('printf',DoPrintf);
  RegistMethod('printfln',DoPrintfln);
  RegistMethod('doEvents',DoDoEvents);
  RegistMethod('sleep',DoSleep);
  RegistMethod('isNull',DoIsNull);
  RegistMethod('isUndefined',DoIsUndefined);
  RegistMethod('nameOf',DoNameOf);
end;

function TJGlobalObject.GetPlatform: String;
begin
  case Win32Platform of
    VER_PLATFORM_WIN32s: Result := 'win32s';
    VER_PLATFORM_WIN32_WINDOWS: Result := 'windows';
    VER_PLATFORM_WIN32_NT: Result := 'nt';
  else
    Result := 'unknown';
  end;
end;

procedure TJGlobalObject.Clear;
begin
  inherited;
end;

{ TJArrayObject }

function TJArrayObject.DoAppend(Param: TJValueList): TJValue;
var
  i: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
      Result := BuildInteger(Add(Param[i]));
end;

function TJArrayObject.DoClear(Param: TJValueList): TJValue;
//クリア
begin
  Result := BuildObject(Self);
  Clear;
end;

constructor TJArrayObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//作成
var
  i: Integer;
  v: TJValue;
begin
  inherited;
  RegistName('Array');

  FItems := TJValueList.Create;
  FSortArgs := TJValueList.Create;
  //2つ追加
  FSortArgs.Add(BuildNull);
  FSortArgs.Add(BuildNull);
  //登録
  if IsParam1(Param) then
  begin
    v := Param[0];
    //初期化する？
    //計算結果はDoubleになることが多いためDoubleも許可する
    if (Param.Count = 1) and IsNumber(@v) then
      length := AsInteger(@v)
    else //要素として追加
      for i := 0 to Param.Count - 1 do
        Add(Param[i]);
  end;
end;

function TJArrayObject.DoDelete(Param: TJValueList): TJValue;
//削除する
var
  i: Integer;
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v);
    try
      //削除
      Delete(i);
    except
      on EListError do
        raise EJThrow.Create(E_INDEX,'');
    end;
  end;
end;

destructor TJArrayObject.Destroy;
//破棄
begin
  Clear;
  FreeAndNil(FSortArgs);
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TJArrayObject.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJArrayObject.GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
var
  i: Integer;
begin
  if not ArrayStyle then
    Result := inherited GetValue(S,ArrayStyle)
  else begin
    try
      i := StrToInt(S);
      //配列を大きくする
      if (i + 1) > FItems.Count then
        FItems.Count := i + 1;

      Result := FItems[i];
    except
      on EConvertError do
        Result := inherited GetValue(S,ArrayStyle);
      on EListError do
        raise EJThrow.Create(E_INDEX,S);
    end;
  end;
end;

procedure TJArrayObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList = nil);
var
  i: Integer;
begin
  if ArrayStyle then
  begin
    try
      i := StrToInt(S);
      //配列を大きくする
      if (i + 1) > FItems.Count then
        FItems.Count := i + 1;

      FItems[i] := Value
    except
      on EConvertError do
        inherited SetValue(S,Value,ArrayStyle);
      on EListError do
        raise EJThrow.Create(E_INDEX,S);
    end;
  end
  else
    inherited SetValue(S,Value,ArrayStyle);;
end;

function TJArrayObject.DoSort(Param: TJValueList): TJValue;
//ソートする
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsConstructor(@v) and Assigned(FEngine) then
    begin
      FFunction := v.vFunction;
      FItems.Sort(ItemsSort2);
      //消す
      FSortArgs[0] := BuildNull;
      FSortArgs[1] := BuildNull;
    end
    else
      raise EJThrow.Create(E_TYPE,'sort compare function error');
  end
  else
    FItems.Sort(ItemsSort1);
end;

function TJArrayObject.ItemsSort1(Item1, Item2: Pointer): Integer;
//通常のソート
var
  p1,p2: PJValue;
begin
  p1 := Item1;
  p2 := Item2;
  Result := AsInteger(p1) - AsInteger(p2);
end;

function TJArrayObject.ItemsSort2(Item1, Item2: Pointer): Integer;
//関数つきソート
var
  p1,p2: PJValue;
  v: TJValue;
  engine: TJEngine;
begin
  engine := TJEngine(FEngine);
  p1 := Item1;
  p2 := Item2;
  FSortArgs[0] := p1^;
  FSortArgs[1] := p2^;
  v := engine.CallExpr(FFunction,FSortArgs,Self);
  Result := AsInteger(@v);
end;

function TJArrayObject.Add(Value: TJValue): Integer;
begin
  Result := FItems.Add(Value);
end;

procedure TJArrayObject.Clear;
//クリアする
begin
  inherited;
  FItems.Clear;
end;

function TJArrayObject.DoConCat(Param: TJValueList): TJValue;
//配列を連結する
var
  i,j: Integer;
  list: TJArrayObject;
  inlist: TJObject;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    list := TJArrayObject.Create(FEngine);
    Result := BuildObject(list);
    //自分を加える
    for i := 0 to FItems.Count - 1 do
      list.Add(FItems[i]);
    //追加を加える
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      //配列の場合は中身をコピー
      if IsArrayObject(@v) then
      begin
        inlist := v.vObject;
        for j := 0 to inlist.GetCount - 1 do
          list.Add(inlist.GetItem(j));
      end
      else
        list.Add(v);
    end;
  end;
end;

function TJArrayObject.DoJoin(Param: TJValueList): TJValue;
//連結した文字列を返す
var
  i: Integer;
  sep,s: String;
  v: TJValue;
begin
  sep := ',';
  s := '';

  if IsParam1(Param) then
  begin
    v := Param[0];
    sep := AsString(@v);
  end;
  //連結
  for i := 0 to FItems.Count - 1 do
  begin
    v := FItems[i];
    if i = 0 then
    begin
      if IsNull(@v) or IsUndefined(@v) then
      else
        s := AsString(@v);
    end
    else begin
      if IsNull(@v) or IsUndefined(@v) then
        s := s + sep
      else
        s := s + sep + AsString(@v);
    end;
  end;

  Result := BuildString(s);
end;

function TJArrayObject.DoPop(Param: TJValueList): TJValue;
//最後の値を返す
begin
  EmptyValue(Result);
  if FItems.Count > 0 then
  begin
    Result := FItems[FItems.Count - 1];
    Delete(FItems.Count - 1);
  end;
end;

function TJArrayObject.DoPush(Param: TJValueList): TJValue;
//加える
var
  i: Integer;
begin
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
      Add(Param[i]);

  Result := BuildInteger(FItems.Count);
end;

function TJArrayObject.DoReverse(Param: TJValueList): TJValue;
//反転する 新しいarrayは作成しない
var
  i,len,cnt,index: Integer;
  v: TJValue;
begin
  Result := BuildObject(Self);
  cnt := FItems.Count;
  len := cnt div 2;
  for i := 0 to len - 1 do
  begin
    v := FItems[i];
    index := cnt - i - 1;
    FItems[i] := FItems[index];
    FItems[index] := v;
  end;
end;

function TJArrayObject.DoShift(Param: TJValueList): TJValue;
//最初の値を返す
begin
  EmptyValue(Result);
  if FItems.Count > 0 then
  begin
    Result := FItems[0];
    Delete(0);
  end;
end;

function TJArrayObject.DoSlice(Param: TJValueList): TJValue;
//配列を抜き出す
var
  i,st,en: Integer;
  list: TJArrayObject;
  v: TJValue;
begin
  st := 0;
  en := MaxInt;
  list := TJArrayObject.Create(FEngine);
  Result := BuildObject(list);
  if IsParam1(Param) then
  begin
    v := Param[0];
    st := AsInteger(@v);
    if st < 0 then
      st := FItems.Count + st;
  end;

  if IsParam2(Param) then
  begin
    v := Param[1];
    en := AsInteger(@v);
    if en < 0 then
      en := FItems.Count + en;
  end;

  try
    for i := st to en - 1 do
      list.Add(FItems[i]);
  except
    on EListError do
      ;
  end;
end;

function TJArrayObject.DoSplice(Param: TJValueList): TJValue;
//値を挿入する
//start,count,v1,v2,v3...
var
  i,start,cnt: Integer;
  list: TJArrayObject;
  v: TJValue;
begin
  EmptyValue(Result);

  if IsParam1(Param) then
  begin
    v := Param[0];
    start := AsInteger(@v);
    //マイナスの場合は逆から
    if start < 0 then
      start := FItems.Count + start;

    if IsParam2(Param) then
    begin
      v := Param[1];
      cnt := AsInteger(@v);

      list := TJArrayObject.Create(FEngine);
      Result := BuildObject(list);

      //削除する
      cnt := start + cnt - 1;
      try
        for i := cnt downto start do
        begin
          //返値に挿入する
          list.Insert(0,FItems[i]);
          Delete(i);
        end;
      except
        on EListError do
          raise EJThrow.Create(E_INDEX,'');
      end;
      //残りの要素を追加する
      try
        for i := Param.Count - 1 downto 2 do
        begin
          v := Param[i];
          Insert(start,v);
        end;
      except
        on EListError do
          raise EJThrow.Create(E_INDEX,'');
      end;
    end;
  end;
end;

function TJArrayObject.DoUnShift(Param: TJValueList): TJValue;
//最初から挿入する
var
  i: Integer;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
    for i := Param.Count - 1 downto 0 do
      Insert(0,Param[i]);
end;

function TJArrayObject.ToString(Value: PJValue): String;
var
  v: TJValue;
begin
  v := DoJoin(nil);
  Result := AsString(@v);
end;

procedure TJArrayObject.SetLength(const Value: Integer);
begin
  FItems.Count := Value;
end;

procedure TJArrayObject.Delete(Index: Integer);
var
  v: TJValue;
begin
  try
    v := FItems[Index];
    FItems.Delete(Index);
  except
    on EListError do
      raise EJThrow.Create(E_INDEX,'');
  end;
end;

procedure TJArrayObject.Insert(Index: Integer; Value: TJValue);
begin
  FItems.Insert(Index,Value);
end;

function TJArrayObject.ValueOf: TJValue;
begin
  Result := DoJoin(nil);
end;

function TJArrayObject.GetItem(Index: Integer): TJValue;
begin
  Result := FItems[Index];
end;

function TJArrayObject.DoAssign(Param: TJValueList): TJValue;
var
  v:  TJValue;
  i: Integer;
  base: TJObject;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsArrayObject(@v) then
    begin
      //clear
      Clear;
      base := v.vObject;
      for i := 0 to base.GetCount - 1 do
        Add(base.GetItem(i));

      Result := BuildObject(Self);
    end
    else
      raise EJThrow.Create(E_TYPE,'array assign error');
  end
  else
    raise EJThrow.Create(E_TYPE,'array assign error');
end;

procedure TJArrayObject.RegistMethods;
begin
  inherited;
  RegistMethod('delete',DoDelete);
  RegistMethod('clear',DoClear);
  RegistMethod('add',DoAppend);
  RegistMethod('sort',DoSort);

  RegistMethod('concat',DoConCat);
  RegistMethod('join',DoJoin);
  RegistMethod('pop',DoPop);
  RegistMethod('push',DoPush);
  RegistMethod('reverse',DoReverse);
  RegistMethod('shift',DoShift);
  RegistMethod('slice',DoSlice);
  RegistMethod('splice',DoSplice);
  RegistMethod('toString',DoToString);
  RegistMethod('unshift',DoUnShift);
  RegistMethod('assign',DoAssign);
end;

class function TJArrayObject.IsArray: Boolean;
begin
  Result := True;
end;

{ TJStringObject }

constructor TJStringObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//文字列オブジェクト
var
  v: TJValue;
begin
  inherited;
  RegistName('String');
  if IsParam1(Param) then
  begin
    v := Param[0];
    FText := AsString(@v);
  end;
end;

function TJStringObject.DoAnchor(Param: TJValueList): TJValue;
//アンカーを返す
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString('<A NAME="' + AsString(@v) + '">' + FText + '</A>');
  end;
end;

function TJStringObject.DoBig(Param: TJValueList): TJValue;
begin
  Result := BuildString('<BIG>' + FText + '</BIG>');
end;

function TJStringObject.DoBlink(Param: TJValueList): TJValue;
begin
  Result := BuildString('<BLINK>' + FText + '</BLINK>');
end;

function TJStringObject.DoBold(Param: TJValueList): TJValue;
begin
  Result := BuildString('<B>' + FText + '</B>');
end;

function TJStringObject.DoCharAt(Param: TJValueList): TJValue;
//1文字を返す
var
  v: TJValue;
begin
  Result := BuildString('');
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(MBGetCharAt(FText,AsInteger(@v) + 1));
  end;
end;

function TJStringObject.DoCharCodeAt(Param: TJValueList): TJValue;
//文字コードを返す
var
  v: TJValue;
{$ifndef UNICODE}
  w: Word;
  s: String;
{$endif}
  ch: Char;
  index: Integer;
begin
  Result := BuildNaN;
  if IsParam1(Param) then
  begin
{$ifdef UNICODE}
    v := Param[0];
    index := AsInteger(@v) + 1;
    if (index >= 1) and (System.Length(FText) >= index) then
    begin
      ch := FText[index];
      Result := BuildInteger(Ord(ch));
    end;
{$else}
    s := MBGetCharAt(FText,AsInteger(@v) + 1);
    if s <> '' then
    begin
      w := Word(s[1]);
      //2バイト文字ならWordにする
      if system.Length(s) > 1 then
        w := (w shl 8) or Word(s[2]);
      Result := BuildInteger(w);
    end;
{$endif}
  end;
end;

function TJStringObject.DoConCat(Param: TJValueList): TJValue;
//文字列を結合する
var
  v: TJValue;
  i: Integer;
  s: String;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    s := FText;
    for i := 0 to Param.Count - 1 do
    begin
      v := param[i];
      s := s + AsString(@v);
    end;
    Result := BuildString(s);
  end;
end;

{$ifdef UNICODE}
{$else}
function TJStringObject.DoCrypt(Param: TJValueList): TJValue;
//ハッシュ
var
  v: TJValue;
  salt: String;
  c: TCrypt3;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    salt := AsString(@v);
  end
  else
    salt := '';

  c := TCrypt3.Create;
  try
    Result := BuildString(c.crypt(FText,salt));
  finally
   c.Free;
  end;
end;
{$endif}

function TJStringObject.DoFixed(Param: TJValueList): TJValue;
begin
  Result := BuildString('<TT>' + FText + '</TT>');
end;

function TJStringObject.DoFontColor(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString('<FONT COLOR="' + AsString(@v) + '">' + FText + '</FONT>');
  end;
end;

function TJStringObject.DoFontSize(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString('<FONT SIZE="' + AsString(@v) + '">' + FText + '</FONT>');
  end;
end;

function TJStringObject.DoFromCharCode(Param: TJValueList): TJValue;
//文字コードを文字に変換
var
  v: TJValue;
  i: Integer;
{$ifdef UNICODE}
  w: Integer;
  a: Word;
  b: Word;
{$else}
  w: Word;
{$endif}
  s: String;
begin
  EmptyValue(Result);
  s := '';
  if IsParam1(Param) then
  begin
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      w := AsInteger(@v);
{$ifdef UNICODE}
      if (w < $10000) then
        s := s + Char(w)
      else
      begin
        //  サロゲートペアと見なす
        w := w - $10000;
        a := $D800 + w div $400;
        b := $DC00 + w mod $400;
        s := s + Char(a) + Char(b);
      end;
{$else}
      if Hi(w) <> 0 then
        s := s + Char(Hi(w));
      s := s + Char(w);
{$endif}
    end;
    Result := BuildString(s);
  end;
end;

{$ifdef UNICODE}
{$else}
function TJStringObject.DoFromEUCToJIS(Param: TJValueList): TJValue;
begin
  Result := BuildString(euc2jis83(FText));
end;

function TJStringObject.DoFromEUCToSJIS(Param: TJValueList): TJValue;
begin
  Result := BuildString(euc2sjis(FText));
end;

function TJStringObject.DoFromJISToEUC(Param: TJValueList): TJValue;
begin
  Result := BuildString(jis2euc(FText));
end;

function TJStringObject.DoFromJISToSJIS(Param: TJValueList): TJValue;
begin
  Result := BuildString(jis2sjis(FText));
end;

function TJStringObject.DoFromSJISToEUC(Param: TJValueList): TJValue;
begin
  Result := BuildString(sjis2euc(FText));
end;

function TJStringObject.DoFromSJISToJIS(Param: TJValueList): TJValue;
begin
  Result := BuildString(sjis2jis83(FText));
end;

function TJStringObject.DoFromUtf8ToSJIS(Param: TJValueList): TJValue;
begin
  Result := BuildString(Utf8ToAnsi(FText));
end;
{$endif}

function TJStringObject.DoIndexOf(Param: TJValueList): TJValue;
//文字を検索
var
  v: TJValue;
  sub: String;
  start: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    sub := AsString(@v);
    //開始位置
    if IsParam2(Param) then
    begin
      v := Param[1];
      start := AsInteger(@v) + 1;
    end
    else
      start := 1;

    Result := BuildInteger(MBIndexOf(sub,FText,start) - 1);
  end;
end;

function TJStringObject.DoItalics(Param: TJValueList): TJValue;
begin
  Result := BuildString('<I>' + FText + '</I>');
end;

function TJStringObject.DoLastIndexOf(Param: TJValueList): TJValue;
//文字を逆から検索
var
  v: TJValue;
  sub: String;
  start: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    sub := AsString(@v);
    //開始位置
    if IsParam2(Param) then
    begin
      v := Param[1];
      start := AsInteger(@v) + 1;
    end
    else
      start := 0; //全部

    Result := BuildInteger(MBLastIndexOf(sub,FText,start) - 1);
  end;
end;

function TJStringObject.DoLink(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString('<A HREF="' + AsString(@v) + '">' + FText + '</A>');
  end;
end;

function TJStringObject.DoLocaleCompare(Param: TJValueList): TJValue;
//文字を比較する
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildInteger(AnsiCompareStr(FText,AsString(@v)));
  end;
end;

function TJStringObject.DoMatch(Param: TJValueList): TJValue;
//正規表現マッチング
var
  re: TJRegExpObject;
  v: TJValue;
  list: TJValueList;
begin
  Result := BuildNull;
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsRegExpObject(@v) then
      re := Param[0].vObject as TJRegExpObject
    else begin//正規表現objectではない場合は文字列とみなす
      re := TJRegExpObject.Create(FEngine,Param);
    end;

    re.IncRef;
    list := TJValueList.Create;
    //文字列
    list.Add(BuildString(FText));
    try
      Result := re.DoExec(list);
      re.DecRef;
    finally
      list.Free;
    end;
  end;
end;

//function TJStringObject.DoOrd(Param: TJValueList): TJValue;
//begin
//end;

function TJStringObject.DoReplace(Param: TJValueList): TJValue;
//正規表現置き換え
var
  re: TJRegExpObject;
  v,newstr: TJValue;
  list: TJValueList;
begin
  EmptyValue(Result);
  newstr := BuildString('');
  if IsParam1(Param) then
  begin
    v := Param[0];
    //第１引数が正規表現
    if IsRegExpObject(@v) then
    begin
      re := Param[0].vObject as TJRegExpObject;
      //置き換え
      if IsParam2(Param) then
        newstr := Param[1];
    end
    else begin//正規表現objectでない場合は文字列とみなす
      re := TJRegExpObject.Create(FEngine,Param);
      //第三があれば置き換え
      if IsParam3(Param) then
        newstr := Param[2]
      //第３が無い場合は２
      else if IsParam2(Param) then
        newstr := Param[1];
    end;

    re.IncRef;
    list := TJValueList.Create;
    //文字列
    list.Add(BuildString(FText));
    list.Add(newstr);
    try
      Result := re.DoReplace(list);
      re.DecRef;
    finally
      list.Free;
    end;
  end;
end;

function TJStringObject.DoReverse(Param: TJValueList): TJValue;
//文字列を反転
begin
  Result := BuildString(MBReverse(FText));
end;

function TJStringObject.DoSearch(Param: TJValueList): TJValue;
//正規表現サーチ
var
  re: TJRegExpObject;
  v: TJValue;
  list: TJValueList;
begin
  Result := BuildInteger(-1);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsRegExpObject(@v) then
      re := Param[0].vObject as TJRegExpObject
    else begin//正規表現objectでない場合は文字列とみなす
      re := TJRegExpObject.Create(FEngine,Param);
    end;

    re.IncRef;
    list := TJValueList.Create;
    //文字列
    list.Add(BuildString(FText));
    try
      v := re.DoExec(list);
      if IsObject(@v) then
        v.vObject.DecRef;
    finally
      list.Free;
    end;
    //返り値はindex
    Result := re.GetValue('index',False);
    re.DecRef;
  end;
end;

function TJStringObject.DoSizeOf(Param: TJValueList): TJValue;
begin
  Result := BuildInteger(system.Length(FText));
end;

function TJStringObject.DoSlice(Param: TJValueList): TJValue;
//文字列を部分コピー
var
  v: TJValue;
  start,last: Integer;
begin
  Result := BuildString('');
  if IsParam1(Param) then
  begin
    v := Param[0];
    start := AsInteger(@v) + 1;
    //last
    if IsParam2(Param) then
    begin
      v := Param[1];
      last := AsInteger(@v) + 1;
    end
    else
      last := MaxInt;

    Result := BuildString(MBSlice(FText,start,last));
  end;
end;

function TJStringObject.DoSmall(Param: TJValueList): TJValue;
begin
  Result := BuildString('<SMALL>' + FText + '</SMALL>');
end;

function TJStringObject.DoSplit(Param: TJValueList): TJValue;
//正規表現分割
var
  re: TJRegExpObject;
  v: TJValue;
  list: TJValueList;
  limit: Integer;
begin
  EmptyValue(Result);
  limit := MaxInt;
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsRegExpObject(@v) then
    begin
      re := Param[0].vObject as TJRegExpObject;
      if IsParam2(Param) then
      begin
        v := Param[1];
        //制限数
        limit := AsInteger(@v);
      end;
    end
    else begin//正規表現objectでない場合は文字列とみなす
      re := TJRegExpObject.Create(FEngine,Param);
      if IsParam2(Param) then
      begin
        v := Param[1];
        if ecma_type.TryAsNumber(@v) then
        begin
          limit := AsInteger(@v);
        end
        else if IsParam3(Param) then
        begin
          //２番目が数値ではなかったので３番目
          v := Param[2];
          limit := AsInteger(@v);
        end;
      end;
    end;

    re.IncRef;
    list := TJValueList.Create;
    //文字列
    list.Add(BuildString(FText));
    //制限数
    list.Add(BuildInteger(limit));
    try
      Result := re.DoSplit(list);
      re.DecRef;
    finally
      list.Free;
    end;
  end;
end;

function TJStringObject.DoStrike(Param: TJValueList): TJValue;
begin
  Result := BuildString('<STRIKE>' + FText + '</STRIKE>');
end;

function TJStringObject.DoSub(Param: TJValueList): TJValue;
begin
  Result := BuildString('<SUB>' + FText + '</SUB>');
end;

function TJStringObject.DoSubStr(Param: TJValueList): TJValue;
//文字列をコピー
var
  v: TJValue;
  start,len: Integer;
begin
  Result := BuildString('');
  if IsParam1(Param) then
  begin
    v := Param[0];
    start := AsInteger(@v) + 1;
    if IsParam2(Param) then
    begin
      v := Param[1];
      len := AsInteger(@v);
    end
    else
      len := MaxInt;

    Result := BuildString(MBCopy(FText,start,len));
  end;
end;

function TJStringObject.DoSubString(Param: TJValueList): TJValue;
//sliceとは少し違う
var
  v: TJValue;
  start,last,i,j: Integer;
begin
  Result := BuildString('');
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v) + 1;
    if i < 1 then
      i := 1;

    if IsParam2(Param) then
    begin
      v := Param[1];
      j := AsInteger(@v) + 1;
      if j < 1 then
        j := 1;
    end
    else
      j := MaxInt;

    start := Min(i,j);
    last  := Max(i,j);
    Result := BuildString(MBCopy(FText,start,last - start));
  end;
end;

function TJStringObject.DoSup(Param: TJValueList): TJValue;
begin
  Result := BuildString('<SUP>' + FText + '</SUP>');
end;

{$ifdef UNICODE}
{$else}
function TJStringObject.DoToEUC(Param: TJValueList): TJValue;
//文字をEUCに変換する
begin
  Result := BuildString(ConvertJCode(FText,EUC_OUT));
end;
{$endif}

function TJStringObject.DoToHankaku(Param: TJValueList): TJValue;
begin
  Result := BuildString(Hankaku(FText));
end;

function TJStringObject.DoToHiragana(Param: TJValueList): TJValue;
begin
  Result := BuildString(Hiragana(FText));
end;

{$ifdef UNICODE}
{$else}
function TJStringObject.DoToJIS(Param: TJValueList): TJValue;
//文字をJISに変換する
begin
  Result := BuildString(ConvertJCode(FText,JIS_OUT));
end;
{$endif}

function TJStringObject.DoToKatakana(Param: TJValueList): TJValue;
begin
  Result := BuildString(Katakana(FText));
end;

function TJStringObject.DoToLowerCase(Param: TJValueList): TJValue;
//小文字化
begin
  Result := BuildString(AnsiLowerCase(FText));
end;

{$ifdef UNICODE}
{$else}
function TJStringObject.DoToSJIS(Param: TJValueList): TJValue;
//文字をSJISに変換
begin
  Result := BuildString(ConvertJCode(FText,SJIS_OUT));
end;
{$endif}

function TJStringObject.DoToUpperCase(Param: TJValueList): TJValue;
//大文字化
begin
  Result := BuildString(AnsiUpperCase(FText));
end;

{$ifdef UNICODE}
{$else}
function TJStringObject.DoToUtf8(Param: TJValueList): TJValue;
//utf8に変換
begin
  Result := BuildString(AnsiToUtf8(FText));
end;

function TJStringObject.DoToWide(Param: TJValueList): TJValue;
//wide文字に変換
var
  p,w: PWideChar;
  len: Integer;
begin
  len := system.Length(FText) * 2 + 2;
  GetMem(p,len);
  try
    w := StringToWideChar(FText,p,len);
    // 変換されてる?
    Result := BuildString(PChar(w));
  finally
    FreeMem(p);
  end;
end;
{$endif}

function TJStringObject.DoToZenkaku(Param: TJValueList): TJValue;
begin
  Result := BuildString(Zenkaku(FText));
end;

function TJStringObject.DoTrim(Param: TJValueList): TJValue;
begin
  Result := BuildString(Trim(FText));
end;

function TJStringObject.DoTrimLeft(Param: TJValueList): TJValue;
begin
  Result := BuildString(TrimLeft(FText));
end;

function TJStringObject.DoTrimRight(Param: TJValueList): TJValue;
begin
  Result := BuildString(TrimRight(FText));
end;

function TJStringObject.DoMultiply(Param:TJValueList): TJValue;
//繰り返し連結
var
  v: TJValue;
  len,idx,cnt,i: Integer;
  buf: String;
begin
  buf := '';
  len := system.Length(FText);
  if IsParam1(Param) and (len > 0) then
  begin
    v := Param[0];
    cnt := AsInteger(@v);
    SetLength(buf,len * cnt);
    idx := 1;
    for i := 1 to cnt do
    begin
      Move(FText[1],buf[idx],len);
      Inc(idx,len);
    end;
  end;
  Result := BuildString(buf);
end;


function TJStringObject.GetCount: Integer;
//文字長を返す
begin
  Result := MBLength(FText);
end;

function TJStringObject.GetItem(Index: Integer): TJValue;
//文字を返す
begin
  Result := BuildString(MBGetCharAt(FText,Index + 1));
end;

function TJStringObject.GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
var
  index,len: Integer;
begin
  //配列式の場合
  if ArrayStyle then
  begin
    //paramがある場合はslice扱い
    if IsParam2(Param) then
      Result := DoSlice(Param)
    else begin
      try
        index := StrToInt(S);
        len := MBLength(FText);
        if index < 0 then
          index := len + index;

        try
          if (index >= 0) and (index < len) then
            Result := BuildString(MBGetCharAt(FText,index + 1))
          else
            raise EJThrow.Create(E_INDEX,S);
        except
            raise EJThrow.Create(E_INDEX,S);
        end;
      except
        on EConvertError do
          raise EJThrow.Create(E_INDEX,S);
      end;
    end;
  end
  else
    Result := inherited GetValue(S,ArrayStyle);
end;

class function TJStringObject.IsArray: Boolean;
begin
  Result := True;
end;

procedure TJStringObject.RegistMethods;
begin
  inherited;
  RegistMethod('charAt',DoCharAt);
  RegistMethod('anchor',DoAnchor);
  RegistMethod('big',DoBig);
  RegistMethod('blink',DoBlink);
  RegistMethod('bold',DoBold);
  RegistMethod('charCodeAt',DoCharCodeAt);
  RegistMethod('concat',DoConCat);
  RegistMethod('fixed',DoFixed);
  RegistMethod('fontcolor',DoFontColor);
  RegistMethod('fontsize',DoFontSize);
  RegistMethod('fromCharCode',DoFromCharCode);
  RegistMethod('indexOf',DoIndexOf);
  RegistMethod('italics',DoItalics);
  RegistMethod('lastIndexOf',DoLastIndexOf);
  RegistMethod('link',DoLink);
  RegistMethod('match',DoMatch);
  RegistMethod('replace',DoReplace);
  RegistMethod('search',DoSearch);
  RegistMethod('slice',DoSlice);
  RegistMethod('small',DoSmall);
  RegistMethod('split',DoSplit);
  RegistMethod('strike',DoStrike);
  RegistMethod('sub',DoSub);
  RegistMethod('substr',DoSubStr);
  RegistMethod('substring',DoSubString);
  RegistMethod('sup',DoSup);
  RegistMethod('localeCompare',DoLocaleCompare);
  RegistMethod('reverse',DoReverse);
  RegistMethod('multiply',DoMultiply);

  RegistMethod('toLowerCase',DoToLowerCase);
  RegistMethod('toUpperCase',DoToUpperCase);
{$ifdef UNICODE}
{$else}
  RegistMethod('toJIS',DoToJIS);
  RegistMethod('toSJIS',DoToSJIS);
  RegistMethod('toEUC',DoToEUC);
  RegistMethod('toWide',DoToWide);
  RegistMethod('toUTF8',DoToUtf8);
{$endif}
  RegistMethod('toString',DoToString);
  RegistMethod('toZenkaku',DoToZenkaku);
  RegistMethod('toHankaku',DoToHankaku);
  RegistMethod('toHiragana',DoToHiragana);
  RegistMethod('toKatakana',DoToKatakana);

{$ifdef UNICODE}
{$else}
  RegistMethod('fromJIStoSJIS',DoFromJISToSJIS);
  RegistMethod('fromJIStoEUC',DoFromJISToEUC);
  RegistMethod('fromEUCtoSJIS',DoFromEUCToSJIS);
  RegistMethod('fromEUCtoJIS',DoFromEUCToJIS);
  RegistMethod('fromSJIStoEUC',DoFromSJISToEUC);
  RegistMethod('fromSJIStoJIS',DoFromSJISToJIS);
  RegistMethod('fromUTF8toSJIS',DoFromUtf8ToSJIS);
  //unix des crypt
  RegistMethod('crypt',DoCrypt);
{$endif}
  //制御文字を消す
  RegistMethod('trim',DoTrim);
  RegistMethod('trimLeft',DoTrimLeft);
  RegistMethod('trimRight',DoTrimRight);

  RegistMethod('sizeOf',DoSizeOf);
end;

function TJStringObject.ReplaceRange(Param: TJValueList): TJValue;
//指定範囲を置換
//文字列を部分コピー
var
  start,last: Integer;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam3(Param) then
  begin
    v := Param[0];
    start := AsInteger(@v) + 1;
    //last
    v := Param[1];
    last := AsInteger(@v) + 1;
    //source
    v := Param[2];
    MBReplace(AsString(@v),FText,start,last);
  end;
end;

procedure TJStringObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList = nil);
var
  index,len: Integer;
begin
  //配列式の場合
  if ArrayStyle then
  begin
    //paramがある場合は範囲置換
    if IsParam2(Param) then
    begin
      //valueを加える
      Param.Add(Value);
      ReplaceRange(Param);
    end
    else begin
      try
        index := StrToInt(S);
        len := MBLength(FText);
        if index < 0 then
          index := len + index;

        try
          //置換
          if (index >= 0) and (index < len) then
            MBSetCharAt(AsString(@Value),FText,index + 1)
          else
            raise EJThrow.Create(E_INDEX,S);
        except
            raise EJThrow.Create(E_INDEX,S);
        end;
      except
        on EConvertError do
          raise EJThrow.Create(E_INDEX,S);
      end;
    end;
  end
  else
    inherited;
end;

function TJStringObject.ToBool: Boolean;
var
  v: TJValue;
begin
  v := BuildString(FText);
  Result := AsBool(@v);
end;

function TJStringObject.ToChar: Char;
var
  v: TJValue;
begin
  v := BuildString(FText);
  Result := ecma_type.AsChar(@v);
end;

function TJStringObject.ToNumber: Double;
var
  v: TJValue;
begin
  v := BuildString(FText);
  Result := AsDouble(@v);
end;

function TJStringObject.ToString(Value: PJValue): String;
begin
  Result := FText;
end;

function TJStringObject.ValueOf: TJValue;
begin
  Result := BuildString(FText);
end;

{ TJNumberObject }

constructor TJNumberObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//数値オブジェクト作成
begin
  inherited;
  RegistName('Number');
  if IsParam1(Param) then
  begin
    FValue := Param[0];
    //数字じゃない場合は 0
    if not ecma_type.TryAsNumber(@FValue) then
      FValue := BuildInteger(0);
  end
  else
    FValue := BuildInteger(0);
end;

function TJNumberObject.DoToChar(Param: TJValueList): TJValue;
begin
  Result := BuildString(ToChar);
end;

function TJNumberObject.DoToExponential(Param: TJValueList): TJValue;
var
  i: Integer;
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v) + 1;
  end
  else
    i := 0;

  Result := BuildString(FloatToStrF(AsDouble(@FValue),ffExponent,i,1));
end;

function TJNumberObject.DoToFixed(Param: TJValueList): TJValue;
var
  i: Integer;
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v);
  end
  else
    i := 0;

  Result := BuildString(FloatToStrF(AsDouble(@FValue),ffFixed,15,i));
end;

function TJNumberObject.DoToPrecision(Param: TJValueList): TJValue;
var
  i: Integer;
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v);
  end
  else
    i := 0;

  Result := BuildString(FloatToStrF(AsDouble(@FValue),ffFixed,15,i));
end;

function TJNumberObject.GetInt: Integer;
begin
  Result := AsInteger(@FValue);
end;

function TJNumberObject.GetNumber: Double;
begin
  Result := AsDouble(@FValue);
end;

function TJNumberObject.GetAsChar: String;
begin
  Result := ToChar;
end;

procedure TJNumberObject.RegistMethods;
begin
  inherited;
  RegistMethod('toString',DoToString);
  RegistMethod('toExponential',DoToExponential);
  RegistMethod('toFixed',DoToFixed);
  RegistMethod('toPrecision',DoToPrecision);
  RegistMethod('toChar',DoToChar);
end;

procedure TJNumberObject.RegistProperties;
//プロパティ登録
var
  v: TJValue;
begin
  v.ValueType := vtNaN;
  RegistProperty('NaN',v);
  v.ValueType := vtDouble;
  v.vDouble := 1.7e+308;
  RegistProperty('MAX_VALUE',v);
  v.ValueType := vtDouble;
  v.vDouble := 5.0e-324;
  RegistProperty('MIN_VALUE',v);
  v := BuildInfinity(False);
  RegistProperty('POSITIVE_INFINITY',v);
  v := BuildInfinity(True);
  RegistProperty('NEGATIVE_INFINITY',v);
end;

procedure TJNumberObject.SetInt(const Value: Integer);
begin
  FValue := BuildInteger(Value);
end;

procedure TJNumberObject.SetNumber(const Value: Double);
begin
  FValue := BuildDouble(Value);
end;

procedure TJNumberObject.SetAsChar(const Value: String);
begin
  if system.Length(Value) > 0 then
    FValue := BuildInteger(Ord(Value[1]))
  else
    FValue := BuildInteger(0);
end;

function TJNumberObject.ToBool: Boolean;
begin
  Result := AsBool(@FValue);
end;

function TJNumberObject.ToChar: Char;
begin
  Result := ecma_type.AsChar(@FValue);
end;

function TJNumberObject.ToNumber: Double;
begin
  Result := AsDouble(@FValue);
end;

function TJNumberObject.ToString(Value: PJValue): String;
begin
  case AsInteger(Value) of
    16: Result := IntToHex(AsInteger(@FValue),1);
     8: Result := IntToOctStr(AsInteger(@FValue));
     2: Result := IntToBitStr(AsInteger(@FValue));
  else
    Result := AsString(@FValue);
  end;
end;

function TJNumberObject.ValueOf: TJValue;
begin
  Result := FValue;
end;

{ TJBooleanObject }

constructor TJBooleanObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('Boolean');
  //RegistMethod('toString',DoToString);
  if IsParam1(Param) then
  begin
    v := Param[0];
    FBool := AsBool(@v);
  end;
end;

function TJBooleanObject.ToBool: Boolean;
begin
  Result := FBool;
end;

function TJBooleanObject.ToChar: Char;
var
  v: TJValue;
begin
  v := BuildBool(FBool);
  Result := ecma_type.AsChar(@v);
end;

function TJBooleanObject.ToNumber: Double;
var
  v: TJValue;
begin
  v := BuildBool(FBool);
  Result := AsDouble(@v);
end;

function TJBooleanObject.ToString(Value: PJValue): String;
var
  v: TJValue;
begin
  v := BuildBool(FBool);
  Result := AsString(@v);
end;

function TJBooleanObject.ValueOf: TJValue;
begin
  Result := BuildBool(FBool);
end;

{ TJRegExpObject }

procedure TJRegExpObject.Assign(Source: TPersistent);
begin
  if Source is TJRegExpObject then
  begin
    FRegExp.Assign((Source as TJRegExpObject).FRegExp);
    RegistSubMatch;
  end
  else
    inherited;
end;

procedure TJRegExpObject.Clear;
begin
  inherited;
  ClearMatch;
end;

procedure TJRegExpObject.ClearMatch;
//matchをクリアする
var
  i: Integer;
  v: TJValue;
begin
  v := BuildString('');
  for i := 0 to 9 do
    RegistProperty('$' + IntToStr(i),v);

  FRegExp.ClearMatched;
end;

constructor TJRegExpObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//作成する
var
  v: TJValue;
  s: String;
begin
  inherited;
  RegistName('RegExp');

  FRegExp := TJRegExp.Create;
  //$1 - $9を登録
  ClearMatch;

  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsRegExp(@v) then
      SetRegExpValue(v)
    else begin
      try
        FRegExp.Source := AsString(@v);
      except
      end;

      if IsParam2(Param) then
      begin
        v := Param[1];
        s := AsString(@v);
        //optionは igm
        if Length(s) <= 3 then
        begin
          FRegExp.IgnoreCase :=  Pos('i',s) > 0;
          FRegExp.Global := Pos('g',s) > 0;
          FRegExp.MultiLine :=  Pos('m',s) > 0;
        end;
      end;
    end;
  end;
end;

destructor TJRegExpObject.Destroy;
//破棄
begin
  FreeAndNil(FReplaceParam);
  FreeAndNil(FRegExp);
  inherited Destroy;
end;

function TJRegExpObject.DoExec(Param: TJValueList): TJValue;
//正規表現マッチングを実行する
var
  v: TJValue;
  list: TJArrayObject;
  i: Integer;
  sl: TStringList;
  inpt: String;
begin
  Result := BuildNull;
  ClearMatch;
  //開始イベント
  if Assigned(FOnMatchStart) then
    FOnMatchStart(Self);

  if IsParam1(Param) then
  begin
    v := Param[0];
    inpt := AsString(@v);
  end
  else begin
    //文字列を取ってくる
    inpt := '';
    if Assigned(FOnExecInput) then
      FOnExecInput(Self,inpt);
  end;

  sl := TStringList.Create;
  try
    //マッチ成功ならば
    if FRegExp.Exec(inpt,sl) then
    begin
      //返値の配列
      list := TJArrayObject.Create(FEngine);
      Result := BuildObject(list);
      for i := 0 to sl.Count - 1 do
        list.Items.Add(sl[i]);

      //$をセット
      RegistSubMatch;
      //成功イベント
      if Assigned(FOnMatchEnd) then
        FOnMatchEnd(Self);
    end;
  finally
    sl.Free;
  end;
end;

function TJRegExpObject.FuncReplace(RE: TJRegExp;
  Matched: String): String;
//置換関数
var
  eng: TJEngine;
  i: Integer;
  v: TJValue;
begin
  if not Assigned(FReplaceParam) then
    FReplaceParam := TJValueList.Create;
  try
    //サブマッチ
    for i := 0 to Length(RE.SubMatch) - 1 do
      FReplaceParam.Add(RE.Submatch[i]);
    //index
    FReplaceParam.Add(RE.Index);
    //元の文字列
    FReplaceParam.Add(RE.Input);
    //関数を実行
    eng := TJEngine(FEngine);
    v := eng.CallExpr(FReplaceFunc,FReplaceParam,Self);
    Result := AsString(@v);
  finally
    //使い終わったらクリア
    FReplaceParam.Clear;
  end;
end;

function TJRegExpObject.DoReplace(Param: TJValueList): TJValue;
//置き換える
var
  inpt,rep: TJValue;
begin
  Result := BuildString('');
  FReplaceFunc := nil;
  if IsParam1(Param) then
  begin
    //submatchを更新するためにtest()
    DoTest(Param);
    //input
    inpt := Param[0];
    if IsParam2(Param) then
      rep := Param[1]
    else
      rep := BuildString('');
    try
      //置換関数呼び出し
      if IsFunction(@rep) and Assigned(FEngine) then
      begin
        FReplaceFunc := rep.vFunction;
        Result := BuildString(
          FRegExp.Replace(
            AsString(@inpt),'',FuncReplace));
      end
      else
        Result := BuildString(
          FRegExp.Replace(
            AsString(@inpt),AsString(@rep)));
    finally
      FReplaceFunc := nil;
    end;
  end;
end;

function TJRegExpObject.DoSplit(Param: TJValueList): TJValue;
//分割する
var
  v: TJValue;
  inp: String;
  sl: TStringList;
  list: TJArrayObject;
  i: Integer;
begin
  list := TJArrayObject.Create(FEngine);
  Result := BuildObject(list);
  if IsParam1(Param) then
  begin
    v := Param[0];
    inp := AsString(@v);

    sl := TStringList.Create;
    try
      if IsParam2(Param) then
      begin
        v := Param[1];
        FRegExp.Split(inp,sl,AsInteger(@v));
      end
      else
        FRegExp.Split(inp,sl);

      for i := 0 to sl.Count - 1 do
        list.Items.Add(sl[i]);
    finally
      sl.Free;
    end;
  end;
end;

function TJRegExpObject.DoTest(Param: TJValueList): TJValue;
//正規表現マッチングをテスト
begin
  Result := BuildBool(False);
  if IsParam1(Param) then
    Result := BuildBool(Test(Param[0]));
end;

function TJRegExpObject.GetGlobal: Boolean;
begin
  Result := FRegExp.global;
end;

function TJRegExpObject.GetIgnoreCase: Boolean;
//ignorecase
begin
  Result := FRegExp.ignoreCase;
end;

function TJRegExpObject.GetIndex: Integer;
begin
  Result := FRegExp.index;
end;

function TJRegExpObject.GetInput: String;
begin
  Result := FRegExp.input;
end;

function TJRegExpObject.GetLastIndex: Integer;
begin
  Result := FRegExp.lastIndex;
end;

function TJRegExpObject.GetLastMatch: String;
begin
  Result := FRegExp.lastMatch;
end;

function TJRegExpObject.GetLastParen: String;
begin
  Result := FRegExp.lastParen;
end;

function TJRegExpObject.GetLeftContext: String;
begin
  Result := FRegExp.leftContext;
end;

function TJRegExpObject.GetMultiLine: Boolean;
begin
  Result := FRegExp.multiline;
end;

function TJRegExpObject.GetRightContext: String;
begin
  Result := FRegExp.rightContext;
end;

function TJRegExpObject.GetSource: String;
begin
  Result := FRegExp.Source;
end;

procedure TJRegExpObject.RegistMethods;
begin
  inherited;
  RegistMethod('exec',DoExec);
  RegistMethod('test',DoTest);
  RegistMethod('replace',DoReplace);
  RegistMethod('split',DoSplit);
end;

procedure TJRegExpObject.RegistSubMatch;
//submatchを登録
var
  i: Integer;
  v: TJValue;
begin
  for i := 0 to Length(FRegExp.SubMatch) - 1 do
  begin
    v := BuildString(FRegExp.SubMatch[i]);
    RegistProperty('$' + IntToStr(i),v);
  end;
end;

procedure TJRegExpObject.SetGlobal(const Value: Boolean);
begin
  FRegExp.global := Value;
end;

procedure TJRegExpObject.SetIgnoreCase(const Value: Boolean);
begin
  FRegExp.IgnoreCase := Value;
end;

procedure TJRegExpObject.SetMultiLine(const Value: Boolean);
begin
  FRegExp.MultiLine := Value;
end;

procedure TJRegExpObject.SetRegExpValue(var Value: TJValue);
//セット
begin
  FRegExp.source := Value.vString;
  if IsRegExp(@Value) then
  begin
    FRegExp.multiline := Pos('m',Value.vRegExpOptions) > 0;
    FRegExp.global := Pos('g',Value.vRegExpOptions) > 0;
    FRegExp.ignoreCase := Pos('i',Value.vRegExpOptions) > 0;
  end;
end;

procedure TJRegExpObject.SetSource(const Value: String);
begin
  FRegExp.Source := Value;
end;

function TJRegExpObject.Test(Value: TJValue): Boolean;
//正規表現マッチングをテスト
begin
  ClearMatch;
  //開始イベント
  if Assigned(FOnMatchStart) then
    FOnMatchStart(Self);

  Result := FRegExp.Test(AsString(@Value));
  //$をセット
  RegistSubMatch;
  //成功イベント
  if Assigned(FOnMatchEnd) then
    FOnMatchEnd(Self);
end;

procedure TJRegExpObject.SetInput(const Value: String);
begin
  FRegExp.input := Value;
end;

function TJRegExpObject.ToString(Value: PJValue): String;
begin
  Result := '/' + FRegExp.Source + '/';
  if FRegExp.ignoreCase then Result := Result + 'i';
  if FRegExp.global     then Result := Result + 'g';
  if FRegExp.multiline  then Result := Result + 'm';
end;

{ TJMathObject }

constructor TJMathObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//作成
begin
  inherited;
  RegistName('Math');

  RegistMethod('abs',DoAbs);
  RegistMethod('exp',DoExp);
  RegistMethod('log',DoLog);
  RegistMethod('sqrt',DoSqrt);
  RegistMethod('ceil',DoCeil);
  RegistMethod('floor',DoFloor);
  RegistMethod('round',DoRound);
  RegistMethod('sin',DoSin);
  RegistMethod('cos',DoCos);
  RegistMethod('tan',DoTan);
  RegistMethod('asin',DoAsin);
  RegistMethod('acos',DoACos);
  RegistMethod('atan',DoAtan);
  RegistMethod('atan2',DoAtan2);
  RegistMethod('max',DoMax);
  RegistMethod('min',DoMin);
  RegistMethod('pow',DoPow);
  RegistMethod('random',DoRandom);

  Randomize;
end;

function TJMathObject.DoAbs(Param: TJValueList): TJValue;
//絶対値
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsDouble(@v) then
      Result := BuildDouble(system.Abs(AsDouble(@v)))
    else
      Result := BuildInteger(system.Abs(AsInteger(@v)));
  end;
end;

function TJMathObject.DoAcos(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(math.ArcCos(AsDouble(@v)));
  end;
end;

function TJMathObject.DoAsin(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(math.ArcSin(AsDouble(@v)));
  end;
end;

function TJMathObject.DoAtan(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(system.ArcTan(AsDouble(@v)));
  end;
end;

function TJMathObject.DoAtan2(Param: TJValueList): TJValue;
var
  v1,v2: TJValue;
begin
  EmptyValue(Result);
  if IsParam2(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    Result := BuildDouble(math.ArcTan2(AsDouble(@v1),AsDouble(@v2)));
  end;
end;

function TJMathObject.DoCeil(Param: TJValueList): TJValue;
//切り上げ
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildInteger(math.Ceil(AsDouble(@v)));
  end;
end;

function TJMathObject.DoCos(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(system.Cos(AsDouble(@v)));
  end;
end;

function TJMathObject.DoExp(Param: TJValueList): TJValue;
//指数関数
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(system.Exp(AsDouble(@v)));
  end;
end;

function TJMathObject.DoFloor(Param: TJValueList): TJValue;
//切り下げ
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildInteger(math.Floor(AsDouble(@v)));
  end;
end;

function TJMathObject.DoLog(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(system.Ln(AsDouble(@v)));
  end;
end;

function TJMathObject.DoMax(Param: TJValueList): TJValue;
//大きい
var
  v: TJValue;
  i: Integer;
  max,d: Double;
begin
  Result := BuildNaN;
  if IsParam1(Param) then
  begin
    v := Param[0];
    if not TryAsNumber(@v) then
      Exit;
    max := AsDouble(@v);
    for i := 1 to Param.Count - 1 do
    begin
      v := Param[i];
      if not TryAsNumber(@v) then
        Exit;
      d := AsDouble(@v);
      if d > max then
        max := d;
    end;
    Result := BuildDouble(max);
  end
  else begin
    //引数なしのときはNEGATIVE_INFINITY
    Result := BuildInfinity(True);
  end;
end;

function TJMathObject.DoMin(Param: TJValueList): TJValue;
//小さい
var
  v: TJValue;
  i: Integer;
  min,d: Double;
begin
  Result := BuildNaN;
  if IsParam1(Param) then
  begin
    v := Param[0];
    if not TryAsNumber(@v) then
      Exit;
    min := AsDouble(@v);
    for i := 1 to Param.Count - 1 do
    begin
      v := Param[i];
      if not TryAsNumber(@v) then
        Exit;
      d := AsDouble(@v);
      if d < min then
        min := d;
    end;
    Result := BuildDouble(min);
  end
  else begin
    //引数なしのときはPOSITIVE_INFINITY
    Result := BuildInfinity(False);
  end;
end;

function TJMathObject.DoPow(Param: TJValueList): TJValue;
//累乗
var
  v1,v2: TJValue;
begin
  EmptyValue(Result);
  if IsParam2(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    Result := BuildDouble(math.Power(AsDouble(@v1),AsDouble(@v2)));
  end;
end;

function TJMathObject.DoRandom(Param: TJValueList): TJValue;
//乱数
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildInteger(system.Random(AsInteger(@v)));
  end
  else
    Result := BuildDouble(system.Random);
end;

function TJMathObject.DoRound(Param: TJValueList): TJValue;
//丸める
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildInteger(system.Round(AsDouble(@v)));
  end;
end;

function TJMathObject.DoSin(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(system.Sin(AsDouble(@v)));
  end;
end;

function TJMathObject.DoSqrt(Param: TJValueList): TJValue;
//平方根
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(system.Sqrt(AsDouble(@v)));
  end;
end;

function TJMathObject.DoTan(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildDouble(math.Tan(AsDouble(@v)));
  end;
end;

function TJMathObject.GetE: Double;
//自然対数の底
begin
  Result := system.Exp(1);
end;

function TJMathObject.GetLN10: Double;
begin
  Result := system.Ln(10);
end;

function TJMathObject.GetLN2: Double;
begin
  Result := system.Ln(2);
end;

function TJMathObject.GetLOG10E: Double;
begin
  Result := 1 / system.Ln(10);
end;

function TJMathObject.GetLOG2E: Double;
begin
  Result := 1 / system.Ln(2);
end;

function TJMathObject.GetPI: Double;
begin
  Result := system.Pi;
end;

function TJMathObject.GetSQRT1_2: Double;
begin
  Result := system.Sqrt(0.5);
end;

function TJMathObject.GetSQRT2: Double;
begin
  Result := system.Sqrt(2);
end;

{ TJDateObject }

constructor TJDateObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
//作成
var
  v: TJValue;
  time: TECMATime;
begin
  inherited;
  RegistName('Date');

  //メソッド登録
  RegistMethod('getFullYear',DoGetFullYear);
  RegistMethod('setFullYear',DoSetFullYear);
  RegistMethod('getYear',DoGetYear);
  RegistMethod('setYear',DoSetYear);
  RegistMethod('getMonth',DoGetMonth);
  RegistMethod('setMonth',DoSetMonth);
  RegistMethod('getDate',DoGetDate);
  RegistMethod('setDate',DoSetDate);
  RegistMethod('getDay',DoGetDay);
  RegistMethod('getHours',DoGetHours);
  RegistMethod('setHours',DoSetHours);
  RegistMethod('getMinutes',DoGetMinutes);
  RegistMethod('setMinutes',DoSetMinutes);
  RegistMethod('getSeconds',DoGetSeconds);
  RegistMethod('setSeconds',DoSetSeconds);
  RegistMethod('getMilliseconds',DoGetMilliSeconds);
  RegistMethod('setMilliseconds',DoSetMilliSeconds);

  RegistMethod('getUTCFullYear',DoGetUTCFullYear);
  RegistMethod('setUTCFullYear',DoSetUTCFullYear);
  RegistMethod('getUTCYear',DoGetUTCYear);
  RegistMethod('setUTCYear',DoSetUTCYear);
  RegistMethod('getUTCMonth',DoGetUTCMonth);
  RegistMethod('setUTCMonth',DoSetUTCMonth);
  RegistMethod('getUTCDate',DoGetUTCDate);
  RegistMethod('setUTCDate',DoSetUTCDate);
  RegistMethod('getUTCDay',DoGetUTCDay);
  RegistMethod('getUTCHours',DoGetUTCHours);
  RegistMethod('setUTCHours',DoSetUTCHours);
  RegistMethod('getUTCMinutes',DoGetUTCMinutes);
  RegistMethod('setUTCMinutes',DoSetUTCMinutes);
  RegistMethod('getUTCSeconds',DoGetUTCSeconds);
  RegistMethod('setUTCSeconds',DoSetUTCSeconds);
  RegistMethod('getUTCMilliseconds',DoGetUTCMilliSeconds);
  RegistMethod('setUTCMilliseconds',DoSetUTCMilliSeconds);

  RegistMethod('getTime',DoGetTime);
  RegistMethod('setTime',DoSetTime);
  RegistMethod('getTimezoneOffset',DoGetTimezoneOffSet);
  RegistMethod('toString',DoToString);
  RegistMethod('toLocaleString',DoToLocaleString);
  RegistMethod('toGMTString',DoToGMTString);
  RegistMethod('toUTCString',DoToUTCString);
  RegistMethod('UTC',DoUTC);
  RegistMethod('parse',DoParse);
  RegistMethod('valueOf',DoGetTime);

  //現在時間
  LocalTime := Now;
  //初期値
  if IsParam1(Param) then
  begin
    if Param.Count = 1 then
    begin
      v := Param[0];
      //数値以外の場合は解析
      if not ecma_type.TryAsNumber(@v) then
        v := DoParse(Param);
      //GMTに変換
      time := Trunc(AsDouble(@v));
      UTC := ECMATimeToDateTime(time);
    end
    else if Param.Count > 1 then
    begin
      //解析していれる
      v := DoParse(Param);
      //GMTに変換
      time := Trunc(AsDouble(@v));
      UTC := ECMATimeToDateTime(time);
    end;
  end;
end;

function TJDateObject.DoGetDate(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(LocalTime,y,m,d);
  Result := BuildInteger(d);
end;

function TJDateObject.DoGetDay(Param: TJValueList): TJValue;
begin
  Result := BuildInteger(DayOfWeek(LocalTime) - 1);
end;

function TJDateObject.DoGetFullYear(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(Localtime,y,m,d);
  Result := BuildInteger(y);
end;

function TJDateObject.DoGetHours(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(Localtime,ho,mi,se,ms);
  Result := BuildInteger(ho);
end;

function TJDateObject.DoGetMilliSeconds(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(Localtime,ho,mi,se,ms);
  Result := BuildInteger(ms);
end;

function TJDateObject.DoGetMinutes(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(Localtime,ho,mi,se,ms);
  Result := BuildInteger(mi);
end;

function TJDateObject.DoGetMonth(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(Localtime,y,m,d);
  Result := BuildInteger(m - 1);
end;

function TJDateObject.DoGetSeconds(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(Localtime,ho,mi,se,ms);
  Result := BuildInteger(se);
end;

function TJDateObject.DoGetTime(Param: TJValueList): TJValue;
begin
  Result := BuildDouble(DateTimeToECMATime(UTC));
end;

function TJDateObject.DoGetTimezoneOffset(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(GetTimeZone,ho,mi,se,ms);
  Result := BuildInteger(0 - (ho * 60));
end;

function TJDateObject.DoGetUTCDate(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(UTC,y,m,d);
  Result := BuildInteger(d);
end;

function TJDateObject.DoGetUTCDay(Param: TJValueList): TJValue;
begin
  Result := BuildInteger(DayOfWeek(UTC) - 1);
end;

function TJDateObject.DoGetUTCFullYear(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(UTC,y,m,d);
  Result := BuildInteger(y);
end;

function TJDateObject.DoGetUTCHours(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(UTC,ho,mi,se,ms);
  Result := BuildInteger(ho);
end;

function TJDateObject.DoGetUTCMilliSeconds(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(UTC,ho,mi,se,ms);
  Result := BuildInteger(ms);
end;

function TJDateObject.DoGetUTCMinutes(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(UTC,ho,mi,se,ms);
  Result := BuildInteger(mi);
end;

function TJDateObject.DoGetUTCMonth(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(UTC,y,m,d);
  Result := BuildInteger(m - 1);
end;

function TJDateObject.DoGetUTCSeconds(Param: TJValueList): TJValue;
var
  ho,mi,se,ms: Word;
begin
  DecodeTime(UTC,ho,mi,se,ms);
  Result := BuildInteger(se);
end;

function TJDateObject.DoGetUTCYear(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(UTC,y,m,d);
  if y < 2000 then
    Dec(y,1900);

  Result := BuildInteger(y - 1900);
end;

function TJDateObject.DoGetYear(Param: TJValueList): TJValue;
var
  y,m,d: Word;
begin
  DecodeDate(LocalTime,y,m,d);
  if y < 2000 then
    Dec(y,1900);

  Result := BuildInteger(y);
end;

function TJDateObject.DoParse(Param: TJValueList): TJValue;
//日付文字列を解析して 1970からの秒数を返す
var
  s: String;
  v: TJValue;
  y,m,d,ho,mi,se,ms: Word;
  date: TDateTime;
begin
  //現在時間
  date := GMTNow;
  Result := BuildDouble(DateTimeToECMATime(date));
  if not IsParam1(Param) then
    Exit;

  if Param.Count = 1 then
  begin
    //１つの文字列
    v := Param[0];
    s := AsString(@v);
    Result := BuildDouble(DateTimeToECMATime(DateParse(s)));
  end
  else begin
    //複数の数字
    DecodeDate(date,y,m,d);
    DecodeTime(date,ho,mi,se,ms);

    try
      v := Param[0];
      y := AsInteger(@v);
      v := Param[1];
      //月は１減らす
      m := AsInteger(@v) - 1;
      v := Param[2];
      d := AsInteger(@v);
      v := Param[3];
      ho := AsInteger(@v);
      v := Param[4];
      mi := AsInteger(@v);
      v := Param[5];
      se := AsInteger(@v);
      v := Param[6];
      ms := AsInteger(@v);
    except
      on EListError do
    end;

    try
      date := EncodeDate(y,m,d);
      date := date + EncodeTime(ho,mi,se,ms);
    except
      on EConvertError do
    end;

    Result := BuildDouble(DateTimeToECMATime(date));
  end;

end;

function TJDateObject.DoSetDate(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    d := AsInteger(@v);
    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetFullYear(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    y := AsInteger(@v);
    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetHours(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    ho := AsInteger(@v);
    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetMilliSeconds(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    ms := AsInteger(@v);
    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetMinutes(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    mi := AsInteger(@v);
    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetMonth(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    m := AsInteger(@v);
    try
      //月は１を足す
      LocalTime := EncodeDate(y,m + 1,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetSeconds(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    se := AsInteger(@v);
    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetTime(Param: TJValueList): TJValue;
var
  v: TJValue;
  time: TECMATime;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    time := AsDouble(@v);
    //UTCで入れる
    UTC := ECMATimeToDateTime(time);
    Result := DoGetTime(nil);
  end;
end;

function TJDateObject.DoSetUTCDate(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    d := AsInteger(@v);
    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCFullYear(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    y := AsInteger(@v);
    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCHours(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    ho := AsInteger(@v);
    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCMilliSeconds(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    ms := AsInteger(@v);
    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCMinutes(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    mi := AsInteger(@v);
    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCMonth(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    m := AsInteger(@v);
    try
      //１を足す
      UTC := EncodeDate(y,m + 1,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCSeconds(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    se := AsInteger(@v);
    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetUTCYear(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(UTC,y,m,d);
    DecodeTime(UTC,ho,mi,se,ms);

    v := Param[0];
    y := AsInteger(@v);
    //補正
    if (y < 100) then
      Inc(y,1900);

    try
      UTC := EncodeDate(y,m,d);
      UTC := UTC + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoSetYear(Param: TJValueList): TJValue;
var
  y,m,d,ho,mi,se,ms: Word;
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    DecodeDate(LocalTime,y,m,d);
    DecodeTime(LocalTime,ho,mi,se,ms);

    v := Param[0];
    y := AsInteger(@v);
    //補正
    if (y < 100) then
      Inc(y,1900);

    try
      LocalTime := EncodeDate(y,m,d);
      LocalTime := LocalTime + EncodeTime(ho,mi,se,ms);
      Result := DoGetTime(nil);
    except
      on EConvertError do
        ;
    end;
  end;
end;

function TJDateObject.DoToGMTString(Param: TJValueList): TJValue;
var
  s: String;
begin
  //Result := BuildString(DateTimeToStr(UTC));
  DateTimeToString(s,FFormat,UTC);
  Result := BuildString(s);
end;

function TJDateObject.DoToLocaleString(Param: TJValueList): TJValue;
begin
  Result := BuildString(ToString);
end;

function TJDateObject.DoToUTCString(Param: TJValueList): TJValue;
var
  s: String;
begin
  //Result := BuildString(DateTimeToStr(UTC));
  DateTimeToString(s,FFormat,UTC);
  Result := BuildString(s);
end;

function TJDateObject.DoUTC(Param: TJValueList): TJValue;
begin
  Result := DoParse(Param);
end;

function TJDateObject.GetLocal: TDateTime;
//Localに変換
begin
  Result := GMTToLocalDateTime(FDate);
end;

function TJDateObject.GetUTC: TDateTime;
//GMTを返す
begin
  Result := FDate;
end;

procedure TJDateObject.SetLocal(const Value: TDateTime);
//Localから変換
begin
  FDate := LocalDateTimeToGMT(Value);
end;

procedure TJDateObject.SetUTC(const Value: TDateTime);
//GMTから変換
begin
  FDate := Value;
end;

function TJDateObject.ToNumber: Double;
begin
  Result := DateTimeToECMATime(UTC);
end;

function TJDateObject.ToString(Value: PJValue): String;
begin
  //Result := DateTimeToStr(LocalTime);
  DateTimeToString(Result,FFormat,LocalTime);
end;

function TJDateObject.ValueOf: TJValue;
begin
  Result := DoGetTime(nil);
end;



{ TJFunctionObject }

constructor TJFunctionObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Function');
  FActivation := TJLocalSymbolTable.Create(nil);
end;

destructor TJFunctionObject.Destroy;
begin
  FreeAndNil(FActivation);
  inherited;
end;

function TJFunctionObject.GetValueImpl(S: String;
  var RetVal: TJValue; Param: TJValueList): Boolean;
begin
  //activationがあれば探す
  //if FActivation.GetValueImpl(FActivation,S,RetVal) then
  //  Result := True
  //else
    Result := inherited GetValueImpl(S,RetVal,Param);
end;

function TJFunctionObject.SetValueImpl(S: String;
  var Value: TJValue; Param: TJValueList): Boolean;
begin
  //activationがあれば探す
  //if FActivation.SetValueImpl(FActivation,S,Value) then
  //  Result := True
  //else
    Result := inherited SetValueImpl(S,Value,Param);
end;

function TJFunctionObject.ToString(Value: PJValue): String;
begin
  Result := '';
end;



end.
