unit ecma_wsh;

//WSH Object
//2002/12/21 ~
//by Wolfy

interface

uses
  Windows,Sysutils,Classes,ecma_type,ecma_object,
{$IFNDEF NO_ACTIVEX}
  ecma_activex,
{$ENDIF}
  ecma_misc,regexpr;

type  
  TJWshNamedObject = class(TJObject)
  private
    function DoExists(Param: TJValueList): TJValue;
    function DoItem(Param: TJValueList): TJValue;
    function GetLength: Integer;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    procedure GetPropertyList(List: TStringList); override;
  published
    property Length: Integer read GetLength;
    property Count: Integer read GetLength;
  end;

  TJWshUnnamedObject = class(TJArrayObject)
  private
    function DoItem(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  published
    property Count;
  end;

  TJWshArgumentsObject = class(TJArrayObject)
  private
    FNamed: TJWshNamedObject;
    FUnnamed: TJWshUnnamedObject;

    function DoItem(Param: TJValueList): TJValue;
    function DoShowUsage(Param: TJValueList): TJValue;
  protected
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure Parse(Args: TJArrayObject);
  published
    property Count;
    property Named: TJWshNamedObject read FNamed;
    property Unnamed: TJWshUnnamedObject read FUnnamed;
  end;

  TJWshBaseStreamObject = class(TJObject)
  private
    FOnRead: TReadStringEvent;
    FOnWrite: TStringEvent;
    function GetAtEndOfLine: Boolean;
    function GetColumn: Integer;
    function GetLine: Integer;

    procedure Write(Param: TJValueList; Line: Boolean; Breaks: Integer = 0);
    function Read(Count: Integer; Line: Boolean): String;
  protected
    function DoClose(Param: TJValueList): TJValue;

    function DoRead(Param: TJValueList): TJValue;
    function DoReadAll(Param: TJValueList): TJValue;
    function DoReadLine(Param: TJValueList): TJValue;
    function DoSkip(Param: TJValueList): TJValue;
    function DoSkipLine(Param: TJValueList): TJValue;

    function DoWrite(Param: TJValueList): TJValue;
    function DoWriteBlankLines(Param: TJValueList): TJValue;
    function DoWriteLine(Param: TJValueList): TJValue;

    procedure RegistMethods; override;    
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;

    property OnWrite: TStringEvent read FOnWrite write FOnWrite;
    property OnRead: TReadStringEvent read FOnRead write FOnRead;

    property AtEndOfLine: Boolean read GetAtEndOfLine;
  published
    property Column: Integer read GetColumn;
    property Line: Integer read GetLine;
  end;

  TJWshReadStreamObject = class(TJWshBaseStreamObject)
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  published
    property AtEndOfLine;
  end;

  TJWshWriteStreamObject = class(TJWshBaseStreamObject)
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
  end;

  TJWScriptObject = class(TJObject)
  private
    FArguments: TJWshArgumentsObject;
    FInteractive: Boolean;
    FStdIn: TJWshReadStreamObject;
    FStdOut: TJWshWriteStreamObject;
    FStdErr: TJWshWriteStreamObject;

    function DoCreateObject(Param: TJValueList): TJValue;
    function DoConnectObject(Param: TJValueList): TJValue;
    function DoDisconnectObject(Param: TJValueList): TJValue;
    function DoEcho(Param: TJValueList): TJValue;
    function DoGetObject(Param: TJValueList): TJValue;
    function DoQuit(Param: TJValueList): TJValue;
    function DoSleep(Param: TJValueList): TJValue;
    function GetFullName: String;
    function GetName: String;
    function GetPath: String;
    function GetScriptFullName: String;
    function GetScriptName: String;
    function GetVersion: String;
    function GetOnStdErr: TStringEvent;
    function GetOnStdOut: TStringEvent;
    function GetStdIn: TReadStringEvent;
    procedure SetOnStdErr(const Value: TStringEvent);
    procedure SetOnStdIn(const Value: TReadStringEvent);
    procedure SetOnStdOut(const Value: TStringEvent);
  protected
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;

    property OnStdOut: TStringEvent read GetOnStdOut write SetOnStdOut;
    property OnStdIn: TReadStringEvent read GetStdIn write SetOnStdIn;
    property OnStdErr: TStringEvent read GetOnStdErr write SetOnStdErr;
  published
    property Arguments: TJWshArgumentsObject read FArguments;
    property FullName: String read GetFullName;
    property Interactive: Boolean read FInteractive write FInteractive;
    property Name: String read GetName;
    property Path: String read GetPath;
    property ScriptFullName: String read GetScriptFullName;
    property ScriptName: String read GetScriptName;
    property StdErr: TJWshWriteStreamObject read FStdErr;
    property StdIn: TJWshReadStreamObject read FStdIn;
    property StdOut: TJWshWriteStreamObject read FStdOut;
    property Version: String read GetVersion;
  end;




implementation


{ TJWScriptObject }

constructor TJWScriptObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WScript');

  FArguments := TJWshArgumentsObject.Create(FEngine,nil,False);
  FArguments.IncRef;
  FStdErr := TJWshWriteStreamObject.Create(FEngine,nil,False);
  FStdErr.IncRef;
  FStdOut := TJWshWriteStreamObject.Create(FEngine,nil,False);
  FStdOut.IncRef;
  FStdIn := TJWshReadStreamObject.Create(FEngine,nil,False);
  FStdIn.IncRef;

end;

destructor TJWScriptObject.Destroy;
begin
  FStdIn.DecRef;
  FStdOut.DecRef;
  FStdErr.DecRef;
  FArguments.DecRef;
  inherited;
end;

function TJWScriptObject.DoConnectObject(Param: TJValueList): TJValue;
//イベント接続
var
  v0,v1: TJValue;
begin
  EmptyValue(Result);
{$IFNDEF NO_ACTIVEX}
  if IsParam2(Param) then
  begin
    v0 := Param[0];
    if IsObject(@v0) and (v0.vObject is TJActiveXObject) then
    begin
      v1 := Param[1];
      (v0.vObject as TJActiveXObject).Prefix := AsString(@v1);
    end;
  end;
{$ENDIF}
end;

function TJWScriptObject.DoCreateObject(Param: TJValueList): TJValue;
//ActiveXObject作成
{$IFNDEF NO_ACTIVEX}
var
  obj: TJActiveXObject;
begin
  obj := TJActiveXObject.Create(FEngine,Param);
  Result := BuildObject(obj);
{$ELSE}
begin
  EmptyValue(Result);
{$ENDIF}
end;

function TJWScriptObject.DoDisconnectObject(Param: TJValueList): TJValue;
//イベント解除
var
  v: TJValue;
begin
  EmptyValue(Result);
{$IFNDEF NO_ACTIVEX}
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsObject(@v) and (v.vObject is TJActiveXObject) then
      (v.vObject as TJActiveXObject).Disconnect;
  end;
{$ENDIF}
end;

function TJWScriptObject.DoEcho(Param: TJValueList): TJValue;
//表示
var
  s,capt: String;
  v: TJValue;
  i: Integer;
begin
  EmptyValue(Result);
  capt := GetApplicationTitle;
  s := '';
  if IsParam1(Param) then
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      s := s + AsString(@v) + ' ';
    end;

  s := TrimRight(s);
{$IFNDEF CONSOLE}
  MsgBox(PChar(s),PChar(capt),MB_OK);
{$ELSE}
  Writeln(s)
{$ENDIF}    
end;

function TJWScriptObject.DoGetObject(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
{ TODO : 未実装 }
end;

function TJWScriptObject.DoQuit(Param: TJValueList): TJValue;
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

function TJWScriptObject.DoSleep(Param: TJValueList): TJValue;
//ミリ秒停止
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Sleep(AsInteger(@v));
  end
  else
    Sleep(0);
end;

function TJWScriptObject.GetFullName: String;
begin
  Result := ParamStr(0);
end;

function TJWScriptObject.GetName: String;
begin
  Result := ExtractFileName(ParamStr(0));
end;

function TJWScriptObject.GetOnStdErr: TStringEvent;
begin
  Result := FStdErr.OnWrite;
end;

function TJWScriptObject.GetOnStdOut: TStringEvent;
begin
  Result := FStdOut.OnWrite;
end;

function TJWScriptObject.GetPath: String;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function TJWScriptObject.GetScriptFullName: String;
//スクリプト名をフルパスで得る
var
  path: String;
  p: PChar;
begin
  Result := '';
  if Assigned(FEngine) then
  begin
    SetLength(path,MAX_PATH);
    if GetFullPathName(PChar(FEngine.GetScriptFilename),MAX_PATH,PChar(path),p) <> 0 then
      Result := PChar(path)
    else
      Result := FEngine.GetScriptFilename;
  end;
end;

function TJWScriptObject.GetScriptName: String;
//スクリプト名を得る
begin
  Result := '';
  if Assigned(FEngine) then
    Result := ExtractFileName(FEngine.GetScriptFilename);
end;

function TJWScriptObject.GetStdIn: TReadStringEvent;
begin
  Result := FStdIn.OnRead;
end;

function TJWScriptObject.GetVersion: String;
begin
  Result := DMS_VERSION;
end;

procedure TJWScriptObject.RegistMethods;
begin
  inherited;
  RegistMethod('CreateObject',DoCreateObject);
  RegistMethod('ConnectObject',DoConnectObject);
  RegistMethod('DisconnectObject',DoDisconnectObject);
  RegistMethod('Echo',DoEcho);
  RegistMethod('GetObject',DoGetObject);
  RegistMethod('Quit',DoQuit);
  RegistMethod('Sleep',DoSleep);
end;

procedure TJWScriptObject.SetOnStdErr(const Value: TStringEvent);
begin
  FStdErr.OnWrite := Value;
end;

procedure TJWScriptObject.SetOnStdIn(const Value: TReadStringEvent);
begin
  FStdIn.OnRead := Value;
end;

procedure TJWScriptObject.SetOnStdOut(const Value: TStringEvent);
begin
  FStdOut.OnWrite := Value;
end;

{ TJWshNamedObject }

constructor TJWshNamedObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WshNamed');

  RegistMethod('Exists',DoExists);
  RegistMethod('Item',DoItem);
end;

function TJWshNamedObject.DoExists(Param: TJValueList): TJValue;
//itemが存在するか
var
  v: TJValue;
begin
  Result := BuildBool(False);
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildBool(HasKey(AsString(@v)));
  end;
end;

function TJWshNamedObject.DoItem(Param: TJValueList): TJValue;
//itemを得る
var
  v: TJValue;
begin
  Result := BuildNull;
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := GetValue(AsString(@v),True);
  end;
end;

function TJWshNamedObject.GetLength: Integer;
//keyの数
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    GetPropertyList(sl);
    Result := sl.Count;
  finally
    sl.Free;
  end;
end;

procedure TJWshNamedObject.GetPropertyList(List: TStringList);
//全てのメンバを得る
begin
  inherited GetKeyList(List,[vtString],[]);
end;

{ TJWshUnnamedObject }

constructor TJWshUnnamedObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WshUnnamed');

  RegistMethod('Item',DoItem);
end;

function TJWshUnnamedObject.DoItem(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildNull;
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := Self.GetValue(AsString(@v),True);
  end;
end;


{ TJWshArgumentsObject }

constructor TJWshArgumentsObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WshArguments');
  FNamed := TJWshNamedObject.Create(FEngine,nil,False);
  FNamed.IncRef;
  FUnnamed := TJWshUnnamedObject.Create(FEngine,nil,False);
  FUnnamed.IncRef;
end;

destructor TJWshArgumentsObject.Destroy;
begin
  FNamed.DecRef;
  FUnnamed.DecRef;
  inherited;
end;

function TJWshArgumentsObject.DoItem(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildNull;
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := Self.GetValue(AsString(@v),True);
  end;
end;

function TJWshArgumentsObject.DoShowUsage(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
{ TODO : 未実装 }
end;

procedure TJWshArgumentsObject.Parse(Args: TJArrayObject);
//Engine.BeforeRunから呼ばれる
var
  i: Integer;
  re: TRegExpr;
  v: TJValue;
begin
  //クリア
  Clear;   
  if Assigned(Args) and (Args.Count > 0) then
  begin
    for i := 0 to Args.Count - 1 do
      Add(Args.GetItem(i));
  end
  else begin //コマンドライン
    for i := 2 to ParamCount do
      Add(BuildString(ParamStr(i)));
  end;
  //分ける
  re := TRegExpr.Create;
  try
    re.Expression := '\/([^\:]+)\:(.*)';
    for i := 0 to Count - 1 do
    begin
      v := GetItem(i);
      if IsString(@v) then
      begin
        //名前付き
        if re.Exec(AsString(@v)) then
          FNamed.SetValue(re.Match[1],BuildString(re.Match[2]),True)
        else
          FUnnamed.Add(v);
      end
      else
        FUnnamed.Add(v);
    end;
  finally
    re.Free;
  end;
end;

procedure TJWshArgumentsObject.RegistMethods;
begin
  inherited;
  RegistMethod('Item',DoItem);
  RegistMethod('ShowUsage',DoShowUsage);
end;

{ TJWshBaseStreamObject }

constructor TJWshBaseStreamObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WshBaseStream');  
end;

function TJWshBaseStreamObject.DoClose(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
  //何もしない
end;

function TJWshBaseStreamObject.DoRead(Param: TJValueList): TJValue;
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

function TJWshBaseStreamObject.DoReadAll(Param: TJValueList): TJValue;
begin
  Result := BuildString(Read(-1,False));
end;

function TJWshBaseStreamObject.DoReadLine(Param: TJValueList): TJValue;
begin
  Result := BuildString(Read(-1,True));
end;

function TJWshBaseStreamObject.DoSkip(Param: TJValueList): TJValue;
begin
{ TODO : 未実装 }
  EmptyValue(Result);
end;

function TJWshBaseStreamObject.DoSkipLine(Param: TJValueList): TJValue;
begin
{ TODO : 未実装 }
  EmptyValue(Result);
end;

function TJWshBaseStreamObject.DoWrite(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
  Write(Param,False);
end;

function TJWshBaseStreamObject.DoWriteBlankLines(
  Param: TJValueList): TJValue;
var
  v: TJValue;
  i: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    i := AsInteger(@v);
  end
  else
    i := 0;

  Write(Param,False,i);
end;

function TJWshBaseStreamObject.DoWriteLine(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
  Write(Param,True);
end;

function TJWshBaseStreamObject.GetAtEndOfLine: Boolean;
begin
{ TODO : 未実装 }
  Result := True;
end;

function TJWshBaseStreamObject.GetColumn: Integer;
begin
{ TODO : 未実装 }
  Result := 0;
end;

function TJWshBaseStreamObject.GetLine: Integer;
begin
{ TODO : 未実装 }
  Result := 1;
end;

function TJWshBaseStreamObject.Read(Count: Integer; Line: Boolean): String;
var
  success: Boolean;
begin
  success := False;
  if Assigned(FOnRead) then
    FOnRead(Self,Result,success,Count,Line);

  if not success then
    raise EJThrow.Create(E_FILE,'read error: STDIN');
end;

procedure TJWshBaseStreamObject.RegistMethods;
begin
  inherited;
  RegistMethod('Close',DoClose);
end;

procedure TJWshBaseStreamObject.Write(Param: TJValueList; Line: Boolean;
  Breaks: Integer);
//書き込み
var
  i: Integer;
  v: TJValue;
  s: String;
begin
  if not Assigned(FOnWrite) then
    Exit;

  if Breaks > 0 then
  begin
    for i := 0 to Breaks - 1 do
      FOnWrite(Self,CRLF);
  end
  else begin
    if IsParam1(Param) then
    begin
      v := Param[0];
      s := AsString(@v);
    end
    else
      s := '';

    if Line then
      FOnWrite(Self,s + CRLF)
    else if s <> '' then
      FOnWrite(Self,s);
  end;
end;

{ TJWshReadStreamObject }

constructor TJWshReadStreamObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WshReadStream');
  RegistMethod('Read',DoRead);
  RegistMethod('ReadAll',DoReadAll);
  RegistMethod('ReadLine',DoReadLine);
  RegistMethod('Skip',DoSkip);
  RegistMethod('SkipLine',DoSkipLine);
end;

{ TJWshWriteStreamObject }

constructor TJWshWriteStreamObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('WshWriteStream');
  RegistMethod('Write',DoWrite);
  RegistMethod('WriteBlankLines',DoWriteBlankLines);
  RegistMethod('WriteLine',DoWriteLine);
end;


end.
