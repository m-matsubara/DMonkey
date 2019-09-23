unit cookielib;

//cookie関係
//2001/04/30
//by Wolfy

interface

uses
  windows,sysutils,classes,regexpr,hashtable,gsocketmisc;

type
  TCookie = class(TObject)
  private
    FHash: TStringHashTable;
    FDomain: String;
    FPath: String;
    FExpire: TDateTime;
    FSecure: Boolean;

    function GetValue(Name: String): String;
    procedure SetValue(Name: String; const Value: String);
    function GetClientText: String;
    function GetServerText: String;
    function GetCount: Integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Url,CookieStr: String); overload;
    procedure Parse(CookieStr: String); overload;

    procedure Clear;
    function HasName(Name: String): Boolean;
    function IsDomain(S: String): Boolean;
    procedure Assign(ACookie: TCookie);
    function Match(AUrl: String): Boolean; overload;
    function Match(ADomain,APath: String): Boolean; overload;
    function IsSame(AUrl: String): Boolean; overload;
    function IsSame(ADomain,APath: String): Boolean; overload;
    function BetterMatch(AUrl: String): Boolean; overload;
    function BetterMatch(ADomain,APath: String): Boolean; overload;
    procedure GetNameList(List: TStrings);

    property Domain: String read FDomain write FDomain;
    property Path: String read FPath write FPath;
    property Expire: TDateTime read FExpire write FExpire;
    property Secure: Boolean read FSecure write FSecure;
    property Value[Name: String]: String read GetValue write SetValue; default;
    property ClientText: String read GetClientText;
    property ServerText: String read GetServerText;
    property Count: Integer read GetCount;
  end;

implementation

{ TCookie }

procedure TCookie.Assign(ACookie: TCookie);
var
  sl: TStringList;
  i: Integer;
begin
  Clear;

  FDomain := ACookie.FDomain;
  FPath := ACookie.FPath;
  FExpire := ACookie.FExpire;
  FSecure := ACookie.FSecure;
  sl := TStringList.Create;
  try
    sl.Text := ACookie.FHash.Keys;
    for i := 0 to sl.Count - 1 do
      FHash[sl[i]] := ACookie.FHash[sl[i]];
  finally
    sl.Free;
  end;
end;

procedure TCookie.Clear;
//クリア
begin
  FHash.Clear;
  FDomain := '';
  FPath := '/';
  FExpire := 0;   
end;

constructor TCookie.Create;
//作成
begin
  inherited Create;
  FHash := TStringHashTable.Create(HASH_100);
  FHash.RaiseException := False;
  Clear;
end;

destructor TCookie.Destroy;
//破棄
begin
  FHash.Free;
  inherited;
end;

function TCookie.GetClientText: String;
//クライアント側
var
  i: Integer;
  sl: TStringList;
  value: String;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := FHash.Keys;
    for i := 0 to sl.Count - 1 do
    begin
      value := FHash[sl[i]];
      if value <> '' then
        Result := Result + sl[i] + '=' + value + ';';
    end;
  finally
    sl.Free;
  end;
end;

function TCookie.GetServerText: String;
//サーバ側
var
  i: Integer;
  sl: TStringList;
begin
  Result := '';
  if FExpire <> 0 then
    Result := 'expires=' + DateTimeToHttpModified(FExpire) + ';';

  if FDomain <> '' then
    Result := Result + 'domain=' + FDomain + ';';

  if FPath <> '' then
    Result := Result + 'path=' + FPath + ';'
  else
    Result := Result + 'path=/;';

  if FSecure then
    Result := Result + 'secure;';

  sl := TStringList.Create;
  try
    sl.Text := FHash.Keys;
    for i := 0 to sl.Count - 1 do
      Result := Result + sl[i] + '=' + FHash[sl[i]] + ';';
  finally
    sl.Free;
  end;
end;

function TCookie.GetValue(Name: String): String;
begin
  Result := FHash[Name];
end;

function TCookie.HasName(Name: String): Boolean;
begin
  Result := FHash.HasKey(Name);
end;

function TCookie.IsDomain(S: String): Boolean;
begin
  Result := (S = FDomain)
end;

function TCookie.IsSame(AUrl: String): Boolean;
//同じか？
var
  ui: TUrlInfo;
begin
  ui := ParseUrl(AUrl);
  Result := IsSame(ui.Host,ui.Path);
end;

function TCookie.Match(AUrl: String): Boolean;
//完全マッチするか？
var
  ui: TUrlInfo;
begin
  ui := ParseUrl(AUrl);
  Result := Match(ui.Host,ui.Path);
end;

function TCookie.Match(ADomain, APath: String): Boolean;
begin
  Result := AnsiSameText(FDomain,ADomain) and
            (AnsiPos(FPath,APath) > 0);
end;

procedure TCookie.Parse(Url,CookieStr: String);
//解析
var
  sl: TStringList;
  i,index: Integer;
  name,value: String;
  ui: TUrlInfo;
begin
  //初期値
  ui := ParseUrl(Url);
  FDomain := ui.Host;
  FPath := ui.Path;

  sl := TStringList.Create;
  try
    //;で分割
    SplitRegExpr(';',CookieStr,sl);
    for i := 0 to sl.Count - 1 do
    begin
      //=で分割
      index := AnsiPos('=',sl[i]);
      if index > 0 then
      begin
        name := Trim(Copy(sl[i],1,index - 1));
        value := Trim(Copy(sl[i],index + 1,MaxInt));
        if AnsiSameText(name,'expires') then
          FExpire := GMTToLocalTime(HttpModifiedToDateTime(value))
        else if AnsiSameText(name,'domain') then
          FDomain := value
        else if AnsiSameText(name,'path') then
          FPath := value
        else begin
          FHash[name] := value;
        end;
      end;
    end;
  finally
    sl.Free;
  end;   
end;

procedure TCookie.SetValue(Name: String; const Value: String);
begin
  FHash[Name] := Value;
end;

function TCookie.IsSame(ADomain, APath: String): Boolean;
begin
  Result := AnsiSameText(FDomain,ADomain) and AnsiSameText(APath,FPath);
end;

procedure TCookie.Parse(CookieStr: String);
begin
  Parse('',CookieStr);
end;

function TCookie.GetCount: Integer;
begin
  Result := FHash.KeyList.Count;
end;

function TCookie.BetterMatch(AUrl: String): Boolean;
var
  ui: TUrlInfo;
begin
  ui := ParseUrl(AUrl);
  Result := BetterMatch(ui.Host,ui.Path);
end;

function TCookie.BetterMatch(ADomain, APath: String): Boolean;
//より良いマッチ
begin
  Result := (AnsiPos(AnsiLowerCase(FDomain),AnsiLowerCase(ADomain)) > 0) and
            (AnsiPos(FPath,APath) > 0);
end;

procedure TCookie.GetNameList(List: TStrings);
begin
  List.Assign(FHash.KeyList);
end;

end.
