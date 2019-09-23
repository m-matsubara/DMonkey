unit ecma_sockobject;

//socket組込みobject
//2001/04/27 ~
//by Wolfy

{$IFDEF VER140}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

interface

uses
  windows,classes,sysutils,
  ecma_type,hashtable,regexpr,jconvert,
  ecma_misc,ecma_object,myclasses,
  ecma_extobject,
  gsocket,gsocketmisc,gsockethttp,gsockethttps,
  gsocketftp,gsocketpop3,cookielib,winsock,
  gsocketsmtp,gsockethtml;

type
  //URL解析分解
  TJURLInfoObject = class(TJObject)
  private
    FInfo: TUrlInfo;
    function DoExpand(Param: TJValueList): TJValue;

    function GetDir: String;
    function GetFilename: String;
    function GetHost: String;
    function GetPass: String;
    function GetPath: String;
    function GetPort: String;
    function GetProtocol: String;
    function GetQuery: String;
    function GetUrl: String;
    function GetUser: String;
    procedure SetDir(const Value: String);
    procedure SetFilename(const Value: String);
    procedure SetHost(const Value: String);
    procedure SetPass(const Value: String);
    procedure SetPath(const Value: String);
    procedure SetPort(const Value: String);
    procedure SetProtocol(const Value: String);
    procedure SetQuery(const Value: String);
    procedure SetUrl(const Value: String);
    procedure SetUser(const Value: String);
    function GetHostname: String;
    procedure SetHostname(const Value: String);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
  published
    property url: String read GetUrl write SetUrl;
    property uri: String read GetUrl write SetUrl;
    property href: String read GetUrl write SetUrl;
    property protocol: String read GetProtocol write SetProtocol;
    property scheme: String read GetProtocol write SetProtocol;
    property username: String read GetUser write SetUser;
    property user: String read GetUser write SetUser;
    property userid: String read GetUser write SetUser;
    property password: String read GetPass write SetPass;
    property pass: String read GetPass write SetPass;
    property host: String read GetHost write SetHost;
    property hostname: String read GetHostname write SetHostname;
    property port: String read GetPort write SetPort;
    property path: String read GetPath write SetPath;
    property directory: String read GetDir write SetDir;
    property dir: String read GetDir write SetDir;
    property filename: String read GetFilename write SetFilename;
    property query: String read GetQuery write SetQuery;
    property search: String read GetQuery write SetQuery;
  end;
  //http
  TJCookieObject = class(TJObject)
  private
    FCookie: TCookie;
    FExpires: TJDateObject;

    function DoParse(Param: TJValueList): TJValue;
    function GetDomain: String;
    function GetExpires: TJDateObject;
    function GetPath: String;
    procedure SetDomain(const Value: String);
    procedure SetPath(const Value: String);
    function GetLength: Integer;
    function GetData: String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    procedure GetPropertyList(List: TStringList); override;
    function HasKey(S: String): Boolean; override;
    function ToString(Value: PJValue = nil): String; override;

    property Cookie: TCookie read FCookie;
  published
    property domain: String read GetDomain write SetDomain;
    property path: String read GetPath write SetPath;
    property expires: TJDateObject read GetExpires;
    property length: Integer read GetLength;
    property data: String read GetData;
  end;

  TJRequestObject = class(TJObject)
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    procedure GetPropertyList(List: TStringList); override;
    function ToString(Value: PJValue = nil): String; override;
  end;

  TJResponseObject = class(TJObject)
  private
    FCookie: TJCookieObject;
    FCode: Integer;
    FText: String;
    FVersion: String;
    function GetCookie: TJCookieObject;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure SetStatus(AVersion: String; ACode: Integer; AText: String);
    procedure Clear; override;
    procedure GetPropertyList(List: TStringList); override;
    function ToString(Value: PJValue = nil): String; override;
  published
    property cookie: TJCookieObject read GetCookie;
    property code: Integer read FCode;
    property text: String read FText;
    property version: String read FVersion;
  end;

  TJBaseSocketObject = class(TJObject)
  private
    FDebug: Boolean;
    FOnPrint: TOnPrineEvent;     
  protected
    //event
    function GetTimeout: Integer; virtual; abstract;
    procedure SetTimeout(const Value: Integer); virtual; abstract;
    function GetBytesRead: Integer; virtual; abstract;
    function GetBytesTotal: Integer; virtual; abstract;
    function GetBytesWrote: Integer; virtual; abstract;
  public
    property OnPrint: TOnPrineEvent read FOnPrint write FOnPrint;
  published
    property timeout: Integer read GetTimeout write SetTimeout;
    property debug: Boolean read FDebug write FDebug;
    property bytesTotal: Integer read GetBytesTotal;
    property bytesRead: Integer read GetBytesRead;
    property bytesWrote: Integer read GetBytesWrote;
  protected
    procedure OnConnect(Sender: TObject);
    procedure OnDisconnect(Sender: TObject);
    procedure OnError(Sender: TObject; Errno: Word; Errmsg: String);
    procedure OnPacketRecvd(Sender: TObject; Bytes: Integer; var DoStop: Boolean);
    procedure OnPacketSend(Sender: TObject; Bytes: Integer; var DoStop: Boolean);
    procedure OnStatus(Sender: TObject; const Status: String);
  end;

  TJHTTPObject = class(TJBaseSocketObject)
  private
    FHTTP: TgHTTP;
    FRequestHeader: TJRequestObject;
    FResponseHeader: TJResponseObject;

    function DoGet(Param: TJValueList): TJValue;
    function DoGetFile(Param: TJValueList): TJValue;
    function DoPost(Param: TJValueList): TJValue;
    function DoHead(Param: TJValueList): TJValue;
    function DoRequest(Param: TJValueList): TJValue;
    function DoResponse(Param: TJValueList): TJValue;
    function DoCapture(Param: TJValueList): TJValue;
    function DoReadln(Param: TJValueList): TJValue;
    function DoRead(Param: TJValueList): TJValue;
    function DoWriteln(Param: TJValueList): TJValue;
    function DoWrite(Param: TJValueList): TJValue;
    function DoConnect(Param: TJValueList): TJValue;
    function DoDisconnect(Param: TJValueList): TJValue;

    function GetAutoRedirect: Boolean;
    function GetLength: Integer;
    function GetVersion: String;
    procedure SetAutoRedirect(const Value: Boolean);
    procedure SetVersion(const Value: String);
    function GetRequestHeader: TJObject;
    function GetResponseHeader: TJResponseObject;
    function GetProxy: String;
    procedure SetProxy(const Value: String);
    procedure BeforeRequest;
    procedure AfterResponse;
  protected
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    function GetBytesRead: Integer; override;
    function GetBytesTotal: Integer; override;
    function GetBytesWrote: Integer; override;
    function CreateHttp: TgHttp; virtual;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property requestHeader: TJObject read GetRequestHeader;
    property responseHeader: TJResponseObject read GetResponseHeader;
    property version: String read GetVersion write SetVersion;
    property length: Integer read GetLength;
    property autoRedirect: Boolean read GetAutoRedirect write SetAutoRedirect;
    property proxy: String read GetProxy write SetProxy;
  protected
    procedure OnRequest(Sender: TObject; var Url,Proxy: String; Headers: THashTable);
    procedure OnResponse(Sender: TObject; const Url,Proxy,StatusMsg: String; var RedirectUrl: String; var StatusNumber: Integer; Headers: THashTable);
    procedure OnSuccess(cmd: THTTPCmdType);
    procedure OnFailure(cmd: THTTPCmdType);
  end;

  TJHTTPSObject = class(TJHTTPObject)
  protected
    function CreateHttp: TgHttp; override;
  end;
  
  //tcp socket
  TJTCPSocketObject = class(TJBaseSocketObject)
  private
    FSock: TgSocket;

    function DoConnect(Param: TJValueList): TJValue;
    function DoDisconnect(Param: TJValueList): TJValue;
    function DoAbort(Param: TJValueList): TJValue;
    function DoRead(Param: TJValueList): TJValue;
    function DoReadln(Param: TJValueList): TJValue;
    function DoWrite(Param: TJValueList): TJValue;
    function DoWriteln(Param: TJValueList): TJValue;
    function DoCapture(Param: TJValueList): TJValue;
    function DoSendFile(Param: TJValueList): TJValue;
    function DoBind(Param: TJValueList): TJValue;
    function DoAccept(Param: TJValueList): TJValue;
    function DoIsConnected(Param: TJValueList): TJValue;

    function GetHost: String;
    function GetPort: Integer;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
  protected
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    function GetBytesRead: Integer; override;
    function GetBytesTotal: Integer; override;
    function GetBytesWrote: Integer; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property host: String read GetHost write SetHost;
    property port: Integer read GetPort write SetPort;
  end;

  TJMailObject = class(TJObject)
  private
    FMailMessage: TMailMessage;
    FAttach: TJStringsObject;

    function DoClear(Param: TJValueList): TJValue;
    function GetAttachments: TJObject;
    function GetBody: String;
    function GetHeader: String;
    function GetLength: Integer;
    function GetMessage: String;
    function GetNumber: Integer;
    procedure SetMessage(const Value: String);
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure Clear; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    procedure GetPropertyList(List: TStringList); override;
  published
    property number: Integer read GetNumber;
    property length: Integer read GetLength;
    property attachments: TJObject read GetAttachments;
    property header: String read GetHeader;
    property body: String read GetBody;
    property message: String read GetMessage write SetMessage;
  end;

  TJPOP3Object = class(TJBaseSocketObject)
  private
    FPOP: TgPOP3;
    FMail: TJMailObject;

    function DoConnect(Param: TJValueList): TJValue;
    function DoDisconnect(Param: TJValueList): TJValue;
    function DoGetMail(Param: TJValueList): TJValue;
    function DoGetSummary(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function GetAttachFilePath: String;
    function GetDeleteOnRead: Boolean;
    function GetMail: TJMailObject;
    function GetMailCount: Integer;
    function GetPassword: String;
    function GetUserId: String;
    procedure SetAttachFilePath(const Value: String);
    procedure SetDeleteOnRead(const Value: Boolean);
    procedure SetPassword(const Value: String);
    procedure SetUserId(const Value: String);
    function GetHost: String;
    function GetPort: Integer;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
  protected
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    function GetBytesRead: Integer; override;
    function GetBytesTotal: Integer; override;
    function GetBytesWrote: Integer; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property attachPath: String read GetAttachFilePath write SetAttachFilePath ;
    property deleteOnRead: Boolean read GetDeleteOnRead write SetDeleteOnRead ;
    property mailCount: Integer read GetMailCount;
    property length: Integer read GetMailCount;
    property password: String read GetPassword write SetPassword ;
    property userid: String read GetUserId write SetUserId ;
    property mail: TJMailObject read GetMail;
    property host: String read GetHost write SetHost;
    property port: Integer read GetPort write SetPort;
  protected
    procedure OnFailure(Sender: TObject);
    procedure OnSuccess(Sender: TObject);
  end;

  TJSMTPObject = class(TJBaseSocketObject)
  private
    FSMTP: TgSMTP;
    FMail: TJMailObject;

    function DoConnect(Param: TJValueList): TJValue;
    function DoDisconnect(Param: TJValueList): TJValue;
    function DoSendMail(Param: TJValueList): TJValue;
    function GetHost: String;
    function GetMail: TJMailObject;
    function GetPort: Integer;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
  protected
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    function GetBytesRead: Integer; override;
    function GetBytesTotal: Integer; override;
    function GetBytesWrote: Integer; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property mail: TJMailObject read GetMail;
    property host: String read GetHost write SetHost;
    property port: Integer read GetPort write SetPort;
  end;

  TJHtmlTagObject = class(TJObject)
  private
    FName: String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;

    procedure AssignTag(Source: THtmlTag);
  published
    property name: String read FName;
  end;

  TJHtmlParserObject = class(TJObject)
  private
    FHtml: String;
    FParser: THtmlParser;

    function DoParse(Param: TJValueList): TJValue;
    function DoParseFile(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    
    function GetHtml: String;
    function GetText: String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    function ToString(Value: PJValue = nil): String; override;
    
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property text: String read GetText;
    property html: String read GetHtml;
    property count: Integer read GetCount;
    property length: Integer read GetCount;
  end;

  TJFtpFilePropertyObject = class(TJObject)
  private
    FFileProperty: TFTPFileProperty;
    function GetAttribute: Integer;
    function GetFileType: String;
    function GetModified: TJDateObject;
    function GetName: String;
    function GetSize: Integer;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function ToString(Value: PJValue = nil): String; override;

    property FileProperty: TFTPFileProperty read FFileProperty write FFileProperty;
  published
    property fileType: String read GetFileType;
    property size: Integer read GetSize;
    property length: Integer read GetSize;
    property name: String read GetName;
    property lastModified: TJDateObject read GetModified;
    property attribute: Integer read GetAttribute;
  end;

  TJFtpObject = class(TJBaseSocketObject)
  private
    FFtp: TgFtp;

    function DoConnect(Param: TJValueList): TJValue;
    function DoDisconnect(Param: TJValueList): TJValue;
    function DoAbort(Param: TJValueList): TJValue;
    function DoChangeDir(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoDownload(Param: TJValueList): TJValue;
    function DoDownloadRestore(Param: TJValueList): TJValue;
    function DoList(Param: TJValueList): TJValue;
    function DoNList(Param: TJValueList): TJValue;
    function DoMakeDir(Param: TJValueList): TJValue;
    function DoReinitialize(Param: TJValueList): TJValue;
    function DoRemoveDir(Param: TJValueList): TJValue;
    function DoRename(Param: TJValueList): TJValue;
    function DoUpload(Param: TJValueList): TJValue;
    function DoUploadAppend(Param: TJValueList): TJValue;
    function DoUploadRestore(Param: TJValueList): TJValue;
    function DoUploadUnique(Param: TJValueList): TJValue;
    function DoPrintWorkDir(Param: TJValueList): TJValue;
    function DoFindFiles(Param: TJValueList): TJValue;
    function DoLogin(Param: TJValueList): TJValue;
    function DoQuit(Param: TJValueList): TJValue;
    function DoType(Param: TJValueList): TJValue;
    function DoCommand(Param: TJValueList): TJValue;

    function GetCurrentDir: String;
    function GetPassiveMode: Boolean;
    function GetPassword: String;
    function GetUserId: String;
    procedure SetPassiveMode(const Value: Boolean);
    procedure SetPassword(const Value: String);
    procedure SetUserId(const Value: String);
    function GetProxy: String;
    procedure SetProxy(const Value: String);
    function GetHost: String;
    function GetPort: Integer;
    procedure SetHost(const Value: String);
    procedure SetPort(const Value: Integer);
    function GetLength: Integer;
  protected
    function GetTimeout: Integer; override;
    procedure SetTimeout(const Value: Integer); override;
    function GetBytesRead: Integer; override;
    function GetBytesTotal: Integer; override;
    function GetBytesWrote: Integer; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property currentDir: String read GetCurrentDir;
    property password: String read GetPassword write SetPassword;
    property userid: String read GetUserId write SetUserId;
    property passiveMode: Boolean read GetPassiveMode write SetPassiveMode;
    property proxy: String read GetProxy write SetProxy;
    property host: String read GetHost write SetHost;
    property port: Integer read GetPort write SetPort;
    property length: Integer read GetLength;
  protected
    procedure OnFailure(var Handled: Boolean; Trans_Type: TFTPCmdType);
    procedure OnSuccess(Trans_Type: TFTPCmdType);
  end;


procedure RegisterDMS(Engine: TJBaseEngine);


implementation

procedure RegisterDMS(Engine: TJBaseEngine);
begin
  Engine.ImportObject('URL',TJUrlInfoObject);
  Engine.ImportObject('Cookie',TJCookieObject);
  //Engine.ImportObject('Response',TJResponseObject);
  Engine.ImportObject('HTTP',TJHTTPObject);
  Engine.ImportObject('HTTPS',TJHTTPSObject);
  Engine.ImportObject('TCPSocket',TJTCPSocketObject);
  Engine.ImportObject('Mail',TJMailObject);
  Engine.ImportObject('POP3',TJPOP3Object);
  Engine.ImportObject('SMTP',TJSMTPObject);
  Engine.ImportObject('HtmlTag',TJHtmlTagObject);
  Engine.ImportObject('HtmlParser',TJHtmlParserObject);
  Engine.ImportObject('FTP',TJFTPObject);
  //Engine.ImportObject('FileProperty',TJFTPFilePropertyObject);
end;


{ TJURLInfoObject }

constructor TJURLInfoObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('URL');
  RegistMethod('expand',DoExpand);

  if IsParam1(Param) then
  begin
    v := Param[0];
    FInfo := ParseUrl(AsString(@v));
  end;
end;

function TJURLInfoObject.DoExpand(Param: TJValueList): TJValue;
//現在のpathをカレントとして絶対pathを作って返す
var
  v: TJValue;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    Result := BuildString(ExpandUrl(FInfo.Url,AsString(@v)));
  end
  else
    Result := BuildString(FInfo.Url);
end;

function TJURLInfoObject.GetDir: String;
begin
  Result := FInfo.Dir;
end;

function TJURLInfoObject.GetFilename: String;
begin
  Result := FInfo.FileName;
end;

function TJURLInfoObject.GetHost: String;
begin
  if FInfo.Port <> '' then
    Result := FInfo.host + ':' + FInfo.Port
  else
    Result := FInfo.Host;
end;

function TJURLInfoObject.GetHostname: String;
begin
  Result := FInfo.Host;
end;

function TJURLInfoObject.GetPass: String;
begin
  Result := FInfo.Password;
end;

function TJURLInfoObject.GetPath: String;
begin
  Result := FInfo.Path;
end;

function TJURLInfoObject.GetPort: String;
begin
  Result := FInfo.Port;
end;

function TJURLInfoObject.GetProtocol: String;
begin
  Result := FInfo.Protocol;
end;

function TJURLInfoObject.GetQuery: String;
begin
  Result := FInfo.Query;
end;

function TJURLInfoObject.GetUrl: String;
begin
  Result := BuildUrl(FInfo);
end;

function TJURLInfoObject.GetUser: String;
begin
  Result := FInfo.Userid;
end;

procedure TJURLInfoObject.SetDir(const Value: String);
begin
  FInfo.Dir := Value;
end;

procedure TJURLInfoObject.SetFilename(const Value: String);
begin
  FInfo.FileName := Value;
end;

procedure TJURLInfoObject.SetHost(const Value: String);
var
  index: Integer;
begin
  index := Pos(':',Value);
  if index > 0 then
  begin
    FInfo.Host := Copy(Value,1,index - 1);
    FInfo.Port := Copy(Value,index + 1,MaxInt);
  end
  else
    FInfo.Host := Value;
end;

procedure TJURLInfoObject.SetHostname(const Value: String);
begin
  FInfo.Host := Value;
end;

procedure TJURLInfoObject.SetPass(const Value: String);
begin
  FInfo.Password := Value;
end;

procedure TJURLInfoObject.SetPath(const Value: String);
begin
  FInfo.Path := Value;
end;

procedure TJURLInfoObject.SetPort(const Value: String);
begin
  FInfo.Port := Value;
end;

procedure TJURLInfoObject.SetProtocol(const Value: String);
begin
  FInfo.Protocol := Value;
end;

procedure TJURLInfoObject.SetQuery(const Value: String);
begin
  FInfo.Query := Value;
end;

procedure TJURLInfoObject.SetUrl(const Value: String);
begin
  FInfo := ParseUrl(Value);
end;

procedure TJURLInfoObject.SetUser(const Value: String);
begin
  FInfo.UserId := Value;
end;

function TJURLInfoObject.ToString(Value: PJValue): String;
begin
  Result := FInfo.Url;
end;

{ TJCookieObject }

constructor TJCookieObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Cookie');
  RegistMethod('parse',DoParse);

  FCookie := TCookie.Create;
  FExpires := TJDateObject.Create(FEngine,nil,False);
  FExpires.IncRef;
end;

destructor TJCookieObject.Destroy;
begin
  FreeAndNil(FCookie);
  FExpires.DecRef;
  inherited;
end;

function TJCookieObject.DoParse(Param: TJValueList): TJValue;
var
  v1,v2: TJValue;

begin
  Result := BuildObject(Self);

  if IsParam2(Param) then
  begin
    v1 := Param[0];
    v2 := Param[1];
    FCookie.Parse(AsString(@v1),AsString(@v2));
  end
  else if IsParam1(Param) then
  begin
    v1 := Param[0];
    FCookie.Parse(AsString(@v1));
  end;
end;

function TJCookieObject.GetData: String;
begin
  Result := FCookie.ClientText;
end;

function TJCookieObject.GetDomain: String;
begin
  Result := FCookie.Domain;
end;

function TJCookieObject.GetExpires: TJDateObject;
begin
  FExpires.UTC := FCookie.Expire;
  Result := FExpires;                 
end;

function TJCookieObject.GetLength: Integer;
begin
  Result := FCookie.Count;
end;

function TJCookieObject.GetPath: String;
begin
  Result := FCookie.Path;
end;

procedure TJCookieObject.GetPropertyList(List: TStringList);
begin
  FCookie.GetNameList(List);
end;

function TJCookieObject.GetValue(S: String; ArrayStyle: Boolean;
  Param: TJValueList): TJValue;
begin
  if ArrayStyle then
    Result := BuildString(FCookie[S])
  else
    Result := inherited GetValue(S,ArrayStyle,Param);
end;

function TJCookieObject.HasKey(S: String): Boolean;
begin
  Result := FCookie.HasName(S);
end;

procedure TJCookieObject.SetDomain(const Value: String);
begin
  FCookie.Domain := Value;
end;

procedure TJCookieObject.SetPath(const Value: String);
begin
  FCookie.Path := Value;
end;

procedure TJCookieObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList);
begin
  if ArrayStyle then
    FCookie[S] := AsString(@Value)
  else
    inherited;
end;

function TJCookieObject.ToString(Value: PJValue): String;
begin
  Result := FCookie.ServerText;
end;

{ TJResponseObject }

procedure TJResponseObject.Clear;
begin
  ClearProperties;
  FCode := 999;
  FText := '';
end;

constructor TJResponseObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Response');

  FCookie := TJCookieObject.Create(FEngine,nil,False);
  FCookie.IncRef;
end;

destructor TJResponseObject.Destroy;
begin
  FCookie.DecRef;
  inherited;
end;

function TJResponseObject.GetCookie: TJCookieObject;
begin
  Result := FCookie;
end;

procedure TJResponseObject.GetPropertyList(List: TStringList);
//全てのメンバを得る
begin
  inherited GetKeyList(List,[vtString],[]);
end;

procedure TJResponseObject.SetStatus(AVersion: String; ACode: Integer; AText: String);
begin
  FVersion := AVersion;
  FCode := ACode;
  FText := AText;
end;

function TJResponseObject.ToString(Value: PJValue): String;
var
  sl: TStringList;
  i: Integer;
  v: TJValue;
begin
  sl := TStringList.Create;
  try
    GetPropertyList(sl);
    Result := 'HTTP/' + FVersion + ' ' + IntToStr(FCode) + ' ' + FText + CRLF;
    for i := 0 to sl.Count - 1 do
    begin
      v := GetValue(sl[i],True);
      Result := Result + sl[i] + ': ' +  AsString(@v) + CRLF;
    end;

    Result := TrimRight(Result);
  finally
    sl.Free;
  end;
end;

{ TJHTTPObject }

procedure TJHTTPObject.AfterResponse;
//レスポンス結果
var
  sl: TStringList;
  i: Integer;
begin
  //response
  FResponseHeader.Clear;
  FResponseHeader.SetStatus(FHTTP.ResHeader.Version,FHTTP.StatusNo,FHTTP.Status);
  FResponseHeader.FCookie.FCookie.Assign(FHTTP.ResHeader.Cookie);
  sl := FHTTP.ResHeader.Hash.KeyList;
  for i := 0 to sl.Count - 1 do
    FResponseHeader.SetValue(sl[i],BuildString(FHTTP.ResHeader.Hash[sl[i]]),True);
end;

procedure TJHTTPObject.BeforeRequest;
//リクエスト準備
var
  sl: TStringList;
  v: TJValue;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    //request
    FHTTP.ReqHeader.Clear;
    FRequestHeader.GetPropertyList(sl);
    for i := 0 to sl.Count - 1 do
    begin
      v := FRequestHeader.GetValue(sl[i],True);
      FHTTP.ReqHeader[sl[i]] := AsString(@v);
    end;
  finally
    sl.Free;
  end;
end;

constructor TJHTTPObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  FHTTP := CreateHTTP;
  FHTTP.OnStatus := OnStatus;
  FHTTP.OnAboutToSend := OnRequest;
  FHTTP.OnConnect := OnConnect;
  FHTTP.OnDisconnect := OnDisconnect;
  FHTTP.OnError := OnError;
  FHTTP.OnFailure := OnFailure;
  FHTTP.OnPacketRecvd := OnPacketRecvd;
  FHTTP.OnPacketSent := OnPacketSend;
  FHTTP.OnResponse := OnResponse;
  FHTTP.OnSuccess := OnSuccess;

  RegistEventName('onStatus');
  RegistEventName('onRequest');
  RegistEventName('onConnect');
  RegistEventName('onDisconnect');
  RegistEventName('onError');
  RegistEventName('onFailure');
  RegistEventName('onPacketRecvd');
  RegistEventName('onPacketSend');
  RegistEventName('onResponse');
  RegistEventName('onSuccess');

  FRequestHeader := TJRequestObject.Create(FEngine,nil,False);
  FRequestHeader.IncRef;
  FResponseHeader := TJResponseObject.Create(FEngine,nil,False);
  FResponseHeader.IncRef;

  RegistMethod('get',DoGet);
  RegistMethod('getFile',DoGetFile);
  RegistMethod('post',DoPost);
  RegistMethod('head',DoHead);
  RegistMethod('request',DoRequest);
  RegistMethod('response',DoResponse);
  RegistMethod('capture',DoCapture);
  RegistMethod('readln',DoReadln);
  RegistMethod('read',DoRead);
  RegistMethod('writeln',DoWriteln);
  RegistMethod('write',DoWrite);
  RegistMethod('connect',DoConnect);
  RegistMethod('disconnect',DoDisconnect);
  RegistMethod('open',DoConnect);
  RegistMethod('close',DoDisconnect);
end;

function TJHTTPObject.CreateHttp: TgHttp;
begin
  RegistName('HTTP');
  Result := TgHttp.Create;
end;

destructor TJHTTPObject.Destroy;
begin
  FHTTP.OnStatus := nil;
  FreeAndNil(FHTTP);
  FRequestHeader.DecRef;
  FResponseHeader.DecRef;
  inherited;           
end;

function TJHTTPObject.DoCapture(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FHTTP.CaptureFile(AsString(@v));
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.capture error');
    end;
  end;
end;

function TJHTTPObject.DoConnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FHTTP.Connect;
  except
    raise EJThrow.Create(E_SOCKET,'HTTP.connect error');
  end;
end;

function TJHTTPObject.DoDisconnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FHTTP.Disconnect;
  except
    raise EJThrow.Create(E_SOCKET,'HTTP.disconnect error');
  end;
end;

function TJHTTPObject.DoGet(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    try
      BeforeRequest;

      v := Param[0];
      Result := BuildString(FHTTP.Get(AsString(@v)));

      AfterResponse;    
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.get error');
    end;
  end;
end;

function TJHTTPObject.DoGetFile(Param: TJValueList): TJValue;
var
  v,f: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    try
      BeforeRequest;

      v := Param[0];
      f := Param[1];
      FHTTP.GetFile(AsString(@v),AsString(@f));

      AfterResponse;
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.get error');
    end;
  end; 
end;

function TJHTTPObject.DoHead(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin      
    try
      BeforeRequest;

      v := Param[0];
      FHTTP.Head(AsString(@v));

      AfterResponse;
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.head error');
    end;
  end;
end;

function TJHTTPObject.DoPost(Param: TJValueList): TJValue;
var
  v,data: TJValue;
begin
  EmptyValue(Result);
  if IsParam2(Param) then
  begin
    try
      BeforeRequest;

      v := Param[0];
      data := Param[1];
      Result := BuildString(FHTTP.Post(AsString(@v),AsString(@data)));

      AfterResponse;
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.post error');
    end;
  end;

end;

function TJHTTPObject.DoRead(Param: TJValueList): TJValue;
var
  v: TJValue;
  len: Integer;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    len := AsInteger(@v);
  end
  else
    len := -1;

  try
    Result := BuildString(FHTTP.Read(len))
  except
    raise EJThrow.Create(E_SOCKET,'HTTP.read error');
  end;
end;

function TJHTTPObject.DoReadln(Param: TJValueList): TJValue;
begin
  try
    Result := BuildString(FHTTP.Readln)
  except
    raise EJThrow.Create(E_SOCKET,'HTTP.readln error');
  end;
end;

function TJHTTPObject.DoRequest(Param: TJValueList): TJValue;
var
  meth,ur,data: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    try
      BeforeRequest;

      meth := Param[0];
      ur := Param[1];
      if IsParam3(Param) then
        data := Param[2]
      else
        data := BuildString('');

      FHTTP.Request(AsString(@meth),AsString(@ur),AsString(@data));
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.request error');
    end;
  end;
end;

function TJHTTPObject.DoResponse(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try try
    FHTTP.Response;
  finally
    //response
    FResponseHeader.Clear;
    FResponseHeader.SetStatus(FHTTP.ResHeader.Version,FHTTP.StatusNo,FHTTP.Status);
    FResponseHeader.FCookie.FCookie.Assign(FHTTP.ResHeader.Cookie);

    AfterResponse;
  end;
  except
    raise EJThrow.Create(E_SOCKET,'HTTP.response error');
  end; 
end;

function TJHTTPObject.DoWrite(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FHTTP.Write(AsString(@v));
    except
      raise EJThrow.Create(E_SOCKET,'HTTP.write error');
    end;
  end;
end;

function TJHTTPObject.DoWriteln(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
  end
  else
    s := '';

  try
    FHTTP.Writeln(s);
  except
    raise EJThrow.Create(E_SOCKET,'HTTP.writeln error');
  end;
end;

function TJHTTPObject.GetAutoRedirect: Boolean;
begin
  Result := FHTTP.AutoRedirect;
end;

function TJHTTPObject.GetBytesRead: Integer;
begin
  Result := FHTTP.BytesRecvd;
end;

function TJHTTPObject.GetBytesTotal: Integer;
begin
  Result := FHTTP.BytesTotal;
end;

function TJHTTPObject.GetBytesWrote: Integer;
begin
  Result := FHTTP.BytesSent;
end;

function TJHTTPObject.GetLength: Integer;
begin
  Result := FHTTP.BodyLength;
end;

function TJHTTPObject.GetProxy: String;
begin
  Result := FHTTP.Proxy;
end;

function TJHTTPObject.GetRequestHeader: TJObject;
begin
  Result := FRequestHeader;
end;

function TJHTTPObject.GetResponseHeader: TJResponseObject;
begin
  Result := FResponseHeader;
end;

function TJHTTPObject.GetTimeout: Integer;
begin
  Result := FHTTP.Timeout;
end;

function TJHTTPObject.GetVersion: String;
begin
  Result :=  FHTTP.Version;
end;

procedure TJHTTPObject.OnFailure(cmd: THTTPCmdType);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onFailure') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onFailure',param);
  finally
    param.Free;
  end;
end;

procedure TJHTTPObject.OnRequest(Sender: TObject; var Url, Proxy: String;
  Headers: THashTable);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onRequest') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onRequest',param);
  finally
    param.Free;
  end;
end;

procedure TJHTTPObject.OnResponse(Sender: TObject; const Url, Proxy,
  StatusMsg: String; var RedirectUrl: String; var StatusNumber: Integer;
  Headers: THashTable);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onResponse') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onResponse',param);
  finally
    param.Free;
  end;
end;

procedure TJHTTPObject.OnSuccess(cmd: THTTPCmdType);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onSuccess') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onSuccess',param);
  finally
    param.Free;
  end;
end;

procedure TJHTTPObject.SetAutoRedirect(const Value: Boolean);
begin
  FHTTP.AutoRedirect := Value;
end;

procedure TJHTTPObject.SetProxy(const Value: String);
begin
  FHTTP.Proxy := Value;
end;

procedure TJHTTPObject.SetTimeout(const Value: Integer);
begin
  FHTTP.Timeout := Value;
end;

procedure TJHTTPObject.SetVersion(const Value: String);
begin
  FHTTP.Version := Value;
end;


{ TJHTTPSObject }

function TJHTTPSObject.CreateHttp: TgHttp;
begin
  RegistName('HTTPS');
  Result := TgHTTPS.Create;
end;

{ TJTCPSocketObject }

constructor TJTCPSocketObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('TCPSocket');

  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FSock.CreateFromServer(BUFFER_SIZE,AsInteger(@v));
    except

      raise EJThrow.Create(E_SOCKET,'TCPSocket.create error');
    end;
  end
  else
    FSock := TgSocket.Create(BUFFER_SIZE);

  FSock.OnStatus := OnStatus;
  FSock.OnConnect := OnConnect;
  FSock.OnDisconnect := OnDisconnect;
  FSock.OnError := OnError;
  FSock.OnPacketRecvd := OnPacketRecvd;
  FSock.OnPacketSent := OnPacketSend;

  RegistEventName('onStatus');
  RegistEventName('onConnect');
  RegistEventName('onDisconnect');
  RegistEventName('onError');
  RegistEventName('onPacketRecvd');
  RegistEventName('onPacketSend');

  RegistMethod('connect',DoConnect);
  RegistMethod('disconnect',DoDisconnect);
  RegistMethod('open',DoConnect);
  RegistMethod('close',DoDisconnect);
  RegistMethod('abort',DoAbort);
  RegistMethod('read',DoRead);
  RegistMethod('readln',DoReadln);
  RegistMethod('write',DoWrite);
  RegistMethod('writeln',DoWriteln);
  RegistMethod('capture',DoCapture);
  RegistMethod('sendFile',DoSendFile);
  RegistMethod('bind',DoBind);
  RegistMethod('accept',DoAccept);
  RegistMethod('isConnected',DoIsConnected);
end;

destructor TJTCPSocketObject.Destroy;
begin
  FSock.OnStatus := nil;
  FSock.Free;
  inherited;
end;

function TJTCPSocketObject.DoAbort(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSock.Abort;
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.abort error');
  end;                                                     
end;

function TJTCPSocketObject.DoAccept(Param: TJValueList): TJValue;
var
  s: TSocket;
  obj: TJTCPSocketObject;
  list: TJValueList;
begin
  try
    s := FSock.Accept;
    if s = INVALID_SOCKET then
      raise EJThrow.Create(E_SOCKET,'TCPSocket.accept error')
    else begin
      list := TJValueList.Create;
      try
        list.Add(BuildInteger(S));
        //socketを送る
        obj := TJTCPSocketObject.Create(FEngine,list);
        Result := BuildObject(obj);
      finally
        list.Free;
      end;
    end;
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.accept error');
  end; 
end;

function TJTCPSocketObject.DoBind(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSock.Bind;
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.bind error');
  end; 
end;

function TJTCPSocketObject.DoCapture(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FSock.CaptureFile(AsString(@v));
    except
      raise EJThrow.Create(E_SOCKET,'TCPSocket.capture error');
    end;
  end;
end;

function TJTCPSocketObject.DoConnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSock.Connect;
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.connect error');
  end;
end;

function TJTCPSocketObject.DoDisconnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSock.Disconnect;
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.disconnect error');
  end;
end;

function TJTCPSocketObject.DoIsConnected(Param: TJValueList): TJValue;
begin
  Result := BuildBool(FSock.Connected);
end;

function TJTCPSocketObject.DoRead(Param: TJValueList): TJValue;
var
  v: TJValue;
  len: Integer;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    len := AsInteger(@v);
  end
  else
    len := -1;

  try
    Result := BuildString(FSock.Read(len));
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.read error');
  end;
end;

function TJTCPSocketObject.DoReadln(Param: TJValueList): TJValue;
begin
  EmptyValue(Result);
  try
    Result := BuildString(FSock.Readln);
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.readln error');
  end; 
end;

function TJTCPSocketObject.DoSendFile(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FSock.SendFile(AsString(@v));
    except
      raise EJThrow.Create(E_SOCKET,'TCPSocket.sendFile error');
    end;
  end;
end;

function TJTCPSocketObject.DoWrite(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FSock.Write(AsString(@v));
    except
      raise EJThrow.Create(E_SOCKET,'TCPSocket.write error');
    end;
  end;
end;

function TJTCPSocketObject.DoWriteln(Param: TJValueList): TJValue;
var
  v: TJValue;
  s: String;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    s := AsString(@v);
  end
  else
    s := '';

  try
    FSock.Writeln(s);
  except
    raise EJThrow.Create(E_SOCKET,'TCPSocket.writeln error');
  end;
end;

function TJTCPSocketObject.GetBytesRead: Integer;
begin
  Result := FSock.BytesRecvd;
end;

function TJTCPSocketObject.GetBytesTotal: Integer;
begin
  Result := FSock.BytesTotal;
end;

function TJTCPSocketObject.GetBytesWrote: Integer;
begin
  Result := FSock.BytesSent;
end;

function TJTCPSocketObject.GetHost: String;
begin
  Result := FSock.Host;
end;

function TJTCPSocketObject.GetPort: Integer;
begin
  Result := FSock.Port;
end;

function TJTCPSocketObject.GetTimeout: Integer;
begin
  Result := FSock.Timeout;
end;

procedure TJTCPSocketObject.SetHost(const Value: String);
begin
  FSock.Host := Value;
end;

procedure TJTCPSocketObject.SetPort(const Value: Integer);
begin
  FSock.Port := Value;
end;

procedure TJTCPSocketObject.SetTimeout(const Value: Integer);
begin
  FSock.Timeout := Value;
end;

{ TJBaseSocketObject }

procedure TJBaseSocketObject.OnConnect(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onConnect') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onConnect',param);
  finally
    param.Free;
  end;
end;

procedure TJBaseSocketObject.OnDisconnect(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onDisconnect') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onDisconnect',param);
  finally
    param.Free;
  end;
end;

procedure TJBaseSocketObject.OnError(Sender: TObject; Errno: Word;
  Errmsg: String);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onError') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    param.Add(Errno);
    param.add(Errmsg);
    CallEvent('','onError',param);
  finally
    param.Free;
  end;
end;

procedure TJBaseSocketObject.OnPacketRecvd(Sender: TObject;
  Bytes: Integer; var DoStop: Boolean);
//イベント
var
  param: TJValueList;
  stp: TJBooleanObject;
begin
  if not IsCallEvent('onRead') then
    Exit;

  stp := TJBooleanObject.Create(FEngine);
  stp.bool := DoStop;

  param := TJValueList.Create;
  try
    param.Add(Self);
    param.Add(Bytes);
    param.Add(stp);
    CallEvent('','onRead',param);
  finally
    DoStop := stp.bool;
    param.Free;
  end;
end;

procedure TJBaseSocketObject.OnPacketSend(Sender: TObject;
  Bytes: Integer; var DoStop: Boolean);
//イベント
var
  param: TJValueList;
  stp: TJBooleanObject;
begin
  if not IsCallEvent('onWrite') then
    Exit;

  stp := TJBooleanObject.Create(FEngine);
  stp.bool := DoStop;

  param := TJValueList.Create;
  try
    param.Add(Self);
    param.Add(Bytes);
    param.Add(stp);
    CallEvent('','onWrite',param);
  finally
    DoStop := stp.bool;
    param.Free;
  end;
end;

procedure TJBaseSocketObject.OnStatus(Sender: TObject;
  const Status: String);
//イベント
var
  param: TJValueList;
begin
  //デバッグイベントを先に
  if FDebug and Assigned(FOnPrint) then
    FOnPrint(Status);

  if not IsCallEvent('onStatus') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    param.Add(Status);
    CallEvent('','onStatus',param);
  finally
    param.Free;
  end;
end;


{ TJMailObject }

procedure TJMailObject.Clear;
begin
  inherited;
  FMailMessage.Clear;
end;

constructor TJMailObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Mail');

  FMailMessage := TMailMessage.Create;
  FAttach := TJStringsObject.Create(FEngine,nil,False);
  FAttach.IncRef;
  RegistMethod('clear',DoClear);
end;

destructor TJMailObject.Destroy;
begin
  FreeAndNil(FMailMessage);
  FAttach.DecRef;
  inherited;
end;

function TJMailObject.DoClear(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  Clear;
end;

function TJMailObject.GetAttachments: TJObject;
begin
  FAttach.Strings.Assign(FMailMessage.Attachments);
  Result := FAttach;
end;

function TJMailObject.GetBody: String;
begin
  Result := FMailMessage.Body.Text;
end;

function TJMailObject.GetHeader: String;
begin
  Result := FMailMessage.Head.Text;
end;

function TJMailObject.GetLength: Integer;
begin
  Result := FMailMessage.Size;
end;

procedure TJMailObject.GetPropertyList(List: TStringList);
begin
  List.Assign(FMailMessage.Hash.KeyList);
end;

function TJMailObject.GetMessage: String;
begin
  Result := FMailMessage.Message;
end;

function TJMailObject.GetNumber: Integer;
begin
  Result := FMailMessage.Number;
end;

function TJMailObject.GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;
begin
  EmptyValue(Result);
  if GetDefaultProperty(Self,S,Result,FEngine) then
    Exit
  else begin
    if ArrayStyle then
    begin
      //配列
      if FMailMessage.Hash.HasKey(S) then
        Result := BuildString(FMailMessage.Value[S])
      else
        raise EJThrow.Create(E_KEY,S);
    end
    else begin
      //メンバ
      if FMailMessage.Hash.HasKey(S) then
        Result := BuildString(FMailMessage.Value[S])
      else
        raise EJThrow.Create(E_NAME,S);
    end;
  end;   
end;

procedure TJMailObject.SetMessage(const Value: String);
begin
  FMailMessage.Message := Value;
end;

procedure TJMailObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList = nil);
begin
  if SetDefaultProperty(Self,S,Value) then
    Exit
  else
    FMailMessage.Hash[S] := AsString(@Value);
end;

{ TJPOP3Object }

constructor TJPOP3Object.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('POP3');

  FPOP := TgPOP3.Create(BUFFER_SIZE);
  FPOP.OnStatus := OnStatus;
  FPOP.OnConnect := OnConnect;
  FPOP.OnDisconnect := OnDisconnect;
  FPOP.OnError := OnError;
  FPOP.OnFailure := OnFailure;
  FPOP.OnPacketRecvd := OnPacketRecvd;
  FPOP.OnPacketSent := OnPacketSend;
  FPOP.OnSuccess := OnSuccess;

  RegistEventName('onStatus');
  RegistEventName('onConnect');
  RegistEventName('onDisconnect');
  RegistEventName('onError');
  RegistEventName('onFailure');
  RegistEventName('onPacketRecvd');
  RegistEventName('onPacketSend');
  RegistEventName('onSuccess');

  if IsParam1(Param) then
  begin
    v := Param[0];
    FPOP.Host := AsString(@v);

    if IsParam2(Param) then
    begin
      v := Param[1];
      FPOP.Port := AsInteger(@v);
    end;
  end;

  FMail := TJMailObject.Create(FEngine,nil,False);
  FMail.IncRef;

  RegistMethod('connect', DoConnect);
  RegistMethod('disconnect', DoDisconnect);
  RegistMethod('open', DoConnect);
  RegistMethod('close', DoDisconnect);
  RegistMethod('getMail', DoGetMail);
  RegistMethod('getSummary', DoGetSummary);
  RegistMethod('delete', DoDelete);
end;

destructor TJPOP3Object.Destroy;
begin
  FPOP.OnStatus := nil;
  FreeAndNil(FPOP);
  FMail.DecRef;
  inherited;
end;

function TJPOP3Object.DoConnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FPOP.Connect;
  except
    raise EJThrow.Create(E_SOCKET,'POP3.connect');
  end;
end;

function TJPOP3Object.DoDelete(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FPOP.DeleteMailMessage(AsInteger(@v));
    except
      raise EJThrow.Create(E_SOCKET,'POP3.delete');
    end;
  end;
end;

function TJPOP3Object.DoDisconnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FPOP.Disconnect;
  except
    raise EJThrow.Create(E_SOCKET,'POP3.disconnect');
  end;
end;

function TJPOP3Object.DoGetMail(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FPOP.GetMailMessage(AsInteger(@v));
      FMail.FMailMessage.Assign(FPOP.MailMessage);
      Result := BuildObject(FMail);
    except
      raise EJThrow.Create(E_SOCKET,'POP3.getMail');
    end;
  end;
end;

function TJPOP3Object.DoGetSummary(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  EmptyValue(Result);
  if IsParam1(Param) then
  begin
    v := Param[0];
    try
      FPOP.GetSummary(AsInteger(@v));
      FMail.FMailMessage.Assign(FPOP.MailMessage);
      Result := BuildObject(FMail);
    except
      raise EJThrow.Create(E_SOCKET,'POP3.getSummary');
    end;
  end;
end;

function TJPOP3Object.GetAttachFilePath: String;
begin
  Result := FPOP.AttachFilePath;
end;

function TJPOP3Object.GetBytesRead: Integer;
begin
  Result := FPOP.BytesRecvd;
end;

function TJPOP3Object.GetBytesTotal: Integer;
begin
  Result := FPOP.BytesTotal;
end;

function TJPOP3Object.GetBytesWrote: Integer;
begin
  Result := FPOP.BytesSent;
end;

function TJPOP3Object.GetDeleteOnRead: Boolean;
begin
  Result := FPOP.DeleteOnRead;
end;

function TJPOP3Object.GetHost: String;
begin
  Result := FPOP.Host;
end;

function TJPOP3Object.GetMail: TJMailObject;
begin
  Result := FMail;
end;

function TJPOP3Object.GetMailCount: Integer;
begin
  Result := FPOP.MailCount;
end;

function TJPOP3Object.GetPassword: String;
begin
  Result := FPOP.Password;
end;

function TJPOP3Object.GetPort: Integer;
begin
  Result := FPOP.Port;
end;

function TJPOP3Object.GetTimeout: Integer;
begin
  Result := FPOP.Timeout;
end;

function TJPOP3Object.GetUserId: String;
begin
  Result := FPOP.UserId;
end;

procedure TJPOP3Object.OnFailure(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onFailure') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onFailure',param);
  finally
    param.Free;
  end;
end;

procedure TJPOP3Object.OnSuccess(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onSuccess') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(Self);
    CallEvent('','onSuccess',param);
  finally
    param.Free;
  end;
end;

procedure TJPOP3Object.SetAttachFilePath(const Value: String);
begin
  FPOP.AttachFilePath := Value;
end;

procedure TJPOP3Object.SetDeleteOnRead(const Value: Boolean);
begin
  FPOP.DeleteOnRead := Value;
end;

procedure TJPOP3Object.SetHost(const Value: String);
begin
  FPOP.Host := Value;
end;

procedure TJPOP3Object.SetPassword(const Value: String);
begin
  FPOP.Password := Value;
end;

procedure TJPOP3Object.SetPort(const Value: Integer);
begin
  FPOP.Port := Value;
end;

procedure TJPOP3Object.SetTimeout(const Value: Integer);
begin
  FPOP.Timeout := Value;
end;

procedure TJPOP3Object.SetUserId(const Value: String);
begin
  FPOP.UserId := Value;
end;

{ TJSMTPObject }

constructor TJSMTPObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('SMTP');
  FSMTP := TgSMTP.Create(BUFFER_SIZE);
  FSMTP.OnStatus := OnStatus;
  FSMTP.OnConnect := OnConnect;
  FSMTP.OnDisconnect := OnDisconnect;
  FSMTP.OnError := OnError;
  FSMTP.OnPacketRecvd := OnPacketRecvd;
  FSMTP.OnPacketSent := OnPacketSend;

  RegistEventName('onStatus');
  RegistEventName('onConnect');
  RegistEventName('onDisconnect');
  RegistEventName('onError');
  RegistEventName('onPacketRecvd');
  RegistEventName('onPacketSend');



  if IsParam1(Param) then
  begin
    v := Param[0];
    FSMTP.Host := AsString(@v);

    if IsParam2(Param) then
    begin
      v := Param[1];
      FSMTP.Port := AsInteger(@v);
    end;
  end;

  FMail := TJMailObject.Create(FEngine,nil,False);
  FMail.IncRef;

  RegistMethod('connect', DoConnect);
  RegistMethod('disconnect', DoDisconnect);
  RegistMethod('open', DoConnect);
  RegistMethod('close', DoDisconnect);
  RegistMethod('sendMail', DoSendMail);
end;

destructor TJSMTPObject.Destroy;
begin
  FSMTP.OnStatus := nil;
  FreeAndNil(FSMTP);
  FMail.DecRef;
  inherited;
end;

function TJSMTPObject.DoConnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSMTP.Connect;
  except
    raise EJThrow.Create(E_SOCKET,'SMTP.connect');
  end;
end;

function TJSMTPObject.DoDisconnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSMTP.Disconnect;
  except
    raise EJThrow.Create(E_SOCKET,'SMTP.disconnect');
  end;
end;

function TJSMTPObject.DoSendMail(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FSMTP.MailMessage.Assign(FMail.FMailMessage);
    FSMTP.SendMail;
  except
    raise EJThrow.Create(E_SOCKET,'SMTP.sendMail');
  end;
end;

function TJSMTPObject.GetBytesRead: Integer;
begin
  Result := FSMTP.BytesRecvd;
end;

function TJSMTPObject.GetBytesTotal: Integer;
begin
  Result := FSMTP.BytesTotal;
end;

function TJSMTPObject.GetBytesWrote: Integer;
begin
  Result := FSMTP.BytesSent;
end;

function TJSMTPObject.GetHost: String;
begin
  Result := FSMTP.Host;
end;

function TJSMTPObject.GetMail: TJMailObject;
begin
  Result := FMail;
end;

function TJSMTPObject.GetPort: Integer;
begin
  Result := FSMTP.Port;
end;

function TJSMTPObject.GetTimeout: Integer;
begin
  Result := FSMTP.Timeout;
end;

procedure TJSMTPObject.SetHost(const Value: String);
begin
  FSMTP.Host := Value;
end;

procedure TJSMTPObject.SetPort(const Value: Integer);
begin
  FSMTP.Port := Value;
end;

procedure TJSMTPObject.SetTimeout(const Value: Integer);
begin
  FSMTP.Timeout := Value;
end; 


{ TJHtmlTagObject }

procedure TJHtmlTagObject.AssignTag(Source: THtmlTag);
var
  sl: TStringList;
  i: Integer;
begin
  //clear
  ClearMembers;
  RegistMethods;

  FName := Source.Name;
  sl := TStringList.Create;
  try
    sl.Text := Source.Keys;
    for i := 0 to sl.Count - 1 do
      SetValue(sl[i],BuildString(Source[sl[i]]),True);
  finally
    sl.Free;
  end;
end;

constructor TJHtmlTagObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('HtmlTag');
  //clear
  ClearMembers;
  RegistMethods;
end;

procedure TJHtmlTagObject.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList = nil);
begin
  if ArrayStyle then
    inherited;
end;

function TJHtmlTagObject.ToString(Value: PJValue): String;
var
  keys: TSTringList;
  i: Integer;
  v: TJValue;
  s: String;
begin
  Result := '<' + FName + ' ';
  //keys := Members.KeyList;
  keys := TStringList.Create;
  try
    GetPropertyList(Keys);
    s := '';
    for i := 0 to keys.Count - 1 do
    begin
      v := GetValue(keys[i],True);
      if AsString(@v) <> '' then
        s := s + keys[i] + '="' + AsSTring(@v) + '" '
      else
        s := s + keys[i] + ' ';
    end;

    Result := Result + TrimRight(s) + '>';
  finally
    keys.Free;
  end;
end;

{ TJHtmlParserObject }

constructor TJHtmlParserObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('HtmlParser');
  RegistMethod('parse',DoParse);
  RegistMethod('clear',DoClear);
  RegistMethod('parseFile',DoParseFile);

  FParser := THtmlParser.Create('');
  DoParse(Param);
end;

destructor TJHtmlParserObject.Destroy;
begin
  FreeAndNil(FParser);
  inherited;
end;

function TJHtmlParserObject.DoClear(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  FParser.Clear;
end;

function TJHtmlParserObject.DoParse(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    FHtml := AsString(@v);
    FParser.Parse(FHtml);
  end;
end;

function TJHtmlParserObject.DoParseFile(Param: TJValueList): TJValue;
var
  sl: TStringList;
  f: String;
  v: TJValue;
begin
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];
    f := AsString(@v);
    if FileExists(f) then
    begin
      sl := TStringList.Create;
      try
        sl.LoadFromFile(f);
        FHtml := sl.Text;
        FParser.Parse(FHtml);
      finally
        sl.Free;
      end;
    end;
  end;
end;

function TJHtmlParserObject.GetCount: Integer;
begin
  Result := FParser.Count;
end;

function TJHtmlParserObject.GetHtml: String;
begin
  Result := FHtml;
end;

function TJHtmlParserObject.GetText: String;
begin
  Result := FParser.Text;
end;

function TJHtmlParserObject.GetValue(S: String;
  ArrayStyle: Boolean; Param: TJValueList = nil): TJValue;

  function TryNumber(S: String; var Num: Integer): Boolean;
  begin
    Num := 0;
    Result := False;
    try
      Num := StrToInt(S);
      Result := True;
    except
    end;
  end;
  
var
  index: Integer;
begin
  if ArrayStyle and TryNumber(S,index) then
    Result := GetItem(index)
  else
    Result := inherited GetValue(S,ArrayStyle);
end;

function TJHtmlParserObject.ToString(Value: PJValue): String;
begin
  Result := FHtml;
end;

function TJHtmlParserObject.GetItem(Index: Integer): TJValue;
var
  obj: TJHtmlTagObject;
  tag: THtmlTag;
begin
  tag := FParser[index];
  if not Assigned(tag) then
    Result := BuildNull
  else begin
    obj := TJHtmlTagObject.Create(FEngine);
    obj.AssignTag(tag);
    Result := BuildObject(obj);
  end;
end;

class function TJHtmlParserObject.IsArray: Boolean;
begin
  Result := True;
end;

{ TJFtpFilePropertyObject }

constructor TJFtpFilePropertyObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('FileProperty');
end;

function TJFtpFilePropertyObject.GetAttribute: Integer;
//ファイル権限
begin
  Result := GetFtpFileAttr(FFileProperty);
end;

function TJFtpFilePropertyObject.GetFileType: String;
//ファイルタイプ
begin
  Result := '';
  case FFileProperty.FileType of
    ftDir: Result := 'd';
    ftFile: Result := '-';
    ftLink: Result := 'l';
  end;
end;

function TJFtpFilePropertyObject.GetModified: TJDateObject;
//日付を返す
begin
  Result := TJDateObject.Create(FEngine);
  Result.LocalTime := FFileProperty.ModifiDate;
end;

function TJFtpFilePropertyObject.GetName: String;
//ファイル名
begin
  Result := FFileProperty.Name;
end;

function TJFtpFilePropertyObject.GetSize: Integer;
//ファイルサイズ
begin
  Result := FFileProperty.Size;
end;  

function TJFtpFilePropertyObject.ToString(Value: PJValue): String;
begin
  Result := FFileProperty.Line;
end;

{ TJFtpObject }

constructor TJFtpObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('FTP');

  FFtp := TgFTP.Create;
  FFtp.OnStatus := OnStatus;
  FFtp.OnFailure := OnFailure;
  FFtp.OnSuccess := OnSuccess;
  FFtp.OnConnect := OnConnect;
  FFtp.OnDisconnect := OnDisconnect;
  FFtp.OnError := OnError;
  FFtp.OnPacketRecvd := OnPacketRecvd;
  FFtp.OnPacketSent := OnPacketSend;

  RegistEventName('onStatus');
  RegistEventName('onConnect');
  RegistEventName('onDisconnect');
  RegistEventName('onError');
  RegistEventName('onFailure');
  RegistEventName('onPacketRecvd');
  RegistEventName('onPacketSend');
  RegistEventName('onSuccess');

  if IsParam1(Param) then
  begin
    v := Param[0];
    FFtp.Host := AsString(@v);

    if IsParam2(Param) then
    begin
      v := Param[1];
      FFtp.Port := AsInteger(@v);
    end;
  end;

  RegistMethod('connect',DoConnect);
  RegistMethod('disconnect',DoDisconnect);
  RegistMethod('abort',DoAbort);
  RegistMethod('changeDir',DoChangeDir);
  RegistMethod('delete',DoDelete);
  RegistMethod('download',DoDownload);
  RegistMethod('downloadRestore',DoDownloadRestore);
  RegistMethod('list',DoList);
  RegistMethod('nlist',DoNList);
  RegistMethod('makeDir',DoMakeDir);
  RegistMethod('reinitialize',DoReinitialize);
  RegistMethod('removeDir',DoRemoveDir);
  RegistMethod('rename',DoRename);
  RegistMethod('upload',DoUpload);
  RegistMethod('uploadAppend',DoUploadAppend);
  RegistMethod('uploadRestore',DoUploadRestore);
  RegistMethod('uploadUnique',DoUploadUnique);
  RegistMethod('printWorkDir',DoPrintWorkDir);
  RegistMethod('findFiles',DoFindFiles);
  RegistMethod('login',DoLogin);
  RegistMethod('quit',DoQuit);
  RegistMethod('type',DoType);
  RegistMethod('command',DoCommand);
end;

destructor TJFtpObject.Destroy;
begin
  FreeAndNil(FFtp);
  inherited;
end;

function TJFtpObject.DoAbort(Param: TJValueList): TJValue;
//ABOR
begin
  Result := BuildObject(Self);
  try
    FFtp.Abort;
  except
    raise EJThrow.Create(E_SOCKET,'FTP.abort error');
  end;
end;

function TJFtpObject.DoChangeDir(Param: TJValueList): TJValue;
//CWD
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      FFtp.ChangeDir(AsString(@v));
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.changeDir error');
  end;
end;

function TJFtpObject.DoCommand(Param: TJValueList): TJValue;
//コマンドを送る
var
  v: TJValue;
begin
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      FFtp.DoCommand(AsString(@v));
      Result := BuildString(FFtp.TransActionReply);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.delete error');
  end;
end;

function TJFtpObject.DoConnect(Param: TJValueList): TJValue;
//接続
begin
  Result := BuildObject(Self);
  try
    FFtp.Connect;
  except
    raise EJThrow.Create(E_SOCKET,'FTP.connect error');
  end;
end;

function TJFtpObject.DoDelete(Param: TJValueList): TJValue;
//DELE
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      FFtp.Delete(AsString(@v));
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.delete error');
  end;
end;

function TJFtpObject.DoDisconnect(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FFtp.Disconnect;
  except
    raise EJThrow.Create(E_SOCKET,'FTP.disconnect error');
  end;   
end;

function TJFtpObject.DoDownload(Param: TJValueList): TJValue;
var
  v: TJValue;
  r,l: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam2(Param) then
    begin
      v := Param[0];
      r := AsString(@v);

      v := Param[1];
      l := AsString(@v);
      FFtp.Download(r,l);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.download error');
  end;
end;

function TJFtpObject.DoDownloadRestore(Param: TJValueList): TJValue;
var
  v: TJValue;
  r,l: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam2(Param) then
    begin
      v := Param[0];
      r := AsString(@v);

      v := Param[1];
      l := AsString(@v);
      FFtp.DownloadRestore(r,l);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.downloadRestore error');
  end;
end;

function TJFtpObject.DoFindFiles(Param: TJValueList): TJValue;
//ファイルをワイルドカードで検索
var
  v: TJValue;
  dir,wild: String;
  recurce: Boolean;
  sl: TStringList;
  so: TJStringsObject;
begin
  dir := '';
  wild := '*.*';
  recurce := False;

  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      dir := AsString(@v);

      if IsParam2(Param) then
      begin
        v := Param[1];
        wild := AsString(@v);

        if IsParam3(Param) then
        begin
          v := Param[2];
          recurce := AsBool(@v);
        end;
      end;

      sl := TStringList.Create;
      try
        FFtp.FindFiles(dir,wild,sl,recurce);
        so := TJStringsObject.Create(FEngine);
        so.Strings.Assign(sl);
        Result := BuildObject(so);
      finally
        sl.Free;
      end;
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.findFiles error');
  end;     
end;

function TJFtpObject.DoList(Param: TJValueList): TJValue;
//LIST
var
  i: Integer;
  ao: TJArrayObject;
  fp: TJFtpFilePropertyObject;
begin
  try
    FFtp.List;
    ao := TJArrayObject.Create(FEngine);
    Result := BuildObject(ao);

    for i := 0 to FFtp.FTPDirectoryList.Count - 1 do
    begin
      //ArrayにFTPFilePropertyを入れる
      fp := TJFtpFilePropertyObject.Create(FEngine);
      fp.FileProperty := FFtp.FtpDirectoryList[i]^;
      ao.Add(BuildObject(fp));
    end;    
  except
    raise EJThrow.Create(E_SOCKET,'FTP.list error');
  end;
end;

function TJFtpObject.DoLogin(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      FFtp.UserId := AsString(@v);

      if IsParam2(Param) then
      begin
        v := Param[1];
        FFtp.Password := AsString(@v);
      end;
    end;

    Result := DoConnect(Param);
  except
    raise EJThrow.Create(E_SOCKET,'FTP.rename error');
  end;
end;

function TJFtpObject.DoMakeDir(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      FFtp.MakeDir(AsString(@v));
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.makeDir error');
  end;
end;

function TJFtpObject.DoNList(Param: TJValueList): TJValue;
//NLST
var
  i: Integer;
  ao: TJArrayObject;
  fp: TJFtpFilePropertyObject;
begin
  try
    FFtp.Nlist;
    ao := TJArrayObject.Create(FEngine);
    Result := BuildObject(ao);
    
    for i := 0 to FFtp.FTPDirectoryList.Count - 1 do
    begin
      //ArrayにFTPFilePropertyを入れる
      fp := TJFtpFilePropertyObject.Create(FEngine);
      fp.FileProperty := FFtp.FtpDirectoryList[i]^;
      ao.Add(BuildObject(fp));
    end;
  except
    raise EJThrow.Create(E_SOCKET,'FTP.nlist error');
  end;
end;

function TJFtpObject.DoPrintWorkDir(Param: TJValueList): TJValue;
begin
  try
    FFtp.PrintWorkDir;
    Result := BuildString(FFtp.CurrentDir);
  except
    raise EJThrow.Create(E_SOCKET,'FTP.printWorkDir error');
  end;
end;

function TJFtpObject.DoQuit(Param: TJValueList): TJValue;
begin
  Result := DoDisconnect(Param);
end;

function TJFtpObject.DoReinitialize(Param: TJValueList): TJValue;
begin
  Result := BuildObject(Self);
  try
    FFtp.Reinitialize;
  except
    raise EJThrow.Create(E_SOCKET,'FTP.reinitialize error');
  end;
end;

function TJFtpObject.DoRemoveDir(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  Result := BuildObject(Self);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      FFtp.RemoveDir(AsString(@v));
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.removeDir error');
  end;
end;

function TJFtpObject.DoRename(Param: TJValueList): TJValue;
var
  v: TJValue;
  old,new: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam2(Param) then
    begin
      v := Param[0];
      old := AsString(@v);

      v := Param[1];
      new := AsString(@v);
      FFtp.Rename(old,new);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.rename error');
  end;
end;

function TJFtpObject.DoType(Param: TJValueList): TJValue;
//モード変更
var
  v: TJValue;
  s: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      s := AsString(@v);
      if AnsiSameText(s,'A') then
        FFtp.Mode(MODE_ASCII)
      else if AnsiSameText(s,'B') then
        FFtp.Mode(MODE_BYTE)
      else
        FFtp.Mode(MODE_IMAGE);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.type error');
  end;
end;

function TJFtpObject.DoUpload(Param: TJValueList): TJValue;
var
  v: TJValue;
  r,l: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam2(Param) then
    begin
      v := Param[0];
      l := AsString(@v);

      v := Param[1];
      r := AsString(@v);
      FFtp.Upload(l,r);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.upload error');
  end;
end;

function TJFtpObject.DoUploadAppend(Param: TJValueList): TJValue;
var
  v: TJValue;
  r,l: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam2(Param) then
    begin
      v := Param[0];
      l := AsString(@v);

      v := Param[1];
      r := AsString(@v);
      FFtp.UploadAppend(l,r);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.uploadAppend error');
  end;
end;

function TJFtpObject.DoUploadRestore(Param: TJValueList): TJValue;
var
  v: TJValue;
  r,l: String;
  pos: Integer;
begin
  Result := BuildObject(Self);
  try
    if IsParam2(Param) then
    begin
      v := Param[0];
      l := AsString(@v);

      v := Param[1];
      r := AsString(@v);

      if IsParam3(Param) then
      begin
        v := Param[2];
        pos := AsInteger(@v);
      end
      else
        pos := 0;

      FFtp.UploadRestore(l,r,pos);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.uploadRestore error');
  end;
end;

function TJFtpObject.DoUploadUnique(Param: TJValueList): TJValue;
var
  v: TJValue;
  l: String;
begin
  Result := BuildObject(Self);
  try
    if IsParam1(Param) then
    begin
      v := Param[0];
      l := AsString(@v);

      FFtp.UploadUnique(l);
    end
    else
      raise EJException.Create('');
  except
    raise EJThrow.Create(E_SOCKET,'FTP.uploadUnique error');
  end;
end;

function TJFtpObject.GetBytesRead: Integer;
begin
  Result := FFtp.BytesRecvd;
end;

function TJFtpObject.GetBytesTotal: Integer;
begin
  Result := FFtp.BytesTotal;
end;

function TJFtpObject.GetBytesWrote: Integer;
begin
  Result := FFtp.BytesSent;
end;

function TJFtpObject.GetCurrentDir: String;
begin
  Result := FFtp.CurrentDir;
end;

function TJFtpObject.GetHost: String;
begin
  Result := FFtp.Host;
end;

function TJFtpObject.GetLength: Integer;
begin
{ TODO : 保留 }
  Result := 0;
end;

function TJFtpObject.GetPassiveMode: Boolean;
begin
  Result := FFtp.PassiveMode;
end;

function TJFtpObject.GetPassword: String;
begin
  Result := FFtp.Password;
end;

function TJFtpObject.GetPort: Integer;
begin
  Result := FFtp.Port;
end;

function TJFtpObject.GetProxy: String;
begin
  Result := FFtp.Proxy;
end;

function TJFtpObject.GetTimeout: Integer;
begin
  Result := FFtp.Timeout;
end;

function TJFtpObject.GetUserId: String;
begin
  Result := FFtp.UserId;
end;

procedure TJFtpObject.OnFailure(var Handled: Boolean;
  Trans_Type: TFTPCmdType);
//イベント
var
  tp: TJValue;
  param: TJValueList;
  ha: TJBooleanObject;
begin
  if IsCallEvent('onFailure') then
  begin
    ha := TJBooleanObject.Create(FEngine);
    ha.bool := Handled;
    
    EnumToValue(TypeInfo(TFTPCmdType),tp,Ord(Trans_Type));

    param := TJValueList.Create;
    try
      param.Add(Self);
      param.add(ha);
      param.Add(tp);
      CallEvent('','onFailure',param);
    finally
      Handled := ha.bool;
      param.Free;
    end;
  end
  else //例外を起こす
    Handled := False;
end;

procedure TJFtpObject.OnSuccess(Trans_Type: TFTPCmdType);
//イベント
var
  tp: TJValue;
  param: TJValueList;
begin
  if not IsCallEvent('onSuccess') then
    Exit;

  EnumToValue(TypeInfo(TFTPCmdType),tp,Ord(Trans_Type));

  param := TJValueList.Create;
  try
    param.Add(Self);
    param.Add(tp);
    CallEvent('','onSuccess',param);
  finally
    param.Free;
  end;
end;

procedure TJFtpObject.SetHost(const Value: String);
begin
  FFtp.Host := Value;
end;

procedure TJFtpObject.SetPassiveMode(const Value: Boolean);
begin
  FFtp.PassiveMode := Value;
end;

procedure TJFtpObject.SetPassword(const Value: String);
begin
  FFtp.Password := Value;
end;

procedure TJFtpObject.SetPort(const Value: Integer);
begin
  FFtp.Port := Value;
end;

procedure TJFtpObject.SetProxy(const Value: String);
begin
  FFtp.Proxy := Value;
end;

procedure TJFtpObject.SetTimeout(const Value: Integer);
begin
  FFtp.Timeout := Value;
end;

procedure TJFtpObject.SetUserId(const Value: String);
begin
  FFtp.UserId := Value;
end;

{ TJRequestObject }

constructor TJRequestObject.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('Request');
end;

procedure TJRequestObject.GetPropertyList(List: TStringList);
begin
  inherited GetKeyList(List,[vtString],[]);
end;

function TJRequestObject.ToString(Value: PJValue): String;
var
  sl: TStringList;
  i: Integer;
  v: TJValue;
begin
  sl := TStringList.Create;
  try
    GetPropertyList(sl);
    for i := 0 to sl.Count - 1 do
    begin
      v := GetValue(sl[i],True);
      Result := Result + sl[i] + ': ' +  AsString(@v) + CRLF;
    end;

    Result := TrimRight(Result);
  finally
    sl.Free;
  end;
end;

end.
