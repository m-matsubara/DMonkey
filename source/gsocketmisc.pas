unit gSocketMisc;

{$IFDEF VER140}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}


//socket関係いろいろ
//by Wolfy


interface

uses
  Windows,SysUtils,Classes,messages,
  hashtable,regexpr
{$IFDEF WS2}
  ,winsock2;
{$ELSE}
  ,Winsock;
{$ENDIF}

const
  INVALID_IP_ADDRESS = u_long(not(0));//$FFFFFFFF;//INADDR_NONE
  MonthString1: array[1..12] of String =
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  MonthString2: array[1..12] of String =
    ('January','February','March','April','May','June','July','August',
     'September','October','November','December');
     
  DayOfWeekString: array[1..7] of String =
    ('Sun', 'Mon', 'Tue', 'Wed','Thu', 'Fri', 'Sat');

  CRLF = #13#10;
  CR = #13;
  LF = #10;
  TAB = #9;
  SINGLE_QUOTE = #39;
  SPACE = #32;
  gSOCKET_MESSAGE = WM_USER + $205;

  SD_RECEIVE = $00;
  SD_SEND = $01;
  SD_BOTH = $02;


  PATH_DOS = 1;
  PATH_UNIX = 2;

  ALPHABET = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

  REGEXP_URL = '[a-zA-Z]+://[^\n\t]+';
  REGEXP_URL_REMOVE_QUOTE = '[a-zA-Z]+://[^\n\t\"]+';
  REGEXP_PROXY = '[a-zA-Z0-9_\-]+[a-zA-Z0-9_\-\.]+:[0-9]{1,5}';

type
  //time_t = Longint;
  //socketの状態
  TSocketState = (ssInvalid,ssValid,ssConnected,ssStateUnknown);
  //Urlレコード
  TUrlInfo = record
    Url,
    Protocol,
    UserId,
    Password,
    Host,
    Port,
    Path,
    Dir,
    FileName,
    Query: String;
  end;
  //link type
  TLinkType =
    (ltA,ltImg,ltFrame,
     ltOther,ltLink,ltBase,
     ltBgsound,ltEmbed,ltObject,
     ltBodyStyle,ltForm,ltIframe,
     ltMetaRefresh,ltBodyBackground,
     ltArea,{ltWindowOpen,}ltImgDynsrc,ltImgLowsrc,
     ltScript,ltParam);

  TLinkSet = set of TLinkType;
  TLinkLayer = (llUpperLayer,llLowerLayer,llOthers);
  TLayerSet = set of TLinkLayer;  


const
  EmptyLinkSet: TLinkSet = [];
  LinkLinkSet: TLinkSet = [ltA,ltArea,ltFrame,ltLink,ltBase,ltIframe,ltMetaRefresh];
  ImageLinkSet: TLinkSet = [ltImg,ltBodyStyle,ltBodyBackground,ltImgDynsrc,ltImgLowsrc];
  MediaLinkSet: TLinkSet = [ltBgsound,ltEmbed,ltObject,ltParam];
  OtherLinkSet: TLinkSet = [ltOther,ltForm,ltScript{,ltWindowOpen}];
  FullLinkSet: TLinkSet =
    [ltA,ltImg,ltFrame,
     ltOther,ltLink,ltBase,
     ltBgsound,ltEmbed,ltObject,
     ltBodyStyle,ltForm,ltIframe,
     ltMetaRefresh,ltBodyBackground,
     ltArea,{ltWindowOpen,}ltImgDynsrc,ltImgLowsrc,ltScript,ltParam];


function LookupHostName(const HostName: String): u_long;
function IPToStr(IP: LongInt): String;
function ErrorToStr(Value: Integer): String;
function GetSocketPort(var Socket: TSocket): string;
{$IFDEF WS2}
function GetSocketIPAddr(Socket: TSocket): string;
{$ELSE}
function GetSocketIPAddr(var Socket: TSocket): string;
{$ENDIF}

function GetLocalIPAddr: u_long;
function SocketState(var Socket: TSocket): TSocketState;

function NormalUrl(Url: String; RemoveQuote: Boolean = False): String;
procedure NormalUrls(Urls: TStrings; RemoveQuote: Boolean = False);

function NormalUrlAddHttp(Url: String): String;
procedure NormalUrlsAddHttp(Urls: TStrings);
//proxy
function NormalProxy(Proxy: String): String;
procedure NormalProxies(Proxies: TStrings);

function ParseUrl(Url: String): TUrlInfo;
procedure ParseProxy(AProxy: String; var Url,Host,Port: String);
function ExtractUrlHostAndPort(const Url: String): String;
function ExpandUrl(Base,Dest: String): String;
function ExtractUrlProtocol(Url: String): String;
function ExtractUrlProtocolHost(const Url: String): String;
procedure ExtractLinks(BaseUrl,Html: String; Urls: TStringList; LinkSet: TLinkSet);

function ExtractUrlHost(const Url: String): String;
function ExtractUrlFilename(const Url: String): String;

function UrlFilenameToDosFilename(const Filename: String): String;
function NormalFilename(Path: String; AddChars: String = ''): String;
function ExchangeUrlToPath(const Url,LocalPath: String): String;

function ExtractUrlFilename2(const Url: String): String;
function ExtractUrlDir(const Url: String): String;
function ExtractUrlReferer(const Url: String): String;
function ExtractUrlAccount(const Url: String): String;
function ExtractUrlAccountEx(Url: String): String ;
function ExtractUrlPath(const Url: String): String;

function ExtractUrlFilename3(const Url: String): String;

function RemoveUrlAnchor(Url: String): String;
function RemoveFileAnchor(Url,Filename: String): String;


procedure HTTPGetURL(URLs: TStringList);
function IsLinkLayer(const Url1,Url2: String): TLinkLayer;
function BuildUrl(UrlInfo: TUrlInfo): String;
function AdjustHttpHeaders(const Headers: String): String;


function HTTPModifiedToDosTime(Date: String): Integer;
function HttpModifiedToDateTime(DateText: String): TDateTime;
function GetMonth(S: String): Integer;
function GMTtoLocaltime(G: TDateTime): TDateTime;
function LocaltimeToGMT(Time: TSystemTime): TDateTime;
//function time_tToDateTime( t:time_t ): TDateTime;
//function DateTimeTotime_t( d:TDateTime ): time_t;
function DateTimeToUnix(const AValue: TDateTime): Int64;
function UnixToDateTime(const AValue: Int64): TDateTime;

function DateToHttpDate(Time: TSystemTime): String;
function DateTimeToHttpModified(Date: TDateTime): String;

function RemoveComma(const S: String): String;
function UniqueFileName(const FileName: String): String;
function RemoveCRLF(const S: String): String;
function IsValidChar(C: Char): Boolean;
procedure AdjustStrings(S: TStrings; AcceptDuplicate: Boolean);
function CheckPath(const Path: String; DirType: Integer): String;
function GetTempName(const Prefix: String): String;
function GetDateTimeSimpleStr(DateTime: TDateTime): String;
procedure HTTPHeaderToHash(const Header: String; HashTable: THashTable);
function HashToHTTPHeader(Headers: THashTable): String;

function _UrlEncode(const AStr,AppendNoConvChars: String; Space2Plus: Boolean): String;
function _UrlDecode(const AStr: String; Plus2Space: Boolean): String;
function IsUrlEncoded(const AStr: String): Boolean;

function Escape(const S: String): String;
function Unescape(const S: String): String;
function EncodeURI(const S: String): String;
function EncodeURIComponent(const S: String): String;
function DecodeURI(const S: String): String;
function DecodeURIComponent(const S: String): String;

function WildcardMatching(const SearchString,Mask: String; IgnoreCase: Boolean): Boolean;
function FormatKillo(Value : Double): String;

function GenerateRandomString(Len: Integer): String;

function ExtractQuotedString(const S: string; Quote: Char): string;



implementation

uses
  misc,gsockethtml;

function LookupHostName(const HostName: String): u_long;
//host を ipに変換
var
  RemoteHost : PHostEnt;
  AnsiHostName: AnsiString;
begin
  //アドレス初期化
  Result := INVALID_IP_ADDRESS;
  //ホスト名がないならば終わり
  if HostName = '' then
    Exit;

  AnsiHostName := HostName;

  //まずIPアドレスかどうか試す
  Result := Inet_Addr(PAnsiChar(AnsiHostName));
  //IPアドレスでないならば
{$IFDEF WS2}
  if Result = INADDR_NONE then //-1 then
{$ELSE}
  if Result = -1 then //INVALID_IP_ADDRESS then
{$ENDIF}
  begin
    //Host名のIPアドレスを調べる
    RemoteHost := GetHostByName(PAnsiChar(HostName));
    //失敗ならば
    if not Assigned(RemoteHost) then
    begin
      Result := INVALID_IP_ADDRESS;
      Exit;
    end;

    //成功ならばアドレスに変換
    Result := u_long(Pointer(RemoteHost^.h_addr_list^)^);
  end;

end;

function IPToStr(IP: LongInt): String;
//ip addressを 文字に
begin
  IP := ntohl(IP);
  Result := IntToStr( IP shr 24) + '.' +
            IntToStr((IP shr 16) and $FF) + '.' +
            IntToStr((IP shr 8 ) and $FF) + '.' +
            IntToStr( IP and $FF);
end;

function ErrorToStr(Value: Integer): String;
//WSAErrorを変換
begin
   Result := 'UNKNOWN ERROR';
   case Value of
      WSABASEERR+4 : Result := 'WSAEINTR';
      WSABASEERR+9 : Result := 'WSAEBADF';
      WSABASEERR+13 : Result := 'WSAEACCES';
      WSABASEERR+14 : Result := 'WSAEFAULT';
      WSABASEERR+22 : Result := 'WSAEINVAL';
      WSABASEERR+24 : Result := 'WSAEMFILE';
      WSABASEERR+35 : Result := 'WSAEWOULDBLOCK';
      WSABASEERR+36 : Result := 'WSAEINPROGRESS';
      WSABASEERR+37 : Result := 'WSAEALREADY';
      WSABASEERR+38 : Result := 'WSAENOTSOCK';
      WSABASEERR+39 : Result := 'WSAEDESTADDRREQ';
      WSABASEERR+40 : Result := 'WSAEMSGSIZE';
      WSABASEERR+41 : Result := 'WSAEPROTOTYPE';
      WSABASEERR+42 : Result := 'WSAENOPROTOOPT';
      WSABASEERR+43 : Result := 'WSAEPROTONOSUPPORT';
      WSABASEERR+44 : Result := 'WSAESOCKTNOSUPPORT';
      WSABASEERR+45 : Result := 'WSAEOPNOTSUPP';
      WSABASEERR+46 : Result := 'WSAEPFNOSUPPORT';
      WSABASEERR+47 : Result := 'WSAEAFNOSUPPORT';
      WSABASEERR+48 : Result := 'WSAEADDRINUSE';
      WSABASEERR+49 : Result := 'WSAEADDRNOTAVAIL';
      WSABASEERR+50 : Result := 'WSAENETDOWN';
      WSABASEERR+51 : Result := 'WSAENETUNREACH';
      WSABASEERR+52 : Result := 'WSAENETRESET';
      WSABASEERR+53 : Result := 'WSAECONNABORTED';
      WSABASEERR+54 : Result := 'WSAECONNRESET';
      WSABASEERR+55 : Result := 'WSAENOBUFS';
      WSABASEERR+56 : Result := 'WSAEISCONN';
      WSABASEERR+57 : Result := 'WSAENOTCONN';
      WSABASEERR+58 : Result := 'WSAESHUTDOWN';
      WSABASEERR+59 : Result := 'WSAETOOMANYREFS';
      WSABASEERR+60 : Result := 'WSAETIMEDOUT';
      WSABASEERR+61 : Result := 'WSAECONNREFUSED';
      WSABASEERR+62 : Result := 'WSAELOOP';
      WSABASEERR+63 : Result := 'WSAENAMETOOLONG';
      WSABASEERR+64 : Result := 'WSAEHOSTDOWN';
      WSABASEERR+65 : Result := 'WSAEHOSTUNREACH';
      WSABASEERR+66 : Result := 'WSAENOTEMPTY';
      WSABASEERR+67 : Result := 'WSAEPROCLIM';
      WSABASEERR+68 : Result := 'WSAEUSERS';
      WSABASEERR+69 : Result := 'WSAEDQUOT';
      WSABASEERR+70 : Result := 'WSAESTALE';
      WSABASEERR+71 : Result := 'WSAEREMOTE';
      WSABASEERR+91 : Result := 'WSASYSNOTREADY';
      WSABASEERR+92 : Result := 'WSAVERNOTSUPPORTED';
      WSABASEERR+93 : Result := 'WSANOTINITIALISED';
      WSABASEERR+101 : Result := 'WSAEDISCON';
      WSABASEERR+1001 : Result := 'WSAHOST_NOT_FOUND';
      WSABASEERR+1002 : Result := 'WSATRY_AGAIN';
      WSABASEERR+1003 : Result := 'WSANO_RECOVERY';
      WSABASEERR+1004 : Result := 'WSANO_DATA';
   end;
end;

function GetSocketPort(var Socket: TSocket): string;
//ソケットのポートを返す
var
  addr: TSockAddrIn;
  addrlen: integer;
begin
  addrlen := sizeof(addr);
  getsockname(Socket,addr,addrlen);
  Result := IntToStr(ntohs(addr.sin_port));
end;

{$IFDEF WS2}
function GetSocketIPAddr(Socket: TSocket): string;
//ソケットのアドレスを返す
var
  addr: TSockAddrIn;
  addrlen: integer;
  szIPAddr: PChar;
begin
  addrlen := sizeof(addr);
  getsockname(Socket,addr,addrlen);
  szIPAddr := inet_ntoa(addr.sin_addr);
  Result := StrPas(szIPAddr);
end;
{$ELSE}
function GetSocketIPAddr(var Socket: TSocket): string;
//ソケットのアドレスを返す
var
  addr: TSockAddrIn;
  addrlen: integer;
  szIPAddr: PAnsiChar;
begin
  addrlen := sizeof(addr);
  getsockname(Socket,addr,addrlen);
  szIPAddr := inet_ntoa(addr.sin_addr);
  Result := StrPas(szIPAddr);
end;
{$ENDIF}


function GetLocalIPAddr: u_long;
//local IP Address
const
  BufSize = 255;
var
  buf: Pointer;
  RemoteHost : PHostEnt;
begin
  buf := nil;
  try
    GetMem(buf,BufSize);
    gethostname(buf,BufSize);
    RemoteHost := GetHostByName(Buf);
    if RemoteHost = nil then
      Result := htonl($07000001)
    else
      Result := u_long(Pointer(RemoteHost^.h_addr_list^)^);
  finally
    if Assigned(buf) then FreeMem(buf,BufSize);
  end;
end;


function ParseUrl(Url: String): TUrlInfo;
//URL解析
var
  U: TUrlInfo;
  Index: Integer;
  tmp: String;
begin
  U.Url := Url;
  U.Protocol := '';
  U.UserId := '';
  U.Password := '';
  U.Host := '';
  U.Port := '';
  U.Path := '';
  U.Dir := '';
  U.FileName := '';
  U.Query := '';
  
  Trim(Url);
  //protocol :// は含まない
  if Pos('://',Url) > 0 then
  begin
    U.Protocol := AnsiLowerCase(Copy(Url,1,Pos('://',Url) - 1));
    Delete(Url,1,Pos('://',Url) + 2);
  end;
  //else begin
    // ://がないときはhttpにしとく(保留）
  //  Proto := 'http';
  //end;
  //host
  if Pos('/',Url) > 0 then
  begin
    U.Host := Copy(Url,1,Pos('/',URL) - 1);
    Delete(Url,1,Pos('/',URL) - 1);
  end
  else begin
    // hostだけならば
    U.Host := Url;
    Url := '/';
  end;
  //user&pass
  if Pos('@',U.Host) > 0 then
  begin
    U.UserId := Copy(U.Host,1,Pos('@',U.Host) - 1);
    U.Password := Copy(U.UserId,Pos(':',U.UserId) + 1,MaxInt);
    Delete(U.UserId,Pos(':',U.UserId),MaxInt);
    Delete(U.Host,1,Pos('@',U.Host));
  end;
  //port
  if Pos(':',U.Host) > 0 then
  begin
    U.Port := Trim(Copy(U.Host,Pos(':',U.Host) + 1,MaxInt));
    Delete(U.Host,Pos(':',U.Host),MaxInt);
    try
      StrToInt(U.Port);
    except
      U.Port := '';
    end;
  end;

  //hostを小文字にする
  U.Host := AnsiLowerCase(U.Host);


  U.Path := Url;
  //ここからhttpとftpで分岐する
  if U.Protocol = 'ftp' then
  begin
    Index := LastDelimiter('/',U.Path);
    U.Dir := Copy(U.Path,1,Index);
    U.FileName := Copy(U.Path,Index + 1,MaxInt);
    U.Query := '';
  end
  else begin
    //まず?を調べる
    Index := LastDelimiter('?',U.Path);
    if Index > 0 then
    begin
      tmp := Copy(U.Path,1,Index - 1);
      U.Query := Copy(U.Path,Index,MaxInt);
      //path
      Index := LastDelimiter('/',tmp);
      U.Dir := Copy(tmp,1,Index);
      U.FileName := Copy(tmp,Index + 1,MaxInt);
    end
    else begin
      Index := LastDelimiter('/',U.Path);
      U.Dir := Copy(U.Path,1,Index);
      U.FileName := Copy(U.Path,Index + 1,MaxInt);
      U.Query := '';
    end;
  end;

  //URL Infoを返す
  Result := U;
end;

procedure ParseProxy(AProxy: String; var Url,Host,Port: String);
//Delegate Proxyを解析して URLとHostとPort を返す
//  /-_-//
var
  Delegate,Proxy: String;
begin
  AProxy := Trim(AProxy);
  Delegate := '';

  if Pos('+',AProxy) > 0 then
  begin
    Proxy := Copy(AProxy,1,Pos('+',AProxy) - 1);
    Delegate := Copy(AProxy,Pos('+',AProxy) + 1,Length(AProxy));
    Delegate := StringReplace(Delegate, '+','/-_-http://',[rfReplaceAll, rfIgnoreCase]);
    Delegate := 'http://' + Delegate + '/-_-';
  end
  else
    Proxy := AProxy;

  if Pos(':',Proxy) > 0 then
  begin
    Host := Copy(Proxy,1,Pos(':',Proxy) - 1);
    Port := Copy(Proxy,Pos(':',Proxy) + 1,Length(Proxy));
  end
  else begin
    Host := Proxy;
    Port := '';
  end;

  //Delegate解析
  if Delegate <> '' then
    Url := Delegate + Url;
end;

function ExtractUrlHostAndPort(const Url: String): String;
//URLからHostとportを取り出す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  if U.Port = '' then
    Result := U.Host
  else
    Result := U.Host + ':' + U.Port;
end;

function ExpandUrl(Base,Dest: String): String;
//URLを絶対パスに変換
var
  S: String;
  index: Integer;
  info: TUrlInfo;
begin
  //ゴミ削除
  index := Pos(';',Dest);
  if index > 0 then
    Delete(Dest,index,MaxInt);

  info := ParseUrl(Base);
  info.Path := Info.Dir;

  //プロトコルが含まれているならば終わり
  if Pos('://',Dest) > 0 then
    Result := Dest
  //最初ならば
  else if Pos('/',Dest) = 1 then
  begin
    info.Path := Dest;
    Result :=  BuildUrl(info);
  end
  //同じ
  else if Pos('./',Dest) = 1 then
  begin
    info.Path := Info.Dir + Copy(Dest,3,MaxInt);
    Result := BuildUrl(info);
  end
  //一つ上
  else if Pos('../',Dest) = 1 then
  begin
    //最後の/を消す
    Base := BuildUrl(info);
    Delete(Base,Length(Base),1);

    while Pos('../',Dest) = 1 do
    begin
      Dest := Copy(Dest,4,MaxInt);
      // host以下に / が含まれていれば Baseを削除
      S := Copy(Base,Pos('://',Base) + 3,MaxInt);
      if LastDelimiter('/',S) > 0 then
        Base := Copy(Base,1,LastDelimiter('/',Base) - 1);
    end;
    Result := Base + '/' + Dest;
  end
  //何もなし
  else begin
    info.Path := info.Dir + Dest;
    Result := BuildUrl(info);
  end;

end;

function ExtractUrlProtocol(Url: String): String;
//URLから プロトコルを返す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := LowerCase(U.Protocol);
end;

function ExtractURLProtocolHost(const Url: String): String;
//URLから プロトコルとホスト名を返す  最後に / は付かない
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := U.Protocol + '://' + U.Host;
end;

function ExtractUrlHost(const Url: String): String;
//urlからhostを返す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := U.Host;
end;

function ExtractUrlDir(const Url: String): String;
//urlからdirを返す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := U.Dir;
end;

function ExtractUrlPath(const Url: String): String;
//urlからpathを返す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := U.Path;
end;

function ExtractUrlReferer(const Url: String): String;
//URLから proto://host:port/dir/ を返す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  if U.Port <> '' then
    Result := U.Protocol + '://' + U.Host + ':' + U.Port + U.Dir
  else
    Result := U.Protocol + '://' + U.Host + U.Dir;
end;

function ExtractUrlAccount(const Url: String): String;
//URLからアカウントを返す
var
  Account: String;
begin
  Account := ExtractUrlDir(Url);
  Delete(Account,Pos('/',Account),1);
  if Length(Account) > 0 then
    Delete(Account,Pos('/',Account),Length(Account));

  Result := Account;
end;

function ExtractUrlAccountEx(Url: String): String ;
//URLからアカウント名を返す
var
  info: TUrlInfo;
  dir: String;
begin
  info := ParseUrl(Url);
  dir := info.Dir;
  Delete(dir,Pos('/',dir),1);
  if Length(dir) > 0 then
    Delete(dir,Pos('/',dir),MaxInt);
  //全てを返す
  Result :=  info.Protocol + '://' + info.Host + '/' + dir;
end;

function ExchangeUrlToPath(const Url,LocalPath: String): String;
//URLをLocalパスに変換
var
  ui: TUrlInfo;
  index: Integer;
begin
  ui := ParseUrl(Url);
  index := AnsiPos('/',ui.Dir);
  while index > 0 do
  begin
    ui.Dir[index] := '\';
    index := AnsiPos('/',ui.Dir);
  end;

  Result := IncludeTrailingBackSlash(LocalPath) +
    NormalFilename(ui.Host + ui.Dir);

  Result := ReplaceRegExpr('\\\\+',Result,'\', True);   // TODO 2007/7/27 m.matsubara (Trueでよい？)
end;


function UrlFilenameToDosFilename(const Filename: String): String;
//URLからファイル名を返す DOSで使えない文字は変換
const
  //NotUsesChar: set of Char = ['\','/',':','*','?','"','<','>','|'];
  NotUsesChar = '\/:*?"<>|';
var
  i: Integer;
  ws: WideString;
begin
  //Result := Filename;
  ws := Filename;

  for i := 1 to Length(ws) do
    if Pos(ws[i],NotUsesChar) > 0 then
      ws[i] := '_';

  Result := ws;
end;

function NormalFilename(Path: String; AddChars: String): String;
//DOSで使えない文字は変換
const
  NotUsesChars = ':/*?"<>|';
var
  i: Integer;
  drive: String;
  ws,notchars: WideString;
begin
  if AnsiPos(':',Path) = 2 then
  begin
    drive := Copy(Path,1,2);
    Delete(Path,1,2);
  end
  else
    drive := '';

  ws := Path;
  notchars := NotUsesChars + AddChars;

  for i := 1 to Length(ws) do
    if Pos(ws[i],notchars) > 0 then
      ws[i] := '_';

  Result := drive + ws;
end;


function ExtractUrlFilename(const Url: String): String;
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := U.Filename + U.Query;

  //httpのみ
  if (Pos('#',Result) > 0) and
    ((U.Protocol = 'http') or (U.Protocol = 'https')) then
    Result := Copy(Result,1,Pos('#',Result) - 1);

  if Result = '' then
    Result := 'index.html';  
end;

function ExtractUrlFilename2(const Url: String): String;
//ファイル名だ蹴返す
var
  U: TUrlInfo;
begin
  U := ParseUrl(Url);
  Result := U.Filename;
end;

function ExtractUrlFilename3(const Url: String): String;
var
  i: Integer;
begin
  Result := Url;
  i := LastDelimiter('/',Url);
  if i > 0 then
    Result := Copy(Result,i + 1,MaxInt);
end;

function RemoveUrlAnchor(Url: String): String;
var
  s,filename: String;
  index: Integer;
begin
  Result := Url;
  s := LowerCase(Url);
  if (Pos('ttp://',s) > 0) or (Pos('ttps://',s) > 0) then
  begin
    index := LastDelimiter('/',Url);
    if index > 0 then
    begin
      filename := Copy(Url,index + 1,MaxInt);
      index := AnsiPos('#',filename);
      if index > 0 then
        Result := Copy(filename,1,index - 1);
    end;
  end;
end;

function RemoveFileAnchor(Url,Filename: String): String;
var
  s: String;
  index: Integer;
begin
  if Filename <> '' then
    Result := Filename
  else
    Result := ExtractUrlFilename2(Url);

  s := LowerCase(Url);
  if (Pos('ttp://',s) > 0) or (Pos('ttps://',s) > 0) then
  begin
    index := LastDelimiter('#',Result);
    if index > 0 then
      Result := Copy(Result,1,index - 1);
  end;
end;

function HttpModifiedToDateTime(DateText: String): TDateTime;
//HTTPサーバから日付を取得
//Last-Modified: Sat, 01 May 1999 12:06:24 GMT
//Last-Modified: Sat, 04 Sep 1999 07:37:06 GMT
var
  temp,time: TDateTime;
  sl: TStringList;
  y,m,d: Word;
begin
  Result := Now;
  DecodeDate(Result,y,m,d);

  sl := TStringList.Create;
  try try
    DateText := Trim(DateText);
    //空白 or ハイフンで分割
    SplitRegExpr('[\s\-]*',DateText,sl);
    //日
    d := StrToIntDef(sl[1],d);
    //月
    m := GetMonth(sl[2]);
    //年
    y := StrToIntDef(sl[3],y);
    //時間
    time := StrToTimeDef(sl[4],0);

    temp := EncodeDate(y,m,d) + time;
    Result := GMTtoLocalTime(temp);
  finally
    sl.Free;
  end;

  except
  end;
end;

function HTTPModifiedToDosTime(Date: String): Integer;
//HTTPサーバから日付を取得 して dos timeに変更
begin
  Result := DateTimeToFileDate(HttpModifiedToDateTime(Date));
end;

function DateToHttpDate(Time: TSystemTime): String;
//現在の日付を GMTのHttp形式に変換
//Date: Tue, 18 Jan 2000 15:49:13 GMT
var
  GMT: TDateTime;
  Week,MonthStr: String;
  Year,Month,Day,Hour,Min,Sec,MSec: Word;
begin
  GMT := LocalTimeToGMT(Time);
  DecodeDate(GMT,Year,Month,Day);
  DecodeTime(GMT,Hour,min,Sec,MSec);
  Week := DayOfWeekString[DayOfWeek(GMT)];
  MonthStr := MonthString1[Month];

  Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
    [Week,Day,MOnthStr,Year,Hour,Min,Sec]);

  //Result := Week + ', ' + IntToStr(Day) + ' ' + MonthStr + ' ' +
  //  IntToStr(Year) + ' ' + IntToStr(Hour) + ':' + IntToStr(Min) + ':' +
  //  IntToStr(Sec) + ' GMT';
end;

function DateTimeToHttpModified(Date: TDateTime): String;
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(Date,st);
  Result := DateToHttpDate(st);
end;

function GetMonth(S: String): Integer;
//月の数字を返す
var
  i: Integer;
begin
  Result := 1;
  S := LowerCase(S);
  //省略
  for i := 1 to 12 do
  begin
    if S = LowerCase(MonthString1[i]) then
    begin
      Result := i;
      Exit;
    end;
  end;
  //フル
  for i := 1 to 12 do
  begin
    if S = LowerCase(MonthString2[i]) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function GMTtoLocaltime(G: TDateTime): TDateTime;
//GMT to Localtime
var
  LocalFileTime,FileTime: TFileTime;
  LocalTime,SysTime: TSystemTime;
  //buf: String;
begin
  //windows systemtimeに変換
  DateTimeToSystemTime(G,SysTime);
  //file timeに変換
  SystemTimeToFileTime(SysTime,FileTime);

  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, LocalTime);
  {with LocalTime do
    buf := Format('%d/%.2d/%.2d %.2d:%.2d:%.2d', [wYear, wMonth, wDay,
        wHour, wMinute, wSecond]);
  Result := StrToDateTime(buf);}

  try
    with LocalTime do
      Result := EncodeDate(wYear,wMonth,wDay) + EncodeTime(wHour,wMinute,wSecond,0)
  except
    on EConvertError do
      Result := Now;
  end;
end;

function LocaltimeToGMT(Time: TSystemTime): TDateTime;
//Localtime to GMT
//var
  //SysTime: TSystemTime;
  //buf: String;
begin
  //windows systemtimeに変換
  //DateTimeToSystemTime(L,SysTime);

  {with Time do
    buf := Format('%d/%.2d/%.2d %.2d:%.2d:%.2d', [wYear, wMonth, wDay,
        wHour, wMinute, wSecond]);
  Result := StrToDateTime(buf);}

  try
    with Time do
      Result := EncodeDate(wYear,wMonth,wDay) + EncodeTime(wHour,wMinute,wSecond,0);
  except
    on EConvertError do
      Result := Now;
  end;

end;

function RemoveComma(const S: String): String;
// , を削除
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do
  begin
    if Result[i] = ',' then
      Delete(Result,i,1);
  end;

  //Result := StringReplace(S,',','',[rfReplaceAll]);
end;

function UniqueFileName(const FileName: String): String;
// uniqueなファイル名を作る
// file_00.xxx形式
var
  Ext,S: String;
  No,Index: Integer;
begin
  Ext := ExtractFileExt(FileName);
  S := ChangeFileExt(FileName,'');
  No := 0;
  Index := LastDelimiter('_',S);
  if Index > 0 then
  begin
    try
      //最後が数値化できれば
      No := StrToInt(Copy(S,Index + 1,MaxInt));
      Inc(No);
      // _以降を消す
      Delete(S,Index,MaxInt);
    except
      ;
    end;
  end;
  //数字を付ける
  Result := S + '_' + Format('%.3u',[No]) + Ext;
end;

function _UrlEncode(const AStr,AppendNoConvChars: String; Space2Plus: Boolean): String;
const
  NoConversionChars = [
    'A'..'Z','a'..'z',
    '0'..'9',
    '-','_','.','!','*','''','(',')']; // '@', '$',];                   
var
  Sp, Rp: PChar;
  NoConversion: set of Char;
  i: Integer;
begin
  NoConversion := NoConversionChars;
  for i := 1 to Length(AppendNoConvChars) do
    NoConversion := NoConversion + [AppendNoConvChars[i]];

  SetLength(Result, Length(AStr) * 3);
  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    if Sp^ in NoConversion then
      Rp^ := Sp^
    else
      if (Sp^ = ' ') and (Space2Plus) then
        Rp^ := '+'
      else
      begin
        FormatBuf(Rp^, 3, '%%%.2x', 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;


function _UrlDecode(const AStr: String; Plus2Space: Boolean): String;
var
  Sp, Rp, Cp: PChar;
begin
  SetLength(Result, Length(AStr));
  //チェック
  if not IsUrlEncoded(AStr) then
  begin
    Result := AStr;
    Exit;
  end;

  Sp := PChar(AStr);
  Rp := PChar(Result);
  while Sp^ <> #0 do
  begin
    if not (Sp^ in ['+','%']) then
      Rp^ := Sp^
    else begin
      if (Sp^ = '+') then
      begin
        if Plus2Space then
          Rp^ := ' '
        else
          Rp^ := '+';
      end
      else begin
        inc(Sp);
        if Sp^ = '%' then
          Rp^ := '%'
        else
        begin
          Cp := Sp;
          Inc(Sp);
          Rp^ := Chr(StrToIntDef(Format('$%s%s',[Cp^, Sp^]),0));
        end;
      end;
    end;
    
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function IsUrlEncoded(const AStr: String): Boolean;
begin
  Result := ExecRegExpr('\%[0-9a-fA-F][0-9a-fA-F]',AStr);
end;

function Escape(const S: String): String;
begin
  Result := _UrlEncode(S,'',True);
end;

function Unescape(const S: String): String;
begin
  Result := _UrlDecode(S,True);
end;

function EncodeURI(const S: String): String;
begin
  Result := _UrlEncode(S,';/?:@&=+,#',False);
end;

function EncodeURIComponent(const S: String): String;
begin
  Result := _UrlEncode(S,'',False);
end;

function DecodeURI(const S: String): String;
begin
  Result := _UrlDecode(S,False);
end;

function DecodeURIComponent(const S: String): String;
begin
  Result := _UrlDecode(S,False);
end;

function RemoveCRLF(const S: String): String;
//crlf & tabを削除
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    if (Result[i] = CR) or (Result[i] = LF) or (Result[i] = TAB) then
      Result[i] := ' ';
  //Result := StringReplace(S,CR,' ',[rfReplaceAll]);
  //Result := StringReplace(Result,LF,' ',[rfReplaceAll]);
  //Result := StringReplace(Result,TAB,' ',[rfReplaceAll]);
end;

function IsValidChar(C: Char): Boolean;
//文字が良いか
const
  InValidChar: set of Char = ['"',SINGLE_QUOTE,' ','<','>'];
begin
  if (Byte(C) < $20) or (Byte(C) > $7f) or (C in InValidChar) then
    Result := False
  else
    Result := True;
end;


procedure ExtractLinks(BaseUrl,Html: String; Urls: TStringList;
  LinkSet: TLinkSet);
//link抽出
//a href      ltA
//link href   ltLink
//base href   ltBase
//frame src,  ltFrame
//img src,  ltImg
//img lowsrc,ltImgLowsrc
//bgsound src ltBgsound
//embed src   ltEmbed
//object codebase  ltObject
//img dynsrc       ltImgDynsrc
//body style="background:url( )"  ltBodyStyle
//form action=                    ltForm
//iframe src                      ltIframe
//meta http-equiv="refresh" content"1;url=U_R_L" ltMetaRefresh
//body background=                               ltBodyBackground
//area href                                      ltArea
//その他                                         ltOther
//script src  ltScript
//param name="src" value=" "    ltParam

  function PreExpandUrl(const Base,Value: String): String;
  begin
    //valueの値をチェックする
    if (Value = '') or (Pos('mailto:',LowerCase(Value)) > 0) then
      Result := ''
    else
      Result := NormalUrl(ExpandUrl(Base,Value));
  end;
  {function PreExpandUrl2(const Base,Value: String): String;
  begin
    //valueの値をチェックする
    if (Value = '') or (Pos('mailto:',LowerCase(Value)) > 0) then
      Result := ''
    else
      Result := NormalUrl(ExpandUrl(Base,Value),True);
  end;
  }
var
  Parser: THtmlParser;
  Tag: THtmlTag;
  i,index: Integer;
  S,Line,tempbaseurl: String;
  SL: TStringList;
begin
//if Pos('mailto:',Tag.Value['href']) = 0 then
  Urls.Clear;
  //ソート済みにする
  Urls.Sorted := True;
  Urls.Duplicates := dupignore;

  Parser := THtmlParser.Create(Html);
  try
    for i := 0 to Parser.Count - 1 do
    begin
      Tag := Parser[i];
      if (Tag.Name = 'a') and (ltA in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['href']))
      else if (Tag.Name = 'link') and (ltLink in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['href']))
      else if (Tag.Name = 'base') and (ltBase in LinkSet) then
      begin
        //base hrefがあれば BaseUrlを入れ替える
        tempbaseurl := Tag['href'];
        if Pos('://',tempbaseurl) > 0 then
          BaseUrl := tempbaseurl
        else
          Urls.Add(PreExpandUrl(BaseUrl,tempbaseurl))
      end
      else if (Tag.Name = 'frame') and (ltFrame in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['src']))
      else if (Tag.Name = 'img') then
      begin
        if (ltImg in LinkSet) then
          Urls.Add(PreExpandUrl(BaseUrl,Tag['src']));

        if (ltImgDynsrc in LinkSet) then
          Urls.Add(PreExpandUrl(BaseUrl,Tag['dynsrc']));

        if (ltImgLowsrc in LinkSet) then
          Urls.Add(PreExpandUrl(BaseUrl,Tag['lowsrc']));
      end
      else if (Tag.Name = 'bgsound') and (ltBgsound in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['src']))
      else if (Tag.Name = 'embed') and (ltEmbed in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['src']))
      else if (Tag.Name = 'object') and (ltObject in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['codebase']))
      else if (Tag.Name = 'form') and (ltForm in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['action']))
      else if (Tag.Name = 'iframe') and (ltIframe in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['src']))
      else if (Tag.Name = 'area') and (ltArea in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['href']))
      else if (Tag.Name = 'body') then
      begin
        if (ltBodyStyle in LinkSet) then
        begin
          S := Tag['style'];
          if Pos('background:url(',S) > 0 then
          begin
            Delete(S,1,Pos('(',S));
            Urls.Add(PreExpandUrl(BaseUrl,Copy(S,1,Pos(')',S) - 1)));
          end
        end;

        if (ltBodyBackground in LinkSet) then
          Urls.Add(PreExpandUrl(BaseUrl,Tag['background']));
      end
      else if (Tag.Name = 'meta') and (ltMetaRefresh in LinkSet) then
      begin
        if Pos('refresh',LowerCase(Tag['http-equiv'])) > 0 then
        begin
          S := Tag['content'];
          if Pos('url=',LowerCase(S)) > 0 then
          begin
            Delete(S,1,Pos('url=',LowerCase(S)) + 3);
            Urls.Add(PreExpandUrl(BaseUrl,S));
          end;
        end;
      end
      else if (Tag.Name = 'script') and (ltScript in LinkSet) then
        Urls.Add(PreExpandUrl(BaseUrl,Tag['src']))
      else if (Tag.Name = 'param') and (ltParam in LinkSet) then
      begin
        if AnsiSameText(Tag['name'],'src') then
          Urls.Add(PreExpandUrl(BaseUrl,Tag['value']));
      end;

    end;
    //その他
    if ltOther in LinkSet then
    begin
      S := Parser.Text;
      Line := '';
      SL := TStringList.Create;
      try
        //区切る
        for i := 1 to Length(S) do
          if IsValidChar(S[i]) then
            Line := Line + S[i]
          else begin
            if Line <> '' then
              SL.Add(Line);

            Line := '';
          end;
        //linkを抽出
        for i := 0 to SL.Count - 1 do
          if Pos('://',SL[i]) > 0 then
            Urls.Add(NormalUrl(SL[i]));
      finally
        SL.Free;
      end;
    end;
  finally
    Parser.Free;
  end;
  //falseにする
  Urls.Sorted := False;
  // #をチェックする
  for i := Urls.Count - 1 downto 0 do
  begin
    s := AnsiLowerCase(Urls[i]);
    index := AnsiPos('#',s);
    if index > 0 then
    begin
      //http or https
      if (AnsiPos('http://',s) > 0) or (AnsiPos('https://',s) > 0) then
        Urls[i] := Copy(Urls[i],1,index - 1);
    end;

  end;

  //AdjustStrings(Urls,False);
end;

procedure AdjustStrings(S: TStrings; AcceptDuplicate: Boolean);
//stringsを整理
// AcceptDuplicate = True で重複を許可
var
  Temp: TStringList;
  i: Integer;
begin
  Temp := TStringList.Create;
  try
    for i := 0 to S.Count - 1 do
      //重複を認めない
      if not AcceptDuplicate then
      begin
        if (S[i] <> '') and (Temp.IndexOf(S[i]) = -1) then
          Temp.Add(Trim(S[i]));
      end
      else
        if S[i] <> '' then
          Temp.Add(Trim(S[i]));

    S.Clear;
    S.AddStrings(Temp);
  finally
    Temp.Free;
  end;
end;

function CheckPath(const Path: String; DirType: Integer): String;
var
  ws: WideString;
//pathの最後をcheck
begin
  Result := Path;
  if Path = '' then
  begin
    if DirType = PATH_DOS then
      Exit
    else begin
      Result := '/';
      Exit;
    end;
  end
  else begin
    ws := Path;
    if Pos('/',ws) > 0 then
    begin
      if ws[Length(ws)] <> '/' then
        Result := ws + '/';
    end
    else begin
      if ws[Length(ws)] <> '\' then
        Result := ws + '\';
    end;
  end;

end;


procedure HTTPGetURL(URLs: TStringList);
//URLを取り出す
var
  HTML,Link: String;
  Start,Last,Temp: Integer;
begin
  HTML := URLs.Text;
  URLs.Clear;
  while Pos('://',HTML) > 0 do
  begin
    Start := Pos('://',HTML);
    Delete(HTML,1,Start - 5);
    Last := 0;

    Temp := Pos(TAB,HTML);
    if Temp > 0 then
      Last := Temp;

    if Temp = 0 then
    begin
      Temp := Pos(CR,HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos(LF,HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos(';',HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos('"',HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos(#39,HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos('>',HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos('<',HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
    begin
      Temp := Pos(#0,HTML);
      if Temp > 0 then
        Last := Temp;
    end;

    if Temp = 0 then
      Last := Length(HTML);

    Link := Copy(HTML,1,Last - 1);

    Temp := Pos('<',Link);
    if Temp > 0 then
      Delete(Link,Temp,Length(Link));

    Temp := Pos('>',Link);
    if Temp > 0 then
      Delete(Link,Temp,Length(Link));

    Temp := Pos('"',Link);
    if Temp > 0 then
      Delete(Link,Temp,Length(Link));

    Temp := Pos(#39,Link);
    if Temp > 0 then
      Delete(Link,Temp,Length(Link));


    if URLs.IndexOf(Link) = -1 then
      URLs.Add(Link);

    Delete(HTML,1,Last);
  end;
end;

function GetTempName(const Prefix: String): String;
//temp fileを得る
var
  TempPath,TempFile: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH,TempPath);
  GetTempFileName(TempPath,PChar(PreFix),0,TempFile);
  Result := String(TempFile);
end;

function IsLinkLayer(const Url1,Url2: String): TLinkLayer;
//urlの関係を調べる
var
  U1,U2: TUrlInfo;
begin
  U1 := ParseUrl(Url1);
  U2 := ParseUrl(Url2);
  //hostが同じ
  if U1.Host = U2.Host then
  begin
    //dirが含まれれば下位
    if Pos(U1.Dir,U2.Dir) > 0 then
      Result := llLowerLayer
    else
      Result := llUpperLayer;
  end
  else //hostが違えば外部
    Result := llOthers;
end;

function BuildUrl(UrlInfo: TUrlInfo): String;
//Urlを構築する
var
  Host: String;
begin
  //user & pass
  if UrlInfo.UserId <> '' then
    Host := UrlInfo.UserId + ':' + UrlInfo.Password + '@' + UrlInfo.Host
  else
    Host := UrlInfo.Host;
  //port
  if UrlInfo.Port <> '' then
  begin
    if UrlInfo.Protocol = 'http' then
    begin
      if UrlInfo.Port <> '80' then
        Host := Host + ':' + UrlInfo.Port;
    end
    else if UrlInfo.Protocol = 'ftp' then
    begin
      if UrlInfo.Port <> '21' then
        Host := Host + ':' + UrlInfo.Port;
    end
    else if UrlInfo.Protocol = 'https' then
    begin
      if UrlInfo.Port <> '443' then
        Host := Host + ':' + UrlInfo.Port;
    end;
  end;

  Result := UrlInfo.Protocol + '://' + Host + UrlInfo.Path;
end;

function SocketState(var Socket: TSocket): TSocketState;
//ソケットの状態
var
  peer_adr: TSockAddr;
{$IFDEF WS2}
  x: Integer;
{$ELSE}
  x: u_int;
{$ENDIF}
begin
  if Socket = INVALID_SOCKET then
    Result := ssInvalid
  else begin
    x := SizeOf(TSockAddr);
    if GetPeerName(Socket,peer_adr,x) = 0 then
      Result := ssConnected
    else
      if WSAGetLastError <> WSAENOTCONN then
        Result := ssStateUnknown
      else
        Result := ssValid
  end;
end;

function AdjustHttpHeaders(const Headers: String): String;
//同名の httpヘッダを削除 後ろを優先する

  function GetHeader(const S: String): String;
  begin
    //headerを取り出す
    if Pos(':',S) > 0 then
      Result := LowerCase(Copy(S,1,Pos(':',S) - 1))
    else
      Result := LowerCase(S);
  end;

var
  SL: TStringList;
  S: String;
  i: Integer;
begin
  Result := '';
  SL := TStringList.Create;
  try
    SL.Text := Headers;
    while SL.Count > 0 do
    begin
      //最後を入れる
      Insert(SL[SL.Count - 1] + CRLF,Result,1);
      //Result := Result + SL[SL.Count - 1] + CRLF;
      S := GetHeader(SL[SL.Count - 1]);
      //削除
      SL.Delete(SL.Count - 1);

      //サーチ して消す
      for i := SL.Count - 1 downto 0 do
        if S = GetHeader(SL[i]) then
          SL.Delete(i);
    end;
  finally
    SL.Free;
  end;

  Result := Trim(Result);
end;

function WildcardMatching(const SearchString,Mask: String; IgnoreCase: Boolean): Boolean;
//windcard
var
  S,M: String;
  SS: String;
  C: WideChar;
begin
  S := SearchString;
  M := Mask;
  if IgnoreCase then
  begin
    S := UpperCase(S);
    M := UpperCase(M);
  end;

  while (Length(S) > 0) and (Length(M) > 0) do
  begin
    SS := Copy(M,1,1);
    C := SS[1];
    if C = '?' then
    begin
      Delete(S,1,1);
      Delete(M,1,1);
    end
    else
      if C = '*' then
      begin
        Delete(M,1,1);
        while (not WildcardMatching(S,M,IgnoreCase)) and
              (Length(S) > 0) do
          Delete(S,1,1);
      end
      else if Copy(S,1,1) = Copy(M,1,1) then
      begin
        Delete(S,1,1);
        Delete(M,1,1);
      end
      else S := '';
  end;

  Result := ((Length(S) = 0) and (Length(M) = 0)) or (M = '*');
end;

function FormatKillo(Value : Double): String;
//数字に ,を付ける
begin
  Result := FormatFloat(',0', Value);
end;

function GetDateTimeSimpleStr(DateTime: TDateTime): String;
//単純化した日付を返す
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Min, Sec, MSec);

  Result := IntToStr(Year) + IntToStr(Month) + IntToStr(Day) +
            IntToStr(Hour) + IntToStr(Min) + IntToStr(Sec);
end;

procedure HTTPHeaderToHash(const Header: String; HashTable: THashTable);
//HTTPヘッダをハッシュテーブルにして返す
var
  i,index: Integer;
  sl: TStringList;
  field,data: String;
begin
  sl := TStringList.Create;
  try
    sl.Text := Header;
    for i := 0 to sl.Count - 1 do
    begin
      index := Pos(':',sl[i]);
      if index > 0 then
      begin
        field := Trim(Copy(sl[i],1,index - 1));
        data := Trim(Copy(sl[i],index + 1,MaxInt));
        //tableに加える
        HashTable[field] := data;
      end;
    end;
  finally
    sl.Free;
  end;
end;

function HashToHTTPHeader(Headers: THashTable): String;
//HashtableをHTTPヘッダ文字列にする
var
  i: Integer;
  sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  try
    sl.Text := Headers.Keys;
    //全てのkeyを取り出して送る
    for i := 0 to sl.Count - 1 do
      Result := Result + sl[i] + ': ' + Headers[sl[i]] + CRLF;
  finally
    sl.Free;
  end;
  //最後のCRLFを消す
  Result := Trim(Result);
end;


{
const
   cStart     = 25569.0;  // = EncodeDate( 1970, 1, 1 )
   cSecPerDay = 86400.0;  // 1日あたりの秒数
   cAdjust    = 9 / 24;   // 時間帯の調整

function time_tToDateTime( t:time_t ): TDateTime;
begin
   Result := ( cStart * cSecPerDay + t ) / cSecPerDay + cAdjust;
end;

function DateTimeTotime_t( d:TDateTime ): time_t;
begin
   Result := Round( ( d - cStart - cAdjust ) * cSecPerDay );
end;
}

const
  UnixDateDelta = 25569;

function DateTimeToUnix(const AValue: TDateTime): Int64;
begin
  Result := Round((AValue - UnixDateDelta) * SecsPerDay);
end;

function UnixToDateTime(const AValue: Int64): TDateTime;
begin
  Result := AValue / SecsPerDay + UnixDateDelta;
end;

function GenerateRandomString(Len: Integer): String;
//ランダム文字列を作成
var
  i: Integer;
begin
  SetLength(Result,Len);
  for i := 1 to Len do
    Result[i] := ALPHABET[Random(52) + 1];
end;

function NormalUrl(Url: String; RemoveQuote: Boolean): String;
//url以外を削除する
var
  re: TRegExpr;
begin
  Result := '';
  re := TRegExpr.Create;
  try try
    if not RemoveQuote then
      re.Expression := REGEXP_URL
    else
      re.Expression := REGEXP_URL_REMOVE_QUOTE;

    if re.Exec(Url) then
      Result := Trim(re.Match[0]);
  except
    on ERegExpr do
  end;

  finally
    re.Free;
  end;
end;

procedure NormalUrls(Urls: TStrings; RemoveQuote: Boolean);
//url以外を削除する
var
  re: TRegExpr;
  i: Integer;
begin
  re := TRegExpr.Create;
  try try
    if not RemoveQuote then
      re.Expression := REGEXP_URL
    else
      re.Expression := REGEXP_URL_REMOVE_QUOTE;

    for i := Urls.Count - 1 downto 0 do
    begin
      //空白は消す
      if Urls[i] = '' then
        Urls.Delete(i)
      else if re.Exec(Urls[i]) then //マッチ
        Urls[i] := Trim(re.Match[0])
      else //マッチしない
        Urls.Delete(i);
    end;
  except
    on ERegExpr do
  end;

  finally
    re.Free;
  end;
end;

function NormalUrlAddHttp(Url: String): String;
//http://を付加する
begin
  Result := NormalUrl(Url,True);
  if (Result = '') and (AnsiPos('/',Url) > 0) then
    Result := 'http://' + Url;
end;

procedure NormalUrlsAddHttp(Urls: TStrings);
//http://を付加する
var
  i: Integer;
begin
  for i := Urls.Count - 1 downto 0 do
  begin
    Urls[i] := NormalUrlAddHttp(Urls[i]);
    if Urls[i] = '' then
      Urls.Delete(i);
  end;
end;


function NormalProxy(Proxy: String): String;
//proxy
var
  re: TRegExpr;
begin
  Result := '';
  re := TRegExpr.Create;
  try try
    re.Expression := REGEXP_PROXY;
    if re.Exec(Proxy) then
      Result := Trim(re.Match[0]);
  except
    on ERegExpr do
  end;

  finally
    re.Free;
  end;
end;

procedure NormalProxies(Proxies: TStrings);
//proxy以外を削除
var
  re: TRegExpr;
  i: Integer;
  flag: Boolean;
  sl: TStringList;
begin
  flag := (Proxies is TStringList) and
          ((Proxies as TStringList).Sorted);

  re := TRegExpr.Create;
  sl := TStringList.Create;
  try
    re.Expression := REGEXP_PROXY;
    for i := Proxies.Count - 1 downto 0 do
    begin
      if re.Exec(Proxies[i]) then
      begin
        //ソートされている
        if flag then
        begin
          Proxies.Delete(i);
          sl.Add(Trim(re.Match[0]));
        end
        else
          Proxies[i] := Trim(re.Match[0]);
      end
      else
        Proxies.Delete(i);
    end;

    //加える
    Proxies.AddStrings(sl);
  finally
    sl.Free;
    re.Free;
  end;
end;

function ExtractQuotedString(const S: string; Quote: Char): string;
var
  P: PChar;
begin
  P := PChar(S);
  if P^ = Quote then
    Result := AnsiExtractQuotedStr(P, Quote)
  else
    Result := S;
end;




initialization
  Randomize;
finalization

end.
