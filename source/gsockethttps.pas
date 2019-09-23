unit gSocketHttps;

{
 TgHTTPS: HTTPS基本クラス 
   Author: Wolfy
 Modified: 00/05/10
  Version: 0.00
}

interface

uses
  Windows,SysUtils,Classes,SyncObjs,winsock,gSocket,gSocketMisc,gSocketHttp,wininet,
  hashtable,jconvert,regexpr,unicodelib;

type
  TgHTTPS = class(TgHTTP)
  private
    //wininet
    FOpenHandle,
    FConnectHandle,
    FRequestHandle: HInternet;
    FInternetFlags: DWord;
  protected      

    function EOS(var Socket: TSocket): Boolean; overload; override;
    function EOS: Boolean; overload; virtual;
    procedure Report(const S: String; Level: Integer); override;
    function ReadBuffer(var FileHandle: HInternet; var Buf; Size: DWord): DWord;
    procedure CaptureFromSocket(var RequestHandle: HInternet; Stream: TStream; Size, Position: Integer);
    function ReadLnFromSocket(var RequestHandle: HInternet): String;

  public
    constructor Create(BufferSize: Integer = BUFFER_SIZE); override;
    destructor Destroy; override;
    //socket
    procedure Connect; override;
    procedure Disconnect; override;
    procedure Abort; override;
    procedure Cancel; override;
    procedure CaptureFile(const FileName: String); override;
    procedure CaptureStream(Stream: TStream; Size: Integer); override;
    procedure CaptureString(var S: String; Size: Integer); override;
    function Read(Value: Integer): String; override;
    function ReadLn: String; override;
    procedure DoCommand(const CommandStr: String; Dummy: Boolean = False); override;
    function ReadVar(var Buf; Size: Integer): Integer; override;
    //http
    procedure Request(Method, Url: String; SendData: String = ''); override;
    procedure Response; override;
  end;




implementation

{ TgHTTPS }

procedure TgHTTPS.Abort;
begin
  Cancel;
end;

procedure TgHTTPS.Cancel;
begin
  FBeenCanceled := True;
  Report('rtn>cancelします',Status_Routines);
  if FConnected then Disconnect;

  raise ESocketCancel.Create('cancel');
end;

procedure TgHTTPS.CaptureFile(const FileName: String);
//captureしてファイルに保存 ファイルは新規
var
  FS: TFileStream;
begin
  //存在しなければ作成
  if not FileExists(FileName) then
    FS := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite)
  else
    FS := TFileStream.Create(FileName,fmOpenWrite or fmShareDenyWrite);

  try
    //最初から
    FS.Size := 0;
    CaptureFromSocket(FRequestHandle,FS,-1,0);
  finally
    FS.Free;
  end;
end;

procedure TgHTTPS.CaptureFromSocket(var RequestHandle: HInternet;
  Stream: TStream; Size, Position: Integer);
var
  rr: Integer;
begin
//  Report('trc>TgHTTPS.CaptureFromScoket',Status_Trace);

  Stream.Position := Position;
  //無制限
  if Size = -1 then
    repeat
      rr := ReadBuffer(RequestHandle,FBuffer^,FBufferSize);
      Stream.WriteBuffer(FBuffer^,rr);
    until EOS
  else
    repeat
      if Size > FBufferSize then
        rr := ReadBuffer(RequestHandle,FBuffer^,FBufferSize)
      else
        rr := ReadBuffer(RequestHandle,FBuffer^,Size);

      Stream.WriteBuffer(FBuffer^,rr);
      //0以下になったら終り
      Dec(Size,rr);
    until EOS or (Size <= 0);
end;

procedure TgHTTPS.CaptureStream(Stream: TStream; Size: Integer);
//socket streamを捕捉する streamの最後に追加する
begin
  Stream.Seek(0,soFromEnd);
  CaptureFromSocket(FRequestHandle,Stream,Size,Stream.Position);
end;

procedure TgHTTPS.CaptureString(var S: String; Size: Integer);
//socket を捕捉する Sの最後に追加する
var
  SS: TStringStream;
  DataS: String;
begin
  SS := TStringStream.Create(DataS);
  try
    SS.Seek(0,soFromEnd);
    CaptureFromSocket(FRequestHandle,SS,Size,SS.Position);
    S := S + SS.DataString;
  finally
    SS.Free;
  end;
end;

procedure TgHTTPS.Connect;
var
  Handled: Boolean;
  Count: Integer;
  ms: DWord;
begin
  Handled := False;
  Count := 0;
  if FConnected then
    Disconnect;

  //internet open
  //proxy 無し
  if FProxy = '' then
    FOpenHandle := InternetOpen(nil,INTERNET_OPEN_TYPE_DIRECT,nil,nil,0)
  else //proxyあり
    FOpenHandle := InternetOpen(nil,INTERNET_OPEN_TYPE_PROXY,
      PChar(FProxy),nil,0);

  if not Assigned(FOpenHandle) then raise ESocketError.Create(GetLastError);
  //timeout
  ms := FTimeout * 1000;
  InternetSetOption(FOpenHandle,INTERNET_OPTION_CONNECT_TIMEOUT,@ms, SizeOf(ms));
  InternetSetOption(FOpenHandle,INTERNET_OPTION_CONTROL_RECEIVE_TIMEOUT,@ms, sizeOf(ms));
  InternetSetOption(FOpenHandle,INTERNET_OPTION_CONTROL_SEND_TIMEOUT,@ms, sizeOf(ms));
  InternetSetOption(FOpenHandle,INTERNET_OPTION_DATA_SEND_TIMEOUT,@ms, SizeOf(ms));
  InternetSetOption(FOpenHandle,INTERNET_OPTION_DATA_RECEIVE_TIMEOUT,@ms, SizeOf(ms));
   
  Report('rtn>' + FHost + 'にconnectします',Status_Routines);
  repeat
    //connect
    FConnectHandle := InternetConnect(FOpenHandle,PChar(FHost),FPort,
      nil,nil,INTERNET_SERVICE_HTTP,0,0);

    if not Assigned(FConnectHandle) then
    begin
      FLastErrorNo := GetLastError;

      Report('err>' + FHost + ':' + IntToStr(FPort) + 'にconnect失敗しました' ,Status_Basic);
      if Assigned(FOnInvalidHost) then
        FOnInvalidHost(Handled);

      if Assigned(FOnConnectionFailed) then
        FOnConnectionFailed(Self);

      if Assigned(FOnError) then
        FOnError(Self,GetLastError,'connect error');
      //5回までトライ
      if handled and (Count < 5) then
        Continue;

      raise ESocketError.Create(GetLastError);
    end
    else
      Report('suc>' + FHost + ':' + IntToStr(FPort) + 'にconnect成功しました' ,Status_Basic);

    Inc(Count);
  until (not Handled) or (Count < 5);

  FConnected := True;
  //connect event
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

constructor TgHTTPS.Create(BufferSize: Integer);
begin
  inherited;
end;

destructor TgHTTPS.Destroy;
begin
  inherited Destroy;
end;

procedure TgHTTPS.Disconnect;
//internet handleを閉じる
begin
  if FConnected then
    Report('nfo>' + FHost + 'から切断します',Status_Basic);

  if FRequestHandle <> nil then
  begin
    InternetCloseHandle(FRequestHandle);
    FRequestHandle := nil;
  end;

  if FConnectHandle <> nil then
  begin
    InternetCloseHandle(FConnectHandle);
    FConnectHandle := nil;
  end;

  if FOpenHandle <> nil then
  begin
    InternetCloseHandle(FOpenHandle);
    FOpenHandle := nil;
  end;

  FConnected := False;

  if Assigned(FOnDisconnect) then
    FOnDisConnect(Self);
end;

procedure TgHTTPS.DoCommand(const CommandStr: String; Dummy: Boolean);
//コマンドを 1つのstringにして返す
begin
  if Trim(CommandStr) <> '' then
    Report('cmd>' + CommandStr,Status_Basic);
end;

function TgHTTPS.EOS(var Socket: TSocket): Boolean;
begin
//  Report('trc>TgHTTPS.EOS',Status_Trace);

  Result := FEOS;
end;

function TgHTTPS.EOS: Boolean;
begin
  Result := FEOS;
end;

function TgHTTPS.Read(Value: Integer): String;
//valueだけ読む
begin
  CaptureString(Result,Value);
end;

function TgHTTPS.ReadBuffer(var FileHandle: HInternet; var Buf;
  Size: DWord): DWord;
var
  r: Boolean;
  stop: Boolean;
begin
//  Report('trc>TgHTTPS.ReadBuffer',Status_Trace);

  FBeenTimeout := False;
  FEOS := True;

  r := InternetReadFile(FileHandle,@Buf,Size,Result);
  if not r then
  begin
    Report('err>socket error',Status_Basic);
    if Assigned(FOnError) then
      FOnError(Self,GetLastError,'');

    raise ESocketError.Create(GetLastError);
  end
  else if Result = 0 then
    FEOS := True
  //受信バイト
  else if Result > 0 then
  begin
    FEOS := False;
    //Report('dbg>socketから ' + IntToStr(Result) + ' bytes読み込みました',Status_Debug);
    Inc(FBytesRecvd,Result);
    Inc(FBytesTotal,Result);
  end;
  //recv event
  stop := False;
  if Assigned(FOnPacketRecvd) then
    FOnPacketRecvd(Self,Result,stop);
  //例外で止める
  if stop then
    raise ESocketError.Create(0);
end;

function TgHTTPS.ReadLn: String;
//一行ずつ読む 改行はつかない
begin
  Result := ReadLnFromSocket(FRequestHandle);
end;

function TgHTTPS.ReadLnFromSocket(var RequestHandle: HInternet): String;
//socketから一行ずつ読む 改行はつかない
//
//CR     CRで改行される保証が無い
//CR LF
//LF     LFであれば必ず改行
var
  C: Char;
  r: Integer;
begin
//  Report('trc>TgHTTPS.ReadLnFromSocket',Status_Trace);

  Result := '';
  repeat
    r := ReadBuffer(RequestHandle,C,1);
    if r <> 1 then
      Break;

    if C = CR then
    else if C = LF then
      Exit
    else
      Result := Result + C;

  until EOS;
end;

function TgHTTPS.ReadVar(var Buf; Size: Integer): Integer;
begin
  Result := ReadBuffer(FRequestHandle,Buf,Size);
end;

procedure TgHTTPS.Report(const S: String; Level: Integer);
//トレースログ ここで例外を起こすのを禁止！！
begin
  try
    //reportlevelより levelが小さければ
    if (FReportLevel >= Level) and (Assigned(FOnStatus)) then
      FOnStatus(Self,S);
  except
  end;
end;


procedure TgHTTPS.Request(Method, Url: String; SendData: String = '');
var
  U: TUrlInfo;
  headstr: String;
  SL: TStringList;
  i: Integer;
begin
  FUrl := Url;
  
  if Method = '' then
    Method := 'GET';

  FBeenCanceled := False;
  FBeenTimeout := False;
  //解析
  U := ParseUrl(Url);

  //host
  FReqHeader.Host := ExtractURLHostAndPort(Url);
  //authorization
  if (U.UserId <> '') or (U.Password <> '') then
    FReqHeader.ParseUserPass(U.UserId,U.PAssword);

  //postデータのヘッダ
  if (Method = 'POST') or (Method = 'PUT') or (Method = 'TRACE') then
  begin
    if SendData <> '' then
    begin
      //senddataを送る
      //Content-Type
      ReqHeader.ContentType := 'application/x-www-form-urlencoded';
      ReqHeader.ContentLength := IntToStr(Length(SendData));
    end;
  end;
  //イベント
  if Assigned(FOnAboutToSend) then
  begin
    FOnAboutToSend(Self,Url,FProxy,FReqHeader.Hash);
    //再び解析
    FUrl := Url;
    U := ParseUrl(Url);
  end; 

  if FEncodeUtf8 then
    U.Path := AnsiToUtf8(U.Path);
  //encodeする?
  if FEncodeUrl and (not IsUrlEncoded(U.Path)) then
    U.Path := EncodeURI(U.Path);
  //host
  FHost := U.Host;
  //port設定
  if U.Protocol = 'https' then
  begin
    if U.Port = '' then
      FPort := INTERNET_DEFAULT_HTTPS_PORT
    else
      FPort := StrToIntDef(U.Port,INTERNET_DEFAULT_HTTPS_PORT);
  end
  else begin
    if U.Port = '' then
      FPort := INTERNET_DEFAULT_HTTP_PORT
    else
      FPort := StrToIntDef(U.Port,INTERNET_DEFAULT_HTTP_PORT);
  end;


  //connect
  Connect;

  //サーバーにヘッダを送信
  Report('rtn>サーバへリクエスト送信',Status_Basic);

  //偽メソッド
  DoCommand(Method + ' ' + U.Path + ' HTTP/' + FVersion);
  //トレースしとく
  SL := TStringList.Create;
  try
    ReqHeader.GetHeader(SL);
    headstr := SL.Text;
    for i := 0 to SL.Count - 1 do
      DoCommand(SL[i]);

  finally
    SL.Free;
  end;

  //flag設定
  FInternetFlags := INTERNET_FLAG_RELOAD or INTERNET_FLAG_HYPERLINK or
                    INTERNET_FLAG_NO_UI or INTERNET_FLAG_NO_AUTO_REDIRECT;

  if U.Protocol = 'https' then
    FInternetFlags := FInternetFlags or
                      INTERNET_FLAG_SECURE or
                      INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;

  FRequestHandle := HttpOpenRequest(FConnectHandle,PChar(Method),
    PChar(U.Path),PChar('HTTP/' + Version),nil,nil,FInternetFlags,0);

  if not Assigned(FRequestHandle) then
    raise ESocketError.Create(GetlastError);

  //postデータのヘッダ
  if Method = 'POST' then
  begin
    if not HttpSendRequest(FRequestHandle,PChar(headstr),Length(headstr),PChar(SendData),Length(SendData)) then
      raise ESocketError.Create(GetLastError);
  end
  else
    if not HttpSendRequest(FRequestHandle,PChar(headstr),Length(headstr),nil,0) then
      raise ESocketError.Create(GetLastError);
end;


procedure TgHTTPS.Response;
var
  S,Temp: String;
  Field,Data: String;
  Buf: PChar;//array[0..65535] of Char;
  Size,rs: DWord;
  SL,tempsl: TStringList;
  i,All,Partial,index: Integer;
begin
  Report('rtn>サーバからのレスポンス',Status_Basic);
  FBodyLength := -1;
  FRedirectUrl := '';
  FStatusNo := 0;
  FHeader := '';
  FDisposition := '';
  FResHeader.Clear;

  Buf := StrAlloc(65535);
  try
    Size := 65535;
    rs := 0;
    if not HttpQueryInfo(FRequestHandle,
           HTTP_QUERY_RAW_HEADERS_CRLF,// or HTTP_QUERY_CUSTOM,
           Buf,Size,rs) then
      raise ESocketError.Create(GetLastError);

    FHeader := String(Buf);
  finally
    StrDispose(Buf);
  end;

  FTransActionReply := FHeader;

  SL := TStringList.Create;
  try
    SL.Text := FHeader;
    for i := 0 to SL.Count - 1 do
    begin
      //sに 1行読む
      S := SL[i];
      if S <> '' then Report('res>' + S,Status_Basic);

      if Copy(S,1,4) = 'HTTP' then
      begin
        Temp := S;
        System.Delete(Temp,1,Pos(' ',Temp));
        //応答番号を入れる
        index := Pos(' ',Temp);
        if index > 0 then
        begin
          FStatusNo := StrToIntDef(Copy(Temp,1,index - 1),999);
          // 応答コードをいれる
          FStatus := Copy(Temp,index + 1,MaxInt);
        end
        else begin
          FStatusNo := StrToIntDef(Temp,999);
          FStatus := '';
        end;
        //version
        FResHeader.Version := Copy(S,6,3);  
      end
      //ヘッダを処理
      else if Pos(':',S) > 0 then
      begin
        //field は ヘッダ名
        Field := LowerCase(Copy(S,1,Pos(':',S) - 1));
        //data は ヘッダのデータ
        Data := Trim(Copy(S,Pos(':',S) + 1,MaxInt));

        if Field = LowerCase(_SETCOOKIE) then
        begin
          //set-cookieは複数呼ばれる
          FResHeader.Cookie.Parse(FUrl,Data);
        end
        else begin
          FResHeader[Field] := Data;
        end;
      end;
    end; //for

    //イベントを先に呼ぶ
    if Assigned(FOnResponse) then
      FOnResponse(Self,FUrl,FProxy,FStatus,FRedirectUrl,FStatusNo,FResHeader.Hash);
    //ヘッダをチェック
    //location
    if FResHeader.HasName(_CONTENTLOCATION) then
      FRedirectUrl := ExpandUrl(FUrl,FResHeader[_CONTENTLOCATION]);

    if FResHeader.HasName(_LOCATION) then
      FRedirectUrl := ExpandUrl(FUrl,FResHeader[_LOCATION]);
    //bodylength
    if FResHeader.HasName(_CONTENTLENGTH) then
      FBodyLength := StrToIntDef(FResHeader[_CONTENTLENGTH],0);
    //bodylength
    if FResHeader.HasName(_CONTENTRANGE) then
    begin
      //fbodylength が -1の場合は
      if FBodyLength = -1 then
      begin
        Temp := FResHeader[_CONTENTRANGE];
        System.Delete(Temp,1,6);//bytes を消す
        Partial := StrToIntDef(Copy(Temp,1,Pos('-',Temp) - 1),0);
        All := StrToIntDef(Copy(Temp,Pos('/',Temp) + 1,MaxInt),0);
        //0でないならば
        if All <> 0 then
          FBodyLength := All - Partial;
      end;
    end;
    //disposition
    if FResHeader.HasName(_CONTENTDISPOSITION) then
    begin
      //ファイル名指定があれば
      Temp := FResHeader[_CONTENTDISPOSITION];
      tempsl := TStringList.Create;
      try
        // ;のみで区切る
        SplitRegExpr('[;]+',Temp,tempsl);
        //空白を消す
        for i := 0 to tempsl.Count - 1 do
          tempsl[i] := Trim(tempsl[i]);

        FDisposition := ExtractQuotedString(tempsl.Values['filename'],'"');
      finally
        tempsl.Free;
      end;
    end;
  finally
    SL.Free;
  end;

  //401の時はイベント
  if (FStatusNo = 401) and Assigned(FOnAuthenticationNeeded) then
    FOnAuthenticationNeeded(Self);

  //応答番号が400以上ならば 例外
  if FStatusNo >= 400 then
    raise EProtocolError.Create('http',FStatus,FStatusNo);
end;



end.
