unit gSocketPop3;
{
 TgPOP3: POP3基本クラス（未完成） 
   Author: Wolfy
 Modified: 00/05/10
  Version: 0.00
}


interface

uses
  Windows,SysUtils,Classes,SyncObjs,gSocket,gSocketMisc,hashtable,regexpr
  ,jconvert
{$IFDEF WS2}
  ,winsock2;
{$ELSE}
  ,Winsock;
{$ENDIF}

const
  _TO = 'To';
  _BCC = 'BCC';
  _CC = 'CC';
  _SUBJECT = 'Subject';
  _MESSAGEID = 'Message-Id';
  _FROM = 'From';
  _CONTENTTYPE = 'Content-Type';
  _CONTENTTRANSFERENCODING = 'Content-Transfer-Encoding';
  _CONTENTDISPOSITION = 'Content-Disposition';

type
  TMailMessage = class(TObject)
  private
    FAttachments: TStringList;
    FHash: TStringHashTable;
    FHead: TStringList;

    FNumber: Integer;
    FSize: Integer;
    FBody: TStringList;
    FMessage: String;
    //multipart 区切り線
    FBoundary: String;
    FFolder: String;
    
    function GetBCC: String;
    function GetCC: String;
    function GetFrom: String;
    function GetMessageId: String;
    function GetSubject: String;
    function GetToMail: String;
    function GetValue(Key: String): String;
    procedure SetBCC(const Value: String);
    procedure SetCC(const Value: String);
    procedure SetFrom(const Value: String);
    procedure SetSubject(const Value: String);
    procedure SetToMail(const Value: String);
    procedure SetValue(Key: String; const Value: String);
    procedure DecodeMultipart(SL: TStringList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Decode(const Mail: String);
    function Encode: String;
    procedure Assign(AMail: TMailMessage);

    property Number: Integer read FNumber write FNumber;
    property Size: Integer read FSize write FSize;
    property Attachments: TStringList read FAttachments;
    property Head: TStringList read FHead;
    property Body: TStringList read FBody;
    property From: String read GetFrom write SetFrom;
    property ToMail: String read GetToMail write SetToMail;
    property CC: String read GetCC write SetCC;
    property BCC: String read GetBCC write SetBCC;
    property MessageId: String read GetMessageId;
    property Subject: String read GetSubject write SetSubject;
    property Folder: String read FFolder write FFolder;
    property Message: String read FMessage write FMessage;
    property Value[Key: String]: String read GetValue write SetValue; default;
    property Hash: TStringHashTable read FHash;
  end;

  TListEvent = procedure(MsgNum,Size,MailCount: Integer) of object;

  TgPOP3 = class(TgSocket)
  protected
    //property
    FDeleteOnRead: Boolean;
    FMailCount: Integer;
    FAttachFilePath: String;
    FPassword: String;
    FUserId: String;
    FMailMessage: TMailMessage;
    FParse: Boolean;
    //event
    FOnAuthenticationFailed: THandlerEvent;
    FOnAuthenticationNeeded: THandlerEvent;
    FOnRetrieveEnd: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    FOnFailure: TNotifyEvent;
    FOnReset: TNotifyEvent;
    FOnRetrieveStart: TNotifyEvent;
    FOnList: TListEvent;

    procedure Response;
  public
    constructor Create(BufferSize: Integer); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    function UniqueId(MailNumber: Integer): String;
    procedure DeleteMailMessage(MailNumber: Integer);
    procedure GetMailMessage(MailNumber: Integer);
    procedure GetSummary(MailNumber: Integer);
    procedure List;
    procedure Reset;

    property AttachFilePath: String read FAttachFilePath write FAttachFilePath ;
    property DeleteOnRead: Boolean read FDeleteOnRead write FDeleteOnRead ;
    property MailCount: Integer read FMailCount;
    property Password: String read FPassword write FPassword ;
    property UserId: String read FUserId write FUserId ;
    property MailMessage: TMailMessage read FMailMessage;
    property Parse: Boolean read FParse write FParse;
    
    property OnAuthenticationFailed: THandlerEvent read FOnAuthenticationFailed write FOnAuthenticationFailed;
    property OnAuthenticationNeeded: THandlerEvent read FOnAuthenticationNeeded write FOnAuthenticationNeeded;
    property OnFailure: TNotifyEvent read FOnFailure write FOnFailure;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property OnReset: TNotifyEvent read FOnReset write FOnReset ;
    property OnRetrieveEnd: TNotifyEvent read FOnRetrieveEnd write FOnRetrieveEnd ;
    property OnRetrieveStart: TNotifyEvent read FOnRetrieveStart write FOnRetrieveStart;
    property OnList: TListEvent read FOnList write FOnList ;
  end;


implementation

{ TMailMessage }

procedure TMailMessage.Decode(const Mail: String);
//メールを解析する
var
  i,index: Integer;
  name,data,contenttype: String;
  sl: TStringList;
begin
  Clear;
  //headerは 連続改行まで
  FHead.Text := Copy(Mail,1,Pos(CRLF + CRLF,Mail) + 1);
  //それ以降は Body
  FBody.Text := Copy(Mail,Pos(CRLF + CRLF,Mail) + 4,MaxInt);

  //header解析
  for i := 0 to FHead.Count - 1 do
  begin
    index := Pos(':',FHead[i]);
    if index > 0 then
    begin
      name := Trim(Copy(FHead[i],1,index - 1));
      data := Trim(Copy(FHead[i],index + 1,MaxInt));
    end
    else begin
      name := FHead[i];
      data := '';
    end;
    //登録
    FHash[name] := data;
  end;

  //マルチパート解析
  contenttype := FHash[_CONTENTTYPE];
  if Pos('multipart/mixed',LowerCase(contenttype)) > 0 then
  begin
    //Content-Type: multipart/mixed; boundary="VGh1LCAxMSBNYXkgMjAwMCAxMTowMjoyMSArMDkwMA=="
    index := Pos('boundary="',LowerCase(contenttype));
    index := index + 10;
    FBoundary := Copy(contenttype,index,MaxInt);
    FBoundary := Copy(FBoundary,1,Pos('"',FBoundary) - 1);
    sl := TStringList.Create;
    try
      //最初の区切りまで飛ばす
      i := 0;
      while (i < FBody.Count) do
      begin
        if (Pos('--',FBody[i]) = 1) and (FBody[i] = FBoundary) then
        begin
          Inc(i);
          Break;
        end
        else
          Inc(i);
      end;
      //区切りがでるまで読み込み
      while (i < FBody.Count) do
      begin
        if (Pos('--',FBody[i]) = 1) and (Pos(FBoundary,FBody[i]) > 0) then
        begin
          //ヘッダを処理
          DecodeMultipart(sl);
          sl.Clear;
        end
        else begin
          sl.Add(FBody[i]);
        end;

        Inc(i);
      end;
      //残り
      //if sl.Count > 0 then
      //  DecodeMultipart(sl);

    finally
      sl.Free;
    end;  
  end
  else //マルチパートでないならばそのまま
    FMessage := FBody.Text;

end;

procedure TMailMessage.Clear;
begin
  FHead.Clear;
  FBody.Clear;
  FHash.Clear;
  FAttachments.Clear;
  FNumber := 0;
  FSize := 0;
  FMessage := '';
  //boudary生成
  FBoundary := GenerateRandomString(42);
  FFolder := '';
end;

constructor TMailMessage.Create;
begin
  inherited Create;
  FHead := TStringList.Create;
  FBody := TStringList.Create;
  FAttachments := TStringList.Create;
  FHash := TStringHashTable.Create(HASH_10,True);
  FHash.RaiseException := False;
  Clear;
end;

destructor TMailMessage.Destroy;
begin
  FHash.Free;
  FHead.Free;
  FBody.Free;
  FAttachments.Free;
  inherited Destroy;    
end;

function TMailMessage.GetBCC: String;
begin
  Result := FHash[_BCC];
end;

function TMailMessage.GetCC: String;
begin
  Result := FHash[_CC];
end;

function TMailMessage.GetFrom: String;
begin
  Result := FHash[_FROM];
end;

function TMailMessage.GetMessageId: String;
begin
  Result := FHash[_MESSAGEID];
end;

function TMailMessage.GetSubject: String;
begin
  Result := FHash[_SUBJECT];
end;

function TMailMessage.GetToMail: String;
begin
  Result := FHash[_TO];
end;

function TMailMessage.GetValue(Key: String): String;
begin
  Result := FHash[Key];
end;

procedure TMailMessage.SetBCC(const Value: String);
begin
  FHash[_BCC] := Value;
end;

procedure TMailMessage.SetCC(const Value: String);
begin
  FHash[_CC] := Value;
end;

procedure TMailMessage.SetFrom(const Value: String);
begin
  FHash[_FROM] := Value;
end;

procedure TMailMessage.SetSubject(const Value: String);
begin
  FHash[_SUBJECT] := Value;
end;

procedure TMailMessage.SetToMail(const Value: String);
begin
  FHash[_TO] := Value;
end;

procedure TMailMessage.SetValue(Key: String; const Value: String);
begin
  FHash[Key] := Value;
end;

procedure TMailMessage.DecodeMultipart(SL: TStringList);
var
  h: TStringHashTable;
  filename,name,data: String;
  s: String;
  i,index: Integer;
  fs: TFileStream;
begin
  //ヘッダを処理
  h := TStringHashTable.Create(HASH_10,True);
  try
    //改行まで飛ばす
    i := 0;
    while ((i < SL.Count) and (SL[i] <> '')) do
    begin
      index := Pos(':',SL[i]);
      if index > 0 then
      begin
        name := Trim(Copy(SL[i],1,index - 1));
        data := Trim(Copy(SL[i],index + 1,MaxInt));
      end
      else begin
        name := SL[i];
        data := '';
      end;
      //登録
      h[name] := data;

      Inc(i);
    end;

    //読み込み
    s := '';
    while (i < SL.Count) do
    begin
      s := s + SL[i] + CRLF;
      Inc(i);
    end;

    //content-typeをチェックして読み込み
    if Pos('text/plain',LowerCase(h[_CONTENTTYPE])) > 0 then
    begin
      //メッセージ
      FMessage := FMessage + s;
    end
    else begin        
      //ファイル保存
      index := Pos('name="',LowerCase(h[_CONTENTTYPE]));
      if index > 0 then
      begin
        name := Copy(h[_CONTENTTYPE],index + 6,MaxInt);
        name := Copy(name,1,Pos('"',name) - 1);
        filename := CheckPath(FFolder,PATH_DOS) + name;
      end
      else begin
        //Content-Disposition: attachment; filename="e.eml"
        index := Pos('filename="',LowerCase(h[_CONTENTDISPOSITION]));
        if index > 0 then
        begin
          name := Copy(h[_CONTENTDISPOSITION],index + 11,MaxInt);
          name := Copy(name,1,Pos('"',name) - 1);
          filename := CheckPath(FFolder,PATH_DOS) + name;
        end
        else  //ランダムに決める
          filename := GetTempName('gpop3');
      end;

      if 'base64' = LowerCase(h[_CONTENTTRANSFERENCODING]) then
      begin
        s := DecodeBase64(s);
        try
          fs := TFileStream.Create(filename,fmCreate);
          try
            fs.Write(s[1],Length(s));
            FAttachments.Add(filename);
          finally
            fs.Free;
          end;
        except
        end;
      end;
      
    end;
  finally
    h.Free;
  end;

end;

procedure TMailMessage.Assign(AMail: TMailMessage);
//コピーする
var
  sl: TStringList;
  i: Integer;
begin
  Clear;
  FAttachments.Assign(AMail.FAttachments);
  FHead.Assign(AMail.FHead);
  FBody.Assign(AMail.FBody);
  FNumber := AMail.FNumber;
  FSize := AMail.FSize;
  FMessage := AMail.FMessage;
  FBoundary := AMail.FBoundary;
  FFolder := AMail.FFolder;
  sl := TStringList.Create;
  try
    sl.Text := AMail.FHash.Keys;
    for i := 0 to sl.Count - 1 do
      FHash[sl[i]] := AMail.FHash[sl[i]]
  finally
    sl.Free;
  end;
end;

function TMailMessage.Encode: String;
//メールを作成する
var
  sl: TStringList;
  i: Integer;
  multipart: Boolean;
  s: String;
  fs: TFileStream;
begin
  multipart := (FAttachments.Count > 0);
  FBoundary := GenerateRandomString(42);
  sl := TStringlist.Create;
  try
    //ヘッダを作る
    FHead.Clear;
    sl.Text := FHash.Keys;
    for i := 0 to sl.Count - 1 do
      FHead.Add(sl[i] + ': ' + FHash[sl[i]]);
    //content-type
    if multipart then
      FHead.Add('Content-Type: multipart/mixed; boundary="' + FBoundary + '"')
    else
      FHead.Add('Content-Type: text/plain; charset="iso-2022-jp"');
    //date
    FHead.Add('Date: ' + DateTimeToHTTPModified(Now));
    FHead.Add('MIME-Version: 1.0');
    FHead.Add('Content-Transfer-Encoding: 7bit');   
    //改行
    FHead.Add('');
    //bodyを作る
    FBody.Clear;
    if not multipart then
      FBody.Add(FMessage)
    else begin
      //最初
      FBody.Add('--' + FBoundary);
      FBody.Add('Content-Type: text/plain');
      FBody.Add('');
      FBody.Add(FMessage);
      for i := 0 to FAttachments.Count - 1 do
      begin
        FBody.Add('--' + FBoundary);
        FBody.Add('Content-Type: application/octet-stream; name="' +
          ExtractFilename(FAttachments[i]) + '"');
        FBody.Add('Content-Disposition: filename="' +
          ExtractFilename(FAttachments[i]) + '"');
        FBody.Add('Content-Transfer-Encoding: Base64');
        FBody.Add('');
        try
          fs := TFileStream.Create(FAttachments[i],fmOpenRead);
          try
            SetLength(s,fs.Size);
            fs.Read(s[1],fs.Size);
            FBody.Add(EncodeBase64(s));
          finally
            fs.Free;
          end;
        except
          raise;
        end;
      end;
      //最後
      FBody.Add('--' + FBoundary + '--');
    end;
  finally
    sl.Free;
  end;
  Result := FHead.Text + FBody.Text;

end;

{ TgPOP3 }

procedure TgPOP3.Connect;
var
  Needed,Failed: Boolean;
  Cnt: Integer;
begin
  inherited Connect;
  Response;
  //初期化
  FConnected := True;
  FMailMessage.Clear;
  FMailCount := 0;
  Cnt := 0;

  Report('nfo>loginします',Status_Informational);
  while Cnt < 2 do
  begin
    Needed := False;
    Failed := False;
    //カウント 2になったら終り
    Inc(Cnt);
    //needed
    if (FUserId = '') then
    begin
      if Assigned(FOnAuthenticationNeeded) then
        FOnAuthenticationNeeded(Needed);

      if Needed and (Cnt < 2) then
        Continue
      else begin
        Report('err>useridがありません',Status_Basic);
        raise EProtocolError.Create('AuthenticationNeeded',FStatus,FStatusNo);
      end;
    end;
    
    try
      DoCommand('USER '+ FUserId);
      Response;
      DoCommand('PASS '+ FPassword);
      Response;
      Break;
    except
      if Assigned(FOnAuthenticationFailed) then
        FOnAuthenticationFailed(Failed);

      if Failed and (Cnt < 2) then
        Continue
      else 
        raise
    end;
  end;

  //STATでメール数確認
  try
    DoCommand('STAT');
    Response;
    FMailCount := StrToInt(Copy(FStatus,1,Pos(' ',FStatus) - 1));
  except
    ;
  end;  
end;

constructor TgPOP3.Create(BufferSize: Integer);
begin
  inherited Create(BufferSize);
  FMailMessage := TMailMessage.Create;
  FDeleteOnRead := False;
  FPort := 110;
end;

procedure TgPOP3.DeleteMailMessage(MailNumber: Integer);
//削除リストに入れとく
begin
  Report('nfo>メールを削除します',Status_Informational);

  try
    DoCommand('DELE ' + IntToStr(MailNumber));
    Response;
    if Assigned(FOnSuccess) then
      FOnSuccess(Self);

    Report('nfo>メール削除終了',Status_Informational);
  except
    if Assigned(FOnFailure) then
      FOnFailure(Self);

    Report('err>メール削除に失敗しました',Status_Basic);
    raise;
  end;
end;

destructor TgPOP3.Destroy;
begin
  FMailMessage.Free;
  inherited Destroy;
end;

procedure TgPOP3.Disconnect;
begin
  Report('nfo>logoutします',Status_Informational);

  if FConnected then
  begin
    try
      DoCommand('QUIT');
      Response;
    except
      ;
    end
  end;
  FConnected := False;

  inherited Disconnect;
end;

procedure TgPOP3.GetMailMessage(MailNumber: Integer);
//メールをget
var
  S,Mail: String;
  head: Boolean;
begin
  Report('nfo>メールを取得します',Status_Informational);

  try
    DoCommand('RETR '+ IntToStr(MailNumber));
    Response;
    if Assigned(FOnRetrieveStart) then
      FOnRetrieveStart(Self);

    FMailMessage.Clear;
    //添付ファイル保存
    FMailMessage.Folder := FAttachFilePath;
    Mail := '';
    head := True;
    repeat
      if head then
      begin
        S := ResultCommand;
        if S = '' then
          head := False;
      end
      else
        S := Readln;

      if S = '.' then
        Break;

      Mail := Mail + S + CRLF;
    until EOS(FSocket);
    //解析
    FMailMessage.Decode(Mail);

    if Assigned(FOnRetrieveEnd) then
      FOnRetrieveEnd(Self);

    Report('nfo>メール取得終了',Status_Informational);
    //if Assigned(FOnSuccess) then FOnSuccess(Self);
    //削除
    if FDeleteOnRead then
      DeleteMailMessage(MailNumber);
  except
    //if Assigned(FOnFailure) then FOnFailure(Self);
    Report('err>メール取得に失敗しました',Status_Basic);
    raise
  end;  
end;

procedure TgPOP3.GetSummary(MailNumber: Integer);
//summaryをget
var
  S,mail: String;
begin
  Report('nfo>ヘッダを取得します',Status_Informational);

  try
    DoCommand('TOP '+ IntToStr(MailNumber) + ' 1');
    Response;
    if Assigned(FOnRetrieveStart) then
      FOnRetrieveStart(Self);

    FMailMessage.Clear;;
    mail := '';
    repeat
      S := ResultCommand;
      if S = '.' then
        Break
      else
        mail := mail + S + CRLF;
    until EOS(FSocket);

    FMailMessage.Decode(mail);
    //number付加
    FMailMessage.Number := MailNumber;
    if Assigned(FOnRetrieveEnd) then
      FOnRetrieveEnd(Self);

    Report('nfo>ヘッダを取得終了',Status_Informational);
    //if Assigned(FOnSuccess) then FOnSuccess(Self);
  except
    //if Assigned(FOnFailure) then FOnFailure(Self);
    Report('err>ヘッダ取得に失敗しました',Status_Basic);
    raise;
  end;
end;

procedure TgPOP3.List;
var
  S,Temp: String;
  Msg,Size: Integer;
begin
  Report('nfo>listを取得します',Status_Informational);

  DoCommand('LIST');
  Response;
  FMailCount := 0;

  repeat
    S := ResultCommand;
    try
      Msg := StrToInt(Copy(S,1,Pos(' ',S) - 1));
      Temp := Copy(S,Pos(' ',S) + 1,Length(S));
      //サイズが( )されてた場合
      if Pos('(',Temp) > 0 then
      begin
        Temp := Copy(Temp,Pos('(',Temp) + 1,MaxInt);
        Size := StrToInt(Copy(Temp,1,Pos(')',Temp) - 1));
      end
      else
        Size := StrToInt(Temp);

      if Assigned(FOnList) then
        FOnList(Msg,Size,FMailCount);

      Inc(FMailCount);
    except
      ;
    end;

  until EOS(FSocket) or (S = '.');

  Report('nfo>list取得終了',Status_Informational);
end;

procedure TgPOP3.Reset;
//削除クリア
var
  Handler: Boolean;
begin
  Report('nfo>削除メールをリセットします',Status_Informational);

  Handler := False;
  if (not FConnected) and Assigned(FOnConnectionRequired) then
  begin
    FOnConnectionRequired(Handler);
    if not Handler then Exit;
  end;

  DoCommand('RSET');
  Response;
  if Assigned(FOnReset) then FOnReset(Self);
  
  Report('nfo>リセット正常終了',Status_Informational);
end;

procedure TgPOP3.Response;
//pop3レスポンス
var
  S: String;
begin
  S := ResultCommand;
  if Copy(S,1,3) = '+OK' then
    FStatus := Copy(S,5,Length(S))
  else if Copy(S,1,4) = '-ERR' then
  begin
    FStatus := Copy(S,6,Length(S));
    raise EProtocolError.Create('pop3',S,500);
  end
  else begin
    FStatus := '';
    raise EProtocolError.Create('pop3',S,999);
  end;
end;

function TgPOP3.UniqueId(MailNumber: Integer): String;
//message id
begin
  Report('nfo>message-idを取得します',Status_Informational);

  try
    GetSummary(MailNumber);
  except
    if Assigned(FOnFailure) then
      FOnFailure(Self);

    Report('err>message-id取得に失敗しました',Status_Basic);
    raise;
  end;

  Result := FMailMessage.MessageId;
  Report('nfo>message-id取得終了',Status_Informational);
end;

end.
