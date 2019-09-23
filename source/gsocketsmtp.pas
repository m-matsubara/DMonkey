unit gsocketsmtp;

//gsocket SMTP (by Wolfy)
//2001/04/29

interface

uses
  Windows,SysUtils,Classes,SyncObjs,gSocket,gSocketMisc,hashtable,regexpr
  ,jconvert,gsocketpop3,winsock;

type
  TgSMTP = class(TgSocket)
  private
    FMailMessage: TMailMessage;

    procedure Response;
    function GetFrom: String;
    function GetToMail: String;
    procedure SetFrom(const Value: String);
    procedure SetToMail(const Value: String);
  public
    constructor Create(BufferSize: Integer); override;
    destructor Destroy; override;

    procedure Connect; override;
    procedure Disconnect; override;
    procedure SendMail;

    property MailMessage: TMailMessage read FMailMessage;
    property From: String read GetFrom write SetFrom;
    property ToMail: String read GetToMail write SetToMail;  
  end;



implementation

{ TgSMTP }

procedure TgSMTP.Connect;
//接続
begin
  inherited;
  FConnected := True;
  Response; 

  if FStatusNo <> 220 then
    raise EProtocolError.Create('SMTP',FStatus,FStatusNo);

  DoCommand('HELO ' + GetSocketIPAddr(FSocket));
  Response;

  if FStatusNo <> 250 then
    raise EProtocolError.Create('SMTP',FStatus,FStatusNo);
end;

constructor TgSMTP.Create(BufferSize: Integer);
begin
  inherited Create(BufferSize);
  FMailMessage := TMailMessage.Create;
  FPort := 25;
end;

destructor TgSMTP.Destroy;
begin
  Disconnect;
  FMailMessage.Free;
  inherited;
end;

procedure TgSMTP.Disconnect;
//切断
begin
  if FConnected then
  begin
    try
      DoCommand('QUIT');
      //Response;
    except

    end;

    FConnected := false;
  end;
  inherited;  
end;

function TgSMTP.GetFrom: String;
begin
  Result := FMailMessage['From'];
end;

function TgSMTP.GetToMail: String;
begin
  Result := FMailMessage['To'];
end;

procedure TgSMTP.Response;
//レスポンス
var
  S: String;
begin
  S := ResultCommand;
  FStatusNo := StrToIntDef(Copy(S,1,3),999);
  FStatus := Copy(S,5,MaxInt);
  //400以上は例外
  if FStatusNo >= 400 then
    raise EProtocolError.Create('SMTP',FStatus,FStatusNo);
end;

procedure TgSMTP.SendMail;
//メールを送信
var
  i: Integer;
  sl: TStringList;
  head: Boolean;
begin
  Report('nfo>メールを送信します',Status_Informational);

  DoCommand('MAIL FROM:<' + From + '>');
  Response;
  if FStatusNo <> 250 then
    raise EProtocolError.Create('SMTP',FStatus,FStatusNo);

  DoCommand('RCPT TO:<' + ToMail + '>');
  Response;
  if FStatusNo <> 250 then
    raise EProtocolError.Create('SMTP',FStatus,FStatusNo);
  //送信
  DoCommand('DATA');
  Response;
  if FStatusNo <> 354 then
    raise EProtocolError.Create('SMTP',FStatus,FStatusNo);

  sl := TStringList.Create;
  try
    head := True;
    sl.Text := FMailMessage.Encode;
    for i := 0 to sl.Count - 1 do
    begin
      if head then
        DoCommand(sl[i])
      else
        Writeln(sl[i]);

      if sl[i] = '' then
        head := False;
    end;
    //最後
    Writeln('');
    Writeln('.');
  finally
    sl.Free;
  end;

  Report('nfo>メールの送信を終了しました',Status_Informational);
end;

procedure TgSMTP.SetFrom(const Value: String);
begin
  FMailMessage['From'] := Value;
end;

procedure TgSMTP.SetToMail(const Value: String);
begin
  FMailMessage['To'] := Value;
end;

end.
