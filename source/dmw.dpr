program dmw;

{..$UNDEF MemCheckStackTrace}

uses
  memcheck,
  windows,
  sysutils,
  Forms,
  _dmw1 in '_dmw1.pas' {Form1};

{$R *.RES}

{$IFDEF MemCheckStackTrace}
procedure GetExceptInfoFunc(Obj: TObject;
  var Message: string; var ExceptionRecord: PExceptionRecord);
begin
  if Obj is Exception then
  begin
    Message := Exception(Obj).Message;
    if Obj is EExternal then
      ExceptionRecord := EExternal(Obj).ExceptionRecord;
  end;
end;

procedure SetExceptMessageFunc(Obj: TObject; const NewMessage: string);
begin
  if Obj is Exception then
    Exception(Obj).Message := NewMessage;
end;
{$ENDIF}

begin
{$IFDEF MemCheckStackTrace}
  // 例外ハンドラをインストール
  // なぜ MemCheck ユニット内でやらないかというと、MemCheck ユニットが
  // SysUtils ユニットよりも先にリンクされる必要があるため。
  MemCheckInstallExceptionHandler(GetExceptInfoFunc, SetExceptMessageFunc);
{$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

