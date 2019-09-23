program dm;
{$APPTYPE CONSOLE}

//コンソール
//2001/04/30
//Wolfy

{..$UNDEF MemCheckStackTrace}

uses
{$IFDEF MemCheckStackTrace}
  memcheck,
{$ENDIF}
  windows,
  sysutils,
  _dm_main,
  activex;

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

var
  main: TDMMain;
begin
{$IFDEF MemCheckStackTrace}
  // 例外ハンドラをインストール
  // なぜ MemCheck ユニット内でやらないかというと、MemCheck ユニットが
  // SysUtils ユニットよりも先にリンクされる必要があるため。
  MemCheckInstallExceptionHandler(GetExceptInfoFunc, SetExceptMessageFunc);
{$ENDIF}
  OleInitialize(nil);
  try
    main := TDMMain.Create;
    try try
      main.Run;
    finally
      main.Free;
    end;
    except
      on E:Exception do
      begin
        writeln(e.ClassName + ': ' + e.Message);
        readln;
      end;
    end;
    //readln;
  finally
    OleUninitialize;
  end;
end.
