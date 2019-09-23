unit _dm_main;

//コンソールメイン
//2001/04/30
//by Wolfy

{..$DEFINE PRINT_TIME}

interface

uses
  windows,dmonkey,ecma_type,classes,sysutils,ecma_misc,_test;

type
  TDMMain = class(TObject)
  private
    FDM: TDMonkey;

    procedure DMStdout(Sender: TObject; S: String);
    procedure DMStdin(Sender: TObject; var S: String; var Success: Boolean;
      Count: Integer; Line: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;


implementation

{ TDMMain }

constructor TDMMain.Create;
begin
  inherited Create;
  FDM := TDMonkey.Create(nil);
  FDM.CompiledBinary := True;
  FDM.OnStdout := DMStdout;
  FDM.OnStderr := DMStdout;
  FDM.OnStdin := DMStdin;
  //var無しはGlobal変数にする
  FDM.RegistVar := rvGlobal;

  FDM.ImportObject('Test',TTestObject);
end;

destructor TDMMain.Destroy;
begin
  FreeAndNil(FDM);
  inherited;
end;

procedure TDMMain.DMStdin(Sender: TObject; var S: String; var Success: Boolean;
  Count: Integer; Line: Boolean);
begin
  Readln(S);
  Success := True;
end;

procedure TDMMain.DMStdout(Sender: TObject; S: String);
begin
  Write(S);
end;

procedure TDMMain.Run;
var
  param: TJValueList;
  i: Integer;
  filename,s: String;
begin
  param := TJValueList.Create;
  try
    if ParamCount < 1 then
    begin
      Writeln('usage: dm SCRIPT_NAME');
      Writeln('usage: dm -l ONE_LINE_SCRIPT(can not use double-quote)');
      Exit;
    end
    else begin
      if ParamStr(1) = '-l' then
      begin
        s := '';
        for i := 2 to ParamCount do
          s := s + ' ' + ParamStr(i);

        //one liner
        FDM.Compile(s);
        FDM.Run;
      end
      else begin
        filename := ParamStr(1);
        if ExtractFileExt(filename) = '' then
          filename := filename + DMS_EXT;

        if not FileExists(filename) then
        begin
          Writeln('file not found');
          Exit;
        end
        else begin
          for i := 2 to ParamCount do
            param.Add(BuildString(ParamStr(i)));

          FDM.CompileFile(filename);
          FDM.Run(param);
        end;
      end;
    end;
  finally
    param.Free;
  end;

{$IFDEF PRINT_TIME}
  Writeln('');
  Write('compile: ' + MSecToStr2(FDM.TookTimeToCompile));
  Writeln('  run: ' + MSecToStr2(FDM.TookTimeToRun));
{$ENDIF}
end;


end.
