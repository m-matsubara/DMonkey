unit _dmw1;

//Window版
//2001/04/30
//by Wolfy

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, ActnList, ExtCtrls, StdCtrls, ToolWin, DMonkey,ecma_type,
  Spin,ecma_misc,_test,shellapi,drag_drop;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    StatusBar1: TStatusBar;
    actFile: TAction;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    MainMenu1: TMainMenu;
    ToolBar1: TToolBar;
    File1: TMenuItem;
    actRun: TAction;
    N1: TMenuItem;
    DM: TDMS;
    actClear: TAction;
    actClearSource: TAction;
    actClearStdout: TAction;
    actClearDebug: TAction;
    Clear1: TMenuItem;
    Source1: TMenuItem;
    Stdout1: TMenuItem;
    Debug1: TMenuItem;
    actRunRun: TAction;
    Run1: TMenuItem;
    actRunFunction: TAction;
    Function1: TMenuItem;
    edtFunction: TEdit;
    mmStdout: TMemo;
    mmDebug: TMemo;
    mmSource: TMemo;
    opd: TOpenDialog;
    svd: TSaveDialog;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileOpen1: TMenuItem;
    actFileClose1: TMenuItem;
    actFileExit: TAction;
    N2: TMenuItem;
    Exit1: TMenuItem;
    actClearAll: TAction;
    All1: TMenuItem;
    ToolButton1: TToolButton;
    Edit1: TEdit;
    actRunRun2: TAction;
    actRunThread: TAction;
    edtThread: TSpinEdit;
    actRunThread1: TMenuItem;
    actRunAbort: TAction;
    N3: TMenuItem;
    actRunAbort1: TMenuItem;
    actTest: TAction;
    Test1: TMenuItem;
    Test21: TMenuItem;
    Test31: TMenuItem;
    procedure actFileExecute(Sender: TObject);
    procedure DMDegugout(Sender: TObject; S: String);
    procedure DMStdout(Sender: TObject; S: String);
    procedure actRunExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actClearSourceExecute(Sender: TObject);
    procedure actClearStdoutExecute(Sender: TObject);
    procedure actClearDebugExecute(Sender: TObject);
    procedure actRunRunExecute(Sender: TObject);
    procedure actRunFunctionExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure DMNewObject(Sender: TObject; JObject: TJObject);
    procedure actRunThreadExecute(Sender: TObject);
    procedure actRunAbortExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    procedure Test21Click(Sender: TObject);
    procedure mmSourceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mmStdoutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private 宣言 }

    FCount: Integer;
    FIndex: Integer;
    FAbort: Boolean;
    DragDropTarget1: TDragDropTarget;
    DragDropSource1: TDragDropSource;

    procedure DMOnDone(Sender: TObject);
    procedure DMOnStep(Sender: TObject; var Abort: Boolean);
    procedure DMOnError(Sender: TObject; LineNo: Integer; Msg: String);
    procedure ThreadDone(Sender: TObject);
    procedure DropFileEvent(Sender: TWinControl; X,Y: Integer; Files: TStrings);
    procedure DropUrlEvent(Sender: TWinControl; X,Y: Integer; Url: String);
    procedure DragDropOverEvent(Sender: TWinControl; X, Y: Integer; var Accept: Boolean);
  public
    { Public 宣言 }
  end;


  TRunThread = class(TThread)
  private
    FScript: String;
    FStdout: TStringList;
    FDebugout: TStringList;

    procedure DMDegugout(Sender: TObject; S: String);
    procedure DMStdout(Sender: TObject; S: String);
  protected
    procedure Execute; override;
  public
    constructor Create(S: String);
    destructor Destroy; override;
  end;


var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.actFileExecute(Sender: TObject);
begin
//
end;

procedure TForm1.DMDegugout(Sender: TObject; S: String);
begin
  mmDebug.Lines.Add(S);
end;

procedure TForm1.DMStdout(Sender: TObject; S: String);
begin
  if LastDelimiter(CRLF,S) = (Length(S)) then
    Delete(S,Length(S) - 1,2);

  mmStdout.Lines.Add(S);
end;

procedure TForm1.actRunExecute(Sender: TObject);
begin
//
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DM.OnDone := DMOnDone;
  DM.OnStep := DMOnStep;
  DM.OnError := DMOnError;
  //import
  DM.ImportObject('Test',TTestObject);

  DragDropTarget1 := TDragDropTarget.Create(Self);
  DragDropTarget1.OnDragDropFiles := DropFileEvent;
  DragDropTarget1.OnDragDropText := DropUrlEvent;
  DragDropTarget1.OnDragDropOver := DragDropOverEvent;
  DragDropTarget1.Target := mmSource;

  DragDropSource1 := TDragDropSource.Create(Self);


  if FileExists('__source.dms') then
    mmSource.Lines.LoadFromFile('__source.dms');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  mmSource.Lines.SaveToFile('__source.dms');
end;

procedure TForm1.actClearExecute(Sender: TObject);
begin
  //
end;

procedure TForm1.actClearSourceExecute(Sender: TObject);
begin
  mmSource.Clear;
end;

procedure TForm1.actClearStdoutExecute(Sender: TObject);
begin
  mmStdout.Clear;
end;

procedure TForm1.actClearDebugExecute(Sender: TObject);
begin
  mmDebug.Clear;
end;

procedure TForm1.actRunRunExecute(Sender: TObject);
begin
  if DM.Compile(mmSource.Lines.Text) then
    DM.Run
  else
    DM.Clear;

  StatusBar1.Panels[0].Text := 'compile: ' + MSecToStr2(DM.TookTimeToCompile) +
                           ' run: ' + MSecToStr2(DM.TookTimeToRun);
end;

procedure TForm1.actRunFunctionExecute(Sender: TObject);
var
  param: TJValueList;
  v: TJValue;
begin
  param := TJValueList.Create;
  try
    if DM.Compile(mmSource.Lines.Text) then
    begin
      DM.Run;
      DM.CallFunction(edtFunction.Text,param,v);
    end;
  finally
    param.Free;
  end;
end;

procedure TForm1.actFileOpenExecute(Sender: TObject);
begin
  if opd.Execute then
    mmSource.Lines.LoadFromFile(opd.Filename);
end;

procedure TForm1.actFileSaveExecute(Sender: TObject);
begin
  if svd.Execute then
    mmSource.Lines.SaveToFile(svd.Filename);
end;

procedure TForm1.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actClearAllExecute(Sender: TObject);
begin
  mmSource.Clear;
  mmStdout.Clear;
  mmDebug.Clear;
end;

procedure TForm1.DMOnDone(Sender: TObject);
begin
  Edit1.Text := IntToSTr(DM.ObjectCount);
end;

procedure TForm1.DMNewObject(Sender: TObject; JObject: TJObject);
begin
  Edit1.Text := IntToSTr(DM.ObjectCount);
end;

procedure TForm1.DMOnStep(Sender: TObject; var Abort: Boolean);
begin
  //Application.ProcessMessages;
end;

{ TRunThread }

constructor TRunThread.Create(S: String);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FScript := S;
  FStdout := TStringList.Create;
  FDebugout := TStringList.Create;
end;

destructor TRunThread.Destroy;
begin
  FreeAndNil(FStdout);
  FreeAndNil(FDebugout);
  inherited; 
end;

procedure TRunThread.DMDegugout(Sender: TObject; S: String);
begin
  FDebugout.Add(S);
end;

procedure TRunThread.DMStdout(Sender: TObject; S: String);
begin
  FStdout.Add(S);
end;

procedure TRunThread.Execute;
var
  dm: TDMonkey;
begin
  dm := TDMonkey.Create(nil);
  try
    dm.OnDebugout := DMDegugout;
    dm.OnStderr := DMStdout;
    dm.OnStdout := DMStdout;

    if dm.Compile(FScript) then
      dm.Run;
  finally
    dm.Free;
  end;
end;

procedure TForm1.actRunThreadExecute(Sender: TObject);
//スレッドで実行
var
  i: Integer;
begin
  FAbort := False;
  FIndex := 0;
  for i := 0 to edtThread.Value - 1 do
  begin
    if FAbort or Application.Terminated then
      Break;

    with TRunThread.Create(mmSource.Lines.Text) do
    begin
      OnTerminate := ThreadDone;
      Inc(FCount);
      Inc(FIndex);
      StatusBar1.Panels[0].Text := IntToStr(FIndex) + ': ' + IntToStr(FCount);

      Resume;
    end;

    while FCount > 30 do
    begin
      Application.ProcessMessages;
      Sleep(5);
    end;

    Application.ProcessMessages;
  end;

  while FCount > 0 do
    Application.ProcessMessages;

  showmessage('done');  
end;

procedure TForm1.ThreadDone(Sender: TObject);
var
  th: TRunThread;
begin
  th := Sender as TRunThread;
  mmStdout.Lines.AddStrings(th.FStdout);
  //mmDebug.Lines.AddStrings(th.FDebugout);

  Dec(FCount);
  StatusBar1.Panels[0].Text := IntToStr(FIndex) + ': ' + IntToStr(FCount);
end;

procedure TForm1.actRunAbortExecute(Sender: TObject);
begin
  FAbort := True;
end;

procedure TForm1.actTestExecute(Sender: TObject);
var
  s: String;
  date: TDateTime;
begin
  s := edtFunction.Text;
  mmStdout.Lines.Add(Format('%s %d %s',[s,MBLength(s),MBCopy(s,2,4)]));

  MBInsert('あ',s,4);
  mmStdout.Lines.Add(Format('%s: %s',['MBInsert(''あ'',s,4)',s]));

  MBDelete(s,4,2);
  mmStdout.Lines.Add(Format('%s: %s',['MBDelete(s,4,2)',s]));

  MBSetCharAt('お',s,4);
  mmStdout.Lines.Add(Format('%s: %s',['MBSetCharAt(''お'',s,4)',s]));
  mmStdout.Lines.Add(Format('%s: %s',['MBGetCharAt(s,4)',MBGetCharAt(s,4)]));
  mmStdout.Lines.Add(Format('%s: %s',['MBSlice(s,2,4)',MBSlice(s,2,4)]));

  mmStdout.Lines.Add(Format('%s: %d',['MBIndexOf(''お'',s,2)',MBIndexOf('お',s,2)]));
  mmStdout.Lines.Add(Format('%s: %d',
    ['MBLastIndexOf(''お'',s,2)',MBLastIndexOf('お',s,2)]));

  mmStdout.Lines.Add(Format('%s: %s',['MBReverse(s)',MBreverse(s)]));

  date := 25569;
  mmStdout.Lines.Add(DateTimeToStr(date));
end;

procedure TForm1.DMOnError(Sender: TObject; LineNo: Integer; Msg: String);
var
  index: Integer;
begin
  StatusBar1.Panels[1].Text := Format('%d: %s',[LineNo,Msg]);
  //カーソル移動
  index := mmSource.Perform(EM_LINEINDEX,LineNo - 1,0);
  if index > -1 then
  begin
    mmSource.SelStart := index;
    mmSource.Perform(EM_SCROLLCARET,0,0);
  end;
end;

procedure TForm1.Test21Click(Sender: TObject);
begin
  EnumSetNames(TypeInfo(TAnchors),MMDebug.Lines);
end;

procedure TForm1.DropFileEvent(Sender: TWinControl; X,Y: Integer; Files: TStrings);
begin
  mmSource.lines.LoadFromFile(files[0]);
end;

procedure TForm1.DropUrlEvent(Sender: TWinControl; X,Y: Integer; Url: String);
begin
  mmstdout.Lines.Add(url);
end;

procedure TForm1.mmSourceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  //if (ssRight in Shift) and (mmSource.SelLength > 0) then
  //  DragDropSource1.DoDragText(mmSource.SelText);
end;

procedure TForm1.mmStdoutMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//var
//  sl: TStringList;
begin
  {if (ssRight in Shift) and (mmStdout.SelLength > 0) then
  begin
    sl := TStringList.Create;
    try
      sl.Text := mmStdout.SelText;
      DragDropSource1.DoDragFiles('',sl);
    finally
      sl.Free;
    end;
  end;}
end;

procedure TForm1.DragDropOverEvent(Sender: TWinControl; X, Y: Integer;
  var Accept: Boolean);
begin

end;

end.
