unit _vclmemo;

//VCLフォーム作成用ユニット

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, Menus, StdCtrls,ecma_vcl,ComCtrls;

type
  TfrmVCLMemo = class(TForm)
    actionList: TActionList;
    mainMenu: TMainMenu;
    memo: TMemo;
    actFile: TAction;
    actEdit: TAction;
    actFormat: TAction;
    actHelp: TAction;
    actFile1: TMenuItem;
    actEdit1: TMenuItem;
    actFormat1: TMenuItem;
    actHelp1: TMenuItem;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSave: TAction;
    actFileSaveAs: TAction;
    actFilePage: TAction;
    actFilePrint: TAction;
    actFileClose: TAction;
    N1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    N2: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N3: TMenuItem;
    X1: TMenuItem;
    actEditUndo: TAction;
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    actEditFind: TAction;
    actEditFindNext: TAction;
    actEditReplace: TAction;
    actEditGoto: TAction;
    actEditSelectAll: TAction;
    actEditDateTime: TAction;
    Undo1: TMenuItem;
    N4: TMenuItem;
    Cut1: TMenuItem;
    Cut2: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N5: TMenuItem;
    F1: TMenuItem;
    FindNext1: TMenuItem;
    Replace1: TMenuItem;
    G1: TMenuItem;
    N6: TMenuItem;
    SelectAll1: TMenuItem;
    D1: TMenuItem;
    actFormatWordWrap: TAction;
    actFormatFont: TAction;
    actHelpTopic: TAction;
    actHelpAbout: TAction;
    W1: TMenuItem;
    Font1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    N7: TMenuItem;
    About1: TMenuItem;
    openDialog: TOpenDialog;
    saveDialog: TSaveDialog;
    fontDialog: TFontDialog;
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;


implementation

{$R *.DFM}



end.
