unit _testfrm;

//VCLフォーム作成用ユニット

interface

uses
  Windows, Messages, SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs,  ActnList,
  Menus, ExtCtrls, ComCtrls, ToolWin, StdCtrls, CheckLst, Spin,
  ecma_type,ecma_object;

type
  TfrmVCLTest = class(TForm)
    MainMenu1: TMainMenu;
    aaa1: TMenuItem;
    bbb1: TMenuItem;
    ccc1: TMenuItem;
    PopupMenu1: TPopupMenu;
    ddd1: TMenuItem;
    eee1: TMenuItem;
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    ListBox1: TListBox;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    RadioGroup1: TRadioGroup;
    Panel1: TPanel;
    Image1: TImage;
    CheckListBox1: TCheckListBox;
    Splitter1: TSplitter;
    Memo1: TMemo;
    TabControl1: TTabControl;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ProgressBar1: TProgressBar;
    TreeView1: TTreeView;
    ListView1: TListView;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    SpinEdit1: TSpinEdit;
    UpDown1: TUpDown;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton22: TToolButton;
    ToolButton21: TToolButton;
    PopupMenu2: TPopupMenu;
    menu21: TMenuItem;
    TabSheet2: TTabSheet;
    ActionList1: TActionList;
    Action1: TAction;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FontDialog1: TFontDialog;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

implementation

{$R *.DFM}


end.
