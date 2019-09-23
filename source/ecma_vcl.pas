unit ecma_vcl;

interface

uses
  System.UITypes,
  System.Types,
  Windows,Sysutils,Classes,ecma_type,forms,controls,StdCtrls,messages,
  ecma_object,ecma_extobject,menus,extctrls,checklst,comctrls,hashtable,
  toolwin,graphics,spin,typinfo,ActnList,dialogs,ecma_re,drag_drop;

{$if CompilerVersion<=18.5}
type
  NativeInt = Integer;
{$ifend}

{テンプレート
  TJVCLBase_ = class(TJVCL_)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; AFactory: TJObjectFactory; Param: TJValueList; RegisteringFactory: Boolean = True); override;
    function Get_: T_;
    class function VCLClassType: TClass; override;
  published
  protected
    //イベント
  end;

  TJVCL_ = class(TJVCLBase_)
  protected
    procedure CreateVCL; override;
  end;

//イベント登録
var
  c: T_;
begin
  c := Get_;
  if not Assigned(c) then
    Exit;

  c.OnCanResize := OnCanResize;
  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDblClick := OnDblClick;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('on_') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','on_',param);
  finally
    param.Free;
  end;
end;

//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    Get_.
  except
    on E:Exception do
      Error(E.Message);
  end;

//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildBool(False);

  try
    i := Param[0];
    if CheckRange(AsInteger(@i)) then
    begin
      if IsParam2(Param) then
      begin
        //setter
        Result := Param[1];
        Get._[AsInteger(@i)] := AsBool(@Result);
      end
      else begin
        //getter
        Result := BuildBool(
          Get._[AsInteger(@i)]);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;

  }

type
  TJVCLComponent = class;
  TJVCLBaseAction = class;
  TJVCLBaseForm = class;
  TJVCLWinControl = class;

  TJVCLCastFunc = function(VCL: TPersistent; Engine: TJBaseEngine): TJVCLPersistent;

  TJVCLCaster = class(TObject)
  protected
    FItems: array of TJVCLCastFunc;
  public
    constructor Create;
    procedure Add(Func: TJVCLCastFunc); virtual;
    function Cast(VCL: TPersistent; Engine: TJBaseEngine): TJVCLPersistent; virtual;
  end;

  TJVCLNotify = class(TComponent)
  private
    FVCLObject: TJVCLComponent;
  public
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  TJVCLComponent = class(TJVCLPersistent)
  private
    FTag: Integer;
    function DoComponents(Param: TJValueList): TJValue;

    function GetComponentCount: Integer;
    function GetComponentIndex: Integer;
    procedure SetComponentIndex(const Value: Integer);
    function GetVCLOwner: TJVCLComponent;
  protected
    FNotify: TJVCLNotify;
    FComponents: TJHash;

    procedure CreateObjects; override;
    procedure DestroyVCL; override;
    function GetValueImpl(S: String; var RetVal: TJValue; Param: TJValueList = nil): Boolean; override;
    procedure RegistComponents; virtual;

    procedure Set_Action(const Value: TJVCLBaseAction); virtual;
    function Get_Action: TJVCLBaseAction; virtual;
    function GetSender(Sender: TObject): TJVCLComponent;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure GetPropertyList(List: TStringList); override;
    function HasKey(S: String): Boolean; override;
    procedure GetEventList(List: TStrings); override;

    function RegistVCL(AVCL: TPersistent; ACanDestroy: Boolean): Boolean; override;
    class function VCLClassType: TClass; override;
    function GetComponent: TComponent;

  published
    property ComponentCount: Integer read GetComponentCount;
    property ComponentIndex: Integer read GetComponentIndex write SetComponentIndex;
    property Owner: TJVCLComponent read GetVCLOwner;
    property Tag: Integer read FTag write FTag;
  protected
    //Notifyイベントは全部ここに集める
    procedure OnClick(Sender: TObject);
    procedure OnChange(Sender: TObject);
    procedure OnDblClick(Sender: TObject);
    procedure OnResize(Sender: TObject);
    procedure OnPopup(Sender: TObject);
    procedure OnEnter(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnActivate(Sender: TObject);
    procedure OnDeactivate(Sender: TObject);
    procedure OnCreate(Sender: TObject);
    procedure OnDestroy(Sender: TObject);
    procedure OnHide(Sender: TObject);
    procedure OnPaint(Sender: TObject);
    procedure OnShow(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnDropDown(Sender: TObject);
    procedure OnMoved(Sender: TObject);
    procedure OnClickCheck(Sender: TObject);
    procedure OnHint(Sender: TObject);
    procedure OnExecute(Sender: TObject);
    procedure OnUpdate(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnFolderChange(Sender: TObject);
    procedure OnSelectionChange(Sender: TObject);
    procedure OnTypeChange(Sender: TObject);
    procedure OnFind(Sender: TObject);
    procedure OnReplace(Sender: TObject);
  end;

  TJVCLControl = class(TJVCLComponent)
  private
    function DoBringToFront(Param: TJValueList): TJValue;
    function DoClientToScreen(Param: TJValueList): TJValue;
    function DoHasParent(Param: TJValueList): TJValue;
    function DoHide(Param: TJValueList): TJValue;
    function DoInvalidate(Param: TJValueList): TJValue;
    function DoPerform(Param: TJValueList): TJValue;
    function DoRefresh(Param: TJValueList): TJValue;
    function DoRepaint(Param: TJValueList): TJValue;
    function DoScreenToClient(Param: TJValueList): TJValue;
    function DoSendToBack(Param: TJValueList): TJValue;
    function DoSetBounds(Param: TJValueList): TJValue;
    function DoShow(Param: TJValueList): TJValue;
    function DoUpdate(Param: TJValueList): TJValue;

    procedure SetParent(const Value: TJVCLWinControl);
    function GetParent: TJVCLWinControl;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetControl: TControl;
    class function VCLClassType: TClass; override;
  published
    property parent: TJVCLWinControl read GetParent write SetParent;
  protected
    //イベント
    procedure OnCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure OnConstrainedResize(Sender: TObject; var MinWidth,
      MinHeight, MaxWidth, MaxHeight: Integer);
    procedure OnContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure OnDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure OnDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure OnEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure OnEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnStartDock(Sender: TObject;
      var DragObject: TDragDockObject);
    procedure OnStartDrag(Sender: TObject;
      var DragObject: TDragObject);
  end;

  TJVCLWinControl = class(TJVCLControl)
  private
    function DoCanFocus(Param: TJValueList): TJValue;
    function DoContainsControl(Param: TJValueList): TJValue;
    function DoControlAtPos(Param: TJValueList): TJValue;
    function DoDisableAlign(Param: TJValueList): TJValue;
    function DoEnableAlign(Param: TJValueList): TJValue;
    function DoFindChildControl(Param: TJValueList): TJValue;
    function DoFlipChildren(Param: TJValueList): TJValue;
    function DoFocused(Param: TJValueList): TJValue;
    function DoHandleAllocated(Param: TJValueList): TJValue;
    function DoHandleNeeded(Param: TJValueList): TJValue;
    function DoRealign(Param: TJValueList): TJValue;
    function DoScaleBy(Param: TJValueList): TJValue;
    function DoScrollBy(Param: TJValueList): TJValue;
    function DoSetFocus(Param: TJValueList): TJValue;
    function DoControls(Param: TJValueList): TJValue;

    function GetHandle: Integer;
    function GetControlCount: Integer;
    function GetParentWindow: Integer;
    function GetShowing: Boolean;
    function GetVisibleDockClientCount: Integer;
    procedure SetParentWindow(const Value: Integer);
    function GetacceptDrop: Boolean;
    procedure SetacceptDrop(const Value: Boolean);
  protected
    FDragDropTarget: TDragDropTarget;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    procedure GetEventList(List: TStrings); override;
    function GetWinControl: TWinControl;
    class function VCLClassType: TClass; override;
  protected
    //イベント
    procedure OnGetSiteInfo(Sender: TObject;
      DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
      var CanDock: Boolean);
    procedure OnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnUnDock(Sender: TObject; Client: TControl;
      NewTarget: TWinControl; var Allow: Boolean);
    procedure OnDockDrop(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer);
    procedure OnDockOver(Sender: TObject; Source: TDragDockObject;
      X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure OnDragDropFiles(Sender: TWinControl; X,Y: Integer; Files: TStrings);
    procedure OnDragDropText(Sender: TWinControl; X,Y: Integer; Text: String);
    //procedure OnDragDropOver(Sender: TWinControl; X, Y: Integer; var Accept: Boolean);
  published
    property VisibleDockClientCount: Integer read GetVisibleDockClientCount;
    property ControlCount: Integer read GetControlCount;
    property handle: Integer read GetHandle;
    property ParentWindow: Integer read GetParentWindow write SetParentWindow;
    property Showing: Boolean read GetShowing;
    property acceptDrop: Boolean read GetacceptDrop write SetacceptDrop;
  end;

  TJVCLBaseForm = class(TJVCLWinControl)
  private
    FMain: Boolean;
    FDragDropSource: TDragDropSource;
    function DoClose(Param: TJValueList): TJValue;
    function DoCloseQuery(Param: TJValueList): TJValue;
    function DoFocusControl(Param: TJValueList): TJValue;
    function DoHide(Param: TJValueList): TJValue;
    function DoPrint(Param: TJValueList): TJValue;
    function DoRelease(Param: TJValueList): TJValue;
    function DoSendCancelMode(Param: TJValueList): TJValue;
    function DoSetFocusedControl(Param: TJValueList): TJValue;
    function DoShow(Param: TJValueList): TJValue;
    function DoShowModal(Param: TJValueList): TJValue;
    function DoArrangeIcons(Param: TJValueList): TJValue;
    function DoCascade(Param: TJValueList): TJValue;
    function DoNext(Param: TJValueList): TJValue;
    function DoPrevious(Param: TJValueList): TJValue;
    function DoTile(Param: TJValueList): TJValue;
    function DoDoDragDropFiles(Param: TJValueList): TJValue;
    function DoDoDragDropText(Param: TJValueList): TJValue;

    function DoSaveToFile(Param: TJValueList): TJValue;
    function DoSaveToText(Param: TJValueList): TJValue;
    function DoLoadFromFile(Param: TJValueList): TJValue;
    function DoLoadFromText(Param: TJValueList): TJValue;
    function GetActive: Boolean;
  protected
    procedure RegistEvents; override;
    procedure LoadComponents(Stream: TStream); virtual;
    procedure LoadComponentsFromFile(Filename: String);
    procedure LoadComponentsFromText(S: String);
    procedure SaveComponents(Stream: TStream); virtual;
    procedure SaveComponentsToFile(Filename: String);
    function SaveComponentsToText: String;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetForm: TForm;
    function GetCustomForm: TCustomForm;
    class function VCLClassType: TClass; override;
  published
    property main: Boolean read FMain write FMain;
    property Active: Boolean read GetActive;
    //property icon;
  protected
    //イベント
    procedure OnClose(Sender: TObject; var Action: TCloseAction);
    procedure OnCloseQuery(Sender: TObject; var CanClose: Boolean);
    function  OnHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
  end;

  TJVCLForm = class(TJVCLBaseForm)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseButton = class(TJVCLWinControl)
  private
    function DoClick(Param: TJValueList): TJValue; virtual;
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetButton: TButton;
    function GetButtonControl: TButtonControl;
    class function VCLClassType: TClass; override;
  published
    property action: TJVCLBaseAction read Get_Action write Set_Action;
  end;

  TJVCLButton = class(TJVCLBaseButton)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseEdit = class(TJVCLWinControl)
  private
    function DoClear(Param: TJValueList): TJValue;
    function DoClearSelection(Param: TJValueList): TJValue;
    function DoCopyToClipboard(Param: TJValueList): TJValue;
    function DoCutToClipboard(Param: TJValueList): TJValue;
    function DoPasteFromClipboard(Param: TJValueList): TJValue;
    function DoUndo(Param: TJValueList): TJValue;
    function DoClearUndo(Param: TJValueList): TJValue;
    function DoSelectAll(Param: TJValueList): TJValue;
    function GetCanUndo: Boolean;
    function GetModified: Boolean;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    procedure SetModified(const Value: Boolean);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelText(const Value: string);
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetEdit: TEdit;
    function GetCustomEdit: TCustomEdit;
    class function VCLClassType: TClass; override;
  published
    property CanUndo: Boolean read GetCanUndo;
    property Modified: Boolean read GetModified write SetModified;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
  end;

  TJVCLEdit = class(TJVCLBaseEdit)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseMemo = class(TJVCLBaseEdit)
  private
    FLines: TJBaseStringsObject;
    FCaretPos: TJPoint;
    function GetCaretPos: TJPoint;
    function GetLines: TJBaseStringsObject;
    function GetText: String;
    procedure SetText(const Value: String);
  protected
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetMemo: TMemo;
    function GetCustomMemo: TCustomMemo;
    class function VCLClassType: TClass; override;
  published
    property lines: TJBaseStringsObject read GetLines;
    property caretPos: TJPoint read GetCaretPos;
    property text: String read GetText write SetText;
  end;

  TJVCLMemo = class(TJVCLBaseMemo)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseSpinEdit = class(TJVCLBaseEdit)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetSpinEdit: TSpinEdit;
    class function VCLClassType: TClass; override;
  published
  end;

  TJVCLSpinEdit = class(TJVCLBaseSpinEdit)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseLabel = class(TJVCLControl)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetLabel: TLabel;
    class function VCLClassType: TClass; override;
  published
  protected
  end;

  TJVCLLabel = class(TJVCLBaseLabel)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseTimer = class(TJVCLComponent)
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetTimer: TTimer;
    class function VCLClassType: TClass; override;
  published
  end;

  TJVCLTimer = class(TJVCLBaseTimer)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseMenuItem = class(TJVCLComponent)
  private
    function DoInitiateAction(Param: TJValueList): TJValue;
    function DoInsert(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    function DoClick(Param: TJValueList): TJValue;
    function DoFind(Param: TJValueList): TJValue;
    function DoIndexOf(Param: TJValueList): TJValue;
    function DoIsLine(Param: TJValueList): TJValue;
    function DoGetParentMenu(Param: TJValueList): TJValue;
    function DoNewTopLine(Param: TJValueList): TJValue;
    function DoNewBottomLine(Param: TJValueList): TJValue;
    function DoInsertNewLineBefore(Param: TJValueList): TJValue;
    function DoInsertNewLineAfter(Param: TJValueList): TJValue;
    function DoAdd(Param: TJValueList): TJValue;
    function DoRemove(Param: TJValueList): TJValue;
    function DoRethinkHotkeys(Param: TJValueList): TJValue;
    function DoRethinkLines(Param: TJValueList): TJValue;
    function DoItems(Param: TJValueList): TJValue;

    function GetCommand: Integer;
    function GetHandle: Integer;
    function GetMenuIndex: Integer;
    function GetParent: TJVCLBaseMenuItem;
    procedure SetMenuIndex(const Value: Integer);

  protected
    procedure RegistEvents; override;
    function CheckRange(Index: Integer): Boolean;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetMenuItem: TMenuItem;
    class function VCLClassType: TClass; override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property action: TJVCLBaseAction read Get_Action write Set_Action;
    property Command: Integer read GetCommand;
    property Handle: Integer read GetHandle;
    property Count: Integer read GetCount;
    property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
    property Parent: TJVCLBaseMenuItem read GetParent;
  end;

  TJVCLMenuItem = class(TJVCLBaseMenuItem)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseMenu = class(TJVCLComponent)
  private
    FItems: TJVCLBaseMenuItem;
    function DoDispatchCommand(Param: TJValueList): TJValue;
    function DoDispatchPopup(Param: TJValueList): TJValue;
    function DoFindItem(Param: TJValueList): TJValue;
    function DoGetHelpContext(Param: TJValueList): TJValue;

    function GetHandle: Integer;
    function GetWindowHandle: Integer;
    procedure SetWindowHandle(const Value: Integer);
    function GetItems: TJVCLBaseMenuItem;
  protected
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetMenu: TMenu;
    class function VCLClassType: TClass; override;
  published
    property Handle: Integer read GetHandle;
    property WindowHandle: Integer read GetWindowHandle write SetWindowHandle;
    property items: TJVCLBaseMenuItem read GetItems;
  protected
    procedure OnChange(Sender: TObject; Source: TMenuItem;
      Rebuild: Boolean);
  end;

  TJVCLMenu = class(TJVCLBaseMenu)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseMainMenu = class(TJVCLBaseMenu)
  private
    function DoMerge(Param: TJValueList): TJValue;
    function DoUnmerge(Param: TJValueList): TJValue;
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetMainMenu: TMainMenu;
    class function VCLClassType: TClass; override;
  published
  protected
  end;

  TJVCLMainMenu = class(TJVCLBaseMainMenu)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBasePopupMenu = class(TJVCLBaseMenu)
  private
    function DoPopup(Param: TJValueList): TJValue;

    function GetPopupComponent: TJVCLComponent;
    procedure SetPopupComponent(const Value: TJVCLComponent);
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetPopupMenu: TPopupMenu;
    class function VCLClassType: TClass; override;
  published
    property PopupComponent: TJVCLComponent read GetPopupComponent write SetPopupComponent;
  protected
  end;

  TJVCLPopupMenu = class(TJVCLBasePopupMenu)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseCheckBox = class(TJVCLWinControl)
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetCheckBox: TCheckBox;
    function GetCustomCheckBox: TCustomCheckBox;
    class function VCLClassType: TClass; override;
  published
    property action: TJVCLBaseAction read Get_Action write Set_Action;
  end;

  TJVCLCheckBox = class(TJVCLBaseCheckBox)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseRadioButton = class(TJVCLWinControl)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetRadioButton: TRadioButton;
    class function VCLClassType: TClass; override;
  published
    property action: TJVCLBaseAction read Get_Action write Set_Action;
  protected
  end;

  TJVCLRadioButton = class(TJVCLBaseRadioButton)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseListBox = class(TJVCLWinControl)
  private
    FItems: TJBaseStringsObject;

    function DoClear(Param: TJValueList): TJValue;
    function DoClearSelection(Param: TJValueList): TJValue;
    function DoDeleteSelected(Param: TJValueList): TJValue;
    function DoItemAtPos(Param: TJValueList): TJValue;
    function DoItemRect(Param: TJValueList): TJValue;
    function DoSelectAll(Param: TJValueList): TJValue;
    function DoSelected(Param: TJValueList): TJValue;

    function GetItemIndex: Integer;
    function GetItems: TJBaseStringsObject;
    function GetSelCount: Integer;
    function GetTopIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetTopIndex(const Value: Integer);
  protected
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
    function CheckRange(Index: Integer): Boolean;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetListBox: TListBox;
    function GetCustomListBox: TCustomListBox;
    class function VCLClassType: TClass; override;
  published
    property Items: TJBaseStringsObject read GetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelCount: Integer read GetSelCount;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
  protected
  end;

  TJVCLListBox = class(TJVCLBaseListBox)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseComboBox = class(TJVCLWinControl)
  private
    FItems: TJBaseStringsObject;

    function DoClear(Param: TJValueList): TJValue;
    function DoSelectAll(Param: TJValueList): TJValue;

    function GetCharCase: String;
    function GetDroppedDown: Boolean;
    function GetItemIndex: Integer;
    function GetItems: TJBaseStringsObject;
    function GetSelLength: Integer;
    function GetSelStart: Integer;
    function GetSelText: string;
    procedure SetCharCase(const Value: String);
    procedure SetDroppedDown(const Value: Boolean);
    procedure SetItemIndex(const Value: Integer);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(const Value: Integer);
    procedure SetSelText(const Value: string);
  protected
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetComboBox: TComboBox;
    function GetCustomComboBox: TCustomComboBox;
    class function VCLClassType: TClass; override;
  published
    property CharCase: String read GetCharCase write SetCharCase;
    property DroppedDown: Boolean read GetDroppedDown write SetDroppedDown;
    property Items: TJBaseStringsObject read GetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelLength: Integer read GetSelLength write SetSelLength;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelText: string read GetSelText write SetSelText;
  protected
  end;

  TJVCLComboBox = class(TJVCLBaseComboBox)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseGroupBox = class(TJVCLWinControl)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetGroupBox: TGroupBox;
    class function VCLClassType: TClass; override;
  published
  protected
  end;

  TJVCLGroupBox = class(TJVCLBaseGroupBox)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseRadioGroup = class(TJVCLWinControl)
  private
    FItems: TJBaseStringsObject;
    function GetItems: TJBaseStringsObject;
  protected
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetRadioGroup: TRadioGroup;
    class function VCLClassType: TClass; override;
  published
     property Items: TJBaseStringsObject read GetItems;
  protected
  end;

  TJVCLRadioGroup = class(TJVCLBaseRadioGroup)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBasePanel = class(TJVCLWinControl)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetPanel: TPanel;
    class function VCLClassType: TClass; override;
  published
  protected
  end;

  TJVCLPanel = class(TJVCLBasePanel)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseSplitter = class(TJVCLControl)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetSplitter: TSplitter;
    class function VCLClassType: TClass; override;
  published
  protected
    procedure OnCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
  end;

  TJVCLSplitter = class(TJVCLBaseSplitter)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseCheckListBox = class(TJVCLBaseListBox)
  private
    function DoChecked(Param: TJValueList): TJValue;
    function DoItemEnabled(Param: TJValueList): TJValue;
    function DoState(Param: TJValueList): TJValue;
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetCheckListBox: TCheckListBox;
    class function VCLClassType: TClass; override;
  published
  protected
  end;

  TJVCLCheckListBox = class(TJVCLBaseCheckListBox)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseImage = class(TJVCLControl)
  private

  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetImage: TImage;
    class function VCLClassType: TClass; override;
  published

  protected
    procedure OnProgress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
      const Msg: String);
  end;

  TJVCLImage = class(TJVCLBaseImage)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseTabControl = class(TJVCLWinControl)
  private
    FTabs: TJBaseStringsObject;
    function DoIndexOfTabAt(Param: TJValueList): TJValue;
    function DoGetHitTestInfoAt(Param: TJValueList): TJValue;
    function DoTabRect(Param: TJValueList): TJValue;
    function DoScrollTabs(Param: TJValueList): TJValue;

    function GetDisplayRect: TJRect;
    function GetRowCount: Integer;
    function GetTabs: TJBaseStringsObject;
  protected
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetTabControl: TTabControl;
    function GetCustomTabControl: TCustomTabControl;
    class function VCLClassType: TClass; override;
  published
    property RowCount: Integer read GetRowCount;
    property DisplayRect: TJRect read GetDisplayRect;
    property tabs: TJBaseStringsObject read GetTabs;
  protected
    procedure OnChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure OnDrawTab(Control: TCustomTabControl;
      TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure OnGetImageIndex(Sender: TObject; TabIndex: Integer;
      var ImageIndex: Integer);
  end;

  TJVCLTabControl = class(TJVCLBaseTabControl)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBasePageControl = class;

  TJVCLBaseTabSheet = class(TJVCLWinControl)
  private
    function GetPageControl: TJVCLBasePageControl;
    function GetTabIndex: Integer;
    procedure SetPageControl(const Value: TJVCLBasePageControl);
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetTabSheet: TTabSheet;
    class function VCLClassType: TClass; override;
  published
    property PageControl: TJVCLBasePageControl read GetPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex;
  protected
  end;

  TJVCLTabSheet = class(TJVCLBaseTabSheet)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBasePageControl = class(TJVCLBaseTabControl)
  private
    function DoFindNextPage(Param: TJValueList): TJValue;
    function DoSelectNextPage(Param: TJValueList): TJValue;
    function DoPages(Param: TJValueList): TJValue;

    function GetActivePageIndex: Integer;
    function GetPageCount: Integer;
    procedure SetActivePageIndex(const Value: Integer);
  protected
    procedure RegistEvents; override;
    function CheckRange(Index: Integer): Boolean;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetPageControl: TPageControl;
    class function VCLClassType: TClass; override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property PageCount: Integer read GetPageCount;
  protected
  end;

  TJVCLPageControl = class(TJVCLBasePageControl)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseProgressBar = class(TJVCLWinControl)
  private
    function DoStepIt(Param: TJValueList): TJValue;
    function DoStepBy(Param: TJValueList): TJValue;
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetProgressBar: TProgressBar;
    class function VCLClassType: TClass; override;
  end;

  TJVCLProgressBar = class(TJVCLBaseProgressBar)
  protected
    procedure CreateVCL; override;
  end;

  //Item生成はTListView.OnInsert
  //Item破棄はTListView.OnDeleting

  TJVCLBaseListView = class;
  TJVCLBaseListItems = class;

  TJVCLBaseListItem = class(TJVCLPersistent)
  private
    FData: TJObject;
    FListView: TJVCLBaseListView;
    FSubItems: TJBaseStringsObject;

    function DoCancelEdit(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoDisplayRect(Param: TJValueList): TJValue;
    function DoEditCaption(Param: TJValueList): TJValue;
    function DoMakeVisible(Param: TJValueList): TJValue;
    function DoUpdate(Param: TJValueList): TJValue;
    function DoWorkArea(Param: TJValueList): TJValue;
    function DoSubItemImages(Param: TJValueList): TJValue;

    function GetCaption: string;
    function GetChecked: Boolean;
    function GetCut: Boolean;
    function GetData: TJObject;
    function GetDropTarget: Boolean;
    function GetFocused: Boolean;
    function GetHandle: Integer;
    function GetImageIndex: Integer;
    function GetIndent: Integer;
    function GetIndex: Integer;
    function GetLeft: Integer;
    function GetOverlayIndex: Integer;
    function GetPosition: TJObject;
    function GetSelected: Boolean;
    function GetStateIndex: Integer;
    function GetSubItems: TJBaseStringsObject;
    function GetTop: Integer;
    procedure SetCaption(const Value: string);
    procedure SetChecked(const Value: Boolean);
    procedure SetCut(const Value: Boolean);
    procedure SetData(const Value: TJObject);
    procedure SetDropTarget(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetIndent(const Value: Integer);
    procedure SetLeft(const Value: Integer);
    procedure SetOverlayIndex(const Value: Integer);
    procedure SetPosition(const Value: TJObject);
    procedure SetSelected(const Value: Boolean);
    procedure SetStateIndex(const Value: Integer);
    procedure SetTop(const Value: Integer);
    function GetListItems: TJVCLBaseListItems;
    function GetListView: TJVCLBaseListView;

    function CheckSubItemsRange(Index: Integer): Boolean;
  protected
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
    procedure Notification(AObject: TJNotify); override;
    procedure CheckVCL(Param: TJValueList = nil; ArgCount: Integer = 0); override;
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetListItem: TListItem;
    class function VCLClassType: TClass; override;
    function RegistVCL(AVCL: TPersistent; ACanDestroy: Boolean): Boolean; override;
  published
    property Caption: string read GetCaption write SetCaption;
    property Checked: Boolean read GetChecked write SetChecked;
    property Cut: Boolean read GetCut write SetCut;
    property Data: TJObject read GetData write SetData;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Focused: Boolean read GetFocused write SetFocused;
    property Handle: Integer read GetHandle;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property Indent: Integer read GetIndent write SetIndent default 0;
    property Index: Integer read GetIndex;
    property Left: Integer read GetLeft write SetLeft;
    property ListView: TJVCLBaseListView read GetListView;
    property Owner: TJVCLBaseListItems read GetListItems;
    property OverlayIndex: Integer read GetOverlayIndex write SetOverlayIndex;
    property Position: TJObject read GetPosition write SetPosition;
    property Selected: Boolean read GetSelected write SetSelected;
    property StateIndex: Integer read GetStateIndex write SetStateIndex;
    property SubItems: TJBaseStringsObject read GetSubItems;
    property Top: Integer read GetTop write SetTop;
  protected
  end;


  TJVCLBaseListItems = class(TJVCLPersistent)
  private
    FListView: TJVCLBaseListView;

    function DoAdd(Param: TJValueList): TJValue;
    function DoBeginUpdate(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoEndUpdate(Param: TJValueList): TJValue;
    function DoIndexOf(Param: TJValueList): TJValue;
    function DoInsert(Param: TJValueList): TJValue;
    function DoItem(Param: TJValueList): TJValue;

    function GetHandle: Integer;
    function Get_Owner: TJVCLBaseListView;
    procedure SetCount(const Value: Integer);

    function CheckRange(Index: Integer): Boolean;
    function Item(Index: Integer; SetValue: PJValue = nil): TJValue;
  protected
    procedure CheckVCL(Param: TJValueList = nil; ArgCount: Integer = 0); override;
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetListItems: TListItems;
    class function VCLClassType: TClass; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property Count: Integer read GetCount write SetCount;
    property Handle: Integer read GetHandle;
    property Owner: TJVCLBaseListView read Get_Owner;
  end;

  TJVCLBaseListColumns = class(TJVCLPersistent)
  private
    function DoItems(Param: TJValueList): TJValue;
    function DoAdd(Param: TJValueList): TJValue;
  protected
    function CheckRange(Index: Integer): Boolean;
    procedure RegistMethods; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetListColumns: TListColumns;
    class function VCLClassType: TClass; override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property Count: Integer read GetCount;
  end;

  TJVCLBaseListView = class(TJVCLWinControl)
  private
    FListItems: TJVCLBaseListItems;
    FColumns: TJVCLBaseListColumns;

    function DoAlphaSort(Param: TJValueList): TJValue;
    function DoArrange(Param: TJValueList): TJValue;
    function DoFindCaption(Param: TJValueList): TJValue;
    function DoFindData(Param: TJValueList): TJValue;
    function DoGetHitTestInfoAt(Param: TJValueList): TJValue;
    function DoGetItemAt(Param: TJValueList): TJValue;
    function DoGetNearestItem(Param: TJValueList): TJValue;
    function DoGetNextItem(Param: TJValueList): TJValue;
    function DoGetSearchString(Param: TJValueList): TJValue;
    function DoIsEditing(Param: TJValueList): TJValue;
    function DoScroll(Param: TJValueList): TJValue;
//    function DoCustomSort(Param: TJValueList): TJValue;
    function DoStringWidth(Param: TJValueList): TJValue;
    function DoUpdateItems(Param: TJValueList): TJValue;

    function GetBoundingRect: TJRect;
    function GetDropTarget: TJVCLBaseListItem;
    function GetFocused: TJVCLBaseListItem;
    function GetItemIndex: Integer;
    function GetSelCount: Integer;
    function GetSelection: TJVCLBaseListItem;
    function GetTopItem: TJVCLBaseListItem;
    function GetViewOrigin: TJPoint;
    function GetVisibleRowCount: Integer;
    procedure SetDropTarget(const Value: TJVCLBaseListItem);
    procedure SetFocused(const Value: TJVCLBaseListItem);
    procedure SetItemIndex(const Value: Integer);
    procedure SetSelection(const Value: TJVCLBaseListItem);
    function GetColumns: TJVCLBaseListColumns;
    function GetListItems: TJVCLBaseListItems;
  protected
    procedure RegistMethods; override;
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
    procedure Notification(AObject: TJNotify); override;
    function InitializeItem(Item: TListItem): TListItem;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetListView: TListView;
    function GetCustomListView: TCustomListView;
    class function VCLClassType: TClass; override;
    function RegistVCL(AVCL: TPersistent; ACanDestroy: Boolean): Boolean; override;
  published
    property items: TJVCLBaseListItems read GetListItems;
    property column: TJVCLBaseListColumns read GetColumns;
    //property Canvas: TCanvas read FCanvas;
    property DropTarget: TJVCLBaseListItem read GetDropTarget write SetDropTarget;
    property ItemFocused: TJVCLBaseListItem read GetFocused write SetFocused;
    property SelCount: Integer read GetSelCount;
    property Selected: TJVCLBaseListItem read GetSelection write SetSelection;
    property TopItem: TJVCLBaseListItem read GetTopItem;
    property ViewOrigin: TJPoint read GetViewOrigin;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property BoundingRect: TJRect read GetBoundingRect;
    //property WorkAreas: TWorkAreas read FWorkAreas;]
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  protected
    procedure OnAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure OnAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure OnAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OnChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure OnChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
    procedure OnColumnClick(Sender: TObject; Column: TListColumn);
    procedure OnColumnDragged(Sender: TObject);
    procedure OnColumnRightClick(Sender: TObject;
      Column: TListColumn; Point: TPoint);
    procedure OnCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure OnCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure OnCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure OnCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure OnData(Sender: TObject; Item: TListItem);
    procedure OnDataFind(Sender: TObject; Find: TItemFind;
      const FindString: String; const FindPosition: TPoint;
      FindData: Pointer; StartIndex: Integer; Direction: TSearchDirection;
      Wrap: Boolean; var Index: Integer);
    procedure OnDataHint(Sender: TObject; StartIndex,
      EndIndex: Integer);
    procedure OnDataStateChange(Sender: TObject; StartIndex,
      EndIndex: Integer; OldState, NewState: TItemStates);
    procedure OnDeletion(Sender: TObject; Item: TListItem);
    procedure OnDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure OnEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure OnEditing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure OnGetImageIndex(Sender: TObject; Item: TListItem);
    procedure OnGetSubItemImage(Sender: TObject; Item: TListItem;
      SubItem: Integer; var ImageIndex: Integer);
    procedure OnInfoTip(Sender: TObject; Item: TListItem;
      var InfoTip: String);
    procedure OnInsert(Sender: TObject; Item: TListItem);
    procedure OnSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  end;

  TJVCLListView = class(TJVCLBaseListView)
  protected
    procedure CreateVCL; override;
  end;


  //Node生成はTTreeNodes
  //Node破棄はTTreeView.OnDeleting

  TJVCLBaseTreeView = class;
  TJVCLBaseTreeNodes = class;

  TJVCLBaseTreeNode = class(TJVCLPersistent)
  private
    FTreeView: TJVCLBaseTreeView;
    FData: TJObject;

    function DoAlphaSort(Param: TJValueList): TJValue;
    function DoCollapse(Param: TJValueList): TJValue;
    //function DoCustomSort(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoDeleteChildren(Param: TJValueList): TJValue;
    function DoDisplayRect(Param: TJValueList): TJValue;
    function DoEditText(Param: TJValueList): TJValue;
    function DoEndEdit(Param: TJValueList): TJValue;
    function DoExpand(Param: TJValueList): TJValue;
    function DogetFirstChild(Param: TJValueList): TJValue;
    function DoGetLastChild(Param: TJValueList): TJValue;
    function DoGetNext(Param: TJValueList): TJValue;
    function DoGetNextChild(Param: TJValueList): TJValue;
    function DogetNextSibling(Param: TJValueList): TJValue;
    function DoGetNextVisible(Param: TJValueList): TJValue;
    function DoGetPrev(Param: TJValueList): TJValue;
    function DoGetPrevChild(Param: TJValueList): TJValue;
    function DogetPrevSibling(Param: TJValueList): TJValue;
    function DoGetPrevVisible(Param: TJValueList): TJValue;
    function DoHasAsParent(Param: TJValueList): TJValue;
    function DoIndexOf(Param: TJValueList): TJValue;
    function DoMakeVisible(Param: TJValueList): TJValue;
    function DoMoveTo(Param: TJValueList): TJValue;
    function DoItem(Param: TJValueList): TJValue;

    function GetAbsoluteIndex: Integer;
    function GetHasChildren: Boolean;
    function GetCut: Boolean;
    function GetData: TJObject;
    function GetDeleting: Boolean;
    function GetDropTarget: Boolean;
    function GetExpanded: Boolean;
    function GetFocused: Boolean;
    function GetHandle: Integer;
    function GetImageIndex: Integer;
    function GetIndex: Integer;
    function GetIsVisible: Boolean;
    function GetItemId: Integer;
    function GetLevel: Integer;
    function GetOverlayIndex: Integer;
    function Get_Owner: TJVCLBaseTreeNodes;
    function GetParent: TJVCLBaseTreeNode;
    function GetSelected: Boolean;
    function GetSelectedIndex: Integer;
    function GetStateIndex: Integer;
    function GetText: string;
    function GetTreeView: TJVCLBaseTreeView;
    procedure SetHasChildren(const Value: Boolean);
    procedure SetCut(const Value: Boolean);
    procedure SetData(const Value: TJObject);
    procedure SetDropTarget(const Value: Boolean);
    procedure SetExpanded(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure SetImageIndex(const Value: Integer);
    procedure SetOverlayIndex(const Value: Integer);
    procedure SetSelected(const Value: Boolean);
    procedure SetSelectedIndex(const Value: Integer);
    procedure SetStateIndex(const Value: Integer);
    procedure SetText(const Value: string);

    function CheckRange(Index: Integer): Boolean;
    function Item(Index: Integer; SetValue: PJValue = nil): TJValue;
  protected
    procedure RegistMethods; override;
    procedure Notification(AObject: TJNotify); override;
    procedure DestroyVCL; override;
    procedure CheckVCL(Param: TJValueList = nil; ArgCount: Integer = 0); override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetTreeNode: TTreeNode;
    class function VCLClassType: TClass; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property Count: Integer read GetCount;
    property Cut: Boolean read GetCut write SetCut;
    property Data: TJObject read GetData write SetData;
    property Deleting: Boolean read GetDeleting;
    property Focused: Boolean read GetFocused write SetFocused;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Selected: Boolean read GetSelected write SetSelected;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Handle: Integer read GetHandle;
    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
    property ImageIndex: Integer read GetImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
    property IsVisible: Boolean read GetIsVisible;
    property ItemId: Integer read GetItemId;
    property Level: Integer read GetLevel;
    property OverlayIndex: Integer read GetOverlayIndex write SetOverlayIndex;
    property Owner: TJVCLBaseTreeNodes read Get_Owner;
    property Parent: TJVCLBaseTreeNode read GetParent;
    property SelectedIndex: Integer read GetSelectedIndex write SetSelectedIndex;
    property StateIndex: Integer read GetStateIndex write SetStateIndex;
    property Text: string read GetText write SetText;
    property TreeView: TJVCLBaseTreeView read GetTreeView;
  end;

  TJVCLBaseTreeNodes = class(TJVCLPersistent)
  private
    FTreeView: TJVCLBaseTreeView;

    function DoAddChildFirst(Param: TJValueList): TJValue;
    function DoAddChild(Param: TJValueList): TJValue;
    function DoAddChildObjectFirst(Param: TJValueList): TJValue;
    function DoAddChildObject(Param: TJValueList): TJValue;
    function DoAddFirst(Param: TJValueList): TJValue;
    function DoAdd(Param: TJValueList): TJValue;
    function DoAddObjectFirst(Param: TJValueList): TJValue;
    function DoAddObject(Param: TJValueList): TJValue;
    function DoBeginUpdate(Param: TJValueList): TJValue;
    function DoClear(Param: TJValueList): TJValue;
    function DoDelete(Param: TJValueList): TJValue;
    function DoEndUpdate(Param: TJValueList): TJValue;
    function DoGetFirstNode(Param: TJValueList): TJValue;
    function DoGetNode(Param: TJValueList): TJValue;
    function DoInsert(Param: TJValueList): TJValue;
    function DoInsertObject(Param: TJValueList): TJValue;
    function DoItem(Param: TJValueList): TJValue;

    function GetHandle: Integer;
    function Get_Owner: TJVCLBaseTreeView;

    function CheckRange(Index: Integer): Boolean;
    function Item(Index: Integer): TJValue;
    function InitializeNode(Node: TTreeNode): TTreeNode;
  protected
    function DoAssign(Param: TJValueList): TJValue; override;
    procedure RegistMethods; override;
    procedure CheckVCL(Param: TJValueList = nil; ArgCount: Integer = 0); override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetTreeNodes: TTreeNodes;
    class function VCLClassType: TClass; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property Count: Integer read GetCount;
    property Handle: Integer read GetHandle;
    property Owner: TJVCLBaseTreeView read Get_Owner;
  end;

  TJVCLBaseTreeView = class(TJVCLWinControl)
  private
    FTreeNodes: TJVCLBaseTreeNodes;

    function DoAlphaSort(Param: TJValueList): TJValue;
//    function DoCustomSort(Param: TJValueList): TJValue;
    function DoFullCollapse(Param: TJValueList): TJValue;
    function DoFullExpand(Param: TJValueList): TJValue;
    function DoGetHitTestInfoAt(Param: TJValueList): TJValue;
    function DoGetNodeAt(Param: TJValueList): TJValue;
    function DoIsEditing(Param: TJValueList): TJValue;
    function DoLoadFromFile(Param: TJValueList): TJValue;
    function DoSaveToFile(Param: TJValueList): TJValue;

    function GetDropTarget: TJVCLBaseTreeNode;
    function GetSelection: TJVCLBaseTreeNode;
    function GetTopItem: TJVCLBaseTreeNode;
    function GetTreeNodes: TJVCLBaseTreeNodes;
    procedure SetDropTarget(const Value: TJVCLBaseTreeNode);
    procedure SetSelection(const Value: TJVCLBaseTreeNode);
    procedure SetTopItem(const Value: TJVCLBaseTreeNode);
  protected
    procedure RegistEvents; override;
    procedure RegistMethods; override;
    procedure Notification(AObject: TJNotify); override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
    function InitializeNode(Node: TTreeNode): TTreeNode;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetTreeView: TTreeView;
    function GetCustomTreeView: TCustomTreeView;
    class function VCLClassType: TClass; override;
    function RegistVCL(AVCL: TPersistent; ACanDestroy: Boolean): Boolean; override;
  published
    //property Canvas: TCanvas read FCanvas;
    property DropTarget: TJVCLBaseTreeNode read GetDropTarget write SetDropTarget;
    property Selected: TJVCLBaseTreeNode read GetSelection write SetSelection;
    property TopItem: TJVCLBaseTreeNode read GetTopItem write SetTopItem;
    property Items: TJVCLBaseTreeNodes read GetTreeNodes;
  protected
    procedure OnAdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure OnAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure OnChange(Sender: TObject; Node: TTreeNode);
    procedure OnChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure OnCollapsed(Sender: TObject; Node: TTreeNode);
    procedure OnCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure OnCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure OnCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure OnCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure OnDeletion(Sender: TObject; Node: TTreeNode);
    procedure OnEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure OnEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure OnExpanded(Sender: TObject; Node: TTreeNode);
    procedure OnExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure OnGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure OnGetSelectedIndex(Sender: TObject; Node: TTreeNode);
  end;

  TJVCLTreeView = class(TJVCLBaseTreeView)
  protected
    procedure CreateVCL; override;
  end;


  TJVCLBaseStatusBar = class;

  TJVCLBaseStatusPanels = class(TJVCLPersistent)
  private
    function DoItems(Param: TJValueList): TJValue;
    function DoAdd(Param: TJValueList): TJValue;

    function Items(Index: Integer; SetValue: PJValue = nil): TJValue;
  protected
    function CheckRange(Index: Integer): Boolean;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetStatusPanels: TStatusPanels;
    class function VCLClassType: TClass; override;
    function GetValue(S: String; ArrayStyle: Boolean; Param: TJValueList = nil): TJValue; override;
    procedure SetValue(S: String; Value: TJValue; ArrayStyle: Boolean; Param: TJValueList = nil); override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property Count: Integer read GetCount;
  end;

  TJVCLBaseStatusBar = class(TJVCLWinControl)
  private
    FPanels: TJVCLBaseStatusPanels;
    function GetPanels: TJVCLBaseStatusPanels;

  protected
    procedure RegistEvents; override;
    procedure CreateObjects; override;
    procedure DestroyVCL; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
    function GetStatusBar: TStatusBar;
    class function VCLClassType: TClass; override;
  published
    property panels: TJVCLBaseStatusPanels read GetPanels;
    property action: TJVCLBaseAction read Get_Action write Set_Action;
  protected
    procedure OnDrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
  end;

  TJVCLStatusBar = class(TJVCLBaseStatusBar)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseToolButton = class(TJVCLControl)
  private
    function GetIndex: Integer;
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetToolButton: TToolButton;
    class function VCLClassType: TClass; override;
  published
    property Index: Integer read GetIndex;
    property action: TJVCLBaseAction read Get_Action write Set_Action;
  end;

  TJVCLToolButton = class(TJVCLBaseToolButton)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLToolWindow = class(TJVCLWinControl)
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetToolWindow: TToolWindow;
    class function VCLClassType: TClass; override;
  published
  end;

  TJVCLBaseToolBar = class(TJVCLToolWindow)
  private
    function DoTrackMenu(Param: TJValueList): TJValue;
    function DoButtons(Param: TJValueList): TJValue;

    function GetButtonCount: Integer;
    function GetRowCount: Integer;
  protected
    procedure RegistEvents; override;
    function CheckRange(Index: Integer): Boolean;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetToolBar: TToolBar;
    class function VCLClassType: TClass; override;
    class function IsArray: Boolean; override;
    function GetItem(Index: Integer): TJValue; override;
    function GetCount: Integer; override;
  published
    property ButtonCount: Integer read GetButtonCount;
    property RowCount: Integer read GetRowCount;
  protected
    procedure OnAdvancedCustomDraw(Sender: TToolBar;
      const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure OnAdvancedCustomDrawButton(Sender: TToolBar;
      Button: TToolButton; State: TCustomDrawState;
      Stage: TCustomDrawStage; var Flags: TTBCustomDrawFlags;
      var DefaultDraw: Boolean);
    procedure OnCustomDraw(Sender: TToolBar; const ARect: TRect;
      var DefaultDraw: Boolean);
    procedure OnCustomDrawButton(Sender: TToolBar;
      Button: TToolButton; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  end;

  TJVCLToolBar = class(TJVCLBaseToolBar)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseCoolBar = class(TJVCLToolWindow)
  private

  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetCoolBar: TCoolBar;
    class function VCLClassType: TClass; override;
  published
  protected

  end;

  TJVCLCoolBar = class(TJVCLBaseCoolBar)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseUpDown = class(TJVCLToolWindow)
  private
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetUpDown: TUpDown;
    class function VCLClassType: TClass; override;
  published
  protected
    procedure OnChanging(Sender: TObject; var AllowChange: Boolean);
    procedure OnChangingEx(Sender: TObject; var AllowChange: Boolean;
{$if CompilerVersion >= 26.0}
      NewValue: Integer; Direction: TUpDownDirection);
{$else}
      NewValue: Smallint; Direction: TUpDownDirection);
{$endif}
    procedure OnClick(Sender: TObject; Button: TUDBtnType);
  end;

  TJVCLUpDown = class(TJVCLBaseUpDown)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseAction = class(TJVCLComponent)
  private
    function DoExecute(Param: TJValueList): TJValue;
    function DoUpdate(Param: TJValueList): TJValue;
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetBasicAction: TBasicAction;
    class function VCLClassType: TClass; override;
  published
  protected
    procedure OnHint(var HintStr: String; var CanShow: Boolean);
  end;

  TJVCLAction = class(TJVCLBaseAction)
  protected
    procedure RegistEvents; override;
    procedure CreateVCL; override;
  public
    function GetAction: TAction;
    class function VCLClassType: TClass; override;
  end;

  TJVCLBaseCommonDialog = class(TJVCLComponent)
  protected
    function DoExecute(Param: TJValueList): TJValue; virtual;

    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetCommonDialog: TCommonDialog;
    class function VCLClassType: TClass; override;
  published
  end;

  TJVCLBaseOpenDialog = class(TJVCLBaseCommonDialog)
  private
  protected
    function DoExecute(Param: TJValueList): TJValue; override;

    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetOpenDialog: TOpenDialog;
    class function VCLClassType: TClass; override;
  published
  protected
    //イベント
    procedure OnCanClose(Sender: TObject; var CanClose: Boolean);
    procedure OnIncludeItem(const OFN: TOFNotifyEx; var Include: Boolean);
  end;

  TJVCLOpenDialog = class(TJVCLBaseOpenDialog)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseSaveDialog = class(TJVCLBaseOpenDialog)
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetSaveDialog: TSaveDialog;
    class function VCLClassType: TClass; override;
  end;

  TJVCLSaveDialog = class(TJVCLBaseSaveDialog)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseFontDialog = class(TJVCLBaseCommonDialog)
  private
  protected
    function DoExecute(Param: TJValueList): TJValue; override;

    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetFontDialog: TFontDialog;
    class function VCLClassType: TClass; override;
  published
  protected
    procedure OnApply(Sender: TObject; Wnd: HWND);
  end;

  TJVCLFontDialog = class(TJVCLBaseFontDialog)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseFindDialog = class(TJVCLBaseCommonDialog)
  private
    function DoCloseDialog(Param: TJValueList): TJValue;
  protected
    function DoExecute(Param: TJValueList): TJValue; override;

    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetFindDialog: TFindDialog;
    class function VCLClassType: TClass; override;
  end;

  TJVCLFindDialog = class(TJVCLBaseFindDialog)
  protected
    procedure CreateVCL; override;
  end;

  TJVCLBaseReplaceDialog = class(TJVCLBaseFindDialog)
  protected
    procedure RegistEvents; override;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil; RegisteringFactory: Boolean = True); override;
    function GetReplaceDialog: TReplaceDialog;
    class function VCLClassType: TClass; override;
  end;

  TJVCLReplaceDialog = class(TJVCLBaseReplaceDialog)
  protected
    procedure CreateVCL; override;
  end;




function VCLCaster: TJVCLCaster;


procedure RegisterDMS(Engine: TJBaseEngine);

implementation

uses
  ecma_engine;

procedure RegisterDMS(Engine: TJBaseEngine);
begin
  Engine.ImportObject('VCLForm',TJVCLForm);
  Engine.ImportObject('VCLEdit',TJVCLEdit);
  Engine.ImportObject('VCLButton',TJVCLButton);
  Engine.ImportObject('VCLMemo',TJVCLMemo);
  Engine.ImportObject('VCLLabel',TJVCLLabel);
  Engine.ImportObject('VCLTimer',TJVCLTimer);
  Engine.ImportObject('VCLMenuItem',TJVCLMenuItem);
  Engine.ImportObject('VCLMainMenu',TJVCLMainMenu);
  Engine.ImportObject('VCLPopupMenu',TJVCLPopupMenu);
  Engine.ImportObject('VCLCheckBox',TJVCLCheckBox);
  Engine.ImportObject('VCLRadioButton',TJVCLRadioButton);
  Engine.ImportObject('VCLListBox',TJVCLListBox);
  Engine.ImportObject('VCLComboBox',TJVCLComboBox);
  Engine.ImportObject('VCLGroupBox',TJVCLGroupBox);
  Engine.ImportObject('VCLRadioGroup',TJVCLRadioGroup);
  Engine.ImportObject('VCLPanel',TJVCLPanel);
  Engine.ImportObject('VCLSplitter',TJVCLSplitter);
  Engine.ImportObject('VCLCheckListBox',TJVCLCheckListBox);
  Engine.ImportObject('VCLImage',TJVCLImage);
  Engine.ImportObject('VCLTabControl',TJVCLTabControl);
  Engine.ImportObject('VCLTabSheet',TJVCLTabSheet);
  Engine.ImportObject('VCLPageControl',TJVCLPageControl);
  Engine.ImportObject('VCLProgressBar',TJVCLProgressBar);
  Engine.ImportObject('VCLTreeView',TJVCLTreeView);
  Engine.ImportObject('VCLListView',TJVCLListView);
  Engine.ImportObject('VCLStatusBar',TJVCLStatusBar);
  Engine.ImportObject('VCLToolButton',TJVCLToolButton);
  Engine.ImportObject('VCLToolBar',TJVCLToolBar);
  Engine.ImportObject('VCLCoolBar',TJVCLCoolBar);
  Engine.ImportObject('VCLAction',TJVCLAction);
  Engine.ImportObject('VCLOpenDialog',TJVCLOpenDialog);
  Engine.ImportObject('VCLSaveDialog',TJVCLSaveDialog);
  Engine.ImportObject('VCLFontDialog',TJVCLFontDialog);
  Engine.ImportObject('VCLFindDialog',TJVCLFindDialog);
  Engine.ImportObject('VCLReplaceDialog',TJVCLReplaceDialog);
  Engine.ImportObject('VCLSpinEdit',TJVCLSpinEdit);
  Engine.ImportObject('VCLUpDown',TJVCLUpDown);
end;

procedure RegisterVCLClasses;
//シリアライズ用に登録
begin
  RegisterClasses([
    TForm,
    TEdit,
    TButton,
    TMemo,
    TLabel,
    TTimer,
    TMenuItem,
    TMainMenu,
    TPopupMenu,
    TCheckBox,
    TRadioButton,
    TListBox,
    TComboBox,
    TGroupBox,
    TRadioGroup,
    TPanel,
    TSplitter,
    TCheckListBox,
    TImage,
    TTabControl,
    TTabSheet,
    TPageControl,
    TProgressBar,
    TTreeView,
    TListView,
    TStatusBar,
    TToolButton,
    TToolBar,
    TCoolBar,
    TAction,
    TActionList,
    TOpenDialog,
    TSaveDialog,
    TFontDialog,
    TFindDialog,
    TReplaceDialog,
    TSpinEdit,
    TUpDown
    ]);
end;

function BaseCast(VCL: TPersistent; Engine: TJBaseEngine): TJVCLPersistent;
//型変換する
//別のunitでVCLを加えた場合にはVCLCasterにcast関数を加える
begin
  Result := nil;
  //順番に注意
  if VCL is TMenuItem then
    Result := TJVCLBaseMenuItem.Create(Engine)

  else if VCL is TBasicAction then
    Result := TJVCLBaseAction.Create(Engine)

  else if VCL is TLabel then
    Result := TJVCLBaseLabel.Create(Engine)

  else if VCL is TEdit then
    Result := TJVCLBaseEdit.Create(Engine)

  else if VCL is TMemo then
    Result := TJVCLBaseMemo.Create(Engine)

  else if VCL is TToolButton then
    Result := TJVCLBaseToolButton.Create(Engine)

  else if VCL is TCheckBox then
    Result := TJVCLBaseCheckBox.Create(Engine)

  else if VCL is TRadioButton then
    Result := TJVCLBaseRadioButton.Create(Engine)

  else if VCL is TButton then
    Result := TJVCLBaseButton.Create(Engine)

  else if VCL is TComboBox then
    Result := TJVCLBaseComboBox.Create(Engine)

  else if VCL is TTimer then
    Result := TJVCLBaseTimer.Create(Engine)

  else if VCL is TSpinEdit then
    Result := TJVCLBaseSpinEdit.Create(Engine)

  else if VCL is TCheckListBox then
    Result := TJVCLBaseCheckListBox.Create(Engine)

  else if VCL is TListBox then
    Result := TJVCLBaseListBox.Create(Engine)

  else if VCL is TPanel then
    Result := TJVCLBasePanel.Create(Engine)

  else if VCL is TRadioGroup then
    Result := TJVCLBaseRadioGroup.Create(Engine)

  else if VCL is TGroupBox then
    Result := TJVCLBaseGroupBox.Create(Engine)

  else if VCL is TCoolBar then
    Result := TJVCLBaseCoolBar.Create(Engine)

  else if VCL is TToolBar then
    Result := TJVCLBaseToolBar.Create(Engine)

  else if VCL is TPopupMenu then
    Result := TJVCLBasePopupMenu.Create(Engine)

  else if VCL is TMainMenu then
    Result := TJVCLBaseMainMenu.Create(Engine)

  else if VCL is TForm then
    Result := TJVCLBaseForm.Create(Engine)

  else if VCL is TSplitter then
    Result := TJVCLBaseSplitter.Create(Engine)

  else if VCL is TProgressBar then
    Result := TJVCLBaseProgressBar.Create(Engine)

  else if VCL is TListItem then
    Result := TJVCLBaseListItem.Create(Engine)

  else if VCL is TListItems then
    Result := TJVCLBaseListItems.Create(Engine)

  else if VCL is TListColumns then
    Result := TJVCLBaseListColumns.Create(Engine)

  else if VCL is TListView then
    Result := TJVCLBaseListView.Create(Engine)

  else if VCL is TTreeNode then
    Result := TJVCLBaseTreeNode.Create(Engine)

  else if VCL is TTreeNodes then
    Result := TJVCLBaseTreeNodes.Create(Engine)

  else if VCL is TTreeView then
    Result := TJVCLBaseTreeView.Create(Engine)

  else if VCL is TStatusBar then
    Result := TJVCLBaseStatusBar.Create(Engine)

  else if VCL is TImage then
    Result := TJVCLBaseImage.Create(Engine)

  else if VCL is TPageControl then
    Result := TJVCLBasePageControl.Create(Engine)

  else if VCL is TTabSheet then
    Result := TJVCLBaseTabSheet.Create(Engine)

  else if VCL is TTabControl then
    Result := TJVCLBaseTabControl.Create(Engine)

  else if VCL is TStatusPanels then
    Result := TJVCLBaseStatusPanels.Create(Engine)

  else if VCL is TUpDown then
    Result := TJVCLBaseUpDown.Create(Engine)

  else if VCL is TSaveDialog then
    Result := TJVCLBaseSaveDialog.Create(Engine)

  else if VCL is TOpenDialog then
    Result := TJVCLBaseOpenDialog.Create(Engine)

  else if VCL is TFontDialog then
    Result := TJVCLBaseFontDialog.Create(Engine)

  else if VCL is TFindDialog then
    Result := TJVCLBaseFindDialog.Create(Engine)

  else if VCL is TReplaceDialog then
    Result := TJVCLBaseReplaceDialog.Create(Engine)

  else if VCL is TWinControl then
    Result := TJVCLWinControl.Create(Engine)

  else if VCL is TControl then
    Result := TJVCLControl.Create(Engine)

  else if VCL is TComponent then
    Result := TJVCLComponent.Create(Engine)

  else if VCL is TPersistent then
    Result := TJVCLPersistent.Create(Engine);
end;

{ TODO : VCLはメインスレッドでしか使えないので、スレッドセーフにする必要なし }
var
  __VCLCaster__: TJVCLCaster;

function VCLCaster: TJVCLCaster;
begin
  if not Assigned(__VCLCaster__) then
    __VCLCaster__ := TJVCLCaster.Create;

  Result := __VCLCaster__;
end;

{ TJVCLCaster }

procedure TJVCLCaster.Add(Func: TJVCLCastFunc);
//cast関数を加える
begin
  SetLength(FItems,Length(FItems) + 1);
  FItems[High(FItems)] := Func;
end;

function TJVCLCaster.Cast(VCL: TPersistent;
  Engine: TJBaseEngine): TJVCLPersistent;
//型変換
var
  i: Integer;
begin
  Result := nil;
  //終わり
  if not Assigned(VCL) then
    Exit;

  //tagをチェックする
  if (VCL is TComponent) and (TComponent(VCL).Tag <> 0) then
  begin
    if TObject(TComponent(VCL).Tag) is TJVCLPersistent then
    begin
      Result := TJVCLPersistent(TComponent(VCL).Tag);
      Exit;
    end;
  end;

  //逆順に検索
  for i := Length(FItems) - 1 downto 0 do
  begin
    Result := FItems[i](VCL,Engine);
    if Assigned(Result) then
      Break;
  end;

  //登録
  if Assigned(Result) then
    Result.RegistVCL(VCL,False);
end;

constructor TJVCLCaster.Create;
begin
  inherited;
  Add(BaseCast);
end;

{ TJVCLNotify }

procedure TJVCLNotify.Notification(AComponent: TComponent;
  Operation: TOperation);
//所有VCLオブジェクトの破棄
begin
  inherited;
  if (Operation = opRemove) and Assigned(AComponent) then
  begin
    //全てのイベントを削除
    SetDefaultMethodNil(AComponent);
    FVCLObject.FVCL := nil;
    //tagを消す
    AComponent.Tag := 0;
  end;
end;

{ TJVCLComponent }

constructor TJVCLComponent.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLComponent');
  RegistMethod('components',DoComponents);
  //member object作成はCreateObjectsで
end;

procedure TJVCLComponent.CreateObjects;
begin
  inherited;
  FNotify := TJVCLNotify.Create(nil);
  FNotify.FVCLObject := Self;
  FComponents := TJHash.Create(HASH_20);
end;

destructor TJVCLComponent.Destroy;
begin
  //先に開放する
  DestroyVCL;
  FreeAndNil(FComponents);
  FreeAndNil(FNotify);
  inherited;
end;

procedure TJVCLComponent.GetPropertyList(List: TStringList);
//property list
begin
  List.BeginUpdate;
  try
    List.Clear;
    List.AddStrings(FDefaultProperties);
    //子コンポーネント
    List.AddStrings(FComponents.KeyList);
  finally
    List.EndUpdate;
  end;
end;

function TJVCLComponent.GetValueImpl(S: String;
  var RetVal: TJValue; Param: TJValueList = nil): Boolean;
//値を得る
begin
  //vcl persistentで探す
  Result := inherited GetValueImpl(S,RetVal,Param);
  //componentから探す
  if (not Result) and FComponents.GetValue(S,RetVal) then
    Result := True;
end;

procedure TJVCLComponent.DestroyVCL;
//VCLをクリア
begin
  if not Assigned(FVCL) then
    Exit;

  //componentsを先にクリア
  FComponents.Clear;

  //開放する
  if FCanDestroy then
    FreeAndNil(FVCL) //FreeするとFNotify.notifucationが呼ばれてイベント消去
  else begin
    //以前のイベントをすべて消す
    SetDefaultMethodNil(FVCL);
    //終了通知を消す
    GetComponent.RemoveFreeNotification(FNotify);
    FNotify.RemoveFreeNotification(GetComponent);
    //tagを消す
    GetComponent.Tag := 0;
  end;

  FVCL := nil;
end;

function TJVCLComponent.RegistVCL(AVCL: TPersistent;
  ACanDestroy: Boolean): Boolean;
//VCLを登録
begin
  Result := inherited RegistVCL(AVCL,ACanDestroy);
  //componentの場合
  if Result then
  begin
    //終了通知をセット
    GetComponent.FreeNotification(FNotify);
    //tagを入れる
    FTag := GetComponent.Tag;
    //コンポのtagに自分のポインタを入れる
    GetComponent.Tag := Integer(Self);
    //子コンポーネント登録
    RegistComponents;
  end;
end;

class function TJVCLComponent.VCLClassType: TClass;
begin
  Result := TComponent;
end;

procedure TJVCLComponent.RegistComponents;
//子コンポーネントを登録する
var
  i: Integer;
  per,chi: TComponent;
  obj: TJVCLComponent;
begin
  if not Assigned(FVCL) then
    Exit;

  per := GetComponent;
  for i := 0 to per.ComponentCount - 1 do
  begin
    chi := per.Components[i];
    //名前があればhashにセット
    if chi.Name <> '' then
    begin
      //型変換
      obj := VCLCaster.Cast(chi,FEngine) as TJVCLComponent;
      FComponents.SetValue(chi.Name,BuildObject(obj));
    end;
  end;
end;

procedure TJVCLComponent.OnChange(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onChange') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnClick(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onClick') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onClick',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnDblClick(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onDblClick') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onDblClick',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnResize(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onResize') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onResize',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnPopup(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onPopup') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onPopup',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnActivate(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onActivate') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onActivate',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnCreate(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onCreate') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onCreate',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnDeactivate(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onDeactivate') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onDeactivate',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnDestroy(Sender: TObject);
var
  param: TJValueList;
begin
  FVCL := nil;
  if not IsCallEvent('onDestroy') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onDestroy',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnEnter(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onEnter') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onEnter',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnExit(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onExit') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onExit',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnHide(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onHide') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onHide',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnPaint(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onPaint') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onPaint',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnShow(Sender: TObject);
var
  param: TJValueList;
begin
  if not IsCallEvent('onShow') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onShow',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnTimer(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onTimer') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onTimer',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnDropDown(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onDropDown') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onDropDown',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnMoved(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onMoved') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onMoved',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnClickCheck(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onClickCheck') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onClickCheck',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnHint(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onHint') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onHint',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnExecute(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onExecute') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onExecute',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnUpdate(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onUpdate') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onUpdate',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnClose(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onClose') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onClose',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnFolderChange(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onFolderChange') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onFolderChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnSelectionChange(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onSelectionChange') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onSelectionChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnTypeChange(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onTypeChange') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onTypeChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnFind(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onFind') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onFind',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLComponent.OnReplace(Sender: TObject);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onReplace') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onReplace',param);
  finally
    param.Free;
  end;
end;

function TJVCLComponent.DoComponents(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  comp: TComponent;
begin
  CheckVCL(Param,1);
  Result := BuildNull;
  v := Param[0];
  try
    comp := GetComponent.Components[AsInteger(@v)];
    if FComponents.GetValue(comp.Name,Result) then
    else
      Result := BuildObject(VCLCaster.Cast(comp,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLComponent.GetComponentCount: Integer;
begin
  if IsVCL then
    Result := GetComponent.ComponentCount
  else
    Result := 0;
end;

function TJVCLComponent.GetComponentIndex: Integer;
begin
  if IsVCL then
    Result := GetComponent.ComponentIndex
  else
    Result := -1;
end;

procedure TJVCLComponent.SetComponentIndex(const Value: Integer);
begin
  CheckVCL;
  GetComponent.ComponentIndex := Value;
end;

function TJVCLComponent.GetComponent: TComponent;
begin
  Result := FVCL as TComponent;
end;

function TJVCLComponent.GetVCLOwner: TJVCLComponent;
begin
  if IsVCL then
    Result := VCLCaster.Cast(
      GetComponent.Owner,FEngine) as TJVCLComponent
  else
    Result := nil;
end;

function TJVCLComponent.Get_Action: TJVCLBaseAction;
//actionを得る
var
  v: TJValue;
begin
  CheckVCL;
  Result := nil;
  if GetDefaultProperty(FVCL,'Action',v,FEngine) and IsVCLObject(@v) and
{ TODO : TJVCLBaseActionが返ってくる }
     (v.vObject is TJVCLBaseAction) then
  begin
    Result := v.vObject as TJVCLBaseAction;
  end;
end;

procedure TJVCLComponent.Set_Action(const Value: TJVCLBaseAction);
//actionをセットする
var
  v: TJValue;
  nt: TNotifyEvent;
begin
  CheckVCL;
  if Assigned(Value) then
  begin
    //onclickを消す
    v := BuildNull;
    if SetDefaultProperty(FVCL,'OnClick',v) then
    begin
      //actionをセットする
      v := BuildObject(Value);
      SetDefaultProperty(FVCL,'Action',v);
    end;
  end
  else begin
    //actionを消す
    v := BuildNull;
    if SetDefaultProperty(FVCL,'Action',v) then
    begin
      //onclickをセット
      nt := OnClick;
      v := BuildEvent(TMethod(nt));
      SetDefaultProperty(FVCL,'OnClick',v,TypeInfo(TNotifyEvent));
    end;
  end;
end;

function TJVCLComponent.GetSender(Sender: TObject): TJVCLComponent;
//イベントのsenderを得る
begin
  Result := nil;
  if Sender is TComponent then
    Result := VCLCaster.Cast(
      Sender as TPersistent,
      FEngine) as TJVCLComponent;

  //見つからない場合は自分
  if (not Assigned(Result)) and IsVCL then
    Result := Self;
end;

function TJVCLComponent.HasKey(S: String): Boolean;
begin
  Result := inherited HasKey(S);
  if not Result then
    Result := FComponents.HasKey(S);
end;

procedure TJVCLComponent.GetEventList(List: TStrings);
var
  re: TJRegExp;
  i: Integer;
begin
  inherited GetEventList(List);

  if IsVCL and (List.Count = 0) then
  begin
    //イベント名登録
    GetDefaultProperties(FVCL,List,True);
    if List.Count > 0 then
    begin
      //Onをonに変える
      re := TJRegExp.Create;
      try
        re.global := True;
        re.multiline := True;
        re.ignoreCase := False;
        re.source := '^On';
        List.Text := re.Replace(List.Text,'on');
      finally
        re.Free;
      end;
      //event登録
      for i := 0 to List.Count - 1 do
        RegistEventName(List[i]);
    end;
  end;

end;

{ TJVCLControl }

constructor TJVCLControl.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('VCLControl');
  RegistMethod('bringToFront',DoBringToFront);
  RegistMethod('clientToScreen',DoClientToScreen);
  RegistMethod('hasParent',DoHasParent);
  RegistMethod('hide',DoHide);
  RegistMethod('invalidate',DoInvalidate);
  RegistMethod('perform',DoPerform);
  RegistMethod('refresh',DoRefresh);
  RegistMethod('repaint',DoRepaint);
  RegistMethod('screenToClient',DoScreenToClient);
  RegistMethod('sendToBack',DoSendToBack);
  RegistMethod('show',DoShow);
  RegistMethod('update',DoUpdate);
  RegistMethod('setBounds',DoSetBounds);

  //親をセット
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsObject(@v) and (v.vObject is TJVCLWinControl) then
      SetParent(v.vObject as TJVCLWinControl);
  end;
end;

function TJVCLControl.DoBringToFront(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.BringToFront;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoClientToScreen(Param: TJValueList): TJValue;
var
  v,tmp: TJValue;
  pt: TPoint;
  ss: TJPoint;
begin
  CheckVCL(Param,1);

  ss := TJPoint.Create(FEngine);
  Result := BuildObject(ss);

  v := Param[0];
  if IsObject(@v) then
  begin
    tmp := v.vObject.GetValue('x',True);
    pt.x := AsInteger(@tmp);
    tmp := v.vObject.GetValue('y',True);
    pt.y := AsInteger(@tmp);
  end
  else
    ArgsError;

  try
    ss.__Point := GetControl.ClientToScreen(pt);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoHasParent(Param: TJValueList): TJValue;
begin
  CheckVCL;
  try
    Result := BuildBool(GetControl.HasParent);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoHide(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.Hide;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoInvalidate(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.Invalidate;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoPerform(Param: TJValueList): TJValue;
var
  m: Cardinal;
  w,l:  Integer;
  v: TJValue;
begin
  CheckVCL(Param,3);

  v := Param[0];
  m := Cardinal(AsInteger(@v));
  v := Param[1];
  w := AsInteger(@v);
  v := Param[2];
  l := AsInteger(@v);

  try
    Result := BuildInteger(GetControl.Perform(m,w,l));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoRefresh(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.Refresh;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoRepaint(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.Repaint;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoScreenToClient(Param: TJValueList): TJValue;
var
  v,tmp: TJValue;
  pt: TPoint;
  ss: TJPoint;
begin
  CheckVCL(Param,1);
  ss := TJPoint.Create(FEngine);
  Result := BuildObject(ss);

  v := Param[0];
  if IsObject(@v) then
  begin
    tmp := v.vObject.GetValue('x',True);
    pt.x := AsInteger(@tmp);
    tmp := v.vObject.GetValue('y',True);
    pt.y := AsInteger(@tmp);
  end
  else
    ArgsError;

  try
    ss.__Point := GetControl.ScreenToClient(pt);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoSendToBack(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.SendToBack;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoSetBounds(Param: TJValueList): TJValue;
var
  v: TJValue;
  l,t,w,h: Integer;
begin
  CheckVCL(Param,4);
  Result := BuildObject(Self);

  v := Param[0];
  l := AsInteger(@v);
  v := Param[1];
  t := AsInteger(@v);
  v := Param[2];
  w := AsInteger(@v);
  v := Param[3];
  h := AsInteger(@v);

  try
    GetControl.SetBounds(l,t,w,h);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoShow(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.Show;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.DoUpdate(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetControl.Update;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLControl.GetControl: TControl;
//WinControlを得る
begin
  Result := FVCL as TControl;
end;

function TJVCLControl.GetParent: TJVCLWinControl;
//parentを返す
begin
  if IsVCL and (GetControl.Parent is TWinControl) then
    Result := VCLCaster.Cast(GetControl.Parent,FEngine) as TJVCLWinControl
  else
    Result := nil;
end;

procedure TJVCLControl.OnCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
//イベント
var
  nw,nh: TJNumberObject;
  rs: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onCanResize') then
    Exit;

  nw := TJNumberObject.Create(FEngine);
  nw.int := NewWidth;

  nh := TJNumberObject.Create(FEngine);
  nh.int := NewHeight;

  rs := TJBooleanObject.Create(FEngine);
  rs.bool := Resize;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(nw);
    param.Add(nh);
    param.Add(rs);
    CallEvent('','onCanResize',param);
  finally
    NewWidth := nw.int;
    NewHeight := nh.int;
    Resize := rs.bool;
    param.Free;
  end;
end;

procedure TJVCLControl.OnConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
//イベント
var
  minw,minh,maxw,maxh: TJNumberObject;
  param: TJValueList;
begin
  if not IsCallEvent('onConstrainedResize') then
    Exit;

  minw := TJNumberObject.Create(FEngine);
  minw.int := MinWidth;

  minh := TJNumberObject.Create(FEngine);
  minh.int := MinHeight;

  maxw := TJNumberObject.Create(FEngine);
  maxw.int := MaxWidth;

  maxh := TJNumberObject.Create(FEngine);
  maxh.int := MaxHeight;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(minw);
    param.Add(minh);
    param.Add(maxh);
    param.Add(maxw);
    CallEvent('','ConstrainedResize',param);
  finally
    MinWidth := minw.int;
    MinHeight := minh.int;
    MaxWidth := maxw.int;
    MaxHeight := maxh.int;

    param.Free;
  end;
end;

procedure TJVCLControl.OnContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
//イベント
var
  pt: TJPoint;
  hand: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onContextPopup') then
    Exit;

  pt := TJPoint.Create(FEngine);
  pt.__Point := MousePos;

  hand := TJBooleanObject.Create(FEngine);
  hand.bool := Handled;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(pt);
    param.Add(hand);
    CallEvent('','onContextPopup',param);
  finally
    Handled := hand.bool;
    param.Free;
  end;
end;

procedure TJVCLControl.OnDragDrop(Sender, Source: TObject; X, Y: Integer);
//VCLイベント
var
  s: TJVCLPersistent;
  param: TJValueList;
begin
  if not IsCallEvent('onDragDrop') then
    Exit;

  if Source is TPersistent then
    s := VCLCaster.Cast(Source as TPersistent,FEngine)
  else
    s := nil;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(s);
    param.Add(X);
    param.Add(Y);
    CallEvent('','onDragDrop',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLControl.OnDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
//VCLイベント
var
  st: TJValue;
  so: TJVCLPersistent;
  ap: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onDragOver') then
    Exit;

  if Source is TPersistent then
    so := VCLCaster.Cast(Source as TPersistent,FEngine)
  else
    so := nil;

  EnumToValue(TypeInfo(TDragState),st,Ord(State));

  ap := TJBooleanObject.Create(FEngine);
  ap.bool := Accept;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(so);
    param.Add(X);
    param.Add(Y);
    param.Add(st);
    param.Add(ap);
    CallEvent('','onDragOver',param);
  finally
    Accept := ap.Bool;
    param.Free;
  end;
end;

procedure TJVCLControl.OnEndDock(Sender, Target: TObject; X, Y: Integer);
//VCLイベント
var
  t: TJVCLPersistent;
  param: TJValueList;
begin
  if not IsCallEvent('onEndDock') then
    Exit;

  if Target is TPersistent then
    t := VCLCaster.Cast(Target as TPersistent,FEngine)
  else
    t := nil;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(t);
    param.Add(X);
    param.Add(Y);
    CallEvent('','onEndDock',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLControl.OnEndDrag(Sender, Target: TObject; X, Y: Integer);
//VCLイベント
var
  t: TJVCLPersistent;
  param: TJValueList;
begin
  if not IsCallEvent('onEndDrag') then
    Exit;

  if Target is TPersistent then
    t := VCLCaster.Cast(Target as TPersistent,FEngine)
  else
    t := nil;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(t);
    param.Add(X);
    param.Add(Y);
    CallEvent('','onEndDrag',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLControl.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//イベント
var
  btn: TJValue;
  param: TJValueList;
  sh: String;
begin
  if not IsCallEvent('onMouseDown') then
    Exit;

  EnumToValue(TypeInfo(TMouseButton),btn,Ord(Button));
  sh := SetToStr(TypeInfo(TShiftState),Shift);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(btn);
    param.Add(sh);
    param.Add(X);
    param.Add(Y);
    CallEvent('','onMouseDown',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLControl.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//イベント
var
  ss: String;
  param: TJValueList;
begin
  if not IsCallEvent('onMouseMove') then
    Exit;

  ss := SetToStr(TypeInfo(TShiftState),Shift);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ss);
    param.Add(X);
    param.Add(Y);
    CallEvent('','onMouseMove',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLControl.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
//イベント
var
  ss: String;
  btn: TJValue;
  param: TJValueList;
begin
  if not IsCallEvent('onMouseUp') then
    Exit;

  EnumToValue(TypeInfo(TMouseButton),btn,Ord(Button));

  ss := SetToStr(TypeInfo(TShiftState),Shift);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(btn);
    param.Add(ss);
    param.Add(X);
    param.Add(Y);
    CallEvent('','onMouseUp',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLControl.OnStartDock(Sender: TObject;
  var DragObject: TDragDockObject);
begin
{ TODO : 保留 }
end;

procedure TJVCLControl.OnStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
{ TODO : 保留 }
end;

procedure TJVCLControl.SetParent(const Value: TJVCLWinControl);
//parentをセット
begin
  CheckVCL;
  //入れ替え
  if Assigned(Value) then
    GetControl.Parent := Value.GetWinControl
  else
    GetControl.Parent := nil;
end;

class function TJVCLControl.VCLClassType: TClass;
begin
  Result := TControl;
end;

{ TJVCLWinControl }

constructor TJVCLWinControl.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLWinControl');

  RegistMethod('canFocus',DoCanFocus);
  RegistMethod('containsControl',DoContainsControl);
  RegistMethod('controlAtPos',DoControlAtPos);
  RegistMethod('disableAlign',DoDisableAlign);
  RegistMethod('enableAlign',DoEnableAlign);
  RegistMethod('findChildControl',DoFindChildControl);
  RegistMethod('flipChildren',DoFlipChildren);
  RegistMethod('focused',DoFocused);
  RegistMethod('handleAllocated',DoHandleAllocated);
  RegistMethod('handleNeeded',DoHandleNeeded);
  RegistMethod('realign',DoRealign);
  RegistMethod('scaleBy',DoScaleBy);
  RegistMethod('scrollBy',DoScrollBy);
  RegistMethod('setFocus',DoSetFocus);
  RegistMethod('controls',DoControls);
end;

destructor TJVCLWinControl.Destroy;
begin
  FreeAndNil(FDragDropTarget);
  inherited;
end;

function TJVCLWinControl.DoCanFocus(Param: TJValueList): TJValue;
begin
  CheckVCL;
  try
    Result := BuildBool(GetWinControl.CanFocus);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoContainsControl(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildBool(False);

  v := Param[0];
  if IsObject(@v) and (v.vObject is TJVCLControl) then
  begin
    try
      Result := BuildBool(
        GetWinControl.ContainsControl((v.vObject as TJVCLControl).GetControl));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end;
end;

function TJVCLWinControl.DoControlAtPos(Param: TJValueList): TJValue;
var
  v,tmp: TJValue;
  pt: TPoint;
  dis,win: Boolean;
  ctrl: TControl;
begin
  CheckVCL(Param,2);

  v := Param[0];
  if IsObject(@v) then
  begin
    tmp := v.vObject.GetValue('x',True);
    pt.x := AsInteger(@tmp);
    tmp := v.vObject.GetValue('y',True);
    pt.y := AsInteger(@tmp);
  end
  else
    ArgsError;

  v := Param[1];
  dis := AsBool(@v);

  if IsParam3(Param) then
  begin
    v := Param[2];
    win := AsBool(@v);
  end
  else
    win := False;

  //実行
  try
    ctrl := GetWinControl.ControlAtPos(pt,dis,win);
    Result := BuildObject(VCLCaster.Cast(ctrl,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoControls(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    Result := BuildObject(
      VCLCaster.Cast(GetWinControl.Controls[AsInteger(@v)],FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoDisableAlign(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetWinControl.DisableAlign;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoEnableAlign(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetWinControl.EnableAlign;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoFindChildControl(Param: TJValueList): TJValue;
var
  v: TJValue;
  ctrl: TControl;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    ctrl := GetWinControl.FindChildControl(AsString(@v));
    Result := BuildObject(VCLCaster.Cast(ctrl,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoFlipChildren(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetWinControl.FlipChildren(AsBool(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoFocused(Param: TJValueList): TJValue;
begin
  CheckVCL;
  try
    Result := BuildBool(GetWinControl.Focused);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoHandleAllocated(Param: TJValueList): TJValue;
begin
  CheckVCL;
  try
    Result := BuildBool(GetWinControl.HandleAllocated);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoHandleNeeded(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetWinControl.HandleNeeded;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoRealign(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetWinControl.Realign;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoScaleBy(Param: TJValueList): TJValue;
var
  m,d: TJValue;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  m := Param[0];
  d := Param[1];
  try
    GetWinControl.ScaleBy(AsInteger(@m),AsInteger(@d));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoScrollBy(Param: TJValueList): TJValue;
var
  x,y: TJValue;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  x := Param[0];
  y := Param[1];
  try
    GetWinControl.ScaleBy(AsInteger(@x),AsInteger(@y));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.DoSetFocus(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetWinControl.SetFocus;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLWinControl.GetControlCount: Integer;
begin
  if IsVCL then
    Result := GetWinControl.ControlCount
  else
    Result := 0;
end;

function TJVCLWinControl.GetacceptDrop: Boolean;
begin
  CheckVCL;
  Result := Assigned(FDragDropTarget) and Assigned(FDragDropTarget.Target);
end;

procedure TJVCLWinControl.GetEventList(List: TStrings);
begin
  inherited;
  List.Add('onDragDropText');
  List.Add('onDragDropFiles');
end;

function TJVCLWinControl.GetHandle: Integer;
//ハンドルを得る
begin
  if IsVCL then
    Result := GetWinControl.Handle
  else
    Result := 0;
end;

function TJVCLWinControl.GetParentWindow: Integer;
begin
  if IsVCL then
    Result := GetWinControl.ParentWindow
  else
    Result := 0;
end;

function TJVCLWinControl.GetShowing: Boolean;
begin
  if IsVCL then
    Result := GetWinControl.Showing
  else
    Result := False;
end;

function TJVCLWinControl.GetVisibleDockClientCount: Integer;
begin
  if IsVCL then
    Result := GetWinControl.VisibleDockClientCount
  else
    Result := 0;
end;

function TJVCLWinControl.GetWinControl: TWinControl;
//WinControlを得る
begin
  Result := FVCL as TWinControl;
end;

procedure TJVCLWinControl.OnDockDrop(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer);
begin
{ TODO : 保留 }
end;

procedure TJVCLWinControl.OnDockOver(Sender: TObject;
  Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLWinControl.OnDragDropFiles(Sender: TWinControl; X,Y: Integer;
  Files: TStrings);
//イベント
var
  param: TJValueList;
  s: TJStringsObject;
begin
  if not IsCallEvent('onDragDropFiles') then
    Exit;

  s := TJStringsObject.Create(FEngine);
  s.Strings.Assign(Files);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(X);
    param.Add(Y);
    param.Add(s);
    CallEvent('','onDragDropFiles',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnDragDropText(Sender: TWinControl; X,Y: Integer;
  Text: String);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onDragDropText') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(X);
    param.Add(Y);
    param.Add(Text);
    CallEvent('','onDragDropText',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
//VCLイベント
var
  dc: TJVCLPersistent;
  ir: TJRect;
  mp: TJPoint;
  cd: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onGetSiteInfo') then
    Exit;

  dc := VCLCaster.Cast(DockClient,FEngine);
  ir := TJRect.Create(FEngine);
  ir.__Rect := InfluenceRect;
  mp := TJPoint.Create(FEngine);
  mp.__Point := MousePos;
  cd := TJBooleanObject.Create(FEngine);
  cd.bool := CanDock;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(dc);
    param.Add(ir);
    param.Add(mp);
    param.Add(cd);
    CallEvent('','onGetSiteInfo',param);
  finally
    InfluenceRect := ir.__Rect;
    CanDock := cd.bool;
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//イベント
var
  ss: String;
  k: TJNumberObject;
  param: TJValueList;
begin
  if not IsCallEvent('onKeyDown') then
    Exit;

  k := TJNumberObject.Create(FEngine);
  k.int := Key;

  ss := SetToStr(TypeInfo(TShiftState),Shift);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(k);
    param.Add(ss);
    CallEvent('','onKeyDown',param);
  finally
    Key := k.int;
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnKeyPress(Sender: TObject; var Key: Char);
//イベント
var
  k: TJNumberObject;
  param: TJValueList;
begin
  if not IsCallEvent('onKeyPress') then
    Exit;

  k := TJNumberObject.Create(FEngine);
  k.int := Byte(Key);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(k);
    CallEvent('','onKeyPress',param);
  finally
    Key := Char(k.int);
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
//イベント
var
  ss: String;
  k: TJNumberObject;
  param: TJValueList;
begin
  if not IsCallEvent('onKeyUp') then
    Exit;

  k := TJNumberObject.Create(FEngine);
  k.int := Key;

  ss := SetToStr(TypeInfo(TShiftState),Shift);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(k);
    param.Add(ss);
    CallEvent('','onKeyUp',param);
  finally
    Key := k.int;
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
//イベント
var
  ss: String;
  hd: TJBooleanObject;
  mp: TJPoint;
  param: TJValueList;
begin
  if not IsCallEvent('onMouseWheel') then
    Exit;

  ss := SetToStr(TypeInfo(TShiftState),Shift);
  mp := TJPoint.Create(FEngine);
  mp.__Point := MousePos;

  hd := TJBooleanObject.Create(FEngine);
  hd.bool := Handled;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ss);
    param.Add(WheelDelta);
    param.Add(mp);
    param.Add(hd);
    CallEvent('','onMouseWheel',param);
  finally
    Handled := hd.bool;
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
//イベント
var
  ss: String;
  hd: TJBooleanObject;
  mp: TJPoint;
  param: TJValueList;
begin
  if not IsCallEvent('onMouseWheelDown') then
    Exit;

  ss := SetToStr(TypeInfo(TShiftState),Shift);
  mp := TJPoint.Create(FEngine);
  mp.__Point := MousePos;

  hd := TJBooleanObject.Create(FEngine);
  hd.bool := Handled;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ss);
    param.Add(mp);
    param.Add(hd);
    CallEvent('','onMouseWheelDown',param);
  finally
    Handled := hd.bool;
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
//イベント
var
  ss: String;
  hd: TJBooleanObject;
  mp: TJPoint;
  param: TJValueList;
begin
  if not IsCallEvent('onMouseWheelUp') then
    Exit;

  ss := SetToStr(TypeInfo(TShiftState),Shift);
  mp := TJPoint.Create(FEngine);
  mp.__Point := MousePos;

  hd := TJBooleanObject.Create(FEngine);
  hd.bool := Handled;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ss);
    param.Add(mp);
    param.Add(hd);
    CallEvent('','onMouseWheelUp',param);
  finally
    Handled := hd.bool;
    param.Free;
  end;
end;

procedure TJVCLWinControl.OnUnDock(Sender: TObject; Client: TControl;
  NewTarget: TWinControl; var Allow: Boolean);
//VCLイベント
var
  cl,nt: TJVCLPersistent;
  al: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onUnDock') then
    Exit;

  cl := VCLCaster.Cast(Client,FEngine);
  nt := VCLCaster.Cast(NewTarget,FEngine);
  al := TJBooleanObject.Create(FEngine);
  al.bool := Allow;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(cl);
    param.Add(nt);
    param.Add(al);
    CallEvent('','onUnDock',param);
  finally
    Allow := al.bool;
    param.Free;
  end;
end;

procedure TJVCLWinControl.SetacceptDrop(const Value: Boolean);
//drop targetをセット
begin
  if Value then
  begin
    CheckVCL;
    //無い場合は作成
    if not Assigned(FDragDropTarget) then
    begin
      FDragDropTarget := TDragDropTarget.Create(nil);
      FDragDropTarget.OnDragDropFiles := OnDragDropFiles;
      FDragDropTarget.OnDragDropText := OnDragDropText;
      //FDragDropTarget.OnDragDropOver := OnDragDropOver;
    end;

    FDragDropTarget.Target := GetWinControl;
  end
  else begin
    //ある場合は削除
    if Assigned(FDragDropTarget) then
    begin
      FDragDropTarget.Target := nil;
      FreeAndNil(FDragDropTarget);
    end;
  end;
end;

procedure TJVCLWinControl.SetParentWindow(const Value: Integer);
begin
  CheckVCL;
  GetWinControl.ParentWindow := Value;
end;

class function TJVCLWinControl.VCLClassType: TClass;
begin
  Result := TWinControl;
end;


{ TJVCLBaseForm }

constructor TJVCLBaseForm.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
var
  v: TJValue;
begin
  inherited;
  RegistName('VCLForm');
  RegistMethod('showModal',DoShowModal);
  RegistMethod('close',DoClose);
  RegistMethod('closeQuery',DoCloseQuery);
  RegistMethod('focusControl',DoFocusControl);
  RegistMethod('hide',DoHide);
  RegistMethod('print',DoPrint);
  RegistMethod('release',DoRelease);
  RegistMethod('sendCancelMode',DoSendCancelMode);
  RegistMethod('setFocusedControl',DoSetFocusedControl);
  RegistMethod('show',DoShow);
  RegistMethod('arrangeIcons',DoArrangeIcons);
  RegistMethod('cascade',DoCascade);
  RegistMethod('next',DoNext);
  RegistMethod('previous',DoPrevious);
  RegistMethod('tile',DoTile);
  RegistMethod('loadFromFile',DoLoadFromFile);
  RegistMethod('saveToFile',DoSaveToFile);
  RegistMethod('loadFromText',DoLoadFromText);
  RegistMethod('saveToText',DoSaveToText);
  RegistMethod('doDragDropFiles',DodoDragDropFiles);
  RegistMethod('doDragDropText',DodoDragDropText);

  //trueならばmainをセット
  if IsParam1(Param) then
  begin
    v := Param[0];
    if IsBool(@v) then
      FMain := AsBool(@v);
  end;
end;

destructor TJVCLBaseForm.Destroy;
begin
  FreeAndNil(FDragDropSource);
  inherited;
end;

function TJVCLBaseForm.DoArrangeIcons(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetForm.ArrangeIcons;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoCascade(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetForm.Cascade;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoClose(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomForm.Close;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoCloseQuery(Param: TJValueList): TJValue;
begin
  CheckVCL;
  try
    Result := BuildBool(GetCustomForm.CloseQuery);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoDoDragDropFiles(Param: TJValueList): TJValue;
var
  v,e: TJValue;
  sl: TStringList;
  i: Integer;
  dir: String;
begin
  CheckVCL;
  Result := BuildObject(Self);
  dir := '';

  sl := TStringList.Create;
  try
    //dir,files
    if IsParam2(Param) then
    begin
      v := Param[0];
      dir := AsString(@v);
      v := Param[1];
    end
    //files
    else if IsParam1(Param) then
      v := Param[0]
    else //エラー
      ArgsError;

    //arrayチェック
    if IsArrayObject(@v) then
    begin
      for i := 0 to v.vObject.GetCount - 1 do
      begin
        e := v.vObject.GetItem(i);
        sl.Add(AsString(@e));
      end;
    end
    else //そのまま入れる
      sl.Text := AsString(@v);

    //作成
    if not Assigned(FDragDropSource) then
      FDragDropSource := TDragDropSource.Create(nil);

    try
      FDragDropSource.DoDragFiles(dir,sl);
    except
      on E:Exception do
        Error(E.Message);
    end;
  finally
    sl.Free;
  end;
end;

function TJVCLBaseForm.DoDoDragDropText(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);
  if IsParam1(Param) then
  begin
    v := Param[0];

    //作成
    if not Assigned(FDragDropSource) then
      FDragDropSource := TDragDropSource.Create(nil);

    try
      FDragDropSource.DoDragText(AsString(@v));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseForm.DoFocusControl(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  v := Param[0];
  if IsObject(@v) and (v.vObject is TJVCLWinControl) then
  begin
    try
      GetCustomForm.FocusControl((v.vObject as TJVCLWinControl).GetWinControl);
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseForm.DoHide(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomForm.Hide;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoLoadFromFile(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    LoadComponentsFromFile(AsString(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoLoadFromText(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    LoadComponentsFromText(AsString(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoNext(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetForm.Next;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoPrevious(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetForm.Previous;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoPrint(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomForm.Print;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoRelease(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomForm.Release;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoSaveToFile(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    SaveComponentsToFile(AsString(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoSendCancelMode(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  v := Param[0];
  if IsObject(@v) and (v.vObject is TJVCLControl) then
  begin
    try
      GetCustomForm.SendCancelMode((v.vObject as TJVCLControl).GetControl);
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseForm.DoSetFocusedControl(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  if IsObject(@v) and (v.vObject is TJVCLWinControl) then
  begin
    try
      Result := BuildBool(
        GetCustomForm.SetFocusedControl((v.vObject as TJVCLWinControl).GetWinControl));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseForm.DoShow(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomForm.Show;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoShowModal(Param: TJValueList): TJValue;
begin
  CheckVCL;
  try
    Result := BuildInteger(GetCustomForm.ShowModal);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.DoTile(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetForm.Tile;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseForm.GetActive: Boolean;
begin
  CheckVCL;
  Result := GetCustomForm.Active
end;

function TJVCLBaseForm.GetCustomForm: TCustomForm;
begin
  Result := FVCL as TCustomForm;
end;

function TJVCLBaseForm.GetForm: TForm;
begin
  Result := FVCL as TForm;
end;

procedure TJVCLBaseForm.OnClose(Sender: TObject; var Action: TCloseAction);
//イベント
var
  ac: TJStringObject;
  tmpac: TJValue;
  param: TJValueList;
  eng: TJEngine;
  tmpenum: Integer;
begin
  //mainの場合はterminateする
  if FMain and Assigned(FEngine) then
  begin
    eng := FEngine as TJEngine;
    eng.GlobalObject.Terminate;
  end;

  if not IsCallEvent('onClose') then
    Exit;

  EnumToValue(TypeInfo(TCloseAction),tmpac,Ord(Action));
  ac := TJStringObject.Create(FEngine);
  ac.str := AsString(@tmpac);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ac);
    CallEvent('','onClose',param);
  finally
    //enumを入れ替え
    tmpac := BuildString(ac.str);
    if ValueToEnum(TypeInfo(TCloseAction),tmpac,tmpenum) then
      Action := TCloseAction(tmpenum);

    param.Free;
  end;
end;

procedure TJVCLBaseForm.OnCloseQuery(Sender: TObject; var CanClose: Boolean);
//イベント
var
  cc: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onCloseQuery') then
    Exit;

  cc := TJBooleanObject.Create(FEngine);
  cc.bool := CanClose;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(cc);
    CallEvent('','onCloseQuery',param);
  finally
    CanClose := cc.bool;
    param.Free;
  end;
end;

function TJVCLBaseForm.OnHelp(Command: Word; Data: NativeInt;
  var CallHelp: Boolean): Boolean;
//イベント
var
  ch: TJBooleanObject;
  param: TJValueList;
begin
{ TODO : 戻り値が不明 }
  Result := False;
  if not IsCallEvent('onHelp') then
    Exit;

  ch := TJBooleanObject.Create(FEngine);
  ch.bool := CallHelp;

  param := TJValueList.Create;
  try
    param.Add(Command);
    param.Add(Data);
    param.Add(ch);
    CallEvent('','onHelp',param);
  finally
    CallHelp := ch.bool;
    param.Free;
  end;

  Result := True;
end;

procedure TJVCLBaseForm.OnShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseForm.LoadComponents(Stream: TStream);
//コンポーネントを読み出し
var
  ms: TMemoryStream;
begin
  CheckVCL;
  ms := TMemoryStream.Create;
  try
    try
      ObjectTextToBinary(Stream,ms);
      ms.Seek(0,soFromBeginning);
      ms.ReadComponent(GetComponent);
    except
      on EParserError do
      begin
        Stream.Seek(0,soFromBeginning);
        Stream.ReadComponent(GetComponent);
      end;
    end;
    //コンポーネントを登録
    RegistComponents;
  finally
    ms.Free;
  end;
end;

procedure TJVCLBaseForm.LoadComponentsFromFile(Filename: String);
//コンポーネントを読み出し
var
  fs: TFileStream;
  path: String;
begin
  CheckVCL;
  if not Assigned(FEngine) then
    Exit;
  //ファイル名を得る
  if not FEngine.FindImportFilename(Filename,path) then
    Error('read component error: ' + Filename);

  try
    fs := TFileStream.Create(path,fmOpenRead);
    try
      LoadComponents(fs);
    finally
      fs.Free;
    end;
  except
    Error('read component error: ' + Filename);
  end;
end;

procedure TJVCLBaseForm.LoadComponentsFromText(S: String);
//コンポーネントを読み出し
var
  ss: TStringStream;
begin
  CheckVCL;
  ss := TStringStream.Create(S);
  try
    LoadComponents(ss);
  finally
    ss.Free;
  end;
end;

procedure TJVCLBaseForm.RegistEvents;
var
  form: TForm;
begin
  form := GetForm;
  if not Assigned(form) then
    Exit;

  form.OnActivate := OnActivate;
  form.OnCanResize := OnCanResize;
  //actionのため
  if not Assigned(form.Action) then
    form.OnClick := OnClick;

  form.OnClose := OnClose;
  form.OnCloseQuery := OnCloseQuery;
  form.OnConstrainedResize := OnConstrainedResize;
  form.OnContextPopup := OnContextPopup;
  form.OnCreate := OnCreate;
  form.OnDblClick := OnDblClick;
  form.OnDeactivate := OnDeactivate;
  form.OnDestroy := OnDestroy;
  form.OnDockDrop := OnDockDrop;
  form.OnDockOver := OnDockOver;
  form.OnDragDrop := OnDragDrop;
  form.OnDragOver := OnDragOver;
  form.OnEndDock := OnEndDock;
  form.OnGetSiteInfo := OnGetSiteInfo;
  form.OnHelp := OnHelp;
  form.OnHide := OnHide;
  form.OnKeyDown := OnKeyDown;
  form.OnKeyPress := OnKeyPress;
  form.OnKeyUp := OnKeyUp;
  form.OnMouseDown := OnMouseDown;
  form.OnMouseMove := OnMouseMove;
  form.OnMouseUp := OnMouseUp;
  form.OnMouseWheel := OnMouseWheel;
  form.OnMouseWheelDown := OnMouseWheelDown;
  form.OnMouseWheelUp := OnMouseWheelUp;
  form.OnPaint := OnPaint;
  form.OnResize := OnResize;
  form.OnShortCut := OnShortCut;
  form.OnShow := OnShow;
  form.OnStartDock := OnStartDock;
  form.OnUnDock := OnUnDock;
end;

class function TJVCLBaseForm.VCLClassType: TClass;
begin
  Result := TForm;
end;

procedure TJVCLBaseForm.SaveComponents(Stream: TStream);
//フォームを書き出す
var
  ms: TMemoryStream;
begin
  CheckVCL;
  ms := TMemoryStream.Create;
  try
    ms.WriteComponent(GetComponent);
    ms.Seek(0,soFromBeginning);
    //テキストに変換する
    ObjectBinaryToText(ms,Stream);
  finally
    ms.Free;
  end;
end;

procedure TJVCLBaseForm.SaveComponentsToFile(Filename: String);
//フォームを書き出す
var
  fs: TFileStream;
begin
  CheckVCL;

  try
    fs := TFileStream.Create(Filename,fmCreate);
    try
      SaveComponents(fs);
    finally
      fs.Free;
    end;
  except
    Error('read component error: ' + Filename);
  end;
end;

function TJVCLBaseForm.SaveComponentsToText: String;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    SaveComponents(ss);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

function TJVCLBaseForm.DoSaveToText(Param: TJValueList): TJValue;
begin
  CheckVCL;

  try
    Result := BuildString(SaveComponentsToText);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

{ TJVCLBaseEdit }

constructor TJVCLBaseEdit.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLEdit');
  RegistMethod('clear',DoClear);
  RegistMethod('clearSelection',DoClearSelection);
  RegistMethod('copyToClipboard',DoCopyToClipboard);
  RegistMethod('cutToClipboard',DoCutToClipboard);
  RegistMethod('pasteFromClipboard',DoPasteFromClipboard);
  RegistMethod('undo',DoUndo);
  RegistMethod('clearUndo',DoClearUndo);
  RegistMethod('selectAll',DoSelectAll);
end;

function TJVCLBaseEdit.DoClear(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.Clear;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoClearSelection(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.ClearSelection;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoClearUndo(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.ClearUndo;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoCopyToClipboard(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.CopyToClipboard;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoCutToClipboard(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.CutToClipboard;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoPasteFromClipboard(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.PasteFromClipboard;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoSelectAll(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.SelectAll;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.DoUndo(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomEdit.Undo;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseEdit.GetCanUndo: Boolean;
begin
  CheckVCL;
  Result := GetCustomEdit.CanUndo
end;

function TJVCLBaseEdit.GetCustomEdit: TCustomEdit;
begin
  Result := FVCL as TCustomEdit;
end;

function TJVCLBaseEdit.GetEdit: TEdit;
begin
  Result := FVCL as TEdit;
end;

function TJVCLBaseEdit.GetModified: Boolean;
begin
  CheckVCL;
  Result := GetCustomEdit.Modified
end;

function TJVCLBaseEdit.GetSelLength: Integer;
begin
  CheckVCL;
  Result := GetCustomEdit.SelLength
end;

function TJVCLBaseEdit.GetSelStart: Integer;
begin
  CheckVCL;
  Result := GetCustomEdit.SelStart
end;

function TJVCLBaseEdit.GetSelText: string;
begin
  CheckVCL;
  Result := GetCustomEdit.SelText
end;

procedure TJVCLBaseEdit.RegistEvents;
var
  edit: TEdit;
begin
  edit := GetEdit;
  if not Assigned(edit) then
    Exit;

  edit.OnChange := OnChange;
  edit.OnClick := OnClick;
  edit.OnContextPopup := OnContextPopup;
  edit.OnDblClick := OnDblClick;
  edit.OnDragDrop := OnDragDrop;
  edit.OnDragOver := OnDragOver;
  edit.OnEndDock := OnEndDock;
  edit.OnEndDrag := OnEndDrag;
  edit.OnEnter := OnEnter;
  edit.OnExit := OnExit;
  edit.OnKeyDown := OnKeyDown;
  edit.OnKeyPress := OnKeyPress;
  edit.OnKeyUp := OnKeyUp;
  edit.OnMouseDown := OnMouseDown;
  edit.OnMouseMove := OnMouseMove;
  edit.OnMouseUp := OnMouseUp;
  edit.OnStartDock := OnStartDock;
  edit.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseEdit.SetModified(const Value: Boolean);
begin
  CheckVCL;
  GetCustomEdit.Modified := Value;
end;

procedure TJVCLBaseEdit.SetSelLength(const Value: Integer);
begin
  CheckVCL;
  GetCustomEdit.SelLength := Value;
end;

procedure TJVCLBaseEdit.SetSelStart(const Value: Integer);
begin
  CheckVCL;
  GetCustomEdit.SelStart := Value;
end;

procedure TJVCLBaseEdit.SetSelText(const Value: string);
begin
  CheckVCL;
  GetCustomEdit.SelText := Value;
end;

class function TJVCLBaseEdit.VCLClassType: TClass;
begin
  Result := TEdit;
end;

{ TJVCLBaseButton }

constructor TJVCLBaseButton.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLButton');
  RegistMethod('click',DoClick);
end;

function TJVCLBaseButton.DoClick(Param: TJValueList): TJValue;
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetButton.Click;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseButton.GetButton: TButton;
begin
  Result := FVCL as TButton;
end;

function TJVCLBaseButton.GetButtonControl: TButtonControl;
begin
  Result := FVCL as TButtonControl;
end;

procedure TJVCLBaseButton.RegistEvents;
//イベント登録
var
  btn: TButton;
begin
  btn := GetButton;
  if not Assigned(btn) then
    Exit;
  //actionのため
  if not Assigned(btn.Action) then
    btn.OnClick := OnClick;

  btn.OnContextPopup := OnContextPopup;
  btn.OnDragDrop := OnDragDrop;
  btn.OnDragOver := OnDragOver;
  btn.OnEndDock := OnEndDock;
  btn.OnEndDrag := OnEndDrag;
  btn.OnEnter := OnEnter;
  btn.OnExit := OnExit;
  btn.OnKeyDown := OnKeyDown;
  btn.OnKeyPress := OnKeyPress;
  btn.OnMouseDown := OnMouseDown;
  btn.OnMouseMove := OnMouseMove;
  btn.OnMouseUp := OnMouseUp;
  btn.OnStartDock := OnStartDock;
  btn.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseButton.VCLClassType: TClass;
begin
  Result := TButton;
end;

{ TJVCLBaseMemo }

constructor TJVCLBaseMemo.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLMemo');
end;

procedure TJVCLBaseMemo.CreateObjects;
begin
  inherited;
  FLines := TJBaseStringsObject.Create(FEngine,nil,False);
  FLines.IncRef;
  FCaretPos := TJPoint.Create(FEngine,nil,False);
  FCaretPos.IncRef;
end;

destructor TJVCLBaseMemo.Destroy;
begin
  //DestroyVCLを先に呼ぶためinherited
  inherited;
  FLines.Strings := nil;
  FLines.DecRef;
  FCaretPos.DecRef;
end;

procedure TJVCLBaseMemo.DestroyVCL;
begin
  inherited;
  FLines.Strings := nil;
end;

function TJVCLBaseMemo.GetCaretPos: TJPoint;
begin
  Result := FCaretPos;
  CheckVCL;
  FCaretPos.__Point := GetCustomMemo.CaretPos;
end;

function TJVCLBaseMemo.GetCustomMemo: TCustomMemo;
begin
  Result := FVCL as TCustomMemo;
end;

function TJVCLBaseMemo.GetLines: TJBaseStringsObject;
begin
  CheckVCL;
  Result := FLines;
  FLines.Strings := GetCustomMemo.Lines
end;

function TJVCLBaseMemo.GetMemo: TMemo;
begin
  Result := FVCL as TMemo;
end;

function TJVCLBaseMemo.GetText: String;
begin
  CheckVCL;
  Result := GetCustomMemo.Lines.Text;
end;

procedure TJVCLBaseMemo.RegistEvents;
var
  edit: TMemo;
begin
  edit := GetMemo;
  if not Assigned(edit) then
    Exit;

  edit.OnChange := OnChange;
  edit.OnClick := OnClick;
  edit.OnContextPopup := OnContextPopup;
  edit.OnDblClick := OnDblClick;
  edit.OnDragDrop := OnDragDrop;
  edit.OnDragOver := OnDragOver;
  edit.OnEndDock := OnEndDock;
  edit.OnEndDrag := OnEndDrag;
  edit.OnEnter := OnEnter;
  edit.OnExit := OnExit;
  edit.OnKeyDown := OnKeyDown;
  edit.OnKeyPress := OnKeyPress;
  edit.OnKeyUp := OnKeyUp;
  edit.OnMouseDown := OnMouseDown;
  edit.OnMouseMove := OnMouseMove;
  edit.OnMouseUp := OnMouseUp;
  edit.OnStartDock := OnStartDock;
  edit.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseMemo.SetText(const Value: String);
begin
  CheckVCL;
  GetCustomMemo.Lines.Text := Value;
end;

class function TJVCLBaseMemo.VCLClassType: TClass;
begin
  Result := TMemo;
end;

{ TJVCLBaseLabel }

constructor TJVCLBaseLabel.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLLabel');
end;

function TJVCLBaseLabel.GetLabel: TLabel;
begin
  Result := FVCL as TLabel;
end;

procedure TJVCLBaseLabel.RegistEvents;
var
  c: TLabel;
begin
  c := GetLabel;
  if not Assigned(c) then
    Exit;

  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDblClick := OnDblClick;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseLabel.VCLClassType: TClass;
begin
  Result := TLabel;
end;

{ TJVCLBaseTimer }

constructor TJVCLBaseTimer.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLTimer');
end;

function TJVCLBaseTimer.GetTimer: TTimer;
begin
  Result := FVCL as TTimer;
end;

procedure TJVCLBaseTimer.RegistEvents;
begin
  CheckVCL;
  GetTimer.OnTimer := OnTimer;
end;

class function TJVCLBaseTimer.VCLClassType: TClass;
begin
  Result := TTimer;
end;

{ TJVCLBaseMenu }

constructor TJVCLBaseMenu.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLMenu');

  RegistMethod('dispatchCommand',DoDispatchCommand);
  RegistMethod('dispatchPopup',DoDispatchPopup);
  RegistMethod('findItem',DoFindItem);
  RegistMethod('getHelpContext',DoGetHelpContext);
end;

procedure TJVCLBaseMenu.CreateObjects;
begin
  inherited;
  FItems := TJVCLBaseMenuItem.Create(FEngine,nil,False);
  FItems.IncRef;
end;

destructor TJVCLBaseMenu.Destroy;
begin
  //先にinherited
  inherited;
  FItems.DecRef;
end;

procedure TJVCLBaseMenu.DestroyVCL;
begin
  inherited;
  //消す
  FItems.RegistVCL(nil,False);
end;

function TJVCLBaseMenu.DoDispatchCommand(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    Result := BuildBool(
      GetMenu.DispatchCommand(AsInteger(@v)));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenu.DoDispatchPopup(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    Result := BuildBool(
      GetMenu.DispatchPopup(AsInteger(@v)));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenu.DoFindItem(Param: TJValueList): TJValue;
//VCLメソッド
var
  v,k: TJValue;
  enum: Integer;
  mi: TMenuItem;
  vclmi: TJVCLBaseMenuItem;
begin
  CheckVCL(Param,2);
  Result := BuildNull;
  v := Param[0];
  k := Param[1];
  try
    if ValueToEnum(TypeInfo(TFindItemKind),k,enum) then
    begin
      mi := GetMenu.FindItem(AsInteger(@v),TFindItemKind(enum));
      if Assigned(mi) then
      begin
        vclmi := TJVCLBaseMenuItem.Create(FEngine);
        vclmi.RegistVCL(mi,False);
        Result := BuildObject(vclmi);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenu.DoGetHelpContext(Param: TJValueList): TJValue;
//VCLメソッド
var
  v,by: TJValue;
begin
  CheckVCL(Param,2);
  v := Param[0];
  by := Param[1];
  try
    Result := BuildInteger(
      GetMenu.GetHelpContext(AsInteger(@v),AsBool(@by)));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenu.GetHandle: Integer;
begin
  CheckVCL;
  Result := GetMenu.Handle
end;

function TJVCLBaseMenu.GetItems: TJVCLBaseMenuItem;
begin
  CheckVCL;
  Result := FItems;
  FItems.RegistVCL(GetMenu.Items,False);
end;

function TJVCLBaseMenu.GetMenu: TMenu;
begin
  Result := FVCL as TMenu;
end;

function TJVCLBaseMenu.GetWindowHandle: Integer;
begin
  CheckVCL;
  Result := GetMenu.WindowHandle
end;

procedure TJVCLBaseMenu.OnChange(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
//VCLイベント
var
  param: TJValueList;
  mi: TJVCLBaseMenuItem;
begin
  if not IsCallEvent('onChange') then
    Exit;

  mi := TJVCLBaseMenuItem.Create(FEngine);
  mi.RegistVCL(Source,False);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(mi);
    param.Add(Rebuild);
    CallEvent('','onChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseMenu.SetWindowHandle(const Value: Integer);
begin
  CheckVCL;
  GetMenu.WindowHandle := Value;
end;

class function TJVCLBaseMenu.VCLClassType: TClass;
begin
  Result := TMenu;
end;

{ TJVCLBaseMainMenu }

constructor TJVCLBaseMainMenu.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLMainMenu');

  RegistMethod('merge',DoMerge);
  RegistMethod('unmerge',DoUnmerge);
end;

function TJVCLBaseMainMenu.DoMerge(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    if IsObject(@v) and
      (v.vObject is TJVCLBaseMainMenu) and
      (v.vObject as TJVCLBaseMainMenu).IsVCL then
    begin
      GetMainMenu.Merge((v.vObject as TJVCLBaseMainMenu).GetMainMenu);
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMainMenu.DoUnmerge(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    if IsObject(@v) and
      (v.vObject is TJVCLBaseMainMenu) and
      (v.vObject as TJVCLBaseMainMenu).IsVCL then
    begin
      GetMainMenu.UnMerge((v.vObject as TJVCLBaseMainMenu).GetMainMenu);
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMainMenu.GetMainMenu: TMainMenu;
begin
  Result := FVCL as TMainMenu;
end;

procedure TJVCLBaseMainMenu.RegistEvents;
var
  c: TMainMenu;
begin
  c := GetMainMenu;
  if not Assigned(c) then
    Exit;

  c.OnChange := OnChange;
end;

class function TJVCLBaseMainMenu.VCLClassType: TClass;
begin
  Result := TMainMenu;
end;

{ TJVCLBasePopupMenu }

constructor TJVCLBasePopupMenu.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLPopupMenu');
  RegistMethod('popup',DoPopup);
end;

function TJVCLBasePopupMenu.DoPopup(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  x := Param[0];
  y := Param[1];
  try
    GetPopupMenu.Popup(AsInteger(@x),AsInteger(@y));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBasePopupMenu.GetPopupComponent: TJVCLComponent;
begin
  CheckVCL;
  if Assigned(GetPopupMenu.PopupComponent) then
    Result := VCLCaster.Cast(GetPopupMenu.PopupComponent,FEngine) as TJVCLComponent
  else
    Result := nil;
end;

function TJVCLBasePopupMenu.GetPopupMenu: TPopupMenu;
begin
  Result := FVCL as TPopupMenu;
end;

procedure TJVCLBasePopupMenu.RegistEvents;
var
  c: TPopupMenu;
begin
  c := GetPopupMenu;
  if not Assigned(c) then
    Exit;

  c.OnChange := OnChange;
  c.OnPopup := OnPopup;
end;

procedure TJVCLBasePopupMenu.SetPopupComponent(const Value: TJVCLComponent);
begin
  if IsVCL and Assigned(Value) then
    GetPopupMenu.PopupComponent := Value.GetComponent;
end;

class function TJVCLBasePopupMenu.VCLClassType: TClass;
begin
  Result := TPopupMenu;
end;

{ TJVCLBaseCheckBox }

constructor TJVCLBaseCheckBox.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLCheckBox');
end;

function TJVCLBaseCheckBox.GetCheckBox: TCheckBox;
begin
  Result := FVCL as TCheckBox;
end;

function TJVCLBaseCheckBox.GetCustomCheckBox: TCustomCheckBox;
begin
  Result := FVCL as TCustomCheckBox;
end;

procedure TJVCLBaseCheckBox.RegistEvents;
var
  c: TCheckBox;
begin
  c := GetCheckBox;
  if not Assigned(c) then
    Exit;
  //actionのため
  if not Assigned(c.Action) then
    c.OnClick := OnClick;

  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseCheckBox.VCLClassType: TClass;
begin
  Result := TCheckBox;
end;

{ TJVCLBaseRadioButton }

constructor TJVCLBaseRadioButton.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLRadioButton');
end;

function TJVCLBaseRadioButton.GetRadioButton: TRadioButton;
begin
  Result := FVCL as TRadioButton;
end;

procedure TJVCLBaseRadioButton.RegistEvents;
var
  c: TRadioButton;
begin
  c := GetRadioButton;
  if not Assigned(c) then
    Exit;

  //actionのため
  if not Assigned(c.Action) then
    c.OnClick := OnClick;

  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseRadioButton.VCLClassType: TClass;
begin
  Result := TRadioButton;
end;

{ TJVCLBaseListBox }

function TJVCLBaseListBox.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetCustomListBox.Items.Count);
end;

constructor TJVCLBaseListBox.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLListBox');
  RegistMethod('clear',DoClear);
  RegistMethod('clearSelection',DoClearSelection);
  RegistMethod('deleteSelected',DoDeleteSelected);
  RegistMethod('itemAtPos',DoItemAtPos);
  RegistMethod('itemRect',DoItemRect);
  RegistMethod('selectAll',DoSelectAll);
  RegistMethod('selected',DoSelected);
end;

procedure TJVCLBaseListBox.CreateObjects;
begin
  inherited;
  FItems := TJBaseStringsObject.Create(FEngine,nil,False);
  FItems.IncRef;
end;

destructor TJVCLBaseListBox.Destroy;
begin
  //DestroyVCLを先に呼ぶためinherited
  inherited;
  FItems.Strings := nil;
  FItems.DecRef;
end;

procedure TJVCLBaseListBox.DestroyVCL;
begin
  inherited;
  FItems.Strings := nil;
end;

function TJVCLBaseListBox.DoClear(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomListBox.Clear;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListBox.DoClearSelection(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomListBox.ClearSelection;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListBox.DoDeleteSelected(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomListBox.DeleteSelected;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListBox.DoItemAtPos(Param: TJValueList): TJValue;
//VCLメソッド
var
  pos: TPoint;
  o,exis,x,y: TJValue;
begin
  CheckVCL(Param,2);

  o := Param[0];
  if IsObject(@o) then
  begin
    x := o.vObject.GetValue('x',True);
    y := o.vObject.GetValue('y',True);
    pos.x := AsInteger(@x);
    pos.y := AsInteger(@y);
    exis := Param[1];

    try
      Result := BuildInteger(
        GetCustomListBox.ItemAtPos(pos,AsBool(@exis)));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseListBox.DoItemRect(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  rect: TJRect;
begin
  CheckVCL(Param,1);
  Result := BuildNull;

  v := Param[0];
  try
    if CheckRange(AsInteger(@v)) then
    begin
      rect := TJRect.Create(FEngine);
      rect.__Rect := GetCustomListBox.ItemRect(AsInteger(@v));
      Result := BuildObject(rect);
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListBox.DoSelectAll(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomListBox.SelectAll;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListBox.DoSelected(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildBool(False);

  try
    i := Param[0];
    if CheckRange(AsInteger(@i)) then
    begin
      if IsParam2(Param) then
      begin
        //setter
        Result := Param[1];
        GetCustomListBox.Selected[AsInteger(@i)] := AsBool(@Result);
      end
      else begin
        //getter
        Result := BuildBool(
          GetCustomListBox.Selected[AsInteger(@i)]);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListBox.GetCustomListBox: TCustomListBox;
begin
  Result := FVCL as TCustomListBox;
end;

function TJVCLBaseListBox.GetItemIndex: Integer;
begin
  CheckVCL;
  Result := GetCustomListBox.ItemIndex
end;

function TJVCLBaseListBox.GetItems: TJBaseStringsObject;
begin
  CheckVCL;
  Result := FItems;
  FItems.Strings := GetCustomListBox.Items
end;

function TJVCLBaseListBox.GetListBox: TListBox;
begin
  Result := FVCL as TListBox;
end;

function TJVCLBaseListBox.GetSelCount: Integer;
begin
  CheckVCL;
  Result := GetCustomListBox.SelCount
end;

function TJVCLBaseListBox.GetTopIndex: Integer;
begin
  CheckVCL;
  Result := GetCustomListBox.TopIndex
end;

procedure TJVCLBaseListBox.RegistEvents;
var
  c: TListBox;
begin
  c := GetListBox;
    if not Assigned(c) then
    Exit;

  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDblClick := OnDblClick;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseListBox.SetItemIndex(const Value: Integer);
begin
  CheckVCL;
  GetCustomListBox.ItemIndex := Value;
end;

procedure TJVCLBaseListBox.SetTopIndex(const Value: Integer);
begin
  CheckVCL;
  GetCustomListBox.TopIndex := Value;
end;

class function TJVCLBaseListBox.VCLClassType: TClass;
begin
  Result := TListBox;
end;

{ TJVCLBaseComboBox }

constructor TJVCLBaseComboBox.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLComboBox');

  RegistMethod('clear',DoClear);
  RegistMethod('selectAll',DoSelectAll);
end;

procedure TJVCLBaseComboBox.CreateObjects;
begin
  inherited;
  FItems := TJBaseStringsObject.Create(FEngine,nil,False);
  FItems.IncRef;
end;

destructor TJVCLBaseComboBox.Destroy;
begin
  //DestroyVCLを先に呼ぶためinherited
  inherited;
  FItems.Strings := nil;
  FItems.DecRef;
end;

procedure TJVCLBaseComboBox.DestroyVCL;
begin
  inherited;
  FItems.Strings := nil;
end;

function TJVCLBaseComboBox.DoClear(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomComboBox.Clear;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseComboBox.DoSelectAll(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetCustomComboBox.SelectAll;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseComboBox.GetCharCase: String;
begin
  CheckVCL;
  Result := GetEnumName(TypeInfo(TEditCharCase),Ord(GetCustomComboBox.CharCase))
end;

function TJVCLBaseComboBox.GetComboBox: TComboBox;
begin
  Result := FVCL as TComboBox;
end;

function TJVCLBaseComboBox.GetCustomComboBox: TCustomComboBox;
begin
  Result := FVCL as TCustomComboBox;
end;

function TJVCLBaseComboBox.GetDroppedDown: Boolean;
begin
  CheckVCL;
  Result := GetCustomComboBox.DroppedDown
end;

function TJVCLBaseComboBox.GetItemIndex: Integer;
begin
  CheckVCL;
  Result := GetCustomComboBox.ItemIndex
end;

function TJVCLBaseComboBox.GetItems: TJBaseStringsObject;
begin
  CheckVCL;
  Result := FItems;
  FItems.Strings := GetCustomComboBox.Items
end;

function TJVCLBaseComboBox.GetSelLength: Integer;
begin
  CheckVCL;
  Result := GetCustomComboBox.SelLength
end;

function TJVCLBaseComboBox.GetSelStart: Integer;
begin
  CheckVCL;
  Result := GetCustomComboBox.SelStart
end;

function TJVCLBaseComboBox.GetSelText: string;
begin
  CheckVCL;
  Result := GetCustomComboBox.SelText
end;

procedure TJVCLBaseComboBox.RegistEvents;
var
  c: TComboBox;
begin
  c := GetComboBox;
  if not Assigned(c) then
    Exit;

  c.OnChange := OnChange;
  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDropDown := OnDropDown;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseComboBox.SetCharCase(const Value: String);
var
  enum: Integer;
begin
  CheckVCL;
  enum := GetEnumValue(TypeInfo(TEditCharCase),Value);
  if enum > -1 then
    GetCustomComboBox.CharCase := TEditCharCase(enum);
end;

procedure TJVCLBaseComboBox.SetDroppedDown(const Value: Boolean);
begin
  CheckVCL;
  GetCustomComboBox.DroppedDown := Value;
end;

procedure TJVCLBaseComboBox.SetItemIndex(const Value: Integer);
begin
  CheckVCL;
  GetCustomComboBox.ItemIndex := Value;
end;

procedure TJVCLBaseComboBox.SetSelLength(const Value: Integer);
begin
  CheckVCL;
  GetCustomComboBox.SelLength := Value;
end;

procedure TJVCLBaseComboBox.SetSelStart(const Value: Integer);
begin
  CheckVCL;
  GetCustomComboBox.SelStart := Value;
end;

procedure TJVCLBaseComboBox.SetSelText(const Value: string);
begin
  CheckVCL;
  GetCustomComboBox.SelText := Value;
end;

class function TJVCLBaseComboBox.VCLClassType: TClass;
begin
  Result := TComboBox;
end;

{ TJVCLBaseGroupBox }

constructor TJVCLBaseGroupBox.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLGroupBox');
end;

function TJVCLBaseGroupBox.GetGroupBox: TGroupBox;
begin
  Result := FVCL as TGroupBox;
end;

procedure TJVCLBaseGroupBox.RegistEvents;
var
  c: TGroupBox;
begin
  c := GetGroupBox;
  if not Assigned(c) then
    Exit;

  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDblClick := OnDblClick;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

class function TJVCLBaseGroupBox.VCLClassType: TClass;
begin
  Result := TGroupBox;
end;

{ TJVCLBaseRadioGroup }

constructor TJVCLBaseRadioGroup.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLRadioGroup');
end;

procedure TJVCLBaseRadioGroup.CreateObjects;
begin
  inherited;
  FItems := TJBaseStringsObject.Create(FEngine,nil,False);
  FItems.IncRef;
end;

destructor TJVCLBaseRadioGroup.Destroy;
begin
  //DestroyVCLを先に呼ぶためinherited
  inherited;
  FItems.Strings := nil;
  FItems.DecRef;
end;

procedure TJVCLBaseRadioGroup.DestroyVCL;
begin
  inherited;
  FItems.Strings := nil;
end;

function TJVCLBaseRadioGroup.GetItems: TJBaseStringsObject;
begin
  CheckVCL;
  Result := FItems;
  FItems.Strings := GetRadioGroup.Items
end;

function TJVCLBaseRadioGroup.GetRadioGroup: TRadioGroup;
begin
  Result := FVCL as TRadioGroup;
end;

procedure TJVCLBaseRadioGroup.RegistEvents;
var
  c: TRadioGroup;
begin
  c := GetRadioGroup;
  if not Assigned(c) then
    Exit;

  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseRadioGroup.VCLClassType: TClass;
begin
  Result := TRadioGroup;
end;

{ TJVCLBasePanel }

constructor TJVCLBasePanel.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLPanel');
end;

function TJVCLBasePanel.GetPanel: TPanel;
begin
  Result := FVCL as TPanel;
end;

procedure TJVCLBasePanel.RegistEvents;
var
  c: TPanel;
begin
  c := GetPanel;
  if not Assigned(c) then
    Exit;

  c.OnCanResize := OnCanResize;
  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDblClick := OnDblClick;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

class function TJVCLBasePanel.VCLClassType: TClass;
begin
  Result := TPanel;
end;

{ TJVCLBaseSplitter }

constructor TJVCLBaseSplitter.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLSplitter');
end;

function TJVCLBaseSplitter.GetSplitter: TSplitter;
begin
  Result := FVCL as TSplitter;
end;

procedure TJVCLBaseSplitter.OnCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
//イベント
var
  nw: TJNumberObject;
  ap: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onCanResize') then
    Exit;

  nw := TJNumberObject.Create(FEngine);
  nw.int := NewSize;

  ap := TJBooleanObject.Create(FEngine);
  ap.bool := Accept;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(nw);
    param.Add(ap);
    CallEvent('','onCanResize',param);
  finally
    NewSize := nw.int;
    Accept := ap.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseSplitter.RegistEvents;
var
  c: TSplitter;
begin
  c := GetSplitter;
  if not Assigned(c) then
    Exit;

  c.OnCanResize := OnCanResize;
  c.OnMoved := OnMoved;
  c.OnPaint := OnPaint;
end;

class function TJVCLBaseSplitter.VCLClassType: TClass;
begin
  Result := TSplitter;
end;

{ TJVCLBaseCheckListBox }

constructor TJVCLBaseCheckListBox.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLCheckListBox');

  RegistMethod('checked',DoChecked);
  RegistMethod('itemEnabled',DoItemEnabled);
  RegistMethod('state',DoState);
end;

function TJVCLBaseCheckListBox.DoChecked(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildBool(False);

  try
    i := Param[0];
    if CheckRange(AsInteger(@i)) then
    begin
      if IsParam2(Param) then
      begin
        //setter
        Result := Param[1];
        GetCheckListBox.Checked[AsInteger(@i)] := AsBool(@Result);
      end
      else begin
        //getter
        Result := BuildBool(
          GetCheckListBox.Checked[AsInteger(@i)]);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseCheckListBox.DoItemEnabled(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildBool(False);

  try
    i := Param[0];
    if CheckRange(AsInteger(@i)) then
    begin
      if IsParam2(Param) then
      begin
        //setter
        Result := Param[1];
        GetCheckListBox.ItemEnabled[AsInteger(@i)] := AsBool(@Result);
      end
      else begin
        //getter
        Result := BuildBool(
          GetCheckListBox.ItemEnabled[AsInteger(@i)]);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseCheckListBox.DoState(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
  enum: Integer;
begin
  CheckVCL(Param,1);
  Result := BuildString('');
  try
    i := Param[0];
    if CheckRange(AsInteger(@i)) then
    begin
      if IsParam2(Param) then
      begin
        //setter
        Result := Param[1];
        if ValueToEnum(
          TypeInfo(TCheckBoxState),
          Result,
          enum) then
        begin
          GetCheckListBox.State[AsInteger(@i)] := TCheckBoxState(enum);
        end;
      end
      else begin
        //getter
        EnumToValue(
          TypeInfo(TCheckBoxState),
          Result,
          Ord(GetCheckListBox.State[AsInteger(@i)]));
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseCheckListBox.GetCheckListBox: TCheckListBox;
begin
  Result := FVCL as TCheckListBox;
end;

procedure TJVCLBaseCheckListBox.RegistEvents;
var
  c: TCheckListBox;
begin
  c := GetCheckListBox;
  if not Assigned(c) then
    Exit;

  c.OnClick := OnClick;
  c.OnClickCheck := OnClickCheck;
  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDblClick := OnDblClick;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseCheckListBox.VCLClassType: TClass;
begin
  Result := TCheckListBox;
end;

{ TJVCLBaseImage }

constructor TJVCLBaseImage.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLImage');
end;

function TJVCLBaseImage.GetImage: TImage;
begin
  Result := FVCL as TImage;
end;

procedure TJVCLBaseImage.OnProgress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
  const Msg: String);
//VCLイベント
var
  re: TJRect;
  st: TJValue;
  param: TJValueList;
begin
  if not IsCallEvent('onProgress') then
    Exit;

  EnumToValue(TypeInfo(TProgressStage),st,Ord(Stage));

  re := TJRect.Create(FEngine);
  re.__Rect := R;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(st);
    param.Add(PercentDone);
    param.Add(RedrawNow);
    param.Add(re);
    param.Add(Msg);
    CallEvent('','onProgress',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseImage.RegistEvents;
var
  c: TImage;
begin
  c := GetImage;
  if not Assigned(c) then
    Exit;

  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDblClick := OnDblClick;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnProgress := OnProgress;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseImage.VCLClassType: TClass;
begin
  Result := TImage;
end;

{ TJVCLBaseTabControl }

constructor TJVCLBaseTabControl.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLTabControl');

  RegistMethod('indexOfTabAt',DoIndexOfTabAt);
  RegistMethod('getHitTestInfoAt',DoGetHitTestInfoAt);
  RegistMethod('tabRect',DoTabRect);
  RegistMethod('scrollTabs',DoScrollTabs);
end;

procedure TJVCLBaseTabControl.CreateObjects;
begin
  inherited;
  FTabs := TJBaseStringsObject.Create(FEngine,nil,False);
  FTabs.IncRef;
end;

destructor TJVCLBaseTabControl.Destroy;
begin
  //先に呼ぶ
  inherited;
  FTabs.Strings := nil;
  FTabs.DecRef;
end;

procedure TJVCLBaseTabControl.DestroyVCL;
begin
  inherited;
  FTabs.Strings := nil;
end;

function TJVCLBaseTabControl.DoGetHitTestInfoAt(
  Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
  test: THitTests;
begin
  CheckVCL(Param,2);

  x := Param[0];
  y := Param[1];
  try
    test := GetCustomTabControl.GetHitTestInfoAt(AsInteger(@x),AsInteger(@y));
    Result := BuildString(SetToStr(
      TypeInfo(THitTests),test));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTabControl.DoIndexOfTabAt(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
begin
  CheckVCL(Param,2);
  x := Param[0];
  y := Param[1];
  try
    Result := BuildInteger(
      GetCustomTabControl.IndexOfTabAt(AsInteger(@x),AsInteger(@y)));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTabControl.DoScrollTabs(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetCustomTabControl.ScrollTabs(AsInteger(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTabControl.DoTabRect(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
  rect: TJRect;
begin
  CheckVCL(Param,1);

  i := Param[0];
  try
    rect := TJRect.Create(FEngine);
    rect.__Rect := GetCustomTabControl.TabRect(AsInteger(@i));
    Result := BuildObject(rect);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTabControl.GetCustomTabControl: TCustomTabControl;
begin
  Result := FVCL as TCustomTabControl;
end;

function TJVCLBaseTabControl.GetDisplayRect: TJRect;
begin
  CheckVCL;
  Result := TJRect.Create(FEngine);
  Result.__Rect := GetTabControl.DisplayRect;
end;

function TJVCLBaseTabControl.GetRowCount: Integer;
begin
  CheckVCL;
  Result := GetCustomTabControl.RowCount
end;

function TJVCLBaseTabControl.GetTabControl: TTabControl;
begin
  Result := FVCL as TTabControl;
end;

function TJVCLBaseTabControl.GetTabs: TJBaseStringsObject;
begin
  CheckVCL;
  Result := FTabs;
  Ftabs.Strings := GetTabControl.Tabs
end;

procedure TJVCLBaseTabControl.OnChanging(Sender: TObject;
  var AllowChange: Boolean);
//イベント
var
  ac: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onChanging') then
    Exit;

  ac := TJBooleanObject.Create(FEngine);
  ac.bool := AllowChange;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ac);
    CallEvent('','onChanging',param);
  finally
    AllowChange := ac.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTabControl.OnDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseTabControl.OnGetImageIndex(Sender: TObject;
  TabIndex: Integer; var ImageIndex: Integer);
//イベント
var
  ii: TJNumberObject;
  param: TJValueList;
begin
  if not IsCallEvent('onGetImageIndex') then
    Exit;

  ii := TJNumberObject.Create(FEngine);
  ii.int := ImageIndex;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(TabIndex);
    param.Add(ii);
    CallEvent('','onGetImageIndex',param);
  finally
    ImageIndex := ii.int;
    param.Free;
  end;
end;

procedure TJVCLBaseTabControl.RegistEvents;
var
  c: TTabControl;
begin
  c := GetTabControl;
    if not Assigned(c) then
    Exit;

  c.OnChange := OnChange;
  c.OnChanging := OnChanging;
  c.OnContextPopup := OnContextPopup;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDrawTab := OnDrawTab;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetImageIndex := OnGetImageIndex;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

class function TJVCLBaseTabControl.VCLClassType: TClass;
begin
  Result := TTabControl;
end;

{ TJVCLBasePageControl }

function TJVCLBasePageControl.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and
            (Index < GetPageControl.PageCount);
end;

constructor TJVCLBasePageControl.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLPageControl');

  RegistMethod('findNextPage',DoFindNextPage);
  RegistMethod('selectNextPage',DoSelectNextPage);
  RegistMethod('pages',DoPages);
end;

function TJVCLBasePageControl.DoFindNextPage(Param: TJValueList): TJValue;
//VCLメソッド
var
  v,go,ck: TJValue;
  cur,nex: TTabSheet;
  tab: TJVCLBaseTabSheet;
begin
  CheckVCL(Param,3);
  v := Param[0];
  go := Param[1];
  ck := Param[2];

  if IsVCLObject(@v) and (v.vObject is TJVCLBaseTabSheet) then
    cur := (v.vObject as TJVCLBaseTabSheet).GetTabSheet
  else
    cur := nil;

  try
    nex := GetPageControl.FindNextPage(cur,AsBool(@go),AsBool(@ck));
    if Assigned(nex) then
    begin
      tab := TJVCLBaseTabSheet.Create(FEngine);
      tab.RegistVCL(nex,False);
      Result := BuildObject(tab);
    end
    else
      Result := BuildNull;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBasePageControl.DoPages(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  Result := GetItem(AsInteger(@v));
end;

function TJVCLBasePageControl.DoSelectNextPage(
  Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetPageControl.SelectNextPage(AsBool(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBasePageControl.GetActivePageIndex: Integer;
begin
  CheckVCL;
  Result := GetPageControl.ActivePageIndex;
end;

function TJVCLBasePageControl.GetCount: Integer;
begin
  Result := GetPageCount;
end;

function TJVCLBasePageControl.GetItem(Index: Integer): TJValue;
//VCLメソッド
var
  pg: TTabSheet;
  tab: TJVCLBaseTabSheet;
begin
  CheckVCL;
  if CheckRange(Index) then
  begin
    try
      pg := GetPageControl.Pages[Index];
      if Assigned(pg) then
      begin
        tab := TJVCLBaseTabSheet.Create(FEngine);
        tab.RegistVCL(pg,False);
        Result := BuildObject(tab);
      end
      else
        Result := BuildNull;
    except
      on E:Exception do
        Error(E.Message);
    end;
  end;
end;

function TJVCLBasePageControl.GetPageControl: TPageControl;
begin
  Result := FVCL as TPageControl;
end;

function TJVCLBasePageControl.GetPageCount: Integer;
begin
  CheckVCL;
  Result := GetPageControl.PageCount;
end;

class function TJVCLBasePageControl.IsArray: Boolean;
begin
  Result := True;
end;

procedure TJVCLBasePageControl.RegistEvents;
var
  c: TPageControl;
begin
  c := GetPageControl;
    if not Assigned(c) then
    Exit;

  c.OnChange := OnChange;
  c.OnChanging := OnChanging;
  c.OnContextPopup := OnContextPopup;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDrawTab := OnDrawTab;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetImageIndex := OnGetImageIndex;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

procedure TJVCLBasePageControl.SetActivePageIndex(const Value: Integer);
begin
  CheckVCL;
  GetPageControl.ActivePageIndex := Value;;
end;

class function TJVCLBasePageControl.VCLClassType: TClass;
begin
  Result := TPageControl;
end;

{ TJVCLBaseProgressBar }

constructor TJVCLBaseProgressBar.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLProgressBar');

  RegistMethod('stepIt',DoStepIt);
  RegistMethod('stepBy',DoStepBy);
end;

function TJVCLBaseProgressBar.DoStepBy(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetProgressBar.StepBy(AsInteger(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseProgressBar.DoStepIt(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetProgressBar.StepIt;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseProgressBar.GetProgressBar: TProgressBar;
begin
  Result := FVCL as TProgressBar;
end;

procedure TJVCLBaseProgressBar.RegistEvents;
var
  c: TProgressBar;
begin
  c := GetProgressBar;
  if not Assigned(c) then
    Exit;

  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseProgressBar.VCLClassType: TClass;
begin
  Result := TProgressBar;
end;

{ TJVCLBaseStatusBar }

constructor TJVCLBaseStatusBar.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLStatusBar');
end;

procedure TJVCLBaseStatusBar.CreateObjects;
begin
  inherited;
  FPanels := TJVCLBaseStatusPanels.Create(FEngine,nil,False);
  FPanels.IncRef;
end;

destructor TJVCLBaseStatusBar.Destroy;
begin
  //先に呼ぶ
  inherited;
  FPanels.DecRef;
end;

procedure TJVCLBaseStatusBar.DestroyVCL;
begin
  inherited;
  FPanels.RegistVCL(nil,False);
end;

function TJVCLBaseStatusBar.GetPanels: TJVCLBaseStatusPanels;
begin
  CheckVCL;
  Result := FPanels;
  FPanels.RegistVCL(GetStatusBar.Panels,False);
end;

function TJVCLBaseStatusBar.GetStatusBar: TStatusBar;
begin
  Result := FVCL as TStatusBar;
end;

procedure TJVCLBaseStatusBar.OnDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
//イベント
var
  param: TJValueList;
  pnl: TJVCLPersistent;
  rec: TJRect;
begin
  if not IsCallEvent('onDrawPanel') then
    Exit;

  pnl := TJVCLPersistent.Create(FEngine);
  pnl.RegistVCL(Panel,False);
  rec := TJRect.Create(FEngine);
  rec.__Rect := Rect;

  param := TJValueList.Create;
  try
    param.Add(GetSender(StatusBar));
    param.Add(pnl);
    param.Add(rec);
    CallEvent('','onDrawPanel',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseStatusBar.RegistEvents;
var
  c: TStatusBar;
begin
  c := GetStatusBar;
  if not Assigned(c) then
    Exit;

  //actionのため
  if not Assigned(c.Action) then
    c.OnClick := OnClick;

  c.OnContextPopup := OnContextPopup;
  c.OnDblClick := OnDblClick;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDrawPanel := OnDrawPanel;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnHint := OnHint;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseStatusBar.VCLClassType: TClass;
begin
  Result := TStatusBar;
end;

{ TJVCLBaseToolBar }

function TJVCLBaseToolBar.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetToolBar.ButtonCount);
end;

constructor TJVCLBaseToolBar.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLToolBar');

  RegistMethod('trackMenu',DoTrackMenu);
  RegistMethod('buttons',DoButtons);
end;

function TJVCLBaseToolBar.DoButtons(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  i := Param[0];
  Result := GetItem(AsInteger(@i));
end;

function TJVCLBaseToolBar.DoTrackMenu(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildBool(False);

  v := Param[0];
  try
    if IsObject(@v) and (v.vObject is TJVCLBaseToolButton) then
    begin
      Result := BuildBool(
        GetToolBar.TrackMenu((v.vObject as TJVCLBaseToolButton).GetToolButton));
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseToolBar.GetButtonCount: Integer;
begin
  CheckVCL;
  Result := GetToolBar.ButtonCount;
end;

function TJVCLBaseToolBar.GetCount: Integer;
begin
  Result := GetButtonCount;
end;

function TJVCLBaseToolBar.GetItem(Index: Integer): TJValue;
//VCLメソッド
var
  btn: TToolButton;
  vbtn: TJVCLBaseToolButton;
begin
  CheckVCL;
  Result := BuildNull;
  try
    if CheckRange(Index) then
    begin
      btn := GetToolBar.Buttons[Index];
      vbtn := TJVCLBaseToolButton.Create(FEngine);
      vbtn.RegistVCL(btn,False);
      Result := BuildObject(vbtn);
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseToolBar.GetRowCount: Integer;
begin
  CheckVCL;
  Result := GetToolBar.RowCount;
end;

function TJVCLBaseToolBar.GetToolBar: TToolBar;
begin
  Result := FVCL as TToolBar;
end;

class function TJVCLBaseToolBar.IsArray: Boolean;
begin
  Result := True;
end;

procedure TJVCLBaseToolBar.OnAdvancedCustomDraw(Sender: TToolBar;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseToolBar.OnAdvancedCustomDrawButton(Sender: TToolBar;
  Button: TToolButton; State: TCustomDrawState; Stage: TCustomDrawStage;
  var Flags: TTBCustomDrawFlags; var DefaultDraw: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseToolBar.OnCustomDraw(Sender: TToolBar;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseToolBar.OnCustomDrawButton(Sender: TToolBar;
  Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseToolBar.RegistEvents;
var
  c: TToolBar;
begin
  c := GetToolBar;
  if not Assigned(c) then
    Exit;

  c.OnAdvancedCustomDraw := OnAdvancedCustomDraw;
  c.OnAdvancedCustomDrawButton := OnAdvancedCustomDrawButton;
  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnCustomDraw := OnCustomDraw;
  c.OnCustomDrawButton := OnCustomDrawButton;
  c.OnDblClick := OnDblClick;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

class function TJVCLBaseToolBar.VCLClassType: TClass;
begin
  Result := TToolBar;
end;

{ TJVCLBaseMenuItem }

function TJVCLBaseMenuItem.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetMenuItem.Count);
end;

constructor TJVCLBaseMenuItem.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLMenuItem');

  RegistMethod('initiateAction',DoInitiateAction);
  RegistMethod('insert',DoInsert);
  RegistMethod('delete',DoDelete);
  RegistMethod('clear',DoClear);
  RegistMethod('click',DoClick);
  RegistMethod('find',DoFind);
  RegistMethod('indexOf',DoIndexOf);
  RegistMethod('isLine',DoIsLine);
  RegistMethod('getParentMenu',DoGetParentMenu);
  RegistMethod('newTopLine',DoNewTopLine);
  RegistMethod('newBottomLine',DoNewBottomLine);
  RegistMethod('insertNewLineBefore',DoInsertNewLineBefore);
  RegistMethod('insertNewLineAfter',DoInsertNewLineAfter);
  RegistMethod('add',DoAdd);
  RegistMethod('remove',DoRemove);
  RegistMethod('rethinkHotkeys',DoRethinkHotkeys);
  RegistMethod('rethinkLines',DoRethinkLines);
  RegistMethod('items',DoItems);
end;

function TJVCLBaseMenuItem.DoAdd(Param: TJValueList): TJValue;
//VCLメソッド
type
  TMenuItems = array of TMenuItem;
var
  v: TJValue;
  i,ad: Integer;
  items: TMenuItems;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);
  try
    ad := 0;
    for i := 0 to Param.Count - 1 do
    begin
      v := Param[i];
      if IsVCLObject(@v) and
        (v.vObject is TJVCLMenuItem) and
        (v.vObject as TJVCLMenuItem).IsVCL then
      begin
        Inc(ad);
        SetLength(items,ad);
        items[ad - 1] := (v.vObject as TJVCLMenuItem).GetMenuItem;
      end;
    end;

    if Length(items) > 0 then
      GetMenuItem.Add(items);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoClear(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  try
    GetMenuItem.Clear;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoClick(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  try
    GetMenuItem.Click;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoDelete(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    if CheckRange(AsInteger(@v)) then
      GetMenuItem.Delete(AsInteger(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoFind(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  itm: TMenuItem;
  vitm: TJVCLBaseMenuItem;
begin
  CheckVCL(Param,1);

  v := Param[0];
  try
    itm := GetMenuItem.Find(AsString(@v));
    if Assigned(itm) then
    begin
      vitm := TJVCLBaseMenuItem.Create(FEngine);
      vitm.RegistVCL(itm,False);
      Result := BuildObject(vitm);
    end
    else
      Result := BuildNull;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoGetParentMenu(Param: TJValueList): TJValue;
//VCLメソッド
var
  mn: TMenu;
  vmn: TJVCLBaseMenu;
begin
  CheckVCL;
  try
    mn := GetMenuItem.GetParentMenu;
    if Assigned(mn) then
    begin
      vmn := TJVCLBaseMenu.Create(FEngine);
      vmn.RegistVCL(mn,False);
      Result := BuildObject(vmn);
    end
    else
      Result := BuildNull;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoIndexOf(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildInteger(-1);

  v := Param[0];
  try
    if IsVCLObject(@v) and (v.vObject is TJVCLMenuItem) then
      Result := BuildInteger(
        GetMenuItem.IndexOf((v.vObject as TJVCLMenuItem).GetMenuItem));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoInitiateAction(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetMenuItem.InitiateAction;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoInsert(Param: TJValueList): TJValue;
//VCLメソッド
var
  idx,itm: TJValue;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  idx := Param[0];
  itm := Param[1];
  try
    if IsVCLObject(@itm) and
      (itm.vObject is TJVCLMenuItem) and
      (itm.vObject as TJVCLMenuItem).IsVCL then
    begin
      GetMenuItem.Insert(
        AsInteger(@idx),
        (itm.vObject as TJVCLMenuItem).GetMenuItem);
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoInsertNewLineAfter(
  Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildInteger(-1);

  v := Param[0];
  try
    if IsVCLObject(@v) and
      (v.vObject is TJVCLMenuItem) and
      (v.vObject as TJVCLMenuItem).IsVCL then
    begin
      Result := BuildInteger(
        GetMenuItem.InsertNewLineAfter(
          (v.vObject as TJVCLMenuItem).GetMenuItem));
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoInsertNewLineBefore(
  Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildInteger(-1);

  v := Param[0];
  try
    if IsVCLObject(@v) and
      (v.vObject is TJVCLMenuItem) and
      (v.vObject as TJVCLMenuItem).IsVCL then
    begin
      Result := BuildInteger(
        GetMenuItem.InsertNewLineBefore(
          (v.vObject as TJVCLMenuItem).GetMenuItem));
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoIsLine(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetMenuItem.IsLine);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoItems(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  Result := GetItem(AsInteger(@v));
end;

function TJVCLBaseMenuItem.DoNewBottomLine(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;

  try
    Result := BuildInteger(GetMenuItem.NewBottomLine);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoNewTopLine(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildInteger(GetMenuItem.NewTopLine);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoRemove(Param: TJValueList): TJValue;
//VCLメソッド
var
  itm: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  itm := Param[0];
  try
    if IsVCLObject(@itm) and
      (itm.vObject is TJVCLMenuItem) and
      (itm.vObject as TJVCLMenuItem).IsVCL then
    begin
      GetMenuItem.Remove(
        (itm.vObject as TJVCLMenuItem).GetMenuItem);
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoRethinkHotkeys(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;

  try
    Result := BuildBool(GetMenuItem.RethinkHotkeys);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.DoRethinkLines(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetMenuItem.RethinkLines);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseMenuItem.GetCommand: Integer;
begin
  CheckVCL;
  Result := GetMenuItem.Command
end;

function TJVCLBaseMenuItem.GetCount: Integer;
begin
  CheckVCL;
  Result := GetMenuItem.Count;
end;

function TJVCLBaseMenuItem.GetHandle: Integer;
begin
  CheckVCL;
  Result := GetMenuItem.Handle;
end;

function TJVCLBaseMenuItem.GetItem(Index: Integer): TJValue;
var
  itm: TMenuItem;
  vitem: TJVCLBaseMenuItem;
begin
  CheckVCL;
  if CheckRange(Index) then
  begin
    itm := GetMenuItem.Items[Index];
    if Assigned(itm) then
    begin
      vitem := TJVCLBaseMenuItem.Create(FEngine);
      vitem.RegistVCL(itm,False);
      Result := BuildObject(vitem);
    end
    else
      Result := BuildNull;
  end
  else
    ArgsError;
end;

function TJVCLBaseMenuItem.GetMenuIndex: Integer;
begin
  CheckVCL;
  Result := GetMenuItem.MenuIndex;
end;

function TJVCLBaseMenuItem.GetMenuItem: TMenuItem;
begin
  Result := FVCL as TMenuItem;
end;

function TJVCLBaseMenuItem.GetParent: TJVCLBaseMenuItem;
var
  itm: TMenuItem;
begin
  CheckVCL;
  itm := GetMenuItem.Parent;
  if Assigned(itm) then
  begin
    Result := TJVCLBaseMenuItem.Create(FEngine);
    Result.RegistVCL(itm,False);
  end
  else
    Result := nil;
end;

class function TJVCLBaseMenuItem.IsArray: Boolean;
begin
  Result := True;
end;

procedure TJVCLBaseMenuItem.RegistEvents;
var
  c: TMenuItem;
begin
  c := GetMenuItem;
  if not Assigned(c) then
    Exit;

  //actionのため
  if not Assigned(c.Action) then
    c.OnClick := OnClick;
end;

procedure TJVCLBaseMenuItem.SetMenuIndex(const Value: Integer);
begin
  CheckVCL;
  GetMenuItem.MenuIndex := Value;
end;

class function TJVCLBaseMenuItem.VCLClassType: TClass;
begin
  Result := TMenuItem;
end;


{ TJVCLBaseTabSheet }

constructor TJVCLBaseTabSheet.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLTabSheet');
end;

function TJVCLBaseTabSheet.GetPageControl: TJVCLBasePageControl;
begin
  CheckVCL;
  Result := TJVCLBasePageControl.Create(FEngine);
  Result.RegistVCL(GetTabSheet.PageControl,False);
end;

function TJVCLBaseTabSheet.GetTabIndex: Integer;
begin
  CheckVCL;
  Result := GetTabSheet.TabIndex;
end;

function TJVCLBaseTabSheet.GetTabSheet: TTabSheet;
begin
  Result := FVCL as TTabSheet;
end;

procedure TJVCLBaseTabSheet.RegistEvents;
var
  c: TTabSheet;
begin
  c := GetTabSheet;
  if not Assigned(c) then
    Exit;

  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnHide := OnHide;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnShow := OnShow;
  c.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseTabSheet.SetPageControl(
  const Value: TJVCLBasePageControl);
begin
  CheckVCL;
  if Assigned(Value) and Value.IsVCL then
    GetTabsheet.PageControl := Value.GetPageControl;
end;

class function TJVCLBaseTabSheet.VCLClassType: TClass;
begin
  Result := TTabSheet;
end;

{ TJVCLBaseToolButton }

constructor TJVCLBaseToolButton.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLToolButton');
end;

function TJVCLBaseToolButton.GetIndex: Integer;
begin
  CheckVCL;
  Result := GetToolButton.Index;
end;

function TJVCLBaseToolButton.GetToolButton: TToolButton;
begin
  Result := FVCL as TToolButton;
end;

procedure TJVCLBaseToolButton.RegistEvents;
var
  c: TToolButton;
begin
  c := GetToolButton;
  if not Assigned(c) then
    Exit;

  //actionのため
  if not Assigned(c.Action) then
    c.OnClick := OnClick;

  c.OnContextPopup := OnContextPopup;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseToolButton.VCLClassType: TClass;
begin
  Result := TToolButton;
end;

{ TJVCLToolWindow }

constructor TJVCLToolWindow.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLToolWindow');
end;

function TJVCLToolWindow.GetToolWindow: TToolWindow;
begin
  Result := FVCL as TToolWindow;
end;

procedure TJVCLToolWindow.RegistEvents;
var
  c: TToolWindow;
begin
  c := GetToolWindow;
  if not Assigned(c) then
    Exit;

  //c.On := On;
end;

class function TJVCLToolWindow.VCLClassType: TClass;
begin
  Result := TToolWindow;
end;

{ TJVCLBaseCoolBar }

constructor TJVCLBaseCoolBar.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('CoolBar')
end;

function TJVCLBaseCoolBar.GetCoolBar: TCoolBar;
begin
  Result := FVCL as TCoolBar;
end;

procedure TJVCLBaseCoolBar.RegistEvents;
var
  c: TCoolBar;
begin
  c := GetCoolBar;
  if not Assigned(c) then
    Exit;

  c.OnChange := OnChange;
  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnDblClick := OnDblClick;
  c.OnDockDrop := OnDockDrop;
  c.OnDockOver := OnDockOver;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnGetSiteInfo := OnGetSiteInfo;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
  c.OnUnDock := OnUnDock;
end;

class function TJVCLBaseCoolBar.VCLClassType: TClass;
begin
  Result := TCoolBar;
end;

{ TJVCLForm }

procedure TJVCLForm.CreateVCL;
begin
  RegistVCL(TForm.Create(nil),True);
end;

{ TJVCLEdit }

procedure TJVCLEdit.CreateVCL;
begin
  RegistVCL(TEdit.Create(nil),True);
end;

{ TJVCLButton }

procedure TJVCLButton.CreateVCL;
begin
  RegistVCL(TButton.Create(nil),True);
end;

{ TJVCLMemo }

procedure TJVCLMemo.CreateVCL;
begin
  RegistVCL(TMemo.Create(nil),True);
end;

{ TJVCLLabel }

procedure TJVCLLabel.CreateVCL;
begin
  RegistVCL(TLabel.Create(nil),True);
end;

{ TJVCLTimer }

procedure TJVCLTimer.CreateVCL;
begin
  RegistVCL(TTimer.Create(nil),True);
end;

{ TJVCLMenuItem }

procedure TJVCLMenuItem.CreateVCL;
begin
  RegistVCL(TMenuItem.Create(nil),True);
end;

{ TJVCLMenu }

procedure TJVCLMenu.CreateVCL;
begin
  RegistVCL(TMenu.Create(nil),True);
end;

{ TJVCLPopupMenu }

procedure TJVCLPopupMenu.CreateVCL;
begin
  RegistVCL(TPopupMenu.Create(nil),True);
end;

{ TJVCLCheckBox }

procedure TJVCLCheckBox.CreateVCL;
begin
  RegistVCL(TCheckBox.Create(nil),True);
end;

{ TJVCLRadioButton }

procedure TJVCLRadioButton.CreateVCL;
begin
  RegistVCL(TRadioButton.Create(nil),True);
end;

{ TJVCLListBox }

procedure TJVCLListBox.CreateVCL;
begin
  RegistVCL(TListBox.Create(nil),True);
end;

{ TJVCLComboBox }

procedure TJVCLComboBox.CreateVCL;
begin
  RegistVCL(TComboBox.Create(nil),True);
end;

{ TJVCLGroupBox }

procedure TJVCLGroupBox.CreateVCL;
begin
  RegistVCL(TGroupBox.Create(nil),True);
end;

{ TJVCLRadioGroup }

procedure TJVCLRadioGroup.CreateVCL;
begin
  RegistVCL(TRadioGroup.Create(nil),True);
end;

{ TJVCLPanel }

procedure TJVCLPanel.CreateVCL;
begin
  RegistVCL(TPanel.Create(nil),True);
end;

{ TJVCLSplitter }

procedure TJVCLSplitter.CreateVCL;
begin
  RegistVCL(TSplitter.Create(nil),True);
end;

{ TJVCLCheckListBox }

procedure TJVCLCheckListBox.CreateVCL;
begin
  RegistVCL(TCheckListBox.Create(nil),True);
end;

{ TJVCLImage }

procedure TJVCLImage.CreateVCL;
begin
  RegistVCL(TImage.Create(nil),True);
end;

{ TJVCLTabControl }

procedure TJVCLTabControl.CreateVCL;
begin
  RegistVCL(TTabControl.Create(nil),True);
end;

{ TJVCLTabSheet }

procedure TJVCLTabSheet.CreateVCL;
begin
  RegistVCL(TTabSheet.Create(nil),True);
end;

{ TJVCLPageControl }

procedure TJVCLPageControl.CreateVCL;
begin
  RegistVCL(TPageControl.Create(nil),True);
end;

{ TJVCLProgressBar }

procedure TJVCLProgressBar.CreateVCL;
begin
  RegistVCL(TProgressBar.Create(nil),True);
end;

{ TJVCLStatusBar }

procedure TJVCLStatusBar.CreateVCL;
begin
  RegistVCL(TStatusBar.Create(nil),True);
end;

{ TJVCLToolButton }

procedure TJVCLToolButton.CreateVCL;
begin
  RegistVCL(TToolButton.Create(nil),True);
end;

{ TJVCLToolBar }

procedure TJVCLToolBar.CreateVCL;
begin
  RegistVCL(TToolBar.Create(nil),True);
end;

{ TJVCLMainMenu }

procedure TJVCLMainMenu.CreateVCL;
begin
  RegistVCL(TMainMenu.Create(nil),True);
end;

{ TJVCLCoolBar }

procedure TJVCLCoolBar.CreateVCL;
begin
  RegistVCL(TCoolBar.Create(nil),True);
end;


{ TJVCLBaseSpinEdit }

constructor TJVCLBaseSpinEdit.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLSpinEdit');
end;

function TJVCLBaseSpinEdit.GetSpinEdit: TSpinEdit;
begin
  Result := FVCL as TSpinEdit;
end;

procedure TJVCLBaseSpinEdit.RegistEvents;
var
  edit: TSpinEdit;
begin
  edit := GetSpinEdit;
  if not Assigned(edit) then
    Exit;

  edit.OnChange := OnChange;
  edit.OnClick := OnClick;
  edit.OnDblClick := OnDblClick;
  edit.OnDragDrop := OnDragDrop;
  edit.OnDragOver := OnDragOver;
  edit.OnEndDrag := OnEndDrag;
  edit.OnEnter := OnEnter;
  edit.OnExit := OnExit;
  edit.OnKeyDown := OnKeyDown;
  edit.OnKeyPress := OnKeyPress;
  edit.OnKeyUp := OnKeyUp;
  edit.OnMouseDown := OnMouseDown;
  edit.OnMouseMove := OnMouseMove;
  edit.OnMouseUp := OnMouseUp;
  edit.OnStartDrag := OnStartDrag;
end;

class function TJVCLBaseSpinEdit.VCLClassType: TClass;
begin
  Result := TSpinEdit;
end;

{ TJVCLSpinEdit }

procedure TJVCLSpinEdit.CreateVCL;
begin
  RegistVCL(TSpinEdit.Create(nil),True);
end;

{ TJVCLBaseUpDown }

constructor TJVCLBaseUpDown.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLUpDown');
end;

function TJVCLBaseUpDown.GetUpDown: TUpDown;
begin
  Result := FVCL as TUpDown;
end;

procedure TJVCLBaseUpDown.OnChanging(Sender: TObject;
  var AllowChange: Boolean);
//VCLイベント
var
  param: TJValueList;
  ac: TJBooleanObject;
begin
  if not IsCallEvent('onChanging') then
    Exit;

  ac := TJBooleanObject.Create(FEngine);
  ac.bool := AllowChange;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ac);
    CallEvent('','onChanging',param);
  finally
    AllowChange := ac.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseUpDown.OnChangingEx(Sender: TObject;
{$if CompilerVersion >= 26.0}
  var AllowChange: Boolean; NewValue: Integer;
{$else}
  var AllowChange: Boolean; NewValue: Smallint;
{$endif}
  Direction: TUpDownDirection);
//VCLイベント
var
  enum: String;
  param: TJValueList;
  ac: TJBooleanObject;
begin
  if not IsCallEvent('onChangingEx') then
    Exit;

  ac := TJBooleanObject.Create(FEngine);
  ac.bool := AllowChange;

  enum := GetEnumName(TypeInfo(TUpDownDirection),Ord(Direction));

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(ac);
    param.Add(NewValue);
    param.Add(enum);
    CallEvent('','onChangingEx',param);
  finally
    AllowChange := ac.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseUpDown.OnClick(Sender: TObject; Button: TUDBtnType);
//VCLイベント
var
  param: TJValueList;
  enum: String;
begin
  if not IsCallEvent('onClick') then
    Exit;

  enum := GetEnumName(TypeInfo(TUDBtnType),Ord(Button));

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(enum);
    CallEvent('','onClick',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseUpDown.RegistEvents;
//イベント登録
var
  c: TUpDown;
begin
  c := GetUpDown;
  if not Assigned(c) then
    Exit;

  c.OnChanging := OnChanging;
  c.OnChangingEx := OnChangingEx;
  c.OnClick := OnClick;
  c.OnContextPopup := OnContextPopup;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
end;

class function TJVCLBaseUpDown.VCLClassType: TClass;
begin
  Result := TUpDown;
end;

{ TJVCLUpDown }

procedure TJVCLUpDown.CreateVCL;
begin
  RegistVCL(TUpDown.Create(nil),True);
end;


{ TJVCLBaseStatusPanels }

function TJVCLBaseStatusPanels.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetStatusPanels.Count);
end;

constructor TJVCLBaseStatusPanels.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLStatusPanels');
  RegistMethod('add',DoAdd);
  RegistMethod('items',DoItems);
end;

function TJVCLBaseStatusPanels.DoAdd(Param: TJValueList): TJValue;
var
  pnl: TStatusPanel;
  vpnl: TJVCLPersistent;
begin
  CheckVCL;
  pnl := GetStatusPanels.Add;
  vpnl := TJVCLPersistent.Create(FEngine);
  vpnl.RegistVCL(pnl,False);
  Result := BuildObject(vpnl);
end;

function TJVCLBaseStatusPanels.DoItems(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  i := Param[0];

  if IsParam2(Param) then
  begin
    Result := Param[1];
    Result := Items(AsInteger(@i),@Result)
  end
  else
    Result := Items(AsInteger(@i));
end;

function TJVCLBaseStatusPanels.GetCount: Integer;
begin
  CheckVCL;
  Result := GetStatusPanels.Count;
end;

function TJVCLBaseStatusPanels.GetItem(Index: Integer): TJValue;
begin
  CheckVCL;
  Result := Items(Index);
end;

function TJVCLBaseStatusPanels.GetStatusPanels: TStatusPanels;
begin
  Result := FVCL as TStatusPanels;
end;

function TJVCLBaseStatusPanels.GetValue(S: String; ArrayStyle: Boolean;
  Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Result := Items(AsInteger(@v))
  else
    Result := inherited GetValue(S,ArrayStyle,Param);
end;

class function TJVCLBaseStatusPanels.IsArray: Boolean;
begin
  Result := True;
end;

function TJVCLBaseStatusPanels.Items(Index: Integer;
  SetValue: PJValue): TJValue;
var
  p: TPersistent;
  pnl: TStatusPanel;
  vpnl: TJVCLPersistent;
begin
  CheckVCL;
  Result := BuildNull;
  try
    if CheckRange(Index) then
    begin
      if Assigned(SetValue) then
      begin
        //setter
        Result := SetValue^;
        if IsVCLObject(@Result) then
        begin
          p := (Result.vObject as TJVCLPersistent).GetVCL;
          if p is TStatusPanel then
            GetStatusPanels.Items[Index] := (p as TStatusPanel);
        end
        else
          ArgsError;
      end
      else begin
        //getter
        pnl := GetStatusPanels.Items[Index];
        vpnl := TJVCLPersistent.Create(FEngine);
        vpnl.RegistVCL(pnl,False);
        Result := BuildObject(vpnl);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

procedure TJVCLBaseStatusPanels.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList);
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Items(AsInteger(@v),@Value)
  else
    inherited;
end;

class function TJVCLBaseStatusPanels.VCLClassType: TClass;
begin
  Result := TStatusPanels;
end;

{ TJVCLBaseAction }


constructor TJVCLBaseAction.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLAction');
  RegistMethod('execute',DoExecute);
  RegistMethod('update',DoUpdate);
end;

function TJVCLBaseAction.DoExecute(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(
      GetBasicAction.Execute);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseAction.DoUpdate(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(
      GetBasicAction.Update);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseAction.GetBasicAction: TBasicAction;
begin
  Result := FVCL as TBasicAction;
end;

procedure TJVCLBaseAction.OnHint(var HintStr: String;
  var CanShow: Boolean);
//VCLイベント
var
  s: TJStringObject;
  cs: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onHint') then
    Exit;

  s := TJStringObject.Create(FEngine);
  s.str := HintStr;
  cs := TJBooleanObject.Create(FEngine);
  cs.bool := CanShow;

  param := TJValueList.Create;
  try
    param.Add(s);
    param.Add(cs);
    CallEvent('','onHint',param);
  finally
    HintStr := s.str;
    CanSHow := cs.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseAction.RegistEvents;
//イベント登録
var
  c: TBasicAction;
begin
  c := GetBasicAction;
  if not Assigned(c) then
    Exit;

  c.OnExecute := OnExecute;
  c.OnUpdate := OnUpdate;
end;

class function TJVCLBaseAction.VCLClassType: TClass;
begin
  Result := TBasicAction;
end;

{ TJVCLAction }

procedure TJVCLAction.CreateVCL;
begin
  RegistVCL(TAction.Create(nil),True);
end;

function TJVCLAction.GetAction: TAction;
begin
  Result := FVCL as TAction;
end;

procedure TJVCLAction.RegistEvents;
//イベント登録
var
  c: TAction;
begin
  c := GetAction;
  if not Assigned(c) then
    Exit;

  c.OnExecute := OnExecute;
  c.OnUpdate := OnUpdate;
  c.OnHint := OnHint;
end;

class function TJVCLAction.VCLClassType: TClass;
begin
  Result := TAction;
end;

{ TJVCLBaseCommonDialog }

constructor TJVCLBaseCommonDialog.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLDiaog');
  RegistMethod('execute',DoExecute);
end;

function TJVCLBaseCommonDialog.DoExecute(Param: TJValueList): TJValue;
begin
  //何も無し
  EmptyValue(Result);
end;

function TJVCLBaseCommonDialog.GetCommonDialog: TCommonDialog;
begin
  Result := FVCL as TCommonDialog;
end;

procedure TJVCLBaseCommonDialog.RegistEvents;
//イベント登録
var
  c: TCommonDialog;
begin
  c := GetCommonDialog;
  if not Assigned(c) then
    Exit;

  c.OnClose := OnClose;
  c.OnShow := OnShow;
end;

class function TJVCLBaseCommonDialog.VCLClassType: TClass;
begin
  Result := TCommonDialog;
end;

{ TJVCLBaseOpenDialog }

constructor TJVCLBaseOpenDialog.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLOpenDialog');
end;

function TJVCLBaseOpenDialog.DoExecute(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetOpenDialog.Execute);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseOpenDialog.GetOpenDialog: TOpenDialog;
begin
  Result := FVCL as TOpenDialog;
end;

procedure TJVCLBaseOpenDialog.OnCanClose(Sender: TObject;
  var CanClose: Boolean);
//VCLイベント
var
  cc: TJBooleanObject;
  param: TJValueList;
begin
  if not IsCallEvent('onCanClose') then
    Exit;

  cc := TJBooleanObject.Create(FEngine);
  cc.bool := CanClose;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));

    param.Add(cc);
    CallEvent('','onCanClose',param);
  finally
    CanClose := cc.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseOpenDialog.OnIncludeItem(const OFN: TOFNotifyEx;
  var Include: Boolean);
begin
{ TODO : 保留 }
end;

procedure TJVCLBaseOpenDialog.RegistEvents;
//イベント登録
var
  c: TOpenDialog;
begin
  c := GetOpenDialog;
  if not Assigned(c) then
    Exit;

  c.OnClose := OnClose;
  c.OnShow := OnShow;
  c.OnCanClose := OnCanClose;
  c.OnFolderChange := OnFolderChange;
  c.OnIncludeItem := OnIncludeItem;
  c.OnSelectionChange := OnSelectionChange;
  c.OnTypeChange := OnTypeChange;
end;

class function TJVCLBaseOpenDialog.VCLClassType: TClass;
begin
  Result := TOpenDialog;
end;

{ TJVCLOpenDialog }

procedure TJVCLOpenDialog.CreateVCL;
begin
  RegistVCL(TOpenDialog.Create(nil),True);
end;

{ TJVCLBaseSaveDialog }

constructor TJVCLBaseSaveDialog.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLSaveDialog');
end;

function TJVCLBaseSaveDialog.GetSaveDialog: TSaveDialog;
begin
  Result := FVCL as TSaveDialog;
end;

class function TJVCLBaseSaveDialog.VCLClassType: TClass;
begin
  Result := TSaveDialog;
end;

{ TJVCLSaveDialog }

procedure TJVCLSaveDialog.CreateVCL;
begin
  RegistVCL(TSaveDialog.Create(nil),True);
end;

{ TJVCLBaseFontDialog }

constructor TJVCLBaseFontDialog.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLFontDialog');
end;

function TJVCLBaseFontDialog.DoExecute(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetFontDialog.Execute);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseFontDialog.GetFontDialog: TFontDialog;
begin
  Result := FVCL as TFontDialog;
end;

procedure TJVCLBaseFontDialog.OnApply(Sender: TObject; Wnd: HWND);
//VCLイベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onApply') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(Wnd);
    CallEvent('','onApply',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseFontDialog.RegistEvents;
var
  c: TFontDialog;
begin
  c := GetFontDialog;
  if not Assigned(c) then
    Exit;

  c.OnClose := OnClose;
  c.OnShow := OnShow;
  c.OnApply := OnApply;
end;

class function TJVCLBaseFontDialog.VCLClassType: TClass;
begin
  Result := TFontDialog;
end;

{ TJVCLFontDialog }

procedure TJVCLFontDialog.CreateVCL;
begin
  RegistVCL(TFontDialog.Create(nil),True);
end;

{ TJVCLBaseFindDialog }

constructor TJVCLBaseFindDialog.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLFindDialog');
  RegistMethod('closeDialog',DoCloseDialog);
end;

function TJVCLBaseFindDialog.DoCloseDialog(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetFindDialog.CloseDialog;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseFindDialog.DoExecute(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetFindDialog.Execute);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseFindDialog.GetFindDialog: TFindDialog;
begin
  Result := FVCL as TFindDialog;
end;

procedure TJVCLBaseFindDialog.RegistEvents;
var
  c: TFindDialog;
begin
  c := GetFindDialog;
  if not Assigned(c) then
    Exit;

  c.OnClose := OnClose;
  c.OnShow := OnShow;
  c.OnFind := OnFind;
end;

class function TJVCLBaseFindDialog.VCLClassType: TClass;
begin
  Result := TFindDialog;
end;

{ TJVCLFindDialog }

procedure TJVCLFindDialog.CreateVCL;
begin
  RegistVCL(TFindDialog.Create(nil),True);
end;

{ TJVCLBaseReplaceDialog }

constructor TJVCLBaseReplaceDialog.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLReplaceDialog');
end;

function TJVCLBaseReplaceDialog.GetReplaceDialog: TReplaceDialog;
begin
  Result := FVCL as TReplaceDialog;
end;

procedure TJVCLBaseReplaceDialog.RegistEvents;
var
  c: TReplaceDialog;
begin
  c := GetReplaceDialog;
  if not Assigned(c) then
    Exit;

  c.OnClose := OnClose;
  c.OnShow := OnShow;
  c.OnFind := OnFind;
  c.OnReplace := OnReplace;
end;

class function TJVCLBaseReplaceDialog.VCLClassType: TClass;
begin
  Result := TReplaceDialog;
end;

{ TJVCLReplaceDialog }

procedure TJVCLReplaceDialog.CreateVCL;
begin
  RegistVCL(TReplaceDialog.Create(nil),True);
end;


{ TJVCLBaseListItem }

function ListItemToVCL(ListItem: TListItem; ListView: TJVCLBaseListView;
  Engine: TJBaseEngine): TJVCLBaseListItem;
begin
  if Assigned(ListItem) then
  begin
    if Assigned(ListItem.Data) then
      Result := TObject(ListItem.Data) as TJVCLBaseListItem
    else begin
      Result := TJVCLBaseListItem.Create(Engine);
      Result.RegistVCL(ListItem,False);
      Result.FListView := ListView;
    end;
  end
  else
    Result := nil;
end;

function VCLToListItem(var Value: TJValue; var ListItem: TListItem): Boolean;
begin
  Result := False;
  ListItem := nil;
  if IsNull(@Value) then
    Result := True
  else if IsObject(@Value) and (Value.vObject is TJVCLBaseListItem) then
  begin
    Result := True;
    ListItem := (Value.vObject as TJVCLBaseListItem).GetListItem;
  end;
end;


constructor TJVCLBaseListItem.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLListItem');
end;

function TJVCLBaseListItem.DoCancelEdit(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetListItem.CancelEdit;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.DoDelete(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  EmptyValue(Result);
  try
    GetListItem.Delete;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.DoDisplayRect(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  code: Integer;
  rec: TJRect;
begin
  CheckVCL(Param,1);
  Result := BuildNull;

  v := Param[0];
  if ValueToEnum(TypeInfo(TDisplayCode),v,code) then
  begin
    try
      rec := TJRect.Create(FEngine);
      rec.__Rect := GetListItem.DisplayRect(TDisplayCode(code));
      Result := BuildObject(rec);
    except
      on E:Exception do
        Error(E.Message);
    end;
  end;
end;

function TJVCLBaseListItem.DoEditCaption(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(
      GetListItem.EditCaption);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.DoMakeVisible(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetListItem.MakeVisible(AsBool(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.DoSubItemImages(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildInteger(-1);

  try
    i := Param[0];
    if CheckSubItemsRange(AsInteger(@i)) then
    begin
      if IsParam2(Param) then
      begin
        //setter
        Result := Param[1];
        GetListItem.SubItemImages[AsInteger(@i)] := AsInteger(@Result);
      end
      else begin
        //getter
        Result := BuildInteger(
          GetListItem.SubItemImages[AsInteger(@i)]);
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.DoUpdate(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetListItem.Update;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.DoWorkArea(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildInteger(GetListItem.WorkArea);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItem.GetCaption: string;
begin
  CheckVCL;
  Result := GetListItem.Caption;
end;

function TJVCLBaseListItem.GetChecked: Boolean;
begin
  CheckVCL;
  Result := GetListItem.Checked;
end;

function TJVCLBaseListItem.GetCut: Boolean;
begin
  CheckVCL;
  Result := GetListItem.Cut;
end;

function TJVCLBaseListItem.GetData: TJObject;
begin
  Result := FData;
end;

function TJVCLBaseListItem.GetDropTarget: Boolean;
begin
  CheckVCL;
  Result := GetListItem.DropTarget;
end;

function TJVCLBaseListItem.GetFocused: Boolean;
begin
  CheckVCL;
  Result := GetListItem.Focused;
end;

function TJVCLBaseListItem.GetHandle: Integer;
begin
  CheckVCL;
  Result := GetListItem.Handle;
end;

function TJVCLBaseListItem.GetImageIndex: Integer;
begin
  CheckVCL;
  Result := GetListItem.ImageIndex;
end;

function TJVCLBaseListItem.GetIndent: Integer;
begin
  CheckVCL;
  Result := GetListItem.Indent;
end;

function TJVCLBaseListItem.GetIndex: Integer;
begin
  CheckVCL;
  Result := GetListItem.Index;
end;

function TJVCLBaseListItem.GetLeft: Integer;
begin
  CheckVCL;
  Result := GetListItem.Left;
end;

function TJVCLBaseListItem.GetListItem: TListItem;
begin
  Result := FVCL as TListItem;
end;

function TJVCLBaseListItem.GetOverlayIndex: Integer;
begin
  CheckVCL;
  Result := GetListItem.OverlayIndex;
end;

function TJVCLBaseListItem.GetPosition: TJObject;
var
  po: TJPoint;
begin
  CheckVCL;
  po := TJPoint.Create(FEngine);
  po.__Point := GetLisTItem.GetPosition;
  Result := po;
end;

function TJVCLBaseListItem.GetSelected: Boolean;
begin
  CheckVCL;
  Result := GetListItem.Selected;
end;

function TJVCLBaseListItem.GetStateIndex: Integer;
begin
  CheckVCL;
  Result := GetListItem.StateIndex;
end;

function TJVCLBaseListItem.GetSubItems: TJBaseStringsObject;
begin
  CheckVCL;
  Result := FSubItems;
end;

function TJVCLBaseListItem.GetTop: Integer;
begin
  CheckVCL;
  Result := GetListItem.Top;
end;

procedure TJVCLBaseListItem.SetCaption(const Value: string);
begin
  CheckVCL;
  GetListItem.Caption := Value;
end;

procedure TJVCLBaseListItem.SetChecked(const Value: Boolean);
begin
  CheckVCL;
  GetListItem.Checked := Value;
end;

procedure TJVCLBaseListItem.SetCut(const Value: Boolean);
begin
  CheckVCL;
  GetListItem.Cut := Value;
end;

procedure TJVCLBaseListItem.SetData(const Value: TJObject);
begin
  //参照カウントを増やす
  if Assigned(Value) then
    Value.IncRef;

  if Assigned(FData) then
  begin
    //通知を取り除く
    FData.RemoveFreeNotification(Self);
    RemoveFreeNotification(FData);
    //参照カウントを減らす
    FData.DecRef;
  end;
  //入れ替え
  FData := Value;
  //通知を仕掛ける
  if Assigned(FData) then
    FData.FreeNotification(Self);
end;

procedure TJVCLBaseListItem.SetDropTarget(const Value: Boolean);
begin
  CheckVCL;
  GetListItem.DropTarget := Value;
end;

procedure TJVCLBaseListItem.SetFocused(const Value: Boolean);
begin
  CheckVCL;
  GetListItem.Focused := Value;
end;

procedure TJVCLBaseListItem.SetImageIndex(const Value: Integer);
begin
  CheckVCL;
  GetListItem.ImageIndex := Value;
end;

procedure TJVCLBaseListItem.SetIndent(const Value: Integer);
begin
  CheckVCL;
  GetListItem.Indent := Value;
end;

procedure TJVCLBaseListItem.SetLeft(const Value: Integer);
begin
  CheckVCL;
  GetListItem.Left := Value;
end;

procedure TJVCLBaseListItem.SetOverlayIndex(const Value: Integer);
begin
  CheckVCL;
  GetListItem.OverlayIndex := Value;
end;

procedure TJVCLBaseListItem.SetPosition(const Value: TJObject);
var
  po: TPoint;
  v: TJValue;
begin
  CheckVCL;
  if not Assigned(Value) then
    Error;

  v := Value.GetValue('x',False);
  po.x := AsInteger(@v);
  v := Value.GetValue('y',False);
  po.y := AsInteger(@v);

  GetListItem.Position := po;
end;

procedure TJVCLBaseListItem.SetSelected(const Value: Boolean);
begin
  CheckVCL;
  GetListItem.Selected := Value;
end;

procedure TJVCLBaseListItem.SetStateIndex(const Value: Integer);
begin
  CheckVCL;
  GetListItem.StateIndex := Value;
end;

procedure TJVCLBaseListItem.SetTop(const Value: Integer);
begin
  CheckVCL;
  GetListItem.Top := Value;
end;

class function TJVCLBaseListItem.VCLClassType: TClass;
begin
  Result := TListItem;
end;

destructor TJVCLBaseListItem.Destroy;
begin
  inherited;
  FSubItems.Strings := nil;
  FSubItems.DecRef;
end;

procedure TJVCLBaseListItem.DestroyVCL;
begin
  inherited;
  FListView := nil;
  FSubItems.Strings := nil;
  //Dataを削除
  SetData(nil);
end;

procedure TJVCLBaseListItem.CreateObjects;
begin
  inherited;
  FSubItems := TJBaseStringsObject.Create(FEngine,nil,False);
  FSubItems.IncRef;
end;

function TJVCLBaseListItem.RegistVCL(AVCL: TPersistent;
  ACanDestroy: Boolean): Boolean;
//SubItemsを登録
begin
  Result := inherited RegistVCL(AVCL,ACanDestroy);
  if Result then
    FSubItems.Strings := GetListItem.SubItems
  else
    FSubItems.Strings := nil;
end;

function TJVCLBaseListItem.GetListItems: TJVCLBaseListItems;
begin
  CheckVCL;
  Result := FListView.FListItems;
end;

function TJVCLBaseListItem.GetListView: TJVCLBaseListView;
begin
  CheckVCL;
  Result := FListView;
end;

procedure TJVCLBaseListItem.Notification(AObject: TJNotify);
//終了通知
begin
  inherited;
  if AObject = FData then
    FData := nil
  else if AObject = FListView then
    FListView := nil
end;

function TJVCLBaseListItem.CheckSubItemsRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetListItem.SubItems.Count);
end;

procedure TJVCLBaseListItem.CheckVCL(Param: TJValueList;
  ArgCount: Integer);
begin
  inherited;
  if not Assigned(FListView) then
    Error('vcl is null');
end;

procedure TJVCLBaseListItem.RegistMethods;
begin
  inherited;
  RegistMethod('cancelEdit',DoCancelEdit);
  RegistMethod('delete',DoDelete);
  RegistMethod('displayRect',DoDisplayRect);
  RegistMethod('editCaption',DoEditCaption);
  RegistMethod('makeVisible',DoMakeVisible);
  RegistMethod('update',DoUpdate);
  RegistMethod('workArea',DoWorkArea);
  RegistMethod('subItemImages',DoSubItemImages);
end;

{ TJVCLBaseListItems }

function TJVCLBaseListItems.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetListItems.Count);
end;

procedure TJVCLBaseListItems.CheckVCL(Param: TJValueList;
  ArgCount: Integer);
begin
  inherited;
  if not Assigned(FListView) then
    Error('vcl is null');
end;

constructor TJVCLBaseListItems.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLListItems');
end;

function TJVCLBaseListItems.DoAdd(Param: TJValueList): TJValue;
//VCLメソッド
var
  item: TListItem;
begin
  CheckVCL;
  try
    item := GetListItems.Add;
    Result := BuildObject(
      ListItemToVCL(item,FListView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItems.DoBeginUpdate(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetListItems.BeginUpdate;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItems.DoClear(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetListItems.Clear;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItems.DoDelete(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetListItems.Delete(AsInteger(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItems.DoEndUpdate(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetListItems.EndUpdate;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItems.DoIndexOf(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  item: TListItem;
begin
  CheckVCL(Param,1);
  v := Param[0];
  if VCLToListItem(v,item) then
  begin
    try
      Result := BuildInteger(GetListItems.IndexOf(item));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseListItems.DoInsert(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  item: TListItem;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    item := GetListItems.Insert(AsInteger(@v));
    Result := BuildObject(ListItemToVCL(item,FListView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListItems.DoItem(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  i := Param[0];
  if IsParam2(Param) then
  begin
    Result := Param[1];
    Result := Item(AsINteger(@i),@Result);
  end
  else
    Result := Item(AsInteger(@i));
end;

function TJVCLBaseListItems.GetCount: Integer;
begin
  CheckVCL;
  Result := GetListItems.Count;
end;

function TJVCLBaseListItems.GetHandle: Integer;
begin
  CheckVCL;
  Result := GetListItems.Handle;
end;

function TJVCLBaseListItems.GetItem(Index: Integer): TJValue;
begin
  Result := Item(Index);
end;

function TJVCLBaseListItems.GetListItems: TListItems;
begin
  Result := FVCL as TListItems;
end;

function TJVCLBaseListItems.GetValue(S: String; ArrayStyle: Boolean;
  Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Result := Item(AsInteger(@v))
  else
    Result := inherited GetValue(S,ArrayStyle,Param);
end;

function TJVCLBaseListItems.Get_Owner: TJVCLBaseListView;
begin
  CheckVCL;
  Result := FListView;
end;

class function TJVCLBaseListItems.IsArray: Boolean;
begin
  Result := True;
end;

function TJVCLBaseListItems.Item(Index: Integer;
  SetValue: PJValue): TJValue;
//デフォルトプロパティ
//VCLメソッド
var
  item: TListItem;
  data: Pointer;
begin
  CheckVCL;
  Result := BuildNull;
  try
    if CheckRange(Index) then
    begin
      if Assigned(SetValue) then
      begin
        //setter
        Result := SetValue^;
        if VCLToListItem(Result,item) then
        begin
          //DataにはTJVCLBaseListItemが入ってるので上書きしないよう保存
          data := GetListItems.Item[Index].Data;
          //assign
          GetListItems.Item[Index] := item;
          //Dataを戻す
          GetListItems.Item[Index].Data := data;
          //TJVCLBAseListItem.Dataを移す
          ListItemToVCL(
            GetListItems.Item[Index],
            FListView,FEngine).Data :=
              ListItemToVCL(item,FListView,FEngine).Data;
        end
        else
          ArgsError;
      end
      else begin
        //getter
        Result := BuildObject(
          ListItemToVCL(GetListItems.Item[Index],
            FListView,FEngine));
      end;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

procedure TJVCLBaseListItems.RegistMethods;
begin
  inherited;
  RegistMethod('add',DoAdd);
  RegistMethod('beginUpdate',DoBeginUpdate);
  RegistMethod('clear',DoClear);
  RegistMethod('delete',DoDelete);
  RegistMethod('endUpdate',DoEndUpdate);
  RegistMethod('indexOf',DoIndexOf);
  RegistMethod('insert',DoInsert);
  RegistMethod('item',DoItem);
end;

procedure TJVCLBaseListItems.SetCount(const Value: Integer);
begin
  CheckVCL;
  GetListItems.Count := Value;
end;

procedure TJVCLBaseListItems.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList);
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Item(AsInteger(@v),@Value)
  else
    inherited;
end;

class function TJVCLBaseListItems.VCLClassType: TClass;
begin
  Result := TListItems;
end;

{ TJVCLBaseListColumns }

function TJVCLBaseListColumns.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetListColumns.Count);
end;

constructor TJVCLBaseListColumns.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLListColumns');
end;

function TJVCLBaseListColumns.DoAdd(Param: TJValueList): TJValue;
//VCLメソッド
var
  p: TJVCLPersistent;
begin
  CheckVCL;
  try
    p := TJVCLPersistent.Create(FEngine);
    p.RegistVCL(GetListColumns.Add,False);
    Result := BuildObject(p);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListColumns.DoItems(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  Result := GetItem(AsInteger(@v));
end;

function TJVCLBaseListColumns.GetCount: Integer;
begin
  CheckVCL;
  Result := GetListColumns.Count;
end;

function TJVCLBaseListColumns.GetItem(Index: Integer): TJValue;
var
  p: TJVCLPersistent;
begin
  CheckVCL;
  if CheckRange(Index) then
  begin
    p := TJVCLPersistent.Create(FEngine);
    p.RegistVCL(GetListColumns.Items[Index],False);
    Result := BuildObject(p);
  end
  else
    ArgsError;
end;

function TJVCLBaseListColumns.GetListColumns: TListColumns;
begin
  Result := FVCL as TListColumns
end;

class function TJVCLBaseListColumns.IsArray: Boolean;
begin
  Result := True;
end;

procedure TJVCLBaseListColumns.RegistMethods;
begin
  inherited;
  RegistMethod('items',DoItems);
  RegistMethod('add',DoAdd);
end;

class function TJVCLBaseListColumns.VCLClassType: TClass;
begin
  Result := TListColumns;
end;

{ TJVCLBaseListView }

constructor TJVCLBaseListView.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLListView');
end;

procedure TJVCLBaseListView.CreateObjects;
begin
  inherited;
  FListItems := TJVCLBaseListItems.Create(FEngine,nil,False);
  FListItems.IncRef;
  FListItems.FListView := Self;
  FColumns := TJVCLBaseListColumns.Create(FEngine,nil,False);
  FColumns.IncRef;
end;

function TJVCLBaseListView.RegistVCL(AVCL: TPersistent;
  ACanDestroy: Boolean): Boolean;
begin
  Result := inherited RegistVCL(AVCL,ACanDestroy);
  if Result then
  begin
    //ListItemsをセット
    FListItems.RegistVCL(GetListView.Items,False);
    FListItems.FListView := Self;
    FColumns.RegistVCL(GetListView.Columns,False);
  end;
end;

destructor TJVCLBaseListView.Destroy;
begin
  inherited;
  FListItems.FListView := nil;
  FListItems.DecRef;
  FColumns.DecRef;
end;

procedure TJVCLBaseListView.DestroyVCL;
begin
  inherited;
  FListItems.RegistVCL(nil,False);
  FColumns.RegistVCL(nil,False);
end;

procedure TJVCLBaseListView.Notification(AObject: TJNotify);
//終了通知を受ける
var
  item: TJVCLBaseListItem;
begin
  inherited;
  //アイテムの終了を受ける
  if AObject is TJVCLBaseListItem then
  begin
    item := AObject as TJVCLBaseListItem;
    //アイテムがあればDataを無効にする
    if item.IsVCL then
      item.GetListItem.Data := nil;
  end;
end;

function TJVCLBaseListView.InitializeItem(Item: TListItem): TListItem;
//ListItemを初期化
var
  vitem: TJVCLBaseListItem;
begin
  Result := Item;
  //virtual modeの場合は関係ない
  if (not GetListView.OwnerData) and
      Assigned(Item) and (Item.Data = nil) then
  begin
    vitem := TJVCLBaseListItem.Create(FEngine);
    vitem.RegistVCL(Item,False);
    vitem.IncRef;
    vitem.FListView := Self;
    //終了通知
    vitem.FreeNotification(Self);
    //Dataに入れる
    Item.Data := vitem;
  end;
end;

procedure TJVCLBaseListView.OnInsert(Sender: TObject; Item: TListItem);
//挿入イベント
var
  param: TJValueList;
begin
  //初期化
  Item := InitializeItem(Item);
  //イベント
  if IsCallEvent('onInsert') then
  begin
    param := TJValueList.Create;
    try
      param.Add(GetSender(Sender));
      param.Add(ListItemToVCL(Item,Self,FEngine));
      CallEvent('','onInsert',param);
    finally
      param.Free;
    end;
  end;
end;

procedure TJVCLBaseListView.OnDeletion(Sender: TObject; Item: TListItem);
//削除イベント
var
  param: TJValueList;
  vitem: TJVCLBaseListItem;
begin
  if IsCallEvent('onDeletion') then
  begin
    param := TJValueList.Create;
    try
      param.Add(GetSender(Sender));
      param.Add(ListItemToVCL(Item,Self,FEngine));
      CallEvent('','onDeletion',param);
    finally
      param.Free;
    end;
  end;

  //Dataに入れたオブジェクトを削除
  if (not GetListView.OwnerData) and
     Assigned(Item) and (TObject(Item.Data) is TJVCLBaseListItem) then
  begin
    vitem := TObject(Item.Data) as TJVCLBaseListItem;
    vitem.FListView := nil;
    //終了通知を削除
    vitem.RemoveFreeNotification(Self);
    RemoveFreeNotification(vitem);
    vitem.RegistVCL(nil,False);
    //終わり
    vitem.DecRef;
    Item.Data := nil;
  end;
end;

function TJVCLBaseListView.DoAlphaSort(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetListView.AlphaSort);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoArrange(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  enum: Integer;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    if ValueToEnum(TypeInfo(TListArrangement),v,enum) then
      GetListView.Arrange(TListArrangement(enum))
    else
      ArgsError;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

(*
function TJVCLBaseListView.DoCustomSort(Param: TJValueList): TJValue;
begin
{ TODO : 保留 }
  EmptyValue(Result);
end;
*)

function TJVCLBaseListView.DoFindCaption(Param: TJValueList): TJValue;
//VCLメソッド
var
  st,v,pa,incl,wr: TJValue;
begin
  CheckVCL(Param,5);
  Result := BuildNull;

  st := Param[0];
  v := Param[1];
  pa := Param[2];
  incl := Param[3];
  wr := Param[4];

  try
    Result := BuildObject(ListItemToVCL(
      GetListView.FindCaption(
        AsInteger(@st),AsString(@v),AsBool(@pa),
        AsBool(@incl),AsBool(@wr)),Self,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoFindData(Param: TJValueList): TJValue;
begin
{ TODO : 保留 }
  EmptyValue(Result);
end;

function TJVCLBaseListView.DoGetHitTestInfoAt(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
  ht: THitTests;
begin
  CheckVCL(Param,2);
  x := Param[0];
  y := Param[1];
  try
    ht := GetListView.GetHitTestInfoAt(AsInteger(@x),AsInteger(@y));
    Result := BuildString(
      SetToStr(TypeInfo(THitTests),ht));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoGetItemAt(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
begin
  CheckVCL(Param,2);
  x := Param[0];
  y := Param[1];
  try
    Result := BuildObject(
      ListItemToVCL(
        GetListView.GetItemAt(AsInteger(@x),AsInteger(@y)),
          Self,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoGetNearestItem(Param: TJValueList): TJValue;
//VCLメソッド
var
  v,x,y: TJValue;
  po: TPoint;
  enum: Integer;
begin
  CheckVCL(Param,2);
  Result := BuildNull;

  v := Param[0];
  if IsObject(@v) then
  begin
    x := v.vObject.GetValue('x',False);
    po.x := AsInteger(@x);
    y := v.vObject.GetValue('y',False);
    po.y := AsInteger(@y);

    v := Param[1];
    if ValueToEnum(TypeInfo(TSearchDirection),v,enum) then
    begin
      try
        Result := BuildObject(
          ListItemToVCL(
            GetListView.GetNearestItem(po,TSearchDirection(enum)),
              Self,FEngine));
      except
        on E:Exception do
          Error(E.Message);
      end;
    end
    else
      ArgsError;
  end
  else
    ArgsError;
end;

function TJVCLBaseListView.DoGetNextItem(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  dir: Integer;
  stt: TItemStates;
  item: TListItem;
begin
  CheckVCL(Param,3);
  Result :=  BuildNull;

  v := Param[0];
  if IsObject(@v) and (v.vObject is TJVCLBaseListItem) then
  begin
    item := (v.vObject as TJVCLBaseListItem).GetListItem;

    v := Param[1];
    if ValueToEnum(TypeInfo(TSearchDirection),v,dir) then
    begin
      v := Param[2];
      StrToSet(TypeInfo(TItemStates),AsString(@v),stt);

      try
        Result := BuildObject(
          ListItemToVCL(
            GetListView.GetNextItem(
              item,TSearchDirection(dir),stt),Self,FEngine));
      except
        on E:Exception do
          Error(E.Message);
      end;
    end
    else
      ArgsError;
  end
  else
    ArgsError;
end;

function TJVCLBaseListView.DoGetSearchString(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildString(GetListView.GetSearchString);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoIsEditing(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetListView.IsEditing);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoScroll(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  x := Param[0];
  y := Param[1];
  try
    GetListView.Scroll(AsInteger(@x),AsInteger(@y));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoStringWidth(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    Result := BuildInteger(
      GetListView.StringWidth(AsString(@v)));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.DoUpdateItems(Param: TJValueList): TJValue;
//VCLメソッド
var
  f,l: TJValue;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  f := Param[0];
  l := Param[1];
  try
    GetListView.UpdateItems(AsInteger(@f),AsInteger(@l));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseListView.GetBoundingRect: TJRect;
begin
  CheckVCL;
  Result := TJRect.Create(FEngine);
  Result.__Rect := GetListView.BoundingRect;
end;

function TJVCLBaseListView.GetCustomListView: TCustomListView;
begin
  Result := FVCL as TCustomListView;
end;

function TJVCLBaseListView.GetDropTarget: TJVCLBaseListItem;
begin
  CheckVCL;
  Result := ListItemToVCL(GetListView.DropTarget,Self,FEngine);
end;

function TJVCLBaseListView.GetFocused: TJVCLBaseListItem;
begin
  CheckVCL;
  Result := ListItemToVCL(GetListView.ItemFocused,Self,FEngine);
end;

function TJVCLBaseListView.GetItemIndex: Integer;
begin
  CheckVCL;
  Result := GetCustomListView.ItemIndex;
end;

function TJVCLBaseListView.GetListView: TListView;
begin
  Result := FVCL as TListView;
end;

function TJVCLBaseListView.GetSelCount: Integer;
begin
  CheckVCL;
  Result := GetListView.SelCount;
end;

function TJVCLBaseListView.GetSelection: TJVCLBaseListItem;
begin
  CheckVCL;
  Result := ListItemToVCL(GetListView.Selected,Self,FEngine);
end;

function TJVCLBaseListView.GetTopItem: TJVCLBaseListItem;
begin
  CheckVCL;
  Result := ListItemToVCL(GetListView.TopItem,Self,FEngine);
end;

function TJVCLBaseListView.GetViewOrigin: TJPoint;
begin
  CheckVCL;
  Result := TJPoint.Create(FEngine);
  Result.__Point := GetListView.ViewOrigin;
end;

function TJVCLBaseListView.GetVisibleRowCount: Integer;
begin
  CheckVCL;
  Result := GetListView.VisibleRowCount;
end;

procedure TJVCLBaseListView.OnAdvancedCustomDraw(Sender: TCustomListView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
//イベント
var
  st: TJValue;
  param: TJValueList;
  rec: TJRect;
  dd: TJBooleanObject;
begin
  if not IsCallEvent('onAdvancedCustomDraw') then
    Exit;

  rec := TJRect.Create(FEngine);
  EnumToValue(TypeInfo(TCustomDrawStage),st,Ord(Stage));
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(rec);
    param.Add(st);
    param.Add(dd);
    CallEvent('','onAdvancedCustomDraw',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
//イベント
var
  stg: TJValue;
  param: TJValueList;
  dd: TJBooleanObject;
  stt: String;
begin
  if not IsCallEvent('onAdvancedCustomDrawItem') then
    Exit;

  stt := SetToStr(TypeInfo(TCustomDrawState),State);
  EnumToValue(TypeInfo(TCustomDrawStage),stg,Ord(Stage));
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(stt);
    param.Add(stg);
    param.Add(dd);
    CallEvent('','onAdvancedCustomDrawItem',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
//イベント
var
  stg: TJValue;
  param: TJValueList;
  dd: TJBooleanObject;
  stt: String;
begin
  if not IsCallEvent('onAdvancedCustomDrawSubItem') then
    Exit;

  stt := SetToStr(TypeInfo(TCustomDrawState),State);
  EnumToValue(TypeInfo(TCustomDrawStage),stg,Ord(Stage));
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(SubItem);
    param.Add(stt);
    param.Add(stg);
    param.Add(dd);
    CallEvent('','onAdvancedCustomDrawSubItem',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
//イベント
var
  cha: TJValue;
  param: TJValueList;
begin
  if not IsCallEvent('onChange') then
    Exit;

  EnumToValue(TypeInfo(TItemChange),cha,Ord(Change));

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(cha);
    CallEvent('','onChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnChanging(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
//イベント
var
  cha: TJValue;
  param: TJValueList;
  aa: TJBooleanObject;
begin
  if not IsCallEvent('onChanging') then
    Exit;

  EnumToValue(TypeInfo(TItemChange),cha,Ord(Change));
  aa := TJBooleanObject.Create(FEngine);
  aa.bool := AllowChange;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(cha);
    param.Add(aa);
    CallEvent('','onChanging',param);
  finally
    AllowChange := aa.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnColumnClick(Sender: TObject;
  Column: TListColumn);
//イベント
var
  param: TJValueList;
  col: TJVCLPersistent;
begin
  if not IsCallEvent('onColumnClick') then
    Exit;

  col := TJVCLPersistent.Create(FEngine);
  col.RegistVCL(Column,False);

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(col);
    CallEvent('','onColumnClick',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnColumnDragged(Sender: TObject);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onColumnDragged') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    CallEvent('','onColumnDragged',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnColumnRightClick(Sender: TObject;
  Column: TListColumn; Point: TPoint);
//イベント
var
  param: TJValueList;
  po: TJPoint;
  col: TJVCLPersistent;
begin
  if not IsCallEvent('onColumnRightClick') then
    Exit;

  col := TJVCLPersistent.Create(FEngine);
  col.RegistVCL(Column,False);

  po := TJPoint.Create(FEngine);
  po.__Point := Point;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(col);
    param.Add(po);
    CallEvent('','onColumnRightClick',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
//イベント
var
  param: TJValueList;
  com: TJNumberObject;
begin
  if not IsCallEvent('onCompare') then
    Exit;

  com := TJNumberObject.Create(FEngine);
  com.int := Compare;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item1,Self,FEngine));
    param.Add(ListItemToVCL(Item2,Self,FEngine));
    param.Add(Data);
    param.Add(com);
    CallEvent('','onCompare',param);
  finally
    Compare := com.int;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
//イベント
var
  param: TJValueList;
  rec: TJRect;
  dd: TJBooleanObject;
begin
  if not IsCallEvent('onCustomDraw') then
    Exit;

  rec := TJRect.Create(FEngine);
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(rec);
    param.Add(dd);
    CallEvent('','onCustomDraw',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
//イベント
var
  param: TJValueList;
  dd: TJBooleanObject;
  stt: String;
begin
  if not IsCallEvent('onCustomDrawItem') then
    Exit;

  stt := SetToStr(TypeInfo(TCustomDrawState),State);
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(stt);
    param.Add(dd);
    CallEvent('','onCustomDrawItem',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
//イベント
var
  param: TJValueList;
  dd: TJBooleanObject;
  stt: String;
begin
  if not IsCallEvent('onCustomDrawSubItem') then
    Exit;

  stt := SetToStr(TypeInfo(TCustomDrawState),State);
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(SubItem);
    param.Add(stt);
    param.Add(dd);
    CallEvent('','onCustomDrawSubItem',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnData(Sender: TObject; Item: TListItem);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onData') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    CallEvent('','onData',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnDataFind(Sender: TObject; Find: TItemFind;
  const FindString: String; const FindPosition: TPoint; FindData: Pointer;
  StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
  var Index: Integer);
//イベント
var
  fnd,dir: TJValue;
  param: TJValueList;
  fp: TJPoint;
  i: TJNumberObject;
begin
  if not IsCallEvent('onDataFind') then
    Exit;

  EnumToValue(TypeInfo(TItemFind),fnd,Ord(Find));
  fp := TJPoint.Create(FEngine);
  fp.__Point := FindPosition;
  EnumToValue(TypeInfo(TsearchDirection),dir,Ord(Direction));
  i := TJNumberObject.Create(FEngine);
  i.int := Index;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(fnd);
    param.Add(FindString);
    param.Add(fp);
    param.Add(Integer(FindData));
    param.Add(StartIndex);
    param.Add(dir);
    param.Add(Wrap);
    param.Add(i);
    CallEvent('','onDataFind',param);
  finally
    Index := i.int;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnDataHint(Sender: TObject; StartIndex,
  EndIndex: Integer);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onDataHint') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(StartIndex);
    param.Add(EndIndex);
    CallEvent('','onDataHint',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnDataStateChange(Sender: TObject; StartIndex,
  EndIndex: Integer; OldState, NewState: TItemStates);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onDataStateChange') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(StartIndex);
    param.Add(EndIndex);
    param.Add(SetToStr(TypeInfo(TItemState),OldState));
    param.Add(SetToStr(TypeInfo(TItemState),NewState));
    CallEvent('','onDataStateChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
//イベント
var
  param: TJValueList;
  rec: TJRect;
begin
  if not IsCallEvent('onDrawItem') then
    Exit;

  rec := TJRect.Create(Fengine);
  rec.__Rect := Rect;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(rec);
    param.Add(SetToStr(TypeInfo(TOwnerdrawState),State));
    CallEvent('','onDrawItem',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnEdited(Sender: TObject; Item: TListItem;
  var S: String);
//イベント
var
  param: TJValueList;
  str: TJStringObject;
begin
  if not IsCallEvent('onEdited') then
    Exit;

  str := TJStringObject.Create(Fengine);
  str.str := S;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(str);
    CallEvent('','onEdited',param);
  finally
    S := str.str;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnEditing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
//イベント
var
  param: TJValueList;
  al: TJBooleanObject;
begin
  if not IsCallEvent('onEditing') then
    Exit;

  al := TJBooleanObject.Create(Fengine);
  al.bool := AllowEdit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(al);
    CallEvent('','onEditing',param);
  finally
    AllowEdit := al.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnGetImageIndex(Sender: TObject;
  Item: TListItem);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onGetImageIndex') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    CallEvent('','onGetImageIndex',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnGetSubItemImage(Sender: TObject;
  Item: TListItem; SubItem: Integer; var ImageIndex: Integer);
//イベント
var
  param: TJValueList;
  ii: TJNumberObject;
begin
  if not IsCallEvent('onGetSubItemImage') then
    Exit;

  ii := TJNumberObject.Create(FEngine);
  ii.int := ImageIndex;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(SubItem);
    param.Add(ii);
    CallEvent('','onGetSubItemImage',param);
  finally
    ImageIndex := ii.int;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnInfoTip(Sender: TObject; Item: TListItem;
  var InfoTip: String);
//イベント
var
  param: TJValueList;
  tip: TJStringObject;
begin
  if not IsCallEvent('onInfoTip') then
    Exit;

  tip := TJStringObject.Create(FEngine);
  tip.str := InfoTip;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(tip);
    CallEvent('','onInfoTip',param);
  finally
    InfoTip := tip.str;
    param.Free;
  end;
end;

procedure TJVCLBaseListView.OnSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onSelectItem') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(ListItemToVCL(Item,Self,FEngine));
    param.Add(Selected);
    CallEvent('','onSelectItem',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseListView.RegistEvents;
//イベント登録
var
  c: TListView;
begin
  c := GetListView;
  if not Assigned(c) then
    Exit;

  c.OnAdvancedCustomDraw := OnAdvancedCustomDraw;
  c.OnAdvancedCustomDrawItem := OnAdvancedCustomDrawItem;
  c.OnAdvancedCustomDrawSubItem := OnAdvancedCustomDrawSubItem;
  c.OnChange := OnChange;
  c.OnChanging := OnChanging;
  c.OnClick := OnClick;
  c.OnColumnClick := OnColumnClick;
  c.OnColumnDragged := OnColumnDragged;
  c.OnColumnRightClick := OnColumnRightClick;
  c.OnCompare := OnCompare;
  c.OnContextPopup := OnContextPopup;
  c.OnCustomDraw := OnCustomDraw;
  c.OnCustomDrawItem := OnCustomDrawItem;
  c.OnCustomDrawSubItem := OnCustomDrawSubItem;
  c.OnData := OnData;
  c.OnDataFind := OnDataFind;
  c.OnDataHint := OnDataHint;
  c.OnDataStateChange := OnDataStateChange;
  c.OnDblClick := OnDblClick;
  c.OnDeletion := OnDeletion;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnDrawItem := OnDrawItem;
  c.OnEdited := OnEdited;
  c.OnEditing := OnEditing;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnGetImageIndex := OnGetImageIndex;
  c.OnGetSubItemImage := OnGetSubItemImage;
  c.OnInfoTip := OnInfoTip;
  c.OnInsert := OnInsert;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnResize := OnResize;
  c.OnSelectItem := OnSelectItem;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseListView.SetDropTarget(const Value: TJVCLBaseListItem);
begin
  CheckVCL;
  if Assigned(Value) then
    GetListView.DropTarget := Value.GetListItem
  else
    GetListView.DropTarget := nil;
end;

procedure TJVCLBaseListView.SetFocused(const Value: TJVCLBaseListItem);
begin
  CheckVCL;
  if Assigned(Value) then
    GetListView.ItemFocused := Value.GetListItem
  else
    GetListView.ItemFocused := nil;
end;

procedure TJVCLBaseListView.SetItemIndex(const Value: Integer);
begin
  CheckVCL;
  GetCustomListView.ItemIndex := Value;
end;

procedure TJVCLBaseListView.SetSelection(const Value: TJVCLBaseListItem);
begin
  CheckVCL;
  if Assigned(Value) then
    GetListView.Selected :=Value.GetListItem
  else
    GetListView.Selected := nil;
end;

class function TJVCLBaseListView.VCLClassType: TClass;
begin
  Result := TListView;
end;

function TJVCLBaseListView.GetColumns: TJVCLBaseListColumns;
begin
  CheckVCL;
  Result := FColumns;
end;

function TJVCLBaseListView.GetListItems: TJVCLBaseListItems;
begin
  CheckVCL;
  Result := FListItems;
end;

procedure TJVCLBaseListView.RegistMethods;
begin
  inherited;
  RegistMethod('alphaSort',DoAlphaSort);
  RegistMethod('arrange',DoArrange);
  RegistMethod('findCaption',DoFindCaption);
  RegistMethod('findData',DoFindData);
  RegistMethod('getHitTestInfoAt',DoGetHitTestInfoAt);
  RegistMethod('getItemAt',DoGetItemAt);
  RegistMethod('getNearestItem',DoGetNearestItem);
  RegistMethod('getNextItem',DoGetNextItem);
  RegistMethod('getSearchString',DoGetSearchString);
  RegistMethod('isEditing',DoIsEditing);
  RegistMethod('scroll',DoScroll);
  //RegistMethod('customSort',DoCustomSort);
  RegistMethod('stringWidth',DoStringWidth);
  RegistMethod('updateItems',DoUpdateItems);
end;



{ TJVCLListView }

procedure TJVCLListView.CreateVCL;
begin
  RegistVCL(TListView.Create(nil),True);
end;

{ TJVCLBaseTreeNode }

function TreeNodeToVCL(TreeNode: TTreeNode; TreeView: TJVCLBaseTreeView;
  Engine: TJBaseEngine): TJVCLBaseTreeNode;
begin
  if Assigned(TreeNode) then
  begin
    if Assigned(TreeNode.Data) then
      Result := TObject(TreeNode.Data) as TJVCLBaseTreeNode
    else begin
      //Result := TJVCLBaseTreeNode.Create(Engine);
      //Result.RegistVCL(TreeNode,False);
      //Result.FTreeView := TreeView;
{ TODO : TreeViewには仮想モードがないのでたぶん大丈夫 }
      Result := TObject(TreeView.InitializeNode(TreeNode).Data) as TJVCLBaseTreeNode;
    end;
  end
  else
    Result := nil;
end;

function VCLToTreeNode(var Value: TJValue; var TreeNode: TTreeNode): Boolean;
begin
  Result := False;
  TreeNode := nil;
  if IsNull(@Value) then
    Result := True
  else if IsObject(@Value) and (Value.vObject is TJVCLBaseTreeNode) then
  begin
    Result := True;
    TreeNode := (Value.vObject as TJVCLBaseTreeNode).GetTreeNode;
  end;
end;

constructor TJVCLBaseTreeNode.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLTreeNode');
end;

function TJVCLBaseTreeNode.DoAlphaSort(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetTreeNode.AlphaSort);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoCollapse(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetTreeNode.Collapse(AsBool(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoDelete(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  EmptyValue(Result);
  try
    GetTreeNode.Delete;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoDeleteChildren(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetTreeNode.DeleteChildren;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoDisplayRect(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  rec: TJRect;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    rec := TJRect.Create(FEngine);
    rec.__Rect := GetTreeNode.DisplayRect(AsBool(@v));
    Result := BuildObject(rec);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoEditText(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetTreeNode.EditText);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoEndEdit(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetTreeNode.EndEdit(AsBool(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoExpand(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetTreeNode.Expand(AsBool(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DogetFirstChild(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getFirstChild,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetLastChild(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getLastChild,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetNext(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getNext,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetNextChild(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,1);
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    try
      Result := BuildObject(
        TreeNodeToVCL(
          GetTreeNode.getNextChild(node),
          FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNode.DogetNextSibling(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getNextSibling,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetNextVisible(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getNextVisible,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetPrev(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getPrev,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetPrevChild(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,1);
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    try
      Result := BuildObject(
        TreeNodeToVCL(
          GetTreeNode.getPrevChild(node),
          FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNode.DogetPrevSibling(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getPrevSibling,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoGetPrevVisible(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeNode.getPrevVisible,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoHasAsParent(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,1);
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    try
      Result := BuildBool(GetTreeNode.HasAsParent(node));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNode.DoIndexOf(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,1);
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    try
      Result := BuildInteger(GetTreeNode.IndexOf(node));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNode.DoItem(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  i := Param[0];
  if IsParam2(Param) then
  begin
    Result := Param[1];
    Result := Item(AsInteger(@i),@Result);
  end
  else
    Result := Item(AsInteger(@i));
end;

function TJVCLBaseTreeNode.DoMakeVisible(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);
  try
    GetTreeNode.MakeVisible;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNode.DoMoveTo(Param: TJValueList): TJValue;
//VCLメソッド
//MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode)
var
  v: TJValue;
  node: TTreeNode;
  mode: Integer;
begin
  CheckVCL(Param,2);
  Result := BuildObject(Self);

  v := Param[0];
  if VCLtoTreeNode(v,node) then
  begin
    v := Param[1];
    if ValueToEnum(TypeInfo(TNodeAttachMode),v,mode) then
    begin
      try
        GetTreeNode.MoveTo(node,TNodeAttachMode(mode));
      except
        on E:Exception do
          Error(E.Message);
      end;
    end
    else
      ArgsError;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNode.GetAbsoluteIndex: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.AbsoluteIndex;
end;

function TJVCLBaseTreeNode.GetHasChildren: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.HasChildren;
end;

function TJVCLBaseTreeNode.GetCount: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.Count;
end;

function TJVCLBaseTreeNode.GetCut: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.Cut;
end;

function TJVCLBaseTreeNode.GetData: TJObject;
begin
  Result := FData;
end;

function TJVCLBaseTreeNode.GetDeleting: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.Deleting;
end;

function TJVCLBaseTreeNode.GetDropTarget: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.DropTarget;
end;

function TJVCLBaseTreeNode.GetExpanded: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.Expanded;
end;

function TJVCLBaseTreeNode.GetFocused: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.Focused;
end;

function TJVCLBaseTreeNode.GetHandle: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.Handle;
end;

function TJVCLBaseTreeNode.GetImageIndex: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.ImageIndex;
end;

function TJVCLBaseTreeNode.GetIndex: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.Index;
end;

function TJVCLBaseTreeNode.GetIsVisible: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.IsVisible;
end;

function TJVCLBaseTreeNode.GetItemId: Integer;
begin
  CheckVCL;
  Result := Integer(GetTreeNode.itemId);
end;

function TJVCLBaseTreeNode.GetLevel: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.Level;
end;

function TJVCLBaseTreeNode.GetOverlayIndex: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.OverlayIndex;
end;

function TJVCLBaseTreeNode.Get_Owner: TJVCLBaseTreeNodes;
begin
  CheckVCL;
  Result := FTreeView.FTreeNodes;
end;

function TJVCLBaseTreeNode.GetParent: TJVCLBaseTreeNode;
begin
  CheckVCL;
  Result := TreeNodeToVCL(GetTreeNode.Parent,FTreeView,FEngine);
end;

function TJVCLBaseTreeNode.GetSelected: Boolean;
begin
  CheckVCL;
  Result := GetTreeNode.Selected;
end;

function TJVCLBaseTreeNode.GetSelectedIndex: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.SelectedIndex;
end;

function TJVCLBaseTreeNode.GetStateIndex: Integer;
begin
  CheckVCL;
  Result := GetTreeNode.StateIndex;
end;

function TJVCLBaseTreeNode.GetText: string;
begin
  CheckVCL;
  Result := GetTreeNode.Text;
end;

function TJVCLBaseTreeNode.GetTreeNode: TTreeNode;
begin
  Result := FVCL as TTreeNode;
end;

function TJVCLBaseTreeNode.GetTreeView: TJVCLBaseTreeView;
begin
  CheckVCL;
  Result := FTreeView;
end;

procedure TJVCLBaseTreeNode.SetHasChildren(const Value: Boolean);
begin
  CheckVCL;
  GetTreeNode.HasChildren := Value;
end;

procedure TJVCLBaseTreeNode.SetCut(const Value: Boolean);
begin
  CheckVCL;
  GetTreeNode.Cut := Value;
end;

procedure TJVCLBaseTreeNode.SetData(const Value: TJObject);
//Dataをセット
begin
  //参照カウントを増やす
  if Assigned(Value) then
    Value.IncRef;

  if Assigned(FData) then
  begin
    //終了通知を削除
    FData.RemoveFreeNotification(Self);
    RemoveFreeNotification(FData);
    //参照カウントを減らす
    FData.DecRef;
  end;

  //入れ替え
  FData := Value;
  //終了通知をセット
  if Assigned(FData) then
    FData.FreeNotification(Self);
end;

procedure TJVCLBaseTreeNode.SetDropTarget(const Value: Boolean);
begin
  CheckVCL;
  GetTreeNode.DropTarget := Value;
end;

procedure TJVCLBaseTreeNode.SetExpanded(const Value: Boolean);
begin
  CheckVCL;
  GetTreeNode.Expanded := Value;
end;

procedure TJVCLBaseTreeNode.SetFocused(const Value: Boolean);
begin
  CheckVCL;
  GetTreeNode.Focused := Value;
end;

procedure TJVCLBaseTreeNode.SetImageIndex(const Value: Integer);
begin
  CheckVCL;
  GetTreeNode.ImageIndex := Value;
end;

procedure TJVCLBaseTreeNode.SetOverlayIndex(const Value: Integer);
begin
  CheckVCL;
  GetTreeNode.OverlayIndex := Value;
end;

procedure TJVCLBaseTreeNode.SetSelected(const Value: Boolean);
begin
  CheckVCL;
  GetTreeNode.Selected := Value;
end;

procedure TJVCLBaseTreeNode.SetSelectedIndex(const Value: Integer);
begin
  CheckVCL;
  GetTreeNode.SelectedIndex := Value;
end;

procedure TJVCLBaseTreeNode.SetStateIndex(const Value: Integer);
begin
  CheckVCL;
  GetTreeNode.StateIndex := Value;
end;

procedure TJVCLBaseTreeNode.SetText(const Value: string);
begin
  CheckVCL;
  GetTreeNode.Text := Value;
end;

class function TJVCLBaseTreeNode.VCLClassType: TClass;
begin
  Result := TTreeNode;
end;

procedure TJVCLBaseTreeNode.RegistMethods;
begin
  inherited;
  RegistMethod('alphaSort',DoAlphaSort);
  RegistMethod('collapse',DoCollapse);
  //RegistMethod('customSort',DoCustomSort);
  RegistMethod('delete',DoDelete);
  RegistMethod('deleteChildren',DoDeleteChildren);
  RegistMethod('displayRect',DoDisplayRect);
  RegistMethod('editText',DoEditText);
  RegistMethod('endEdit',DoEndEdit);
  RegistMethod('expand',DoExpand);
  RegistMethod('getFirstChild',DogetFirstChild);
  RegistMethod('getLastChild',DoGetLastChild);
  RegistMethod('getNext',DoGetNext);
  RegistMethod('getNextChild',DoGetNextChild);
  RegistMethod('getNextSibling',DogetNextSibling);
  RegistMethod('getNextVisible',DoGetNextVisible);
  RegistMethod('getPrev',DoGetPrev);
  RegistMethod('getPrevChild',DoGetPrevChild);
  RegistMethod('getPrevSibling',DogetPrevSibling);
  RegistMethod('getPrevVisible',DoGetPrevVisible);
  RegistMethod('hasAsParent',DoHasAsParent);
  RegistMethod('indexOf',DoIndexOf);
  RegistMethod('makeVisible',DoMakeVisible);
  RegistMethod('moveTo',DoMoveTo);
  RegistMethod('item',DoItem);
end;

procedure TJVCLBaseTreeNode.Notification(AObject: TJNotify);
begin
  inherited;
  if AObject = FData then
    FData := nil
  else if AObject = FTreeView then
    FTreeView := nil;
end;

function TJVCLBaseTreeNode.Item(Index: Integer;
  SetValue: PJValue): TJValue;
//デフォルトプロパティ
var
  node: TTreeNode;
  data: Pointer;
begin
  CheckVCL;
  Result := BuildNull;
  if CheckRange(Index) then
  begin
    try
      if Assigned(SetValue) then
      begin
        //setter
        Result := SetValue^;
        if VCLToTreeNode(Result,node) then
        begin
          //TreeNode.DataにTJVCLBaseTreeNodeが入ってるので保存
          data := GetTreeNode.Item[Index].Data;
          //assign
          GetTreeNode.Item[Index] := node;
          //Dataを戻す
          GetTreeNode.Item[Index].Data := data;
          //TJVCLBaseTreeNode.Dataを移す
          TreeNodeToVCL(
            GetTreeNode[Index],
            FTreeView,FEngine).Data :=
              TreeNodeToVCL(node,FTreeView,FEngine).Data;
        end
        else
          ArgsError;
      end
      else begin
        //getter
        Result := BuildObject(
          TreeNodeToVCL(GetTreeNode[Index],
            FTreeView,FEngine));
      end;
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

procedure TJVCLBaseTreeNode.DestroyVCL;
begin
  inherited;
  FTreeView := nil;
  SetData(nil);
end;

procedure TJVCLBaseTreeNode.CheckVCL(Param: TJValueList;
  ArgCount: Integer);
begin
  inherited;
  if not Assigned(FTreeView) then
    Error('vcl is null');
end;

function TJVCLBaseTreeNode.GetValue(S: String; ArrayStyle: Boolean;
  Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Result := Item(AsInteger(@v))
  else
    Result := inherited GetValue(S,ArrayStyle,Param);
end;

procedure TJVCLBaseTreeNode.SetValue(S: String; Value: TJValue;
  ArrayStyle: Boolean; Param: TJValueList);
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Item(AsInteger(@v),@Value)
  else
    inherited;
end;

function TJVCLBaseTreeNode.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetTreeNode.Count);
end;

function TJVCLBaseTreeNode.GetItem(Index: Integer): TJValue;
begin
  Result := Item(Index);
end;

class function TJVCLBaseTreeNode.IsArray: Boolean;
begin
  Result := True;
end;

{ TJVCLBaseTreeNodes }

constructor TJVCLBaseTreeNodes.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLTreeNodes');
end;

function TJVCLBaseTreeNodes.DoAdd(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,2);
  Result := BuildNull;
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    v := Param[1];
    try
      Result := BuildObject(
        TreeNodeToVCL(
          InitializeNode(GetTreeNodes.Add(node,AsString(@v))),
            FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNodes.DoAddChild(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,2);
  Result := BuildNull;
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    v := Param[1];
    try
      Result := BuildObject(
        TreeNodeToVCL(
          InitializeNode(GetTreeNodes.AddChild(node,AsString(@v))),
            FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNodes.DoAddChildFirst(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,2);
  Result := BuildNull;
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    v := Param[1];
    try
      Result := BuildObject(
        TreeNodeToVCL(
          InitializeNode(GetTreeNodes.AddChildFirst(node,AsString(@v))),
            FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNodes.DoAddChildObject(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,3);
  v := Param[2];
  Result := DoAddChild(Param);
  if IsObject(@Result) and IsObject(@v) then
    (Result.vObject as TJVCLBaseTreeNode).Data := v.vObject;
end;

function TJVCLBaseTreeNodes.DoAddChildObjectFirst(
  Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,3);
  v := Param[2];
  Result := DoAddChildFirst(Param);
  if IsObject(@Result) and IsObject(@v) then
    (Result.vObject as TJVCLBaseTreeNode).Data := v.vObject;
end;

function TJVCLBaseTreeNodes.DoAddFirst(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,2);
  Result := BuildNull;
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    v := Param[1];
    try
      Result := BuildObject(
        TreeNodeToVCL(
          InitializeNode(GetTreeNodes.AddFirst(node,AsString(@v))),
            FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNodes.DoAddObject(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,3);
  v := Param[2];
  Result := DoAdd(Param);
  if IsObject(@Result) and IsObject(@v) then
    (Result.vObject as TJVCLBaseTreeNode).Data := v.vObject;
end;

function TJVCLBaseTreeNodes.DoAddObjectFirst(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,3);
  v := Param[2];
  Result := DoAddFirst(Param);
  if IsObject(@Result) and IsObject(@v) then
    (Result.vObject as TJVCLBaseTreeNode).Data := v.vObject;
end;

function TJVCLBaseTreeNodes.DoAssign(Param: TJValueList): TJValue;
begin
{ TODO : 保留 }
  EmptyValue(Result);
end;

function TJVCLBaseTreeNodes.DoBeginUpdate(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetTreeNodes.BeginUpdate;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNodes.DoClear(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetTreeNodes.Clear;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNodes.DoDelete(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);
  v := Param[0];
  if VCLToTreeNode(v,node) and (node <> nil) then
  begin
    try
      GetTreeNodes.Delete(node);
    except
      on E:Exception do
        Error(E.Message);
    end;
  end;
end;

function TJVCLBaseTreeNodes.DoEndUpdate(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetTreeNodes.EndUpdate;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNodes.DoGetFirstNode(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildObject(
      TreeNodeToVCL(
        GetTreeNodes.GetFirstNode,FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNodes.DoGetNode(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  v := Param[0];
  try
    Result := BuildObject(
      TreeNodeToVCL(
        GetTreeNodes.GetNode(Pointer(AsInteger(@v))),
          FTreeView,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeNodes.DoInsert(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
  node: TTreeNode;
begin
  CheckVCL(Param,2);
  Result := BuildNull;
  v := Param[0];
  if VCLToTreeNode(v,node) then
  begin
    v := Param[1];
    try
      Result := BuildObject(
        TreeNodeToVCL(
          InitializeNode(GetTreeNodes.Insert(node,AsString(@v))),
            FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

function TJVCLBaseTreeNodes.DoInsertObject(Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  CheckVCL(Param,3);
  v := Param[2];
  Result := DoInsert(Param);
  if IsObject(@Result) and IsObject(@v) then
    (Result.vObject as TJVCLBaseTreeNode).Data := v.vObject;
end;

function TJVCLBaseTreeNodes.DoItem(Param: TJValueList): TJValue;
//VCLメソッド
var
  i: TJValue;
begin
  CheckVCL(Param,1);
  i := Param[0];
  //getter
  Result := Item(AsInteger(@i));
end;

function TJVCLBaseTreeNodes.GetCount: Integer;
begin
  CheckVCL;
  Result := GetTreeNodes.Count;
end;

function TJVCLBaseTreeNodes.GetHandle: Integer;
begin
  CheckVCL;
  Result := GetTreeNodes.Handle;
end;

function TJVCLBaseTreeNodes.Get_Owner: TJVCLBaseTreeView;
begin
  CheckVCL;
  Result := FTreeView;
end;

function TJVCLBaseTreeNodes.GetTreeNodes: TTreeNodes;
begin
  Result := FVCL as TTreeNodes;
end;

class function TJVCLBaseTreeNodes.VCLClassType: TClass;
begin
  Result := TTreeNodes;
end;

procedure TJVCLBaseTreeNodes.RegistMethods;
begin
  inherited;
  RegistMethod('addChildFirst',DoAddChildFirst);
  RegistMethod('addChild',DoAddChild);
  RegistMethod('addChildObjectFirst',DoAddChildObjectFirst);
  RegistMethod('addChildObject',DoAddChildObject);
  RegistMethod('addFirst',DoAddFirst);
  RegistMethod('add',DoAdd);
  RegistMethod('addObjectFirst',DoAddObjectFirst);
  RegistMethod('addObject',DoAddObject);
  RegistMethod('beginUpdate',DoBeginUpdate);
  RegistMethod('clear',DoClear);
  RegistMethod('delete',DoDelete);
  RegistMethod('endUpdate',DoEndUpdate);
  RegistMethod('getFirstNode',DoGetFirstNode);
  RegistMethod('getNode',DoGetNode);
  RegistMethod('insert',DoInsert);
  RegistMethod('insertObject',DoInsertObject);
  RegistMethod('item',DoItem);
end;

procedure TJVCLBaseTreeNodes.CheckVCL(Param: TJValueList;
  ArgCount: Integer);
begin
  inherited;
  if not Assigned(FTreeView) then
    Error('vcl is null');
end;

function TJVCLBaseTreeNodes.GetValue(S: String; ArrayStyle: Boolean;
  Param: TJValueList): TJValue;
var
  v: TJValue;
begin
  v := BuildString(S);
  if ArrayStyle and TryAsNumber(@v) then
    Result := Item(AsInteger(@v))
  else
    Result := inherited GetValue(S,ArrayStyle,Param);
end;

function TJVCLBaseTreeNodes.Item(Index: Integer): TJValue;
//デフォルトプロパティ
begin
  CheckVCL;
  Result := BuildNull;
  if CheckRange(Index) then
  begin
    try
      //getter
      Result := BuildObject(
        TreeNodeToVCL(GetTreeNodes.item[Index],
          FTreeView,FEngine));
    except
      on E:Exception do
        Error(E.Message);
    end;
  end
  else
    ArgsError;
end;

procedure TJVCLBaseTreeNodes.DestroyVCL;
begin
  inherited;
  FTreeView := nil;
end;

function TJVCLBaseTreeNodes.CheckRange(Index: Integer): Boolean;
begin
  CheckVCL;
  Result := (Index > -1) and (Index < GetTreeNodes.Count);
end;

function TJVCLBaseTreeNodes.InitializeNode(Node: TTreeNode): TTreeNode;
//nodeを初期化
begin
  CheckVCL;
  Result := FTreeView.InitializeNode(Node);
end;

function TJVCLBaseTreeNodes.GetItem(Index: Integer): TJValue;
begin
  Result := Item(Index);
end;

class function TJVCLBaseTreeNodes.IsArray: Boolean;
begin
  Result := True;
end;

{ TJVCLBaseTreeView }

constructor TJVCLBaseTreeView.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('VCLTreeView');
end;

procedure TJVCLBaseTreeView.CreateObjects;
begin
  inherited;
  FTreeNodes := TJVCLBaseTreeNodes.Create(FEngine,nil,False);
  FTreeNodes.IncRef;
  FTreeNodes.FTreeView := Self;
end;

destructor TJVCLBaseTreeView.Destroy;
begin
  inherited;
  FTreeNodes.FTreeView := nil;
  FTreeNodes.DecRef;
end;

procedure TJVCLBaseTreeView.DestroyVCL;
begin
  inherited;
  FTreeNodes.RegistVCL(nil,False);
end;

function TJVCLBaseTreeView.InitializeNode(Node: TTreeNode): TTreeNode;
//nodeを初期化
var
  vnode: TJVCLBaseTreeNode;
begin
  Result := Node;
  if Assigned(Node) and (Node.Data = nil) then
  begin
    vnode := TJVCLBaseTreeNode.Create(FEngine);
    vnode.RegistVCL(Node,False);
    vnode.IncRef;
    vnode.FTreeView := Self;
    //終了通知はTreeViewへ
    vnode.FreeNotification(Self);
    //Dataに入れる
    Node.Data := vnode;
  end;
end;

procedure TJVCLBaseTreeView.OnDeletion(Sender: TObject; Node: TTreeNode);
//削除イベント
var
  param: TJValueList;
  vnode: TJVCLBaseTreeNode;
begin
  if IsCallEvent('onDeletion') then
  begin
    param := TJValueList.Create;
    try
      param.Add(GetSender(Sender));
      param.Add(TreeNodeToVCL(Node,Self,FEngine));
      CallEvent('','onDeletion',param);
    finally
      param.Free;
    end;
  end;

  //Dataに入れたオブジェクトを削除
  if Assigned(Node) and (TObject(Node.Data) is TJVCLBaseTreeNode) then
  begin
    vnode := TObject(Node.Data) as TJVCLBaseTreeNode;
    //終了通知を削除
    vnode.RemoveFreeNotification(Self);
    RemoveFreeNotification(vnode);
    vnode.RegistVCL(nil,False);
    //参照カウントを減らす
    vnode.DecRef;
    //Dataを空にする
    Node.Data := nil;
  end;
end;

procedure TJVCLBaseTreeView.Notification(AObject: TJNotify);
//終了通知を受ける
var
  node: TJVCLBaseTreeNode;
begin
  inherited;
  //Dataをnilにする
  if AObject is TJVCLBaseTreeNode then
  begin
    node := (AObject as TJVCLBaseTreeNode);
    if node.IsVCL then
      node.GetTreeNode.Data := nil;
  end;
end;

function TJVCLBaseTreeView.DoAlphaSort(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetTreeView.AlphaSort);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

(*
function TJVCLBaseTreeView.DoCustomSort(Param: TJValueList): TJValue;
begin
{ TODO : 保留 }
  EmptyValue(Result);
end;
*)

function TJVCLBaseTreeView.DoFullCollapse(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetTreeView.FullCollapse;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.DoFullExpand(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  Result := BuildObject(Self);

  try
    GetTreeView.FullExpand;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.DoGetHitTestInfoAt(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
  hit: THitTests;
begin
  CheckVCL(Param,2);
  x := Param[0];
  y := Param[1];
  try
    hit := GetTreeView.GetHitTestInfoAt(AsInteger(@x),AsInteger(@y));
    Result := BuildString(
      SetToStr(TypeInfo(THitTests),hit));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.DoGetNodeAt(Param: TJValueList): TJValue;
//VCLメソッド
var
  x,y: TJValue;
begin
  CheckVCL(Param,2);
  x := Param[0];
  y := Param[1];
  try
    Result := BuildObject(
      TreeNodeToVCL(GetTreeView.GetNodeAt(AsInteger(@x),AsInteger(@y)),
        Self,FEngine));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.DoIsEditing(Param: TJValueList): TJValue;
//VCLメソッド
begin
  CheckVCL;
  try
    Result := BuildBool(GetTreeView.IsEditing);
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.DoLoadFromFile(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetTreeView.LoadFromFile(AsString(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.DoSaveToFile(Param: TJValueList): TJValue;
//VCLメソッド
var
  v: TJValue;
begin
  CheckVCL(Param,1);
  Result := BuildObject(Self);

  v := Param[0];
  try
    GetTreeView.SaveToFile(AsString(@v));
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

function TJVCLBaseTreeView.GetCustomTreeView: TCustomTreeView;
begin
  Result := FVCL as TCustomTreeView;
end;

function TJVCLBaseTreeView.GetDropTarget: TJVCLBaseTreeNode;
begin
  CheckVCL;
  Result := TreeNodeToVCL(GetCustomTreeView.DropTarget,Self,FEngine);
end;

function TJVCLBaseTreeView.GetSelection: TJVCLBaseTreeNode;
begin
  CheckVCL;
  Result := TreeNodeToVCL(GetCustomTreeView.Selected,Self,FEngine);
end;

function TJVCLBaseTreeView.GetTopItem: TJVCLBaseTreeNode;
begin
  CheckVCL;
  Result := TreeNodeToVCL(GetCustomTreeView.TopItem,Self,FEngine);
end;

function TJVCLBaseTreeView.GetTreeNodes: TJVCLBaseTreeNodes;
begin
  CheckVCL;
  Result := FTreeNodes;
end;

function TJVCLBaseTreeView.GetTreeView: TTreeView;
begin
  Result := FVCL as TTreeView;
end;

procedure TJVCLBaseTreeView.OnAdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
//イベント
var
  st: TJValue;
  param: TJValueList;
  rec: TJRect;
  dd: TJBooleanObject;
begin
  if not IsCallEvent('onAdvancedCustomDraw') then
    Exit;

  rec := TJRect.Create(FEngine);
  EnumToValue(TypeInfo(TCustomDrawStage),st,Ord(Stage));
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(rec);
    param.Add(st);
    param.Add(dd);
    CallEvent('','onAdvancedCustomDraw',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnAdvancedCustomDrawItem(
  Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
  Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
//イベント
var
  stg: TJValue;
  param: TJValueList;
  dd,pi: TJBooleanObject;
  stt: String;
begin
  if not IsCallEvent('onAdvancedCustomDrawItem') then
    Exit;

  stt := SetToStr(TypeInfo(TCustomDrawState),State);
  EnumToValue(TypeInfo(TCustomDrawStage),stg,Ord(Stage));
  pi := TJBooleanObject.Create(FEngine);
  pi.bool := PaintImages;
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.Add(stt);
    param.Add(stg);
    param.add(pi);
    param.Add(dd);
    CallEvent('','onAdvancedCustomDrawItem',param);
  finally
    DefaultDraw := dd.bool;
    PaintImages := pi.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnChange(Sender: TObject; Node: TTreeNode);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onChange') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    CallEvent('','onChange',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
//イベント
var
  param: TJValueList;
  aa: TJBooleanObject;
begin
  if not IsCallEvent('onChanging') then
    Exit;

  aa := TJBooleanObject.Create(FEngine);
  aa.bool := AllowChange;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.Add(aa);
    CallEvent('','onChanging',param);
  finally
    AllowChange := aa.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnCollapsed(Sender: TObject; Node: TTreeNode);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onCollapsed') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    CallEvent('','onCollapsed',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
//イベント
var
  param: TJValueList;
  ac: TJBooleanObject;
begin
  if not IsCallEvent('onCollapsing') then
    Exit;

  ac := TJBooleanObject.Create(FEngine);
  ac.bool := AllowCollapse;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.add(ac);
    CallEvent('','onCollapsing',param);
  finally
    AllowCollapse := ac.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
//イベント
var
  param: TJValueList;
  com: TJNumberObject;
begin
  if not IsCallEvent('onCompare') then
    Exit;

  com := TJNumberObject.Create(FEngine);
  com.int := Compare;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node1,Self,FEngine));
    param.Add(TreeNodeToVCL(Node2,Self,FEngine));
    param.Add(Data);
    param.Add(com);
    CallEvent('','onCompare',param);
  finally
    Compare := com.int;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
//イベント
var
  param: TJValueList;
  rec: TJRect;
  dd: TJBooleanObject;
begin
  if not IsCallEvent('onCustomDraw') then
    Exit;

  rec := TJRect.Create(FEngine);
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(rec);
    param.Add(dd);
    CallEvent('','onCustomDraw',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
//イベント
var
  param: TJValueList;
  dd: TJBooleanObject;
  stt: String;
begin
  if not IsCallEvent('onCustomDrawItem') then
    Exit;

  stt := SetToStr(TypeInfo(TCustomDrawState),State);
  dd := TJBooleanObject.Create(FEngine);
  dd.bool := DefaultDraw;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.Add(stt);
    param.Add(dd);
    CallEvent('','onCustomDrawItem',param);
  finally
    DefaultDraw := dd.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
//イベント
var
  param: TJValueList;
  str: TJStringObject;
begin
  if not IsCallEvent('onEdited') then
    Exit;

  str := TJStringObject.Create(Fengine);
  str.str := S;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.Add(str);
    CallEvent('','onEdited',param);
  finally
    S := str.str;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
//イベント
var
  param: TJValueList;
  al: TJBooleanObject;
begin
  if not IsCallEvent('onEditing') then
    Exit;

  al := TJBooleanObject.Create(Fengine);
  al.bool := AllowEdit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.Add(al);
    CallEvent('','onEditing',param);
  finally
    AllowEdit := al.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnExpanded(Sender: TObject; Node: TTreeNode);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onExpanded') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    CallEvent('','onExpanded',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
//イベント
var
  param: TJValueList;
  ae: TJBooleanObject;
begin
  if not IsCallEvent('onExpanding') then
    Exit;

  ae := TJBooleanObject.Create(FEngine);
  ae.bool := AllowExpansion;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    param.add(ae);
    CallEvent('','onExpanding',param);
  finally
    AllowExpansion := ae.bool;
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnGetImageIndex(Sender: TObject;
  Node: TTreeNode);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onGetImageIndex') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    CallEvent('','onGetImageIndex',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.OnGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
//イベント
var
  param: TJValueList;
begin
  if not IsCallEvent('onGetSelectedIndex') then
    Exit;

  param := TJValueList.Create;
  try
    param.Add(GetSender(Sender));
    param.Add(TreeNodeToVCL(Node,Self,FEngine));
    CallEvent('','onGetSelectedIndex',param);
  finally
    param.Free;
  end;
end;

procedure TJVCLBaseTreeView.RegistEvents;
//イベント登録
var
  c: TTreeView;
begin
  c := GetTreeView;
  if not Assigned(c) then
    Exit;

  c.OnAdvancedCustomDraw := OnAdvancedCustomDraw;
  c.OnAdvancedCustomDrawItem := OnAdvancedCustomDrawItem;
  c.OnChange := OnChange;
  c.OnChanging := OnChanging;
  c.OnClick := OnClick;
  c.OnCollapsed := OnCollapsed;
  c.OnCollapsing := OnCollapsing;
  c.OnCompare := OnCompare;
  c.OnContextPopup := OnContextPopup;
  c.OnCustomDraw := OnCustomDraw;
  c.OnCustomDrawItem := OnCustomDrawItem;
  c.OnDblClick := OnDblClick;
  c.OnDeletion := OnDeletion;
  c.OnDragDrop := OnDragDrop;
  c.OnDragOver := OnDragOver;
  c.OnEdited := OnEdited;
  c.OnEditing := OnEditing;
  c.OnEndDock := OnEndDock;
  c.OnEndDrag := OnEndDrag;
  c.OnEnter := OnEnter;
  c.OnExit := OnExit;
  c.OnExpanded := OnExpanded;
  c.OnExpanding := OnExpanding;
  c.OnGetImageIndex := OnGetImageIndex;
  c.OnGetSelectedIndex := OnGetSelectedIndex;
  c.OnKeyDown := OnKeyDown;
  c.OnKeyPress := OnKeyPress;
  c.OnKeyUp := OnKeyUp;
  c.OnMouseDown := OnMouseDown;
  c.OnMouseMove := OnMouseMove;
  c.OnMouseUp := OnMouseUp;
  c.OnStartDock := OnStartDock;
  c.OnStartDrag := OnStartDrag;
end;

procedure TJVCLBaseTreeView.RegistMethods;
begin
  inherited;
  RegistMethod('alphaSort',DoAlphaSort);
  //RegistMethod('customSort',DoCustomSort);
  RegistMethod('fullCollapse',DoFullCollapse);
  RegistMethod('fullExpand',DoFullExpand);
  RegistMethod('getHitTestInfoAt',DoGetHitTestInfoAt);
  RegistMethod('getNodeAt',DoGetNodeAt);
  RegistMethod('isEditing',DoIsEditing);
  RegistMethod('loadFromFile',DoLoadFromFile);
  RegistMethod('saveToFile',DoSaveToFile);
end;

function TJVCLBaseTreeView.RegistVCL(AVCL: TPersistent;
  ACanDestroy: Boolean): Boolean;
begin
  Result := inherited RegistVCL(AVCL,ACanDestroy);
  if Result then
  begin
    FTreeNodes.RegistVCL(GetTreeView.Items,False);
    FTreeNodes.FTreeView := Self;
  end;
end;

procedure TJVCLBaseTreeView.SetDropTarget(const Value: TJVCLBaseTreeNode);
begin
  CheckVCL;
  if Assigned(Value) then
    GetCustomTreeView.DropTarget := Value.GetTreeNode
  else
    GetCustomTreeView.DropTarget := nil;
end;

procedure TJVCLBaseTreeView.SetSelection(const Value: TJVCLBaseTreeNode);
begin
  CheckVCL;
  if Assigned(Value) then
    GetCustomTreeView.Selected := Value.GetTreeNode
  else
    GetCustomTreeView.Selected := nil;
end;

procedure TJVCLBaseTreeView.SetTopItem(const Value: TJVCLBaseTreeNode);
begin
  CheckVCL;
  if Assigned(Value) then
    GetCustomTreeView.TopItem := Value.GetTreeNode
  else
    GetCustomTreeView.TopItem := nil;
end;

class function TJVCLBaseTreeView.VCLClassType: TClass;
begin
  Result := TTreeView;
end;

{ TJVCLTreeView }

procedure TJVCLTreeView.CreateVCL;
begin
  RegistVCL(TTreeView.Create(nil),True);
end;





initialization
  //class登録
  RegisterVCLClasses;

finalization
  FreeAndNil(__VCLCaster__);

end.
