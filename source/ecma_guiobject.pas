unit ecma_guiobject;

{$IFDEF CONSOLE}
  {$DEFINE NO_OWNER}
{$ENDIF}
{$IFDEF MB_NO_OWNER}
  {$DEFINE NO_OWNER}
{$ENDIF}

interface

uses
  Windows,Sysutils,Classes,ecma_type,ecma_checklistfrm,ecma_extobject,ecma_misc,
  Forms,Math;

type
  TJCheckListBox = class(TJObject)
  private
    FItems: TJStringsObject;
    FChecks: array of Boolean;
    FIndex: Integer;
    function GetItemCount: Integer;
    procedure StringsOnChange(Sender: TObject);
  protected
    function DoGetChecked(Param: TJValueList): TJValue;
    function DoSetChecked(Param: TJValueList): TJValue;
    function DoExecute(Param: TJValueList): TJValue;
  public
    constructor Create(AEngine: TJBaseEngine; Param: TJValueList = nil;
      RegisteringFactory: Boolean = True); override;
    destructor Destroy; override;
  published
    property items: TJStringsObject read FItems;
    property count: Integer read GetItemCount;
    property index: Integer read FIndex;
  end;



procedure RegisterDMS(Engine: TJBaseEngine);


implementation

procedure RegisterDMS(Engine: TJBaseEngine);
begin
  Engine.ImportObject('CheckListBox',TJCheckListBox);
end;


{ TJCheckListBox }

constructor TJCheckListBox.Create(AEngine: TJBaseEngine;
  Param: TJValueList; RegisteringFactory: Boolean);
begin
  inherited;
  RegistName('CheckListBox');
  //メソッド登録
  RegistMethod('getChecked',DoGetChecked);
  RegistMethod('setChecked',DoSetChecked);
  RegistMethod('execute',DoExecute);
  //
  FItems := TJStringsObject.Create(FEngine,nil,False);
  FItems.IncRef;
  //イベントをセット
  FItems.Strings.OnChange := StringsOnChange;

  FIndex := -1;
end;

destructor TJCheckListBox.Destroy;
begin
  FItems.Strings.OnChange := nil;
  FItems.DecRef;
  inherited;
end;

function TJCheckListBox.DoExecute(Param: TJValueList): TJValue;
//実行する
var
  frm: TfrmCheckList;
  i,idx,w,h: Integer;
  capt,lbl: String;
  v: TJValue;
begin
  capt := '';
  lbl := '';
  idx := 0;
  w := 0;//width
  h := 0;//height

  for i := 0 to GetParamCount(Param) - 1 do
  begin
    v := Param[i];
    case i of
      0: capt := AsString(@v);
      1: lbl := AsString(@v);
      2: idx := AsInteger(@v);
      3: w := AsInteger(@v);
      4: h := AsInteger(@v);
    end;
  end;

{$IFNDEF NO_OWNER}
  frm := TfrmCheckList.Create(Application.MainForm);
{$ELSE}
  frm := TfrmCheckList.Create(nil);
{$ENDIF}
  try
    if capt <> '' then
      frm.Caption := capt
    else
      frm.Caption := GetApplicationTitle;
    frm.lblText.Caption := lbl;
    if w > 0 then
      frm.ClientWidth := Max(w,200);
    if h > 0 then
      frm.ClientHeight := Max(h,200);

    //セットする
    frm.lbCheck.Items.Assign(FItems.Strings);
    frm.lbCheck.ItemIndex := idx;
    for i := 0 to Length(FChecks) - 1 do
      frm.lbCheck.Checked[i] := FChecks[i];

    if frm.ShowModal = IDOK then
    begin
      Result := BuildBool(True);
      //戻す
      for i := 0 to frm.lbCheck.Items.Count - 1 do
        FChecks[i] := frm.lbCheck.Checked[i];
    end
    else
      Result := BuildBool(False);
    //選択項目
    FIndex := frm.lbCheck.ItemIndex;
  finally
    //frm.Release;
    frm.Free;
  end;
end;

function TJCheckListBox.DoGetChecked(Param: TJValueList): TJValue;
var
  v: TJValue;
  index: Integer;
begin
  if IsParam1(Param) then
  begin
    v := Param[0];
    index := AsInteger(@v);
    try
      Result := BuildBool(FChecks[index]);
    except
      raise EJThrow.Create(E_INDEX,'CheckListBox.getChecked Index Error');
    end;
  end
  else
    Result := BuildBool(False);
end;

function TJCheckListBox.DoSetChecked(Param: TJValueList): TJValue;
var
  v: TJValue;
  index: Integer;
begin
  Result := BuildObject(Self);
  if IsParam2(Param) then
  begin
    v := Param[0];
    index := AsInteger(@v);
    v := Param[1];
    try
      FChecks[index] := AsBool(@v);
    except
      raise EJThrow.Create(E_INDEX,'CheckListBox.setChecked Index Error');
    end;
  end;
end;

function TJCheckListBox.GetItemCount: Integer;
begin
  Result := FItems.Strings.Count;
end;

procedure TJCheckListBox.StringsOnChange(Sender: TObject);
begin
  //boolean配列をセットする
  SetLength(FChecks,FItems.Strings.Count);
end;

end.
