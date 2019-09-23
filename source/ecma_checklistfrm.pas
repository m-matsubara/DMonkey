unit ecma_checklistfrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst;

type
  TfrmCheckList = class(TForm)
    lbCheck: TCheckListBox;
    lblText: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private êÈåæ }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public êÈåæ }
  end;

implementation

{$R *.DFM}

{ TfrmCheckList }

procedure TfrmCheckList.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Owner is TCustomForm then
    Params.WndParent := (Owner as TCustomForm).Handle
  else if Assigned(Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle;
end;

end.
