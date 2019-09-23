unit ecma_promptfrm;

//Global.prompt
//2001/04/21
//by Wolfy

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmPrompt = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    lblText: TLabel;
    edtPrompt: TEdit;
  private
    { Private êÈåæ }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public êÈåæ }
  end;

implementation

{$R *.DFM}

{ TfrmPrompt }

procedure TfrmPrompt.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Owner is TCustomForm then
    Params.WndParent := (Owner as TCustomForm).Handle
  else if Assigned(Application) and Assigned(Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle;
end;

end.
