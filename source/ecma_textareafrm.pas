unit ecma_textareafrm;

//Global.textArea
//2001/04/28
//by Wolfy

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmTextArea = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    mmText: TMemo;
    lblText: TLabel;
    btnClear: TButton;
    procedure btnClearClick(Sender: TObject);
  private
    { Private êÈåæ }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public êÈåæ }
  end;

implementation

{$R *.DFM}

procedure TfrmTextArea.btnClearClick(Sender: TObject);
begin
  mmText.Clear;
end;

procedure TfrmTextArea.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Owner is TCustomForm then
    Params.WndParent := (Owner as TCustomForm).Handle
  else if Assigned(Application) and Assigned(Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle;
end;

end.
