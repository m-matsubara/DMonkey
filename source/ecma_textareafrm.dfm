object frmTextArea: TfrmTextArea
  Left = 194
  Top = 107
  BorderStyle = bsDialog
  Caption = 'frmTextArea'
  ClientHeight = 275
  ClientWidth = 492
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS UI Gothic'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object lblText: TLabel
    Left = 10
    Top = 8
    Width = 4
    Height = 12
  end
  object btnOK: TButton
    Left = 410
    Top = 28
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 410
    Top = 59
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object mmText: TMemo
    Left = 6
    Top = 26
    Width = 395
    Height = 243
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnClear: TButton
    Left = 410
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Clear'
    TabOrder = 3
    OnClick = btnClearClick
  end
end
