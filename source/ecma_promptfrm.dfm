object frmPrompt: TfrmPrompt
  Left = 503
  Top = 161
  BorderStyle = bsDialog
  Caption = 'frmPrompt'
  ClientHeight = 73
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
    Left = 12
    Top = 8
    Width = 381
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    WordWrap = True
  end
  object btnOK: TButton
    Left = 408
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 408
    Top = 40
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object edtPrompt: TEdit
    Left = 10
    Top = 41
    Width = 385
    Height = 20
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
