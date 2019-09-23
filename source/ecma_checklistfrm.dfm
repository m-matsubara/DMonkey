object frmCheckList: TfrmCheckList
  Left = 586
  Top = 394
  BorderStyle = bsDialog
  ClientHeight = 243
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '‚l‚r ‚oƒSƒVƒbƒN'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 12
  object lblText: TLabel
    Left = 16
    Top = 8
    Width = 4
    Height = 12
  end
  object lbCheck: TCheckListBox
    Left = 8
    Top = 26
    Width = 321
    Height = 207
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 12
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 336
    Top = 26
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 336
    Top = 57
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
