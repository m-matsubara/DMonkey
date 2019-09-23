object frmVCLTest: TfrmVCLTest
  Left = 464
  Top = 25
  Width = 555
  Height = 589
  Anchors = [akLeft, akTop, akRight]
  Caption = 'frmVCLTest'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '‚l‚r ‚oƒSƒVƒbƒN'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = PopupMenu1
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 76
    Top = 42
    Width = 33
    Height = 12
    Caption = 'Label1'
  end
  object Image1: TImage
    Left = 12
    Top = 250
    Width = 105
    Height = 105
  end
  object Splitter1: TSplitter
    Left = 0
    Top = 466
    Width = 547
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Edit1: TEdit
    Left = 100
    Top = 66
    Width = 221
    Height = 20
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 12
    Top = 78
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 104
    Top = 94
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 2
  end
  object RadioButton1: TRadioButton
    Left = 216
    Top = 94
    Width = 81
    Height = 17
    Caption = 'RadioButton1'
    TabOrder = 3
  end
  object ListBox1: TListBox
    Left = 16
    Top = 118
    Width = 133
    Height = 65
    ItemHeight = 12
    Items.Strings = (
      'ooo')
    MultiSelect = True
    TabOrder = 4
  end
  object ComboBox1: TComboBox
    Left = 168
    Top = 118
    Width = 145
    Height = 20
    ItemHeight = 12
    TabOrder = 5
    Text = 'ComboBox1'
  end
  object GroupBox1: TGroupBox
    Left = 164
    Top = 150
    Width = 149
    Height = 45
    Caption = 'GroupBox1'
    TabOrder = 6
  end
  object RadioGroup1: TRadioGroup
    Left = 328
    Top = 138
    Width = 217
    Height = 85
    Caption = 'RadioGroup1'
    TabOrder = 7
  end
  object Panel1: TPanel
    Left = 0
    Top = 469
    Width = 547
    Height = 55
    Align = alBottom
    Caption = 'Panel1'
    TabOrder = 8
  end
  object CheckListBox1: TCheckListBox
    Left = 128
    Top = 250
    Width = 125
    Height = 105
    ItemHeight = 12
    Items.Strings = (
      'aaa'
      'bbb'
      'ccc')
    TabOrder = 9
  end
  object Memo1: TMemo
    Left = 340
    Top = 46
    Width = 197
    Height = 77
    Lines.Strings = (
      'Memo1')
    TabOrder = 10
  end
  object TabControl1: TTabControl
    Left = 264
    Top = 206
    Width = 105
    Height = 153
    TabOrder = 11
    Tabs.Strings = (
      'abcb'
      'efg')
    TabIndex = 0
  end
  object PageControl1: TPageControl
    Left = 404
    Top = 242
    Width = 137
    Height = 149
    ActivePage = TabSheet1
    TabOrder = 12
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
    end
  end
  object ProgressBar1: TProgressBar
    Left = 12
    Top = 194
    Width = 150
    Height = 16
    Min = 0
    Max = 100
    TabOrder = 13
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 366
    Width = 121
    Height = 97
    Indent = 19
    TabOrder = 14
  end
  object ListView1: TListView
    Left = 140
    Top = 366
    Width = 229
    Height = 89
    Columns = <
      item
      end>
    TabOrder = 15
    ViewStyle = vsReport
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 524
    Width = 547
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object Button2: TButton
    Left = 12
    Top = 220
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 17
  end
  object Button3: TButton
    Left = 436
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 18
  end
  object Button4: TButton
    Left = 440
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 19
  end
  object CoolBar1: TCoolBar
    Left = 0
    Top = 0
    Width = 547
    Height = 37
    Bands = <
      item
        Control = ToolBar1
        ImageIndex = -1
        MinHeight = 29
        Width = 543
      end>
    object ToolBar1: TToolBar
      Left = 9
      Top = 0
      Width = 530
      Height = 29
      Caption = 'ToolBar1'
      TabOrder = 0
      object ToolButton1: TToolButton
        Left = 0
        Top = 2
        Caption = 'ToolButton1'
        ImageIndex = 0
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 2
        Caption = 'ToolButton2'
        ImageIndex = 1
      end
      object ToolButton3: TToolButton
        Left = 46
        Top = 2
        Caption = 'ToolButton3'
        ImageIndex = 2
      end
      object ToolButton4: TToolButton
        Left = 69
        Top = 2
        Caption = 'ToolButton4'
        ImageIndex = 3
      end
      object ToolButton5: TToolButton
        Left = 92
        Top = 2
        Caption = 'ToolButton5'
        ImageIndex = 4
      end
      object ToolButton23: TToolButton
        Left = 115
        Top = 2
        Width = 8
        Caption = 'ToolButton23'
        ImageIndex = 22
        Style = tbsSeparator
      end
      object ToolButton6: TToolButton
        Left = 123
        Top = 2
        Caption = 'ToolButton6'
        ImageIndex = 5
      end
      object ToolButton7: TToolButton
        Left = 146
        Top = 2
        Caption = 'ToolButton7'
        ImageIndex = 6
      end
      object ToolButton8: TToolButton
        Left = 169
        Top = 2
        Caption = 'ToolButton8'
        ImageIndex = 7
      end
      object ToolButton9: TToolButton
        Left = 192
        Top = 2
        Caption = 'ToolButton9'
        ImageIndex = 8
      end
      object ToolButton10: TToolButton
        Left = 215
        Top = 2
        Caption = 'ToolButton10'
        ImageIndex = 9
      end
      object ToolButton24: TToolButton
        Left = 238
        Top = 2
        Width = 8
        Caption = 'ToolButton24'
        ImageIndex = 22
        Style = tbsSeparator
      end
      object ToolButton11: TToolButton
        Left = 246
        Top = 2
        Caption = 'ToolButton11'
        ImageIndex = 10
      end
      object ToolButton12: TToolButton
        Left = 269
        Top = 2
        Caption = 'ToolButton12'
        ImageIndex = 11
      end
      object ToolButton13: TToolButton
        Left = 292
        Top = 2
        Caption = 'ToolButton13'
        ImageIndex = 12
      end
      object ToolButton14: TToolButton
        Left = 315
        Top = 2
        Caption = 'ToolButton14'
        ImageIndex = 13
      end
      object ToolButton15: TToolButton
        Left = 338
        Top = 2
        Caption = 'ToolButton15'
        ImageIndex = 14
      end
      object ToolButton22: TToolButton
        Left = 361
        Top = 2
        Width = 8
        Caption = 'ToolButton22'
        ImageIndex = 21
        Style = tbsSeparator
      end
      object ToolButton16: TToolButton
        Left = 369
        Top = 2
        Caption = 'ToolButton16'
        ImageIndex = 15
      end
      object ToolButton17: TToolButton
        Left = 392
        Top = 2
        Caption = 'ToolButton17'
        ImageIndex = 16
      end
      object ToolButton18: TToolButton
        Left = 415
        Top = 2
        Caption = 'ToolButton18'
        ImageIndex = 17
      end
      object ToolButton19: TToolButton
        Left = 438
        Top = 2
        Caption = 'ToolButton19'
        ImageIndex = 18
      end
      object ToolButton20: TToolButton
        Left = 461
        Top = 2
        Caption = 'ToolButton20'
        ImageIndex = 19
      end
      object ToolButton21: TToolButton
        Left = 484
        Top = 2
        Width = 8
        Caption = 'ToolButton21'
        ImageIndex = 20
        Style = tbsSeparator
      end
    end
  end
  object SpinEdit1: TSpinEdit
    Left = 136
    Top = 216
    Width = 121
    Height = 21
    MaxValue = 0
    MinValue = 0
    TabOrder = 21
    Value = 0
  end
  object UpDown1: TUpDown
    Left = 376
    Top = 252
    Width = 16
    Height = 24
    Min = 0
    Position = 0
    TabOrder = 22
    Wrap = False
  end
  object MainMenu1: TMainMenu
    Left = 4
    Top = 38
    object aaa1: TMenuItem
      Caption = 'aaa'
      object bbb1: TMenuItem
        Caption = 'bbb'
      end
      object ccc1: TMenuItem
        Action = Action1
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 36
    Top = 38
    object ddd1: TMenuItem
      Caption = 'ddd'
    end
    object eee1: TMenuItem
      Caption = 'eee'
    end
  end
  object Timer1: TTimer
    Left = 172
    Top = 44
  end
  object PopupMenu2: TPopupMenu
    Left = 4
    Top = 68
    object menu21: TMenuItem
      Caption = 'menu2'
    end
  end
  object ActionList1: TActionList
    Left = 136
    Top = 40
    object Action1: TAction
      Caption = 'Action1'
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 372
    Top = 332
  end
  object SaveDialog1: TSaveDialog
    Left = 372
    Top = 364
  end
  object FontDialog1: TFontDialog
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = '‚l‚r ‚oƒSƒVƒbƒN'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 376
    Top = 396
  end
  object FindDialog1: TFindDialog
    Left = 376
    Top = 428
  end
  object ReplaceDialog1: TReplaceDialog
    Left = 408
    Top = 428
  end
end
