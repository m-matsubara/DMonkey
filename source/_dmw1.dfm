object Form1: TForm1
  Left = 225
  Top = 148
  Width = 796
  Height = 511
  Caption = '["D"Monkey] Script Engine for Delphi'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ÇlÇr ÇoÉSÉVÉbÉN'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 12
  object StatusBar1: TStatusBar
    Left = 0
    Top = 446
    Width = 788
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 788
    Height = 417
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 681
      Top = 0
      Width = 4
      Height = 417
      Cursor = crHSplit
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 681
      Height = 417
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Panel2'
      TabOrder = 0
      object Splitter2: TSplitter
        Left = 0
        Top = 275
        Width = 681
        Height = 4
        Cursor = crVSplit
        Align = alTop
      end
      object mmStdout: TMemo
        Left = 0
        Top = 279
        Width = 681
        Height = 138
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 1
        WantTabs = True
        WordWrap = False
        OnMouseMove = mmStdoutMouseMove
      end
      object mmSource: TMemo
        Left = 0
        Top = 0
        Width = 681
        Height = 275
        Align = alTop
        Font.Charset = SHIFTJIS_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'ÇlÇr ÇoÉSÉVÉbÉN'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnMouseMove = mmSourceMouseMove
      end
    end
    object mmDebug: TMemo
      Left = 685
      Top = 0
      Width = 103
      Height = 417
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 788
    Height = 29
    ButtonHeight = 21
    Caption = 'ToolBar1'
    TabOrder = 2
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Action = actRunRun
    end
    object edtFunction: TEdit
      Left = 23
      Top = 2
      Width = 156
      Height = 21
      Hint = 'Function Name'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'abcdefg'
    end
    object Edit1: TEdit
      Left = 179
      Top = 2
      Width = 121
      Height = 21
      Hint = 'Object Count'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = 'Edit1'
    end
    object edtThread: TSpinEdit
      Left = 300
      Top = 2
      Width = 69
      Height = 21
      Hint = 'ÉXÉåÉbÉhêî'
      MaxValue = 2147483647
      MinValue = 1
      TabOrder = 2
      Value = 10
    end
  end
  object ActionList1: TActionList
    Left = 8
    Top = 72
    object actFile: TAction
      Category = 'File'
      Caption = 'File'
      OnExecute = actFileExecute
    end
    object actRun: TAction
      Category = 'Run'
      Caption = 'Run'
      OnExecute = actRunExecute
    end
    object actClear: TAction
      Category = 'Clear'
      Caption = 'Clear'
      OnExecute = actClearExecute
    end
    object actClearSource: TAction
      Category = 'Clear'
      Caption = 'Source'
      OnExecute = actClearSourceExecute
    end
    object actClearStdout: TAction
      Category = 'Clear'
      Caption = 'Stdout'
      OnExecute = actClearStdoutExecute
    end
    object actClearDebug: TAction
      Category = 'Clear'
      Caption = 'Debug'
      OnExecute = actClearDebugExecute
    end
    object actRunRun: TAction
      Category = 'Run'
      Caption = 'Run'
      Hint = 'Run'
      ShortCut = 116
      OnExecute = actRunRunExecute
    end
    object actRunFunction: TAction
      Category = 'Run'
      Caption = 'Function'
      OnExecute = actRunFunctionExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = 'Open'
      OnExecute = actFileOpenExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = 'Save'
      OnExecute = actFileSaveExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = actFileExitExecute
    end
    object actClearAll: TAction
      Category = 'Clear'
      Caption = 'All'
      ShortCut = 114
      OnExecute = actClearAllExecute
    end
    object actRunRun2: TAction
      Category = 'Run'
      Caption = 'Run'
      Hint = 'Run'
      ShortCut = 120
      OnExecute = actRunRunExecute
    end
    object actRunThread: TAction
      Category = 'Run'
      Caption = 'RunThread'
      Hint = 'ÉXÉåÉbÉhÇ≈é¿çs'
      ShortCut = 117
      OnExecute = actRunThreadExecute
    end
    object actRunAbort: TAction
      Category = 'Run'
      Caption = 'Abort'
      OnExecute = actRunAbortExecute
    end
    object actTest: TAction
      Category = 'Test'
      Caption = 'Test'
      OnExecute = actTestExecute
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 40
    object File1: TMenuItem
      Action = actFile
      object actFileOpen1: TMenuItem
        Action = actFileOpen
      end
      object actFileClose1: TMenuItem
        Action = actFileSave
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actFileExit
      end
    end
    object N1: TMenuItem
      Action = actRun
      object Run1: TMenuItem
        Action = actRunRun
      end
      object Function1: TMenuItem
        Action = actRunFunction
      end
      object actRunThread1: TMenuItem
        Action = actRunThread
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object actRunAbort1: TMenuItem
        Action = actRunAbort
      end
    end
    object Clear1: TMenuItem
      Action = actClear
      object All1: TMenuItem
        Action = actClearAll
      end
      object Source1: TMenuItem
        Action = actClearSource
      end
      object Stdout1: TMenuItem
        Action = actClearStdout
      end
      object Debug1: TMenuItem
        Action = actClearDebug
      end
    end
    object Test1: TMenuItem
      Action = actTest
    end
    object Test21: TMenuItem
      Caption = 'Test2'
      OnClick = Test21Click
    end
    object Test31: TMenuItem
      Caption = 'Test3'
    end
  end
  object DM: TDMS
    LibraryPath.Strings = (
      'C:\SourceForge\dmonkey\dmonkey\test')
    CompiledBinary = False
    OnStdout = DMStdout
    OnStderr = DMStdout
    OnNewObject = DMNewObject
    Left = 8
    Top = 109
  end
  object opd: TOpenDialog
    DefaultExt = 'dms'
    FileName = 'test_unit.dms'
    Filter = 'Script|*.js;*.es;*.dms|all|*.*'
    Left = 24
    Top = 149
  end
  object svd: TSaveDialog
    DefaultExt = 'dms'
    Filter = 'Script|*.js;*.es;*.dms|all|*.*'
    Left = 56
    Top = 149
  end
end
