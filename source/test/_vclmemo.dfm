object frmVCLMemo: TfrmVCLMemo
  Left = 473
  Top = 109
  Width = 333
  Height = 213
  Caption = 'VCLメモ帳'
  Color = clBtnFace
  Font.Charset = SHIFTJIS_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ＭＳ Ｐゴシック'
  Font.Style = []
  Menu = mainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 12
  object memo: TMemo
    Left = 0
    Top = 0
    Width = 325
    Height = 167
    Align = alClient
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'ＭＳ Ｐゴシック'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object actionList: TActionList
    Left = 52
    Top = 28
    object actFile: TAction
      Category = 'File'
      Caption = 'ファイル(&F)'
    end
    object actEdit: TAction
      Category = 'Edit'
      Caption = '編集(&E)'
    end
    object actFormat: TAction
      Category = 'Format'
      Caption = '書式(&O)'
    end
    object actHelp: TAction
      Category = 'Help'
      Caption = 'ヘルプ(&H)'
    end
    object actFileNew: TAction
      Category = 'File'
      Caption = '新規(&N)'
      ShortCut = 16462
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = '開く(&O)'
      ShortCut = 16463
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '上書き保存(&S)'
      ShortCut = 16467
    end
    object actFileSaveAs: TAction
      Category = 'File'
      Caption = '名前を付けて保存(&A)...'
    end
    object actFilePage: TAction
      Category = 'File'
      Caption = 'ページ設定(&U)...'
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '印刷(&P)...'
      ShortCut = 16464
    end
    object actFileClose: TAction
      Category = 'File'
      Caption = 'メモ帳の終了(&X)'
    end
    object actEditUndo: TAction
      Category = 'Edit'
      Caption = '元に戻す(&U)'
      ShortCut = 16474
    end
    object actEditCut: TAction
      Category = 'Edit'
      Caption = '切り取り(&T)'
      ShortCut = 16472
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = 'コピー(&C)'
      ShortCut = 16451
    end
    object actEditPaste: TAction
      Category = 'Edit'
      Caption = '貼り付け(&P)'
      ShortCut = 16470
    end
    object actEditDelete: TAction
      Category = 'Edit'
      Caption = '削除(&L)'
      ShortCut = 46
    end
    object actEditFind: TAction
      Category = 'Edit'
      Caption = '検索(&F)...'
      ShortCut = 16454
    end
    object actEditFindNext: TAction
      Category = 'Edit'
      Caption = '次を検索(&N)'
      ShortCut = 114
    end
    object actEditReplace: TAction
      Category = 'Edit'
      Caption = '置換(&R)...'
      ShortCut = 16456
    end
    object actEditGoto: TAction
      Category = 'Edit'
      Caption = '行へ移動(&G)'
      ShortCut = 16455
    end
    object actEditSelectAll: TAction
      Category = 'Edit'
      Caption = 'すべて選択(&A)'
      ShortCut = 16449
    end
    object actEditDateTime: TAction
      Category = 'Edit'
      Caption = '日付と時間(&D)'
      ShortCut = 116
    end
    object actFormatWordWrap: TAction
      Category = 'Format'
      Caption = '右端で折り返す(&W)'
    end
    object actFormatFont: TAction
      Category = 'Format'
      Caption = 'フォント(&F)...'
    end
    object actHelpTopic: TAction
      Category = 'Help'
      Caption = 'トピックの検索(&H)'
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = 'バージョン情報(&A)'
    end
  end
  object mainMenu: TMainMenu
    Left = 12
    Top = 28
    object actFile1: TMenuItem
      Action = actFile
      object N1: TMenuItem
        Action = actFileNew
      end
      object Open1: TMenuItem
        Action = actFileOpen
      end
      object Save1: TMenuItem
        Action = actFileSave
      end
      object SaveAs1: TMenuItem
        Action = actFileSaveAs
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PrintSetup1: TMenuItem
        Action = actFilePage
      end
      object Print1: TMenuItem
        Action = actFilePrint
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object X1: TMenuItem
        Action = actFileClose
      end
    end
    object actEdit1: TMenuItem
      Action = actEdit
      object Undo1: TMenuItem
        Action = actEditUndo
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Action = actEditCut
      end
      object Cut2: TMenuItem
        Action = actEditCopy
      end
      object Paste1: TMenuItem
        Action = actEditPaste
      end
      object Delete1: TMenuItem
        Action = actEditDelete
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object F1: TMenuItem
        Action = actEditFind
      end
      object FindNext1: TMenuItem
        Action = actEditFindNext
      end
      object Replace1: TMenuItem
        Action = actEditReplace
      end
      object G1: TMenuItem
        Action = actEditGoto
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Action = actEditSelectAll
      end
      object D1: TMenuItem
        Action = actEditDateTime
      end
    end
    object actFormat1: TMenuItem
      Action = actFormat
      object W1: TMenuItem
        Action = actFormatWordWrap
      end
      object Font1: TMenuItem
        Action = actFormatFont
      end
    end
    object actHelp1: TMenuItem
      Action = actHelp
      object SearchforHelpOn1: TMenuItem
        Action = actHelpTopic
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Action = actHelpAbout
      end
    end
  end
  object openDialog: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'txt|*.txt|*.*|*.*'
    Title = '開く'
    Left = 28
    Top = 76
  end
  object saveDialog: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'txt|*.txt|*.*|*.*'
    Title = '保存'
    Left = 60
    Top = 76
  end
  object fontDialog: TFontDialog
    Font.Charset = SHIFTJIS_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'ＭＳ Ｐゴシック'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 92
    Top = 80
  end
end
