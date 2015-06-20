inherited ClassesListDockingForm: TClassesListDockingForm
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'PHP Class Inspector'
  ClientHeight = 632
  ClientWidth = 385
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  ExplicitWidth = 401
  ExplicitHeight = 666
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 613
    Width = 385
    Height = 19
    Panels = <>
    SimplePanel = True
    SizeGrip = False
  end
  object edtSearch: TLabeledEdit
    Left = 4
    Top = 18
    Width = 373
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 37
    EditLabel.Height = 13
    EditLabel.Caption = 'Search:'
    TabOrder = 1
    OnChange = edtSearchChange
  end
  object cbxShowPrivateProtected: TCheckBox
    Left = 4
    Top = 551
    Width = 373
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Show private && protected'
    TabOrder = 2
    OnClick = cbxShowPrivateProtectedClick
  end
  object cbxShowSeparately: TCheckBox
    Left = 4
    Top = 568
    Width = 373
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Show attributes && methods separately'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = cbxShowSeparatelyClick
  end
  object ClassListTreeView: TVirtualStringTree
    Left = 4
    Top = 44
    Width = 373
    Height = 501
    Anchors = [akLeft, akTop, akRight, akBottom]
    EmptyListMessage = 'No classes found'
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = TreeViewImageList
    Margin = 2
    TabOrder = 4
    OnDrawText = ClassListTreeViewDrawText
    OnGetText = ClassListTreeViewGetText
    OnGetImageIndex = ClassListTreeViewGetImageIndex
    OnNodeDblClick = ClassListTreeViewNodeDblClick
    Columns = <>
  end
  object cbxShowMethodParameters: TCheckBox
    Left = 4
    Top = 585
    Width = 373
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Show method parameters'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = cbxShowMethodParametersClick
  end
  object TreeViewImageList: TImageList
    Left = 240
    Top = 384
  end
  object ClassLinkingListStartTimer: TTimer
    Interval = 200
    OnTimer = ClassLinkingListStartTimerTimer
    Left = 240
    Top = 336
  end
end
