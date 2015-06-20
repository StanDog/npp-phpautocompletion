inherited AboutForm: TAboutForm
  AlignWithMargins = True
  Left = 640
  Top = 457
  BorderStyle = bsToolWindow
  Caption = 'About ACCPC'
  ClientHeight = 193
  ClientWidth = 401
  KeyPreview = True
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  ExplicitWidth = 407
  ExplicitHeight = 217
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 235
    Height = 13
    Caption = 'Auto completion for custom PHP classes (ACCPC)'
  end
  object Label2: TLabel
    Left = 24
    Top = 40
    Width = 265
    Height = 13
    Caption = 'Version {sAppVersionDisplay} (Win32, Unicode) for NPP'
  end
  object Label3: TLabel
    Left = 24
    Top = 67
    Width = 188
    Height = 13
    Caption = 'Written by Stanislav Eckert, 2013-2015,'
  end
  object Label4: TLabel
    Left = 218
    Top = 67
    Width = 160
    Height = 13
    Cursor = crHandPoint
    Caption = 'stane-npp@users.sourceforge.net'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = Label4Click
  end
  object Label5: TLabel
    Left = 24
    Top = 85
    Width = 216
    Height = 13
    Caption = 'Base plugin template by Damjan Zobo Cvetko'
  end
  object Label6: TLabel
    Left = 24
    Top = 112
    Width = 160
    Height = 13
    Caption = 'Additional third parties code'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 24
    Top = 131
    Width = 305
    Height = 13
    Caption = '- Virtual Treeview v5.5.3 (JAM Software, www.jam-software.com)'
  end
  object Button1: TButton
    Left = 163
    Top = 157
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
end
