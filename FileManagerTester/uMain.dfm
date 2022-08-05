object fMain: TfMain
  Left = 0
  Top = 0
  Caption = 'fMain'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 538
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = '*'
  end
  object Button1: TButton
    Left = 552
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object bAddMod: TButton
    Left = 552
    Top = 39
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'bAddMod'
    TabOrder = 2
    OnClick = bAddModClick
  end
  object tv: TTreeView
    Left = 8
    Top = 40
    Width = 186
    Height = 251
    Anchors = [akLeft, akTop, akBottom]
    Indent = 19
    TabOrder = 3
  end
  object bRefresh: TButton
    Left = 552
    Top = 70
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'bRefresh'
    TabOrder = 4
    OnClick = bRefreshClick
  end
  object ListView1: TListView
    Left = 200
    Top = 40
    Width = 346
    Height = 251
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    TabOrder = 5
    OnDblClick = ListView1DblClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'lod'
    Filter = 'lod|*.lod'
    Left = 584
    Top = 136
  end
  object SaveDialog1: TSaveDialog
    Left = 560
    Top = 192
  end
end
