object FormMain: TFormMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Cryptomeria GUI'
  ClientHeight = 240
  ClientWidth = 404
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxRSA: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 185
    Height = 234
    Align = alLeft
    Caption = 'RSA'
    TabOrder = 0
    ExplicitHeight = 224
    object ButtonGenerateKeyPair: TButton
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 175
      Height = 25
      Align = alTop
      Caption = 'Generate a key pair'
      TabOrder = 0
      OnClick = ButtonGenerateKeyPairClick
    end
    object ButtonEncryptFileRSA: TButton
      AlignWithMargins = True
      Left = 5
      Top = 49
      Width = 175
      Height = 25
      Align = alTop
      Caption = 'Encrypt a file'
      TabOrder = 1
      OnClick = ButtonEncryptFileRSAClick
    end
    object ButtonDecryptFileRSA: TButton
      AlignWithMargins = True
      Left = 5
      Top = 80
      Width = 175
      Height = 25
      Align = alTop
      Caption = 'Decrypt a file'
      TabOrder = 2
      OnClick = ButtonDecryptFileRSAClick
    end
  end
  object GroupBoxAES: TGroupBox
    AlignWithMargins = True
    Left = 194
    Top = 3
    Width = 191
    Height = 234
    Align = alLeft
    Caption = 'AES'
    TabOrder = 1
    ExplicitHeight = 224
    DesignSize = (
      191
      234)
    object ButtonDecryptAES: TButton
      Left = 10
      Top = 191
      Width = 164
      Height = 25
      Margins.Left = 10
      Margins.Right = 10
      Anchors = []
      Caption = 'Decrypt'
      TabOrder = 0
      OnClick = ButtonDecryptAESClick
      ExplicitTop = 182
    end
    object ButtonEncryptAES: TButton
      Left = 10
      Top = 158
      Width = 164
      Height = 25
      Margins.Left = 10
      Margins.Right = 10
      Anchors = []
      Caption = 'Encrypt'
      TabOrder = 1
      OnClick = ButtonEncryptAESClick
      ExplicitTop = 151
    end
    object LabeledEditCiphertext: TLabeledEdit
      Left = 10
      Top = 130
      Width = 164
      Height = 21
      Margins.Left = 10
      Margins.Top = 20
      Margins.Right = 10
      Anchors = []
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.Caption = 'Ciphertext'
      TabOrder = 2
      ExplicitTop = 124
    end
    object LabeledEditPassword: TLabeledEdit
      Left = 10
      Top = 38
      Width = 164
      Height = 21
      Margins.Left = 10
      Margins.Top = 20
      Margins.Right = 10
      Anchors = []
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      TabOrder = 3
      ExplicitTop = 36
    end
    object LabeledEditPlaintext: TLabeledEdit
      Left = 10
      Top = 84
      Width = 164
      Height = 21
      Margins.Left = 10
      Margins.Top = 20
      Margins.Right = 10
      Anchors = []
      EditLabel.Width = 42
      EditLabel.Height = 13
      EditLabel.Caption = 'Plaintext'
      TabOrder = 4
      ExplicitTop = 80
    end
  end
end
