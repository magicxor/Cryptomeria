unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls;

type
  TFormMain = class(TForm)
    GroupBoxRSA: TGroupBox;
    GroupBoxAES: TGroupBox;
    ButtonGenerateKeyPair: TButton;
    ButtonEncryptFileRSA: TButton;
    ButtonDecryptFileRSA: TButton;
    ButtonDecryptAES: TButton;
    ButtonEncryptAES: TButton;
    LabeledEditCiphertext: TLabeledEdit;
    LabeledEditPassword: TLabeledEdit;
    LabeledEditPlaintext: TLabeledEdit;
    procedure ButtonGenerateKeyPairClick(Sender: TObject);
    procedure ButtonEncryptFileRSAClick(Sender: TObject);
    procedure ButtonDecryptFileRSAClick(Sender: TObject);
    procedure ButtonEncryptAESClick(Sender: TObject);
    procedure ButtonDecryptAESClick(Sender: TObject);
  private const
    C_ALL_FILES_FILTER = 'All files|*';
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses CryptoSetRSA, CryptorRSA, CryptoSetAES, CryptorAES, KeyPairGenerator,
  Logger, TPLB3.Asymetric, System.Threading;

{$R *.dfm}

procedure TFormMain.ButtonDecryptFileRSAClick(Sender: TObject);
var
  OD: TOpenDialog;
  SD: TSaveDialog;
  Step1, Step2, Step3: boolean;
  LCryptoSet: ICryptoSetRSA;
  LCryptor: ICryptorRSA;
  PlantextFile, CiphertextFile, KeyFile: string;
begin
  Step1 := false;
  Step2 := false;
  Step3 := false;

  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := 'Private RSA key in the TPLockBox 3 format|*.lpv';
    if OD.Execute then
    begin
      Screen.Cursor := crHourGlass;
      Step1 := true;
      KeyFile := OD.FileName;
    end
    else
      Step1 := false;
  finally
    FreeAndNil(OD);
    Screen.Cursor := crDefault;
  end;

  if Step1 then
  begin
    OD := TOpenDialog.Create(Self);
    try
      OD.Filter := C_ALL_FILES_FILTER;
      if OD.Execute then
      begin
        Screen.Cursor := crHourGlass;
        Step2 := true;
        CiphertextFile := OD.FileName;
      end
      else
        Step2 := false;
    finally
      FreeAndNil(OD);
      Screen.Cursor := crDefault;
    end;

    if Step2 then
    begin
      SD := TSaveDialog.Create(nil);
      try
        SD.Filter := C_ALL_FILES_FILTER;
        if SD.Execute then
        begin
          Screen.Cursor := crHourGlass;
          Step3 := true;
          PlantextFile := SD.FileName;
        end;
      finally
        FreeAndNil(SD);
        Screen.Cursor := crDefault;
      end;

      if Step3 then
      begin
        LCryptoSet := TCryptoSetRSA.Create;
        LCryptor := TCryptorRSA.Create(LCryptoSet);
        LCryptor.DecryptFile(PlantextFile, CiphertextFile, KeyFile);
      end;
    end;
  end;
end;

procedure TFormMain.ButtonEncryptFileRSAClick(Sender: TObject);
var
  OD: TOpenDialog;
  SD: TSaveDialog;
  Step1, Step2, Step3: boolean;
  LCryptoSet: ICryptoSetRSA;
  LCryptor: ICryptorRSA;
  PlantextFile, CiphertextFile, KeyFile: string;
begin
  Step1 := false;
  Step2 := false;
  Step3 := false;

  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := 'Public RSA key in the TPLockBox 3 format|*.lpb';
    if OD.Execute then
    begin
      Screen.Cursor := crHourGlass;
      Step1 := true;
      KeyFile := OD.FileName;
    end
    else
      Step1 := false;
  finally
    FreeAndNil(OD);
    Screen.Cursor := crDefault;
  end;

  if Step1 then
  begin
    OD := TOpenDialog.Create(Self);
    try
      OD.Filter := C_ALL_FILES_FILTER;
      if OD.Execute then
      begin
        Screen.Cursor := crHourGlass;
        Step2 := true;
        PlantextFile := OD.FileName;
      end
      else
        Step2 := false;
    finally
      FreeAndNil(OD);
      Screen.Cursor := crDefault;
    end;

    if Step2 then
    begin
      SD := TSaveDialog.Create(nil);
      try
        SD.Filter := C_ALL_FILES_FILTER;
        if SD.Execute then
        begin
          Screen.Cursor := crHourGlass;
          Step3 := true;
          CiphertextFile := SD.FileName;
        end;
      finally
        FreeAndNil(SD);
        Screen.Cursor := crDefault;
      end;

      if Step3 then
      begin
        LCryptoSet := TCryptoSetRSA.Create;
        LCryptor := TCryptorRSA.Create(LCryptoSet);
        LCryptor.EncryptFile(PlantextFile, CiphertextFile, KeyFile);
      end;
    end;
  end;
end;

procedure TFormMain.ButtonGenerateKeyPairClick(Sender: TObject);
var
  SD: TSaveDialog;
  LKeyPairGenerator: IKeyPairGenerator;
  FuturePairGenerator: IFuture<IKeyPairGenerator>;
begin
  FuturePairGenerator := TTask.Future<IKeyPairGenerator>(
    function: IKeyPairGenerator
    var
      LCryptoSet: ICryptoSetRSA;
    begin
      LCryptoSet := TCryptoSetRSA.Create;
      Result := TKeyPairGenerator.Create(LCryptoSet);
    end);
  FuturePairGenerator.Start;

  SD := TSaveDialog.Create(nil);
  try
    SD.Filter := 'RSA keys in the TPLockBox 3 format|*.lpr;*.lpv;*.lpb';
    if SD.Execute then
    begin
      Screen.Cursor := crHourGlass;
      LKeyPairGenerator := FuturePairGenerator.Value;
      LKeyPairGenerator.SavePairToFile(SD.FileName + '.lpr');
      LKeyPairGenerator.SavePrivateKeyToFile(SD.FileName + '.lpv');
      LKeyPairGenerator.SavePublicKeyToFile(SD.FileName + '.lpb');
    end;
  finally
    FreeAndNil(SD);
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.ButtonEncryptAESClick(Sender: TObject);
var
  LCryptoSet: ICryptoSetAES;
  LCryptor: ICryptorAES;
  Plaintext, Ciphertext, Password: string;
begin
  Plaintext := LabeledEditPlaintext.Text;
  Ciphertext := LabeledEditCiphertext.Text;
  Password := LabeledEditPassword.Text;

  LCryptoSet := TCryptoSetAES.Create;
  LCryptor := TCryptorAES.Create(LCryptoSet);
  LCryptor.EncryptString(Plaintext, Ciphertext, Password);

  LabeledEditPlaintext.Text := Plaintext;
  LabeledEditCiphertext.Text := Ciphertext;
  LabeledEditPassword.Text := Password;
end;

procedure TFormMain.ButtonDecryptAESClick(Sender: TObject);
var
  LCryptoSet: ICryptoSetAES;
  LCryptor: ICryptorAES;
  Plaintext, Ciphertext, Password: string;
begin
  Plaintext := LabeledEditPlaintext.Text;
  Ciphertext := LabeledEditCiphertext.Text;
  Password := LabeledEditPassword.Text;

  LCryptoSet := TCryptoSetAES.Create;
  LCryptor := TCryptorAES.Create(LCryptoSet);
  LCryptor.DecryptString(Plaintext, Ciphertext, Password);

  LabeledEditPlaintext.Text := Plaintext;
  LabeledEditCiphertext.Text := Ciphertext;
  LabeledEditPassword.Text := Password;
end;

end.
