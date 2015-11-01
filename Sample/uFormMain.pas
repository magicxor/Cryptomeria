unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.WinXCtrls,
  CryptorRSA, CryptorAES, KeyPairGenerator;

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
    procedure FormCreate(Sender: TObject);
  private
    FCryptorRSA: ICryptorRSA;
    FCryptorAES: ICryptorAES;
    FKeyPairGen: IKeyPairGenerator;

  const
    C_ALL_FILES_FILTER = 'All files|*';
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses System.Threading, Spring.Container;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FCryptorRSA := GlobalContainer.Resolve<ICryptorRSA>;
  FCryptorAES := GlobalContainer.Resolve<ICryptorAES>;
  FKeyPairGen := GlobalContainer.Resolve<IKeyPairGenerator>;
end;

procedure TFormMain.ButtonDecryptFileRSAClick(Sender: TObject);
var
  OD: TOpenDialog;
  SD: TSaveDialog;
  Step1, Step2, Step3: boolean;
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
        FCryptorRSA.DecryptFile(PlantextFile, CiphertextFile, KeyFile);
      end;
    end;
  end;
end;

procedure TFormMain.ButtonEncryptFileRSAClick(Sender: TObject);
var
  OD: TOpenDialog;
  SD: TSaveDialog;
  Step1, Step2, Step3: boolean;
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
        FCryptorRSA.EncryptFile(PlantextFile, CiphertextFile, KeyFile);
      end;
    end;
  end;
end;

procedure TFormMain.ButtonGenerateKeyPairClick(Sender: TObject);
var
  SD: TSaveDialog;
  FuturePairGenerator: IFuture<boolean>;
  FutureCompleted: boolean;
begin
  FuturePairGenerator := TTask.Future<boolean>(
    function: boolean
    begin
      FKeyPairGen.GenerateNewKeyPair;
      Result := true;
    end);
  FuturePairGenerator.Start;

  SD := TSaveDialog.Create(nil);
  try
    SD.Filter := 'RSA keys in the TPLockBox 3 format|*.lpr;*.lpv;*.lpb';
    if SD.Execute then
    begin
      Screen.Cursor := crHourGlass;
      FutureCompleted := FuturePairGenerator.Value; // <== wait for the future execution completed
      FKeyPairGen.SavePairToFile(SD.FileName + '.lpr');
      FKeyPairGen.SavePrivateKeyToFile(SD.FileName + '.lpv');
      FKeyPairGen.SavePublicKeyToFile(SD.FileName + '.lpb');
    end;
  finally
    FreeAndNil(SD);
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.ButtonEncryptAESClick(Sender: TObject);
var
  Plaintext, Ciphertext, Password: string;
begin
  Plaintext := LabeledEditPlaintext.Text;
  Ciphertext := LabeledEditCiphertext.Text;
  Password := LabeledEditPassword.Text;

  FCryptorAES.EncryptString(Plaintext, Ciphertext, Password);

  LabeledEditPlaintext.Text := Plaintext;
  LabeledEditCiphertext.Text := Ciphertext;
  LabeledEditPassword.Text := Password;
end;

procedure TFormMain.ButtonDecryptAESClick(Sender: TObject);
var
  Plaintext, Ciphertext, Password: string;
begin
  Plaintext := LabeledEditPlaintext.Text;
  Ciphertext := LabeledEditCiphertext.Text;
  Password := LabeledEditPassword.Text;

  FCryptorAES.DecryptString(Plaintext, Ciphertext, Password);

  LabeledEditPlaintext.Text := Plaintext;
  LabeledEditCiphertext.Text := Ciphertext;
  LabeledEditPassword.Text := Password;
end;

end.
