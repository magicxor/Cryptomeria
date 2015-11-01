unit CryptorAES;

interface

uses System.Classes, CryptoSetAES;

type
  ICryptorAES = interface
    ['{13CB7544-CC4C-4A06-8FD8-C3952A844721}']
    procedure EncryptStream(APlaintextStream, ACiphertextStream: TStream; const APassword: string);
    procedure EncryptFile(const APlaintextFilePath, ACiphertextFilePath, APassword: string);
    procedure EncryptString(const APlaintextString: string; var ACiphertextString: string; const APassword: string);

    procedure DecryptStream(APlaintextStream, ACiphertextStream: TStream; const APassword: string);
    procedure DecryptFile(const APlaintextFilePath, ACiphertextFilePath, APassword: string);
    procedure DecryptString(var APlaintextString: string; const ACiphertextString: string; const APassword: string);
  end;

  TCryptorAES = class(TInterfacedObject, ICryptorAES)
  private
    FCryptoSetAES: ICryptoSetAES;
  public
    procedure EncryptStream(APlaintextStream, ACiphertextStream: TStream; const APassword: string);
    procedure EncryptFile(const APlaintextFilePath, ACiphertextFilePath, APassword: string);
    procedure EncryptString(const APlaintextString: string; var ACiphertextString: string; const APassword: string);

    procedure DecryptStream(APlaintextStream, ACiphertextStream: TStream; const APassword: string);
    procedure DecryptFile(const APlaintextFilePath, ACiphertextFilePath, APassword: string);
    procedure DecryptString(var APlaintextString: string; const ACiphertextString: string; const APassword: string);

    constructor Create(ACryptoSetAES: ICryptoSetAES);
  end;

implementation

uses System.SysUtils, System.IOUtils, TPLB3.Constants;

{ TCryptorAES }

constructor TCryptorAES.Create(ACryptoSetAES: ICryptoSetAES);
begin
  FCryptoSetAES := ACryptoSetAES;

  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));
end;

procedure TCryptorAES.DecryptFile(const APlaintextFilePath, ACiphertextFilePath, APassword: string);
begin
  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));

  if (TFile.Exists(ACiphertextFilePath) and not(APassword.IsEmpty)) then
  begin
    FCryptoSetAES.Codec.Password := APassword;
    FCryptoSetAES.Codec.DecryptFile(APlaintextFilePath, ACiphertextFilePath);

    FCryptoSetAES.Codec.Burn;
  end;
end;

procedure TCryptorAES.DecryptStream(APlaintextStream, ACiphertextStream: TStream; const APassword: string);
begin
  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));

  if (not(ACiphertextStream.Size=0) and not(APassword.IsEmpty)) then
  begin
    APlaintextStream.Position := 0;
    ACiphertextStream.Position := 0;
    FCryptoSetAES.Codec.Password := APassword;
    FCryptoSetAES.Codec.DecryptStream(APlaintextStream, ACiphertextStream);

    FCryptoSetAES.Codec.Burn;
  end;
end;

procedure TCryptorAES.DecryptString(var APlaintextString: string; const ACiphertextString,
  APassword: string);
begin
  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));

  if (not(ACiphertextString.IsEmpty) and not(APassword.IsEmpty)) then
  begin
    FCryptoSetAES.Codec.Password := APassword;
    FCryptoSetAES.Codec.DecryptString(APlaintextString, ACiphertextString);

    FCryptoSetAES.Codec.Burn;
  end;
end;

procedure TCryptorAES.EncryptFile(const APlaintextFilePath, ACiphertextFilePath, APassword: string);
begin
  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));

  if (TFile.Exists(APlaintextFilePath) and not(APassword.IsEmpty)) then
  begin
    FCryptoSetAES.Codec.Password := APassword;
    FCryptoSetAES.Codec.EncryptFile(APlaintextFilePath, ACiphertextFilePath);

    FCryptoSetAES.Codec.Burn;
  end;
end;

procedure TCryptorAES.EncryptStream(APlaintextStream, ACiphertextStream: TStream; const APassword: string);
begin
  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));

  if (not(APlaintextStream.Size=0) and not(APassword.IsEmpty)) then
  begin
    APlaintextStream.Position := 0;
    ACiphertextStream.Position := 0;
    FCryptoSetAES.Codec.Password := APassword;
    FCryptoSetAES.Codec.EncryptStream(APlaintextStream, ACiphertextStream);

    FCryptoSetAES.Codec.Burn;
  end;
end;

procedure TCryptorAES.EncryptString(const APlaintextString: string; var ACiphertextString: string;
  const APassword: string);
begin
  Assert((FCryptoSetAES.Codec <> nil) and (FCryptoSetAES.Codec.StreamCipherId = BlockCipher_ProgId));

  if (not(APlaintextString.IsEmpty) and not(APassword.IsEmpty)) then
  begin
    FCryptoSetAES.Codec.Password := APassword;
    FCryptoSetAES.Codec.EncryptString(APlaintextString, ACiphertextString);

    FCryptoSetAES.Codec.Burn;
  end;
end;

end.
