unit CryptorRSA;

interface

uses System.Classes, CryptoSetRSA;

type
  ICryptorRSA = interface
    ['{21B0C6BD-CBE8-4BE5-881B-737099CAC7F4}']
    procedure EncryptStream(APlaintextStream, ACiphertextStream, APublicKeyStream: TStream); overload;
    procedure EncryptStream(APlaintextStream, ACiphertextStream: TStream; const APublicKeyPath: string); overload;

    procedure EncryptFile(const APlaintextFilePath, ACiphertextFilePath: string; APublicKeyStream: TStream); overload;
    procedure EncryptFile(const APlaintextFilePath, ACiphertextFilePath, APublicKeyPath: string); overload;

    procedure EncryptString(const APlaintextString: string; var ACiphertextString: string; APublicKeyStream: TStream); overload;
    procedure EncryptString(const APlaintextString: string; var ACiphertextString: string; const APublicKeyPath: string); overload;

    procedure DecryptStream(APlaintextStream, ACiphertextStream, APrivateKeyStream: TStream); overload;
    procedure DecryptStream(APlaintextStream, ACiphertextStream: TStream; const APrivateKeyPath: string); overload;

    procedure DecryptFile(const APlaintextFilePath, ACiphertextFilePath: string; APrivateKeyStream: TStream); overload;
    procedure DecryptFile(const APlaintextFilePath, ACiphertextFilePath, APrivateKeyPath: string); overload;

    procedure DecryptString(var APlaintextString: string; const ACiphertextString: string; APrivateKeyStream: TStream); overload;
    procedure DecryptString(var APlaintextString: string; const ACiphertextString: string; const APrivateKeyPath: string); overload;
  end;

  TCryptorRSA = class(TInterfacedObject, ICryptorRSA)
  private
    FCryptoSetRSA: ICryptoSetRSA;
  public
    procedure EncryptStream(APlaintextStream, ACiphertextStream, APublicKeyStream: TStream); overload;
    procedure EncryptStream(APlaintextStream, ACiphertextStream: TStream; const APublicKeyPath: string); overload;

    procedure EncryptFile(const APlaintextFilePath, ACiphertextFilePath: string; APublicKeyStream: TStream); overload;
    procedure EncryptFile(const APlaintextFilePath, ACiphertextFilePath, APublicKeyPath: string); overload;

    procedure EncryptString(const APlaintextString: string; var ACiphertextString: string; APublicKeyStream: TStream); overload;
    procedure EncryptString(const APlaintextString: string; var ACiphertextString: string; const APublicKeyPath: string); overload;

    procedure DecryptStream(APlaintextStream, ACiphertextStream, APrivateKeyStream: TStream); overload;
    procedure DecryptStream(APlaintextStream, ACiphertextStream: TStream; const APrivateKeyPath: string); overload;

    procedure DecryptFile(const APlaintextFilePath, ACiphertextFilePath: string; APrivateKeyStream: TStream); overload;
    procedure DecryptFile(const APlaintextFilePath, ACiphertextFilePath, APrivateKeyPath: string); overload;

    procedure DecryptString(var APlaintextString: string; const ACiphertextString: string; APrivateKeyStream: TStream); overload;
    procedure DecryptString(var APlaintextString: string; const ACiphertextString: string; const APrivateKeyPath: string); overload;

    constructor Create(ACryptoSetRSA: ICryptoSetRSA);
  end;

implementation

uses System.SysUtils, System.IOUtils, TPLB3.Constants, TPLB3.Asymetric;

{ TCryptorRSA }

constructor TCryptorRSA.Create(ACryptoSetRSA: ICryptoSetRSA);
begin
  FCryptoSetRSA := ACryptoSetRSA;

  Assert((FCryptoSetRSA.Codec <> nil) and (FCryptoSetRSA.Codec.StreamCipherId = RSA_ProgId));
end;

procedure TCryptorRSA.DecryptFile(const APlaintextFilePath, ACiphertextFilePath: string;
  APrivateKeyStream: TStream);
begin
  if (TFile.Exists(ACiphertextFilePath) and not(APrivateKeyStream.Size=0)) then
  begin
    APrivateKeyStream.Position := 0;
    FCryptoSetRSA.Signatory.LoadKeysFromStream(APrivateKeyStream, [partPrivate]);
    FCryptoSetRSA.Codec.DecryptFile(APlaintextFilePath, ACiphertextFilePath);

    FCryptoSetRSA.Codec.Burn;
  end;
end;

procedure TCryptorRSA.DecryptFile(const APlaintextFilePath, ACiphertextFilePath,
  APrivateKeyPath: string);
var
  KeyStream: TStringStream;
begin
  if (TFile.Exists(ACiphertextFilePath) and TFile.Exists(APrivateKeyPath)) then
  begin
    KeyStream := TStringStream.Create;
    try
      KeyStream.LoadFromFile(APrivateKeyPath);
      DecryptFile(APlaintextFilePath, ACiphertextFilePath, KeyStream);
    finally
      FreeAndNil(KeyStream);
    end;
  end;
end;

procedure TCryptorRSA.DecryptString(var APlaintextString: string; const ACiphertextString,
  APrivateKeyPath: string);
var
  KeyStream: TStringStream;
begin
  if (not(ACiphertextString.IsEmpty) and TFile.Exists(APrivateKeyPath)) then
  begin
    KeyStream := TStringStream.Create;
    try
      KeyStream.LoadFromFile(APrivateKeyPath);
      DecryptString(APlaintextString, ACiphertextString, KeyStream);
    finally
      FreeAndNil(KeyStream);
    end;
  end;
end;

procedure TCryptorRSA.DecryptString(var APlaintextString: string; const ACiphertextString: string;
  APrivateKeyStream: TStream);
begin
  if (not(ACiphertextString.IsEmpty) and not(APrivateKeyStream.Size = 0)) then
  begin
    APrivateKeyStream.Position := 0;
    FCryptoSetRSA.Signatory.LoadKeysFromStream(APrivateKeyStream, [partPrivate]);
    FCryptoSetRSA.Codec.DecryptString(APlaintextString, ACiphertextString);

    FCryptoSetRSA.Codec.Burn;
  end;
end;

procedure TCryptorRSA.DecryptStream(APlaintextStream, ACiphertextStream: TStream;
  const APrivateKeyPath: string);
var
  KeyStream: TStringStream;
begin
  if (not(ACiphertextStream.Size=0) and TFile.Exists(APrivateKeyPath)) then
  begin
    KeyStream := TStringStream.Create;
    try
      KeyStream.LoadFromFile(APrivateKeyPath);
      DecryptStream(APlaintextStream, ACiphertextStream, KeyStream);
    finally
      FreeAndNil(KeyStream);
    end;
  end;
end;

procedure TCryptorRSA.DecryptStream(APlaintextStream, ACiphertextStream,
  APrivateKeyStream: TStream);
begin
  if (not(ACiphertextStream.Size=0) and not(APrivateKeyStream.Size=0)) then
  begin
    APlaintextStream.Position := 0;
    ACiphertextStream.Position := 0;
    APrivateKeyStream.Position := 0;
    FCryptoSetRSA.Signatory.LoadKeysFromStream(APrivateKeyStream, [partPrivate]);
    FCryptoSetRSA.Codec.DecryptStream(APlaintextStream, ACiphertextStream);

    FCryptoSetRSA.Codec.Burn;
  end;
end;

procedure TCryptorRSA.EncryptFile(const APlaintextFilePath, ACiphertextFilePath: string;
  APublicKeyStream: TStream);
begin
  if (TFile.Exists(APlaintextFilePath) and not(APublicKeyStream.Size=0)) then
  begin
    APublicKeyStream.Position := 0;
    FCryptoSetRSA.Signatory.LoadKeysFromStream(APublicKeyStream, [partPublic]);
    FCryptoSetRSA.Codec.EncryptFile(APlaintextFilePath, ACiphertextFilePath);

    FCryptoSetRSA.Codec.Burn;
  end;
end;

procedure TCryptorRSA.EncryptFile(const APlaintextFilePath, ACiphertextFilePath,
  APublicKeyPath: string);
var
  KeyStream: TStringStream;
begin
  if (TFile.Exists(APlaintextFilePath) and TFile.Exists(APublicKeyPath)) then
  begin
    KeyStream := TStringStream.Create;
    try
      KeyStream.LoadFromFile(APublicKeyPath);
      EncryptFile(APlaintextFilePath, ACiphertextFilePath, KeyStream);
    finally
      FreeAndNil(KeyStream);
    end;
  end;
end;

procedure TCryptorRSA.EncryptString(const APlaintextString: string; var ACiphertextString: string;
  const APublicKeyPath: string);
var
  KeyStream: TStringStream;
begin
  if (not(APlaintextString.IsEmpty) and TFile.Exists(APublicKeyPath)) then
  begin
    KeyStream := TStringStream.Create;
    try
      KeyStream.LoadFromFile(APublicKeyPath);
      EncryptString(APlaintextString, ACiphertextString, KeyStream);
    finally
      FreeAndNil(KeyStream);
    end;
  end;
end;

procedure TCryptorRSA.EncryptString(const APlaintextString: string; var ACiphertextString: string;
  APublicKeyStream: TStream);
begin
  if (not(APlaintextString.IsEmpty) and not(APublicKeyStream.Size=0)) then
  begin
    APublicKeyStream.Position := 0;
    FCryptoSetRSA.Signatory.LoadKeysFromStream(APublicKeyStream, [partPublic]);
    FCryptoSetRSA.Codec.EncryptString(APlaintextString, ACiphertextString);

    FCryptoSetRSA.Codec.Burn;
  end;
end;

procedure TCryptorRSA.EncryptStream(APlaintextStream, ACiphertextStream: TStream;
  const APublicKeyPath: string);
var
  KeyStream: TStringStream;
begin
  if (not(APlaintextStream.Size=0) and TFile.Exists(APublicKeyPath)) then
  begin
    KeyStream := TStringStream.Create;
    try
      KeyStream.LoadFromFile(APublicKeyPath);
      EncryptStream(APlaintextStream, ACiphertextStream, KeyStream);
    finally
      FreeAndNil(KeyStream);
    end;
  end;
end;

procedure TCryptorRSA.EncryptStream(APlaintextStream, ACiphertextStream, APublicKeyStream: TStream);
begin
  if (not(APlaintextStream.Size=0) and not(APublicKeyStream.Size=0)) then
  begin
    APlaintextStream.Position := 0;
    ACiphertextStream.Position := 0;
    APublicKeyStream.Position := 0;
    FCryptoSetRSA.Signatory.LoadKeysFromStream(APublicKeyStream, [partPublic]);
    FCryptoSetRSA.Codec.EncryptStream(APlaintextStream, ACiphertextStream);

    FCryptoSetRSA.Codec.Burn;
  end;
end;

end.
