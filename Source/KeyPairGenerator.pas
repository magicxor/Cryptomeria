unit KeyPairGenerator;

interface

uses System.Classes, TPLB3.Asymetric, CryptoSetRSA;

type
  IKeyPairGenerator = interface
    ['{5B6C0EB1-57D7-4D59-99D3-381FDFA6EE4E}']
    procedure SavePairToFile(AFilePath: string);
    procedure SavePrivateKeyToFile(AFilePath: string);
    procedure SavePublicKeyToFile(AFilePath: string);
    procedure SavePairToStream(var KeyStream: TStream);
    procedure SavePrivateKeyToStream(var KeyStream: TStream);
    procedure SavePublicKeyToStream(var KeyStream: TStream);
  end;

  TKeyPairGenerator = class(TInterfacedObject, IKeyPairGenerator)
  private
    FCryptoSet: ICryptoSetRSA;
    procedure SaveToFile(AFilePath: string; KeyStoragePartSet: TKeyStoragePartSet);
    procedure SaveToStream(var KeyStream: TStream; KeyStoragePartSet: TKeyStoragePartSet);
  public
    procedure SavePairToFile(AFilePath: string);
    procedure SavePrivateKeyToFile(AFilePath: string);
    procedure SavePublicKeyToFile(AFilePath: string);
    procedure SavePairToStream(var KeyStream: TStream);
    procedure SavePrivateKeyToStream(var KeyStream: TStream);
    procedure SavePublicKeyToStream(var KeyStream: TStream);
    constructor Create(ACryptoSet: ICryptoSetRSA);
  end;

implementation

uses TPLB3.CryptographicLibrary, TPLB3.Codec, TPLB3.Signatory, TPLB3.Constants, System.SysUtils,
  Logger;

{ TKeyPairGenerator }

procedure TKeyPairGenerator.SavePrivateKeyToStream(var KeyStream: TStream);
begin
  SaveToStream(KeyStream, [partPrivate]);
end;

procedure TKeyPairGenerator.SavePublicKeyToStream(var KeyStream: TStream);
begin
  SaveToStream(KeyStream, [partPublic]);
end;

procedure TKeyPairGenerator.SavePairToStream(var KeyStream: TStream);
begin
  SaveToStream(KeyStream, [partPublic, partPrivate]);
end;

procedure TKeyPairGenerator.SavePrivateKeyToFile(AFilePath: string);
begin
  SaveToFile(AFilePath, [partPrivate]);
end;

procedure TKeyPairGenerator.SavePublicKeyToFile(AFilePath: string);
begin
  SaveToFile(AFilePath, [partPublic]);
end;

procedure TKeyPairGenerator.SavePairToFile(AFilePath: string);
begin
  SaveToFile(AFilePath, [partPublic, partPrivate]);
end;

procedure TKeyPairGenerator.SaveToFile(AFilePath: string; KeyStoragePartSet: TKeyStoragePartSet);
var
  LKeyStream: TStringStream;
begin
  Assert(FCryptoSet <> nil);
  LKeyStream := TStringStream.Create(AFilePath);
  try
    LKeyStream.Position := 0;
    FCryptoSet.Signatory.StoreKeysToStream(LKeyStream, KeyStoragePartSet);
    LKeyStream.SaveToFile(AFilePath);
  finally
    FreeAndNil(LKeyStream);
  end;
end;

procedure TKeyPairGenerator.SaveToStream(var KeyStream: TStream; KeyStoragePartSet: TKeyStoragePartSet);
begin
  Assert((KeyStream <> nil) and (FCryptoSet <> nil));
  KeyStream.Position := 0;
  FCryptoSet.Signatory.StoreKeysToStream(KeyStream, KeyStoragePartSet);
end;

constructor TKeyPairGenerator.Create(ACryptoSet: ICryptoSetRSA);
begin
  inherited Create;

  FCryptoSet := ACryptoSet;

  Assert((FCryptoSet <> nil) and (FCryptoSet.Codec <> nil) and (FCryptoSet.Signatory <> nil));

  if FCryptoSet.Signatory.GenerateKeys then
    TLogger.LogEvent('RSA keys successfully created.')
  else if FCryptoSet.Signatory.Codec.isUserAborted then
    TLogger.LogEvent('RSA keys generation aborted at user request.')
  else
    TLogger.LogError('RSA keys generation failed.');
end;

end.
