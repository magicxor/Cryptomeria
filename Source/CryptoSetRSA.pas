unit CryptoSetRSA;

interface

uses TPLB3.CryptographicLibrary, TPLB3.Codec, TPLB3.Signatory;

type
  ICryptoSetRSA = interface
    ['{9227E7DF-A0C4-4E29-BC3A-C3D26AEBD0CD}']
    function GetCodec: TCodec;
    property Codec: TCodec read GetCodec;

    function GetSignatory: TSignatory;
    property Signatory: TSignatory read GetSignatory;
  end;

  TCryptoSetRSA = class(TInterfacedObject, ICryptoSetRSA)
  private
    FCryptographicLibrary: TCryptographicLibrary;
    FCodec: TCodec;
    FSignatory: TSignatory;
  public const
    DefaultKeySize = 1024;
  public
    function GetCodec: TCodec;
    property Codec: TCodec read GetCodec;

    function GetSignatory: TSignatory;
    property Signatory: TSignatory read GetSignatory;

    constructor Create(AKeySize: integer = DefaultKeySize);
    destructor Destroy; override;
  end;

implementation

uses System.SysUtils, TPLB3.Constants, TPLB3.Random;

{ TCryptoSetRSA }

constructor TCryptoSetRSA.Create(AKeySize: integer = DefaultKeySize);
begin
  inherited Create;

  TRandomStream.Instance.Randomize();
  FCryptographicLibrary := TCryptographicLibrary.Create(nil);
  try
    FCodec := TCodec.Create(nil);
    try
      FCodec.CryptoLibrary := FCryptographicLibrary;
      FCodec.StreamCipherId := RSA_ProgId;
      FCodec.ChainModeId := CBC_ProgId;
      FCodec.AsymetricKeySizeInBits := AKeySize;
      FSignatory := TSignatory.Create(nil);
      try
        FSignatory.Codec := FCodec;
      except
        FreeAndNil(FSignatory);
      end;
    except
      FreeAndNil(FCodec);
    end;
  except
    FreeAndNil(FCryptographicLibrary);
  end;
end;

destructor TCryptoSetRSA.Destroy;
begin
  if FSignatory <> nil then
    FreeAndNil(FSignatory);

  if FCodec <> nil then
    FreeAndNil(FCodec);

  if FCryptographicLibrary <> nil then
    FreeAndNil(FCryptographicLibrary);

  inherited;
end;

function TCryptoSetRSA.GetCodec: TCodec;
begin
  Assert(FCodec <> nil);
  Result := FCodec;
end;

function TCryptoSetRSA.GetSignatory: TSignatory;
begin
  Assert(FSignatory <> nil);
  Result := FSignatory;
end;

end.
