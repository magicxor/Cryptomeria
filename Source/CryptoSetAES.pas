unit CryptoSetAES;

interface

uses TPLB3.CryptographicLibrary, TPLB3.Codec, {$INCLUDE LoggerImpl.inc};

type
  ICryptoSetAES = interface
    ['{63E985AA-07AB-4E76-A3DA-9CF18125DA3C}']
    function GetCodec: TCodec;
    property Codec: TCodec read GetCodec;
  end;

  TCryptoSetAES = class(TInterfacedObject, ICryptoSetAES)
  private
    FCryptographicLibrary: TCryptographicLibrary;
    FCodec: TCodec;
    FLogger: ILogger;
  public const
    DefaultKeySize = 256;
  public
    function GetCodec: TCodec;
    property Codec: TCodec read GetCodec;

    constructor Create(ALogger: ILogger; AKeySize: integer = DefaultKeySize);
    destructor Destroy; override;
  end;

implementation

uses System.SysUtils, TPLB3.Constants, TPLB3.Random;

{ TCryptoSetAES }

constructor TCryptoSetAES.Create(ALogger: ILogger; AKeySize: integer = DefaultKeySize);
begin
  inherited Create;

  FLogger := ALogger;

  TRandomStream.Instance.Randomize();
  FCryptographicLibrary := TCryptographicLibrary.Create(nil);
  try
    FCodec := TCodec.Create(nil);
    try
      FCodec.CryptoLibrary := FCryptographicLibrary;
      FCodec.StreamCipherId := BlockCipher_ProgId;
      FCodec.BlockCipherId := Format(AES_ProgId, [AKeySize]);
      FCodec.ChainModeId := CBC_ProgId;
    except
      FreeAndNil(FCodec);
      FLogger.Fatal('An error occurred during the interaction with FCodec');
    end;
  except
    FreeAndNil(FCryptographicLibrary);
    FLogger.Fatal('An error occurred during the interaction with FCryptographicLibrary');
  end;
end;

destructor TCryptoSetAES.Destroy;
begin
  if FCodec <> nil then
    FreeAndNil(FCodec)
  else
    FLogger.Error('FCodec = nil');

  if FCryptographicLibrary <> nil then
    FreeAndNil(FCryptographicLibrary)
  else
    FLogger.Error('FCryptographicLibrary = nil');

  inherited;
end;

function TCryptoSetAES.GetCodec: TCodec;
begin
  Assert(FCodec <> nil);
  Result := FCodec;
end;

end.
