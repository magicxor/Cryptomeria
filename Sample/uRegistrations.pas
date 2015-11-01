unit uRegistrations;

interface

uses
  Spring.Container;

procedure RegisterTypes(const AContainer: TContainer);

implementation

uses
  MX.SimpleLogger, CryptorAES, CryptorRSA, CryptoSetAES, CryptoSetRSA, KeyPairGenerator;

procedure RegisterTypes(const AContainer: TContainer);
begin
  AContainer.RegisterType<TSimpleLogger>.AsSingleton.Implements<ILogger>; // can use Spring ILogger

  AContainer.RegisterType<TCryptoSetAES>.Implements<ICryptoSetAES>.DelegateTo(
    function: TCryptoSetAES
    begin
      Result := TCryptoSetAES.Create(GlobalContainer.Resolve<ILogger>); // constructor execution needed!
    end);

  AContainer.RegisterType<TCryptoSetRSA>.Implements<ICryptoSetRSA>.DelegateTo(
    function: TCryptoSetRSA
    begin
      Result := TCryptoSetRSA.Create(GlobalContainer.Resolve<ILogger>); // constructor execution needed!
    end);

  AContainer.RegisterType<TKeyPairGenerator>.Implements<IKeyPairGenerator>;

  AContainer.RegisterType<TCryptorAES>.Implements<ICryptorAES>;
  AContainer.RegisterType<TCryptorRSA>.Implements<ICryptorRSA>;

  AContainer.Build;
end;

end.
