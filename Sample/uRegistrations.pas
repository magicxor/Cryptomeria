unit uRegistrations;

interface

uses
  Spring.Container;

procedure RegisterTypes(const AContainer: TContainer);

implementation

uses
  CryptorAES, CryptorRSA, CryptoSetAES, CryptoSetRSA, KeyPairGenerator;

procedure RegisterTypes(const AContainer: TContainer);
begin
  AContainer.RegisterType<TCryptoSetAES>.Implements<ICryptoSetAES>.DelegateTo(
    function: TCryptoSetAES
    begin
      Result := TCryptoSetAES.Create(); // needs constructor execution!
    end);

  AContainer.RegisterType<TCryptoSetRSA>.Implements<ICryptoSetRSA>.DelegateTo(
    function: TCryptoSetRSA
    begin
      Result := TCryptoSetRSA.Create(); // needs constructor execution!
    end);

  AContainer.RegisterType<TKeyPairGenerator>.Implements<IKeyPairGenerator>;

  AContainer.RegisterType<TCryptorAES>.Implements<ICryptorAES>;
  AContainer.RegisterType<TCryptorRSA>.Implements<ICryptorRSA>;

  AContainer.Build;
end;

end.
