program CryptomeriaGUI;

uses
  Vcl.Forms, Spring.Container,
  uFormMain in 'uFormMain.pas' {FormMain},
  CryptorAES in '..\Source\CryptorAES.pas',
  CryptorRSA in '..\Source\CryptorRSA.pas',
  CryptoSetAES in '..\Source\CryptoSetAES.pas',
  CryptoSetRSA in '..\Source\CryptoSetRSA.pas',
  KeyPairGenerator in '..\Source\KeyPairGenerator.pas',
  uRegistrations in 'uRegistrations.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ELSE}
  ReportMemoryLeaksOnShutdown := false;
{$ENDIF}
  RegisterTypes(GlobalContainer);

  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
