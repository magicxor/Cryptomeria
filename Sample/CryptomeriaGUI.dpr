program CryptomeriaGUI;

uses
  Vcl.Forms,
  uFormMain in 'uFormMain.pas' {FormMain},
  CryptorAES in '..\Source\CryptorAES.pas',
  CryptorRSA in '..\Source\CryptorRSA.pas',
  CryptoSetAES in '..\Source\CryptoSetAES.pas',
  CryptoSetRSA in '..\Source\CryptoSetRSA.pas',
  KeyPairGenerator in '..\Source\KeyPairGenerator.pas',
  Logger in '..\Source\Logger.pas';

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ELSE}
  ReportMemoryLeaksOnShutdown := false;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := true;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
