unit Logger;

interface

type
  TLogger = class
  public
    class procedure LogEvent(const ALogStr: string);
    class procedure LogWarning(const ALogStr: string);
    class procedure LogError(const ALogStr: string);
  end;

const
{$IFDEF DEBUG}
  CDebug = true;
{$ELSE}
  CDebug = false;
{$ENDIF}

implementation

uses System.SysUtils, Winapi.Windows;

const
  CDateTimeFormat = 'dd.mm.yyyy hh:nn:ss';
  { TLogger }

class procedure TLogger.LogEvent(const ALogStr: string);
begin
  if CDebug then
    OutputDebugString(PWideChar(string.Join(' ', [FormatDateTime(CDateTimeFormat, Now), 'Event:',
      ALogStr])));
end;

class procedure TLogger.LogWarning(const ALogStr: string);
begin
  if CDebug then
    OutputDebugString(PWideChar(string.Join(' ', [FormatDateTime(CDateTimeFormat, Now), 'Warning:',
      ALogStr])));
end;

class procedure TLogger.LogError(const ALogStr: string);
begin
  if CDebug then
    OutputDebugString(PWideChar(string.Join(' ', [FormatDateTime(CDateTimeFormat, Now), 'Error:',
      ALogStr])));
end;

end.
