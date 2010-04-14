unit piped_process_p;

{*
Spustanie procesov a zachytavanie vystupu
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Process, Dialogs;
  
type TPipedProcessChunkParser = procedure (AOutput : TStrings; AChunk : string) of object;

procedure PipedProcessExecute(ACommandLine : string; AOutput : TStrings; AChunkParser : TPipedProcessChunkParser);

implementation

procedure PipedProcessExecute(ACommandLine : string; AOutput : TStrings; AChunkParser : TPipedProcessChunkParser);
const
  BUFFER_SIZE = 2048;
var
  p : TProcess;
  buffer : array of char;
  n,i : longint;
  poms, chunk : string;
begin
  P := TProcess.Create(nil);
  {$ifdef WINDOWS}
  P.ShowWindow := swoHIDE;
  {$endif}
  P.CommandLine := ACommandLine;
  P.Options := [poUsePipes];
  P.Execute;
  setlength(buffer,BUFFER_SIZE);
  poms := '';
  n := 1;
  while P.Running or (n > 0) do
  begin
    n := P.Output.Read(buffer[0], BUFFER_SIZE);
    if n > 0 then
    begin
      chunk := '';
      for i := 0 to n-1 do
        chunk := chunk + buffer[i];
      poms := poms + chunk;
      if assigned(AOutput) then
        AOutput.text := poms;
      if assigned(AOutput) and assigned(AChunkParser) then
        AChunkParser(AOutput,chunk);
    end else
      sleep(100)
  end;
  setlength(buffer,0);
  p.Free;
end;

end.

