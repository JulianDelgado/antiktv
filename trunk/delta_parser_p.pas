unit delta_parser_p;

{ contiguous single-pass from-the-scratch redesigned parser based on development
  gamma parser, which was intended to replace quite-monolitic beta parser }

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes;

type
  TDeltaParser = class(TStringList)
  public
    Position : integer;
    Stacker : TList;
    Recent : TStringList;
    SubLength : integer;

    constructor Create; virtual;
    destructor Destroy; override;

    function Next(ASubString : string) : integer;
    function Skip : integer;
    function Decp : integer;

    function Between(AFrom, ATo: integer): string;
    function Bet(AFrom,ATo : integer) : string;
    
    function RecentLength : integer;
  end;

implementation

{ TDeltaParser }

constructor TDeltaParser.Create;
begin
  Position := 1;
  Recent := TStringList.Create;
  Stacker := TList.Create;
end;

destructor TDeltaParser.Destroy;
begin
  Recent.Free;
  Stacker.Free;
  inherited;
end;

function TDeltaParser.Between(AFrom, ATo: integer): string;
begin
  result := copy(Text,AFrom,ATo-AFrom+1);
end;

function TDeltaParser.Bet(AFrom, ATo: integer): string;
var f,t : integer;
begin
  f := integer(Stacker[AFrom]);
  t := integer(Stacker[ATo]);
  result := Between(f,t);
end;

function TDeltaParser.Decp: integer;
begin
  Position := Position - 1;
  Stacker.Insert(0,pointer(position));
  result := position;
end;

function TDeltaParser.Next(ASubString: string): integer;
var a : integer;
begin
  result := 0;
  a := pos(ASubString,copy(Text,Position,maxint));
  if a>0 then
  begin
    result := position + a - 1;
    Position := result;
    Recent.Insert(0,ASubString);
    Stacker.Insert(0,pointer(position));
  end;
end;

function TDeltaParser.Skip: integer;
begin
  if Recent.Count>0 then
    Position := Position + length(Recent[0]);
  result := Position;
  Stacker.Insert(0,pointer(position));
end;

function TDeltaParser.RecentLength : integer;
begin
  result := 0;
  if Recent.Count > 0 then
    result := length(Recent[0]);
end;

end.
