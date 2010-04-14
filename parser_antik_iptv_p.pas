unit parser_antik_iptv_p;

{*
Parser webstranky IPTV antik.sk
}

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, delta_parser_p;

function ParserAntikIpTvExecute(AFileName : string; ATimeList, ATitleList, AIdList : TStringList) : boolean;

implementation

{*
Parsuje html stranku AFileName s programom 1 stanice
Vysledok zapisuje do stringlistov ATimeList (casi relacii), ATitleList (nazvy
relacii) a AIdList (id programov, pouziva sa na stahovanie videa)
}
function ParserAntikIpTvExecute(AFileName : string; ATimeList, ATitleList, AIdList : TStringList) : boolean;
var p : TDeltaParser;
    a,b : integer;
    s : string;
begin
  ATimeList.Clear;
  ATitleList.Clear;
  AIdList.Clear;
  p := TDeltaParser.Create;
  p.LoadFromFile(AFileName);  
  repeat
    // hladame cas
    a := p.Next('      &nbsp;');
    a := a+p.RecentLength;
    b := p.Next('    </td>');
    b := b;
    s := p.Between(a,b);     
    if (a<0)or(b<0)or(length(s)<=4) then
      break;
    //writeln('--CAS ... a=',a,' b=',b,' text=',s,' ls=',length(s));    
    ATimeList.Add(s);
    //writeln('cas=',s);
    // hladame nazov programu
    p.Next('td width="100%" align="left" valign="top"');
    a := p.Next('>')+6;
    b := p.Next('</td>')-1;
    if (a<0)or(b<0) then
      break;
    s := p.Between(a,b);
    if pos('>',s) <= 0 then
    begin
      //writeln('a=',a,' b=',b,' text=',s);
      ATitleList.Add(s);
      //writeln('program=',s);
      // hladame id programu
      a := p.Next('show_detail_');
      a := a + p.RecentLength;
      b := p.Next('"');
      b := b - p.RecentLength;
      s := p.Between(a,b);
      AIdList.Add(s)
      //writeln('id=',s);
   end;    
   //writeln;
  until false;
  p.Free;
end;

end.
