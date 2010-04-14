unit main_form_f;

{
In windows with lazarus <= 25 use -dWindowsUnicodeSupport to rebuild lazarus with utf support
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, parser_antik_iptv_p, piped_process_p, Process, EditBtn, ExtDlgs, DateUtils,
  Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnDatum1: TButton;
    CalendarDialog1: TCalendarDialog;
    cbStanica1: TComboBox;
    lbZoznam1: TListBox;
    mnuKopirovatOdkaz1: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure btnDatum1Click(Sender: TObject);
    procedure cbStanica1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbZoznam1DblClick(Sender: TObject);
    procedure mnuKopirovatOdkaz1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    TimeList, TitleList, IdList : TStringList;
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.cbStanica1Change(Sender: TObject);
var i : integer;
    prefixtmp : string;
begin
  // refresh zoznamu programov
  // vymazeme list
  lbZoznam1.Items.Clear;
  IdList.Clear;
  lbZoznam1.Items.Add('Čakajte prosím...');
  // zistime id stanice
  i := 0;
  case cbStanica1.ItemIndex of
    1: i := 18; // stv1
    2: i := 19; // stv2
    3: i := 17; // markiza
    4: i := 15; // ct1
    5: i := 24; // ct2
    //4: i := 20; // joj
    //7: i := 25; // eurosport
  end;
  // stiahneme stranku
  application.ProcessMessages;
  prefixtmp := '/tmp/';
  {$ifdef WINDOWS}
  prefixtmp := '';
  {$endif}
  PipedProcessExecute('wget -O '+prefixtmp+'tmp.antiktv.'+GetEnvironmentVariable('USER')+' http://iptv.antik.sk/epg/archiv_epg_under_screen/date/'+IntToStr(DateTimeToUnix(Trunc(CalendarDialog1.Date)))+'/channel/'+inttostr(i),nil,nil);
  // parsujeme
  lbZoznam1.Items.Clear;
  ParserAntikIpTvExecute(prefixtmp+'tmp.antiktv.'+GetEnvironmentVariable('USER'),TimeList,TitleList,IdList);
  for i := 0 to TitleList.Count-1 do
    lbZoznam1.Items.Add(TimeList[i]+' - '+TitleList[i]);
end;

procedure TForm1.btnDatum1Click(Sender: TObject);
begin
  // zobrazime kalendar s vyberom datumu
  if CalendarDialog1.Execute then
  begin
    btnDatum1.Caption := FormatDateTime('d.m.yyyy',CalendarDialog1.Date);
    cbStanica1Change(cbStanica1);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // alokacia pom. premennych
  TimeList := TStringList.Create;
  TitleList := TStringList.Create;
  IdList := TStringList.Create;
  // vychodzi datum - dnes
  CalendarDialog1.Date := now;
  btnDatum1.Caption := FormatDateTime('d.m.yyyy',CalendarDialog1.Date);
end;

procedure TForm1.lbZoznam1DblClick(Sender: TObject);
begin
  // prehrajeme zvolene id
  if (lbZoznam1.ItemIndex >= 0)
  and(IdList.Count > lbZoznam1.ItemIndex) then
  begin
    with TProcess.Create(nil) do
    try
      {$ifdef WINDOWS}
      CommandLine := 'c:\Progra~1\VideoLAN\VLC\vlc.exe ftp://10.254.9.2/'+IdList[lbZoznam1.ItemIndex]+'.ts';
      {$else}
      CommandLine := 'vlc ftp://10.254.9.2/'+IdList[lbZoznam1.ItemIndex]+'.ts';
      {$endif}
      Execute;
    finally
      Free;
    end;
  end;
end;

procedure TForm1.mnuKopirovatOdkaz1Click(Sender: TObject);
var poms : string;
begin
  // skopirujeme adresu odkazu do klipboardu
  poms := 'ftp://10.254.9.2/'+IdList[lbZoznam1.ItemIndex]+'.ts';
  if InputQuery('Adresa odkazu','Odkaz',poms) then
    with TEdit.Create(application) do
    try
      Text := poms;
      SelectAll;
      CopyToClipboard;
    finally
      free;
    end;
end;

initialization
  {$I main_form_f.lrs}

end.

