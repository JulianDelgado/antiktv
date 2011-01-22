unit Unit1;

{
TODO:
- opravit odsadzovanie
- precistit kod
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, StdCtrls, CheckLst, ExtCtrls, Process, Sockets;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnRefresh1: TButton;
    btnAllow1: TButton;
    btnDeny1: TButton;
    btnClear1: TButton;
    btnCheckHost: TButton;
    CheckListBox1: TCheckListBox;
    edtFilter1: TEdit;
    IdleTimer1: TIdleTimer;
    procedure btnCheckHostClick(Sender: TObject);
    procedure btnDeny1Click(Sender: TObject);
    procedure btnRefresh1Click(Sender: TObject);
    procedure btnAllow1Click(Sender: TObject);
    procedure btnClearRecent1Click(Sender: TObject);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    recent : TStringList;
    hosts : TStringList;
    blacklist : TStringList;
    function InHosts(AHostName : string) : boolean;
  end; 

var
  Form1: TForm1; 

function GetHostIp(AHostName : string) : string;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // allocation
  recent := TStringList.Create;
  hosts := TStringList.Create;
  blacklist := TStringList.Create;
  // load known hosts and blacklist
  hosts.LoadFromFile('/etc/hosts');
  blacklist.LoadFromFile('/etc/hosts.blacklist');
  // initial refresh
  btnRefresh1.Click;
end;

procedure TForm1.IdleTimer1Timer(Sender: TObject);
begin
  // close after 30s idle
  Close;
end;

function TForm1.InHosts(AHostName: string): boolean;
var i : integer;
begin
  // return true if AHostName is already in /etc/hosts
  result := false;
  for i := 0 to hosts.Count-1 do
    if Pos(' '+AHostName+' ',hosts[i]+' ') > 0 then
    begin
      result := true;
      exit;
    end;
end;

procedure TForm1.btnRefresh1Click(Sender: TObject);
var i,k : integer;
begin
  // load /var/cache/pdnsd/recent and show in checklistbox in reverse order
  recent.LoadFromFile('/var/cache/pdnsd/recent');
  CheckListBox1.Items.Clear;
  k := 0;
  for i := recent.count-1 downto 0 do
  begin
    // filter
    if edtFilter1.Text<>'' then
      if pos(edtFilter1.Text,recent[i]) <= 0 then
        continue;
    // dont display hosts on blacklist
    if blacklist.IndexOf(recent[i]) >= 0 then
       continue;
    // dont display hosts already in /etc/hosts
    if InHosts(recent[i]) then
      continue;
    inc(k);
    // add to checklistbox
    CheckListBox1.Items.add(recent[i]);
  end;
  Caption := 'pdnsd client - '+inttostr(CheckListBox1.Items.Count)+' / '+inttostr(k)+' / '+inttostr(recent.count)+' items';
end;

procedure TForm1.btnDeny1Click(Sender: TObject);
var i : integer;
    sl : TStringList;
    s : string;
begin
  // crete list of to be blacklisted hosts (without IP adresses!)
  try
    sl := TStringList.Create;
    for i := 0 to CheckListBox1.Items.Count-1 do
      if CheckListBox1.Checked[i] then
      begin
        s := CheckListBox1.Items[i];
        if pos(' --> ',s) > 0 then
           s := copy(s,1,pos(' --> ',s)-1);
        sl.Add(s);
      end;
    // confirm deny
    //if MessageDlg('Confirm BLACKLIST following hosts:'#13#13+sl.text+#13#13'Are you sure you want to BLACKLIST them?',mtWarning,[mbYes,mbCancel],0) <> mrYes then
    //   exit;
    // add selected to blacklist so they wont be displayed ever again (e.g. facebook)
    blacklist.AddStrings(sl);
    // save new blacklist and refresh view
    blacklist.SaveToFile('/etc/hosts.blacklist');
    btnRefresh1.Click;
  finally
    sl.Free;
  end;
end;

procedure TForm1.btnCheckHostClick(Sender: TObject);
var i,a : integer;
    s,ip,host,ip2 : string;
    start : boolean;
begin
  // this check all items in /etc/hosts if they contain space (forbiden) or IP that is no longer valid
  hosts.LoadFromFile('/etc/hosts');
  hosts.SaveToFile('/tmp/hosts.old');
  CheckListBox1.Items.Clear;
  start := false;
  for i := 0 to hosts.count-1 do
  begin
    caption := inttostr(i)+'/'+inttostr(hosts.count);
    Application.ProcessMessages;
    s := trim(hosts[i]);
    // only start parsing host from "# --pdnsd start--" mark
    if s = '# --pdnsd start--' then
      start := true;
    if not start then
      continue;
    // split line to IP and host
    a := pos(' ',s);
    if (length(s)<=0) or (a <= 0) then
      continue;
    ip := trim(copy(s,1,a-1));
    if ip = '#' then
      continue;
    host := trim(copy(s,a+1,maxint));
    // more address per IP are not allowed for now, just keep only first one
    a := pos(' ',host);
    if a > 0 then
    begin
      host := trim(copy(host,1,a-1));
      // remove second host
      hosts[i] := ip+' '+host;
    end;
    // check real IP address
    ip2 := GetHostIp(host);
    // if it looks reasonable (good length, few dots) update it
    if (length(ip2) >= 7)and(pos('.',ip2)>0) then
      hosts[i] := ip2+' '+host;
    CheckListBox1.Items.Add('ok: '+hosts[i]);
    application.processmessages;
  end;
  hosts.SaveToFile('/etc/hosts');
end;

function GetHostIp(AHostName : string) : string;
var p : TProcess;
    sl : TStringList;
    i : integer;
begin
  // use nslookup to find IP address of a given host, it use google DNS server so it work regardless
  // of /etc/resolv.conf where 127.0.0.1 should be anyway
  result := '';
  // run nslookup foo.com 8.8.8.8
  p := TProcess.Create(nil);
  p.CommandLine := 'nslookup '+AHostName+' 8.8.8.8';
  p.Options := p.Options + [poWaitOnExit, poUsePipes];
  p.Execute;
  // parse its output, return first address found
  sl := TStringList.Create;
  sl.LoadFromStream(p.Output);
  for i := 0 to sl.Count-1 do
      if pos('Address: ',sl[i]) = 1 then
      begin
         result := copy(sl[i],10,maxint);
         break;
      end;
  // showmessage(sl.text);
  sl.Free;
  p.Free;
end;

procedure TForm1.btnAllow1Click(Sender: TObject);
var i,a : integer;
    s,host,ip : string;
    sl : TStringList;
begin
  // append selected lines to /etc/hosts
  try
    // create new list of to be added hosts and IPs
    sl := TStringList.Create;
    for i := 0 to CheckListBox1.Items.Count-1 do
      if CheckListBox1.Checked[i] then
      begin
        s := CheckListBox1.Items[i];
        a := pos(' --> ',s);
        host := copy(s,1,a-1);
        ip := copy(s,a+4,maxint);
        sl.add(ip+' '+host);
      end;
    // confirm adding
    //if MessageDlg('Confirm adding following hosts:'#13#13+sl.text+#13#13'Are you sure you want to add them to /etc/hosts?',mtConfirmation,[mbYes,mbCancel],0) <> mrYes then
    //   exit;
    // add to /etc/hosts and save it!
    hosts.LoadFromFile('/etc/hosts');
    hosts.AddStrings(sl);
    hosts.SaveToFile('/etc/hosts');
    // refresh
    btnRefresh1.Click;
  finally
    sl.Free;
  end;
end;

procedure TForm1.btnClearRecent1Click(Sender: TObject);
begin
  // clear recent
  recent.Clear;
  recent.SaveToFile('/var/cache/pdnsd/recent');
  CheckListBox1.Items.Clear;
  ShowMessage('After clearing recent cache, it is recomended to restart pdnsd daemon!');
end;

procedure TForm1.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var a : integer;
    ip : string;
begin
  // when click on item, find it's IP adress
  // remove previously counted IP
  a := pos(' --> ',CheckListBox1.Items[index]);
  if a > 0 then
     CheckListBox1.Items[index] := copy(CheckListBox1.Items[index],1,a-1);
  // if not checked, don't lookup IP
  if not CheckListBox1.Checked[index] then
     exit;
  // find IP and add it to end
  ip := GetHostIp(CheckListBox1.Items[index]);
  if ip <> '' then
     CheckListBox1.Items[index] := CheckListBox1.Items[index] + ' --> ' + ip;
end;

initialization
  {$I unit1.lrs}

end.

