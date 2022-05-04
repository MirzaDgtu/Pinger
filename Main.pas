unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.StrUtils, System.IniFiles,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, System.ImageList, Vcl.ImgList,
  Vcl.ToolWin, System.Actions, Vcl.ActnList, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient,
  IdSMTPBase, IdSMTP, IdRawBase, IdRawClient, IdIcmpClient, Vcl.ExtCtrls, System.Diagnostics;

const
  SFileIniName      = 'devices.ini';
  SDeviceSection    = 'DEVICES';

type
  TfMain = class(TForm)
    grDeviceParametr: TGroupBox;
    edNameDevice: TEdit;
    edIpAdress: TEdit;
    btnAddDevice: TBitBtn;
    btnGetDeviceAdress: TBitBtn;
    lvDevices: TListView;
    Label1: TLabel;
    Label2: TLabel;
    TB: TToolBar;
    btnRefreshLv: TToolButton;
    IL: TImageList;
    sbBottom: TStatusBar;
    AL: TActionList;
    actAddDevice: TAction;
    actGetDeviceAdress: TAction;
    actRefreshLv: TAction;
    btnDeleteDevice: TToolButton;
    actDeleteDevice: TAction;
    btnPingToServer: TToolButton;
    actPintToDevice: TAction;
    IdClient: TIdIcmpClient;
    Splitter1: TSplitter;
    gbxLog: TGroupBox;
    memoLog: TMemo;
    btnAutoPingDevices: TToolButton;
    actAutoPingDevices: TAction;
    actSaveLog: TAction;
    btnSaveLog: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure actAddDeviceExecute(Sender: TObject);
    procedure actGetDeviceAdressExecute(Sender: TObject);
    procedure edNameDeviceChange(Sender: TObject);
    procedure actRefreshLvExecute(Sender: TObject);
    procedure actDeleteDeviceExecute(Sender: TObject);
    procedure sbBottomDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure actPintToDeviceExecute(Sender: TObject);
    procedure lvDevicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actAutoPingDevicesExecute(Sender: TObject);
    procedure actSaveLogExecute(Sender: TObject);
  private
    FfileIni: TIniFile;
    FfileIniPath: string;
    { Private declarations }
    procedure WriteDevice(Name, IP_Adress: string);
    function GetDevice(Section, Name: string): String;
    procedure AddDeviceToLv(caption, subCaption: string);
    procedure setInfoToSB(countDevices, countDevicesOffline: integer);
    procedure pingToHost(item: TListItem);


    procedure SetfileIni(const Value: TIniFile);
    procedure SetfileIniPath(const Value: string);

    function isEmptyText(value: string): Boolean;
    function getNameDevice(value: string): String;
    function getAdressDevice(value: string): String;

  protected
    property fileIni: TIniFile read FfileIni write SetfileIni;
    property fileIniPath: string read FfileIniPath write SetfileIniPath;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

{ TfMain }

procedure TfMain.actAddDeviceExecute(Sender: TObject);
begin
  if (not isEmptyText(edNameDevice.Text)) and (not isEmptyText(edIpAdress.Text)) then
    Begin
      WriteDevice(edNameDevice.Text, edIpAdress.Text);
      actRefreshLvExecute(nil);
    End
  else
    MessageBox(Handle, PChar('��������� ���������� �����'), PChar('�������� ����������'), MB_OK+MB_ICONINFORMATION);
end;

procedure TfMain.actAutoPingDevicesExecute(Sender: TObject);
var
  i, cntDevicesOffline: integer;
begin
  cntDevicesOffline := 0;
  if lvDevices.Items.Count > 0 then
    try
      for I := 0 to lvDevices.Items.Count - 1  do
        try
           pingToHost(lvDevices.Items.Item[i]);
        except
          memoLog.Lines.Add('������ �������� ���� �� ���������� - ' + lvDevices.Items.Item[i].Caption);
        end;
    finally
       actSaveLogExecute(nil);

       for I := 0 to lvDevices.Items.Count - 1 do 
        if lvDevices.Items.Item[i].ImageIndex = 22 then
            inc(cntDevicesOffline);

       setInfoToSB(lvDevices.Items.Count, cntDevicesOffline); 
    end;
end;

procedure TfMain.actDeleteDeviceExecute(Sender: TObject);
begin
  if  (lvDevices.Selected <> nil) and
      (MessageDlg('�� ������������� ������� ������� ���������� ' + lvDevices.Selected.Caption + ' �� ������?',
                mtWarning, mbYesNo, 0) = ID_YES) then
    try
      fileIni.DeleteKey(SDeviceSection, lvDevices.Selected.Caption);
    finally
      actRefreshLvExecute(nil);
    End;

end;

procedure TfMain.actGetDeviceAdressExecute(Sender: TObject);
begin
   if (not isEmptyText(edNameDevice.Text)) then
     edIpAdress.Text := GetDevice(SDeviceSection, edNameDevice.Text);
end;


procedure TfMain.actPintToDeviceExecute(Sender: TObject);
begin
  if lvDevices.Selected <> nil then
    pingToHost(lvDevices.Selected);
end;

procedure TfMain.actRefreshLvExecute(Sender: TObject);
var
  ls: TStringList;
  i: integer;
  name, adress: string;
begin

  Screen.Cursor := crHourGlass;

  try
    ls := TStringList.Create;
    fileIni.ReadSectionValues(SDeviceSection, ls);

    lvDevices.Items.BeginUpdate;
    lvDevices.Items.Clear;

    for I := 0 to ls.Count - 1 do
      Begin
        name := getNameDevice(ls[i]);
        adress := getAdressDevice(ls[i]);

        AddDeviceToLv(name, adress);
      End;
  finally
    lvDevices.SortType := stBoth;
    lvDevices.SortType := stNone;
    lvDevices.Items.EndUpdate;
    setInfoToSB(lvDevices.Items.Count, 0);
    Screen.Cursor := crDefault;
  end;
end;

procedure TfMain.actSaveLogExecute(Sender: TObject);
begin
  if not memoLog.Lines.Text.IsEmpty then
    Begin
      memoLog.Lines.SaveToFile(ExtractFilePath(GetModuleName(0)) + 'Log\log_' + FormatDateTime('ddMMyyyy-hhmm', Now()) + '.log');
      memoLog.Lines.Add('��� ������� ��������');
    End;
end;

procedure TfMain.AddDeviceToLv(caption, subCaption: string);
var
  lvItem: TListItem;
begin

  try
    lvDevices.Items.BeginUpdate;
    lvItem := lvDevices.Items.Add;

    lvItem.Caption := caption;
    lvItem.SubItems.Add(subCaption);
    lvItem.ImageIndex := 18;
  finally
    lvDevices.Items.EndUpdate;
  end;

end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  SetfileIniPath(ExtractFilePath(GetModuleName(0)) + SFileIniName);
  fileIni := TIniFile.Create(fileIniPath);
  actRefreshLvExecute(nil);
end;


function TfMain.getAdressDevice(value: string): String;
begin
  if not isEmptyText(value) then
    Result := Copy(value, Pos('=', value) + 1, Length(value) - Pos('=', value) + 1);
end;

function TfMain.GetDevice(Section, Name: string): String;
begin
  if fileIni.ValueExists(Section, name) then
    Result := fileIni.ReadString(Section, Name, EmptyStr);
end;

function TfMain.getNameDevice(value: string): String;
begin
  if not isEmptyText(value) then
    Result :=  Copy(value, 0, Pos('=', value)-1);
end;

function TfMain.isEmptyText(value: string): Boolean;
begin
  Result := Length(Trim(value)) = 0;
end;

procedure TfMain.lvDevicesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
   edNameDevice.Text := Item.Caption;
   edIpAdress.Text := Item.SubItems[0];
end;

procedure TfMain.pingToHost(item: TListItem);
var
  timer: TStopWatch;
  i, cntSuccessPing: integer;
begin
  cntSuccessPing := 0;

  try
    memoLog.Lines.Add('�������� ����� � ������ - ' + item.SubItems[0]);
    IdClient.Host := item.SubItems[0];

    for i := 0 to 3 do
      Begin
        timer := TStopwatch.StartNew;
        IdClient.Ping();
        timer.Stop;

       // memoLog.Lines.Add(Format('����� - %d, ����� ���������� - %d ��', [i+1, timer.Elapsed.Milliseconds]));
         memoLog.Lines.Add(Format('����� - %d, ����� ���������� - %d ��', [i+1, IdClient.ReplyStatus.MsRoundTripTime]));

        if //(IdClient.ReplyStatus.MsRoundTripTime > 0) AND
           (IdClient.ReplyStatus.MsRoundTripTime < 999) then
          Inc(cntSuccessPing);

      End;

      if cntSuccessPing = 4 then
        item.ImageIndex := 21
      else if (cntSuccessPing >= 2) and (cntSuccessPing < 4 )  then
              item.ImageIndex := 25
      else if cntSuccessPing <= 0 then           
        item.ImageIndex := 22;        
        
       memoLog.Lines.Add(EmptyStr);

  except
    on ex: Exception do
      Begin
        memoLog.Lines.Add('��������� ������ �������� �����!');
        memoLog.Lines.Add('��������� - ' + ex.Message);
        memoLog.Lines.Add('��������� ������������ ������');
        memoLog.Lines.Add(EmptyStr);
      End;
  end;
end;

procedure TfMain.sbBottomDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  with sbBottom.Canvas do
    Begin
      Font.Style := Font.Style + [fsBold];
      Font.Name := 'Times New Roman';

      if Panel = sbBottom.Panels[0] then
        Begin
          Font.Color := clGreen;
          IL.Draw(sbBottom.Canvas, Rect.Left, Rect.Top, 18);
        End;

      if Panel = sbBottom.Panels[1] then
        Begin
          Font.Color := clRed;
          IL.Draw(sbBottom.Canvas, Rect.Left, Rect.Top, 22);
        End;

      TextOut(Rect.Left + 17, Rect.Top, Panel.Text);
    End;

end;

procedure TfMain.SetfileIni(const Value: TIniFile);
begin
  FfileIni := Value;
end;

procedure TfMain.SetfileIniPath(const Value: string);
begin
  FfileIniPath := Value;
end;

procedure TfMain.setInfoToSB(countDevices, countDevicesOffline: integer);
begin
  sbBottom.Panels[0].Text :=  Format('��������� - %d', [countDevices]);
  sbBottom.Panels[1].Text :=  Format('�� � ���� - %d', [countDevicesOffline]);
end;

procedure TfMain.edNameDeviceChange(Sender: TObject);
var
  curs: integer;
begin
  curs := edNameDevice.SelStart;
  edNameDevice.Text := AnsiUpperCase(edNameDevice.Text);
  edNameDevice.SelStart := curs;
end;

procedure TfMain.WriteDevice(Name, IP_Adress: string);
begin
  fileIni.WriteString(SDeviceSection, Name, IP_Adress);
end;



end.
