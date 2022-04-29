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
  private
    FfileIni: TIniFile;
    FfileIniPath: string;
    { Private declarations }
    procedure WriteDevice(Name, IP_Adress: string);
    function GetDevice(Section, Name: string): String;
    procedure AddDeviceToLv(caption, subCaption: string);
    procedure setInfoToSB(countDevices: integer);
    procedure pingToHost(host: string);


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
    MessageBox(Handle, PChar('Проверьте заполнение полей'), PChar('Добавить устройство'), MB_OK+MB_ICONINFORMATION);
end;

procedure TfMain.actAutoPingDevicesExecute(Sender: TObject);
begin
  if lvDevices.Items.Count > 0 then
    try

    finally

    end;
end;

procedure TfMain.actDeleteDeviceExecute(Sender: TObject);
begin
  if  (lvDevices.Selected <> nil) and
      (MessageDlg('Вы действительно желаете удалить устройство ' + lvDevices.Selected.Caption + ' из списка?',
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
  pingToHost(lvDevices.Selected.SubItems[0]);
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
    setInfoToSB(lvDevices.Items.Count);
    Screen.Cursor := crDefault;
  end;
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

procedure TfMain.pingToHost(host: string);
var
  timer: TStopWatch;
  i: integer;
begin
  if lvDevices.Selected <> nil then
  try
    memoLog.Lines.Add('Проверка связи с хостом - ' + host);
    IdClient.Host := host;

    for i := 0 to 3 do
      Begin
        timer := TStopwatch.StartNew;
        IdClient.Ping();
        timer.Stop;

        memoLog.Lines.Add(Format('Пакет - %d, Время выполнения - %d мс', [i+1, timer.Elapsed.Milliseconds]));
        memoLog.Lines.Add('IdCLient timeout - ' + IdClient.ReplyStatus.MsRoundTripTime.ToString);


      End;
  except
    on ex: Exception do
      Begin
        memoLog.Lines.Add('Произошла ошибка проверки связи!');
        memoLog.Lines.Add('Сообщение - ' + ex.Message);
        memoLog.Lines.Add('Проверьте корректность адреса');
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

procedure TfMain.setInfoToSB(countDevices: integer);
begin
  sbBottom.Panels[0].Text :=  Format('Устройств - %d', [countDevices]);
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
