unit __nw {namespace my_sample};

{$mode objfpc}{$H+}

{$ifdef MSWINDOWS}
  {$ifdef CPU64}
    {$R openssl64.rc}
  {$else  CPU64}
    {$R openssl32.rc}
  {$endif CPU64}
{$endif MSWINDOWS}

interface

uses
  Classes,
  SysUtils,
  LazUtf8,
  __common,
  fphttpclient, opensslsockets,
  __fs
  {$ifdef MSWINDOWS}
  , Windows
  , __win32
  //, __nw_res
  , __resource
  {$endif}  ;

type
  TNetworkSystem = class
  private
    m_client: TFPHTTPClient;
    m_content_length: int64;
    //m_current_length: int64;
    m_progress_meter: string;

  public
    constructor Create();
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    function DownloadFile(FileName: string; URL: string): boolean;
    function DownloadString(URL: string): string;
  protected
    procedure CB_OnHeaders(Sender: TObject);
    procedure CB_OnDataReceived(Sender: TObject;
      const ContentLength, CurrentPos: int64);
  end;

implementation

constructor TNetworkSystem.Create();
var
  tempDir: WideString;
  dllPath: WideString;
  sha1: string;
  sha2: string;
  dllDir: string;
begin
  SafeWriteLn('TNetworkSystem.Create()');
  m_client := TFPHTTPClient.Create(nil);
  m_client.AllowRedirect := True;
  echo(m_client = nil, '(A)');
end;

destructor TNetworkSystem.Destroy();
begin
  SafeWriteLn('TNetworkSystem.Destroy()');
  m_client.Free;
  inherited;
end;

function TNetworkSystem.DownloadString(URL: string): string;
begin
  echo(m_client = nil, '(B)');
  m_client.AllowRedirect := True;
  Result := m_client.Get(URL);
end;

procedure TNetworkSystem.CB_OnHeaders(Sender: TObject);
var
  index: integer;
begin
  m_content_length := -1;
  //m_current_length := 0;
  m_progress_meter := '';
  for index := 0 to Pred(m_client.ResponseHeaders.Count) do
  begin
    if LowerCase(m_client.ResponseHeaders.Names[index]) = 'content-length' then
    begin
      m_content_length :=
        StrToInt64(m_client.ResponseHeaders.ValueFromIndex[index]);
    end;
  end;
end;

procedure TNetworkSystem.CB_OnDataReceived(Sender: TObject;
  const ContentLength, CurrentPos: int64);
var
  currentPercent: double = 0;
  progressMeter: string;
begin
  if m_content_length <> 0 then
    currentPercent := (CurrentPos * 100.0) / m_content_length;
  //if (currentPos - m_current_length)<(1024*1024) then exit;
  //m_current_length := CurrentPos;
  progressMeter := Format('  Progress: %f%% %s/%s', [currentPercent, FormatByteSize(CurrentPos), FormatByteSize(m_content_length)]);
  if progressMeter = m_progress_meter then exit;
  m_progress_meter := progressMeter;
  SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
  SafeWrite(#13); // brings your cursor to the beginning of the line
  SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
  //printf('  Progress: %f%% %s/%s'#10, [currentPercent, FormatByteSize(CurrentPos),
  //  FormatByteSize(m_content_length)]);
  __common.SafeWriteLn(m_progress_meter);
end;

function TNetworkSystem.DownloadFile(FileName: string; URL: string): boolean;
var
  guid: TGuid;
  temp: string;
begin
  SysUtils.ForceDirectories(ExtractFilePath(FileName));
  printf('DownloadFile():'#10, []);
  printf('  FileName: %s:'#10, [FileName]);
  printf('  URL: %s:'#10, [URL]);
  SafeWriteLn();
  m_content_length := 0;
  CreateGuid(guid);
  temp := FileName + GuidToString(guid) + '.tmp';
  with m_client do
  begin
    OnHeaders := @CB_OnHeaders;
    OnDataReceived := @CB_OnDataReceived;
    AllowRedirect := True;
    Get(url, temp);
    Result := __fs.RenameFile(temp, FileName);
  end;
end;

var
  tempDir: WideString;
  dllPath: WideString;
  sha1: string;
  sha2: string;
  dllDir: string;

initialization
  begin
  {$ifdef MSWINDOWS}
    dllDir := __resource.TResourceSystem.InstallZip('LAZARUS_COMMON_OPENSSL_ZIP');
    __win32.Win32.LoadLibrary(dllDir + '\libssl-1_1-x64.dll');
  {$endif}
  end;

end.
