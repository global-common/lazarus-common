unit __git {namespace my_sample};

{$mode objfpc}{$H+}

{$ifdef MSWINDOWS}
{$R git.rc}
{$endif}

interface

uses
  Classes,
  SysUtils,
  LazUtf8,
  Process, Pipes,
  httpprotocol,
  __common
  , __resource
  ;

type
  GitSystem = class
  private
    m_client: string;
    m_content_length: int64;

  public
    constructor Create();
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    function Clone(URL: string; Dest: string): boolean;
    function CloneAuto(URL: string): string;
    //protected
  end;

implementation

{$ifdef MSWINDOWS}
function PrepareGitZip(): string;
begin
  Result := TResourceSystem.InstallZip(Format('LAZARUS_COMMON_GIT%u_ZIP', [os_bit]));
  echo(Result, '__git.PrepareGitZip()');
end;
{$endif}

function EncodeRepoUrl(URL: string): string;
begin
  Result := StringReplace(URL, '://', '--', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '--', [rfReplaceAll]);
  Result := httpprotocol.HTTPEncode(Result);
end;

constructor GitSystem.Create();
begin
  SafeWriteLn('GitSystem.Create()');
  {$ifndef MSWINDOWS}
  m_client := '/usr/bin/git';
  {$else}
  m_client := PrepareGitZip() + '/cmd/git.exe';
  {$endif}
end;

destructor GitSystem.Destroy();
begin
  SafeWriteLn('GitSystem.Destroy()');
  inherited;
end;

function GitSystem.Clone(URL: string; Dest: string): boolean;
var
  Process: TProcess;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  BytesRead: int64;
  NumBytes: int64;
  Output: TInputPipeStream;
  OutputFile: TFileStream;
//const
//  READ_BYTES = 10 * 1024 * 1024;
begin
  if SysUtils.DirectoryExists(Dest) then
  begin
    Process := TProcess.Create(nil);
    try
      Process.Options := [poWaitOnExit];
      Process.Executable := m_client;
      Process.CurrentDirectory := Dest;
      Process.Parameters.Clear;
      Process.Parameters.Add('pull');
      Process.Execute;
    finally
      Process.Free;
    end;
    exit;
  end;
  Process := TProcess.Create(nil);
  try
    Process.Options := [poWaitOnExit];
    Process.Executable := m_client;
    Process.Parameters.Clear;
    Process.Parameters.Add('clone');
    Process.Parameters.Add(URL);
    Process.Parameters.Add(Dest);
    Process.Execute;
    Result := (Process.ExitCode = 0);
  finally
    Process.Free;
  end;
end;

function GitSystem.CloneAuto(URL: string): string;
var
  encoded: string;
  repo: string;
begin
  encoded := EncodeRepoUrl(URL);
  echo(encoded);
  repo := ExcludeTrailingPathDelimiter(config_dir()) + '\.repo\' + encoded;
  echo(repo);
  Self.Clone(URL, repo);
  Result := repo;
end;

initialization
  begin
  end;

end.
