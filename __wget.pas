unit __wget {namespace my_sample};

{$mode objfpc}{$H+}

{$ifdef MSWINDOWS}
{$R wget.rc}
{$endif}

interface

uses
  Classes,
  SysUtils,
  LazUtf8,
  Process, Pipes,
  UTF8Process,
  __common
  //,
  //__fs
  //{$ifdef MSWINDOWS}
  //, __win32
  //{$endif}
  ;

type
  WgetSystem = class
  private
    m_client: string;
    m_content_length: int64;

  public
    constructor Create();
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    function DownloadFile(FileName: string; URL: string): boolean;
    function DownloadFile2(FileName: string; URL: string): boolean;
    function DownloadString(URL: string): rawbytestring;
    //protected
  end;

implementation

{$ifdef MSWINDOWS}
function PrepareWgetExeName(Name: string): string;
var
  guid: string;
  S: Classes.TResourceStream;
  F: Classes.TFileStream;
begin
  guid := __common.resource_sha256(Name);
  Result := temp_dir() + '\tmp.' + SysUtils.ExtractFileName(applicaton_fullpath()) +
    '.' + Name + '.' + guid + '.wget.exe';
  SafeWriteLn(Result);
  if SysUtils.FileExists(Result) then
  begin
    printf('reusing exe file: %s'#10, [Result]);
  end
  else
  begin
    if not __common.resource_to_file(Name, Result) then
      Exit;
  end;
end;

{$endif}

constructor WgetSystem.Create();
begin
  SafeWriteLn('WgetSystem.Create()');
  {$ifndef MSWINDOWS}
  m_client := '/usr/bin/wget';
  {$else}
  m_client := PrepareWgetExeName(Format('LAZARUS_COMMON_WGET%d_EXE', [os_bit()]));
  {$endif}
end;

destructor WgetSystem.Destroy();
begin
  SafeWriteLn('WgetSystem.Destroy()');
  inherited;
end;

function WgetSystem.DownloadString(URL: string): rawbytestring;
var
  Process: TProcess;
  MemStream: TMemoryStream;
  BytesRead: int64;
  NumBytes: int64;
  Output: TInputPipeStream;
  OutputString: rawbytestring;
const
  READ_BYTES = 1024;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := m_client;
    Process.Parameters.Clear;
    Process.Parameters.Add('--quiet');
    Process.Parameters.Add('--no-check-certificate');
    Process.Parameters.Add('-O');
    Process.Parameters.Add('-');
    Process.Parameters.Add(URL);
    Process.Options := [poUsePipes];
    Process.Execute;
    Output := Process.Output;
    BytesRead := 0;
    MemStream := TMemoryStream.Create;
    SafeWriteLn();
    while True do
    begin
      MemStream.SetSize(BytesRead + READ_BYTES); // make sure we have room
      NumBytes := Process.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
      if NumBytes > 0 then
      begin
        Inc(BytesRead, NumBytes);
        //echo(BytesRead, 'BytesRead'); //Output progress to screen.
        SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
        SafeWrite(#13); // brings your cursor to the beginning of the line
        SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
        printf('  Progress: %s'#10, [FormatByteSize(BytesRead)]);
      end
      else
        BREAK; // Program has finished execution.
    end;
    MemStream.SetSize(BytesRead);
    try
      SetLength(OutputString, MemStream.Size);
      MemStream.Position := 0;
      MemStream.ReadBuffer(OutputString[1], MemStream.Size);
      Result := OutputString;
    finally
      MemStream.Free;
    end;
  finally
    Process.Free;
  end;
end;

function WgetSystem.DownloadFile(FileName: string; URL: string): boolean;
var
  Process: TProcess;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  BytesRead: int64;
  NumBytes: int64;
  Output: TInputPipeStream;
  OutputFile: TFileStream;
const
  READ_BYTES = 10 * 1024 * 1024;
begin
  Result := False;
  SysUtils.ForceDirectories(ExtractFilePath(FileName));
  Process := TProcess.Create(nil);
  try
    Process.Executable := m_client;
    Process.Parameters.Clear;
    Process.Parameters.Add('--quiet');
    Process.Parameters.Add('--no-check-certificate');
    Process.Parameters.Add('-O');
    Process.Parameters.Add('-');
    Process.Parameters.Add(URL);
    Process.Options := [poUsePipes];
    Process.Execute;
    Output := Process.Output;
    FileStream := TFileStream.Create(FileName, fmCreate);
    BytesRead := 0;
    MemStream := TMemoryStream.Create;
    MemStream.SetSize(READ_BYTES); // make sure we have room
    SafeWriteLn();
    while True do
    begin
      NumBytes := Process.Output.Read(MemStream.Memory^, READ_BYTES);
      FileStream.Write(MemStream.Memory^, NumBytes);
      if NumBytes > 0 then
      begin
        Inc(BytesRead, NumBytes);
        //echo(BytesRead, 'BytesRead'); //Output progress to screen.
        SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
        SafeWrite(#13); // brings your cursor to the beginning of the line
        SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
        printf('  Progress: %s'#10, [FormatByteSize(BytesRead)]);
      end
      else
        BREAK; // Program has finished execution.
    end;
  finally
    MemStream.Free;
    FileStream.Free;
    Process.Free;
  end;
  Result := True;
end;

function WgetSystem.DownloadFile2(FileName: string; URL: string): boolean;
var
  Process: TProcessUtf8;
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  BytesRead: int64;
  NumBytes: int64;
  Output: TInputPipeStream;
  OutputFile: TFileStream;
const
  READ_BYTES = 10 * 1024 * 1024;
begin
  Result := False;
  SysUtils.ForceDirectories(ExtractFilePath(FileName));
  Process := TProcessUtf8.Create(nil);
  //try
  Process.Executable := m_client;
  Process.Parameters.Clear;
  Process.Parameters.Add('--no-check-certificate');
  Process.Parameters.Add('-O');
  Process.Parameters.Add(FileName);
  Process.Parameters.Add(URL);
  Process.Options := [poWaitOnExit];
  Process.Execute;
  //Output := Process.Output;
  //FileStream := TFileStream.Create(FileName, fmCreate);
  //BytesRead := 0;
  //MemStream := TMemoryStream.Create;
  //MemStream.SetSize(READ_BYTES); // make sure we have room
  //SafeWriteLn();
  //while True do
  //begin
  //  NumBytes := Process.Output.Read(MemStream.Memory^, READ_BYTES);
  //  FileStream.Write(MemStream.Memory^, NumBytes);
  //  if NumBytes > 0 then
  //  begin
  //    Inc(BytesRead, NumBytes);
  //    //echo(BytesRead, 'BytesRead'); //Output progress to screen.
  //    SafeWrite(#27'[1A'); // moves your cursor up one line, but in the same column
  //    SafeWrite(#13); // brings your cursor to the beginning of the line
  //    SafeWrite(#27'[2K'); // erases the entire line your cursor is currently on
  //    printf('  Progress: %s'#10, [FormatByteSize(BytesRead)]);
  //  end
  //  else
  //    BREAK; // Program has finished execution.
  //end;
  //finally
  //MemStream.Free;
  //FileStream.Free;
  //Process.Free;
  //end;
  Result := True;
end;

initialization
  begin
  end;

end.
