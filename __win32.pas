{$ifdef MSWINDOWS}
unit __win32 {namespace my_sample};

{$mode objfpc}{$H+}
{$notes off}

interface

uses
  Windows, Classes, SysUtils, LazUTF8, __common;

type
  Win32 = class
    //private
    //  length, Width: integer;

  public
    constructor Create();
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    class function GetModuleFileName(hModule: HINST): string;
    class function GetTempPath: string;
    class function LoadLibrary(FileName: string): THandle;
    class function LoadLibrary(FileName: WideString): THandle;
    class procedure MessageBox(hWnd: HWND; Text: string; Caption: string;
      uType: UINT = MB_OK);
    class procedure MessageBox(hWnd: HWND; Text: WideString;
      Caption: WideString; uType: UINT = MB_OK);
    class function MessageBoxYesNo(hWnd: HWND; Text: string; Caption: string): boolean;
    class function MessageBoxYesNo(hWnd: HWND; Text: WideString;
      Caption: WideString): boolean;
    class function CreateResourceStream(Name: string;
      ResType: PChar = RT_RCDATA): TResourceStream;
    class procedure RemoveFileOnReboot(FileName: string);
    class procedure RemoveFileOnReboot(FileName: WideString);
  end;

implementation


constructor Win32.Create();
begin
end;

destructor Win32.Destroy();
begin
  inherited;
end;

class function Win32.GetModuleFileName(hModule: HINST): string;
var
  pc: pwidechar;
begin
  pc := pwidechar(StrAlloc((MAX_PATH + 1) * sizeof(wchar)));
  GetModuleFileNameW(
    hModule,
    pc,
    MAX_PATH
    );
  Result := UTF16ToUTF8(WideString(pc));
  StrDispose(pc);
end;

class function Win32.GetTempPath: string;
var
  pc: pwidechar;
begin
  pc := pwidechar(StrAlloc((MAX_PATH + 1) * 2));
  GetTempPathW(MAX_PATH, pc);
  Result := UTF16ToUTF8(WideString(pc));
  StrDispose(pc);
end;

class function Win32.LoadLibrary(FileName: string): THandle;
var
  FileNameW: WideString;
begin
  FileNameW := Utf8ToUtf16(FileName);
  Result := LoadLibraryExW(@FileNameW[1], 0, LOAD_LIBRARY_SEARCH_DEFAULT_DIRS or LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR);
end;

class function Win32.LoadLibrary(FileName: WideString): THandle;
begin
  Result := LoadLibraryExW(@FileName[1], 0, LOAD_LIBRARY_SEARCH_DEFAULT_DIRS or LOAD_LIBRARY_SEARCH_DLL_LOAD_DIR);
end;

class procedure Win32.MessageBox(hWnd: HWND; Text: string; Caption: string;
  uType: UINT);
var
  TextW: WideString;
  CaptionW: WideString;
begin
  TextW := UTF8ToUTF16(Text);
  CaptionW := UTF8ToUTF16(Caption);
  MessageBoxW(0, @TextW[1], @CaptionW[1], MB_OK);
end;

class procedure Win32.MessageBox(hWnd: HWND; Text: WideString;
  Caption: WideString; uType: UINT);
begin
  MessageBoxW(0, @Text[1], @Caption[1], MB_OK);
end;

class function Win32.MessageBoxYesNo(hWnd: HWND; Text: string; Caption: string): boolean;
var
  TextW: WideString;
  CaptionW: WideString;
begin
  TextW := UTF8ToUTF16(Text);
  CaptionW := UTF8ToUTF16(Caption);
  Result := (MessageBoxW(0, @TextW[1], @CaptionW[1], MB_YESNO) = idYes);
end;

class function Win32.MessageBoxYesNo(hWnd: HWND; Text: WideString;
  Caption: WideString): boolean;
begin
  Result := (MessageBoxW(0, @Text[1], @Caption[1], MB_YESNO) = idYes);
end;

class function Win32.CreateResourceStream(Name: string; ResType: PChar): TResourceStream;
begin
  Result := TResourceStream.Create(System.HInstance, Name, ResType);
end;

class procedure Win32.RemoveFileOnReboot(FileName: string);
var
  FileNameW: WideString;
begin
  FileNameW := Utf8ToUtf16(FileName);
  MoveFileExW(@FileNameW[1], nil, MOVEFILE_DELAY_UNTIL_REBOOT);
end;

class procedure Win32.RemoveFileOnReboot(FileName: WideString);
begin
  MoveFileExW(@FileName[1], nil, MOVEFILE_DELAY_UNTIL_REBOOT);
end;

end.
{$else}
unit __win32 {namespace my_sample};
interface
implementation
end.
{$endif}

