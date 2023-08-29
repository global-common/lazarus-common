unit __fs;

{$mode objfpc}{$H+}

{$notes off}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Variants;

function DeleteFile(path: string): boolean;
function DeleteDirectory(path: string): boolean;
function RenameFile(OldName: string; NewName: string): boolean;
function RenameDirectory(OldName: string; NewName: string): boolean;

implementation

uses
  LazUTF8;

function DeleteFile(path: string): boolean;
begin
  Result := False;
  if not SysUtils.FileExists(path) then
  begin
    Result := True;
    Exit;
  end
  else
  begin
    if not SysUtils.DeleteFile(path) then Exit;
  end;
  Result := True;
end;

function DeleteDirectory(path: string): boolean;
begin
  Result := False;
  if not SysUtils.DirectoryExists(path) then
  begin
    Result := True;
    Exit;
  end
  else
  begin
    if not FileUtil.DeleteDirectory(path, False) then Exit;
  end;
  Result := True;
end;

function RenameFile(OldName: string; NewName: string): boolean;
begin
  Result := False;
  //__common.msgbox('RenameFile(A)' + NewName);
  if SysUtils.FileExists(NewName) then
  begin
    //__common.msgbox('RenameFile(B)' + NewName);
    if not __fs.DeleteFile(NewName) then Exit;
  end;
  //__common.msgbox('RenameFile(C)' + NewName);
  if SysUtils.DirectoryExists(NewName) then
  begin
    //__common.msgbox('RenameFile(D)' + NewName);
    if not __fs.DeleteDirectory(NewName) then Exit;
  end;
  //__common.msgbox('RenameFile(E)' + NewName);
  Result := SysUtils.RenameFile(OldName, NewName);
  //if not Result then
  //  __common.msgbox('RenameFile(F)' + NewName);
end;

function RenameDirectory(OldName: string; NewName: string): boolean;
begin
  Result := False;
  if SysUtils.FileExists(NewName) then
  begin
    //__common.msgbox('RenameDirectory(0.1)' + NewName);
    if not SysUtils.DeleteFile(NewName) then
    begin
      //__common.msgbox('RenameDirectory(1)' + NewName);
      Exit;
    end;
  end;
  if SysUtils.DirectoryExists(NewName) then
  begin
    //__common.msgbox('RenameDirectory(0.2)' + NewName);
    if not __fs.DeleteDirectory(NewName) then
    begin
      //__common.msgbox('RenameDirectory(2)' + NewName);
      Exit;
    end;
  end;
  //Result := SysUtils.RenameFile(OldName + '/', NewName + '/');
  Result := SysUtils.RenameFile(OldName, NewName);
end;

end.
