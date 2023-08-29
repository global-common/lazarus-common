unit spider_lib;

{$mode objfpc}{$H+}

{$notes off}

interface

uses
  Classes,
  SysUtils,
  json_lib;

//type

function GetFolderList(const ADirectory: string): TJArray;

implementation

//uses

function GetFolderList(const ADirectory: string): TJArray;
var
  SearchRec: TSearchRec;
begin
  Result := TJArray.Create();
  if FindFirst(ADirectory + '\*', faDirectory, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and
        (SearchRec.Attr and faDirectory <> 0) then
        Result.Add(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

end.
