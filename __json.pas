unit __json;

{$mode objfpc}{$H+}

{$notes off}

interface

uses
  fpjson;

type
  JData = TJSONData;
  JArray = TJSONArray;
  JObject = TJSONObject;
  JNull = TJSONNull;
  JInteger = TJSONInt64Number;
  JDouble = TJSONFloatNumber;
  JString = TJSONString;

function parse(s: string): TJSONData;
function typeof(x: TJSONData): string;
function null(): TJSONNull;
function is_null(x: TJSONData): boolean;

implementation

uses
  jsonparser,
  TypInfo;

function parse(s: string): TJSONData;
begin
  Result := GetJSON(s);
end;

function typeof(x: TJSONData): string;
begin
  Result := GetEnumName(TypeInfo(TJSONtype), Ord(x.JSONType));
end;

function null(): TJSONNull;
begin
  Result := TJSONNull.Create;
end;

function is_null(x: TJSONData): boolean;
begin
  Result := x.JSONType = jtNull;
end;

end.
