unit calc_lib;

{$mode objfpc}{$H+}
{$notes off}

//{$R calc.rc}

interface

function add(a,b:integer): integer;
function sub(a,b:integer): integer;

implementation

uses __common, __resource
  {$ifdef MSWINDOWS}
  , calc_res
  {$endif}
  ;

type
  TAdd2 = function(a: integer; b: integer): integer;

var
  handle: THandle;

function add(a,b:integer): integer;
{$ifdef MSWINDOWS}
var
  Add2: TAdd2;
  {$endif}
begin
   //result:=a+b
   {$ifdef MSWINDOWS}
   Add2 := TAdd2(GetProcAddress(Handle, 'add2'));
   Result := Add2(a, b);
   {$else}
   Result := a + b;
   {$endif}
end;

function sub(a,b:integer): integer;
begin
   result:=a-b
end;

initialization
begin
  {$ifdef MSWINDOWS}
  handle := __resource.TResourceSystem.LoadDllByName('UNIT_CALC_LIB_CALC_DLL');
  {$endif}
end;

finalization
begin
  {$ifdef MSWINDOWS}
  FreeLibrary(handle);
  {$endif}
end;

end.
