unit rectangle_lib {namespace my_sample};

{$mode objfpc}{$H+}
{$notes off}

interface

type
  Rectangle = class
  private
    length, Width: integer;

  public
    constructor Create(l, w: integer);
    // Free で destructor が呼ばれるために override が必要
    destructor Destroy(); override;
    procedure setlength(l: integer);
    function getlength(): integer;
    procedure setwidth(w: integer);
    function getwidth(): integer;
    procedure draw;
  end;

implementation

constructor Rectangle.Create(l, w: integer);
begin
  length := l;
  Width := w;
end;

destructor Rectangle.Destroy();
begin
  WriteLn('Rectangle.destroy()');
  inherited;
end;

procedure Rectangle.setlength(l: integer);
begin
  length := l;
end;

procedure Rectangle.setwidth(w: integer);
begin
  Width := w;
end;

function Rectangle.getlength(): integer;
begin
  getlength := length;
end;

function Rectangle.getwidth(): integer;
begin
  getwidth := Width;
end;

procedure Rectangle.draw;
var
  i, j: integer;
begin
  for i := 1 to length do
  begin
    for j := 1 to Width do
      Write(' * ');
    writeln;
  end;
end;

end.
