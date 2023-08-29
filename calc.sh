set -uvx
set -e
g++64 -shared -o win64/calc.dll -I$HOME/common/include calc.cpp -static
g++32 -shared -o win32/calc.dll -I$HOME/common/include calc.cpp -static
