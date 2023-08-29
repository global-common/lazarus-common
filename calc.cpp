#include <iostream>
#include <stdint.h>
#include "strconv.h"

extern "C" __declspec(dllexport)
int32_t add2(int32_t a, int32_t b)
{
    std::cout << a << "+" << b << std::endl;
    return a + b;
}
