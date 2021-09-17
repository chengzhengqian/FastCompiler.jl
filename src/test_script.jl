# in PKg mode, ]dev  FastCompiler
using Revise

using FastCompiler
using JITFunc
# it only compiles the following function, no condition, scale input, a tuple of reture values
function test_func(x1,x2,x3,x4)
    y1=x1*x2
    y2=x1*y1*3+x3+1
    y5=y1+y2*y2
    y6=y1*y2*y2
    return y5,y1,y2,y6
end

func=compile(test_func)
test_input=rand(4)
result1=collect(test_func(test_input...))
result2=func(test_input...)

saveFastFunc(func,"test_fast_func2")
func_loaded=loadFastFunc("test_fast_func2")
result3=func_loaded(test_input...)

