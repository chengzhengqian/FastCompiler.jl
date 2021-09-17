module FastCompiler

export FastFunc, compile, saveFastFunc, loadFastFunc
# for debug
# export FastFunc, compile, get_func_info,get_return_info,is_ssa
using JITFunc
using CZQUtils
using DelimitedFiles

function get_return_info(ast)
    ret=ast.code[end-1]
    if("$(ret.args[1])"=="Core.tuple")
        return ret.args[2:end]
    end
end

function is_ssa(expr)
    if(isa(expr,Expr))
        return expr.head!=Symbol("=")
    end
    return false
end


function get_func_info(test_func)
    ms=methods(test_func)
    m=collect(ms)[1]
    ast=Base.uncompressed_ast(m)
    N_arg=m.nargs-1                 # we don't count self
    N_local=length(ast.slotnames)-1-N_arg # number of local variables
    ret_info=get_return_info(ast)
    N_return=length(ret_info)
    # ast.code
    # we first need to remove the return code
    # notice the slot number-1 -> our slot index
    N_slot=N_arg+N_local
    N_ssa=0
    ssa_map=Dict{Int64,Int64}()
    code=ast.code[1:(end-2)]
    for (idx,expr) in enumerate(code)
        if(is_ssa(expr) || isa(expr,Core.SlotNumber))
            N_ssa+=1
            ssa_map[idx]=N_slot+N_ssa
        end
    end
    N_total=N_slot+N_ssa
    constant_map=Dict{Any,Int64}()
    function map_to_idx(expr::Union{Core.SSAValue,Core.SlotNumber})
        if(isa(expr, Core.SlotNumber))
            return expr.id-1
        end
        if(isa(expr, Core.SSAValue))
            return ssa_map[expr.id]
        end        
    end
    function map_to_idx(expr::Number)
        num=convert(Float64,expr)
        if(!(num in keys(constant_map)))
            constant_map[num]=N_total+length(constant_map)+1
        end        
        return constant_map[num]
    end
    function compile_code_to_ssa(expr,idx)
        if(isa(expr,Expr))
            if(expr.head==Symbol("="))
                target_idx=map_to_idx(expr.args[1])
                if(isa(expr.args[2],Expr))
                    if(expr.args[2].head==(:call))
                        op="$(expr.args[2].args[1])"
                        args_idx=map_to_idx.(expr.args[2].args[2:end])
                        return [op,target_idx,args_idx]
                    else
                        error("unexpected head for $(expr)")
                    end
                elseif(isa(expr.args[2],Core.SlotNumber))
                    return ["=",target_idx,[map_to_idx(expr.args[2])]]
                elseif(isa(expr.args[2],Number))
                    return ["=",target_idx,[map_to_idx(expr.args[2])]]
                else
                    error("unexpected $(expr)")
                end
            else
                target_idx=ssa_map[idx]
                op="$(expr.args[1])"
                args_idx=map_to_idx.(expr.args[2:end])
                return  [op,target_idx,args_idx]
            end
        end
        if(isa(expr,Core.SlotNumber))
            target_idx=ssa_map[idx]
            op="="
            args_idx=[map_to_idx(expr)]
            return  [op,target_idx,args_idx]
        end        
    end
    code_ssa=[compile_code_to_ssa(code[idx],idx) for idx in 1:length(code)]
    # compile_code_to_ssa(code[11],1)
    # expr=code[11]
    # expr=code[1]; dump(expr)
    # in the end, we make sure we have
    map_to_idx(-1.0)
    ret_idx=map_to_idx.(ret_info)
    code_ssa,N_total+length(constant_map),N_return,N_arg,ret_idx,constant_map
    # hold constant, N_total increase one if add one constance
end

struct FastFunc
    func::Func
    data::Vector{Float64}
    ret_idx
end

"""
filename="./test_fastfunc"
"""
function saveFastFunc(func::FastFunc,filename)
    saveFunc(func.func,"$(filename)_func")
    saveData(func.ret_idx,"$(filename)_ret_idx")
    saveData(func.data,"$(filename)_data")
end

function loadFastFunc(filename)
    # we need to read as int
    ret_idx=reshape(readdlm("$(filename)_ret_idx",'\n',Int),:)
    data=reshape(loadData("$(filename)_data"),:)
    func=loadFunc("$(filename)_func")
    FastFunc(func,data,ret_idx)
end




function compile(test_func,filename="temp")
    code_ssa,N_data,N_return,N_arg,ret_idx,const_map=get_func_info(test_func)
    buffer=IOBuffer()
    print(buffer,"BITS 64\n")
    for code in code_ssa
        compile_code(code,buffer,const_map[-1.0])
    end
    print(buffer,"ret\n")
    asm=String(take!(buffer))
    jitFunc=Func(asm, filename;mode="nasm")
    data_vec=Vector{Float64}(undef,N_data)
    for (k,v) in const_map
        data_vec[v]=k
    end
    FastFunc(jitFunc,data_vec,ret_idx)
end

function (func::FastFunc)(para...)
    para=collect(para)
    N_arg=length(para)
    func.data[1:N_arg]=para
    # @call (func.func)((Ptr{Float64},)=>Cvoid,func.data)
    # ccall(func.func.ptr,Cvoid,(Ptr{Float64},),func.data)
    runFunc(func.func,func.data)
    func.data[func.ret_idx]
end


function compile_add(code,buffer)
    print(buffer,"movq xmm0,  [rdi+ $((code[3][1]-1)*8)]\n")
    for i in 2:length(code[3])
        print(buffer,"addsd xmm0,    [rdi+ $((code[3][i]-1)*8)]\n")
    end
    print(buffer,"movq     [rdi+ $((code[2]-1)*8)],xmm0\n")
end

function compile_mul(code,buffer)
    print(buffer,"movq xmm0,    [rdi+ $((code[3][1]-1)*8)]\n")
    for i in 2:length(code[3])
        print(buffer,"mulsd xmm0,   [rdi+ $((code[3][i]-1)*8)]\n")
    end
    print(buffer,"movq   [rdi+ $((code[2]-1)*8)],xmm0\n")
end

function compile_sub(code,buffer,minus_one)
    print(buffer,"movq xmm0,    [rdi+ $((code[3][1]-1)*8)]\n")
    if(length(code[3])==1)
        print(buffer,"mulsd xmm0,   [rdi+ $((minus_one-1)*8)]\n")
    else
        for i in 2:length(code[3])
            print(buffer,"subsd xmm0,   [rdi+ $((code[3][i]-1)*8)]\n")
        end
    end
    print(buffer,"movq   [rdi+ $((code[2]-1)*8)],xmm0\n")
end

function compile_equal(code,buffer)
    print(buffer,"movq xmm0,    [rdi+ $((code[3][1]-1)*8)]\n")
    print(buffer,"movq   [rdi+ $((code[2]-1)*8)],xmm0\n")
end


function compile_code(code,buffer,minus_one)
    if(occursin("+" ,code[1]))
        compile_add(code,buffer)
    elseif(occursin("*",code[1]))
        compile_mul(code,buffer)
    elseif(occursin("-",code[1]))
        compile_sub(code,buffer,minus_one)
    elseif(occursin("=",code[1]))
        compile_equal(code,buffer)
    else
        error("unsupported op $(code)\n")
    end
end

end


