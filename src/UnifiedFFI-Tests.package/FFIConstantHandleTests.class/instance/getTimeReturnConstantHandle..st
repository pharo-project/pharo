primitives
getTimeReturnConstantHandle: t
     ^self ffiCall: #(FFIConstantHandle time(TIME_T_PTR t) ) module: LibC