primitives
getTime: t
     ^self ffiCall: #(long time(TIME_T_PTR t) ) module: LibC
             