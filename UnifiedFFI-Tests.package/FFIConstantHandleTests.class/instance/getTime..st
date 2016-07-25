primitives
getTime: t
     ^self ffiCall: #(long time(FFIConstantHandle *t) ) module: LibC