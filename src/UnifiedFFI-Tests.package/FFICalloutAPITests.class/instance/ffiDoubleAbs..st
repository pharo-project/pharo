primitives atomic
ffiDoubleAbs: number 
	^ self ffiCall: #( double DoubleAbsolute (double number) ) options: #(+optCallbackCall)