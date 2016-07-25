primitives atomic
ffiFloatAbs: number 
	^ self ffiCall: #( float FloatAbsolute (float number) ) options: #(+optCallbackCall)