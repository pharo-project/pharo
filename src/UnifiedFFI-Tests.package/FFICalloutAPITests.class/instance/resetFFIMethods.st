running
resetFFIMethods
	| ffiMethods |
	ffiMethods := FFICalloutAPITests methodDict select: [ :method | (method selector beginsWith: 'ffi') or: [method selector beginsWith: 'prim'] ].
	ffiMethods do: #recompile