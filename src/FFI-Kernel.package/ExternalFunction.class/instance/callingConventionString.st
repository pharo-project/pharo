printing
callingConventionString
	(flags allMask: FFICallTypeApi) 
		ifTrue:[^'apicall']
		ifFalse:[^'cdecl']