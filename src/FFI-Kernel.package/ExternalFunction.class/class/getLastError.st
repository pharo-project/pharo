error handling
getLastError
	"Return the last error from an external call.
	Only valid immediately after the external call failed."
	<primitive: 'primitiveFFIGetLastError' module:'SqueakFFIPrims'>
	^-1