initialize-release
forceLoading
	"Primitive. Force loading the given library.
	The primitive will fail if the library is not available
	or if anything is wrong with the receiver."
	<primitive: 'primitiveForceLoad' module:'SqueakFFIPrims'>
	^self externalCallFailed "The primitive will set the error code"