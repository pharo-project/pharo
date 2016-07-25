*FFI-Kernel
externalCallFailed
	"Raise an error after a failed call to an external function"
	| errCode |
	errCode := ExternalFunction getLastError. "this allows us to look at the actual error code"
	^self error: (ExternalFunction errorMessageFor: errCode).