pool initialization
initializeErrorConstants
	"FFIConstants initializeErrorConstants"

	"No callout mechanism available"
	FFINoCalloutAvailable := -1.
	"generic error"
	FFIErrorGenericError := 0.
	"primitive invoked without ExternalFunction"
	FFIErrorNotFunction := 1.
	"bad arguments to primitive call"
	FFIErrorBadArgs := 2.

	"generic bad argument"
	FFIErrorBadArg := 3.
	"int passed as pointer"
	FFIErrorIntAsPointer := 4.
	"bad atomic type (e.g., unknown)"
	FFIErrorBadAtomicType := 5.
	"argument coercion failed"
	FFIErrorCoercionFailed := 6.
	"Type check for non-atomic types failed"
	FFIErrorWrongType := 7.
	"struct size wrong or too large"
	FFIErrorStructSize := 8.
	"unsupported calling convention"
	FFIErrorCallType := 9.
	"cannot return the given type"
	FFIErrorBadReturn := 10.
	"bad function address"
	FFIErrorBadAddress := 11.
	"no module given but required for finding address"
	FFIErrorNoModule := 12.
	"function address not found"
	FFIErrorAddressNotFound := 13.
	"attempt to pass 'void' parameter"
	FFIErrorAttemptToPassVoid := 14.
	"module not found"
	FFIErrorModuleNotFound := 15.
	"external library invalid"
	FFIErrorBadExternalLibrary := 16.
	"external function invalid"
	FFIErrorBadExternalFunction := 17.
	"ExternalAddress points to ST memory (don't you dare to do this!)"
	FFIErrorInvalidPointer := 18.
	"Stack frame required more than 16k bytes to pass arguments."
	FFIErrorCallFrameTooBig := 19