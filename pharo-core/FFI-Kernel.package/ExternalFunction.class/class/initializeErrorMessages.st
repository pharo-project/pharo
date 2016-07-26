class initialization
initializeErrorMessages
	"ExternalFunction initializeErrorMessages"
	FFIErrorMessages := Dictionary new.
	FFIErrorMessages
		at: FFINoCalloutAvailable put: 'Callout mechanism not available';
		at: FFIErrorGenericError put: 'A call to an external function failed';
		at: FFIErrorNotFunction put: 'Only ExternalFunctions can be called';
		at: FFIErrorBadArgs put: 'Bad arguments in primitive invocation';
		at: FFIErrorBadArg put: 'Bad argument for external function';
		at: FFIErrorIntAsPointer put: 'Cannot use integer as pointer';
		at: FFIErrorBadAtomicType put: 'Unknown atomic type in external call';
		at: FFIErrorCoercionFailed put: 'Could not coerce arguments';
		at: FFIErrorWrongType put: 'Wrong type in external call';
		at: FFIErrorStructSize put: 'Bad structure size in external call';
		at: FFIErrorCallType put: 'Unsupported calling convention';
		at: FFIErrorBadReturn put: 'Cannot return the given type';
		at: FFIErrorBadAddress put: 'Bad function address';
		at: FFIErrorNoModule put: 'No module to load address from';
		at: FFIErrorAddressNotFound put: 'Unable to find function address';
		at: FFIErrorAttemptToPassVoid put: 'Cannot pass ''void'' parameter';
		at: FFIErrorModuleNotFound put: 'External module not found';
		at: FFIErrorBadExternalLibrary put: 'External library is invalid';
		at: FFIErrorBadExternalFunction put: 'External function is invalid';
		at: FFIErrorInvalidPointer put: 'Attempt to pass invalid pointer';
		at: FFIErrorCallFrameTooBig put: 'Call requires more than 16k of stack space';
	yourself