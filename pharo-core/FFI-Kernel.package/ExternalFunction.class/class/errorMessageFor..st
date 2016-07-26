error handling
errorMessageFor: code
	"Return the error message for the given error code from the foreign function interface"
	^FFIErrorMessages at: code ifAbsent:['Call to external function failed'].