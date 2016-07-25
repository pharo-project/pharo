error handling
externalCallFailedWith: primErrorCode
	"Raise an error after a failed call to an external function.
	 The primFailCode could be any of:
		- a symbol; one of the standard primitive errors defined in Smalltalk primitiveErrorTable
		- nil; the VM does not support primitive errors and is not providing error codes
		- an integer; one of the FFI codes incremented by Smalltalk primitiveErrorTable size + 2
		  so as not to clash with the standard primitive errors."
	^self error: (primErrorCode isInteger
					ifTrue: [self errorMessageFor: primErrorCode - (Smalltalk primitiveErrorTable size + 2)]
					ifFalse: [primErrorCode isNil
								ifTrue: ['Call to external function failed']
								ifFalse: [primErrorCode]])