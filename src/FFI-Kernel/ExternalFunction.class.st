"
This class represents an external function called from Smalltalk. Instances of ExternalFunction can be created if the address/parameters of the function are known by some other means than loading from a shared library or compiling the appropriate primitive specification.

Instance variables:
	flags	<Integer>	a set of flags encoding the calling convention
	args	<Array of: ExternalType>		the parameters of the function

Implementation notes:

The arguments consist of an array with the first element defining the return type, the remaining arguments defining the parameters of the call.

"
Class {
	#name : #ExternalFunction,
	#superclass : #ExternalObject,
	#instVars : [
		'flags',
		'argTypes'
	],
	#classVars : [
		'FFIErrorMessages'
	],
	#pools : [
		'FFIConstants'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'compiler support' }
ExternalFunction class >> atomicTypeNamed: aString [
	^ExternalType atomicTypeNamed: aString
]

{ #category : #constants }
ExternalFunction class >> callTypeAPI [
	^FFICallTypeApi
]

{ #category : #constants }
ExternalFunction class >> callTypeCDecl [
	^FFICallTypeCDecl
]

{ #category : #'compiler support' }
ExternalFunction class >> callingConventionFor: aString [
	"Return the constant describing the calling convention for the given string specification or nil if unknown."
	aString = 'cdecl:' ifTrue:[^self callTypeCDecl].
	aString = 'apicall:' ifTrue:[^self callTypeAPI].
	^nil
]

{ #category : #'compiler support' }
ExternalFunction class >> callingConventionModifierFor: aString [
	"Return the constant describing the calling convention modifier for the given string specification or nil if unknown."
	aString = 'threaded' ifTrue:[^FFICallFlagThreaded].
	^nil
]

{ #category : #'error handling' }
ExternalFunction class >> errorMessageFor: code [
	"Return the error message for the given error code from the foreign function interface"
	^FFIErrorMessages at: code ifAbsent:['Call to external function failed'].
]

{ #category : #'error handling' }
ExternalFunction class >> externalCallFailed [
	"Raise an error after a failed call to an external function"
	| errCode |
	errCode := self getLastError. "this allows us to look at the actual error code"
	^self error: (self errorMessageFor: errCode).
]

{ #category : #'error handling' }
ExternalFunction class >> externalCallFailedWith: primErrorCode [

	"Raise an error after a failed call to an external function.
	 The primFailCode could be any of:
		- a symbol; one of the standard primitive errors defined in Smalltalk primitiveErrorTable
		- nil; the VM does not support primitive errors and is not providing error codes
		- an integer; one of the FFI codes incremented by Smalltalk primitiveErrorTable size + 2
		  so as not to clash with the standard primitive errors."

	^ self
		error:
			( primErrorCode isInteger
				ifTrue: [ self errorMessageFor: primErrorCode - ( Smalltalk primitiveErrorTable size + 2 ) ]
				ifFalse:
					[ primErrorCode ifNil: [ 'Call to external function failed' ] ifNotNil: [ primErrorCode ] ] )
]

{ #category : #'compiler support' }
ExternalFunction class >> forceTypeNamed: aString [
	^ExternalType forceTypeNamed: aString
]

{ #category : #'error handling' }
ExternalFunction class >> getLastError [
	"Return the last error from an external call.
	Only valid immediately after the external call failed."
	<primitive: 'primitiveFFIGetLastError' module:'SqueakFFIPrims'>
	^-1
]

{ #category : #'class initialization' }
ExternalFunction class >> initialize [
	"ExternalFunction initialize"
	FFIConstants initialize. "ensure proper initialization"
	self initializeErrorMessages.
	(Smalltalk specialObjectsArray at: 47) == self 
		ifFalse:[Smalltalk recreateSpecialObjectsArray].

]

{ #category : #'class initialization' }
ExternalFunction class >> initializeErrorMessages [
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
]

{ #category : #'compiler support' }
ExternalFunction class >> isValidType: anObject [
	^anObject isBehavior and:[anObject includesBehavior: ExternalStructure]
]

{ #category : #'compiler support' }
ExternalFunction class >> structTypeNamed: aString [
	^ExternalType structTypeNamed: aString
]

{ #category : #'class initialization' }
ExternalFunction class >> unload [
	"Clean out the splObj array"
	Smalltalk specialObjectsArray from: 44 to: 48 put: nil.

]

{ #category : #accessing }
ExternalFunction >> argTypes [
	^argTypes
]

{ #category : #printing }
ExternalFunction >> callingConventionString [
	(flags allMask: FFICallTypeApi) 
		ifTrue:[^'apicall']
		ifFalse:[^'cdecl']
]

{ #category : #accessing }
ExternalFunction >> errorCodeName [
	^nil
]

{ #category : #accessing }
ExternalFunction >> flags [
	^flags
]

{ #category : #initialization }
ExternalFunction >> initialize [
	"Initialize the receiver"
	handle := ExternalAddress new.
]

{ #category : #invoking }
ExternalFunction >> invoke [
	^self invokeWithArguments: #()
]

{ #category : #invoking }
ExternalFunction >> invokeWith: arg1 [
	^self invokeWithArguments: (Array with: arg1)
]

{ #category : #invoking }
ExternalFunction >> invokeWith: arg1 with: arg2 [
	^self invokeWithArguments: (Array with: arg1 with: arg2)
]

{ #category : #invoking }
ExternalFunction >> invokeWith: arg1 with: arg2 with: arg3 [
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3)
]

{ #category : #invoking }
ExternalFunction >> invokeWith: arg1 with: arg2 with: arg3 with: arg4 [
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3 with: arg4)
]

{ #category : #invoking }
ExternalFunction >> invokeWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 [
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3 with: arg4 with: arg5)
]

{ #category : #invoking }
ExternalFunction >> invokeWith: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 [
	^self invokeWithArguments: (Array with: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6)
]

{ #category : #invoking }
ExternalFunction >> invokeWithArguments: argArray [
	"Manually invoke the receiver, representing an external function."
	<primitive: 'primitiveCalloutWithArgs' module: 'SqueakFFIPrims' error: ec>
	^self externalCallFailed
]

{ #category : #accessing }
ExternalFunction >> module [
	^nil
]

{ #category : #accessing }
ExternalFunction >> name [
	^nil
]

{ #category : #printing }
ExternalFunction >> printOn: aStream [
	aStream
		nextPut:$<;
		nextPutAll: self callingConventionString; nextPutAll:': '.
	{ 'threaded' } with: { FFICallFlagThreaded } do:
		[:modifier :flag|
		(flags anyMask: flag) ifTrue:
			[aStream nextPutAll: modifier; space]].
	aStream print: argTypes first; space.
	self name == nil
		ifTrue:[aStream nextPutAll:'(*) ']
		ifFalse:[aStream print: self name asString; space].
	aStream nextPut:$(.
	2 to: argTypes size do:[:i|
		aStream print: (argTypes at: i).
		i < argTypes size ifTrue:[aStream space]].
	aStream nextPut:$).
	self module == nil ifFalse:[
		aStream space; nextPutAll:'module: '; print: self module asString.
	].
	self errorCodeName == nil ifFalse:[
		aStream space; nextPutAll:'error: '; nextPutAll: self errorCodeName.
	].
	aStream nextPut:$>
]

{ #category : #invoking }
ExternalFunction >> tryInvokeWithArguments: argArray [
	"Sent from the debugger to simulate an FFI call."
	<primitive: 'primitiveCalloutWithArgs' module: 'SqueakFFIPrims' error: ec>
	
	^thisContext class primitiveFailTokenFor: ec
]
