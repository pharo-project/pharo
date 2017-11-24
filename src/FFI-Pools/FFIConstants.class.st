"
Constants definitions for FFI
"
Class {
	#name : #FFIConstants,
	#superclass : #SharedPool,
	#classVars : [
		'FFIAtomicTypeMask',
		'FFIAtomicTypeShift',
		'FFICallFlagThreaded',
		'FFICallTypeApi',
		'FFICallTypeCDecl',
		'FFICallTypesMask',
		'FFIErrorAddressNotFound',
		'FFIErrorAttemptToPassVoid',
		'FFIErrorBadAddress',
		'FFIErrorBadArg',
		'FFIErrorBadArgs',
		'FFIErrorBadAtomicType',
		'FFIErrorBadExternalFunction',
		'FFIErrorBadExternalLibrary',
		'FFIErrorBadReturn',
		'FFIErrorCallFrameTooBig',
		'FFIErrorCallType',
		'FFIErrorCoercionFailed',
		'FFIErrorGenericError',
		'FFIErrorIntAsPointer',
		'FFIErrorInvalidPointer',
		'FFIErrorModuleNotFound',
		'FFIErrorNoModule',
		'FFIErrorNotFunction',
		'FFIErrorStructSize',
		'FFIErrorWrongType',
		'FFIFlagAtomic',
		'FFIFlagPointer',
		'FFIFlagStructure',
		'FFINoCalloutAvailable',
		'FFIStructSizeMask',
		'FFITypeBool',
		'FFITypeDoubleFloat',
		'FFITypeSignedByte',
		'FFITypeSignedChar',
		'FFITypeSignedInt',
		'FFITypeSignedLongLong',
		'FFITypeSignedShort',
		'FFITypeSingleFloat',
		'FFITypeUnsignedByte',
		'FFITypeUnsignedChar',
		'FFITypeUnsignedInt',
		'FFITypeUnsignedLongLong',
		'FFITypeUnsignedShort',
		'FFITypeVoid'
	],
	#category : #'FFI-Pools'
}

{ #category : #'pool initialization' }
FFIConstants class >> initialize [
	"FFIConstants initialize"
	
	self 
		initializeTypeConstants;
		initializeErrorConstants;
		initializeCallingConventions
]

{ #category : #'private - initialization' }
FFIConstants class >> initializeCallingConventions [
	"These constants map onto the flags inst var of an ExternalFunction.
	 The first eight bits define the calling convention.  Attribute flags
	 occupy bits 8 on up."
	"FFIConstants initializeCallingConventions"
	FFICallTypesMask := 255.
	FFICallTypeCDecl := 0.
	FFICallTypeApi := 1.
	FFICallFlagThreaded := 256
]

{ #category : #'private - initialization' }
FFIConstants class >> initializeErrorConstants [
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
]

{ #category : #'private - initialization' }
FFIConstants class >> initializeTypeConstants [
	"type void"
	FFITypeVoid := 0.

	"type bool"
	FFITypeBool := 1.

	"basic integer types.
	note: (integerType anyMask: 1) = integerType isSigned"

	FFITypeUnsignedByte := 2.
	FFITypeSignedByte := 3.
	FFITypeUnsignedShort := 4.
	FFITypeSignedShort := 5.
	FFITypeUnsignedInt := 6.
	FFITypeSignedInt := 7.

	"64bit types"
	FFITypeUnsignedLongLong := 8.
	FFITypeSignedLongLong := 9.

	"special integer types"
	FFITypeUnsignedChar := 10.
	FFITypeSignedChar := 11.

	"float types"
	FFITypeSingleFloat := 12.
	FFITypeDoubleFloat := 13.

	"type flags"
	FFIFlagAtomic := 16r40000. "type is atomic"
	FFIFlagPointer := 16r20000. "type is pointer to base type"
	FFIFlagStructure := 16r10000. "baseType is structure of 64k length"
	FFIStructSizeMask := 16rFFFF. "mask for max size of structure"
	FFIAtomicTypeMask := 16r0F000000. "mask for atomic type spec"
	FFIAtomicTypeShift := 24. "shift for atomic type"

]
