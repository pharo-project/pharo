"
I override some methods to correctly generate TFFI methods when using UFFI.
"
Class {
	#name : #TFCalloutMethodBuilder,
	#superclass : #FFICalloutMethodBuilder,
	#classVars : [
		'ABI_MAPPING'
	],
	#pools : [
		'TFAbiTypes'
	],
	#category : #'ThreadedFFI-UFFI'
}

{ #category : #'calling conventions' }
TFCalloutMethodBuilder class >> abiConstantFor: aString [

	| key |
	aString = OSPlatform current ffiCallingConvention ifTrue: [
		^ DEFAULT_ABI ].
	key := {
		       Smalltalk vm operatingSystemName asSymbol.
		       Smalltalk vm architectureName asSymbol.
		       aString asSymbol }.

	^ self abiConstantMapping at: key ifAbsent: [
		  self error: 'The requested ABI is not available for this architecture:'
		  , key printString ]
]

{ #category : #'calling convention' }
TFCalloutMethodBuilder class >> abiConstantMapping [

	^ ABI_MAPPING ifNil: [ self initializeABIMapping ]
]

{ #category : #'initialization - private' }
TFCalloutMethodBuilder class >> initializeABIMapping [

	"self initializeABIMapping"

	^ ABI_MAPPING := {
		  ({ #Win32. #x86_64. #win64 } -> WIN_X86_64_WIN64).
		  ({ #Win32. #x86_64. #gnuw64 } -> WIN_X86_64_GNUW64).
		  ({ #Win32. #x86_64. #cdecl } -> WIN_X86_64_GNUW64).

		  ({ #'Mac OS'. #x86_64. #unix64 } -> DARWIN_X86_64_UNIX64).
		  ({ #'Mac OS'. #x86_64. #win64 } -> DARWIN_X86_64_WIN64).
		  ({ #'Mac OS'. #x86_64. #gnuw64 } -> DARWIN_X86_64_GNUW64).

		  ({ #unix. #x86_64. #unix64 } -> UNIX_X86_64_UNIX64).
		  ({ #unix. #x86_64. #win64 } -> UNIX_X86_64_WIN64).
		  ({ #unix. #x86_64. #gnuw64 } -> UNIX_X86_64_GNUW64).

		  ({ #Win32. #i686. #cdecl } -> WIN_X86_32_SYSV).
		  ({ #Win32. #i686. #sysv } -> WIN_X86_32_SYSV).
		  ({ #Win32. #i686. #stdcall } -> WIN_X86_32_STDCALL).
		  ({ #Win32. #i686. #thisCall } -> WIN_X86_32_THISCALL).
		  ({ #Win32. #i686. #fastCall } -> WIN_X86_32_FASTCALL).
		  ({ #Win32. #i686. #ms_cdecl } -> WIN_X86_32_MS_CDECL).
		  ({ #Win32. #i686. #pascal } -> WIN_X86_32_PASCAL).
		  ({ #Win32. #i686. #register } -> WIN_X86_32_REGISTER).

		  ({ #unix. #i686. #cdecl } -> UNIX_X86_32_SYSV).
		  ({ #unix. #i686. #sysv } -> UNIX_X86_32_SYSV).
		  ({ #unix. #i686. #thisCall } -> UNIX_X86_32_THISCALL).
		  ({ #unix. #i686. #fastCall } -> UNIX_X86_32_FASTCALL).
		  ({ #unix. #i686. #stdcall } -> UNIX_X86_32_STDCALL).
		  ({ #unix. #i686. #ms_cdecl } -> UNIX_X86_32_MS_CDECL).
		  ({ #unix. #i686. #pascal } -> UNIX_X86_32_PASCAL).
		  ({ #unix. #i686. #register } -> UNIX_X86_32_REGISTER).

		} asDictionary
]

{ #category : #'calling convention' }
TFCalloutMethodBuilder >> abiConstantFor: aSelector [

	^ self class abiConstantFor: aSelector
]

{ #category : #private }
TFCalloutMethodBuilder >> createFFICalloutLiteralFromSpec: functionSpec [

	^ TFExternalFunction
		  name: functionSpec functionName
		  moduleName: self libraryName
		  parameterTypes:
		  (functionSpec arguments collect: #tfExternalTypeWithArity)
		  returnType: functionSpec returnType tfExternalTypeWithArity
		  fixedArgumentCount: fixedArgumentCount
		  abi: (self abiConstantFor: calloutAPI callingConvention)
]

{ #category : #private }
TFCalloutMethodBuilder >> generateFFICallout: builder spec: functionSpec ffiLibrary: ffiLibrary [

	TFCalloutAPI isTracing ifTrue: [
		TFCalloutAPI trace: sender.
		builder
			pushLiteral: TFCalloutAPI;
			pushLiteral: sender;
			send: #trace: ].

	builder
		pushLiteral: (ffiLibrary uniqueInstance runner).

	"save ffi call as literal"
	builder pushLiteral: (self createFFICalloutLiteralFromSpec: functionSpec).
	"iterate arguments in order (in the function) to create the function call"

	functionSpec arguments
		do: [ :each |
			each emitArgument: builder context: sender inCallout: self requestor.
			each resolvedType tfExternalTypeWithArity emitMarshallToPrimitive: builder ].

	"create the array"
	builder pushConsArray: functionSpec arguments size.
	builder addTemp: #argumentsArray.
	builder storeTemp: #argumentsArray.

	"send call and store into result"
	builder send: #invokeFunction:withArguments:.

	functionSpec arguments withIndexDo: [ :each :index|
		each emitReturnArgument: builder context: sender.
		each resolvedType tfExternalTypeWithArity
			emitFreeIfNeededOfIndex: index
			argumentsArrayTempName: #argumentsArray
			withBuilder: builder  ].

	"Additional marshall in the case of TFFI"
	functionSpec returnType resolvedType tfExternalTypeWithArity emitMarshallFromPrimitive: builder.

	"convert in case return type needs it. And return reseult"

	^ functionSpec returnType
		emitReturn: builder
		resultTempVar: #result
		context: sender
		inCallout: self requestor
]
