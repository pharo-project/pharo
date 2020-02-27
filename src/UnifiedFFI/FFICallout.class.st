"
I'm implement a callout mechanism to be used with nb calls formatting.

Callout arguments can be either:
- an integer constant, boolean or nil
- a type name (string or symbol)
- a class name
- a class variable
- any other object, which responds to #asExternalTypeOn:

Options: 
Options may change the execution/compilation of ffi calls. 
Not many are predefined, here a small explanation of them: 

optIndirectCall 		This will perform an ""indirect function call"" as explained here: https://en.wikipedia.org/wiki/Function_pointer

"
Class {
	#name : #FFICallout,
	#superclass : #Object,
	#instVars : [
		'fnSpec',
		'options',
		'requestor',
		'methodArgs',
		'receiver',
		'method',
		'resolutionMode',
		'stringEncodingStrategy'
	],
	#classVars : [
		'TypeAliases'
	],
	#category : #'UnifiedFFI-Callouts'
}

{ #category : #'class initialization' }
FFICallout class >> initialize [
	self initializeTypeAliases
]

{ #category : #'class initialization' }
FFICallout class >> initializeTypeAliases [
	"self initializeTypeAliases"
	TypeAliases := Dictionary newFromPairs:
	#(
		"not really a type, useful only as return type or with pointers "
		void 				FFIVoid
		"0/!0  <-> false/true "
		bool 				FFIBool
		"fixed size integer types, byte order is platform dependent "
		int8 				FFIInt8
		uint8 				FFIUInt8
		int16 				FFIInt16
		uint16 			FFIUInt16
		int32 				FFIInt32
		uint32 			FFIUInt32
		int64 				FFIInt64
		uint64 			FFIUInt64
		" aliases to common C compiler types.. some of them are platform dependent, 
		  some is not.. to be sorted out later "		
		signedByte 		int8
		unsignedByte 	uint8
		signedShort 		int16
		unsignedShort 	uint16
		signedChar 		int8  
		unsignedChar 	uint8
		schar 				int8
		uchar 				uint8
		signedLong 		int32
		unsignedLong 	uint32
		sbyte 				int8
		byte 				uint8
		short 				int16
		ushort 			uint16
		long 				FFILong
		ulong 				FFIULong
		longlong 			int64
		ulonglong 		uint64
		uint 				uint32
		int 				int32
		"unsigned for sizes.. usually same size as platform's word size"
		size_t 			FFISizeT
		"character type. uint8 <-> accepts Character/Smallint as argument, converts 
		 return to Character "
		Character 		FFICharacterType
		char 				FFICharacterType
		"Floats fixed-size. platform-dependent byte order"
		float16 			FFIFloat16
		float32 			FFIFloat32
		float64 			FFIFloat64
		float128 			FFIFloat128
		"Floats, C type name aliases"
		float 				float32
		double 			float64
		shortFloat 		float16
		"Special types"
		oop 				FFIOop
		ByteArray       FFIOop
		ExternalAddress FFIOop)
	
]

{ #category : #accessing }
FFICallout class >> typeAliases [
	^ TypeAliases
]

{ #category : #private }
FFICallout >> aliasForType: aTypeName [
	| alias |

	alias := aTypeName.
	(requestor notNil and: [ requestor respondsTo: #externalTypeAlias: ]) 
		ifTrue: [ 
			alias := requestor externalTypeAlias: aTypeName.
			alias ifNil: [ alias := aTypeName ] ].
	" internal aliases "
	^ TypeAliases at: alias ifAbsent: [ alias ]
]

{ #category : #configuration }
FFICallout >> beNonStrict [

	self beStrict: false
]

{ #category : #configuration }
FFICallout >> beStrict [
	
	self beStrict: true
]

{ #category : #configuration }
FFICallout >> beStrict: aBoolean [
	
	self options at: #optStrict put: aBoolean
]

{ #category : #accessing }
FFICallout >> callType: aCallType [
	aCallType == #cdecl ifTrue: [ ^ self cdecl ].
	aCallType == #stdcall ifTrue: [ ^ self stdcall ].
	
	self error: 'unknown call type'
]

{ #category : #accessing }
FFICallout >> fnSpec [
	^ fnSpec
]

{ #category : #'argument loaders' }
FFICallout >> indirectLoader: aLoader byIndex: anIndex [
	"I do not have a case where it is used, so I don't know how to implement it. 
	 I will wait until an use case emerge..."
	^ self notYetImplemented
]

{ #category : #initialization }
FFICallout >> initialize [
	super initialize.
	options := Dictionary new.
]

{ #category : #configuration }
FFICallout >> isMandatoryStringEncoding [

	^ self optionAt: #optStringEncodingMandatory
]

{ #category : #testing }
FFICallout >> isStrict [

	^ self resolutionMode isStrict
]

{ #category : #'argument loaders' }
FFICallout >> loaderForArgNamed: argName [
	| loader |
	"try getting the argument from the method arguments"
	loader := self loaderFromMethodArgsNamed: argName.
	loader ifNil: [ 
		"special case, receiver argument"
		argName = 'self' ifTrue: [ loader := self receiverArgumentLoader ].
		loader ifNil: [ 
			"Ask the requestor for the argument"
			loader := requestor ffiInstVarArgument: argName generator: self.
			loader ifNil: [ | binding |
				binding := requestor ffiBindingOf: argName.
				binding ifNil: [ FFIVariableNameNotFound signalFor: argName].
				loader := FFIClassVariableArgument new 
					argName: argName;
					yourself ] ] ].
	^ loader
]

{ #category : #'argument loaders' }
FFICallout >> loaderForArgNamed: argName indirectIndex: anIndex [
	| loader |
	loader := self loaderForArgNamed: argName.
	anIndex ifNotNil: [ loader := self indirectLoader: loader byIndex: anIndex ].
	^ loader
]

{ #category : #'argument loaders' }
FFICallout >> loaderFromMethodArgsNamed: argName [
	| index |
	methodArgs ifNil: [ ^ nil ].
	index := methodArgs indexOf: argName ifAbsent: [ nil ].
	^ index ifNotNil: [ 
		"ok, this is a method argument"
		^ FFIMethodArgument new 
			argName: argName;
			index: index;
			yourself ].

]

{ #category : #accessing }
FFICallout >> method [
	^ method
]

{ #category : #accessing }
FFICallout >> method: aMethod [
	method := aMethod
]

{ #category : #accessing }
FFICallout >> methodArgs [
	^ methodArgs
]

{ #category : #accessing }
FFICallout >> methodArgs: anArray [
	methodArgs := anArray
]

{ #category : #accessing }
FFICallout >> namedFnSpec: namedFn [
	fnSpec := self newSpecParser parseNamedFunction: namedFn.
	
]

{ #category : #accessing }
FFICallout >> newSpecParser [
	^ FFIFunctionParser new requestor: self

]

{ #category : #accessing }
FFICallout >> optionAt: optionName [
	^ self options 
		at: optionName 
		ifAbsent: false
]

{ #category : #accessing }
FFICallout >> options [
	^ options
]

{ #category : #accessing }
FFICallout >> options: anObject [
	self parseOptions: anObject
]

{ #category : #accessing }
FFICallout >> parseOptions: optionsArray [
	"parse an array, which is a sequence of options in a form of: 
	
	#( + option1 option2 option3: param3  - option5 -option6: ... )
	
	each time the #+ is seen, the options which follow it will be subject for inclusion
	and, correspondingly, if #- seen, then they will be excluded	.

	By default, (if none of #+ or #- specified initially), all options are subject for inclusion.
	
	If in inclusion mode, non-unary options will put the following element as its unary symbols counterparts value, rather than true.
	"

	| include option rs |
	include := true.
	rs := optionsArray readStream.
	[(option := rs next) isNil]
		whileFalse: [ 
			option
				caseOf:
					{(#+ -> [ include := true ]).
					(#- -> [ include := false ])}
				otherwise: [ 
					(option beginsWith: 'opt') not
						ifTrue: [ ^ self error: 'Invalid option name ' , option , ', must begin with ''opt''' ].
					option numArgs > 1
						ifTrue: [ ^ self error: 'Does not support setting options with more than one parameter' ].
					options at: (option allButLast: option numArgs) asSymbol put: (include and: [ option isUnary or: [ rs next ] ]) ] ]
]

{ #category : #accessing }
FFICallout >> receiver [
	^ receiver
]

{ #category : #accessing }
FFICallout >> receiver: anObject [
	receiver := anObject
]

{ #category : #private }
FFICallout >> receiverArgumentLoader [
	^ FFISelfArgument new
]

{ #category : #accessing }
FFICallout >> requestor [
	^ requestor
]

{ #category : #accessing }
FFICallout >> requestor: aRequestor [
	requestor := aRequestor
]

{ #category : #accessing }
FFICallout >> resolutionMode [
	
	^ (self optionAt: #optStrict)
		ifTrue: [ FFIStrictResolutionMode new ]
		ifFalse: [ FFIInferenceResolutionMode new ]
]

{ #category : #accessing }
FFICallout >> resolveExternalType: anObject [ 
	^ (self resolveType: anObject) externalType 
]

{ #category : #configuration }
FFICallout >> resolveStringEncodingStrategy [

	| encodingOptions |
	encodingOptions := (self options associations
		select: [ :e | (e key beginsWith: 'optStringEncoding') and: [ e value ] ]
		thenCollect: [ :e | e key allButFirst: 'optStringEncoding' size ]) asSet.
	
	"Do not take into account optStringEncodingMandatory as an encoding"
	encodingOptions remove: 'Mandatory' ifAbsent: [  ].
	
	encodingOptions size > 1 ifTrue: [ self error: 'Conflicting string encoding options' ].
	
	(self isMandatoryStringEncoding and: [ encodingOptions isEmpty ])
		ifTrue: [ self error: 'String encoding option not specified' ].
	
	^ encodingOptions isEmpty
		ifTrue: [ FFILegacyStringEncodingStrategy new ]
		ifFalse: [ FFIStringEncodingStrategy forEncoding: encodingOptions anyOne ]
]

{ #category : #accessing }
FFICallout >> resolveType: aTypeName [
	" a type name could be
	 - a class variable name
	- a class name
	- a type name 
	- a type name, followed by arbitrary number pointer chars - $*"
	| name newName resolver binding ptrArity |
	
	newName := aTypeName.
	ptrArity := 0.
	"resolve aliases and pointers"
	[ 
		name := newName asString trimRight.
		newName := self aliasForType: name.
		newName last = $* ifTrue: [ 
			ptrArity := ptrArity + 1.
			newName := newName allButLast ].
		name = newName 
	] whileFalse.

	resolver := requestor 
		ifNotNil: [ 
			requestor isClass
				ifTrue: [ requestor instanceSide ]
				ifFalse: [ requestor ] ]
		ifNil: [ self class ].
		
	binding := resolver ffiBindingOf: name asSymbol.
	binding ifNotNil: [ 
		^ (binding value asExternalTypeOn: self) pointerArity: ptrArity ].

	^ self error: 'Unable to resolve external type: ' , aTypeName asString
]

{ #category : #'spec parsing' }
FFICallout >> resolveUntypedArgument: anArgument [

	^ self resolutionMode resolveUndeclaredTypeForArgument: anArgument withResolver: self
]

{ #category : #accessing }
FFICallout >> sender: aSenderContext [
	| nArgs |
	self requestor: aSenderContext method methodClass.
	nArgs := aSenderContext method numArgs.
	methodArgs := aSenderContext method argumentNames.
	self receiver: aSenderContext receiver.
	self assert: (methodArgs size = nArgs).

]

{ #category : #configuration }
FFICallout >> stringEncodingStrategy [

	^ stringEncodingStrategy ifNil: [ 
		stringEncodingStrategy := self resolveStringEncodingStrategy ]
]

{ #category : #configuration }
FFICallout >> stringEncodingStrategy: aStringEncodingStrategy [

	stringEncodingStrategy := aStringEncodingStrategy
]

{ #category : #private }
FFICallout >> typeName: aName pointerArity: ptrArity [
	^ (self resolveType: aName) pointerArity: ptrArity
]

{ #category : #errors }
FFICallout >> unsupportedUntypedLiteral: aLiteral [
	
	FFIUnsupportedUntypedLiteral signalFor: aLiteral
]
