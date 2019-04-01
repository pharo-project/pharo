Extension { #name : #ExternalType }

{ #category : #'*UnifiedFFI' }
ExternalType >> atomicSelector [
	^ AtomicSelectors at: self atomicType
]

{ #category : #'*UnifiedFFI' }
ExternalType >> offsetReadFieldAt: offsetVariableName [
	"Answer a string defining the accessor to an entity of the receiver type starting at the 
	 given byte offset. 
	 Private. Used for field definition only.
	 NOTE: This is used on UFFI to define field accessors that depends on a class variable 
	 (this works like this to allow mapping 32bits and 64bits structures)"
	self isPointerType ifTrue:
		[| accessor |
		accessor := self pointerSize caseOf: {
						[nil]	->	[#pointerAt:].
						[4]	->	[#shortPointerAt:].
						[8]	->	[#longPointerAt:] }.
		 ^String streamContents:
			[:s|
			 referentClass
				ifNil:
					[s nextPutAll: '^ExternalData fromHandle: (handle ', accessor, ' ';
						nextPutAll: offsetVariableName;
						nextPutAll: ') type: ExternalType ';
						nextPutAll: (AtomicTypeNames at: self atomicType);
						nextPutAll: ' asPointerType']
				ifNotNil:
					[s nextPutAll: '^';
						print: referentClass;
						nextPutAll: ' fromHandle: (handle ', accessor, ' ';
						nextPutAll: offsetVariableName;
						nextPut: $)]]].

	self isAtomic ifFalse: "structure type"
		[^ self offsetReadStructFieldAt: offsetVariableName ].

	"Atomic non-pointer types"
	^String streamContents:
		[:s|
		s nextPutAll:'^handle ';
			nextPutAll: (AtomicSelectors at: self atomicType);
			space; nextPutAll: offsetVariableName].
]

{ #category : #'*UnifiedFFI' }
ExternalType >> offsetReadStructFieldAt: offsetVariableName [
	^ '^ {1} fromHandle: (handle structAt: {2} length: {1} byteSize)' 
		format: { 
			referentClass name.
			offsetVariableName }

]

{ #category : #'*UnifiedFFI' }
ExternalType >> offsetWriteFieldAt: offsetVariableName with: valueName [
	"Answer a string defining the accessor to an entity of the receiver type starting at the given byte offset. 
	 Private. Used for field definition only."
	self isPointerType ifTrue:
		[| accessor |
		accessor := self pointerSize caseOf: {
						[nil]	->	[#pointerAt:].
						[4]	->	[#shortPointerAt:].
						[8]	->	[#longPointerAt:] }.
		^String streamContents:
			[:s|
			s nextPutAll:'handle ', accessor, ' ';
				nextPutAll: offsetVariableName;
				nextPutAll:' put: ';
				nextPutAll: valueName;
				nextPutAll:' getHandle.']].

	self isAtomic ifFalse:[
		^ self offsetWriteStructFieldAt: offsetVariableName with: valueName ].

	^String streamContents:[:s|
		s nextPutAll:'handle ';
			nextPutAll: (AtomicSelectors at: self atomicType); space; 
			nextPutAll: offsetVariableName;
			nextPutAll:' put: ';
			nextPutAll: valueName].
]

{ #category : #'*UnifiedFFI' }
ExternalType >> offsetWriteStructFieldAt: offsetVariableName with: valueName [
	^ 'handle structAt: {1} put: {2} getHandle length: {3} byteSize' 
		format: {
			offsetVariableName. 
			valueName. 
			referentClass name }
]

{ #category : #'*UnifiedFFI' }
ExternalType >> withPointerArity: aNumber [
	self flag: #pharoTodo. "FFI does not understand arity>1 (for now), so I will ignore it and return 
	 the #referencedType in case I want a pointer... this needs to be fixed."
	^ aNumber > 0  
		ifTrue: [ self asPointerType  ]
		ifFalse: [ self ]
]
