*UnifiedFFI
offsetReadFieldAt: offsetVariableName
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