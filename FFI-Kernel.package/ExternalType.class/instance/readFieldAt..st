private
readFieldAt: byteOffset
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
			 referentClass
				ifNil:
					[s nextPutAll: '^ExternalData fromHandle: (handle ', accessor, ' ';
						print: byteOffset;
						nextPutAll: ') type: ExternalType ';
						nextPutAll: (AtomicTypeNames at: self atomicType);
						nextPutAll: ' asPointerType']
				ifNotNil:
					[s nextPutAll: '^';
						print: referentClass;
						nextPutAll: ' fromHandle: (handle ', accessor, ' ';
						print: byteOffset;
						nextPut: $)]]].

	self isAtomic ifFalse: "structure type"
		[^String streamContents:[:s|
			s nextPutAll:'^';
				print: referentClass;
				nextPutAll:' fromHandle: (handle structAt: ';
				print: byteOffset;
				nextPutAll:' length: ';
				print: self byteSize;
				nextPutAll:')']].

	"Atomic non-pointer types"
	^String streamContents:
		[:s|
		s nextPutAll:'^handle ';
			nextPutAll: (AtomicSelectors at: self atomicType);
			space; print: byteOffset].