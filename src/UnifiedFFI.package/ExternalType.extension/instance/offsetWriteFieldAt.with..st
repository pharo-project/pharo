*UnifiedFFI
offsetWriteFieldAt: offsetVariableName with: valueName
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