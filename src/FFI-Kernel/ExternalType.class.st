"
An external type represents the type of external objects.

Instance variables:
	compiledSpec	<WordArray>		Compiled specification of the external type
	referentClass	<Behavior | nil>	Class type of argument required
	referencedType	<ExternalType>	Associated (non)pointer type with the receiver

Compiled Spec:
The compiled spec defines the type in terms which are understood by the VM. Each word is defined as:
	bits 0...15 	- byte size of the entity
	bit 16		- structure flag (FFIFlagStructure)
				  This flag is set if the following words define a structure
	bit 17		- pointer flag (FFIFlagPointer)
				  This flag is set if the entity represents a pointer to another object
	bit 18		- atomic flag (FFIFlagAtomic)
				  This flag is set if the entity represents an atomic type.
				  If the flag is set the atomic type bits are valid.
	bits 19...23	- unused
	bits 24...27	- atomic type (FFITypeVoid ... FFITypeDoubleFloat)
	bits 28...31	- unused

Note that all combinations of the flags FFIFlagPointer, FFIFlagAtomic, and FFIFlagStructure are invalid, EXCEPT from the following:

	FFIFlagPointer + FFIFlagAtomic:
		This defines a pointer to an atomic type (e.g., 'char*', 'int*').
		The actual atomic type is represented in the atomic type bits.

	FFIFlagPointer + FFIFlagStructure:
		This defines a structure which is a typedef of a pointer type as in
			typedef void* VoidPointer;
			typedef Pixmap* PixmapPtr;
		It requires a byte size of four (e.g. a 32bit pointer) to work correctly.

[Note: Other combinations may be allowed in the future]

"
Class {
	#name : #ExternalType,
	#superclass : #Object,
	#instVars : [
		'compiledSpec',
		'referentClass',
		'referencedType',
		'pointerSize'
	],
	#classVars : [
		'AtomicSelectors',
		'AtomicTypeNames',
		'AtomicTypes',
		'StructTypes'
	],
	#pools : [
		'FFIConstants'
	],
	#category : #'FFI-Kernel'
}

{ #category : #private }
ExternalType class >> atomicTypeNamed: aString [
	^AtomicTypes at: aString ifAbsent:[nil]
]

{ #category : #'type constants' }
ExternalType class >> bool [
	^AtomicTypes at: 'bool'
]

{ #category : #'type constants' }
ExternalType class >> byte [
	"byte defaults to unsigned byte"
	^self unsignedByte
]

{ #category : #'type constants' }
ExternalType class >> char [
	"char defaults to unsigned char"
	^self unsignedChar
]

{ #category : #housekeeping }
ExternalType class >> cleanupUnusedTypes [
	"ExternalType cleanupUnusedTypes"
	| value |
	StructTypes keys do:[:key|
		value := StructTypes at: key.
		value == nil ifTrue:[StructTypes removeKey: key ifAbsent:[]]].
]

{ #category : #'type constants' }
ExternalType class >> double [
	^AtomicTypes at: 'double'
]

{ #category : #'type constants' }
ExternalType class >> float [
	^AtomicTypes at: 'float'
]

{ #category : #private }
ExternalType class >> forceTypeNamed: aString [
	^self newTypeNamed: aString force: true
]

{ #category : #'class initialization' }
ExternalType class >> initialize [
	"ExternalType initialize"
	self initializeFFIConstants.
	self initializeDefaultTypes.
]

{ #category : #'class initialization' }
ExternalType class >> initializeAtomicTypes [
	"ExternalType initialize"
	| atomicType byteSize type typeName |
	#(
		"name		atomic id		byte size"
		('void' 		0 				0)
		('bool' 		1 				1)
		('byte' 		2 				1)
		('sbyte' 	3 				1)
		('ushort' 	4 				2)
		('short' 		5 				2)
		('ulong' 	6 				4)
		('long' 		7 				4)
		('ulonglong' 8 				8)
		('longlong' 	9 				8)
		('char' 		10 				1)
		('schar' 	11 				1)
		('float' 		12 				4)
		('double' 	13 				8)
	) do:[:typeSpec| | compiled |
		typeName := typeSpec first.
		atomicType := typeSpec second.
		byteSize := typeSpec third.
		compiled := WordArray with: ((byteSize bitOr: FFIFlagAtomic) bitOr:
				(atomicType bitShift: FFIAtomicTypeShift)).
		type := (AtomicTypes at: typeName).
		type compiledSpec: compiled.
		compiled := WordArray with: ((self pointerSpec bitOr: FFIFlagAtomic) bitOr:
				(atomicType bitShift: FFIAtomicTypeShift)).
		type asPointerType compiledSpec: compiled.
	].
]

{ #category : #'class initialization' }
ExternalType class >> initializeDefaultTypes [
	"ExternalType initialize"
	| type pointerType |
	AtomicTypes = nil ifTrue:[
		"Create new atomic types and setup the dictionaries"
		AtomicTypes := Dictionary new.
		StructTypes := WeakValueDictionary new.
		AtomicTypeNames valuesDo:[:k|
			type := self basicNew.
			pointerType := self basicNew.
			AtomicTypes at: k put: type.
			type setReferencedType: pointerType.
			pointerType setReferencedType: type.
		].
	].
	self initializeAtomicTypes.
	self initializeStructureTypes.
	"AtomicTypes := nil"
]

{ #category : #'class initialization' }
ExternalType class >> initializeFFIConstants [
	"ExternalType initialize"
	FFIConstants initialize. "ensure proper initialization"
	AtomicTypeNames := IdentityDictionary new.
	AtomicSelectors := IdentityDictionary new.
	AtomicTypeNames
		at: FFITypeVoid put: 'void';
		at: FFITypeBool put: 'bool';
		at: FFITypeUnsignedByte put: 'byte';
		at: FFITypeSignedByte put: 'sbyte';
		at: FFITypeUnsignedShort put: 'ushort';
		at: FFITypeSignedShort put: 'short';
		at: FFITypeUnsignedInt put: 'ulong';
		at: FFITypeSignedInt put: 'long';
		at: FFITypeUnsignedLongLong put: 'ulonglong';
		at: FFITypeSignedLongLong put: 'longlong';
		at: FFITypeUnsignedChar put: 'char';
		at: FFITypeSignedChar put: 'schar';
		at: FFITypeSingleFloat put: 'float';
		at: FFITypeDoubleFloat put: 'double';
	yourself.

	AtomicSelectors
		at: FFITypeVoid put: #voidAt:;
		at: FFITypeBool put: #booleanAt:;
		at: FFITypeUnsignedByte put: #unsignedByteAt:;
		at: FFITypeSignedByte put: #signedByteAt:;
		at: FFITypeUnsignedShort put: #unsignedShortAt:;
		at: FFITypeSignedShort put: #signedShortAt:;
		at: FFITypeUnsignedInt put: #unsignedLongAt:;
		at: FFITypeSignedInt put: #signedLongAt:;
		at: FFITypeUnsignedLongLong put: #unsignedLongLongAt:;
		at: FFITypeSignedLongLong put: #signedLongLongAt:;
		at: FFITypeUnsignedChar put: #unsignedCharAt:;
		at: FFITypeSignedChar put: #signedCharAt:;
		at: FFITypeSingleFloat put: #floatAt:;
		at: FFITypeDoubleFloat put: #doubleAt:;
	yourself
]

{ #category : #'class initialization' }
ExternalType class >> initializeStructureTypes [
	"ExternalType initialize"
	| referentClass pointerType |
	self cleanupUnusedTypes.
	StructTypes keysAndValuesDo:[:referentName :type|
		referentClass := (self environment at: referentName ifAbsent:[nil]).
		(referentClass isBehavior and:[
			referentClass includesBehavior: ExternalStructure])
				ifFalse:[referentClass := nil].
		type compiledSpec: 
			(WordArray with: self structureSpec).
		type newReferentClass: referentClass.
		pointerType := type asPointerType.
		pointerType compiledSpec: 
			(WordArray with: self pointerSpec).
		pointerType newReferentClass: referentClass.
	].
]

{ #category : #'type constants' }
ExternalType class >> long [
	^self signedLong
]

{ #category : #'instance creation' }
ExternalType class >> new [
	"Use either the type constants or #externalType for creating external types"
	^self shouldNotImplement
]

{ #category : #private }
ExternalType class >> newTypeNamed: aString force: aBool [
	| sym type referentClass pointerType |
	sym := aString asSymbol.
	type := StructTypes at: aString ifAbsent:[nil].
	type == nil ifFalse:[^type].
	referentClass := self environment at: sym ifAbsent:[nil].
	(referentClass isBehavior and:[referentClass includesBehavior: ExternalStructure])
		ifFalse:[referentClass := nil].
	"If we don't have a referent class and are not forced to create a type get out"
	(referentClass == nil and:[aBool not]) ifTrue:[^nil].
	type := self basicNew compiledSpec: 
		(WordArray with: self structureSpec).
	pointerType := self basicNew compiledSpec: 
		(WordArray with: self pointerSpec).
	type setReferencedType: pointerType.
	pointerType setReferencedType: type.
	type newReferentClass: referentClass.
	pointerType newReferentClass: referentClass.
	StructTypes at: sym put: type.
	^type
]

{ #category : #housekeeping }
ExternalType class >> noticeModificationOf: aClass [
	"A subclass of ExternalStructure has been redefined.
	Clean out any obsolete references to its type."
	| type |
	aClass isBehavior ifFalse:[^nil]. "how could this happen?"
	aClass withAllSubclassesDo:[:cls|
		type := StructTypes at: cls name ifAbsent:[nil].
		type == nil ifFalse:[
			type newReferentClass: cls.
			type asPointerType newReferentClass: cls].
	].
]

{ #category : #housekeeping }
ExternalType class >> noticeRemovalOf: aClass [
	"A subclass of ExternalStructure is being removed.
	Clean out any obsolete references to its type."
	| type |
	type := StructTypes at: aClass name ifAbsent:[nil].
	type == nil ifFalse:[
		type newReferentClass: nil.
		type asPointerType newReferentClass: nil].

]

{ #category : #housekeeping }
ExternalType class >> noticeRenamingOf: aClass from: oldName to: newName [
	"An ExternalStructure has been renamed from oldName to newName.
	Keep our type names in sync."
	| type |
	type := StructTypes at: oldName ifAbsent:[nil].
	type == nil ifFalse:[StructTypes at: newName put: type].
	StructTypes removeKey: oldName ifAbsent:[].
]

{ #category : #private }
ExternalType class >> pointerSpec [
	^(Smalltalk wordSize bitOr: FFIFlagPointer)
]

{ #category : #'type constants' }
ExternalType class >> sbyte [
	^self signedByte
]

{ #category : #'type constants' }
ExternalType class >> schar [
	^self signedChar
]

{ #category : #'type constants' }
ExternalType class >> short [
	^self signedShort
]

{ #category : #'type constants' }
ExternalType class >> signedByte [
	^AtomicTypes at: 'sbyte'
]

{ #category : #'type constants' }
ExternalType class >> signedChar [
	^AtomicTypes at: 'schar'
]

{ #category : #'type constants' }
ExternalType class >> signedLong [
	^AtomicTypes at: 'long'
]

{ #category : #'type constants' }
ExternalType class >> signedLongLong [
	^AtomicTypes at: 'longlong'
]

{ #category : #'type constants' }
ExternalType class >> signedShort [
	^AtomicTypes at: 'short'
]

{ #category : #'type constants' }
ExternalType class >> string [
	^(AtomicTypes at: 'char') asPointerType
]

{ #category : #private }
ExternalType class >> structTypeNamed: aSymbol [
	aSymbol == nil ifTrue:[^nil].
	^self newTypeNamed: aSymbol force: false
]

{ #category : #private }
ExternalType class >> structureSpec [
	^FFIFlagStructure
]

{ #category : #'type constants' }
ExternalType class >> ulong [
	^self unsignedLong
]

{ #category : #'type constants' }
ExternalType class >> unsignedByte [
	^AtomicTypes at: 'byte'
]

{ #category : #'type constants' }
ExternalType class >> unsignedChar [
	^AtomicTypes at: 'char'
]

{ #category : #'type constants' }
ExternalType class >> unsignedLong [
	^AtomicTypes at: 'ulong'
]

{ #category : #'type constants' }
ExternalType class >> unsignedLongLong [
	^AtomicTypes at: 'ulonglong'
]

{ #category : #'type constants' }
ExternalType class >> unsignedShort [
	^AtomicTypes at: 'ushort'
]

{ #category : #'type constants' }
ExternalType class >> ushort [
	^self unsignedShort
]

{ #category : #'type constants' }
ExternalType class >> void [
	^AtomicTypes at: 'void'
]

{ #category : #converting }
ExternalType >> asNonPointerType [
	"convert the receiver into a non pointer type"
	self isPointerType
		ifTrue:[^referencedType]
		ifFalse:[^self]
]

{ #category : #converting }
ExternalType >> asPointerType [
	"convert the receiver into a pointer type"
	self isPointerType
		ifTrue:[^self]
		ifFalse:[^referencedType]
]

{ #category : #converting }
ExternalType >> asPointerType: anotherPointerSize [
	"convert the receiver into a pointer type"
	| type |
	type := self asPointerType.
	^type pointerSize = anotherPointerSize
		ifTrue: [type]
		ifFalse:
			[type copy pointerSize: anotherPointerSize; yourself]
]

{ #category : #accessing }
ExternalType >> atomicType [
	^(self headerWord bitAnd: FFIAtomicTypeMask) >> FFIAtomicTypeShift
]

{ #category : #accessing }
ExternalType >> byteSize [
	"Return the size in bytes of this type"
	^self headerWord bitAnd: FFIStructSizeMask
]

{ #category : #accessing }
ExternalType >> compiledSpec [
	"Return the compiled spec of the receiver"
	^compiledSpec
]

{ #category : #private }
ExternalType >> compiledSpec: aWordArray [
	compiledSpec := aWordArray.
]

{ #category : #private }
ExternalType >> embeddedSpecWithSize: typeSize [
	"Return a compiled spec for embedding in a new compiled spec."
	| spec header |
	spec := self compiledSpec copy.
	header := spec at: 1.
	header := (header bitAnd: FFIStructSizeMask bitInvert32) bitOr: typeSize.
	spec at: 1 put: header.
	(self isStructureType and:[self isPointerType not])
		ifTrue:[spec := spec copyWith: self class structureSpec].
	^spec
]

{ #category : #private }
ExternalType >> externalTypeName [
	^'ExternalType ', (AtomicTypeNames at: self atomicType), ' asPointerType'
]

{ #category : #private }
ExternalType >> headerWord [
	"Return the compiled header word"
	^compiledSpec at: 1
]

{ #category : #testing }
ExternalType >> isAtomic [
	"Return true if the receiver describes a built-in type"
	^self headerWord anyMask: FFIFlagAtomic
]

{ #category : #testing }
ExternalType >> isIntegerType [
	"Return true if the receiver is a built-in integer type"
	| type |
	type := self atomicType.
	^type > FFITypeBool and:[type <= FFITypeUnsignedLongLong]
]

{ #category : #testing }
ExternalType >> isPointerType [
	"Return true if the receiver represents a pointer type"
	^self isStructureType not and:[self headerWord anyMask: FFIFlagPointer]
]

{ #category : #testing }
ExternalType >> isSigned [
	"Return true if the receiver is a signed type.
	Note: Only useful for integer types."
	^self atomicType anyMask: 1
]

{ #category : #testing }
ExternalType >> isStructureType [
	"Return true if the receiver represents a structure type"
	^self headerWord anyMask: FFIFlagStructure
]

{ #category : #testing }
ExternalType >> isUnsigned [
	"Return true if the receiver is an unsigned type.
	Note: Only useful for integer types."
	^self isSigned not
]

{ #category : #testing }
ExternalType >> isVoid [
	"Return true if the receiver describes a plain 'void' type"
	^self isAtomic and:[self atomicType = 0]
]

{ #category : #private }
ExternalType >> newReferentClass: aClass [

	"The class I'm referencing has changed. Update my spec."

	referentClass := aClass.
	self isPointerType
		ifTrue: [ ^ self ].	"for pointers only the referentClass changed"
	compiledSpec := referentClass
		ifNil: [ "my class has been removed - make me 'struct { void }'" WordArray with: FFIFlagStructure ]
		ifNotNil: [ "my class has been changed - update my compiledSpec" referentClass compiledSpec ]
]

{ #category : #accessing }
ExternalType >> pointerSize [
	"Answer the pointer size of the receiver, if specified."
	^pointerSize
]

{ #category : #private }
ExternalType >> pointerSize: anInteger [
	| spec |
	pointerSize := anInteger.
	(((spec := compiledSpec at: 1) anyMask: FFIFlagPointer)
	 and: [(spec bitAnd: FFIStructSizeMask) ~= anInteger]) ifTrue:
		[compiledSpec := compiledSpec shallowCopy.
		 compiledSpec at: 1 put: (spec bitClear: FFIStructSizeMask) + anInteger]
]

{ #category : #printing }
ExternalType >> printAtomicType: spec on: aStream [
	self assert: (spec anyMask: FFIFlagAtomic).
	aStream nextPutAll: (#(	'void' 'unsigned char' 'unsigned char' 'signed char'
							'unsigned short' 'short' 'unsigned long' 'long'
							'unsigned long long' 'long long' 'char' 'signed char'
							'float' 'double') at: ((spec bitAnd: FFIAtomicTypeMask) bitShift: FFIAtomicTypeShift negated) + 1).
	aStream space.
	(spec anyMask: FFIFlagPointer) ifTrue:
		[aStream nextPut: $*]
]

{ #category : #printing }
ExternalType >> printOn: aStream [

	referentClass
		ifNil: [ aStream nextPutAll: ( AtomicTypeNames at: self atomicType ) ]
		ifNotNil: [ aStream nextPutAll: referentClass name ].
	self isPointerType
		ifTrue: [ aStream nextPut: $* ]
]

{ #category : #printing }
ExternalType >> printStructureFieldStartingAt: initialSpecIndex withName: name inClass: structureClass on: aStream indent: indent [
	"Print the structure's field starting at initialSpecIndex and answer the index in compiledSpec of the subsequent type."
	| spec subStructureClassBinding |
	aStream tab: indent.
	spec := compiledSpec at: initialSpecIndex.
	(spec anyMask: FFIFlagAtomic) ifTrue:
		[self printAtomicType: spec on: aStream.
		 aStream nextPutAll: (name ifNotNil: [name] ifNil: ['foo']).
		 ^initialSpecIndex + 1].
	 subStructureClassBinding := (structureClass >> name) literals detect:
									[:l| l isVariableBinding and: [l value inheritsFrom: ExternalStructure]].
	(spec bitClear: FFIStructSizeMask) = FFIFlagStructure ifTrue:
		[| next |
		 next := initialSpecIndex + 1.
		 aStream
			nextPutAll: subStructureClassBinding value compositeName;
			nextPutAll: ' {'.
		 subStructureClassBinding value fields withIndexDo:
			[:tuple :i|
			aStream cr.
			next := self printStructureFieldStartingAt: next
						withName: tuple first
						inClass: subStructureClassBinding value
						on: aStream
						indent: indent + 1.
			aStream nextPut: $;].
		 aStream crtab: indent; nextPut: $}.
		 name ifNotNil: [aStream space; nextPutAll: name].
		 self assert: (next - 1 = compiledSpec size or: [(compiledSpec at: next) = FFIFlagStructure]).
		 ^next <= compiledSpec size ifTrue: [next + 1] ifFalse: [next]].
	self assert: (spec anyMask: FFIFlagPointer).
	(subStructureClassBinding value isKindOf: ExternalUnion)
			ifTrue: [aStream nextPutAll: 'union ']
			ifFalse: [aStream nextPutAll: 'struct '].
	aStream nextPutAll: subStructureClassBinding value name; nextPutAll: ' *'; nextPutAll: name.
	^initialSpecIndex + 1
]

{ #category : #printing }
ExternalType >> printTypedefOn: s [
	s nextPutAll: 'typedef '.
	(referentClass notNil
	 and: [(compiledSpec first bitClear: FFIStructSizeMask) = FFIFlagStructure])
		ifTrue:
			[| next |
			 next := 2.
			 s
				nextPutAll: referentClass compositeName;
				nextPutAll: ' {'.
			 referentClass fields withIndexDo:
				[:tuple :i|
				s cr.
				next := self printStructureFieldStartingAt: next
							withName: tuple first
							inClass: referentClass
							on: s
							indent: 1.
				s nextPut: $;].
			 s cr; nextPutAll: '} '.
			 self assert: (next - 1 = compiledSpec size or: [(compiledSpec at: next) = FFIFlagStructure])]
		ifFalse:
			[self printAtomicType: compiledSpec first on: s].
	s nextPutAll: (referentClass ifNotNil: [referentClass name] ifNil: ['foo'])
]

{ #category : #private }
ExternalType >> readFieldAt: byteOffset [
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
]

{ #category : #accessing }
ExternalType >> referentClass [
	"Return the class specifying the receiver"
	^referentClass
]

{ #category : #private }
ExternalType >> setReferencedType: aType [
	referencedType := aType
]

{ #category : #printing }
ExternalType >> storeOn: aStream [

	referentClass
		ifNil: [ aStream
				nextPutAll: ExternalType name;
				space;
				nextPutAll: ( AtomicTypeNames at: self atomicType )
			]
		ifNotNil: [ aStream
				nextPut: $(;
				nextPutAll: ExternalType name;
				space;
				nextPutAll: #structTypeNamed:;
				space;
				store: referentClass name;
				nextPut: $)
			].
	self isPointerType
		ifTrue: [ aStream
				space;
				nextPutAll: #asPointer
			]
]

{ #category : #printing }
ExternalType >> typedef [
	^String streamContents: [:s| self printTypedefOn: s]
]

{ #category : #private }
ExternalType >> writeFieldAt: byteOffset with: valueName [
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
				print: byteOffset;
				nextPutAll:' put: ';
				nextPutAll: valueName;
				nextPutAll:' getHandle.']].

	self isAtomic ifFalse:[
		^String streamContents:[:s|
			s nextPutAll:'handle structAt: ';
				print: byteOffset;
				nextPutAll:' put: ';
				nextPutAll: valueName;
				nextPutAll:' getHandle';
				nextPutAll:' length: ';
				print: self byteSize;
				nextPutAll:'.']].

	^String streamContents:[:s|
		s nextPutAll:'handle ';
			nextPutAll: (AtomicSelectors at: self atomicType);
			space; print: byteOffset;
			nextPutAll:' put: ';
			nextPutAll: valueName].
]
