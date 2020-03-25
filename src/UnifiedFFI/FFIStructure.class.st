"
A structure is data-type that joins together a group of variables, so-called fields.
Each field of a structure has a name and a type.
Structures are often used to group related values, so they can be manipulated together.
For example, let's consider a structure that models a fraction, i.e., a number that has a numerator and a denominator. Both numerator and denominator can be defined as fields of type ==int==.
Such fraction structure data-type, and a function calculating a double precision floting point number from it, are defined in C as follows:

[[[language=c
typedef struct
{
	int numerator;
	int denominator;
} fraction;

double fraction_to_double(fraction* a_fraction){
  return a_fraction -> numerator / (double)(a_fraction -> denominator);
}
]]]

!!!! Defining a structure with ==FFIStructure==

Structures are declared in uFFI as subclasses of the ==FFIStructure== class defining the same fields as defined in C.
For example, defining our fraction structure is done as follows, defining a subclass of ==FFIStructure==, a ==fieldsDesc== class-side method returning the specification of the structure fields, and finally sending the ==rebuildFieldAccessors== message to the structure class we created.

[[[language=smalltalk
FFIStructure subclass: #FractionStructure
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'FFITutorial'

FractionStructure class >> fieldsDesc [
	^ #(
		int numerator;
		int denominator;
		)
]

FractionStructure rebuildFieldAccessors.
]]]

Doing this will automatically generate some boilerplate code to manipulate the structure.
You will see that the structure class gets redefined as follows, containing some auto-generated accessors.

[[[language=smalltalk
FFIStructure subclass: #FractionStructure
	instanceVariableNames: ''
	classVariableNames: 'OFFSET_DENOMINATOR OFFSET_NUMERATOR'
	package: 'FFITutorial'

FractionStructure >> denominator [
	""This method was automatically generated""
	^handle signedLongAt: OFFSET_DENOMINATOR
]

FractionStructure >> denominator: anObject [
	""This method was automatically generated""
	handle signedLongAt: OFFSET_DENOMINATOR put: anObject
]

FractionStructure >> numerator [
	""This method was automatically generated""
	^handle signedLongAt: OFFSET_NUMERATOR
]

FractionStructure >> numerator: anObject [
	""This method was automatically generated""
	handle signedLongAt: OFFSET_NUMERATOR put: anObject
]
]]]

Once a structure type is defined, we can allocate structures from it using the ==new== and ==externalNew== messages, that will allocate it in the Pharo heap or the external C heap respectively.

[[[language=smalltalk
""In Pharo heap""
aFraction := FractionStructure new.

""In C heap""
aFraction := FractionStructure externalNew.
]]]

We read or write in our structure using the auto-generated accessors.

[[[language=smalltalk
aFraction numerator: 40.
aFraction denominator: 7.
]]]

And we can use it as an argument in a call\-out by using its type.

[[[language=smalltalk
FFITutorial >> fractionToDouble: aFraction [
  ^ self ffiCall: #(double fraction_to_double(FractionStructure* a_fraction))
]

FFITutorial new fractionToDouble: aFraction.
>>> 5.714285714285714
]]]

!!!! Implementation details

I'm an extension of ExternalStructure.
I extend my parent functionality by adding: 

- field description 
- field accessing implementation 
- ...
"
Class {
	#name : #FFIStructure,
	#superclass : #ExternalStructure,
	#classVars : [
		'LastCompilationPlatform'
	],
	#classInstVars : [
		'externalStructureAlignment'
	],
	#category : #'UnifiedFFI-Objects'
}

{ #category : #converting }
FFIStructure class >> asExternalTypeOn: generator [
	^ FFIExternalStructureType objectClass: self
]

{ #category : #private }
FFIStructure class >> compileFields: specArray withAccessors: aSymbol [
	| offset fieldSpec defineBoolean|
	defineBoolean := aSymbol = #always.
	fieldSpec := self fieldSpec.
	offset := 0.	
	externalStructureAlignment := 1.
	fieldSpec fieldsAndTypesDo: [ :fieldName :type |
		offset := offset alignedTo: type typeAlignment.
		self defineFieldOffset: fieldName value: offset + 1.  
		(defineBoolean and: [ fieldName isSymbol ]) ifTrue: [ 
			self defineFieldAccessorsFor: fieldName type: type ].
		offset := offset + type typeSize.
		externalStructureAlignment := externalStructureAlignment max: type typeAlignment  ].
	offset := offset alignedTo: externalStructureAlignment.
	
	"Real compiled spec is the compiled spec of fields plus a header with structure size and 
	 structure flag"
	"Temporal type to ensure cyclic (pointer) references will work (case of linked lists, 
	 for example). I do not like it, but it works :S"
	compiledSpec := {ExternalType pointerSpec}. 
	"Now I can reconsider it"
	compiledSpec := fieldSpec compileSpec copyWithFirst: (offset bitOr: FFIFlagStructure).
	ExternalType noticeModificationOf: self.
	^ compiledSpec
]

{ #category : #private }
FFIStructure class >> defineFieldAccessorsFor: fieldName startingAt: byteOffset type: type [
	"Define read/write accessors for the given field. 
	 This method is equivallent to his parent, but it changes a couple of things: 
	 - type is FFIExternalType and not ExternalType
	 - it generates to another protocol, not accessing"
	| code |
	
	(type isVoid and: [ type isPointer not ]) ifTrue:[ ^ self ].
	
	code := fieldName,'
	"This method was automatically generated"
	', (type readFieldAt: byteOffset).
	self 
		compile: code 
		classified: self generatedFieldProtocolName.
	
	code := fieldName,': anObject
	"This method was automatically generated"
	', (type writeFieldAt: byteOffset with:'anObject').
	self 
		compile: code 
		classified: self generatedFieldProtocolName.
]

{ #category : #private }
FFIStructure class >> defineFieldAccessorsFor: fieldName type: type [
	"Define read/write accessors for the given field. 
	 This method is equivallent to his parent, but it changes a couple of things: 
	 - type is FFIExternalType and not ExternalType
	 - it generates to another protocol, not accessing"
	| code |
	
	(type isVoid and: [ type isPointer not ]) ifTrue:[ ^ self ].
	
	code := fieldName,'
	"This method was automatically generated"
	', (type offsetReadFieldAt: (self offsetVariableNameFor: fieldName)).
	self 
		compile: code 
		classified: self generatedFieldProtocolName.
	
	code := fieldName,': anObject
	"This method was automatically generated"
	', (type offsetWriteFieldAt: (self offsetVariableNameFor: fieldName) with:'anObject').
	self 
		compile: code 
		classified: self generatedFieldProtocolName.
]

{ #category : #private }
FFIStructure class >> defineFieldOffset: fieldName value: offset [
	"Add offset values to classPool."
	| offsetVarName |
	offsetVarName := self offsetVariableNameFor: fieldName.
	"Is this field defined in a superclass?"
	(superclass bindingOf: offsetVarName)
		ifNil: [
			(self hasClassVarNamed: offsetVarName) 
				ifFalse: [ self addClassVarNamed: offsetVarName ].
			self classVarNamed: offsetVarName put: offset ].
]

{ #category : #'register marshalling' }
FFIStructure class >> emitFlatStructureLayoutInto: flatStructureLayout [
	flatStructureLayout alignTo: self structureAlignment.
	self fieldSpec fieldsAndTypesDo: [ :field :type |
		type emitFlatStructureLayoutFieldInto: flatStructureLayout
	].
	flatStructureLayout alignTo: self structureAlignment.
]

{ #category : #'instance creation' }
FFIStructure class >> externalNew [
	"Create an instance of the receiver on the external heap"
	^self fromHandle: (ExternalAddress allocate: self structureSize)
]

{ #category : #private }
FFIStructure class >> fieldSpec [
	^ FFIExternalStructureFieldParser new  
		parseFields: self fieldsDesc structure: self.

]

{ #category : #'field definition' }
FFIStructure class >> fields [
	^ self fieldsDesc
]

{ #category : #'field definition' }
FFIStructure class >> fieldsDesc [
	"override this method in a subclass to get an automatically generated
	field accessors.
	
	The field description format is following:
	
	#( 
 		type1 name1;
		type2 name2;
		....
	)
	"
	^ #()
]

{ #category : #'register marshalling' }
FFIStructure class >> flatStructureLayout [
	| result |
	result := FFIExternalStructureFlatLayout new.
	self emitFlatStructureLayoutInto: result.
	^ result
]

{ #category : #'instance creation' }
FFIStructure class >> fromHandle: aHandle [
	"Offsets are calculated when specs are compiled, but sometimes there is a possibility that 
	 user access them before actually using the compiled spec, so I need to be sure they are 
	 initialised before anything. This is why I send #compiledSpec here."
	self compiledSpec. 
	^ (super fromHandle: aHandle) initialize 
]

{ #category : #private }
FFIStructure class >> generatedFieldProtocolName [
	^ 'accessing structure variables'
]

{ #category : #testing }
FFIStructure class >> isNormalAlignedStructure [
	^ true
]

{ #category : #accessing }
FFIStructure class >> nestedStructures [

	"Retuns the direct nested structures of this structure"
	^ self fieldSpec types
		select: [ :each | each isExternalStructure and: [ each isPointer not ] ]
		thenCollect: [ :each | each objectClass ]
]

{ #category : #private }
FFIStructure class >> offsetFieldPrefix [
	^ 'OFFSET'
]

{ #category : #private }
FFIStructure class >> offsetVariableNameFor: fieldName [
	^ (self offsetFieldPrefix, '_', fieldName asUppercase) asSymbol
]

{ #category : #private }
FFIStructure class >> rebuildFieldAccessors [

	Author useAuthor: 'UFFIGenerator' during: [
		self removeAllOffsetVariables. 
		self 
			compileFields: self fields 
			withAccessors: #always]
]

{ #category : #private }
FFIStructure class >> removeAllMethodsInProtocol: protocolName [
	| protocol |
	
	protocol := self organization protocolNamed: protocolName.
	protocol ifNil: [ ^ self ].
	
	protocol methods do: [ :each | self removeSelector: each ]
]

{ #category : #private }
FFIStructure class >> removeAllOffsetVariables [
	self classPool keys 
		select: [ :each | each beginsWith: 'OFFSET_' ]
		thenDo: [ :each | self removeClassVarNamed: each ]
]

{ #category : #initialization }
FFIStructure class >> reset [
	"Reset this structure and all its nested structures"
	self resetStructureIfNotIn: Set new
]

{ #category : #'class management' }
FFIStructure class >> resetAllStructures [
	"Reset the offsets of all structures.
	This is required when there is a platform change since type sizes and padding rules change.
	
	Manage nested structures with a recursive approach.
	Stop recursing using a tracking collection of already reset structures."
	
	| alreadyReset currentCompilationPlatform |
	currentCompilationPlatform := { OSPlatform current family . Smalltalk vm wordSize }.
	LastCompilationPlatform = currentCompilationPlatform
		ifTrue: [ ^ self ].
	
	LastCompilationPlatform := currentCompilationPlatform.
	alreadyReset := IdentitySet new.
	self allSubclassesDo: [ :each | each resetStructureIfNotIn: alreadyReset ].
]

{ #category : #'class management' }
FFIStructure class >> resetStructureIfNotIn: alreadyReset [
	"Reset myself by recompiling all my fields.
	
	Reset all my nested structures recursively if any.
	Stop recursing using a tracking collection of already reset structures."
	(alreadyReset includes: self)
		ifTrue: [ ^ self ].

	alreadyReset add: self.
	self nestedStructures do: [ :each | each resetStructureIfNotIn: alreadyReset ].
	self compileFields.
]

{ #category : #accessing }
FFIStructure class >> structureAlignment [
	externalStructureAlignment ifNil: [ self compileFields ].
	^ externalStructureAlignment
]

{ #category : #accessing }
FFIStructure class >> structureSize [
	^ self byteSize
]

{ #category : #accessing }
FFIStructure >> address [
	self deprecated: 'Use #getHandle' on: '27 October 2015' in: 'Pharo5'. 
	^ self getHandle
]

{ #category : #converting }
FFIStructure >> pointer [
	self getHandle isExternalAddress 
		ifFalse: [ self error: 'Structures need to be moved to external memory space before passing them as pointers.' ].
	^ self getHandle pointer
]

{ #category : #printing }
FFIStructure >> printOn: aStream [ 
	"Append to the argument, aStream, the names and values of all the record's variables."
	aStream nextPutAll: self class name; nextPutAll: ' ( '; cr.
	self class fieldSpec fieldNames do: [ :field |
		aStream nextPutAll: field; nextPut: $:; space; tab.
			(self perform: field ) printOn: aStream.
			] separatedBy: [ aStream cr ].
	aStream cr; nextPut: $)
]
