"
An ExternalStructure is for representing external data that is
- either a structure composed of different fields (a struct of C language)
- or an alias for another type (like a typedef of C language)

It reserves enough bytes of data for representing all the fields.

The data is stored into the handle instance variable which can be of two different types:
	- ExternalAddress
		If the handle is an external address then the object described does not reside in the Smalltalk object memory.
	- ByteArray
		If the handle is a byte array then the object described resides in Smalltalk memory.

A specific structure is defined by subclassing ExternalStructure and specifying its #fields via a class side method.
For example if we define a subclass:
	ExternalStructure subclass: #StructExample
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'garbage'.
Then declare the fields like this:
    StructExample class compile: 'fields  ^#( (name ''char*'') (color ''ulong'') )' classified: 'garbage'.

It means that this type is composed of two different fields:
- a string (accessed thru the field #name)
- and an unsigned 32bit integer (accessed thru the field #color).
It represents the following C type:
   struct StructExample {char *name; uint32_t color; };

The accessors for those fields can be generated automatically like this:
	StructExample defineFields.
As can be verified in a Browser:
	StructExample browse.
We see that name and color fields are stored sequentially in different zones of data.

The total size of the structure can be verified with:
	StructExample byteSize = (Smalltalk wordSize + 4).

An ExternalStructure can also be used for defining an alias.
The fields definition must contain only 2 elements: an eventual accessor (or nil) and the type.
For example, We can define a machine dependent 'unsigned long' like this:
	ExternalStructure subclass: #UnsignedLong
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'garbage'.
Then set the fields like this:
    UnsignedLong class compile: 'fields  ^(Smalltalk wordSize=4 or: [Smalltalk platformName=''Win64''])
		ifTrue: [#(nil ''ulong'')] ifFalse: [#(nil ''ulonglong'')]' classified: 'garbage'.
And verify the size on current platform:
	UnsignedLong byteSize.
	
Then, the class names 'UnsignedLong' and 'StructExamples' acts as a type specification.
They can be used for composing other types, and for defining prototype of external functions:

LibraryExample>>initMyStruct: aStructExample name: name color: anInteger
	<cdecl: void 'init_my_struct'( StructExample * char * UnsignedLong )>
	self externalCallFailed



"
Class {
	#name : #ExternalStructure,
	#superclass : #ExternalObject,
	#classVars : [
		'PreviousPlatform'
	],
	#pools : [
		'FFIConstants'
	],
	#classInstVars : [
		'compiledSpec'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'field definition' }
ExternalStructure class >> byteSize [
	"Return the size in bytes of this structure."
	^self compiledSpec first bitAnd: FFIStructSizeMask
]

{ #category : #'field definition' }
ExternalStructure class >> checkFieldLayoutChange [
	"Recompile the spec and field accessors if the layout changed.
	Answer true if the layout changed.
	This is usefull at system startup if some structure are machine dependent.
	No provision is made for correct initialization order of nested structures.
	The correct order of invocation is left at upper responsibility."
	
	| newCompiledSpec oldCompiledSpec |
	oldCompiledSpec := compiledSpec.
	newCompiledSpec := self compileFields: self fields withAccessors: #never.
	oldCompiledSpec = newCompiledSpec ifTrue: [^false].
	"only regenerate the automatically generated fields: the others are under user responsibility"
	compiledSpec := self compileFields: self fields withAccessors: #generated.
	ExternalType noticeModificationOf: self.
	^true
]

{ #category : #'field definition' }
ExternalStructure class >> compileAlias: spec withAccessors: aSymbol [

	"Define all the fields in the receiver.
	Return the newly compiled spec."

	| fieldName fieldType isPointerField externalType newCompiledSpec |

	fieldName := spec first.
	fieldType := spec second.
	isPointerField := fieldType last = $*.
	fieldType := fieldType copyWithout: $*.
	externalType := ExternalType atomicTypeNamed: fieldType.
	externalType
		ifNil:
			[ "non-atomic" Symbol hasInterned: fieldType ifTrue: [ :sym | externalType := ExternalType structTypeNamed: sym ] ].
	externalType
		ifNil: [ self trace: '(' , fieldType , ' is void)'.
			externalType := ExternalType void
			].
	isPointerField
		ifTrue: [ externalType := externalType asPointerType ].
	( fieldName notNil and: [ self shouldGenerate: fieldName policy: aSymbol ] )
		ifTrue: [ self defineAliasAccessorsFor: fieldName type: externalType ].
	newCompiledSpec := isPointerField
		ifTrue: [ WordArray with: ( ExternalType structureSpec bitOr: ExternalType pointerSpec ) ]
		ifFalse: [ externalType compiledSpec ].
	^ newCompiledSpec
]

{ #category : #'field definition' }
ExternalStructure class >> compileAllFields [
	"ExternalStructure compileAllFields"
	self withAllSubclassesDo:[:cls|
		cls compileFields.
	].
]

{ #category : #'field definition' }
ExternalStructure class >> compileFields [
	"Compile the field definition of the receiver.
	Return the newly compiled spec."
	^self compileFields: self fields
]

{ #category : #'field definition' }
ExternalStructure class >> compileFields: fieldSpec [
	"Compile the field definition of the receiver.
	Return the newly compiled spec."
	compiledSpec := self compileFields: fieldSpec withAccessors: #never.
	ExternalType noticeModificationOf: self.
	^compiledSpec
]

{ #category : #'field definition' }
ExternalStructure class >> compileFields: specArray withAccessors: aSymbol [ 
	"Compile a type specification for the FFI machinery.
	Return the newly compiled spec.
	Eventually generate the field accessors according to following rules:
	- aSymbol = #always always generate the accessors
	- aSymbol = #never never generate the accessors
	- aSymbol = #generated only generate the auto-generated accessors
	- aSymbol = #absent only generate the absent accessors"
	| byteOffset typeSpec newCompiledSpec |
	(specArray size > 0 and: [specArray first class ~~ Array]) ifTrue:
		[^ self compileAlias: specArray withAccessors: aSymbol].
	byteOffset := 1.
	typeSpec := WriteStream on: (WordArray new: 10).
	typeSpec nextPut: FFIFlagStructure.
	"dummy for size"
	specArray do:
		[:spec | | fieldName fieldType isPointerField externalType typeSize selfRefering |
		fieldName := spec first.
		fieldType := spec second.
		isPointerField := fieldType last = $*.
		fieldType := (fieldType findTokens: ' *') first.
		externalType := ExternalType atomicTypeNamed: fieldType.
		selfRefering := isPointerField and: [externalType isNil and: [fieldType = self asString]].
		selfRefering ifTrue: [
			externalType := ExternalType void asPointerType
		] ifFalse:[
			externalType == nil ifTrue: ["non-atomic"
				Symbol
					hasInterned: fieldType
					ifTrue: [:sym | externalType := ExternalType structTypeNamed: sym].
			].
			externalType == nil ifTrue:[
				self trace: '(' , fieldType , ' is void)'.
				externalType := ExternalType void.
			].
			isPointerField ifTrue: [externalType := externalType asPointerType: self pointerSize]].
			typeSize := externalType byteSize.
			spec size > 2 ifTrue: ["extra size"
				spec third < typeSize
					ifTrue: [^ self error: 'Explicit type size is less than expected'].
				typeSize := spec third.
			].
			(fieldName notNil and: [self shouldGenerate: fieldName policy: aSymbol]) ifTrue: [
				self defineFieldAccessorsFor: fieldName startingAt: byteOffset type: externalType.
			].
			typeSpec nextPutAll: (externalType embeddedSpecWithSize: typeSize).
			byteOffset := byteOffset + typeSize.
		].
	newCompiledSpec := typeSpec contents.
	newCompiledSpec at: 1 put: (byteOffset - 1 bitOr: FFIFlagStructure).
	^ newCompiledSpec
]

{ #category : #'field definition' }
ExternalStructure class >> compiledSpec [
	"Return the compiled spec of the receiver"
	^compiledSpec ifNil:[self compileFields].
]

{ #category : #converting }
ExternalStructure class >> compositeName [
	^'struct'
]

{ #category : #'field definition' }
ExternalStructure class >> defineAliasAccessorsFor: fieldName type: type [
	"Define read/write accessors for the given field"
	| code refClass argName |
	(type isVoid and:[type isPointerType not]) ifTrue:[^self].
	refClass := type referentClass.
	code := String streamContents:[:s|
		s 
			nextPutAll: fieldName; crtab;
			nextPutAll:'"This method was automatically generated"'; crtab;
			nextPut: $<; nextPutAll: #generated; nextPut: $>; crtab.
		refClass == nil 
			ifTrue:[(type isAtomic and:[type isPointerType not]) 
				ifTrue:[s nextPutAll:'^handle']
				ifFalse:[s nextPutAll:'^ExternalData fromHandle: handle'.
						type isPointerType ifTrue:[s nextPutAll:' asExternalPointer'].
						s nextPutAll:' type: ';
						nextPutAll: type externalTypeName]]
			ifFalse:[s nextPutAll:'^', refClass name,' fromHandle: handle'.
					type isPointerType ifTrue:[s nextPutAll:' asExternalPointer']]].
	self compile: code classified: 'accessing'.

	code := String streamContents:[:s|
		argName := refClass == nil 
			ifTrue:[(type isAtomic and:[type isPointerType not])
				ifTrue:['anObject']
				ifFalse:['anExternalData']]
			ifFalse:['a',refClass name].
		s
			nextPutAll: fieldName,': '; nextPutAll: argName; crtab;
			nextPutAll:'"This method was automatically generated"'; crtab;
			nextPut: $<; nextPutAll: #generated; nextPut: $>; crtab.
		(refClass == nil and:[type isAtomic and:[type isPointerType not]])
			ifTrue:[s nextPutAll:'handle := ', argName]
			ifFalse:[s nextPutAll:'handle := ', argName,' getHandle'.
					type isPointerType ifTrue:[s nextPutAll:' asByteArrayPointer']]].
	self compile: code classified: 'accessing'.
]

{ #category : #'field definition' }
ExternalStructure class >> defineFieldAccessorsFor: fieldName startingAt: byteOffset type: type [
	"Define read/write accessors for the given field"
	| comment |
	(type isVoid and: [type isPointerType not]) ifTrue:[^self].
	comment := String streamContents: [:strm |
		strm crtab; nextPutAll: '"This method was automatically generated. See '; nextPutAll: self class name; nextPutAll: '>>fields."'; crtab.
		strm nextPut: $<; nextPutAll: #generated; nextPut: $>; crtab.].
	self maybeCompileAccessor: fieldName, comment, (type readFieldAt: byteOffset)
		withSelector: fieldName asSymbol.
	self maybeCompileAccessor: fieldName,': anObject', comment, (type writeFieldAt: byteOffset with: 'anObject')
		withSelector: (fieldName, ':') asSymbol
]

{ #category : #'field definition' }
ExternalStructure class >> defineFields [
	"Define all the fields in the receiver"
	self defineFields: self fields.
]

{ #category : #'field definition' }
ExternalStructure class >> defineFields: fieldSpec [
	"Define all the fields in the receiver"
	compiledSpec := self compileFields: fieldSpec withAccessors: #always.
	ExternalType noticeModificationOf: self.
	^compiledSpec
]

{ #category : #'class management' }
ExternalStructure class >> doneCompiling [
	"I have been recompiled. Update any types that reference me."
	ExternalType noticeModificationOf: self.
]

{ #category : #'instance creation' }
ExternalStructure class >> externalNew [
	"Create an instance of the receiver on the external heap"
	^self fromHandle: (ExternalAddress allocate: self byteSize)
]

{ #category : #converting }
ExternalStructure class >> externalType [
	"Return an external type describing the receiver as a structure"
	^ExternalType structTypeNamed: self name
]

{ #category : #'field definition' }
ExternalStructure class >> fields [
	"Return the fields defining the receiver"
	^#()
]

{ #category : #'class management' }
ExternalStructure class >> fileOutInitializerOn: aFileStream [
	super fileOutInitializerOn: aFileStream.
	aFileStream cr.
	aFileStream cr.
	aFileStream nextChunkPut: self name , ' compileFields'.
	aFileStream cr.
]

{ #category : #'class management' }
ExternalStructure class >> fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool [
	super fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: aBool.
	(aBool and:[moveSource not]) ifTrue: 
		[aFileStream cr.
		aFileStream cr.
		aFileStream nextChunkPut: self name , ' compileFields'.
		aFileStream cr]
]

{ #category : #'instance creation' }
ExternalStructure class >> fromHandle: aHandle [
	^self basicNew setHandle: aHandle
]

{ #category : #'system startup' }
ExternalStructure class >> install [
	"Resuming the image on another architecture may require a re-compilation of structure layout."
	| newPlatform |
	newPlatform := Smalltalk platformName.
	PreviousPlatform = newPlatform
		ifFalse:
			[self recompileStructures.
			PreviousPlatform := newPlatform]
]

{ #category : #compiling }
ExternalStructure class >> maybeCompileAccessor: aString withSelector: selector [
	(self compiledMethodAt: selector ifAbsent: []) ifNotNil:
		[:existingMethod|
		existingMethod getSourceFromFile asString = aString ifTrue:
			[^self]].
	self compile: aString classified: #accessing
]

{ #category : #'instance creation' }
ExternalStructure class >> new [
	^self fromHandle: (ByteArray new: self byteSize)
]

{ #category : #'class management' }
ExternalStructure class >> obsolete [
	"The receiver is becoming obsolete. 
	NOTE: You if you remove the whole class category at once, you cannot
	assume that the ExternalType class is still present."

	self environment at: #ExternalType ifPresent: [:class | class noticeRemovalOf: self].
	^ super obsolete
]

{ #category : #accessing }
ExternalStructure class >> pointerSize [
	"Answer the size of pointers for this class.  By default answer nil.
	 Subclasses that contain pointers must define the size of a pointer if the code is to operate on 64-bit and 32-bit platforms.
	 Currently we have no way of converting a type between 32- and 64- bit versions beyond recompiling it."
	^nil
]

{ #category : #'system startup' }
ExternalStructure class >> recompileStructures [
	"Check and update the layout of all subclasses for host machine dependency.
	Arrange to check the inner nested structures first."
	
	"ExternalStructure recompileStructures"
	| sorted unsorted |
	unsorted := self withAllSubclasses.
	sorted := OrderedCollection new: unsorted size.
	self sortStructs: unsorted into: sorted.
	sorted do: [:e | e checkFieldLayoutChange]
]

{ #category : #'class management' }
ExternalStructure class >> rename: aString [
	| oldName |
	oldName := name.
	super rename: aString.
	oldName = name ifFalse:[ExternalType noticeRenamingOf: self from: oldName to: name].
]

{ #category : #'field definition' }
ExternalStructure class >> shouldGenerate: fieldname policy: aSymbol [ 
	"Answer true if the field accessors must be compiled.
	Do so according to the following rules:
	- aSymbol = #always always generate the accessors
	- aSymbol = #never never generate the accessors
	- aSymbol = #generated only re-generate the auto-generated accessors
	- aSymbol = #absent only generate the absent accessors"
	aSymbol = #never ifTrue: [^ false].
	aSymbol = #always ifTrue: [^ true].
	aSymbol = #absent ifTrue: [^ (self methodDictionary includesKey: fieldname) not].
	aSymbol = #generated
		ifTrue: [^ (self methodDictionary includesKey: fieldname)
				and: [(self methodDictionary at: fieldname) pragmas
						anySatisfy: [:p | p selector = #generated]]].
	self error: 'unknow generation policy'
]

{ #category : #'field definition' }
ExternalStructure class >> sortStructs: structureClasses into: sortedClasses [ 
	"Sort the structure definitions so as to obtain a correct initialization order."
	
	[| structClass prevStructClass dependsOnOtherTypes |
	structureClasses isEmpty ifTrue: [^ self].
	structClass := structureClasses anyOne.
	
	[dependsOnOtherTypes := structClass typeNamesFromWhichIDepend.
	prevStructClass := structureClasses detect: [:c | c ~~ structClass and: [dependsOnOtherTypes includes: c name]] ifNone: [nil].
	prevStructClass isNil]
		whileFalse: [structClass := prevStructClass].

	"we found a structure/alias which does not depend on other structures/aliases
	add the corresponding class to the initialization list"
	sortedClasses add: (structureClasses remove: structClass)] repeat
]

{ #category : #'field definition' }
ExternalStructure class >> typeNamesFromWhichIDepend [
	"Answer the set of type names of my fields (including pointer stars)"
	| f |
	(f := self fields) isEmpty ifTrue: [^Set new].
	f first isArray ifFalse: [^Set with: f second].
	^f collect: [:e | e second] as: Set
]

{ #category : #converting }
ExternalStructure class >> typedef [
	^self externalType typedef
]

{ #category : #printing }
ExternalStructure >> compositeName [
	^self class compositeName
]

{ #category : #finalization }
ExternalStructure >> free [
	"Free the handle pointed to by the receiver"
	(handle notNil and:[handle isExternalAddress]) ifTrue:[handle free].
	handle := nil.
]

{ #category : #printing }
ExternalStructure >> longPrintOn: aStream [ 
	"Append to the argument, aStream, the names and values of all the record's variables."
	| fields |
	fields := self class fields.
	(fields isEmpty or: [fields first isNil])
		ifTrue: [fields := #()]
		ifFalse: [fields first isArray ifFalse: [fields := Array with: fields]].
	fields do: [ :field |
		field first ifNotNil:
			[aStream nextPutAll: field first; nextPut: $:; space; tab.
			(self perform: field first) printOn: aStream.
			aStream cr]].
]
