"
This class provides an abstract base for all structures that can be used by external functions. ExternalStructures have two possible handle types:
	- ExternalAddress
		If the handle is an external address then the object described does not reside in the Smalltalk object memory.
	- ByteArray
		If the handle is a byte array then the object described resides in Smalltalk memory.
Useful methods should be implemented by subclasses of ExternalStructure using the common ByteArray/ExternalAddress platform dependent access protocol which will transparently access the correct memory location.
"
Class {
	#name : #ExternalStructure,
	#superclass : #ExternalObject,
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
ExternalStructure class >> compileAlias: spec withAccessors: aBool [
	"Define all the fields in the receiver.
	Return the newly compiled spec."
	| fieldName fieldType isPointerField externalType |
	fieldName := spec first.
	fieldType := spec second.
	isPointerField := fieldType last = $*.
	fieldType := fieldType copyWithout: $*.
	externalType := ExternalType atomicTypeNamed: fieldType.
	externalType == nil ifTrue:["non-atomic"
		Symbol hasInterned: fieldType ifTrue:[:sym|
			externalType := ExternalType structTypeNamed: sym]].
	externalType == nil ifTrue:[
		Transcript show:'(', fieldType,' is void)'.
		externalType := ExternalType void].
	isPointerField ifTrue:[externalType := externalType asPointerType].
	(fieldName notNil and:[aBool]) ifTrue:[
		self defineAliasAccessorsFor: fieldName
			type: externalType].
	isPointerField 
		ifTrue:[compiledSpec := WordArray with: 
					(ExternalType structureSpec bitOr: ExternalType pointerSpec)]
		ifFalse:[compiledSpec := externalType compiledSpec].
	ExternalType noticeModificationOf: self.
	^compiledSpec
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
	^self compileFields: fieldSpec withAccessors: false.
]

{ #category : #'field definition' }
ExternalStructure class >> compileFields: specArray withAccessors: aBool [ 
	"Define all the fields in the receiver. 
	Return the newly compiled spec."
	| byteOffset typeSpec |
	(specArray size > 0 and: [specArray first class ~~ Array]) ifTrue:
		[^ self compileAlias: specArray withAccessors: aBool].
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
				Transcript show: '(' , fieldType , ' is void)'.
				externalType := ExternalType void.
			].
			isPointerField ifTrue: [externalType := externalType asPointerType: self pointerSize]].
			typeSize := externalType byteSize.
			spec size > 2 ifTrue: ["extra size"
				spec third < typeSize
					ifTrue: [^ self error: 'Explicit type size is less than expected'].
				typeSize := spec third.
			].
			(fieldName notNil and: [aBool]) ifTrue: [
				self defineFieldAccessorsFor: fieldName startingAt: byteOffset type: externalType.
			].
			typeSpec nextPutAll: (externalType embeddedSpecWithSize: typeSize).
			byteOffset := byteOffset + typeSize.
		].
	compiledSpec := typeSpec contents.
	compiledSpec at: 1 put: (byteOffset - 1 bitOr: FFIFlagStructure).
	ExternalType noticeModificationOf: self.
	^ compiledSpec
]

{ #category : #'field definition' }
ExternalStructure class >> compiledSpec [
	"Return the compiled spec of the receiver"
	^compiledSpec ifNil:[self compileFields].
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
			nextPutAll:'"This method was automatically generated"'; crtab.
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
			nextPutAll:'"This method was automatically generated"'; crtab.
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
	comment := ('\	"This method was automatically generated. See ', self class name, '>>fields."\	') withCRs.
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
ExternalStructure class >> defineFields: fields [
	"Define all the fields in the receiver"
	self compileFields: fields withAccessors: true.
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

	Smalltalk at: #ExternalType ifPresent: [:class | class noticeRemovalOf: self].
	^ super obsolete
]

{ #category : #accessing }
ExternalStructure class >> pointerSize [
	"Answer the size of pointers for this class.  By default answer nil.
	 Subclasses that contain pointers must define the size of a pointer if the code is to operate on 64-bit and 32-bit platforms.
	 Currently we have no way of converting a type between 32- and 64- bit versions beyond recompiling it."
	^nil
]

{ #category : #'class management' }
ExternalStructure class >> rename: aString [
	| oldName |
	oldName := name.
	super rename: aString.
	oldName = name ifFalse:[ExternalType noticeRenamingOf: self from: oldName to: name].
]

{ #category : #converting }
ExternalStructure class >> typedef [
	^self externalType typedef
]

{ #category : #'initialize-release' }
ExternalStructure >> free [
	"Free the handle pointed to by the receiver"
	(handle ~~ nil and:[handle isExternalAddress]) ifTrue:[handle free].
	handle := nil.
]

{ #category : #printing }
ExternalStructure >> longPrintOn: aStream [ 
	"Append to the argument, aStream, the names and values of all the record's variables."
	| fields |
	fields := self class fields.
	(fields isEmpty or: [fields first isNil]) ifTrue: [fields := #()]
		ifFalse: [(fields first isKindOf: Array) ifFalse: [fields := Array with: fields]].
	fields do: [ :field |
		field first notNil ifTrue: [
			aStream nextPutAll: field first; nextPut: $:; space; tab.
			(self perform: field first) printOn: aStream.
			aStream cr]].
]
