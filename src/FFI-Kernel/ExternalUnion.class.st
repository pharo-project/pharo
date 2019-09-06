"
An ExternalUnion is for representing external data that is a union of possible fields.
It corresponds to the C type union.
It reserves enough bytes of data for representing the largest field.

A specific union is defined by subclassing ExternalUnion and specifying its #fields via a class side.

For example if we define a subclass:
	ExternalUnion subclass: #UnionExample
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'garbage'.
Then set the fields like this:
    UnionExample class compile: 'fields  ^#( (name ''char*'') (color ''ulong'') )' classified: 'garbage'.

It means that this type will represent
- either a string (accessed through the field #name)
- or an unsigned 32bit integer (accessed thru the field #color).

It represents the following C type:
   union UnionExample {char *name; uint32_t color; };

The accessors for those fields can be generated automatically like this:
	UnionExample defineFields.
As can be verified in a Browser:
	UnionExample browse.
We see that color and name fields both interpret the same zone of data (starting at 1st byte), but with a different interpretation.
The size of the union can be verified with:
	UnionExample byteSize = (Smalltalk wordSize max: 4).
"
Class {
	#name : #ExternalUnion,
	#superclass : #ExternalStructure,
	#category : #'FFI-Kernel'
}

{ #category : #'field definition' }
ExternalUnion class >> compileFields: specArray withAccessors: aSymbol [ 
	"Compile a type specification for the FFI machinery.
	Return the newly compiled spec.
	Eventually generate the field accessors according to the policy defined in aSymbol."
	| byteOffset maxByteSize typeSpec newCompiledSpec |
	(specArray size > 0 and: [specArray first class ~~ Array]) ifTrue:
		[^ self error: 'unions must have fields defined by sub-Array'].
	byteOffset := 1.
	maxByteSize := 0.
	typeSpec := WriteStream on: (WordArray new: specArray size + 1).
	typeSpec nextPut: FFIFlagStructure.
	"dummy for size"
	specArray do:
		[:spec |
		| fieldName fieldType isPointerField externalType typeSize selfRefering |
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
			maxByteSize := maxByteSize max: typeSize.
		].
	newCompiledSpec := typeSpec contents.
	newCompiledSpec at: 1 put: (maxByteSize bitOr: FFIFlagStructure).
	^ newCompiledSpec
]

{ #category : #converting }
ExternalUnion class >> compositeName [
	^'union'
]
