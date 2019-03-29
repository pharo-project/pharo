"
Instances of ExternalData explicitly describe objects with associated type. They can be used for describing atomic C types like arrays of atomic types (e.g., 'int[]') or pointer to atomic types (e.g., 'int *').

Instance variables:
	type	<Integer | Behavior>	The basic type of the receiver.

The encoding of type is equivalent to that of the basic type in class ExternalType. The interpretation of whether the receiver describes an array of data or a pointer to data depends on the contents of the instance variable 'handle'. If handle contains an ExternalAddress the receiver is treated as pointer to type. If the handle contains a ByteArray the receiver is interpreted as describing an array of type. Note that both interpretations are treated equivalent in external calls, e.g., if one describes an argument to an external call as taking 'int*' then, depending on the type of handle either the actual contents (if ExternalAddress) or a pointer to the contents (if ByteArray) is passed.


"
Class {
	#name : #ExternalData,
	#superclass : #ExternalStructure,
	#instVars : [
		'type'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'class initialization' }
ExternalData class >> compileFields [
	"Ensure proper initialization of ExternalType when first loading"
	ExternalType initialize.
	^super compileFields
]

{ #category : #'field definition' }
ExternalData class >> fields [
	"ExternalData defineFields"
	"Note: The definition is for completeness only.
	ExternalData is treated specially by the VM."
	^#(nil 'void*')
]

{ #category : #'instance creation' }
ExternalData class >> fromHandle: aHandle type: aType [
	"Create a pointer to the given type"
	"ExternalData fromHandle: ExternalAddress new type: ExternalType float"
	^self basicNew setHandle: aHandle type: aType
]

{ #category : #'instance creation' }
ExternalData class >> new [
	"You better not..."
	^self shouldNotImplement
]

{ #category : #conversion }
ExternalData >> fromCString [
	"Assume that the receiver represents a C string and convert it to a Smalltalk string. hg 2/25/2000 14:18"

	| stream index char |
	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	stream := WriteStream on: String new.
	index := 1.
	[(char := handle unsignedCharAt: index) = 0 asCharacter] whileFalse: [
		stream nextPut: char.
		index := index + 1].
	^stream contents
]

{ #category : #conversion }
ExternalData >> fromCStrings [
	"Assume that the receiver represents a set of C strings and is teerminated by a empty string and convert it to a Smalltalk ordered collection of strings"

	| stream index char strings str |
	type isPointerType ifFalse: [self error: 'External object is not a pointer type.'].
	strings := OrderedCollection new.
	index := 1.
	[
		stream := WriteStream on: String new.
		[(char := handle unsignedCharAt: index) = 0 asCharacter]
			whileFalse: [
				stream nextPut: char.
				index := index + 1
			].
		str := stream contents.
		strings addLast: str.
		str size = 0
	] whileFalse.
	^strings
]

{ #category : #printing }
ExternalData >> printOn: stream [

	stream 
		nextPut: $(;
		print: type; 
		nextPut: $). 
	self getHandle printOn: stream.
]

{ #category : #private }
ExternalData >> setHandle: aHandle type: aType [
	handle := aHandle.
	type := aType.
]
