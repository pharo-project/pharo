Class {
	#name : #FFICallbackArgumentReader,
	#superclass : #Object,
	#instVars : [
		'callback',
		'callbackContext',
		'currentStackOffset',
		'extractedArguments',
		'returnValueHolder'
	],
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #'as yet unclassified' }
FFICallbackArgumentReader class >> forCallback: aCallback inContext: aCallbackContext [

	^ self basicNew
		callbackContext: aCallbackContext;
		callback: aCallback;
		initialize;
		yourself.
]

{ #category : #accessing }
FFICallbackArgumentReader >> arguments [
	
	^ extractedArguments
]

{ #category : #extracting }
FFICallbackArgumentReader >> basicExtractInteger [
	
	| baseAddressToRead offsetOfBaseAddress type pair |
	
	type := self stackIntegerType.
	pair := self nextBaseAddressFor: type.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.
	
	^ type handle: baseAddressToRead at: offsetOfBaseAddress.
]

{ #category : #extracting }
FFICallbackArgumentReader >> basicExtractUnsignedInteger [
	
	| baseAddressToRead offsetOfBaseAddress type pair |
	
	type := self stackUnsignedIntegerType.
	pair := self nextBaseAddressFor: type.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.
	
	^ type handle: baseAddressToRead at: offsetOfBaseAddress.
]

{ #category : #accessing }
FFICallbackArgumentReader >> callback [
	^ callback
]

{ #category : #accessing }
FFICallbackArgumentReader >> callback: anObject [
	callback := anObject
]

{ #category : #accessing }
FFICallbackArgumentReader >> callbackContext [
	^ callbackContext
]

{ #category : #accessing }
FFICallbackArgumentReader >> callbackContext: anObject [
	callbackContext := anObject.

]

{ #category : #extracting }
FFICallbackArgumentReader >> extractCharacterType [

	extractedArguments add: self basicExtractUnsignedInteger asCharacter
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractDouble [

	| type pair baseAddressToRead offsetOfBaseAddress |
	type := FFIFloat64 new.
	pair := self nextBaseAddressFor: type.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.

	extractedArguments add: (type handle: baseAddressToRead at: offsetOfBaseAddress).

]

{ #category : #extracting }
FFICallbackArgumentReader >> extractExternalString: type [ 
	
	| pair baseAddressToRead offsetOfBaseAddress |
	pair := self nextBaseAddressFor: type.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.

	extractedArguments add: (type handle: baseAddressToRead at: offsetOfBaseAddress).
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractFloat [

	| type pair baseAddressToRead offsetOfBaseAddress |
	type := FFIFloat32 new.
	pair := self nextBaseAddressFor: type.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.

	extractedArguments add: (type handle: baseAddressToRead at: offsetOfBaseAddress).
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractIntegerType [

	extractedArguments add: self basicExtractInteger
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractNext: aFFIType [

	aFFIType isPointer
		ifTrue: [ aFFIType extractPointerFrom: self ]
		ifFalse: [ aFFIType extractFromCallbackOn: self ]
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractPointerOfType: aType [

	| pair baseAddressToRead offsetOfBaseAddress |
	pair := self nextBaseAddressFor: aType.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.

	extractedArguments add: (aType handle: baseAddressToRead at: offsetOfBaseAddress).

]

{ #category : #extracting }
FFICallbackArgumentReader >> extractStructType: type [ 
	
	| pair baseAddressToRead offsetOfBaseAddress |
	pair := self nextBaseAddressForStructure: type.
	baseAddressToRead := pair first.
	offsetOfBaseAddress := pair second.

	extractedArguments add: (type handle: baseAddressToRead at: offsetOfBaseAddress).
]

{ #category : #initialization }
FFICallbackArgumentReader >> initialize [
	
	| returnType |

	super initialize.

	currentStackOffset := 1.
	extractedArguments := OrderedCollection new.
	
	"If the function returns an struct by copy there is a hidden parameter with a pointer storing it".
	returnType := callback functionSpec returnType.
	(returnType isExternalStructure and: [ returnType isPointer not ]) ifTrue: [ 
		returnValueHolder := self stackPointer pointerAt: 1.
		currentStackOffset := currentStackOffset + Smalltalk wordSize.
	]
]

{ #category : #'calculating-offsets' }
FFICallbackArgumentReader >> nextBaseAddressFor: type [

	| baseAddressToRead offsetOfBaseAddress |

	baseAddressToRead := self stackPointer.
	offsetOfBaseAddress := currentStackOffset.
	currentStackOffset := currentStackOffset + type externalTypeSize.

	^ {baseAddressToRead. offsetOfBaseAddress}
]

{ #category : #'calculating-offsets' }
FFICallbackArgumentReader >> nextBaseAddressForStructure: type [

	^ self nextBaseAddressFor: type
]

{ #category : #types }
FFICallbackArgumentReader >> stackIntegerType [
	
	^ self subclassResponsibility
]

{ #category : #accessing }
FFICallbackArgumentReader >> stackPointer [
	^ callbackContext stackp
]

{ #category : #types }
FFICallbackArgumentReader >> stackUnsignedIntegerType [
	
	^ self subclassResponsibility
]
