"
I am an abstract class that is responsible for extracting the arguments of a callback.
The callbacks provides the arguments in different forms depending of the architecture.
Each of my subclasses implements special behavior for the given architectures.
I have the common behavior for all the architectures.

Basically, I am a stateful object that is used during the process of extraction.
I am created with a callback and a callback context.
A new instance of myself is created in each callback invocation, and it should not be reused.

From the callback I extract the type of return of the callback and the type of the parameters.

I am used in the method FFICallback >> #argumentsFor: stackPointer context: callbackContext.
Each of the architectures is able to create the correct subclass of me.

My users just use the #extractArguments method and then access teh arguments through #arguments.

In the extractArguments, I take the arguments from the FFICallback and send my message extractNext: with the type of the arguments.

The #extractNext: method checks if the type represents a pointer or a value type. 
If it is a pointer the message #extractPointerOfType: is used. If it is a value,
there is a double dispatch with the type. The message #extractFromCallbackOn: is sent to the type.

Then the type use one of the following methods to indicate its type: 

#extractCharacterType
#extractDouble
#extractFloat
#extractIntegerType 
#extractExternalString: 
#extractStructType:

Each of this methods calculates the base address and the correct offset to extract the value or the pointer. For this calculation the messages #nextBaseAddressFor: aType and #nextBaseAddressForStructure: aStructType are used.

My base implementation extracts all the parameters from the stack. So the base address is always the stack pointer and the offset is updated according to the type.

Once the base address is opteined, the type objects is used with the message #handle:at: to get the value from the stack.

These methods all modify the collection of extracted elements.

Note: 

When a callback should return a struct by copy, the caller function allocates the returning struct. 
The pointer to this struct is stored in a new first parameter to the callback. This hidden parameter is extracted in the #initialize if it is required.
The callback can access to the pointer through the returnValueHolder instance variable.
"
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
	#category : #'UnifiedFFI-Callbacks'
}

{ #category : #'instance creation' }
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
FFICallbackArgumentReader >> basicExtractPointer [
	
	^ ExternalAddress fromAddress: (self basicExtractUnsignedInteger)
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
FFICallbackArgumentReader >> extractArguments [

	callback functionSpec arguments do: [ :type | self extractNext: type resolvedType ].

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
		ifTrue: [ self extractPointerOfType: aFFIType ]
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
	returnType := callback functionSpec returnType resolvedType.
	(returnType isExternalStructure and: [ returnType isPointer not ]) ifTrue: [ 
		returnValueHolder := self stackPointer pointerAt: 1.
		currentStackOffset := currentStackOffset + Smalltalk wordSize.
	]
]

{ #category : #'calculating-offsets' }
FFICallbackArgumentReader >> nextBaseAddressFor: type [

	"I calculate the base address to read an argument from a callback context.
	In my implementation all the arguments are read from the stack.
	The space of each type depends of its own size."

	| baseAddressToRead offsetOfBaseAddress typeSize |

	typeSize := type isPointer ifTrue: [ Smalltalk wordSize ] ifFalse: [ type externalTypeSize ].

	baseAddressToRead := self stackPointer.
	offsetOfBaseAddress := currentStackOffset.
	currentStackOffset := currentStackOffset + typeSize.

	^ {baseAddressToRead. offsetOfBaseAddress}
]

{ #category : #'calculating-offsets' }
FFICallbackArgumentReader >> nextBaseAddressForStructure: type [

	"I calculate the base address to read an struct argument.
	The handling of structs is different in the subclases, but in myself the handle of 
	structs is performed in the stack, so the calculation of addresses is perform in the same
	way as non-struct arguments."


	^ self nextBaseAddressFor: type
]

{ #category : #types }
FFICallbackArgumentReader >> stackIntegerType [
	
	"Depending of the architecture there are different types used in the stack for the integers."
	
	^ self subclassResponsibility
]

{ #category : #accessing }
FFICallbackArgumentReader >> stackPointer [
	^ callbackContext stackp
]

{ #category : #types }
FFICallbackArgumentReader >> stackUnsignedIntegerType [
	
	"Depending of the architecture there are different types used in the stack for the unsigned integers."

	^ self subclassResponsibility
]
