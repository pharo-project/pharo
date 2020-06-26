"
I am the responsible to parse the callback arguments in Win64.
In Win64 the structures are always passed by reference. 
The first 4 parameters are passed in the registry. 
If the parameter is an integer, it is in the integer registers (general purpouse registers).
If the parameter is a float, it is in the float registers (SSE registers).

The first parameter is always in the first register of the set, the second always in the second register, etc. Following the type of arguments.

Ex: 

int f( int a, float b, char c)

The parameter a is in the first integer register, the parameter b is in the second float register, and the parameter c is in the third integer register.

The fifth and following parameters are passed in the stack.
If any of the four first parameters does not fit in the registers are passed in the stack.
"
Class {
	#name : #FFIWin64CallbackArgumentReader,
	#superclass : #FFIAbstract64BitsArgumentReader,
	#instVars : [
		'currentParameterIndex'
	],
	#category : #'UnifiedFFI-Callbacks'
}

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> currentParameterIndex [
	
	^ currentParameterIndex
]

{ #category : #initialization }
FFIWin64CallbackArgumentReader >> extractNext: aType [

	super extractNext: aType.
	currentParameterIndex := currentParameterIndex + 1.
]

{ #category : #extracting }
FFIWin64CallbackArgumentReader >> extractStructType: type [ 
	
	| pointer |
	
	"The struct are always preallocated by the caller and they pass as a pointer."
	
	pointer := self basicExtractPointer.
	self addArgument: (type handle: pointer at: 1).
]

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> floatRegisterPointer [
	
	^ callbackContext floatregargsp
]

{ #category : #initialization }
FFIWin64CallbackArgumentReader >> initialize [

	super initialize.
	currentParameterIndex := 1.
]

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> integerRegisterPointer [
	
	^ callbackContext intregargsp
]

{ #category : #'calculating-offsets' }
FFIWin64CallbackArgumentReader >> nextBaseAddressFor: type [
	
	| baseAddressToRead offsetOfBaseAddress |
	
	^ (self currentParameterIndex <= self numberOfRegisters and: [ type typeSize <= 8])
		ifTrue:[ 
				"If the parameter is a floating point parameter it comes in the floating point set of registers, not in the general purpose ones."
				baseAddressToRead := type isFloatType ifTrue: [self floatRegisterPointer] ifFalse: [self integerRegisterPointer].
				offsetOfBaseAddress := (self currentParameterIndex - 1) * Smalltalk wordSize + 1.
				currentStackOffset := currentStackOffset + Smalltalk wordSize.
				{ baseAddressToRead. offsetOfBaseAddress }] 
		ifFalse:[ super nextBaseAddressFor: type ].


]

{ #category : #'calculating-offsets' }
FFIWin64CallbackArgumentReader >> nextBaseAddressForStructure: type [

	self error: 'The structures all always pointers'
]

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> numberOfRegisters [
	
	"Win64 only uses 4 registers (integer or float), but never more"
	^ 4
]

{ #category : #types }
FFIWin64CallbackArgumentReader >> stackIntegerType [
	
	^ FFIInt64 new
]

{ #category : #types }
FFIWin64CallbackArgumentReader >> stackUnsignedIntegerType [
	
	^ FFIUInt64 new
]
