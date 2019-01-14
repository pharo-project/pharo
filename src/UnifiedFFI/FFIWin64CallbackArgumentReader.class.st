Class {
	#name : #FFIWin64CallbackArgumentReader,
	#superclass : #FFIAbstract64BitsArgumentReader,
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> currentParameterIndex [
	
	^ extractedArguments size + 1
]

{ #category : #extracting }
FFIWin64CallbackArgumentReader >> extractStructType: type [ 
	
	| pointer |
	
	pointer := self basicExtractPointer.
	extractedArguments add: (type handle: pointer at: 1).
]

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> floatRegisterPointer [
	
	^ callbackContext floatregargsp
]

{ #category : #accessing }
FFIWin64CallbackArgumentReader >> integerRegisterPointer [
	
	^ callbackContext intregargsp
]

{ #category : #'calculating-offsets' }
FFIWin64CallbackArgumentReader >> nextBaseAddressFor: type [
	
	| baseAddressToRead offsetOfBaseAddress |
	
	^ self currentParameterIndex <= self numberOfRegisters 
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
