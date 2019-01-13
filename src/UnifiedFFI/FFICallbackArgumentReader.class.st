Class {
	#name : #FFICallbackArgumentReader,
	#superclass : #Object,
	#instVars : [
		'callback',
		'callbackContext',
		'currentStackOffset',
		'extractedArguments'
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

	^ self subclassResponsibility 
]

{ #category : #extracting }
FFICallbackArgumentReader >> basicExtractUnsignedInteger [

	^ self subclassResponsibility 
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
	self subclassResponsibility 
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractFloat [
	self subclassResponsibility 
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

	extractedArguments add: (aType handle: self stackPointer at: currentStackOffset).
	currentStackOffset := currentStackOffset + Smalltalk wordSize
]

{ #category : #extracting }
FFICallbackArgumentReader >> extractStructType: aFFIExternalStructureType [ 
	
	self subclassResponsibility 
]

{ #category : #initialization }
FFICallbackArgumentReader >> initialize [

	currentStackOffset := 1.
	extractedArguments := OrderedCollection new.
]

{ #category : #accessing }
FFICallbackArgumentReader >> stackPointer [
	^ callbackContext stackp
]
