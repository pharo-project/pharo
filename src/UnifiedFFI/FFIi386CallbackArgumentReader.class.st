Class {
	#name : #FFIi386CallbackArgumentReader,
	#superclass : #FFICallbackArgumentReader,
	#instVars : [
		'returnValueHolder'
	],
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #extracting }
FFIi386CallbackArgumentReader >> basicExtractInteger [
	
	^ [ FFIInt32 new handle: self stackPointer at: currentStackOffset ]
		ensure: [ currentStackOffset := currentStackOffset + 4 ]
]

{ #category : #extracting }
FFIi386CallbackArgumentReader >> basicExtractUnsignedInteger [
	
	^ [ FFIUInt32 new handle: self stackPointer at: currentStackOffset ]
		ensure: [ currentStackOffset := currentStackOffset + 4 ]
]

{ #category : #extracting }
FFIi386CallbackArgumentReader >> extractDouble [

	extractedArguments add: (FFIFloat64 new handle: self stackPointer at: currentStackOffset).
	currentStackOffset := currentStackOffset + 8.
]

{ #category : #extracting }
FFIi386CallbackArgumentReader >> extractExternalString: aFFIExternalString [ 
	
	extractedArguments add: (aFFIExternalString basicHandle: self stackPointer at: currentStackOffset).
	currentStackOffset := currentStackOffset + Smalltalk wordSize. 
]

{ #category : #extracting }
FFIi386CallbackArgumentReader >> extractFloat [

	extractedArguments add: (FFIFloat32 new handle: self stackPointer at: currentStackOffset).
	currentStackOffset := currentStackOffset + 4.
]

{ #category : #extracting }
FFIi386CallbackArgumentReader >> extractStructType: aFFIExternalStructureType [ 

	extractedArguments add: (aFFIExternalStructureType handle: self stackPointer at: currentStackOffset).
	currentStackOffset := currentStackOffset + aFFIExternalStructureType objectClass byteSize.
]

{ #category : #extracting }
FFIi386CallbackArgumentReader >> initialize [

	| returnType |
	super initialize.

	"If the function returns an struct by copy there is a hidden parameter with a pointer storing it".
	returnType := callback functionSpec returnType.
	(returnType isExternalStructure and: [ returnType isPointer not ]) ifTrue: [ 
		returnValueHolder := self stackPointer pointerAt: 1.
		currentStackOffset := currentStackOffset + Smalltalk wordSize.
	]
]
