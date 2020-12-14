Class {
	#name : #TFDerivedType,
	#superclass : #Object,
	#classVars : [
		'BOOLEAN',
		'BOOLEAN32',
		'OOP',
		'STRING'
	],
	#category : #'ThreadedFFI-Types'
}

{ #category : #accessing }
TFDerivedType class >> boolean [
	
	^ BOOLEAN ifNil: [ BOOLEAN := TFBooleanType new ]
]

{ #category : #accessing }
TFDerivedType class >> boolean32 [
	
	^ BOOLEAN32 ifNil: [ BOOLEAN32 := TFBoolean32Type new ]
]

{ #category : #accessing }
TFDerivedType class >> objectPointer [
	
	^ OOP ifNil: [ OOP := TFOOPType new ]
]

{ #category : #accessing }
TFDerivedType class >> string [
	
	^ STRING ifNil: [ STRING := TFStringType new ]
]

{ #category : #accessing }
TFDerivedType >> basicType [
	
	^ TFBasicType pointer
]

{ #category : #accessing }
TFDerivedType >> byteSize [
	
	^ self basicType byteSize
]

{ #category : #'reading-writing' }
TFDerivedType >> callbackReadValue: anExternalAddress [
	
	^ self readValue: anExternalAddress 
]

{ #category : #'reading-writing' }
TFDerivedType >> callbackWrite: anObject into: anExternalAddress [
	
	self write: anObject into: anExternalAddress
]

{ #category : #marshalling }
TFDerivedType >> emitFreeIfNeededOfIndex: argIndex argumentsArrayTempName: argumentsArrayTempName withBuilder: anIRBuilder [

]

{ #category : #freeing }
TFDerivedType >> freeValueIfNeeded: aCHeapValueHolder [ 
	
	"Hook into me if you want to free some value sent previously to a C function"
]

{ #category : #accessing }
TFDerivedType >> getHandle [
	
	^ self basicType getHandle
]

{ #category : #testing }
TFDerivedType >> isVoid [
	
	^ self basicType isVoid
]

{ #category : #marshalling }
TFDerivedType >> marshallFromPrimitive: aValue [
	
	^ aValue
	
]

{ #category : #marshalling }
TFDerivedType >> marshallToPrimitive: aValue [
	
	^ aValue
	
]

{ #category : #'reading-writing' }
TFDerivedType >> readReturnValue: anExternalAddress [

	^ self readValue: anExternalAddress
]

{ #category : #'reading-writing' }
TFDerivedType >> readValue: anExternalAddress [
	
	self subclassResponsibility
]

{ #category : #validating }
TFDerivedType >> validate [
	
	self basicType validate
]

{ #category : #'reading-writing' }
TFDerivedType >> write: anObject into: anExternalAddress [
	
	self basicType write: anObject into: anExternalAddress
]
