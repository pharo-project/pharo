"
I know how to handle the marshalling and unmarshalling of structs.
"
Class {
	#name : #TFStructType,
	#superclass : #TFAbstractType,
	#instVars : [
		'members',
		'offsets',
		'targetClass',
		'byteSize'
	],
	#category : #'ThreadedFFI-Types'
}

{ #category : #'instance creation' }
TFStructType class >> finalizeResourceData: handle [

	handle isNull ifTrue: [ ^ self ].

	(self fromHandle: handle)
		free.
		
	handle beNull.
]

{ #category : #'instance creation' }
TFStructType class >> forClass: aClass withMembers: aCollectionOfMembers [
	
	^ self new
		members: aCollectionOfMembers asArray;
		targetClass: aClass;
		autoRelease;
		yourself
]

{ #category : #types }
TFStructType >> basicType [

	^ self
]

{ #category : #size }
TFStructType >> byteSize [ 
	
	self validate.
	^ byteSize ifNil: [ self primitiveByteSize ]
]

{ #category : #reading }
TFStructType >> callbackReadValue: anExternalAddress [

	^ targetClass fromHandle: anExternalAddress.
]

{ #category : #writing }
TFStructType >> callbackWrite: aNumber into: anExternalAddress [

	self write: aNumber getHandle into: anExternalAddress
]

{ #category : #utilities }
TFStructType >> copyFrom: from to: to size: size [ 
	"Copy from the from an externalAdress or a ByteArray or to a ByteArray or an External Address"

	<primitive: 'primitiveCopyFromTo'>
	self primitiveFail 
]

{ #category : #marshalling }
TFStructType >> emitFreeIfNeededOfIndex: anInteger argumentsArrayTempName: aString withBuilder: anIRBuilder [ 

]

{ #category : #marshalling }
TFStructType >> emitMarshallFromPrimitive: anIRBuilder [ 
	
	anIRBuilder addTemp: #__structHandler.
	anIRBuilder storeTemp: #__structHandler.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: targetClass.
	anIRBuilder pushTemp: #__structHandler.
	anIRBuilder send: #fromHandle:
	
]

{ #category : #marshalling }
TFStructType >> emitMarshallToPrimitive: anIRBuilder [ 

	anIRBuilder send: #getHandle
]

{ #category : #primitives }
TFStructType >> free [
	<primitive: 'primitiveFreeStruct'>

	^ self primitiveFailed	
]

{ #category : #utilities }
TFStructType >> freeValueIfNeeded: anExternalAddress [ 
	
	"Nothing to free"
]

{ #category : #testing }
TFStructType >> isVoid [
	^ false
]

{ #category : #marshalling }
TFStructType >> marshallFromPrimitive: aValue [
	
	(aValue isKindOf: targetClass) 
		ifTrue: [ ^ aValue ].

	^ targetClass fromHandle: aValue
]

{ #category : #marshalling }
TFStructType >> marshallToPrimitive: aValue [ 

	^ aValue getHandle
]

{ #category : #accessing }
TFStructType >> members [
	^ members
]

{ #category : #accessing }
TFStructType >> members: anObject [
	members := anObject
]

{ #category : #accessing }
TFStructType >> offsets [
	self validate.
	
	^ offsets
]

{ #category : #primitives }
TFStructType >> primitiveByteSize [
	<primitive: 'primitiveStructByteSize'>

	^ self primitiveFailed	
]

{ #category : #primitives }
TFStructType >> primitiveInitializeStructType [
	<primitive: 'primitiveInitializeStructType'>

	^ self primitiveFailed	
]

{ #category : #reading }
TFStructType >> readReturnValue: anExternalAddress [
	
	| newHandle |
	newHandle := ByteArray new: self byteSize.
	self copyFrom: anExternalAddress to: newHandle size: self byteSize.
	^ newHandle
]

{ #category : #reading }
TFStructType >> readValue: anExternalAddress offset: offset [ 

	| address |

	address := anExternalAddress pointerAt: offset.
	^ targetClass fromHandle: address.
]

{ #category : #accessing }
TFStructType >> targetClass [
	^ targetClass
]

{ #category : #accessing }
TFStructType >> targetClass: anObject [
	targetClass := anObject
]

{ #category : #validating }
TFStructType >> validate [
	self isValid
		ifTrue: [ ^ self ].

	members do: [ :m | m validate ].
	offsets := Array new: members size.
	
	self primitiveInitializeStructType.
	
	byteSize := self primitiveByteSize	
]

{ #category : #writing }
TFStructType >> write: anExternalStructure into: anExternalAddress [

	self copyFrom: anExternalStructure to: anExternalAddress size: self byteSize
]
