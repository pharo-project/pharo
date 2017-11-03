"
A mock for a global send
"
Class {
	#name : #FLGlobalSendMock,
	#superclass : #Object,
	#instVars : [
		'contents'
	],
	#category : #FuelTests-Mocks
}

{ #category : #'instance creation' }
FLGlobalSendMock class >> newInstanceToMaterialize [
	
	^self newWith: #materializing
]

{ #category : #'instance creation' }
FLGlobalSendMock class >> newInstanceToSerialize [
	
	^self newWith: #serializing
]

{ #category : #'instance creation' }
FLGlobalSendMock class >> newWith: aByteSymbol [ 
	
	^self basicNew
		initializeWith: aByteSymbol;
		yourself
]

{ #category : #comparing }
FLGlobalSendMock >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ contents = anObject contents
]

{ #category : #accessing }
FLGlobalSendMock >> contents [
	^ contents
]

{ #category : #accessing }
FLGlobalSendMock >> contents: anObject [
	contents := anObject
]

{ #category : #serialization }
FLGlobalSendMock >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitGlobalSend: self name: #FLGlobalSendMock selector: #newInstanceToMaterialize
]

{ #category : #comparing }
FLGlobalSendMock >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ contents hash
]

{ #category : #initialization }
FLGlobalSendMock >> initializeWith: aByteSymbol [
	
	super initialize.
	contents := aByteSymbol 
]

{ #category : #printing }
FLGlobalSendMock >> printOn: aStream [
	"Append a sequence of characters to aStream that identify the receiver."

	super printOn: aStream.
	aStream
		nextPutAll: ' contents: ';
		print: contents
]
