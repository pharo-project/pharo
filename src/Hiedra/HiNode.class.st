"
I am a node that represents a value in a Hiedra ruler. I know links that connect myself with other nodes of the same ruler (incoming and outgoing).


"
Class {
	#name : #HiNode,
	#superclass : #Object,
	#instVars : [
		'value',
		'incomingLinks',
		'outgoingLinks',
		'rulerPoint'
	],
	#category : #'Hiedra-Model'
}

{ #category : #'instance creation' }
HiNode class >> value: aValue rulerPoint: aPoint [
	^ self basicNew
		initializeWithValue: aValue
			rulerPoint: aPoint;
		yourself
]

{ #category : #accessing }
HiNode >> addIncomingLink: aHiLink [ 
	incomingLinks add: aHiLink
]

{ #category : #accessing }
HiNode >> addOutgoingLink: aHiLink [ 
	outgoingLinks add: aHiLink 
]

{ #category : #accessing }
HiNode >> anyLink [
	"Answer a link (assuming it has at least one either outgoing or incoming link)."

	^ (outgoingLinks ifEmpty: [ incomingLinks ]) first
]

{ #category : #testing }
HiNode >> hasAnyLink [
	^ outgoingLinks isEmpty and: [ incomingLinks isEmpty ]
]

{ #category : #testing }
HiNode >> hasLinkTo: aHiNode [
	^ outgoingLinks anySatisfy: [ :each | each target = aHiNode ]
]

{ #category : #accessing }
HiNode >> incomingLinks [
	^ incomingLinks
]

{ #category : #initialization }
HiNode >> initializeWithValue: aValue rulerPoint: aPoint [

	self initialize.
	rulerPoint := aPoint.
	value := aValue.
	outgoingLinks := LinkedList new.
	incomingLinks := LinkedList new.
]

{ #category : #accessing }
HiNode >> intermediateRowIndicesTo: anotherNode [

	| originRowIndex targetRowIndex |
	originRowIndex := self rulerPoint y.
	targetRowIndex := anotherNode rulerPoint y.

	^ originRowIndex <= targetRowIndex
		ifTrue: [ originRowIndex + 1 to: targetRowIndex - 1 ]
		ifFalse: [ originRowIndex - 1 to: targetRowIndex + 1 by: -1 ]
]

{ #category : #accessing }
HiNode >> outgoingLinks [
	^ outgoingLinks
]

{ #category : #printing }
HiNode >> printOn: aStream [

	super printOn: aStream.
	
	aStream
		nextPut: $(;
		print: value;
		space;
		print: self rulerPoint;
		nextPut: $).
]

{ #category : #accessing }
HiNode >> rulerPoint [
	^ rulerPoint
]

{ #category : #accessing }
HiNode >> theValue [
	^ value
]
