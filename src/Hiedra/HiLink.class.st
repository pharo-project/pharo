"
I am a link used to render a Hiedra ruler.
"
Class {
	#name : #HiLink,
	#superclass : #HiRenderingElement,
	#instVars : [
		'linkBuilder',
		'origin',
		'target',
		'checkpoints'
	],
	#category : #'Hiedra-Rendering'
}

{ #category : #'instance creation' }
HiLink class >> linkBuilder: aLinkBuilder origin: aNode target: anotherNode [

	^ self basicNew
		initializeLinkBuilder: aLinkBuilder origin: aNode target: anotherNode;
		yourself
]

{ #category : #comparing }
HiLink >> = other [

	^ self class = other class and: [
		origin = other origin and: [ 
		target = other target and: [ 
		linkBuilder = other linkBuilder ] ] ]
]

{ #category : #accessing }
HiLink >> checkpoints [
	^ checkpoints
]

{ #category : #accessing }
HiLink >> checkpoints: anObject [
	checkpoints := anObject
]

{ #category : #comparing }
HiLink >> hash [

	^ origin hash bitXor: (target hash bitXor: linkBuilder hash)
]

{ #category : #controlling }
HiLink >> ifNode: nodeBlock ifLink: linkBlock [

	^ linkBlock cull: self
]

{ #category : #initialization }
HiLink >> initializeLinkBuilder: aLinkBuilder origin: aNode target: anotherNode [

	self initialize.
	
	linkBuilder := aLinkBuilder.
	origin := aNode.
	target := anotherNode.
]

{ #category : #accessing }
HiLink >> intermediateIndices [

	| originRowIndex targetRowIndex |
	originRowIndex := origin rowIndex.
	targetRowIndex := target rowIndex.

	^ originRowIndex >= targetRowIndex
		ifTrue: [ originRowIndex - 1 to: targetRowIndex + 1 by: -1 ]
		ifFalse: [ originRowIndex + 1 to: targetRowIndex - 1 ]
]

{ #category : #testing }
HiLink >> isLink [

	^ true
]

{ #category : #accessing }
HiLink >> linkBuilder [
	^ linkBuilder
]

{ #category : #accessing }
HiLink >> origin [
	^ origin
]

{ #category : #printing }
HiLink >> printOn: aStream [

	super printOn: aStream.
	
	aStream
		nextPut: $(;
		print: origin;
		nextPutAll: '->';
		print: target;
		nextPut: $).
]

{ #category : #accessing }
HiLink >> target [
	^ target
]
