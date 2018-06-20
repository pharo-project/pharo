"
Abstract visitor providing generic block visiting capabilities.
Subclasses provide specific node type capabilties

"
Class {
	#name : #RBAbstractBlockVisitor,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'visitBlock'
	],
	#category : #'AST-Core-Visitors'
}

{ #category : #enumerating }
RBAbstractBlockVisitor class >> visit: aTree detect: aBlock [
	
	^self visit: aTree detect: aBlock ifNone: [ NotFound signalFor: aBlock ]
]

{ #category : #enumerating }
RBAbstractBlockVisitor class >> visit: aTree detect: aBlock ifNone: anErrorBlock [
	| result |
	
	result := OrderedCollection new.
	
	self visit: aTree do: [ :node |
		(aBlock value: node) ifTrue: [ ^node ] ].
	
	^anErrorBlock value
]

{ #category : #enumerating }
RBAbstractBlockVisitor class >> visit: aTree do: aBlock [
	^self new 
		visitBlock: aBlock;
		visitNode: aTree
]

{ #category : #enumerating }
RBAbstractBlockVisitor class >> visit: aTree select: aBlock [
	| result |
	result := OrderedCollection new.
	self visit: aTree do: [ :node |
		(aBlock value: node) ifTrue: [ result add: node ] ].
	^result
]

{ #category : #accessing }
RBAbstractBlockVisitor >> visitBlock [
	^ visitBlock
]

{ #category : #accessing }
RBAbstractBlockVisitor >> visitBlock: anObject [
	visitBlock := anObject
]
