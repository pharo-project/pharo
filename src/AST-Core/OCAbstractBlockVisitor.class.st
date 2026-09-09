"
Abstract visitor providing generic block visiting capabilities.
Subclasses provide specific node type capabilties

"
Class {
	#name : 'OCAbstractBlockVisitor',
	#superclass : 'OCProgramNodeVisitor',
	#instVars : [
		'visitBlock'
	],
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'testing' }
OCAbstractBlockVisitor class >> isAbstract [

	^ self == OCAbstractBlockVisitor
]

{ #category : 'enumerating' }
OCAbstractBlockVisitor class >> visit: aTree anySatisfy: aBlock [
	self
		visit: aTree
		do: [ :node | (aBlock value: node) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'enumerating' }
OCAbstractBlockVisitor class >> visit: aTree detect: aBlock [

	^self visit: aTree detect: aBlock ifNone: [ NotFound signalFor: aBlock ]
]

{ #category : 'enumerating' }
OCAbstractBlockVisitor class >> visit: aTree detect: aBlock ifNone: anErrorBlock [
	self
		visit: aTree
		do: [ :node | (aBlock value: node) ifTrue: [ ^ node ] ].
	^ anErrorBlock value
]

{ #category : 'enumerating' }
OCAbstractBlockVisitor class >> visit: aTree do: aBlock [
	^self new
		visitBlock: aBlock;
		visitNode: aTree
]

{ #category : 'enumerating' }
OCAbstractBlockVisitor class >> visit: aTree noneSatisfy: aBlock [
	self
		visit: aTree
		do: [ :node | (aBlock value: node) ifTrue: [ ^ false ] ].
	^ true
]

{ #category : 'enumerating' }
OCAbstractBlockVisitor class >> visit: aTree select: aBlock [
	| result |
	result := OrderedCollection new.
	self visit: aTree do: [ :node |
		(aBlock value: node) ifTrue: [ result add: node ] ].
	^result
]

{ #category : 'accessing' }
OCAbstractBlockVisitor >> visitBlock [
	^ visitBlock
]

{ #category : 'accessing' }
OCAbstractBlockVisitor >> visitBlock: anObject [
	visitBlock := anObject
]
