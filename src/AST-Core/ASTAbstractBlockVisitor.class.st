"
Abstract visitor providing generic block visiting capabilities.
Subclasses provide specific node type capabilties

"
Class {
	#name : 'ASTAbstractBlockVisitor',
	#superclass : 'ASTProgramNodeVisitor',
	#instVars : [
		'visitBlock'
	],
	#category : 'AST-Core-Visitors',
	#package : 'AST-Core',
	#tag : 'Visitors'
}

{ #category : 'testing' }
ASTAbstractBlockVisitor class >> isAbstract [

	^ self == ASTAbstractBlockVisitor
]

{ #category : 'enumerating' }
ASTAbstractBlockVisitor class >> visit: aTree anySatisfy: aBlock [
	self
		visit: aTree
		do: [ :node | (aBlock value: node) ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'enumerating' }
ASTAbstractBlockVisitor class >> visit: aTree detect: aBlock [

	^self visit: aTree detect: aBlock ifNone: [ NotFound signalFor: aBlock ]
]

{ #category : 'enumerating' }
ASTAbstractBlockVisitor class >> visit: aTree detect: aBlock ifNone: anErrorBlock [
	self
		visit: aTree
		do: [ :node | (aBlock value: node) ifTrue: [ ^ node ] ].
	^ anErrorBlock value
]

{ #category : 'enumerating' }
ASTAbstractBlockVisitor class >> visit: aTree do: aBlock [
	^self new
		visitBlock: aBlock;
		visitNode: aTree
]

{ #category : 'enumerating' }
ASTAbstractBlockVisitor class >> visit: aTree noneSatisfy: aBlock [
	self
		visit: aTree
		do: [ :node | (aBlock value: node) ifTrue: [ ^ false ] ].
	^ true
]

{ #category : 'enumerating' }
ASTAbstractBlockVisitor class >> visit: aTree select: aBlock [
	| result |
	result := OrderedCollection new.
	self visit: aTree do: [ :node |
		(aBlock value: node) ifTrue: [ result add: node ] ].
	^result
]

{ #category : 'accessing' }
ASTAbstractBlockVisitor >> visitBlock [
	^ visitBlock
]

{ #category : 'accessing' }
ASTAbstractBlockVisitor >> visitBlock: anObject [
	visitBlock := anObject
]
