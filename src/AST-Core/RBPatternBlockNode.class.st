"
RBPatternBlockNode is the node in matching parse trees (it never occurs in normal Smalltalk code) that executes a block to determine if a match occurs. valueBlock takes two arguments, the first is the actual node that we are trying to match against, and the second node is the dictionary that contains all the metavariable bindings that the matcher has made thus far.

Instance Variables:
	valueBlock	<BlockClosure>	The block to execute when attempting to match this to a node.


"
Class {
	#name : #RBPatternBlockNode,
	#superclass : #RBBlockNode,
	#instVars : [
		'valueBlock'
	],
	#category : #'AST-Core-Pattern'
}

{ #category : #visiting }
RBPatternBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^aProgramNodeVisitor visitPatternBlockNode: self
]

{ #category : #matching }
RBPatternBlockNode >> addArgumentWithNameBasedOn: aString to: aRBBlockNode [
	| name index vars |
	name := aString.
	vars := aRBBlockNode allDefinedVariables.
	index := 0.
	[vars includes: name] whileTrue:
			[index := index + 1.
			name := name , index printString].
	aRBBlockNode
		arguments: (aRBBlockNode arguments copyWith: (RBVariableNode named: name))
]

{ #category : #matching }
RBPatternBlockNode >> constructLookupNodeFor: aString in: aRBBlockNode [
	^ RBMessageNode
		  receiver: RBVariableNode selfNode
		  selector: #lookupMatchFor:in:
		  arguments: (Array with: (RBLiteralNode value: aString) with: aRBBlockNode arguments last copy)
]

{ #category : #matching }
RBPatternBlockNode >> copyInContext: aDictionary [
	^ self replacingBlock value: aDictionary
]

{ #category : #matching }
RBPatternBlockNode >> createMatchingBlock [
	| newBlock |
	self arguments size > 2
		ifTrue:
			[self
				error: 'Search blocks can only contain arguments for the node and matching dictionary'].
	newBlock := RBBlockNode arguments: arguments body: body.
	newBlock arguments isEmpty
		ifTrue: [self addArgumentWithNameBasedOn: 'aNode' to: newBlock].
	newBlock arguments size = 1
		ifTrue: [self addArgumentWithNameBasedOn: 'aDictionary' to: newBlock].
	self replacePatternNodesIn: newBlock.
	^newBlock evaluateForReceiver: self
]

{ #category : #matching }
RBPatternBlockNode >> createReplacingBlock [
	| newBlock |
	self arguments size > 1
		ifTrue:
			[self
				error: 'Replace blocks can only contain an argument for the matching dictionary'].
	newBlock := RBBlockNode arguments: arguments body: body.
	self arguments isEmpty
		ifTrue: [self addArgumentWithNameBasedOn: 'aDictionary' to: newBlock].
	self replacePatternNodesIn: newBlock.
	^newBlock evaluateForReceiver: self
]

{ #category : #'testing - matching' }
RBPatternBlockNode >> isPatternNode [
	^ true
]

{ #category : #matching }
RBPatternBlockNode >> lookupMatchFor: aString in: aDictionary [
	^aDictionary at: aString
		ifAbsent:
			[| variableNode |
			variableNode := RBPatternVariableNode named: aString.
			aDictionary at: variableNode ifAbsent: [nil]]
]

{ #category : #matching }
RBPatternBlockNode >> match: aNode inContext: aDictionary [
	^self matchingBlock value: aNode value: aDictionary
]

{ #category : #matching }
RBPatternBlockNode >> matchingBlock [
	^ valueBlock ifNil: [valueBlock := self createMatchingBlock]
]

{ #category : #matching }
RBPatternBlockNode >> replacePatternNodesIn: aRBBlockNode [
	aRBBlockNode body nodesDo:
			[:each |
			(each isVariable and: [each isPatternNode])
				ifTrue:
					[each
						replaceWith: (self constructLookupNodeFor: each name in: aRBBlockNode)]]
]

{ #category : #matching }
RBPatternBlockNode >> replacingBlock [
	^ valueBlock ifNil: [valueBlock := self createReplacingBlock]
]

{ #category : #accessing }
RBPatternBlockNode >> sentMessages [
	^ OrderedCollection new
]
