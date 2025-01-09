"
ASTPatternBlockNode is the node in matching parse trees (it never occurs in normal Smalltalk code) that executes a block to determine if a match occurs. valueBlock takes two arguments, the first is the actual node that we are trying to match against, and the second node is the dictionary that contains all the metavariable bindings that the matcher has made thus far.

Instance Variables:
	valueBlock	<BlockClosure>	The block to execute when attempting to match this to a node.


"
Class {
	#name : 'OCPatternBlockNode',
	#superclass : 'OCBlockNode',
	#instVars : [
		'valueBlock'
	],
	#category : 'AST-Core-Pattern',
	#package : 'AST-Core',
	#tag : 'Pattern'
}

{ #category : 'visiting' }
OCPatternBlockNode >> acceptVisitor: aProgramNodeVisitor [
	^aProgramNodeVisitor visitPatternBlockNode: self
]

{ #category : 'matching' }
OCPatternBlockNode >> addArgumentWithNameBasedOn: aString to: aBlockNode [
	| name index vars |
	name := aString.
	vars := aBlockNode allDefinedVariables.
	index := 0.
	[vars includes: name] whileTrue:
			[index := index + 1.
			name := name , index printString].
	aBlockNode
		arguments: (aBlockNode arguments copyWith: (OCVariableNode named: name))
]

{ #category : 'matching' }
OCPatternBlockNode >> constructLookupNodeFor: aString in: aBlockNode [
	^ OCMessageNode
		  receiver: OCVariableNode selfNode
		  selector: #lookupMatchFor:in:
		  arguments: (Array with: (OCLiteralNode value: aString) with: aBlockNode arguments last copy)
]

{ #category : 'matching' }
OCPatternBlockNode >> copyInContext: aDictionary [
	^ self replacingBlock value: aDictionary
]

{ #category : 'matching' }
OCPatternBlockNode >> createMatchingBlock [
	| newBlock |
	self arguments size > 2
		ifTrue:
			[self
				error: 'Search blocks can only contain arguments for the node and matching dictionary'].
	newBlock := OCBlockNode arguments: arguments body: body.
	newBlock arguments isEmpty
		ifTrue: [self addArgumentWithNameBasedOn: 'aNode' to: newBlock].
	newBlock arguments size = 1
		ifTrue: [self addArgumentWithNameBasedOn: 'aDictionary' to: newBlock].
	self replacePatternNodesIn: newBlock.
	^newBlock evaluateForReceiver: self
]

{ #category : 'matching' }
OCPatternBlockNode >> createReplacingBlock [
	| newBlock |
	self arguments size > 1
		ifTrue:
			[self
				error: 'Replace blocks can only contain an argument for the matching dictionary'].
	newBlock := OCBlockNode arguments: arguments body: body.
	self arguments isEmpty
		ifTrue: [self addArgumentWithNameBasedOn: 'aDictionary' to: newBlock].
	self replacePatternNodesIn: newBlock.
	^newBlock evaluateForReceiver: self
]

{ #category : 'testing - matching' }
OCPatternBlockNode >> isPatternNode [
	^ true
]

{ #category : 'matching' }
OCPatternBlockNode >> lookupMatchFor: aString in: aDictionary [
	^aDictionary at: aString
		ifAbsent:
			[| variableNode |
			variableNode := OCPatternVariableNode named: aString.
			aDictionary at: variableNode ifAbsent: [nil]]
]

{ #category : 'matching' }
OCPatternBlockNode >> match: aNode inContext: aDictionary [
	^self matchingBlock value: aNode value: aDictionary
]

{ #category : 'matching' }
OCPatternBlockNode >> matchingBlock [
	^ valueBlock ifNil: [valueBlock := self createMatchingBlock]
]

{ #category : 'matching' }
OCPatternBlockNode >> replacePatternNodesIn: aBlockNode [
	aBlockNode body nodesDo:
			[:each |
			(each isVariable and: [each isPatternNode])
				ifTrue:
					[each
						replaceWith: (self constructLookupNodeFor: each name in: aBlockNode)]]
]

{ #category : 'matching' }
OCPatternBlockNode >> replacingBlock [
	^ valueBlock ifNil: [valueBlock := self createReplacingBlock]
]

{ #category : 'accessing' }
OCPatternBlockNode >> sentMessages [
	^ OrderedCollection new
]
