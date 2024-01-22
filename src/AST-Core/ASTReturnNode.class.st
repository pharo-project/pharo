"
RBReturnNode is an AST node that represents a return expression.

Instance Variables:
	return	<Integer>	the position of the ^ character
	value	<RBValueNode>	the value that is being returned


"
Class {
	#name : 'ASTReturnNode',
	#superclass : 'ASTProgramNode',
	#instVars : [
		'value',
		'start'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'class initialization' }
ASTReturnNode class >> initialize [
	self deprecatedAliases: { #RBReturnNode }
]

{ #category : 'instance creation' }
ASTReturnNode class >> start: returnInteger value: aValueNode [
	^self new
		start: returnInteger;
		value: aValueNode
]

{ #category : 'instance creation' }
ASTReturnNode class >> value: aNode [
	^self new
		value: aNode
]

{ #category : 'comparing' }
ASTReturnNode >> = anObject [
	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ self value = anObject value
]

{ #category : 'visiting' }
ASTReturnNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitReturnNode: self
]

{ #category : 'accessing' }
ASTReturnNode >> children [
	^ { value }
]

{ #category : 'testing' }
ASTReturnNode >> containsReturn [
	^ true
]

{ #category : 'matching' }
ASTReturnNode >> copyInContext: aDictionary [
	^ self class new
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : 'comparing' }
ASTReturnNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class
		and: [self value equalTo: anObject value withMapping: aDictionary]
]

{ #category : 'comparing' }
ASTReturnNode >> hash [
	^self value hash
]

{ #category : 'initialization' }
ASTReturnNode >> initialize [
	super initialize.

	start := 0
]

{ #category : 'errors' }
ASTReturnNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self value isFaulty
]

{ #category : 'testing' }
ASTReturnNode >> isReturn [
	^true
]

{ #category : 'matching' }
ASTReturnNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^value match: aNode value inContext: aDictionary
]

{ #category : 'copying' }
ASTReturnNode >> postCopy [
	super postCopy.
	self value: self value copy
]

{ #category : 'adding-removing' }
ASTReturnNode >> removeNode: aNode [

	self parent removeNode: self
]

{ #category : 'replacing' }
ASTReturnNode >> replaceNode: aNode withNode: anotherNode [
	value == aNode ifTrue: [ self value: anotherNode ]
]

{ #category : 'accessing' }
ASTReturnNode >> start [
	^ start
]

{ #category : 'accessing - token' }
ASTReturnNode >> start: anInteger [
	start := anInteger
]

{ #category : 'accessing' }
ASTReturnNode >> stop [
	value ifNil:[^ 0].
	^ value stop
]

{ #category : 'accessing' }
ASTReturnNode >> stop: anInteger [
	self value stop: anInteger
]

{ #category : 'evaluating' }
ASTReturnNode >> value [
	^ value
]

{ #category : 'accessing' }
ASTReturnNode >> value: anExpression [
	value := anExpression.
	value parent: self
]
