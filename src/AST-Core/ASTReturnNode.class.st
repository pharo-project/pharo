"
ASTReturnNode is an AST node that represents a return expression.

Instance Variables:
	return	<Integer>	the position of the ^ character
	value	<ASTValueNode>	the value that is being returned


"
Class {
	#name : #ASTReturnNode,
	#superclass : #ASTProgramNode,
	#instVars : [
		'return',
		'value'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
ASTReturnNode class >> return: returnInteger value: aValueNode [ 
	^self new return: returnInteger value: aValueNode
]

{ #category : #'instance creation' }
ASTReturnNode class >> value: aNode [
	^self return: 0 value: aNode
]

{ #category : #comparing }
ASTReturnNode >> = anObject [
	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ self value = anObject value
]

{ #category : #visiting }
ASTReturnNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitReturnNode: self
]

{ #category : #accessing }
ASTReturnNode >> children [
	^ { value }
]

{ #category : #testing }
ASTReturnNode >> containsReturn [
	^ true
]

{ #category : #matching }
ASTReturnNode >> copyInContext: aDictionary [ 
	^ self class new
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : #comparing }
ASTReturnNode >> equalTo: anObject withMapping: aDictionary [ 
	^self class = anObject class 
		and: [self value equalTo: anObject value withMapping: aDictionary]
]

{ #category : #comparing }
ASTReturnNode >> hash [
	^self value hash
]

{ #category : #initialization }
ASTReturnNode >> initialize [
	super initialize.

	return := 0
]

{ #category : #testing }
ASTReturnNode >> isFaulty [
	^self value isFaulty
]

{ #category : #testing }
ASTReturnNode >> isReturn [
	^true
]

{ #category : #matching }
ASTReturnNode >> match: aNode inContext: aDictionary [ 
	aNode class = self class ifFalse: [^false].
	^value match: aNode value inContext: aDictionary
]

{ #category : #copying }
ASTReturnNode >> postCopy [
	super postCopy.
	self value: self value copy
]

{ #category : #'adding-removing' }
ASTReturnNode >> removeNode: aNode [

	self parent removeNode: self
]

{ #category : #replacing }
ASTReturnNode >> replaceNode: aNode withNode: anotherNode [ 
	value == aNode ifTrue: [ self value: anotherNode ]
]

{ #category : #'accessing - token' }
ASTReturnNode >> return [
	^ return
]

{ #category : #'accessing - token' }
ASTReturnNode >> return: anInteger [
	return := anInteger
]

{ #category : #initialization }
ASTReturnNode >> return: returnInteger value: anExpression [ 
	return := returnInteger.
	self value: anExpression
]

{ #category : #accessing }
ASTReturnNode >> start [
	^ return
]

{ #category : #accessing }
ASTReturnNode >> stop [
	value ifNil:[^ 0].
	^ value stop
]

{ #category : #accessing }
ASTReturnNode >> stop: anInteger [
	self value stop: anInteger
]

{ #category : #evaluating }
ASTReturnNode >> value [
	^ value
]

{ #category : #accessing }
ASTReturnNode >> value: anExpression [ 
	value := anExpression.
	value parent: self
]
