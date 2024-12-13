"
RBReturnNode is an AST node that represents a return expression.

Instance Variables:
	return	<Integer>	the position of the ^ character
	value	<RBValueNode>	the value that is being returned


"
Class {
	#name : #RBReturnNode,
	#superclass : #RBProgramNode,
	#instVars : [
		'value',
		'start'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBReturnNode class >> start: returnInteger value: aValueNode [
	^self new
		start: returnInteger;
		value: aValueNode
]

{ #category : #'instance creation' }
RBReturnNode class >> value: aNode [
	^self new
		value: aNode
]

{ #category : #comparing }
RBReturnNode >> = anObject [
	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ self value = anObject value
]

{ #category : #visiting }
RBReturnNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitReturnNode: self
]

{ #category : #accessing }
RBReturnNode >> children [
	^ { value }
]

{ #category : #testing }
RBReturnNode >> containsReturn [
	^ true
]

{ #category : #matching }
RBReturnNode >> copyInContext: aDictionary [
	^ self class new
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : #comparing }
RBReturnNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class
		and: [self value equalTo: anObject value withMapping: aDictionary]
]

{ #category : #comparing }
RBReturnNode >> hash [
	^self value hash
]

{ #category : #initialization }
RBReturnNode >> initialize [
	super initialize.

	start := 0
]

{ #category : #errors }
RBReturnNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self value isFaulty
]

{ #category : #testing }
RBReturnNode >> isReturn [
	^true
]

{ #category : #matching }
RBReturnNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^value match: aNode value inContext: aDictionary
]

{ #category : #copying }
RBReturnNode >> postCopy [
	super postCopy.
	self value: self value copy
]

{ #category : #'adding-removing' }
RBReturnNode >> removeNode: aNode [

	self parent removeNode: self
]

{ #category : #replacing }
RBReturnNode >> replaceNode: aNode withNode: anotherNode [
	value == aNode ifTrue: [ self value: anotherNode ]
]

{ #category : #accessing }
RBReturnNode >> start [
	^ start
]

{ #category : #'accessing - token' }
RBReturnNode >> start: anInteger [
	start := anInteger
]

{ #category : #accessing }
RBReturnNode >> stop [
	value ifNil:[^ 0].
	^ value stop
]

{ #category : #accessing }
RBReturnNode >> stop: anInteger [
	self value stop: anInteger
]

{ #category : #evaluating }
RBReturnNode >> value [
	^ value
]

{ #category : #accessing }
RBReturnNode >> value: anExpression [
	value := anExpression.
	value parent: self
]
