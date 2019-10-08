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
		'return',
		'value'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBReturnNode class >> return: returnInteger value: aValueNode [ 
	^self new return: returnInteger value: aValueNode
]

{ #category : #'instance creation' }
RBReturnNode class >> value: aNode [
	^self return: 0 value: aNode
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
	^ Array with: value
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

	return := 0
]

{ #category : #testing }
RBReturnNode >> isFaulty [
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

{ #category : #'adding/removing' }
RBReturnNode >> removeNode: aNode [

	self parent removeNode: self
]

{ #category : #replacing }
RBReturnNode >> replaceNode: aNode withNode: anotherNode [ 
	value == aNode ifTrue: [self value: anotherNode]
]

{ #category : #'accessing-token' }
RBReturnNode >> return [
	^ return
]

{ #category : #'accessing-token' }
RBReturnNode >> return: anInteger [
	return := anInteger
]

{ #category : #initialization }
RBReturnNode >> return: returnInteger value: aValueNode [ 
	return := returnInteger.
	self value: aValueNode
]

{ #category : #accessing }
RBReturnNode >> start [
	^return
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
	^value
]

{ #category : #accessing }
RBReturnNode >> value: valueNode [ 
	value := valueNode.
	value parent: self
]
