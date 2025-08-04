"
ASTReturnNode is an AST node that represents a return expression.

Instance Variables:
	return	<Integer>	the position of the ^ character
	value	<ASTValueNode>	the value that is being returned


"
Class {
	#name : 'OCReturnNode',
	#superclass : 'OCProgramNode',
	#instVars : [
		'value',
		'start'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCReturnNode class >> start: returnInteger value: aValueNode [
	^self new
		start: returnInteger;
		value: aValueNode
]

{ #category : 'instance creation' }
OCReturnNode class >> value: aNode [
	^self new
		value: aNode
]

{ #category : 'comparing' }
OCReturnNode >> = anObject [
	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ self value = anObject value
]

{ #category : 'visiting' }
OCReturnNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitReturnNode: self
]

{ #category : 'accessing' }
OCReturnNode >> children [
	^ { value }
]

{ #category : 'testing' }
OCReturnNode >> containsReturn [
	^ true
]

{ #category : 'matching' }
OCReturnNode >> copyInContext: aDictionary [
	^ self class new
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : 'comparing' }
OCReturnNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class
		and: [self value equalTo: anObject value withMapping: aDictionary]
]

{ #category : 'testing' }
OCReturnNode >> hasBlock [

	^ value hasBlock 
]

{ #category : 'testing' }
OCReturnNode >> hasSameExitPoint [

	^ true
]

{ #category : 'testing' }
OCReturnNode >> hasSameExitPoint: aBoolean [

	^ value hasSameExitPoint: aBoolean 
]

{ #category : 'comparing' }
OCReturnNode >> hash [
	^self value hash
]

{ #category : 'initialization' }
OCReturnNode >> initialize [
	super initialize.

	start := 0
]

{ #category : 'errors' }
OCReturnNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self value isFaulty
]

{ #category : 'testing' }
OCReturnNode >> isReturn [
	^true
]

{ #category : 'testing' }
OCReturnNode >> lastIsReturn [ 
	^ true
]

{ #category : 'matching' }
OCReturnNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^value match: aNode value inContext: aDictionary
]

{ #category : 'copying' }
OCReturnNode >> postCopy [
	super postCopy.
	self value: self value copy
]

{ #category : 'adding-removing' }
OCReturnNode >> removeNode: aNode [

	self parent removeNode: self
]

{ #category : 'replacing' }
OCReturnNode >> replaceNode: aNode withNode: anotherNode [
	value == aNode ifTrue: [ self value: anotherNode ]
]

{ #category : 'accessing' }
OCReturnNode >> start [
	^ start
]

{ #category : 'accessing - token' }
OCReturnNode >> start: anInteger [
	start := anInteger
]

{ #category : 'accessing' }
OCReturnNode >> stop [
	value ifNil:[^ 0].
	^ value stop
]

{ #category : 'accessing' }
OCReturnNode >> stop: anInteger [
	self value stop: anInteger
]

{ #category : 'evaluating' }
OCReturnNode >> value [
	^ value
]

{ #category : 'accessing' }
OCReturnNode >> value: anExpression [
	value := anExpression.
	value parent: self
]
