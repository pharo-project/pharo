"
RBAssignmentNode is an AST node for assignment statements.

Instance Variables:
	assignment	<Integer>	position of the :=
	value	<RBValueNode>	the value that we're assigning
	variable	<RBVariableNode>	the variable being assigned


"
Class {
	#name : #RBAssignmentNode,
	#superclass : #RBValueNode,
	#instVars : [
		'variable',
		'assignment',
		'value'
	],
	#category : #'AST-Core-Nodes'
}

{ #category : #'instance creation' }
RBAssignmentNode class >> variable: aVariableNode value: aValueNode [
	^self
		variable: aVariableNode
		value: aValueNode
		position: nil
]

{ #category : #'instance creation' }
RBAssignmentNode class >> variable: aVariableNode value: aValueNode position: anInteger [
	^(self new)
		variable: aVariableNode
			value: aValueNode
			position: anInteger;
		yourself
]

{ #category : #comparing }
RBAssignmentNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^self variable = anObject variable and: [self value = anObject value]
]

{ #category : #visiting }
RBAssignmentNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitAssignmentNode: self
]

{ #category : #'accessing - token' }
RBAssignmentNode >> assignment [
	^ assignment
]

{ #category : #'accessing - token' }
RBAssignmentNode >> assignment: anInteger [
	assignment := anInteger
]

{ #category : #accessing }
RBAssignmentNode >> assignmentOperator [
	^  ':='
]

{ #category : #accessing }
RBAssignmentNode >> assignmentPosition [
	^ assignment
]

{ #category : #testing }
RBAssignmentNode >> assigns: aVariableName [
	^variable name = aVariableName or: [value assigns: aVariableName]
]

{ #category : #accessing }
RBAssignmentNode >> children [
	^ { value . variable }
]

{ #category : #matching }
RBAssignmentNode >> copyInContext: aDictionary [
	^ self class new
		variable: (self variable copyInContext: aDictionary);
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : #testing }
RBAssignmentNode >> directlyUses: aNode [
	^aNode = value ifTrue: [true] ifFalse: [self isDirectlyUsed]
]

{ #category : #comparing }
RBAssignmentNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class and:
			[(self variable equalTo: anObject variable withMapping: aDictionary)
				and: [self value equalTo: anObject value withMapping: aDictionary]]
]

{ #category : #comparing }
RBAssignmentNode >> hash [
	^self variable hash bitXor: self value hash
]

{ #category : #testing }
RBAssignmentNode >> isAssignment [
	^true
]

{ #category : #errors }
RBAssignmentNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self variable isFaulty or: [ self value isFaulty ]
]

{ #category : #matching }
RBAssignmentNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^(variable match: aNode variable inContext: aDictionary)
		and: [value match: aNode value inContext: aDictionary]
]

{ #category : #testing }
RBAssignmentNode >> needsParenthesis [
	^parent
		ifNil: [false]
		ifNotNil: [self precedence > parent precedence]
]

{ #category : #copying }
RBAssignmentNode >> postCopy [
	super postCopy.
	self variable: self variable copy.
	self value: self value copy
]

{ #category : #accessing }
RBAssignmentNode >> precedence [
	^5
]

{ #category : #'adding-removing' }
RBAssignmentNode >> removeNode: aNode [

	self replaceNode: aNode withNode: aNode receiver
]

{ #category : #replacing }
RBAssignmentNode >> replaceNode: aNode withNode: anotherNode [
	value == aNode ifTrue: [self value: anotherNode].
	variable == aNode ifTrue: [self variable: anotherNode]
]

{ #category : #replacing }
RBAssignmentNode >> replaceSourceWith: aNode [
	"Check if we need to convert the assignment. Also check if we are being replaced with a setter message send. If so, create the replacements to edit the original source."

	(aNode isAssignment and: [ aNode assignmentOperator ~= self assignmentOperator ]) ifTrue: [
		self addReplacement: (RBStringReplacement
			replaceFrom: self assignmentPosition
			to: self assignmentPosition + self assignmentOperator size - 1
			with: aNode assignmentOperator).
		(aNode variable = variable and: [ aNode value = value ])
			ifTrue: [ ^ self ] ].
	aNode isMessage ifFalse: [^super replaceSourceWith: aNode].
	aNode receiver isVariable ifFalse: [^super replaceSourceWith: aNode].
	aNode numArgs = 1 ifFalse: [^super replaceSourceWith: aNode].
	(self mappingFor: self value) = aNode arguments first
		ifFalse: [^super replaceSourceWith: aNode].
	(self value hasParentheses not
		and: [aNode arguments first precedence >= aNode precedence])
			ifTrue:
				[self
					addReplacement: (RBStringReplacement
								replaceFrom: self value start
								to: self value start - 1
								with: '(');
					addReplacement: (RBStringReplacement
								replaceFrom: self value stop + 1
								to: self value stop
								with: ')')].
	self addReplacement: (RBStringReplacement
				replaceFrom: self variable start
				to: self assignmentPosition + 1
				with: aNode receiver name , ' ' , aNode selector)
]

{ #category : #accessing }
RBAssignmentNode >> startWithoutParentheses [
	^variable start
]

{ #category : #accessing }
RBAssignmentNode >> stopWithoutParentheses [
	^value stop
]

{ #category : #testing }
RBAssignmentNode >> uses: aNode [
	^aNode = value ifTrue: [true] ifFalse: [self isUsed]
]

{ #category : #accessing }
RBAssignmentNode >> value [
	^value
]

{ #category : #accessing }
RBAssignmentNode >> value: aValueNode [
	value := aValueNode.
	value parent: self
]

{ #category : #accessing }
RBAssignmentNode >> variable [
	^variable
]

{ #category : #accessing }
RBAssignmentNode >> variable: varNode [
	variable := varNode.
	variable parent: self
]

{ #category : #initialization }
RBAssignmentNode >> variable: aVariableNode value: aValueNode position: anInteger [
	self variable: aVariableNode.
	self value: aValueNode.
	assignment := anInteger
]
