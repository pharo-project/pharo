"
ASTAssignmentNode is an AST node for assignment statements.

Instance Variables:
	assignment	<Integer>	position of the :=
	value	<ASTValueNode>	the value that we're assigning
	variable	<ASTVariableNode>	the variable being assigned


"
Class {
	#name : 'OCAssignmentNode',
	#superclass : 'OCValueNode',
	#instVars : [
		'variable',
		'assignment',
		'value'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'instance creation' }
OCAssignmentNode class >> variable: aVariableNode value: aValueNode [
	^self
		variable: aVariableNode
		value: aValueNode
		position: nil
]

{ #category : 'instance creation' }
OCAssignmentNode class >> variable: aVariableNode value: aValueNode position: anInteger [
	^(self new)
		variable: aVariableNode
			value: aValueNode
			position: anInteger;
		yourself
]

{ #category : 'comparing' }
OCAssignmentNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^self variable = anObject variable and: [self value = anObject value]
]

{ #category : 'visiting' }
OCAssignmentNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitAssignmentNode: self
]

{ #category : 'accessing - token' }
OCAssignmentNode >> assignment [
	^ assignment
]

{ #category : 'accessing - token' }
OCAssignmentNode >> assignment: anInteger [
	assignment := anInteger
]

{ #category : 'accessing' }
OCAssignmentNode >> assignmentOperator [
	^  ':='
]

{ #category : 'accessing' }
OCAssignmentNode >> assignmentPosition [
	^ assignment
]

{ #category : 'testing' }
OCAssignmentNode >> assigns: aVariableName [
	^variable name = aVariableName or: [value assigns: aVariableName]
]

{ #category : 'accessing' }
OCAssignmentNode >> children [
	^ { value . variable }
]

{ #category : 'matching' }
OCAssignmentNode >> copyInContext: aDictionary [
	^ self class new
		variable: (self variable copyInContext: aDictionary);
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : 'comparing' }
OCAssignmentNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class and:
			[(self variable equalTo: anObject variable withMapping: aDictionary)
				and: [self value equalTo: anObject value withMapping: aDictionary]]
]

{ #category : 'testing' }
OCAssignmentNode >> hasBlock [

	^ value hasBlock 
]

{ #category : 'testing' }
OCAssignmentNode >> hasSameExitPoint: aBoolean [ 

	^ value hasSameExitPoint: aBoolean
]

{ #category : 'comparing' }
OCAssignmentNode >> hash [
	^self variable hash bitXor: self value hash
]

{ #category : 'testing' }
OCAssignmentNode >> isAssignment [
	^true
]

{ #category : 'errors' }
OCAssignmentNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self variable isFaulty or: [ self value isFaulty ]
]

{ #category : 'testing' }
OCAssignmentNode >> isUsingAsReturnValue: aNode [
	^aNode = value ifTrue: [true] ifFalse: [self isUsedAsReturnValue ]
]

{ #category : 'matching' }
OCAssignmentNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^(variable match: aNode variable inContext: aDictionary)
		and: [value match: aNode value inContext: aDictionary]
]

{ #category : 'testing' }
OCAssignmentNode >> needsParenthesis [
	^parent
		ifNil: [false]
		ifNotNil: [self precedence > parent precedence]
]

{ #category : 'copying' }
OCAssignmentNode >> postCopy [
	super postCopy.
	self variable: self variable copy.
	self value: self value copy
]

{ #category : 'accessing' }
OCAssignmentNode >> precedence [
	^5
]

{ #category : 'adding-removing' }
OCAssignmentNode >> removeNode: aNode [

	self replaceNode: aNode withNode: aNode receiver
]

{ #category : 'replacing' }
OCAssignmentNode >> replaceNode: aNode withNode: anotherNode [
	value == aNode ifTrue: [self value: anotherNode].
	variable == aNode ifTrue: [self variable: anotherNode]
]

{ #category : 'replacing' }
OCAssignmentNode >> replaceSourceWith: aNode [
	"Check if we need to convert the assignment. Also check if we are being replaced with a setter message send. If so, create the replacements to edit the original source."

	(aNode isAssignment and: [ aNode assignmentOperator ~= self assignmentOperator ]) ifTrue: [
		self addReplacement: (OCStringReplacement
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
					addReplacement: (OCStringReplacement
								replaceFrom: self value start
								to: self value start - 1
								with: '(');
					addReplacement: (OCStringReplacement
								replaceFrom: self value stop + 1
								to: self value stop
								with: ')')].
	self addReplacement: (OCStringReplacement
				replaceFrom: self variable start
				to: self assignmentPosition + 1
				with: aNode receiver name , ' ' , aNode selector)
]

{ #category : 'accessing' }
OCAssignmentNode >> startWithoutParentheses [
	^variable start
]

{ #category : 'accessing' }
OCAssignmentNode >> stopWithoutParentheses [
	^value stop
]

{ #category : 'accessing' }
OCAssignmentNode >> value [
	^value
]

{ #category : 'accessing' }
OCAssignmentNode >> value: aValueNode [
	value := aValueNode.
	value parent: self
]

{ #category : 'accessing' }
OCAssignmentNode >> variable [
	^variable
]

{ #category : 'accessing' }
OCAssignmentNode >> variable: varNode [
	variable := varNode.
	variable parent: self
]

{ #category : 'initialization' }
OCAssignmentNode >> variable: aVariableNode value: aValueNode position: anInteger [
	self variable: aVariableNode.
	self value: aValueNode.
	assignment := anInteger
]
