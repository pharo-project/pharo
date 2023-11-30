"
RBAssignmentNode is an AST node for assignment statements.

Instance Variables:
	assignment	<Integer>	position of the :=
	value	<RBValueNode>	the value that we're assigning
	variable	<RBVariableNode>	the variable being assigned


"
Class {
	#name : 'ASTAssignmentNode',
	#superclass : 'ASTValueNode',
	#instVars : [
		'variable',
		'assignment',
		'value'
	],
	#category : 'AST-Core-Nodes',
	#package : 'AST-Core',
	#tag : 'Nodes'
}

{ #category : 'class initialization' }
ASTAssignmentNode class >> initialize [

	self deprecatedAliases: { #ASTAssignmentNode }
]

{ #category : 'instance creation' }
ASTAssignmentNode class >> variable: aVariableNode value: aValueNode [
	^self
		variable: aVariableNode
		value: aValueNode
		position: nil
]

{ #category : 'instance creation' }
ASTAssignmentNode class >> variable: aVariableNode value: aValueNode position: anInteger [
	^(self new)
		variable: aVariableNode
			value: aValueNode
			position: anInteger;
		yourself
]

{ #category : 'comparing' }
ASTAssignmentNode >> = anObject [
	self == anObject ifTrue: [^true].
	self class = anObject class ifFalse: [^false].
	^self variable = anObject variable and: [self value = anObject value]
]

{ #category : 'visiting' }
ASTAssignmentNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitAssignmentNode: self
]

{ #category : 'accessing - token' }
ASTAssignmentNode >> assignment [
	^ assignment
]

{ #category : 'accessing - token' }
ASTAssignmentNode >> assignment: anInteger [
	assignment := anInteger
]

{ #category : 'accessing' }
ASTAssignmentNode >> assignmentOperator [
	^  ':='
]

{ #category : 'accessing' }
ASTAssignmentNode >> assignmentPosition [
	^ assignment
]

{ #category : 'testing' }
ASTAssignmentNode >> assigns: aVariableName [
	^variable name = aVariableName or: [value assigns: aVariableName]
]

{ #category : 'accessing' }
ASTAssignmentNode >> children [
	^ { value . variable }
]

{ #category : 'matching' }
ASTAssignmentNode >> copyInContext: aDictionary [
	^ self class new
		variable: (self variable copyInContext: aDictionary);
		value: (self value copyInContext: aDictionary);
		yourself
]

{ #category : 'testing' }
ASTAssignmentNode >> directlyUses: aNode [
	^aNode = value ifTrue: [true] ifFalse: [self isDirectlyUsed]
]

{ #category : 'comparing' }
ASTAssignmentNode >> equalTo: anObject withMapping: aDictionary [
	^self class = anObject class and:
			[(self variable equalTo: anObject variable withMapping: aDictionary)
				and: [self value equalTo: anObject value withMapping: aDictionary]]
]

{ #category : 'comparing' }
ASTAssignmentNode >> hash [
	^self variable hash bitXor: self value hash
]

{ #category : 'testing' }
ASTAssignmentNode >> isAssignment [
	^true
]

{ #category : 'errors' }
ASTAssignmentNode >> isFaulty [
	self isError ifTrue: [ ^ true ].
	^self variable isFaulty or: [ self value isFaulty ]
]

{ #category : 'matching' }
ASTAssignmentNode >> match: aNode inContext: aDictionary [
	aNode class = self class ifFalse: [^false].
	^(variable match: aNode variable inContext: aDictionary)
		and: [value match: aNode value inContext: aDictionary]
]

{ #category : 'testing' }
ASTAssignmentNode >> needsParenthesis [
	^parent
		ifNil: [false]
		ifNotNil: [self precedence > parent precedence]
]

{ #category : 'copying' }
ASTAssignmentNode >> postCopy [
	super postCopy.
	self variable: self variable copy.
	self value: self value copy
]

{ #category : 'accessing' }
ASTAssignmentNode >> precedence [
	^5
]

{ #category : 'adding-removing' }
ASTAssignmentNode >> removeNode: aNode [

	self replaceNode: aNode withNode: aNode receiver
]

{ #category : 'replacing' }
ASTAssignmentNode >> replaceNode: aNode withNode: anotherNode [
	value == aNode ifTrue: [self value: anotherNode].
	variable == aNode ifTrue: [self variable: anotherNode]
]

{ #category : 'replacing' }
ASTAssignmentNode >> replaceSourceWith: aNode [
	"Check if we need to convert the assignment. Also check if we are being replaced with a setter message send. If so, create the replacements to edit the original source."

	(aNode isAssignment and: [ aNode assignmentOperator ~= self assignmentOperator ]) ifTrue: [
		self addReplacement: (ASTStringReplacement
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
					addReplacement: (ASTStringReplacement
								replaceFrom: self value start
								to: self value start - 1
								with: '(');
					addReplacement: (ASTStringReplacement
								replaceFrom: self value stop + 1
								to: self value stop
								with: ')')].
	self addReplacement: (ASTStringReplacement
				replaceFrom: self variable start
				to: self assignmentPosition + 1
				with: aNode receiver name , ' ' , aNode selector)
]

{ #category : 'accessing' }
ASTAssignmentNode >> startWithoutParentheses [
	^variable start
]

{ #category : 'accessing' }
ASTAssignmentNode >> stopWithoutParentheses [
	^value stop
]

{ #category : 'testing' }
ASTAssignmentNode >> uses: aNode [
	^aNode = value ifTrue: [true] ifFalse: [self isUsed]
]

{ #category : 'accessing' }
ASTAssignmentNode >> value [
	^value
]

{ #category : 'accessing' }
ASTAssignmentNode >> value: aValueNode [
	value := aValueNode.
	value parent: self
]

{ #category : 'accessing' }
ASTAssignmentNode >> variable [
	^variable
]

{ #category : 'accessing' }
ASTAssignmentNode >> variable: varNode [
	variable := varNode.
	variable parent: self
]

{ #category : 'initialization' }
ASTAssignmentNode >> variable: aVariableNode value: aValueNode position: anInteger [
	self variable: aVariableNode.
	self value: aValueNode.
	assignment := anInteger
]
