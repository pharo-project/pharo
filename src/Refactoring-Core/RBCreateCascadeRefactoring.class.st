"
I am  a refactoring used to generate cascades in source code.

Two or more message sends to the same object are replaced by a cascaded message send. It expects a selection of the messages and the receiver variable.

"
Class {
	#name : #RBCreateCascadeRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'selectedInterval',
		'parseTree',
		'sequenceNode',
		'statementNodes',
		'transformedNode'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBCreateCascadeRefactoring class >> combine: anInterval from: aSelector in: aClass [
	^ self new 
		combine: anInterval from: aSelector in: aClass;
		yourself
]

{ #category : #'instance creation' }
RBCreateCascadeRefactoring class >> model: aNamespace combine: anInterval from: aSelector in: aClass [
	^ self new 
		model: aNamespace; 
		combine: anInterval from: aSelector in: aClass;
		yourself
]

{ #category : #preconditions }
RBCreateCascadeRefactoring >> addStatementNode: aNode [
	aNode isMessage
		ifTrue: [ ^ statementNodes add: aNode ].
	aNode isCascade
		ifTrue: [ ^ statementNodes addAll: aNode messages ].
	self refactoringFailure: aNode formattedCode , ' is not a valid message'
]

{ #category : #initialization }
RBCreateCascadeRefactoring >> combine: anInterval from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	selectedInterval := anInterval
]

{ #category : #transforming }
RBCreateCascadeRefactoring >> combineMessages [
	"This combines the messages and adds the assignements of the last statement to the cascade. This is not necessary if there is a return, because the refactoring engine automatically compensates for that."

	| expression |
	transformedNode := RBCascadeNode messages: (statementNodes
		collect: [ :each | each copy ]).
	expression := statementNodes last parent.
	[ expression isAssignment ] whileTrue: [
		transformedNode := RBAssignmentNode
			variable: expression variable
			value: transformedNode.
		expression := expression parent ]
]

{ #category : #transforming }
RBCreateCascadeRefactoring >> compileCode [
	class compileTree: (RBParseTreeRewriter
		replaceStatements: sequenceNode formattedCode
		with: transformedNode formattedCode
		in: self parseTree
		onInterval: selectedInterval)
]

{ #category : #preconditions }
RBCreateCascadeRefactoring >> findReceiverNode [
	"Find the sequence to be combined."

	| receiverNodes |
	receiverNodes := statementNodes 
		collect: [ :each | each receiver ].
	receiverNodes asSet size = 1
		ifFalse: [ self refactoringError: 'All statements must have the same receiver' ].
	(receiverNodes first isLiteralNode or: [ receiverNodes first isVariable ])
		ifFalse: [ self refactoringWarning: 'The receiver is an expression. Proceed with caution' ]
]

{ #category : #preconditions }
RBCreateCascadeRefactoring >> findSequenceNode [
	"Find the sequence to be combined."

	sequenceNode := self parserClass
		parseExpression: self selectedSource
		onError: [ :msg :pos | self refactoringFailure: 'Invalid source to rewrite' ].
	(sequenceNode isSequence and: [ sequenceNode statements size > 1 ])
		ifFalse: [ self refactoringFailure: 'You must select two or more statements' ]
]

{ #category : #preconditions }
RBCreateCascadeRefactoring >> findStatementNodes [
	"Find the sequence to be combined."

	statementNodes := OrderedCollection new.
	sequenceNode statements do: [ :each |
		(sequenceNode isLast: each)
			ifFalse: [ self addStatementNode: each ]
			ifTrue: [
				| current |
				current := each.
				[ current isReturn or: [ current isAssignment ] ]
					whileTrue: [ current := current value ].
				self addStatementNode: current ] ]
]

{ #category : #accessing }
RBCreateCascadeRefactoring >> parseTree [

	parseTree
		ifNil: [ parseTree := class parseTreeFor: selector.
			parseTree ifNil: [ self refactoringFailure: 'Could not parse sources' ]
			].
	^ parseTree
]

{ #category : #preconditions }
RBCreateCascadeRefactoring >> preconditions [
	^ (RBCondition definesSelector: selector in: class) & (RBCondition withBlock: [ 
		self findSequenceNode; findStatementNodes; findReceiverNode.
		true ])
]

{ #category : #accessing }
RBCreateCascadeRefactoring >> selectedSource [
	^  self parseTree source copyFrom: selectedInterval first to: selectedInterval last
]

{ #category : #transforming }
RBCreateCascadeRefactoring >> transform [
	self combineMessages.
	self compileCode
]
