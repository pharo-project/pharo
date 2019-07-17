"
I am a refactoring splitting a cascade message send to multiple messages.

You can select an interval containing a cascade expression. The refactoring will split this expression to two message sends to the receiver. 

My preconditions verify that the selector containing the cascaded message send is defined in this class, and a cascade message can be found.

If the receiver of the cascade expression is a literal or the return value of another message send, I will add another temporary variable for the interim result.
"
Class {
	#name : #RBSplitCascadeRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'selectedInterval',
		'parseTree',
		'cascadeNode',
		'beforeNodes',
		'afterNodes',
		'ancestorNode'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBSplitCascadeRefactoring class >> model: aNamespace split: anInterval from: aSelector in: aClass [
	^ self new
		model: aNamespace;
		split: anInterval from: aSelector in: aClass;
		yourself
]

{ #category : #'instance creation' }
RBSplitCascadeRefactoring class >> split: anInterval from: aSelector in: aClass [
	^ self new 
		split: anInterval from: aSelector in: aClass;
		yourself
]

{ #category : #transforming }
RBSplitCascadeRefactoring >> extractReceiver [
	| name |
	(cascadeNode receiver isLiteralNode or: [ cascadeNode receiver isVariable ])
		ifTrue: [ ^ self ].
	name := self 
		safeVariableNameFor: class 
		temporaries: self parseTree allDefinedVariables 
		basedOn: 'receiver'.
	ancestorNode parent
		addTemporaryNamed: name;
		addNode: (RBAssignmentNode 
			variable: (RBVariableNode named: name)
			value: cascadeNode receiver)
		before: ancestorNode.
	cascadeNode messages
		do: [ :each | each receiver: (RBVariableNode named: name) ]
		
]

{ #category : #preconditions }
RBSplitCascadeRefactoring >> findAncestorNode [
	"The ancestor node is the node that is contained within the sequence. In most cases this is the cascade itself, but it also can be an assignment or a return node."

	ancestorNode := cascadeNode.
	[ ancestorNode parent isSequence not and: [ ancestorNode parent isAssignment ] ]
		whileTrue: [ ancestorNode := ancestorNode parent ].
	[ ancestorNode parent isSequence not and: [ ancestorNode parent isReturn ] ]
		whileTrue: [ ancestorNode := ancestorNode parent ].
	ancestorNode parent isSequence
		ifFalse: [ self refactoringFailure: 'To split this cascade, you must extract it to a temporary first' ]
]

{ #category : #preconditions }
RBSplitCascadeRefactoring >> findCascadeNode [

	"Find the cascade to be split."

	cascadeNode := self parseTree bestNodeFor: selectedInterval.
	[ cascadeNode isNil or: [ cascadeNode isCascade ] ]
		whileFalse: [ cascadeNode := cascadeNode parent ].
	cascadeNode
		ifNil: [ self refactoringFailure: 'The selection doesn''t appear to be within a cascade' ]
]

{ #category : #preconditions }
RBSplitCascadeRefactoring >> findMessageNodes [
	"Find the nodes that form the first part of the cascade and the second part of the cascade."

	beforeNodes := cascadeNode messages
		select: [ :each | each stop <= selectedInterval first ].
	afterNodes := cascadeNode messages
		select: [ :each | selectedInterval last <= each keywordsPositions first].
	(beforeNodes isEmpty or: [ afterNodes isEmpty ])
		ifTrue: [ self refactoringFailure: 'Splitting a cascade into the whole cascade and an empty one is pointless' ].
	(beforeNodes size + afterNodes size = cascadeNode messages size)
		ifFalse: [ self refactoringFailure: 'To set the split boundary place the cursor inbetween two cascaded messages' ]
]

{ #category : #accessing }
RBSplitCascadeRefactoring >> parseTree [

	parseTree
		ifNil: [ parseTree := class parseTreeFor: selector.
			parseTree ifNil: [ self refactoringFailure: 'Could not parse sources' ]
			].
	^ parseTree
]

{ #category : #preconditions }
RBSplitCascadeRefactoring >> preconditions [
	^ (RBCondition definesSelector: selector in: class) & (RBCondition withBlock: [ 
		self findCascadeNode; findAncestorNode; findMessageNodes. 
		true ])
]

{ #category : #initialization }
RBSplitCascadeRefactoring >> split: anInterval from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	selectedInterval := anInterval
]

{ #category : #transforming }
RBSplitCascadeRefactoring >> splitCascade [
	ancestorNode parent
		addNode: (beforeNodes size > 1
			ifTrue: [ RBCascadeNode messages: beforeNodes ]
			ifFalse: [ beforeNodes first ])
		before: ancestorNode.
	afterNodes size > 1
		ifTrue: [ cascadeNode messages: afterNodes ]
		ifFalse: [ cascadeNode replaceWith: afterNodes first ].
	class compileTree: ancestorNode methodNode
]

{ #category : #printing }
RBSplitCascadeRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' split: '.
	selectedInterval storeOn: aStream.
	aStream
		nextPutAll: ' inMethod: #';
		nextPutAll: selector;
		nextPutAll: ' forClass: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBSplitCascadeRefactoring >> transform [
	self extractReceiver.
	self splitCascade
]
