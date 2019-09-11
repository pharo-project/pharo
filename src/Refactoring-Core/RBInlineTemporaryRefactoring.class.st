"
I am a refactoring to replace a temporary variable by code.

All references to the temporary variable in this method are replaced by the value used to initialize the temporary variable. 
The initialization and declaration of this variable will be removed. You need to select the variable and its initial assignment code to apply this refactoring.
"
Class {
	#name : #RBInlineTemporaryRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'sourceInterval',
		'selector',
		'sourceTree',
		'assignmentNode',
		'definingNode'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBInlineTemporaryRefactoring class >> inline: anInterval from: aSelector in: aClass [ 
	^self new 
		inline: anInterval
		from: aSelector
		in: aClass
]

{ #category : #'instance creation' }
RBInlineTemporaryRefactoring class >> model: aRBSmalltalk inline: anInterval from: aSelector in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		inline: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : #transforming }
RBInlineTemporaryRefactoring >> compileMethod [
	class compileTree: sourceTree
]

{ #category : #testing }
RBInlineTemporaryRefactoring >> hasOnlyOneAssignment [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: assignmentNode variable name , ' := ``@object'
		do: [ :aNode :answer | answer + 1 ].
	^ (searcher executeTree: definingNode initialAnswer: 0) == 1
]

{ #category : #initialization }
RBInlineTemporaryRefactoring >> inline: anInterval from: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval
]

{ #category : #preconditions }
RBInlineTemporaryRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[self verifySelectedInterval.
					true])
]

{ #category : #transforming }
RBInlineTemporaryRefactoring >> replaceAssignment [
	assignmentNode parent isSequence 
		ifTrue: [assignmentNode parent removeNode: assignmentNode]
		ifFalse: [assignmentNode replaceWith: assignmentNode value]
]

{ #category : #transforming }
RBInlineTemporaryRefactoring >> replaceReferences [
	| rewriter |
	rewriter := RBParseTreeRewriter new.
	rewriter replaceTree: assignmentNode variable
		withTree: assignmentNode value.
	definingNode removeTemporaryNamed: assignmentNode variable name.
	rewriter executeTree: definingNode
]

{ #category : #printing }
RBInlineTemporaryRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' inline: '.
	sourceInterval storeOn: aStream.
	aStream
		nextPutAll: ' from: #';
		nextPutAll: selector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBInlineTemporaryRefactoring >> transform [
	self
		replaceAssignment;
		replaceReferences;
		compileMethod
]

{ #category : #preconditions }
RBInlineTemporaryRefactoring >> verifySelectedInterval [

	sourceTree := class parseTreeFor: selector.
	sourceTree ifNil: [ self refactoringFailure: 'Could not parse source' ].
	assignmentNode := sourceTree whichNodeIsContainedBy: sourceInterval.
	assignmentNode isAssignment
		ifFalse: [ self refactoringFailure: 'The selected node is not an assignment statement' ].
	definingNode := assignmentNode whoDefines: assignmentNode variable name.
	self hasOnlyOneAssignment
		ifFalse: [ self refactoringError: 'There are multiple assignments to the variable' ].
	( RBReadBeforeWrittenTester
		isVariable: assignmentNode variable name
		writtenBeforeReadIn: definingNode )
		ifFalse: [ self refactoringError: 'The variable is possible read before it is assigned' ]
]
