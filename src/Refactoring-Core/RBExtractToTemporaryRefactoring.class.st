"
Add a new temporary variable for the value of the selected code. Every place in this method using the same piece of code is replaced by accessing this new temporary variable instead.
As the code is now only evaluated once for initializing the variable value, this refactoring may modify the behavior if the code statements didn't evaluate to the same value on every call.

My preconditions verify that the new temporary name is a valid name and isn't already used (neither a temporary, an instance variable or a class variable).
"
Class {
	#name : #RBExtractToTemporaryRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'sourceInterval',
		'selector',
		'newVariableName',
		'parseTree'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBExtractToTemporaryRefactoring class >> extract: anInterval to: aString from: aSelector in: aClass [ 
	^self new 
		extract: anInterval
		to: aString
		from: aSelector
		in: aClass
]

{ #category : #'instance creation' }
RBExtractToTemporaryRefactoring class >> model: aRBSmalltalk extract: anInterval to: aString from: aSelector in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		extract: anInterval
			to: aString
			from: aSelector
			in: aClass;
		yourself
]

{ #category : #preconditions }
RBExtractToTemporaryRefactoring >> checkVariableName [
	(class whoDefinesInstanceVariable: newVariableName) ifNotNil: 
			[self refactoringError: ('<1p> defines an instance variable named <2s>' 
						expandMacrosWith: class
						with: newVariableName)].
	(class whoDefinesClassVariable: newVariableName) ifNotNil: 
			[self refactoringError: ('<1p> defines a class variabled named <2s>' 
						expandMacrosWith: class
						with: newVariableName)].
	(self parseTree allDefinedVariables includes: newVariableName) 
		ifTrue: 
			[self refactoringError: ('<1s> is already a temporary variable name' 
						expandMacrosWith: newVariableName)]
]

{ #category : #transforming }
RBExtractToTemporaryRefactoring >> compileNewMethod [
	class compileTree: self parseTree
]

{ #category : #transforming }
RBExtractToTemporaryRefactoring >> constructAssignmentFrom: aNode [ 
	| valueNode |
	valueNode := RBVariableNode named: newVariableName.
	^RBAssignmentNode variable: valueNode value: aNode
]

{ #category : #initialization }
RBExtractToTemporaryRefactoring >> extract: anInterval to: aString from: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval.
	newVariableName := aString
]

{ #category : #transforming }
RBExtractToTemporaryRefactoring >> insertTemporary [
	| node statementNode nodeReferences children |
	node := self parseTree whichNodeIsContainedBy: sourceInterval.
	(node notNil and: [ node isValue ])
		ifFalse: [ self refactoringFailure: 'Cannot assign to non-value nodes' ].
	children := self parseTree body allChildren.
	nodeReferences := children select: [ :each | each = node ] thenCollect: [ :each | each ].
	statementNode := node statementNode.
	nodeReferences do: [ :each | each replaceWith: (RBVariableNode named: newVariableName) ].
	statementNode parent
		addNode: (self constructAssignmentFrom: node)
			before:
				(node == statementNode
						ifTrue: [ RBVariableNode named: newVariableName ]
						ifFalse: [ statementNode ]);
		addTemporaryNamed: newVariableName
]

{ #category : #'private-accessing' }
RBExtractToTemporaryRefactoring >> parseTree [

	parseTree
		ifNil: [ parseTree := class parseTreeFor: selector.
			parseTree ifNil: [ self refactoringFailure: 'Could not parse method' ]
			].
	^ parseTree
]

{ #category : #preconditions }
RBExtractToTemporaryRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition isValidInstanceVariableName: newVariableName for: class) 
			& (RBCondition withBlock: 
						[self verifySelectedInterval.
						self checkVariableName.
						true])
]

{ #category : #'private-accessing' }
RBExtractToTemporaryRefactoring >> selectedSource [

	| source |

	source := class sourceCodeFor: selector.
	source ifNil: [ self refactoringFailure: 'Couldn''t find sources' ].
	( ( sourceInterval first between: 1 and: source size )
		and: [ sourceInterval last between: 1 and: source size ] )
		ifFalse: [ self refactoringFailure: 'Invalid interval' ].
	^ source copyFrom: sourceInterval first to: sourceInterval last
]

{ #category : #printing }
RBExtractToTemporaryRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' extract: '.
	sourceInterval storeOn: aStream.
	aStream
		nextPutAll: ' to: ''';
		nextPutAll: newVariableName;
		nextPutAll: ''' from: #';
		nextPutAll: selector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBExtractToTemporaryRefactoring >> transform [
	self
		insertTemporary;
		compileNewMethod
]

{ #category : #preconditions }
RBExtractToTemporaryRefactoring >> verifySelectedInterval [
	| selectedParseTree selectedSources |
	selectedSources := self selectedSource.
	selectedParseTree := self parserClass
		parseExpression: selectedSources
		onError: [ :message :position | self refactoringFailure: 'Invalid selection' ].
	selectedParseTree isSequence
		ifTrue: [ self refactoringFailure: 'Cannot assign temp to multiple statements' ]
]
