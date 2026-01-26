"
Add a new temporary variable for the value of the selected code. Every place in this method using the same piece of code is replaced by accessing this new temporary variable instead.
As the code is now only evaluated once for initializing the variable value, this refactoring may modify the behavior if the code statements didn't evaluate to the same value on every call.

My preconditions verify that the new temporary name is a valid name and isn't already used (neither a temporary, an instance variable or a class variable).
"
Class {
	#name : 'RBExtractToTemporaryRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'sourceInterval',
		'selector',
		'newVariableName',
		'parseTree',
		'selectedParseTree'
	],
	#category : 'Refactoring-Core-Refactorings-Unused',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings-Unused'
}

{ #category : 'instance creation' }
RBExtractToTemporaryRefactoring class >> extract: anInterval from: aSelector in: aClass [
	^ self new
		extract: anInterval
		from: aSelector
		in: aClass
]

{ #category : 'instance creation' }
RBExtractToTemporaryRefactoring class >> extract: anInterval to: aString from: aSelector in: aClass [
	^ self new
		extract: anInterval
		to: aString
		from: aSelector
		in: aClass
]

{ #category : 'instance creation' }
RBExtractToTemporaryRefactoring class >> model: aRBSmalltalk extract: anInterval from: aSelector in: aClass [

	^ self new
		  model: aRBSmalltalk;
		  extract: anInterval from: aSelector in: aClass;
		  yourself
]

{ #category : 'instance creation' }
RBExtractToTemporaryRefactoring class >> model: aRBSmalltalk extract: anInterval to: aString from: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		extract: anInterval
			to: aString
			from: aSelector
			in: aClass;
		yourself
]

{ #category : 'preconditions' }
RBExtractToTemporaryRefactoring >> applicabilityPreconditions [

	^ {
			(ReDefinesSelectorsCondition new definesSelectors: (Array with: selector) in: class).
		  	(ReIsValidInstanceVariableName new name: newVariableName).
			(ReCheckVariableNameCondition
				class: class
				variableName: newVariableName 
				parseTree: parseTree ).
		}
]

{ #category : 'transforming' }
RBExtractToTemporaryRefactoring >> compileNewMethod [
	class compileTree: parseTree
]

{ #category : 'transforming' }
RBExtractToTemporaryRefactoring >> constructAssignmentFrom: aNode [
	| valueNode |
	valueNode := OCVariableNode named: newVariableName.
	"Use a copy so aNode retains its original parent.
	In this case the caller is done making use of the parent backlink,
	but it seems sensible to keep it intact whenever possible."
	^OCAssignmentNode variable: valueNode value: aNode copy.
]

{ #category : 'initialization' }
RBExtractToTemporaryRefactoring >> extract: anInterval from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval
]

{ #category : 'initialization' }
RBExtractToTemporaryRefactoring >> extract: anInterval to: aString from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval.
	newVariableName := aString
]

{ #category : 'transforming' }
RBExtractToTemporaryRefactoring >> insertTemporary [

	| node statementNode nodeReferences scope |
	node := parseTree whichNodeIsContainedBy: sourceInterval.
	(node isNotNil and: [ node isValue ]) ifFalse: [
		self refactoringError: 'Cannot assign to non-value nodes' ].
	scope := node methodOrBlockNode.
	nodeReferences := scope allChildren select: [ :each | each = node ].
	"Insert the assignment before the first occurrence to be replaced,
	but in the same scope as the selected expression,
	even if that first occurrence is in a nested block."
	statementNode := nodeReferences first statementNodeIn: scope.
	scope body
		addNode: (self constructAssignmentFrom: node) before: statementNode;
		addTemporaryNamed: newVariableName.
	nodeReferences do: [ :each |
		each replaceWith: (OCVariableNode named: newVariableName) ]
]

{ #category : 'transforming' }
RBExtractToTemporaryRefactoring >> prepareForExecution [

	| selectedSources |
	selectedSources := self selectedSource.
	selectedParseTree := self parserClass
		parseExpression: selectedSources
		onError: [ :message :position | self refactoringError: 'Invalid selection' ].
	selectedParseTree isSequence
		ifTrue: [ self refactoringError: 'Cannot assign temp to multiple statements' ].	
	parseTree ifNil: [ 
			parseTree := class parseTreeForSelector: selector.
			parseTree ifNil: [ self refactoringError: 'Could not parse method' ]
			].
		parseTree doSemanticAnalysis


]

{ #category : 'transforming' }
RBExtractToTemporaryRefactoring >> privateTransform [
	self
		insertTemporary;
		compileNewMethod
]

{ #category : 'private - accessing' }
RBExtractToTemporaryRefactoring >> selectedSource [

	| source |
	source := class sourceCodeFor: selector.
	source ifNil: [ self refactoringError: 'Couldn''t find sources' ].
	( ( sourceInterval first between: 1 and: source size )
		and: [ sourceInterval last between: 1 and: source size ] )
		ifFalse: [ self refactoringError: 'Invalid interval' ].
	^ source copyFrom: sourceInterval first to: sourceInterval last
]

{ #category : 'storing' }
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

{ #category : 'initialization' }
RBExtractToTemporaryRefactoring >> variableName: aString [

	newVariableName := aString
]
