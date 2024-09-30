"
Add a new temporary variable for the value of the selected code. Every place in this method using the same piece of code is replaced by accessing this new temporary variable instead.
As the code is now only evaluated once for initializing the variable value, this refactoring may modify the behavior if the code statements didn't evaluate to the same value on every call.

My preconditions verify that the new temporary name is a valid name and isn't already used (neither a temporary, an instance variable or a class variable).
"
Class {
	#name : #RBExtractToTemporaryTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		#sourceInterval,
		#newVariableName,
		#parseTree
	],
	#category : #'Refactoring2-Transformations-Model-Migrated'
}

{ #category : #api }
RBExtractToTemporaryTransformation class >> extract: anInterval to: aString from: aSelector in: aClass [
	^ self new
		extract: anInterval
		to: aString
		from: aSelector
		in: aClass
]

{ #category : #api }
RBExtractToTemporaryTransformation class >> model: aRBNamespace extract: anInterval to: aString from: aSelector in: aClass [
	^ self new
		model: aRBNamespace;
		extract: anInterval
		to: aString
		from: aSelector
		in: aClass;
		yourself
]

{ #category : #executing }
RBExtractToTemporaryTransformation >> buildTransformations [

	^ self insertTemporary
]

{ #category : #preconditions }
RBExtractToTemporaryTransformation >> checkVariableName [
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
RBExtractToTemporaryTransformation >> constructAssignmentFrom: aNode [
	| valueNode |
	valueNode := RBVariableNode named: newVariableName.
	^RBAssignmentNode variable: valueNode value: aNode
]

{ #category : #api }
RBExtractToTemporaryTransformation >> extract: anInterval to: aString from: aSelector in: aClass [
	class := self model classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval.
	newVariableName := aString
]

{ #category : #executing }
RBExtractToTemporaryTransformation >> insertTemporary [
	^ {RBCustomTransformation
	model: self model
	with: [ :rbMode | | node statementNode nodeReferences  |
	node := self nodeContainedBy: sourceInterval.
	nodeReferences := self referencesTo: node.
	statementNode := node statementNode.
	nodeReferences do: [ :each | each replaceWith: (RBVariableNode named: newVariableName) ].
	statementNode parent
		addNode: (self constructAssignmentFrom: node)
			before:
				(node == statementNode
						ifTrue: [ RBVariableNode named: newVariableName ]
						ifFalse: [ statementNode ]);
		addTemporaryNamed: newVariableName.
		class compileTree: self parseTree]}
]

{ #category : #accessing }
RBExtractToTemporaryTransformation >> nodeContainedBy: anInterval [
	| node |
	node := self parseTree whichNodeIsContainedBy: anInterval.
	(node notNil and: [ node isValue ])
		ifFalse: [ self refactoringFailure: 'Cannot assign to non-value nodes' ].
	^ node
]

{ #category : #accessing }
RBExtractToTemporaryTransformation >> parseTree [

	parseTree
		ifNil: [
			parseTree := class parseTreeForSelector: selector.
			parseTree ifNil: [ self refactoringFailure: 'Could not parse method' ]
		].
	^ parseTree
]

{ #category : #preconditions }
RBExtractToTemporaryTransformation >> preconditions [
	^(RBCondition definesSelector: selector in: class)
		& (RBCondition isValidInstanceVariableName: newVariableName for: class)
			& (RBCondition withBlock:
						[self verifySelectedInterval.
						self checkVariableName.
						true])
]

{ #category : #accessing }
RBExtractToTemporaryTransformation >> referencesTo: aNode [
	| children |
	children := self parseTree body allChildren.
	^ children select: [ :each | each = aNode ]
]

{ #category : #accessing }
RBExtractToTemporaryTransformation >> selectedSource [

	| source |

	source := class sourceCodeFor: selector.
	source ifNil: [ self refactoringFailure: 'Couldn''t find sources' ].
	( ( sourceInterval first between: 1 and: source size )
		and: [ sourceInterval last between: 1 and: source size ] )
		ifFalse: [ self refactoringFailure: 'Invalid interval' ].
	^ source copyFrom: sourceInterval first to: sourceInterval last
]

{ #category : #printing }
RBExtractToTemporaryTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' extract: ';
		nextPutAll: sourceInterval asString ;
		nextPutAll: ' to: ';
		nextPutAll: newVariableName;
		nextPutAll: ' from: ';
		nextPutAll: selector;
		nextPutAll: ' in: ';
		nextPutAll: class name.
	aStream nextPut: $)
]

{ #category : #preconditions }
RBExtractToTemporaryTransformation >> verifySelectedInterval [
	| selectedParseTree selectedSources |
	selectedSources := self selectedSource.
	selectedParseTree := self parserClass
		parseExpression: selectedSources
		onError: [ :message :position | self refactoringFailure: 'Invalid selection' ].
	selectedParseTree isSequence
		ifTrue: [ self refactoringFailure: 'Cannot assign temp to multiple statements' ]
]
