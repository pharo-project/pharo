Class {
	#name : #RBExtractToTemporaryTransformation,
	#superclass : #RBCompositeMethodTransformation,
	#instVars : [
		'sourceInterval',
		'newVariableName',
		'parseTree'
	],
	#category : #'Refactoring2-Transformations-Model-Migration'
}

{ #category : #'instance creation' }
RBExtractToTemporaryTransformation class >> extract: anInterval to: aString from: aSelector in: aClass [ 
	^self new 
		extract: anInterval
		to: aString
		from: aSelector
		in: aClass
]

{ #category : #'instance creation' }
RBExtractToTemporaryTransformation class >> model: aRBSmalltalk extract: anInterval to: aString from: aSelector in: aClass [ 
	^ self new
		model: aRBSmalltalk;
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

{ #category : #'building actions' }
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

{ #category : #preconditions }
RBExtractToTemporaryTransformation >> parseTree [

	parseTree
		ifNil: [ 
			parseTree := class parseTreeFor: selector.
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
	^ children select: [ :each | each = aNode ].
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
