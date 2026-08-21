"
I am a refactoring to replace a temporary variable by code.

All references to the temporary variable in this method are replaced by the value used to initialize the temporary variable. 
The initialization and declaration of this variable will be removed. You need to select the variable and its initial assignment code to apply this refactoring.
"
Class {
	#name : 'ReInlineTemporaryRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'sourceInterval',
		'selector',
		'sourceTree',
		'assignmentNode',
		'definingNode'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
ReInlineTemporaryRefactoring class >> inline: anInterval from: aSelector in: aClass [
	^ self new
		inline: anInterval
		from: aSelector
		in: aClass
]

{ #category : 'instance creation' }
ReInlineTemporaryRefactoring class >> model: aRBSmalltalk inline: anInterval from: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		inline: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : 'preconditions' }
ReInlineTemporaryRefactoring >> applicabilityPreconditions [

	^ {
		  (ReDefinesSelectorsCondition new definesSelectors: (Array with: selector) in: class).
		  (ReIsValidInstanceVariableName new name: assignmentNode variable name).
		  (ReSingleAssignmentCondition new
			   variableName: assignmentNode variable name;
			   definingNode: sourceTree).
		  (ReSingleReadCondition new
			   variableName: assignmentNode variable name;
			   definingNode: sourceTree).
		  (ReVariablesNotReadBeforeWrittenCondition new
			   variables: { assignmentNode variable name };
			   subtree: sourceTree) }
]

{ #category : 'transforming' }
ReInlineTemporaryRefactoring >> compileMethod [
	class compileTree: sourceTree
]

{ #category : 'initialization' }
ReInlineTemporaryRefactoring >> inline: anInterval from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	sourceInterval := anInterval
]

{ #category : 'transforming' }
ReInlineTemporaryRefactoring >> prepareForExecution [
	
	sourceTree := class parseTreeForSelector: selector.
	sourceTree ifNil: [ self refactoringError: 'Could not parse source' ].
	
	assignmentNode := sourceTree whichNodeIsContainedBy: sourceInterval.
	assignmentNode isAssignment
		ifFalse: [ self refactoringError: 'The selected node is not an assignment statement' ].
		
	definingNode := assignmentNode whichUpNodeDefines:
		                assignmentNode variable name.
]

{ #category : 'transforming' }
ReInlineTemporaryRefactoring >> privateTransform [
	self
		replaceAssignment;
		replaceReferences;
		compileMethod
]

{ #category : 'transforming' }
ReInlineTemporaryRefactoring >> replaceAssignment [
	assignmentNode parent isSequence
		ifTrue: [assignmentNode parent removeNode: assignmentNode]
		ifFalse: [assignmentNode replaceWith: assignmentNode value]
]

{ #category : 'transforming' }
ReInlineTemporaryRefactoring >> replaceReferences [
	| rewriter |
	rewriter := self parseTreeRewriter.
	rewriter replaceTree: assignmentNode variable
		withTree: assignmentNode value.
	definingNode removeTemporaryNamed: assignmentNode variable name.
	rewriter executeTree: definingNode
]
