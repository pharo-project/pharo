"
I am a refactoring for renaming temporary variables.
This can be applied to method arguments as well.

The variable declaration and all references in this method are renamed.

My precondition verifies that the new name is a valid variable name and not an existing instance or a class variable name
"
Class {
	#name : 'RBRenameArgumentOrTemporaryRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'interval',
		'oldName',
		'newName',
		'parseTree'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBRenameArgumentOrTemporaryRefactoring class >> model: aRBSmalltalk renameTemporaryFrom: anInterval to: newName in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			interval: anInterval
			newName: newName;
		yourself
]

{ #category : 'instance creation' }
RBRenameArgumentOrTemporaryRefactoring class >> renameTemporaryFrom: anInterval to: newName in: aClass selector: aSelector [
	^ self new
		class: aClass
		selector: aSelector
		interval: anInterval
		newName: newName
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition withBlock: [
			   | methodSource |
			   interval first > interval last ifTrue: [
				   self refactoringError: 'You must select a variable name' ].
			   methodSource := class sourceCodeFor: selector.
			   methodSource size >= interval last ifFalse: [
				   self refactoringError: 'Invalid range for variable' ].
			   oldName := methodSource
				              copyFrom: interval first
				              to: interval last.
			   true ]).
		  (RBCondition isValidInstanceVariableName: newName for: class).
		  (RBCondition definesSelector: selector in: class).
		  (RBCondition definesInstanceVariable: newName in: class) not.
		  (RBCondition definesClassVariable: newName in: class) not }
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval newName: aString [
	class := self classObjectFor: aClass.
	selector := aSelector.
	interval := anInterval.
	newName := aString
]

{ #category : 'tranforming' }
RBRenameArgumentOrTemporaryRefactoring >> privateTransform [
	| definingNode variableNode |
	parseTree := class parseTreeForSelector: selector.
	variableNode := self whichVariableNode: parseTree inInterval: interval name: oldName.
	(variableNode isNil or: [ variableNode isVariable not ])
		ifTrue: [ self refactoringError: oldName asString , ' isn''t a valid variable' ].
	variableNode name = oldName
		ifFalse: [ self refactoringError: 'Invalid selection' ].
	definingNode := variableNode whoDefines: oldName.
	definingNode ifNil: [ self refactoringError: oldName asString , ' isn''t defined by the method' ].
	self renameNode: definingNode.
	class compileTree: parseTree
]

{ #category : 'tranforming' }
RBRenameArgumentOrTemporaryRefactoring >> renameNode: aParseTree [
	(aParseTree whoDefines: newName)
		ifNotNil: [ self refactoringError: newName asString , ' is already defined' ].
	(aParseTree allDefinedVariables includes: newName)
		ifTrue: [ self refactoringError: newName asString , ' is already defined' ].
	(self parseTreeRewriterClass rename: oldName to: newName) executeTree: aParseTree
]

{ #category : 'storing' }
RBRenameArgumentOrTemporaryRefactoring >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' renameTemporaryFrom: '.
	interval storeOn: aStream.
	aStream
		nextPutAll: ' to: ''';
		nextPutAll: newName;
		nextPutAll: ''' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector.
	aStream nextPut: $)
]
