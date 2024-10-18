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
RBRenameArgumentOrTemporaryRefactoring class >> renameTemporaryFrom: anInterval in: aClass selector: aSelector [

	^ self new class: aClass selector: aSelector interval: anInterval
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

	^ self applicabilityPreconditionsIndependentOfTheNewName , self applicabilityPreconditionsOfTheNewName
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditionsIndependentOfTheNewName [
	"Those are the preconditions that should be checked before even asking to the user a new name for the temporary."

	^ {
		  (RBCondition withBlock: [
			   | methodSource |
			   interval first > interval last ifTrue: [ self refactoringError: 'You must select a variable name' ].
			   methodSource := class sourceCodeFor: selector.
			   methodSource size >= interval last ifFalse: [ self refactoringError: 'Invalid range for variable' ].
			   oldName := methodSource copyFrom: interval first to: interval last.
			   true ]).

		  (RBCondition definesSelector: selector in: class) }
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditionsOfTheNewName [
	"Those are the preconditions that should be checked after asking to the user a new name for the temporary."

	^ {
		  (RBCondition isValidInstanceVariableName: newName for: class).

		  (ReCheckVariableNameCondition class: class variableName: newName parseTree: (class parseTreeForSelector: selector)) }
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval [

	class := self classObjectFor: aClass.
	selector := aSelector.
	interval := anInterval
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval newName: aString [
	class := self classObjectFor: aClass.
	selector := aSelector.
	interval := anInterval.
	newName := aString
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> newName: aString [

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

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> sourceClass [

	^ class
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
