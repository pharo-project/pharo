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
		'parseTree',
		'definingNode'
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

	^ { (RBCondition definesSelector: selector in: class) }
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditionsOfTheNewName [
	"Those are the preconditions that should be checked after asking to the user a new name for the temporary."

	^ {
		  (RBCondition isValidInstanceVariableName: newName for: class).

		  (ReCheckVariableNameCondition class: class variableName: newName parseTree: parseTree) }
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval [

	class := self classObjectFor: aClass.
	selector := aSelector.
	interval := anInterval
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval newName: aString [

	self class: aClass selector: aSelector interval: anInterval.
	newName := aString
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> getExistingNameFromInterval [

	| methodSource |
	interval first > interval last ifTrue: [
		self refactoringError: 'You must select a variable name' ].
	methodSource := class sourceCodeFor: selector.
	methodSource size >= interval last ifFalse: [
		self refactoringError: 'Invalid range for variable' ].
	^ methodSource copyFrom: interval first to: interval last.
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> newName: aString [

	newName := aString
]

{ #category : 'transforming' }
RBRenameArgumentOrTemporaryRefactoring >> prepareForExecution [

	| variableNode |
	oldName := self getExistingNameFromInterval.
	parseTree := class parseTreeForSelector: selector.
	variableNode := [ self
		                whichVariableNode: parseTree
		                inInterval: interval
		                name: oldName. ]
		on: OCCodeError 
		do: [ self refactoringError: 'Selected code does not contain any variables or contains multiple of them.' ].
	(variableNode isNil or: [ variableNode isVariable not ]) ifTrue: [
		self refactoringError: oldName asString , ' isn''t a valid variable' ].
	variableNode name = oldName ifFalse: [
		self refactoringError: 'Invalid selection' ].
	definingNode := variableNode whoDefines: oldName.
	definingNode ifNil: [
		self refactoringError:
			oldName asString , ' isn''t defined by the method' ]
]

{ #category : 'transforming' }
RBRenameArgumentOrTemporaryRefactoring >> privateTransform [

	self renameNode: definingNode.
	class compileTree: parseTree
]

{ #category : 'transforming' }
RBRenameArgumentOrTemporaryRefactoring >> renameNode: aParseTree [

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
