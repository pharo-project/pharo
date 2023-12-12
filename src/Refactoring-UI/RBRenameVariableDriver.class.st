"
I'm an abstract model for user interaction for the rename variables refactoring.
"
Class {
	#name : 'RBRenameVariableDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'oldName',
		'newName',
		'lastName',
		'class',
		'shouldEscape'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRenameVariableDriver >> applyChanges [
	"I use the preview presenter to perform changes, but I do not open it"
	(self previewWithChanges: self changes) performChanges
]

{ #category : 'execution' }
RBRenameVariableDriver >> changes [

	refactoring changes changes isEmpty ifTrue: [
		refactoring privateTransform ].
	^ refactoring changes
]

{ #category : 'execution' }
RBRenameVariableDriver >> configureRefactoring [

	refactoring := self refactoringClass
		               variable: oldName
		               class: class
]

{ #category : 'accessing' }
RBRenameVariableDriver >> newName: aString [
	"The only way to define newName is either using the UI (interactive mode) or using the initialization method >>scopes:variable:for:newName:"
	self shouldNotImplement 
]

{ #category : 'private' }
RBRenameVariableDriver >> prepareRefactoring [
	"The Driver is run in scripting mode, therefore the variable `newName` was defined in the method scopes:variable:for:newName:.
	We don't need to ask for the name to the user"
	refactoring newName: newName.
	refactoring checkApplicabilityPreconditions
]

{ #category : 'private' }
RBRenameVariableDriver >> prepareRefactoringInteractively [
	| failedConditions |
	
	"We ask the user for the variable new name"
	[ newName := self requestNewNameBasedOn: lastName.
	shouldEscape ifTrue: [ ^ self ].
	refactoring newName: newName.
	failedConditions := refactoring failedApplicabilityPreconditions.
	failedConditions isEmpty ]
		whileFalse: [
			failedConditions do: [ :cond |
				self application newInform
					label: cond errorString;
					title: 'Invalid name';
					openModal ] ].
]

{ #category : 'factory method' }
RBRenameVariableDriver >> refactoringClass [
	self subclassResponsibility 
]

{ #category : 'private' }
RBRenameVariableDriver >> requestNewNameBasedOn: aName [

	newName := SpRequestDialog new
		           title: 'Please provide a new variable name';
		           text: aName;
		           openModal.

	newName ifNil: [
		shouldEscape := true.
		^ self ].
	newName = oldName ifTrue: [
		shouldEscape := true.
		^ self ].
	lastName := newName.
	^ newName
]

{ #category : 'execution' }
RBRenameVariableDriver >> runRefactoring [

	self configureRefactoring.
	shouldEscape := false.
	
	newName 
		ifNil: [ 
			"run in interactive mode"
			self prepareRefactoringInteractively
		 ]
		ifNotNil: [ 
			"run in script mode"
			self prepareRefactoring
		 ].
	shouldEscape ifTrue: [ ^ self ].
	
	self applyChanges
]

{ #category : 'initialization' }
RBRenameVariableDriver >> scopes: aCollection variable: aString for: aClass [ 
	"The driver is initialized in interactive mode (i.e. we will ask for the newName to the user while running the refactoring"
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	oldName := aString.
	lastName := aString.
	class := aClass
]

{ #category : 'initialization' }
RBRenameVariableDriver >> scopes: aCollection variable: aString for: aClass newName: aString2 [ 
	"The driver is initialized in script mode (i.e. we won't ask for the newName to the user while running the refactoring"
	self scopes: aCollection variable: aString for: aClass.
	newName := aString2
]
