"
I'm an abstract model for user interaction for the rename variables refactoring.
"
Class {
	#name : 'RBRenameVariableDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'oldName',
		'newName',
		'class',
		'shouldEscape',
		'previouslyProposedName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

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

{ #category : 'private' }
RBRenameVariableDriver >> informError: cond [

	self informDialog
		label: cond errorString;
		title: 'Invalid name';
		openModal
]

{ #category : 'private' }
RBRenameVariableDriver >> prepareRefactoringInteractively [

	| failedConditions |
	[
	newName := self requestNewNameBasedOn: previouslyProposedName.
	shouldEscape ifTrue: [ ^ self ].
	refactoring newName: newName.
	failedConditions := refactoring failedApplicabilityPreconditions.
	failedConditions isEmpty ] whileFalse: [
		failedConditions do: [ :cond |
			self informError: cond 
			 ] ]
]

{ #category : 'factory method' }
RBRenameVariableDriver >> refactoringClass [
	self subclassResponsibility 
]

{ #category : 'private' }
RBRenameVariableDriver >> requestNewNameBasedOn: aName [

	newName := self requestDialog
		           title: 'Please provide a new variable name';
		           text: aName;
		           openModal.

	newName ifNil: [
		shouldEscape := true.
		^ self ].
	newName = oldName ifTrue: [
		shouldEscape := true.
		^ self ].
	previouslyProposedName := newName.
	^ newName
]

{ #category : 'execution' }
RBRenameVariableDriver >> runRefactoring [

	self configureRefactoring.
	shouldEscape := false.

	self prepareRefactoringInteractively.
	shouldEscape ifTrue: [ ^ self ].

	self applyChanges
]

{ #category : 'initialization' }
RBRenameVariableDriver >> scopes: aCollection variable: aString for: aClass [ 
	"The driver is initialized in interactive mode (i.e. we will ask for the newName to the user while running the refactoring"
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	oldName := aString.
	previouslyProposedName := aString.
	class := aClass
]

{ #category : 'initialization' }
RBRenameVariableDriver >> scopes: aCollection variable: aString for: aClass newName: aString2 [ 
	"The driver is initialized in script mode (i.e. we won't ask for the newName to the user while running the refactoring"
	self scopes: aCollection variable: aString for: aClass.
	newName := aString2
]
