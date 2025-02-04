"
I'm an abstract model for user interaction for the rename variables refactoring.
"
Class {
	#name : 'ReRenameVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'oldName',
		'class',
		'shouldEscape',
		'previouslyProposedName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReRenameVariableDriver >> configureRefactoring [

	refactoring := self refactoringClass
		               model: model
		               variable: oldName
		               class: class
]

{ #category : 'private' }
ReRenameVariableDriver >> informError: cond [

	self informDialog
		label: cond errorString;
		title: 'Invalid name';
		openModal
]

{ #category : 'private' }
ReRenameVariableDriver >> prepareRefactoringInteractively [

	| failedConditions newName |
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
ReRenameVariableDriver >> refactoringClass [
	self subclassResponsibility 
]

{ #category : 'private' }
ReRenameVariableDriver >> requestNewNameBasedOn: aName [

	| newName |
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
ReRenameVariableDriver >> runRefactoring [

	self configureRefactoring.
	shouldEscape := false.
	"We cannot rename something that doesn'e exist.
	If we don't check this before `prepareRefactoringInteractively` 
	it will loop indefinetely since this precondition will always fail."
	refactoring preconditionDirectlyDefinesVariable check ifFalse: [ ^ self ].
	self prepareRefactoringInteractively.
	shouldEscape ifTrue: [ ^ self ].

	self applyChanges
]

{ #category : 'initialization' }
ReRenameVariableDriver >> scopes: aCollection variable: aString for: aClass [ 
	"The driver is initialized in interactive mode (i.e. we will ask for the newName to the user while running the refactoring"
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	oldName := aString.
	previouslyProposedName := aString.
	class := aClass
]
