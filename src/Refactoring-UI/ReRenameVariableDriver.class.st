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

{ #category : 'testing' }
ReRenameVariableDriver class >> isAbstract [

	^ self = ReRenameVariableDriver
]

{ #category : 'accessing' }
ReRenameVariableDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'private' }
ReRenameVariableDriver >> gatherUserInput [

	| failedConditions newName |
	refactoring preconditionDirectlyDefinesVariable check ifFalse: [ ^ false ].
	[
		shouldEscape := false.
		newName := self requestNewNameBasedOn: previouslyProposedName.
		shouldEscape ifTrue: [ ^ false ].
		refactoring newName: newName.
		failedConditions := refactoring failedApplicabilityPreconditions.
		failedConditions isEmpty ] whileFalse: [ failedConditions do: [ :cond | self informError: cond ] ].

	^ true
]

{ #category : 'private' }
ReRenameVariableDriver >> informError: cond [

	self informDialog
		message: cond errorString;
		title: 'Invalid name';
		openModal
]

{ #category : 'configuration' }
ReRenameVariableDriver >> instantiateRefactoring [

	^ self refactoringClass 
		model: model 
		variable: oldName 
		class: class
]

{ #category : 'factory method' }
ReRenameVariableDriver >> refactoringClass [
	self subclassResponsibility 
]

{ #category : 'private' }
ReRenameVariableDriver >> requestNewNameBasedOn: aName [

	| newName |
	newName := self requestDialog
		           title: 'Rename variable';
		           message: 'Please provide a new variable name';
		           text: aName;
		           openModal.

	(newName isNil or: [ newName = oldName ]) ifTrue: [
			shouldEscape := true.
			^ self ].

	previouslyProposedName := newName.

	^ newName
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
