"
I'm a model for user interaction for the rename method refactoring.
"
Class {
	#name : 'RBRenameInstanceVariableDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'oldName',
		'class',
		'newName',
		'shouldEscape',
		'lastName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRenameInstanceVariableDriver >> changes [

	^ refactoring privateTransform; changes
	
]

{ #category : 'execution' }
RBRenameInstanceVariableDriver >> configureRefactoring [

	refactoring := RBRenameInstanceVariableRefactoring
		               variable: oldName
		               class: class
]

{ #category : 'execution' }
RBRenameInstanceVariableDriver >> requestNewNameBasedOn: aName [

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
RBRenameInstanceVariableDriver >> runRefactoring [

	| failedConditions |
	self configureRefactoring.
	
	shouldEscape := false.

	[ newName := self requestNewNameBasedOn: lastName.
	shouldEscape ifTrue: [ ^ self ].
	refactoring newName: newName.
	failedConditions := refactoring failedApplicabilityPreconditions.
	failedConditions isEmpty ]
		whileFalse: [
			failedConditions do: [ :cond | 
				self inform: cond errorString ] ].

	self applyChanges
]

{ #category : 'initialization' }
RBRenameInstanceVariableDriver >> scopes: aCollection variable: aString for: aClass [ 
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	oldName := aString.
	lastName := aString.
	class := aClass
]
