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
		'shouldEscape'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRenameInstanceVariableDriver >> configureRefactoring [

	refactoring := RBUpFrontPreconditionCheckingCompositeRefactoring new
		               model: model;
		               refactorings: {RBRenameInstanceVariableRefactoring variable: oldName class: class};
		               yourself
]

{ #category : 'accessing' }
RBRenameInstanceVariableDriver >> defaultModel [
	^ RBNamespace onEnvironment: RBBrowserEnvironment new
]

{ #category : 'execution' }
RBRenameInstanceVariableDriver >> requestNewNameBasedOn: aName [

	newName := SpRequestDialog new
		           title: 'Please provide a new variable name';
		           label: 'The new name should be unique among the variables in the class hierarchy';
		           text: aName;
		           openModal.

	newName ifNil: [
		shouldEscape := true.
		^ self ].
	newName = oldName ifTrue: [
		shouldEscape := true.
		^ self ].

	refactoring refactorings first newName: newName.
	^ newName
]

{ #category : 'execution' }
RBRenameInstanceVariableDriver >> runRefactoring [

	| preconditionsPass |

	self configureRefactoring.
	shouldEscape := false.
	preconditionsPass := false.

	[ preconditionsPass ] whileFalse: [
		newName := self requestNewNameBasedOn: oldName.
		shouldEscape ifTrue: [ ^ self ].
		refactoring refactorings first newName: newName.
		
		preconditionsPass := true.
		[ refactoring checkApplicabilityPreconditions ]
			on: RBRefactoringError
			do: [ :ex | preconditionsPass := false ] ].

	self applyChanges
]

{ #category : 'initialization' }
RBRenameInstanceVariableDriver >> scopes: aCollection variable: aString for: aClass [ 
	scopes := aCollection.
	model :=  self defaultModel.
	oldName := aString.
	class := aClass
]
