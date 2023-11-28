"
I am an interaction class to rename both instance and class variables
"
Class {
	#name : 'RBRenameVariableDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'variable',
		'newName',
		'oldName',
		'shouldEscape'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'class name' }
RBRenameVariableDriver class >> rename: aClyVariable [

	^ self new
		  variable: aClyVariable;
		  yourself
]

{ #category : 'execution' }
RBRenameVariableDriver >> classRefactoring [
	^ RBRenameInstanceVariableRefactoring
]

{ #category : 'resources' }
RBRenameVariableDriver >> configureRefactoring [

	refactoring := variable createVariableRefactoring:
		               self classRefactoring
]

{ #category : 'accessing' }
RBRenameVariableDriver >> model: aRBBrowserEnvironment [ 
	model := aRBBrowserEnvironment
]

{ #category : 'execution' }
RBRenameVariableDriver >> requestNewNameBasedOn: aName [

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

	refactoring newName: newName.
	^ newName
]

{ #category : 'execution' }
RBRenameVariableDriver >> runRefactoring [

	self configureRefactoring.
	oldName := variable name.
	shouldEscape := false.
	newName := self requestNewNameBasedOn: oldName.
	"class := model classNamed: variable definingClass name."
	shouldEscape ifTrue: [ ^ self ].

	[ refactoring checkInformativePreconditions ] whileFalse: [
		newName := self requestNewNameBasedOn: newName.
		shouldEscape ifTrue: [ ^ self ] ].

	self applyChanges
]

{ #category : 'accessing' }
RBRenameVariableDriver >> scopes: aCollection [ 
	scopes := aCollection
]

{ #category : 'accessing' }
RBRenameVariableDriver >> variable: aClyVariable [ 
	variable := aClyVariable
]
