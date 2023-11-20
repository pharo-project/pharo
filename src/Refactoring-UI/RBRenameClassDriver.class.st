Class {
	#name : 'RBRenameClassDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'oldName',
		'newName',
		'shouldEscape'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'instance creation' }
RBRenameClassDriver class >> rename: oldName [
	
	^ self new 
		oldName: oldName;
		yourself
]

{ #category : 'execution' }
RBRenameClassDriver >> applyChanges [

	self openPreviewWithChanges: refactoring privateTransform
]

{ #category : 'resources' }
RBRenameClassDriver >> configureRefactoring [
	
		refactoring := RBRenameClassRefactoring rename: oldName.
]

{ #category : 'initialization' }
RBRenameClassDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'accessing' }
RBRenameClassDriver >> model: aRBNamespace [
	"Pay attention when you specify a model that is a selection of the full environment. 
	Indeed imagine that you have a model of just one package, 
	the precondition may say that the new name is not the one of an already existing global 
	that is defined outside of your model (think of OrderedCollection) in this case
	when the refactoring will check then it will say that the name is ok, and you will destroy the system
	by creating a new class with the same name as an existing one."
	
	model := aRBNamespace
]

{ #category : 'accessing' }
RBRenameClassDriver >> oldName: aString [ 
	oldName := aString
]

{ #category : 'execution' }
RBRenameClassDriver >> requestNewNameBasedOn: aName [
	
	newName := SpRequestDialog new 
							title: 'Please provide a new class name' ;
							label: 'The new name should be valid and not the one of an existing class.';
							text: aName;
							openModal.
						
	newName ifNil: [ shouldEscape := true. ^ self ].
	newName = oldName 
		ifTrue: [ shouldEscape := true. ^ self ].
	
	refactoring newName: newName.
	^ newName
]

{ #category : 'execution' }
RBRenameClassDriver >> runRefactoring [
	
	self configureRefactoring.
	refactoring doesClassToBeRenamedExist
		ifFalse: [ self inform: 'The class ', oldName, ' does not exist.'. ^ self ].
	
	"if we see a metaclass then we could take the instance side"
	refactoring isMetaclass 
		ifTrue: [ self inform: 'Please provide a class and not a metaclass: ', oldName, ' is a metaclass.'. ^ self ].
	"usually we do not spawn a dialog out of the blue but we should ask the application.
	It means that the driver should get the application from the tool that invoked it."

	newName := self requestNewNameBasedOn: oldName.
	shouldEscape ifTrue: [ ^ self ].

	[ refactoring isValidClassName check & refactoring isNotGlobal check ] whileFalse: [ 
			newName := self requestNewNameBasedOn: newName.
			shouldEscape ifTrue: [ ^ self ] ].
	self applyChanges
	
]

{ #category : 'accessing' }
RBRenameClassDriver >> scopes: aScope [

	scopes := aScope
]
