Class {
	#name : 'ReRenameClassDriver',
	#superclass : 'ReInteractionDriver',
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
ReRenameClassDriver class >> rename: oldName [
	
	^ self new 
		oldName: oldName;
		yourself
]

{ #category : 'resources' }
ReRenameClassDriver >> configureRefactoring [
	
		refactoring := ReRenameClassRefactoring rename: oldName.
]

{ #category : 'initialization' }
ReRenameClassDriver >> initialize [ 
	
	super initialize.
	shouldEscape := false.
]

{ #category : 'accessing' }
ReRenameClassDriver >> model: aRBNamespace [
	"Pay attention when you specify a model that is a selection of the full environment. 
	Indeed imagine that you have a model of just one package, 
	the precondition may say that the new name is not the one of an already existing global 
	that is defined outside of your model (think of OrderedCollection) in this case
	when the refactoring will check then it will say that the name is ok, and you will destroy the system
	by creating a new class with the same name as an existing one."
	
	model := aRBNamespace
]

{ #category : 'accessing' }
ReRenameClassDriver >> oldName: aString [ 
	oldName := aString
]

{ #category : 'execution' }
ReRenameClassDriver >> requestNewNameBasedOn: aName [

	newName := self defaultRequestDialog
		           title: 'Please provide a new class name';
		           label:
			           'The new name should be valid and not the one of an existing class.';
		           validateAnswer: [ :string :presenter |
			           self validateName: string onPresenter: presenter ];
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
ReRenameClassDriver >> runRefactoring [
	
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

{ #category : 'validation' }
ReRenameClassDriver >> validateName: aName onPresenter: presenter [ 
	
	| condition result |
	condition := RBCondition isValidClassName: aName.
	result := condition check.

	result 
		ifFalse: [ presenter setErrorText: condition errorString  ]
		ifTrue: [ presenter setErrorText: '' ].
		
	^ result
]
