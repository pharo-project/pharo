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

{ #category : 'accessing' }
ReRenameClassDriver class >> driverName [

	^ 'Rename class'
]

{ #category : 'instance creation' }
ReRenameClassDriver class >> rename: oldName [
	
	^ self new 
		oldName: oldName;
		yourself
]

{ #category : 'execution' }
ReRenameClassDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReRenameClassChoice new driver: self).
	items add: (ReBrowseMethodsChoice new driver: self).
	^ items
]

{ #category : 'actions' }
ReRenameClassDriver >> browseMethods [

	| references |
	references := refactoring violations.
	(Smalltalk tools toolNamed: #messageList)
		browse: (references collect: [ :ref | ref origin realClass lookupSelector: ref selector ])
		title: 'Methods containing references to a shared var that will shadow class reference after renaming' 
]

{ #category : 'execution' }
ReRenameClassDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		title: 'There are potential breaking changes!';
		message: 'Watchout potential reference shadowing detected!';
		items: self breakingChoices;
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		yourself
]

{ #category : 'execution' }
ReRenameClassDriver >> gatherUserInput [ 
	"usually we do not spawn a dialog out of the blue but we should ask the application.
	It means that the driver should get the application from the tool that invoked it."
	newName := self requestNewNameBasedOn: oldName.
	shouldEscape ifTrue: [ ^ false "user wants to exit" ].

	[ refactoring isValidClassName check & refactoring isNotGlobal check ] whileFalse: [ 
			newName := self requestNewNameBasedOn: newName.
			shouldEscape ifTrue: [ ^ false ] ].
	^ true
]

{ #category : 'initialization' }
ReRenameClassDriver >> initialize [

	super initialize.
	shouldEscape := false
]

{ #category : 'configuration' }
ReRenameClassDriver >> instantiateRefactoring [

	^ ReRenameClassRefactoring rename: oldName
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

{ #category : 'actions' }
ReRenameClassDriver >> renameClass [

	self applyChanges 
]

{ #category : 'execution' }
ReRenameClassDriver >> requestNewNameBasedOn: aName [

	newName := self requestDialog 
		title: 'Please provide a new class name';
		message: 'The new name should be valid and not the one of an existing class.';
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

{ #category : 'validation' }
ReRenameClassDriver >> validateName: aName onPresenter: presenter [

	| condition result |
	condition := RBCondition isValidClassName: aName.
	result := condition check.

	result
		ifFalse: [ presenter setErrorText: condition errorString ]
		ifTrue: [ presenter errorText: '' ].

	^ result
]
