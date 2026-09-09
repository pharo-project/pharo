Class {
	#name : 'ReRenameClassDriver2',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'oldName',
		'newName',
		'hasShadowing'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenameClassDriver2 class >> driverName [

	^ 'Rename class'
]

{ #category : 'instance creation' }
ReRenameClassDriver2 class >> rename: oldName [
	
	^ self new 
		oldName: oldName;
		yourself
]

{ #category : 'actions' }
ReRenameClassDriver2 >> browseMethods [

	| references |
	references := hasShadowing violator .
	
	(Smalltalk tools toolNamed: #messageList)
		browse: (references collect: [ :ref | ref origin realClass lookupSelector: ref selector ])
		title: 'Methods containing references to a shared var that will shadow class reference after renaming' 
]

{ #category : 'actions' }
ReRenameClassDriver2 >> checkApplicabilityPreconditions [

	| failedCondition |
	
	refactoring newName: newName.
	
	failedCondition := refactoring applicabilityPreconditions
		detect: [ :condition | condition check not ]
		ifNone: [ nil ].
		
	^ failedCondition ifNotNil: [ :condition | condition errorString ]
	
]

{ #category : 'actions' }
ReRenameClassDriver2 >> checkBehaviorPreconditions [

	| stream | 
	
	hasShadowing := self refactoring breakingChangePreconditions first.
	hasShadowing check ifTrue: [ ^ nil ].
	
	stream := String new writeStream.
	hasShadowing violationMessageOn: stream.
	^ stream contents
]

{ #category : 'actions' }
ReRenameClassDriver2 >> createAndSetNewRefactoring [

	super configureRefactoring .
	refactoring newName: newName
]

{ #category : 'actions' }
ReRenameClassDriver2 >> hasFailedBehaviorPreservingPreconditions [

	^ self refactoring breakingChangePreconditions
		anySatisfy: [ :each | each check not ]
]

{ #category : 'initialization' }
ReRenameClassDriver2 >> initialize [

	super initialize
]

{ #category : 'configuration' }
ReRenameClassDriver2 >> instantiateRefactoring [

	^ ReRenameClassRefactoring rename: oldName
]

{ #category : 'accessing' }
ReRenameClassDriver2 >> model: aRBNamespace [
	"Pay attention when you specify a model that is a selection of the full environment. 
	Indeed imagine that you have a model of just one package, 
	the precondition may say that the new name is not the one of an already existing global 
	that is defined outside of your model (think of OrderedCollection) in this case
	when the refactoring will check then it will say that the name is ok, and you will destroy the system
	by creating a new class with the same name as an existing one."
	
	model := aRBNamespace
]

{ #category : 'accessing' }
ReRenameClassDriver2 >> newName [
	^ newName
]

{ #category : 'accessing' }
ReRenameClassDriver2 >> newName: aString [ 
	newName := aString
]

{ #category : 'accessing' }
ReRenameClassDriver2 >> oldName: aString [ 
	oldName := aString
]

{ #category : 'validation' }
ReRenameClassDriver2 >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRenameClassPreviewPresenter ]
]

{ #category : 'actions' }
ReRenameClassDriver2 >> renameClass [

	self applyChanges 
]

{ #category : 'accessing' }
ReRenameClassDriver2 >> runRefactoring [

	| presenter |
	
	super configureRefactoring .
	
	presenter := (self previewPresenterClass newApplication: self application)
		scopes: scopes;
		driver: self;
		name: oldName;
		openModal
]
