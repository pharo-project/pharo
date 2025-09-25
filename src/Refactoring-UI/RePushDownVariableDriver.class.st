Class {
	#name : 'RePushDownVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable',
		'isSharedVariable',
		'hasNoReferences'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RePushDownVariableDriver >> breakingChoices [

	^ {
		  (ReVariableChoice new
			   driver: self;
			   actionBlock: [ self applyChanges ];
			   description: 'Push shared variable down';
			   yourself).
		  (ReBrowseReferencedSharedVariableChoice new
			   driver: self;
			   description: 'Browse shared variable references';
			   yourself).
		  (ReVariableChoice new
			   driver: self;
			   actionBlock: [
					   self applyChanges.
					   self browseSharedVariableReferences ];
			   description: 'Push shared variable down and browse broken references';
			   yourself) }
]

{ #category : 'execution' }
RePushDownVariableDriver >> browseSharedVariableReferences [
	| methods |
	methods := hasNoReferences violators collect: [ :each |
		           class methodDictionary at: each ifAbsent: [ class classSide methodDictionary at: each ] ].

	StMessageBrowser browse: methods title: 'Methods referring to #' , variable highlight: variable asString
]

{ #category : 'resources' }
RePushDownVariableDriver >> configureRefactoring [

	" we should manage multiple instance variables"
	
	refactoring := self refactoringClass
		               model: model
		               variable: variable
		               class: class
]

{ #category : 'configuration' }
RePushDownVariableDriver >> defaultSelectDialog [

	^ self class defaultSelectDialog
		  title: 'Warning you may break your code';
		  label:
			  'The variable you want to push down is referenced from methods in its defining class!';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]

{ #category : 'factory method' }
RePushDownVariableDriver >> refactoringClass [

	^ isSharedVariable
		  ifTrue: [ RePushDownSharedVariableRefactoring ]
		  ifFalse: [ self error: 'Not implemented yet' ]
]

{ #category : 'execution' }
RePushDownVariableDriver >> runRefactoring [

	self configureRefactoring.
	self failedPreconditionsErrorString 
		ifNotEmpty: [:message | ^ self inform: message ].
	
	hasNoReferences := refactoring preconditionHasNoReferences.
	hasNoReferences check
			ifTrue: [ self applyChanges ]
			ifFalse: [ self handleBreakingChanges ]
]

{ #category : 'factory method' }
RePushDownVariableDriver >> scopes: aCollection variable: varName shared: isSharedVar from: aClass [ 

	"The driver is initialized in interactive mode (i.e. we will ask for the newName to the user while running the refactoring"
	
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	class := aClass.
	variable := varName.
	isSharedVariable := isSharedVar
]
