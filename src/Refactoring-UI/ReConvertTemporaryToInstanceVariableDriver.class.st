Class {
	#name : 'ReConvertTemporaryToInstanceVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable',
		'selector',
		'subclassesNotDefiningVar',
		'notMultipleTempOccurences',
		'noReadBeforeWritten'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReConvertTemporaryToInstanceVariableDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	subclassesNotDefiningVar check ifFalse: [ items add: (ReBrowseClassReferencesChoice new driver: self) ].
	noReadBeforeWritten check ifFalse: [ items add: (ReBrowseMethodChoice new driver: self) ].
	notMultipleTempOccurences check ifFalse: [
			items add: (ReBrowseMethodsChoice new
					 driver: self;
					 description: self browseMethodsDescription) ].
	^ items
]

{ #category : 'execution' }
ReConvertTemporaryToInstanceVariableDriver >> browseMethod [

	(application toolNamed: #messageList) browse: { (class realClass methodDict at: selector) }
]

{ #category : 'actions' }
ReConvertTemporaryToInstanceVariableDriver >> browseMethods [

	| violators |
	violators := notMultipleTempOccurences violators.
	(application toolNamed: #messageList) browse: (violators collect: [ :v | v compiledMethod ])
]

{ #category : 'description' }
ReConvertTemporaryToInstanceVariableDriver >> browseMethodsDescription [

	^ 'Browse all methods containing a temporary variable with the same name'
]

{ #category : 'actions' }
ReConvertTemporaryToInstanceVariableDriver >> browseReferences [

	(application toolNamed: #browser) browseClasses: (class allSubclasses select: [ :cls | cls hasInstVarNamed: variable ])
]

{ #category : 'instance creation' }
ReConvertTemporaryToInstanceVariableDriver >> class: aClass selector: aSelector variable: aVariable [

	variable := aVariable.
	selector := aSelector.
	class := aClass
]

{ #category : 'configuration' }
ReConvertTemporaryToInstanceVariableDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  extent: 400 @ 1200;
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]

{ #category : 'execution' }
ReConvertTemporaryToInstanceVariableDriver >> handleBreakingChanges [

	self selectDialogForBreakingChanges ifNotNil: [ :dialog | dialog action ]
]

{ #category : 'configuration' }
ReConvertTemporaryToInstanceVariableDriver >> instantiateRefactoring [

	^ RBTemporaryToInstanceVariableRefactoring class: class selector: selector variable: variable
]

{ #category : 'ui - dialogs' }
ReConvertTemporaryToInstanceVariableDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  subclassesNotDefiningVar check ifFalse: [
					  subclassesNotDefiningVar violationMessageOn: stream.
					  stream cr ].
			  notMultipleTempOccurences violationMessageOn: stream.
			  stream cr.
			  noReadBeforeWritten violationMessageOn: stream.
			  stream cr.
			  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'actions' }
ReConvertTemporaryToInstanceVariableDriver >> pushUpInstanceVariableAndRemoveTemp [

	
]

{ #category : 'execution' }
ReConvertTemporaryToInstanceVariableDriver >> runRefactoring [

	self configureRefactoring.
	refactoring failedApplicabilityPreconditions ifNotEmpty: [ :conditions |
			self informConditions: conditions.
			^ false ].
	self setBreakingChangesPreconditions.
	subclassesNotDefiningVar check & notMultipleTempOccurences check & noReadBeforeWritten check
		ifTrue: [ self applyChanges ]
		ifFalse: [ self handleBreakingChanges ]
]

{ #category : 'instance creation' }
ReConvertTemporaryToInstanceVariableDriver >> scopes: aScope class: aClass selector: aSelector variable: aVariable [

	scopes := aScope.
	variable := aVariable.
	selector := aSelector.
	class := aClass
]

{ #category : 'initialization' }
ReConvertTemporaryToInstanceVariableDriver >> setBreakingChangesPreconditions [

	subclassesNotDefiningVar := refactoring preconditionNoSubclassDefinesVar.
	notMultipleTempOccurences := refactoring preconditionNoMultipleTempOccurences.
	noReadBeforeWritten := refactoring preconditionNoReadBeforeWritten
]
