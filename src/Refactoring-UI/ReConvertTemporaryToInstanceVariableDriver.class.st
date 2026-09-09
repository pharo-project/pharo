Class {
	#name : 'ReConvertTemporaryToInstanceVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable',
		'selector',
		'notMultipleTempOccurences',
		'noReadBeforeWritten'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReConvertTemporaryToInstanceVariableDriver class >> driverName [

	^ 'Convert to instance variable'
]

{ #category : 'accessing' }
ReConvertTemporaryToInstanceVariableDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'execution' }
ReConvertTemporaryToInstanceVariableDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
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
		  yourself
]

{ #category : 'initialization' }
ReConvertTemporaryToInstanceVariableDriver >> hasFailedBehaviorPreservingPreconditions [

	notMultipleTempOccurences := refactoring preconditionNoMultipleTempOccurences.
	noReadBeforeWritten := refactoring preconditionNoReadBeforeWritten.
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
ReConvertTemporaryToInstanceVariableDriver >> instantiateRefactoring [

	^ ReTemporaryToInstanceVariableRefactoring class: class selector: selector variable: variable
]

{ #category : 'ui - dialogs' }
ReConvertTemporaryToInstanceVariableDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  notMultipleTempOccurences violationMessageOn: stream.
			  stream cr.
			  noReadBeforeWritten violationMessageOn: stream.
			  stream cr.
			  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'instance creation' }
ReConvertTemporaryToInstanceVariableDriver >> scopes: aScope class: aClass selector: aSelector variable: aVariable [

	scopes := aScope.
	variable := aVariable.
	selector := aSelector.
	class := aClass
]
