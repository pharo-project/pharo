"
I represent a driver that invokes Inline Method refactoring, processing user interaction during the execution.
"
Class {
	#name : 'ReInlineMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'selector',
		'interval',
		'noOverridden',
		'notExecutionOrder',
		'noSuperMessage'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReInlineMethodDriver class >> driverName [

	^ 'Inline method'
]

{ #category : 'execution' }
ReInlineMethodDriver >> applyChanges [

	self isLastSender ifTrue: [
			| dialog selection |
			dialog := self lastSenderSelectDialog.
			selection := dialog openModal.
			selection ifNil: [ ^ false ].
			selection action.
			^ self ].
	super applyChanges
]

{ #category : 'execution' }
ReInlineMethodDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.

	(noOverridden check and: [ notExecutionOrder check and: [ noSuperMessage check ] ]) ifFalse: [
		items add: (ReInlineMethodChoice new driver: self) ].
	self isLastSender ifTrue: [ items add: (ReInlineAndRemoveMethodChoice new driver: self) ].


	^ items
]

{ #category : 'configuration' }
ReInlineMethodDriver >> defaultSelectDialog [

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
ReInlineMethodDriver >> hasFailedBehaviorPreservingPreconditions [

	noOverridden := refactoring preconditionIsMethodOverridden.
	notExecutionOrder := refactoring preconditionExecutionOrder.
	noSuperMessage := refactoring preconditionSuperMessages.


	^ refactoring failedBreakingChangePreconditions isNotEmpty
]

{ #category : 'action' }
ReInlineMethodDriver >> inlineAndRemoveMethod [

	| removeRefactoring |
	refactoring privateTransform.
	removeRefactoring := ReRemoveMethodsRefactoring model: model classSelectorMapping: { (class -> selector) }.
	removeRefactoring privateTransform.
	removeRefactoring changes changes do: [ :change | refactoring changes addChange: change ].
	self openPreviewWithChanges: refactoring changes
]

{ #category : 'action' }
ReInlineMethodDriver >> inlineMethod [

	refactoring privateTransform.
	self openPreviewWithChanges: refactoring changes
]

{ #category : 'configuration' }
ReInlineMethodDriver >> instantiateRefactoring [

	^ RBInlineMethodRefactoring inline: interval inMethod: selector forClass: class
]

{ #category : 'testing' }
ReInlineMethodDriver >> isLastSender [

	^ (refactoring model allReferencesTo: refactoring inlineSelector) size <= 1
]

{ #category : 'ui - dialogs' }
ReInlineMethodDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  refactoring failedBreakingChangePreconditions do: [ :cond |
					  cond violationMessageOn: stream.
					  stream cr ].
			  self isLastSender ifTrue: [
					  stream
						  nextPutAll: 'This is the last sender of ';
						  nextPutAll: refactoring inlineSelector;
						  nextPutAll: '.' ].
			  stream cr.
			  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'configuration' }
ReInlineMethodDriver >> lastSenderSelectDialog [

	^ super defaultSelectDialog
		  extent: 400 @ 300;
		  title: 'Last sender detected';
		  message: 'The method ' , refactoring inlineSelector , ' has no other senders. Do you want to remove it after inlining?';
		  items: {
				  (ReInlineMethodChoice new driver: self).
				  (ReInlineAndRemoveMethodChoice new driver: self) };
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'instance creation' }
ReInlineMethodDriver >> scopes: aScope interval: anInterval selector: aSelector class: aClass [

	scopes := aScope.
	model := self refactoringScopeOn: scopes first.
	interval := anInterval.
	selector := aSelector.
	class := aClass
]
