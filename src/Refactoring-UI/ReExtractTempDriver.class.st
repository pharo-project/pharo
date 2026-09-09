Class {
	#name : 'ReExtractTempDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'interval',
		'selector',
		'class',
		'notCheckVarName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReExtractTempDriver class >> driverName [

	^ 'Extract to temporary'
]

{ #category : 'execution' }
ReExtractTempDriver >> breakingChoices [

	| choices |
	choices := OrderedCollection new.
	notCheckVarName isFalse ifTrue: [
			choices add: (ReRenameTemporaryVariableChoice new driver: self).
			choices add: (ReNewVariableNameChoice new driver: self).
			choices add: (ReConvertTemporaryToExistingVariableChoice new driver: self) ].


	^ choices
]

{ #category : 'action' }
ReExtractTempDriver >> convertTemporaryToExistingVariable [

	| transformation |
	self applyChanges.
	transformation := RBRemoveTemporaryVariableTransformation
		                  variable: notCheckVarName violators first
		                  inMethod: (class compiledMethodAt: selector) selector
		                  inClass: class name.
	transformation privateTransform.
	self openPreviewWithChanges: transformation changes
]

{ #category : 'configuration' }
ReExtractTempDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'instance creation' }
ReExtractTempDriver >> extract: anInterval from: aSelector in: aClass [

	interval := anInterval.
	selector := aSelector.
	class := aClass
]

{ #category : 'execution' }
ReExtractTempDriver >> gatherUserInput [

	| varName failed errorString |
	varName := ''.
	failed := nil.
	[
		errorString := failed ifNil: [ '' ] ifNotNil: [ self failedPreconditionsErrorString: failed ].
		varName := self getVariableName: varName errorMessage: errorString.
		varName ifNil: [ ^ false ].
		refactoring variableName: varName.
		failed := refactoring failedApplicabilityPreconditions ] doWhileFalse: [ varName isEmptyOrNil not and: [ failed isEmpty ] ].
	^ true
]

{ #category : 'user requests' }
ReExtractTempDriver >> getVariableName: initialAnswer errorMessage: message [

	^ self requestDialog
		  title: 'Extract code to temp variable';
		  message: 'New temp name: ' , message;
		  text: initialAnswer;
		  acceptLabel: 'Ok!';
		  cancelLabel: 'Cancel';
		  onAccept: [ :dialog | dialog presenter text ];
		  openModal
]

{ #category : 'initialization' }
ReExtractTempDriver >> hasFailedBehaviorPreservingPreconditions [

	notCheckVarName := refactoring preconditionCheckVariableName.
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
ReExtractTempDriver >> instantiateRefactoring [

	^ RBExtractToTemporaryRefactoring
		extract: interval
		from: selector 
		in: class
]

{ #category : 'ui - dialogs' }
ReExtractTempDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  refactoring failedBreakingChangePreconditions do: [ :cond |
					  cond check ifFalse: [
							  cond violationMessageOn: stream.
							  stream cr ] ].
			  stream nextPutAll: 'Select the strategies to apply' ]
]
