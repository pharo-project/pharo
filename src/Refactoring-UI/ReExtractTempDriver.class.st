Class {
	#name : 'ReExtractTempDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'interval',
		'selector',
		'class'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
ReExtractTempDriver >> configureRefactoring [

	refactoring := RBExtractToTemporaryRefactoring
			extract: interval
			from: selector 
			in: class.
	refactoring prepareForExecution 
]

{ #category : 'instance creation' }
ReExtractTempDriver >> extract: anInterval from: aSelector in: aClass [

	interval := anInterval.
	selector := aSelector.
	class := aClass
]

{ #category : 'user requests' }
ReExtractTempDriver >> getVariableName: initialAnswer errorMessage: message [

	^ self requestDialog 
		title: 'Extract code to temp variable';
		label: 'New temp name: ' , message;
		text: initialAnswer;
		acceptLabel: 'Ok!';
		cancelLabel: 'Cancel';
		onAccept: [ :dialog | dialog presenter text ];
		openModal
	
	
		
]

{ #category : 'execution' }
ReExtractTempDriver >> runRefactoring [ 

	| varName failed errorString |
	self configureRefactoring.
	varName := ''.
	failed := nil.
	[
		errorString := failed ifNil: [ '' ] ifNotNil: [ self failedPreconditionsErrorString: failed ].
		varName := self getVariableName: varName errorMessage: errorString.
		varName ifNil: [ ^ self ].
		refactoring variableName: varName.
		failed := refactoring failedApplicabilityPreconditions.
	] doWhileFalse: 
		[ varName isEmptyOrNil not and: [ failed isEmpty ] ].
	
	self applyChanges
]
