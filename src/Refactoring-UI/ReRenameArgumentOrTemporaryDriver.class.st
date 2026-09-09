"
I am a driver to manage the interractions during a rename argument or temporary refactoring. 

I'll check that the interval selected by the user is right. I'll ask a new name and check that it is valid. If it is not, I'll ask again to the user except if they cancel.
"
Class {
	#name : 'ReRenameArgumentOrTemporaryDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'sourceNode',
		'newName',
		'method',
		'notCheckVarName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver class >> driverName [

	^ 'Rename argument or temporary variable'
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'execution' }
ReRenameArgumentOrTemporaryDriver >> breakingChoices [

	| choices |

	choices := OrderedCollection new.
	notCheckVarName isFalse ifTrue: [
			choices add: (ReRenameTemporaryVariableChoice new driver: self).
			choices add: (ReNewVariableNameChoice new driver: self).
			choices add: (ReConvertTemporaryToExistingVariableChoice new driver: self) ].

 
	^ choices
]

{ #category : 'action' }
ReRenameArgumentOrTemporaryDriver >> convertTemporaryToExistingVariable [

	| transformation |
	self applyChanges.
	transformation := RBRemoveTemporaryVariableTransformation
		                  variable: notCheckVarName violators first
		                  inMethod: (method methodClass compiledMethodAt: method selector) selector
		                  inClass: method methodClass name.
	transformation privateTransform.
	self openPreviewWithChanges: transformation changes
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'execution' }
ReRenameArgumentOrTemporaryDriver >> gatherUserInput [

	| cancelled |
	cancelled := false.
	[
		refactoring newName: ((requestDialog ifNil: [ self defaultRequestDialog ])
				 title: 'Rename a temp variable';
				 message: 'New name of the variable';
				 text: sourceNode name;
				 "We cannot return in this block so we save the result as a boolean to check it later."onCancel: [ cancelled := true ];
				 openModal).

		cancelled ifTrue: [
				self inform: 'Refactoring cancelled'.
				^ false ].

		refactoring applicabilityPreconditionsOfTheNewName allSatisfy: [ :condition | condition check ] ] whileFalse: [
			refactoring applicabilityPreconditionsOfTheNewName
				detect: [ :condition | condition check not ]
				ifFound: [ :condition | self inform: condition errorString ] ].
	^ true
]

{ #category : 'private ex' }
ReRenameArgumentOrTemporaryDriver >> hasFailedApplicabilityPreconditions [

	(refactoring applicabilityPreconditionsIndependentOfTheNewName reject: [ :condition | condition check ]) ifNotEmpty: [
			self inform: 'The interval selected is not right to rename a temporary variable.'.
			^ true ].
	^ false
]

{ #category : 'initialization' }
ReRenameArgumentOrTemporaryDriver >> hasFailedBehaviorPreservingPreconditions [

	notCheckVarName := refactoring preconditionCheckVariableName.
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver >> instantiateRefactoring [

	^ ReRenameArgumentOrTemporaryRefactoring
		  renameTemporaryFrom: sourceNode sourceInterval
		  in: method origin
		  selector: method selector
]

{ #category : 'ui - dialogs' }
ReRenameArgumentOrTemporaryDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  refactoring failedBreakingChangePreconditions do: [ :cond |
					  cond check ifFalse: [
							  cond violationMessageOn: stream.
							  stream cr ] ].
			  stream nextPutAll: 'Select the strategies to apply' ]
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> method [

	^ method
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> method: anObject [

	method := anObject
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> newName [

	^ newName
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> newName: anObject [

	newName := anObject
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver >> requestDialog [

	^ requestDialog
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> sourceNode [

	^ sourceNode
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> sourceNode: anObject [

	sourceNode := anObject
]
