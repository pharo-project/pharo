"
I am a driver to manage the interractions during a rename argument or temporary refactoring. 

I'll check that the interval selected by the user is right. I'll ask a new name and check that it is valid. If it is not, I'll ask again to the user except if they cancel.
"
Class {
	#name : 'ReRenameArgumentOrTemporaryDriver2',
	#superclass : 'ReInteractionDriver2',
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
ReRenameArgumentOrTemporaryDriver2 class >> driverName [

	^ 'Rename argument or temporary variable'
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 class >> shouldPreviewChangesByDefault [

	^ true
]

{ #category : 'execution' }
ReRenameArgumentOrTemporaryDriver2 >> breakingChoices [

	| choices |

	choices := OrderedCollection new.
	notCheckVarName isFalse ifTrue: [
			choices add: (ReRenameTemporaryVariableChoice new driver: self).
			choices add: (ReNewVariableNameChoice new driver: self).
			choices add: (ReConvertTemporaryToExistingVariableChoice new driver: self) ].

 
	^ choices
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> checkApplicabilityPreconditions [

    | failedCondition |

	 refactoring newName: newName.
    failedCondition := refactoring applicabilityPreconditions
        detect: [ :condition | condition check not ]
        ifNone: [ nil ].

    failedCondition ifNotNil: [
        ^ failedCondition errorString ].

    ^ nil
]

{ #category : 'action' }
ReRenameArgumentOrTemporaryDriver2 >> convertTemporaryToExistingVariable [

	| transformation |
	self halt.
	self applyChanges.
	transformation := RBRemoveTemporaryVariableTransformation
		                  variable: notCheckVarName violators first
		                  inMethod: (method methodClass compiledMethodAt: method selector) selector
		                  inClass: method methodClass name.
	transformation privateTransform.
	self openPreviewWithChanges: transformation changes
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver2 >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> failedConditionMessage: condition [
    | stream |
    stream := WriteStream on: String new.
    condition violationMessageOn: stream.
    ^ stream contents
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> gatherUserInput [
 
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
ReRenameArgumentOrTemporaryDriver2 >> hasFailedApplicabilityPreconditions [

	(refactoring applicabilityPreconditionsIndependentOfTheNewName reject: [ :condition | condition check ]) ifNotEmpty: [
			self inform: 'The interval selected is not right to rename a temporary variable.'.
			^ true ].
	^ false
]

{ #category : 'initialization' }
ReRenameArgumentOrTemporaryDriver2 >> hasFailedBehaviorPreservingPreconditions [

	notCheckVarName := refactoring preconditionCheckVariableName.
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver2 >> instantiateRefactoring [

	^ ReRenameArgumentOrTemporaryRefactoring 
		  renameTemporaryFrom: sourceNode sourceInterval
		  in: method origin
		  selector: method selector
]

{ #category : 'ui - dialogs' }
ReRenameArgumentOrTemporaryDriver2 >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  refactoring failedBreakingChangePreconditions do: [ :cond |
					  cond check ifFalse: [
							  cond violationMessageOn: stream.
							  stream cr ] ].
			  stream nextPutAll: 'Select the strategies to apply' ]
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> method [

	^ method
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> method: anObject [

	method := anObject
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> newName [

	^ newName
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> newName: anObject [

	newName := anObject
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> previewPresenterClass [

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRefactoringPreviewPresenter2 ] 
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver2 >> requestDialog [

	^ requestDialog
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> runRefactoring [

	| presenter |
	"self halt."
	super configureRefactoring.
	presenter := (self previewPresenterClass newApplication: self application)
		              scopes: scopes;
		              driver: self;
		              name: sourceNode name;
		              openModal
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> sourceNode [

	^ sourceNode
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> sourceNode: anObject [

	sourceNode := anObject
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> updateOldName [

	refactoring oldName: newName 
]
