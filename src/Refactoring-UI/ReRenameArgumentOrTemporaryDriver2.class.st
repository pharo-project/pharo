"
I am a driver to manage the interactions during an argument or temporary variable rename refactoring.

I receive the new name from the presenter and use it to configure the refactoring. I ask the refactoring to check its applicability and behavior-preserving preconditions and pass the results to the presenter. I start the rename workflow and provide the changes used to update the preview.
"
Class {
	#name : 'ReRenameArgumentOrTemporaryDriver2',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'sourceNode',
		'newName',
		'method',
		'notCheckVarName',
		'oldName'
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

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> checkApplicabilityPreconditions [

	^ refactoring failedApplicabilityMessage
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> checkBehaviorPreservingPreconditions [

	notCheckVarName := refactoring preconditionCheckVariableName.

	notCheckVarName check
		ifFalse: [ ^ notCheckVarName errorString ].

	^ nil
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver2 >> createAndSetNewRefactoring [	
	super configureRefactoring .
	refactoring newName: newName 
	
	

]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> generateRemoveVariableChanges [
	
	| transformation |
	
	refactoring generateChanges.
	transformation := RBRemoveTemporaryVariableTransformation
								model: refactoring model
		                  variable: newName
		                  inMethod: (method methodClass compiledMethodAt: method selector) selector
		                  inClass: method methodClass.
		
	transformation generateChanges.
	^ transformation changes
	
]

{ #category : 'private ex' }
ReRenameArgumentOrTemporaryDriver2 >> hasFailedApplicabilityPreconditions [

	(refactoring applicabilityPreconditionsIndependentOfTheNewName reject: [ :condition | condition check ]) ifNotEmpty: [
			self inform: 'There are no variables to rename in the selected interval.'.
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

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> isTemp [ 

	^ sourceNode isTempVariable
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

	^ previewPresenterClass ifNil: [ previewPresenterClass := StRenameTempOrArgPreviewPresenter ] 
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver2 >> requestDialog [

	^ requestDialog
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> runRefactoring [

	| presenter |

	super configureRefactoring.
	oldName := sourceNode name.
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
