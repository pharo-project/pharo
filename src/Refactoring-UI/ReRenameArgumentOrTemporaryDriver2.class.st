"
I am a driver to manage the interractions during a rename argument or temporary refactoring. 

I'll check that the interval selected by the user is right. I'll ask a new name and check that it is valid. If it is not, I'll ask again to the user except if they cancel.
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

	| failedCondition |

	failedCondition := refactoring applicabilityPreconditions 
		detect: [ :condition | condition check not ] 			
		ifNone: [ nil ].

	failedCondition ifNotNil: [ ^ failedCondition errorString ].

	^ nil
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
ReRenameArgumentOrTemporaryDriver2 >> generateTransformationChanges [
	
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
ReRenameArgumentOrTemporaryDriver2 >> isTemporaryVariable [ 

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
