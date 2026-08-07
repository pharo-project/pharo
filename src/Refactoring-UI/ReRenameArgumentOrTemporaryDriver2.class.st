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
		'notCheckVarName',
		'temporaryVariableToRemove'
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

	 refactoring newName: newName.
    failedCondition := refactoring applicabilityPreconditions
        detect: [ :condition | condition check not ]
        ifNone: [ nil ].

    failedCondition ifNotNil: [
        ^ failedCondition errorString ].

    ^ nil
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver2 >> checkBehaviorPreservingPreconditions [

    | failedCondition |

	 refactoring newName: newName.
    failedCondition := refactoring breakingChangePreconditions 
        detect: [ :condition | condition check not ]
        ifNone: [ nil ].

    failedCondition ifNotNil: [
		  temporaryVariableToRemove := failedCondition violators first.
        ^ failedCondition errorString ].

    ^ nil
]

{ #category : 'configuration' }
ReRenameArgumentOrTemporaryDriver2 >> createAndSetNewRefactoring [	
	super configureRefactoring .
	refactoring newName: newName 
	
	

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

{ #category : 'action' }
ReRenameArgumentOrTemporaryDriver2 >> prepareTransformation: aTree [

	| transformation |

	transformation := RBRemoveTemporaryVariableTransformation2
		                  variable: temporaryVariableToRemove
		                  inMethod: (method methodClass compiledMethodAt: method selector) selector
		                  inClass: method methodClass.
	transformation privateTransform: aTree.
	
	^ transformation 
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
