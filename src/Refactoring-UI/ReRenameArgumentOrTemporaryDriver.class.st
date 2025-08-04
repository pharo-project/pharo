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
		'method'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
ReRenameArgumentOrTemporaryDriver >> configureRefactoring [

	refactoring := RBRenameArgumentOrTemporaryRefactoring renameTemporaryFrom: sourceNode sourceInterval in: method origin selector: method selector
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

{ #category : 'execution' }
ReRenameArgumentOrTemporaryDriver >> runRefactoring [

	| cancelled |
	self configureRefactoring.
	refactoring prepareForExecution. "This is needed to set some info in the refactoring such as the parse tree or the defining node."
	
	(refactoring applicabilityPreconditionsIndependentOfTheNewName reject: [ :condition | condition check ]) 
		ifNotEmpty: [ ^ self inform: 'The interval selected is not right to rename a temporary variable.' ].

	cancelled := false.
	[ refactoring newName: (self defaultRequestDialog 
			 title: 'Rename a temp variable';
			 label: 'New name of the variable';
			 text: sourceNode name  ;
			 "We cannot return in this block so we save the result as a boolean to check it later."
			 onCancel: [  cancelled := true ];
			 openModal).

	cancelled ifTrue: [ ^ self inform: 'Refactoring cancelled' ].

	refactoring applicabilityPreconditionsOfTheNewName allSatisfy: [ :condition | condition check ] ] 
		whileFalse: [
			refactoring applicabilityPreconditionsOfTheNewName
				detect: [ :condition | condition check ]
				ifFound: [ :condition | self inform: condition errorString ] ].

	self applyChanges
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> sourceNode [

	^ sourceNode
]

{ #category : 'accessing' }
ReRenameArgumentOrTemporaryDriver >> sourceNode: anObject [

	sourceNode := anObject
]
