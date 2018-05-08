Extension { #name : #FinderUI }

{ #category : #'*NECompletion' }
FinderUI >> guessTypeForName: aString [ 
 
	self flag: 'we may be able to do something more sophisticated here, but needed something to prevent a DNU. Returning nil was taken from AbstractTool. See Debugger or Workspace for actual guessing logic'.
	^ nil
]

{ #category : #'*necompletion' }
FinderUI >> isCodeCompletionAllowed [

	^ true
]

{ #category : #'*necompletion' }
FinderUI >> selectedClassOrMetaClass [

	^ self selectedClass
]
