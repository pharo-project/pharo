Extension { #name : #Behavior }

{ #category : #'*SystemCommands-RefactoringSupport' }
Behavior >> scopesIconName [
	^ #class
]

{ #category : #'*SystemCommands-RefactoringSupport' }
Behavior >> subelements [
	^ self organization allProtocols
]
