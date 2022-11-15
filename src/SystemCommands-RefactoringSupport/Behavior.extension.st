Extension { #name : #Behavior }

{ #category : #'*SystemCommands-RefactoringSupport' }
Behavior >> subelements [
	^ self organization allProtocols
]
