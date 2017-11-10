Extension { #name : #Behavior }

{ #category : #'*Refactoring-Environment' }
Behavior >> asEnvironment [
	^ RBClassEnvironment class: self
]
