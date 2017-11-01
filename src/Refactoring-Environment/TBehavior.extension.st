Extension { #name : #TBehavior }

{ #category : #'*Refactoring-Environment' }
TBehavior >> asEnvironment [
	^ RBClassEnvironment class: self
]
