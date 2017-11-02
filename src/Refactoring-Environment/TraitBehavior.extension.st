Extension { #name : #TraitBehavior }

{ #category : #'*Refactoring-Environment' }
TraitBehavior >> asEnvironment [
	^ RBClassEnvironment class: self
]
