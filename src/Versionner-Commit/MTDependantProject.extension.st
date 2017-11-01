Extension { #name : #MTDependantProject }

{ #category : #'*Versionner-Commit' }
MTDependantProject >> isPresent [
	^ self configurationClass notNil
]
