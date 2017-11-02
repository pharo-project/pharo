Extension { #name : #ClassDescription }

{ #category : #'*System-Support' }
ClassDescription >> allUnreferencedClassVariables [
	"Answer a list of the names of all the receiver's unreferenced class vars, including those defined in superclasses"

	^ self systemNavigation allUnreferencedClassVariablesOf: self
]
