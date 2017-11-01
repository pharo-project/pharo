Extension { #name : #TClassDescription }

{ #category : #'*System-Support' }
TClassDescription >> allUnreferencedClassVariables [
	"Answer a list of the names of all the receiver's unreferenced class vars, including those defined in superclasses"

	^ self systemNavigation allUnreferencedClassVariablesOf: self
]
