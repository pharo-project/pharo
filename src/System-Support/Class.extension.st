Extension { #name : #Class }

{ #category : #'*System-Support' }
Class >> allUnreferencedClassVariables [
	"Answer a list of the names of all the receiver's unreferenced class vars, including those defined in superclasses"

	^ self allClassVariables reject: #isReferenced
]
