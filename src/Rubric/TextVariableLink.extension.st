Extension { #name : #TextVariableLink }

{ #category : #'*Rubric-Editing-Core' }
TextVariableLink >> rubActOnClick: anEvent for: aNautilusUI in: aParagraph editor: anEditor [
	anEditor editor referencesTo: self variableName.
	^ true
]
