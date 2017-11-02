Extension { #name : #TextVariableLink }

{ #category : #'*Text-Edition' }
TextVariableLink >> actOnClick: anEvent for: aNautilusUI in: aParagraph editor: anEditor [
	anEditor referencesTo: self variableName.
	^ true
]
