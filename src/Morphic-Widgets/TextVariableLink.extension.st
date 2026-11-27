Extension { #name : 'TextVariableLink' }

{ #category : '*Morphic-Widgets' }
TextVariableLink >> rubActOnClick: anEvent for: aNautilusUI in: aParagraph editor: anEditor [
	anEditor editor referencesTo: self variableName.
	^ true
]
