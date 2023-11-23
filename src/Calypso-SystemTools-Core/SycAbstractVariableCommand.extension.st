Extension { #name : 'SycAbstractVariableCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycAbstractVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemFor: ClySourceCodeContext
]
