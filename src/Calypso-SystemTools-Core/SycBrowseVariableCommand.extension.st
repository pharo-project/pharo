Extension { #name : #SycBrowseVariableCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycBrowseVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]
