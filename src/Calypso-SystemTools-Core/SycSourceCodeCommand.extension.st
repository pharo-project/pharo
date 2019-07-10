Extension { #name : #SycSourceCodeCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycSourceCodeCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.4 for: ClyMethodSourceCodeContext
]
