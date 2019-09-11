Extension { #name : #SycFormatSourceCodeCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycFormatSourceCodeCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 100000 for: ClyMethodSourceCodeContext
]
