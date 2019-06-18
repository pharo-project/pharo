Extension { #name : #SycBrowseClassCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycBrowseClassCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]
