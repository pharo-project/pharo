Extension { #name : 'SycOpenReflectivityMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenReflectivityMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^ClyContextMenuActivation byRootGroupItemOrder: 0.3 for: ClySourceCodeContext
]
